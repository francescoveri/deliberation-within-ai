###############################################################################
# reviewer_robustness_v4.R
#
# Robustness analyses for reviewer comments 1-4 (Minor Revision).
#
#   C1  Baseline gap in DRI      -> ANCOVA + HC3 + wild bootstrap + Lord diagnostic
#   C2  Length confound          -> common support, Spearman + boot CI,
#                                   truncated-prefix length-matched AQuA
#   C3  Small-n / multiplicity   -> Welch, exact Wilcoxon, exact permutation,
#                                   Holm, effect sizes with CIs
#   C4  Drift cap in both arms   -> empirical verification from turn-level data
#
# Dependencies: readxl only. HC3, permutation, bootstrap implemented in base R
# so the script runs on a bare install.
#
# NOTE: the column-mapping block below MUST be checked against your data.
# Run with INSPECT_ONLY <- TRUE first; it prints the structure and stops.
###############################################################################

suppressPackageStartupMessages(library(readxl))

## ---------------------------------------------------------------------------
## 0. CONFIG
## ---------------------------------------------------------------------------

PATH_GROUP      <- "data_new_group.xlsx"        # run-level (n = 20)
PATH_INDIVIDUAL <- "dataPnew_individual.xlsx"   # agent- / turn-level

INSPECT_ONLY <- TRUE     # set FALSE once the mapping below is confirmed
SEED         <- 20260804
N_BOOT       <- 10000
set.seed(SEED)

# Candidate column names. The resolver takes the first match (case-insensitive).
# Edit these to match your files rather than renaming your data.
MAP <- list(
  condition   = c("condition", "architecture", "arm", "design", "pipeline"),
  run_id      = c("run_id", "run", "id", "simulation_id", "batch_id"),
  aqua        = c("aqua", "aqua_mean", "aqua_score", "AQuA"),
  dri_pre     = c("dri_pre", "user_dri_pre", "pre_dri", "dri_t0"),
  dri_post    = c("dri_post", "user_dri_post", "post_dri", "dri_t1"),
  dri_change  = c("dri_change", "delta_dri", "d_dri", "dri_diff"),
  opinion_chg = c("opinion_change", "delta_opinion", "opinion_shift"),
  polar_chg   = c("polarisation_change", "polarization_change",
                  "delta_polarisation", "delta_polarization"),
  n_turns     = c("n_turns", "turns", "num_turns", "turn_count"),
  n_words     = c("n_words", "total_words", "words", "word_count")
)

# Individual/turn-level columns (only needed for C2 prefix test and C4)
MAP_IND <- list(
  condition   = c("condition", "architecture", "arm"),
  run_id      = c("run_id", "run", "id", "simulation_id"),
  agent_id    = c("agent_id", "agent", "persona_id", "participant"),
  turn_index  = c("turn", "turn_index", "turn_no", "contribution_index", "t"),
  position    = c("position", "score", "opinion", "stance", "rating"),
  aqua_turn   = c("aqua", "aqua_turn", "aqua_score", "contribution_aqua")
)

# Which level of `condition` is the structured pipeline?
STRUCTURED_LABEL <- NULL   # e.g. "structured"; NULL = infer as the level with
                           # more turns/words

## ---------------------------------------------------------------------------
## 1. HELPERS
## ---------------------------------------------------------------------------

resolve <- function(df, candidates, required = TRUE, what = "") {
  nm <- tolower(trimws(names(df)))
  for (cand in candidates) {
    hit <- which(nm == tolower(cand))
    if (length(hit)) return(names(df)[hit[1]])
  }
  for (cand in candidates) {                    # relaxed: substring match
    hit <- grep(tolower(cand), nm, fixed = TRUE)
    if (length(hit)) return(names(df)[hit[1]])
  }
  if (required) stop(sprintf("Could not resolve column for '%s'. Tried: %s",
                             what, paste(candidates, collapse = ", ")))
  NA_character_
}

hdr <- function(x) cat("\n", strrep("=", 78), "\n", x, "\n",
                       strrep("=", 78), "\n", sep = "")
sub <- function(x) cat("\n--- ", x, " ---\n", sep = "")
fp  <- function(p) ifelse(p < .0001, "<.0001", sprintf("%.4f", p))

## --- HC3 covariance, implemented directly (no sandwich dependency) ---------
hc3 <- function(fit) {
  X  <- model.matrix(fit)
  e  <- as.numeric(residuals(fit))
  h  <- hatvalues(fit)
  bread <- solve(crossprod(X))
  meat  <- crossprod(X, diag(e^2 / (1 - h)^2) %*% X)
  bread %*% meat %*% bread
}

hc3_table <- function(fit) {
  V  <- hc3(fit)
  b  <- coef(fit)
  se <- sqrt(diag(V))
  t  <- b / se
  df <- df.residual(fit)
  p  <- 2 * pt(-abs(t), df)
  crit <- qt(.975, df)
  data.frame(term = names(b), estimate = b, hc3_se = se,
             t = t, df = df, p = p,
             ci_lo = b - crit * se, ci_hi = b + crit * se,
             row.names = NULL)
}

## --- Wild bootstrap (Rademacher, restricted residuals) ---------------------
## Standard small-n correction. Imposes H0: coefficient of interest = 0.
wild_boot <- function(y, X, idx_test, B = N_BOOT) {
  full  <- lm.fit(X, y)
  b_hat <- full$coefficients[idx_test]
  V     <- hc3(lm(y ~ X - 1))
  t_obs <- b_hat / sqrt(diag(V)[idx_test])

  Xr    <- X[, -idx_test, drop = FALSE]         # restricted design under H0
  restr <- lm.fit(Xr, y)
  yhat0 <- as.numeric(Xr %*% restr$coefficients)
  e0    <- as.numeric(y - yhat0)
  h0    <- diag(Xr %*% solve(crossprod(Xr)) %*% t(Xr))
  e0    <- e0 / sqrt(1 - h0)                    # HC3-style rescaling

  t_star <- numeric(B)
  for (b in seq_len(B)) {
    v  <- sample(c(-1, 1), length(y), replace = TRUE)   # Rademacher
    ys <- yhat0 + e0 * v
    fs <- lm(ys ~ X - 1)
    t_star[b] <- coef(fs)[idx_test] / sqrt(diag(hc3(fs))[idx_test])
  }
  list(t_obs = t_obs,
       p     = mean(abs(t_star) >= abs(t_obs)),
       crit  = quantile(abs(t_star), .95, names = FALSE))
}

## --- Exact two-sided permutation test, full enumeration --------------------
perm_exact <- function(x, g) {
  g <- as.factor(g); lv <- levels(g)
  if (length(lv) != 2) stop("perm_exact needs exactly two groups")
  n <- length(x); n1 <- sum(g == lv[1])
  combs <- combn(n, n1)
  n_perm <- ncol(combs)
  tot <- sum(x)
  m1  <- colSums(matrix(x[combs], nrow = n1)) / n1
  m2  <- (tot - m1 * n1) / (n - n1)
  d   <- m1 - m2
  obs <- mean(x[g == lv[1]]) - mean(x[g == lv[2]])
  tol <- 1e-10
  list(observed = obs, n_perm = n_perm,
       p = mean(abs(d) >= abs(obs) - tol))
}

## --- Effect sizes ----------------------------------------------------------
hedges_g <- function(x, y) {
  nx <- length(x); ny <- length(y)
  sp <- sqrt(((nx - 1) * var(x) + (ny - 1) * var(y)) / (nx + ny - 2))
  d  <- (mean(x) - mean(y)) / sp
  J  <- 1 - 3 / (4 * (nx + ny) - 9)
  d * J
}

cliffs_delta <- function(x, y) {
  m <- outer(x, y, function(a, b) sign(a - b))
  mean(m)
}

boot_ci <- function(x, y, fun, B = N_BOOT) {
  v <- replicate(B, fun(sample(x, replace = TRUE), sample(y, replace = TRUE)))
  c(lo = unname(quantile(v, .025)), hi = unname(quantile(v, .975)))
}

boot_spearman_ci <- function(a, b, B = N_BOOT) {
  n <- length(a)
  v <- replicate(B, { i <- sample(n, replace = TRUE)
                      suppressWarnings(cor(a[i], b[i], method = "spearman")) })
  v <- v[is.finite(v)]
  c(lo = unname(quantile(v, .025)), hi = unname(quantile(v, .975)))
}

## ---------------------------------------------------------------------------
## 2. LOAD + INSPECT
## ---------------------------------------------------------------------------

hdr("DATA STRUCTURE")

grp <- as.data.frame(read_excel(PATH_GROUP))
cat("GROUP file: ", PATH_GROUP, "  dim = ", nrow(grp), " x ", ncol(grp), "\n", sep = "")
print(data.frame(column = names(grp), class = sapply(grp, function(z) class(z)[1]),
                 row.names = NULL))

ind <- NULL
if (file.exists(PATH_INDIVIDUAL)) {
  ind <- as.data.frame(read_excel(PATH_INDIVIDUAL))
  cat("\nINDIVIDUAL file: ", PATH_INDIVIDUAL, "  dim = ", nrow(ind), " x ",
      ncol(ind), "\n", sep = "")
  print(data.frame(column = names(ind), class = sapply(ind, function(z) class(z)[1]),
                   row.names = NULL))
}

if (INSPECT_ONLY) {
  cat("\n>>> INSPECT_ONLY is TRUE. Confirm the MAP block above matches these",
      "\n>>> column names, then set INSPECT_ONLY <- FALSE and re-run.\n")
  quit(save = "no", status = 0)
}

## Resolve group-level columns
G <- lapply(names(MAP), function(k)
  resolve(grp, MAP[[k]],
          required = k %in% c("condition", "aqua", "dri_pre", "dri_post"),
          what = k))
names(G) <- names(MAP)

d <- data.frame(
  cond     = as.factor(grp[[G$condition]]),
  aqua     = as.numeric(grp[[G$aqua]]),
  dri_pre  = as.numeric(grp[[G$dri_pre]]),
  dri_post = as.numeric(grp[[G$dri_post]])
)
d$dri_chg <- if (!is.na(G$dri_change)) as.numeric(grp[[G$dri_change]]) else
             d$dri_post - d$dri_pre
if (!is.na(G$opinion_chg)) d$op_chg    <- as.numeric(grp[[G$opinion_chg]])
if (!is.na(G$polar_chg))   d$pol_chg   <- as.numeric(grp[[G$polar_chg]])
if (!is.na(G$n_turns))     d$n_turns   <- as.numeric(grp[[G$n_turns]])
if (!is.na(G$n_words))     d$n_words   <- as.numeric(grp[[G$n_words]])

## Identify the structured arm
if (is.null(STRUCTURED_LABEL)) {
  key <- if (!is.null(d$n_words)) "n_words" else if (!is.null(d$n_turns)) "n_turns" else NULL
  if (is.null(key)) stop("Set STRUCTURED_LABEL manually: no length column found.")
  mm <- tapply(d[[key]], d$cond, mean)
  STRUCTURED_LABEL <- names(mm)[which.max(mm)]
  cat("\nInferred structured arm from", key, ":", STRUCTURED_LABEL, "\n")
}
d$cond <- relevel(d$cond, ref = setdiff(levels(d$cond), STRUCTURED_LABEL)[1])
d$structured <- as.integer(d$cond == STRUCTURED_LABEL)

STR <- d$structured == 1
DIR <- d$structured == 0
cat("n structured =", sum(STR), " | n direct =", sum(DIR), "\n")

## ---------------------------------------------------------------------------
## 3. DESCRIPTIVES AND COMMON SUPPORT              [C1, C2]
## ---------------------------------------------------------------------------

hdr("DESCRIPTIVES AND COMMON SUPPORT")

desc <- function(v, nm) {
  if (is.null(v)) return(NULL)
  data.frame(outcome = nm,
             M_direct = mean(v[DIR]), SD_direct = sd(v[DIR]),
             min_direct = min(v[DIR]), max_direct = max(v[DIR]),
             M_struct = mean(v[STR]), SD_struct = sd(v[STR]),
             min_struct = min(v[STR]), max_struct = max(v[STR]))
}
desc_tbl <- do.call(rbind, list(
  desc(d$aqua, "AQuA"), desc(d$dri_pre, "DRI pre"), desc(d$dri_post, "DRI post"),
  desc(d$dri_chg, "dDRI"), desc(d$op_chg, "Opinion change"),
  desc(d$pol_chg, "Polarisation change"),
  desc(d$n_turns, "Turns"), desc(d$n_words, "Words")))
print(desc_tbl, digits = 3, row.names = FALSE)

sub("Common support (C2): does the length range overlap?")
for (v in c("n_turns", "n_words")) {
  if (is.null(d[[v]])) next
  rd <- range(d[[v]][DIR]); rs <- range(d[[v]][STR])
  ov <- max(0, min(rd[2], rs[2]) - max(rd[1], rs[1]))
  cat(sprintf("%-8s direct [%.0f, %.0f]  structured [%.0f, %.0f]  overlap = %s\n",
              v, rd[1], rd[2], rs[1], rs[2],
              if (ov > 0) sprintf("%.0f units", ov) else "NONE"))
}
cat("\nIf overlap is NONE, a run-level covariate adjustment for length is\n",
    "extrapolation, not adjustment. Report this and use Section 7 instead.\n", sep = "")

## ---------------------------------------------------------------------------
## 4. PRIMARY TESTS: WELCH / WILCOXON / EXACT PERMUTATION / HOLM    [C3]
## ---------------------------------------------------------------------------

hdr("C3: SMALL-n INFERENCE AND MULTIPLICITY")

outcomes <- list(AQuA = d$aqua, dDRI = d$dri_chg)
if (!is.null(d$op_chg))  outcomes[["Opinion change"]]      <- d$op_chg
if (!is.null(d$pol_chg)) outcomes[["Polarisation change"]] <- d$pol_chg

res <- do.call(rbind, lapply(names(outcomes), function(nm) {
  v  <- outcomes[[nm]]
  w  <- t.test(v[STR], v[DIR])                                    # Welch
  wx <- suppressWarnings(wilcox.test(v[STR], v[DIR], exact = TRUE))
  pm <- perm_exact(v, d$cond)
  g  <- hedges_g(v[STR], v[DIR]); gci <- boot_ci(v[STR], v[DIR], hedges_g)
  cd <- cliffs_delta(v[STR], v[DIR]); cci <- boot_ci(v[STR], v[DIR], cliffs_delta)
  data.frame(outcome = nm,
             M_diff = mean(v[STR]) - mean(v[DIR]),
             welch_p = w$p.value, welch_df = unname(w$parameter),
             wilcox_p = wx$p.value, perm_p = pm$p, n_perm = pm$n_perm,
             hedges_g = g, g_lo = gci["lo"], g_hi = gci["hi"],
             cliff_d = cd, d_lo = cci["lo"], d_hi = cci["hi"],
             row.names = NULL)
}))

res$welch_holm  <- p.adjust(res$welch_p,  method = "holm")
res$perm_holm   <- p.adjust(res$perm_p,   method = "holm")
res$wilcox_holm <- p.adjust(res$wilcox_p, method = "holm")

sub("Ties check (exact Wilcoxon is invalid under ties)")
for (nm in names(outcomes)) {
  v <- outcomes[[nm]]
  cat(sprintf("%-22s ties present: %s\n", nm,
              if (any(duplicated(v))) "YES - exact p unavailable, normal approx used"
              else "no"))
}

sub("Supplementary table (manuscript-ready)")
supp <- data.frame(
  Outcome                    = res$outcome,
  `Welch p`                  = fp(res$welch_p),
  `Wilcoxon p`               = fp(res$wilcox_p),
  `Exact permutation p`      = fp(res$perm_p),
  `Holm-adjusted Welch p`    = fp(res$welch_holm),
  `Holm-adjusted perm p`     = fp(res$perm_holm),
  check.names = FALSE)
print(supp, row.names = FALSE)
cat("\nPermutation enumeration: all", res$n_perm[1], "allocations (complete, not sampled).\n")

sub("Effect sizes with bootstrap CIs")
print(data.frame(Outcome = res$outcome,
                 `Mean diff` = round(res$M_diff, 4),
                 `Hedges g`  = sprintf("%.2f [%.2f, %.2f]", res$hedges_g, res$g_lo, res$g_hi),
                 `Cliff d`   = sprintf("%.2f [%.2f, %.2f]", res$cliff_d, res$d_lo, res$d_hi),
                 check.names = FALSE), row.names = FALSE)

## ---------------------------------------------------------------------------
## 5. BASELINE-ADJUSTED DRI                                        [C1]
## ---------------------------------------------------------------------------

hdr("C1: BASELINE-ADJUSTED DRI SENSITIVITY ANALYSIS")

sub("Which arm starts lower? (this is what drives the reviewer's concern)")
cat(sprintf("Pre-DRI  direct = %.3f   structured = %.3f   gap = %.3f\n",
            mean(d$dri_pre[DIR]), mean(d$dri_pre[STR]),
            mean(d$dri_pre[STR]) - mean(d$dri_pre[DIR])))
cat("The lower-starting arm is:",
    if (mean(d$dri_pre[STR]) < mean(d$dri_pre[DIR])) "STRUCTURED" else "DIRECT",
    "\n=> regression to the mean favours the result only if it is the STRUCTURED arm.\n")

sub("ANCOVA: post-DRI ~ condition + pre-DRI, HC3 SEs")
fit <- lm(dri_post ~ structured + dri_pre, data = d)
print(hc3_table(fit), digits = 4, row.names = FALSE)

sub("Lord's paradox diagnostic: within-condition slope of post on pre")
for (lv in levels(d$cond)) {
  s <- d[d$cond == lv, ]
  f <- lm(dri_post ~ dri_pre, data = s)
  cat(sprintf("  %-14s b = %+.3f (SE %.3f), r = %+.3f\n", lv,
              coef(f)[2], summary(f)$coefficients[2, 2],
              cor(s$dri_pre, s$dri_post)))
}
cat("\nIf the pooled slope is near 0, the change score OVER-corrects for baseline\n",
    "and ANCOVA is the better-specified estimator. Argue this explicitly: it\n",
    "converts comment 1 from a threat into support.\n", sep = "")

sub("Wild bootstrap (Rademacher) on the condition coefficient")
X  <- model.matrix(fit)
wb <- wild_boot(d$dri_post, X, idx_test = which(colnames(X) == "structured"))
cat(sprintf("t_obs = %.3f | wild bootstrap p = %s | 95%% |t| crit = %.3f (B = %d)\n",
            wb$t_obs, fp(wb$p), wb$crit, N_BOOT))
cat("Report alongside HC3: at n = 20, HC3 alone is mildly liberal.\n")

sub("Change-score model for comparison")
print(hc3_table(lm(dri_chg ~ structured, data = d)), digits = 4, row.names = FALSE)

## ---------------------------------------------------------------------------
## 6. LENGTH AND AQuA (RUN LEVEL)                                  [C2]
## ---------------------------------------------------------------------------

hdr("C2: INTERACTION LENGTH AND AQuA")

for (v in c("n_turns", "n_words")) {
  if (is.null(d[[v]])) next
  sub(paste("Within-condition Spearman: AQuA vs", v))
  for (lv in levels(d$cond)) {
    s  <- d[d$cond == lv, ]
    ct <- suppressWarnings(cor.test(s$aqua, s[[v]], method = "spearman"))
    ci <- boot_spearman_ci(s$aqua, s[[v]])
    cat(sprintf("  %-14s rho = %+.3f, p = %s, 95%% boot CI [%+.3f, %+.3f], n = %d\n",
                lv, unname(ct$estimate), fp(ct$p.value), ci["lo"], ci["hi"], nrow(s)))
  }
  cat("\n  Framing: with n = 10 per arm these CIs span most of the range. Report as\n",
      "  UNDERPOWERED, not as evidence that length is unrelated to AQuA.\n", sep = "")
}

sub("Exploratory HC3 model (extrapolative - label it as such)")
if (!is.null(d$n_words)) {
  print(hc3_table(lm(aqua ~ structured + n_words, data = d)), digits = 4, row.names = FALSE)
  cat("\nDo NOT present this as isolating architecture from duration: without\n",
      "common support the length coefficient is identified only by extrapolation.\n", sep = "")
}

## ---------------------------------------------------------------------------
## 7. LENGTH-MATCHED PREFIX TEST                                   [C2 - key]
## ---------------------------------------------------------------------------

hdr("C2: LENGTH-MATCHED AQuA VIA TRUNCATED PREFIXES")

I <- NULL
if (!is.null(ind)) {
  I <- lapply(names(MAP_IND), function(k)
    resolve(ind, MAP_IND[[k]], required = FALSE, what = k))
  names(I) <- names(MAP_IND)
}

if (!is.null(ind) && !is.na(I$aqua_turn) && !is.na(I$turn_index) && !is.na(I$run_id)) {

  ti <- data.frame(run  = ind[[I$run_id]],
                   cond = as.factor(ind[[I$condition]]),
                   turn = as.numeric(ind[[I$turn_index]]),
                   aqua = as.numeric(ind[[I$aqua_turn]]))
  ti <- ti[!is.na(ti$aqua), ]
  ti <- ti[order(ti$run, ti$turn), ]

  # cap = the largest number of contributions available in EVERY direct run
  cap <- min(tapply(ti$turn[ti$cond != STRUCTURED_LABEL],
                    ti$run[ti$cond != STRUCTURED_LABEL], length))
  cat("Matching cap = first", cap, "scored contributions per run.\n\n")

  pre <- do.call(rbind, lapply(split(ti, ti$run), function(s)
    data.frame(run = s$run[1], cond = s$cond[1],
               aqua_prefix = mean(head(s$aqua, cap)),
               aqua_full   = mean(s$aqua))))
  pre$structured <- as.integer(pre$cond == STRUCTURED_LABEL)

  sub("AQuA restricted to matched prefixes")
  cat(sprintf("  direct     M = %.3f (SD %.3f)\n",
              mean(pre$aqua_prefix[pre$structured == 0]),
              sd(pre$aqua_prefix[pre$structured == 0])))
  cat(sprintf("  structured M = %.3f (SD %.3f)\n",
              mean(pre$aqua_prefix[pre$structured == 1]),
              sd(pre$aqua_prefix[pre$structured == 1])))

  pw <- t.test(pre$aqua_prefix[pre$structured == 1], pre$aqua_prefix[pre$structured == 0])
  pp <- perm_exact(pre$aqua_prefix, pre$cond)
  pg <- hedges_g(pre$aqua_prefix[pre$structured == 1], pre$aqua_prefix[pre$structured == 0])
  cat(sprintf("\n  Welch p = %s | exact permutation p = %s | Hedges g = %.2f\n",
              fp(pw$p.value), fp(pp$p), pg))
  cat("\n  THIS is the length-matched analysis the reviewer asked for. If the\n",
      "  effect survives here, comment 2 is answered rather than conceded.\n", sep = "")

} else {
  cat("SKIPPED: no contribution-level AQuA column found in the individual file.\n\n")
  cat("Before conceding comment 2, check whether per-turn AQuA scores were logged.\n",
      "If they exist anywhere (app logs, JSON output, scoring cache), this test\n",
      "requires no new simulation and no rescoring - only re-aggregation.\n", sep = "")
}

## ---------------------------------------------------------------------------
## 8. DRIFT CAP VERIFICATION                                       [C4]
## ---------------------------------------------------------------------------

hdr("C4: EMPIRICAL VERIFICATION OF THE +/-1 ANCHORED-DRIFT CAP")

if (!is.null(ind) && !is.na(I$position) && !is.na(I$agent_id) && !is.na(I$turn_index)) {

  pos <- data.frame(run   = ind[[I$run_id]],
                    cond  = as.factor(ind[[I$condition]]),
                    agent = ind[[I$agent_id]],
                    turn  = as.numeric(ind[[I$turn_index]]),
                    pos   = as.numeric(ind[[I$position]]))
  pos <- pos[!is.na(pos$pos), ]
  pos <- pos[order(pos$run, pos$agent, pos$turn), ]

  key   <- paste(pos$run, pos$agent, sep = "|")
  steps <- do.call(rbind, lapply(split(pos, key), function(s) {
    if (nrow(s) < 2) return(NULL)
    data.frame(cond = s$cond[-1], step = abs(diff(s$pos)))
  }))

  sub("Per-turn absolute position change, by condition")
  vio <- 0
  for (lv in levels(steps$cond)) {
    v <- steps$step[steps$cond == lv]
    n_v <- sum(v > 1 + 1e-9); vio <- vio + n_v
    cat(sprintf("  %-14s n = %4d  max |delta| = %.3f  mean = %.3f  > 1.0: %d (%.1f%%)\n",
                lv, length(v), max(v), mean(v), n_v, 100 * n_v / length(v)))
  }
  cat(sprintf("\n  Total violations across both conditions: %d\n", vio))
  if (vio == 0) {
    cat("  => The +/-1 cap is empirically confirmed as ACTIVE IN BOTH ARMS.\n",
        "     Report this in the manuscript. A data-side demonstration is far\n",
        "     stronger than an assertion about what the source code does.\n", sep = "")
  } else {
    cat("  => VIOLATIONS PRESENT. Resolve before claiming the cap is symmetric.\n",
        "     Check whether these come from ranking-stage rather than\n",
        "     deliberative-turn scores.\n", sep = "")
  }

  sub("Distribution of step sizes (should be discrete and bounded at 1)")
  print(table(round(steps$step, 3), steps$cond))

} else {
  cat("SKIPPED: turn-level position column not found.\n")
  cat("Needed columns: run id, agent id, turn index, position/score.\n")
}

## ---------------------------------------------------------------------------
## 9. EXPORT
## ---------------------------------------------------------------------------

hdr("EXPORT")
write.csv(supp,     "supp_table_inference.csv",   row.names = FALSE)
write.csv(res,      "robustness_full_results.csv", row.names = FALSE)
write.csv(desc_tbl, "descriptives_by_condition.csv", row.names = FALSE)
cat("Written: supp_table_inference.csv, robustness_full_results.csv,\n",
    "         descriptives_by_condition.csv\n", sep = "")
cat("\nSession:", R.version.string, "| seed:", SEED, "| B:", N_BOOT, "\n")
