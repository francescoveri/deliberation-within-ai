# =========================================================================
# Replication script for the batch replication study
#
# "Deliberation Within AI: A Reproducible Pipeline for Multi-Agent
#  Deliberation Simulation"
#
# =========================================================================


# -------------------------------------------------------------------------
# 0. Packages
# -------------------------------------------------------------------------
# install.packages(c("readxl", "dplyr", "tidyr", "purrr", "tibble",
#                    "sandwich", "lmtest"))

library(readxl)
library(dplyr)
library(tidyr)
library(purrr)
library(tibble)
library(sandwich)   # HC3 covariance
library(lmtest)     # coeftest / coefci

options(stringsAsFactors = FALSE)


# -------------------------------------------------------------------------
# 1. Load data
# -------------------------------------------------------------------------
group_file      <- "data_new_group.xlsx"
individual_file <- "dataPnew_individual.xlsx"

group      <- read_excel(group_file)
individual <- read_excel(individual_file)

# Ensure condition order: differences below are Structured - Direct
lvls <- c("direct_deliberation", "structured_pipeline")

group      <- group      %>% mutate(condition = factor(condition, levels = lvls))
individual <- individual %>% mutate(condition = factor(condition, levels = lvls))

stopifnot(!any(is.na(group$condition)), !any(is.na(individual$condition)))


# -------------------------------------------------------------------------
# 2. Helper functions
# -------------------------------------------------------------------------

# Run-level descriptive stats by condition
summarise_by_condition <- function(data, var) {
  data %>%
    group_by(condition) %>%
    summarise(
      n    = sum(!is.na(.data[[var]])),
      mean = mean(.data[[var]], na.rm = TRUE),
      sd   = sd(.data[[var]], na.rm = TRUE),
      .groups = "drop"
    )
}

# Welch test on run-level means. Main parametric test used in the paper.
welch_test <- function(data, var) {
  f  <- as.formula(paste(var, "~ condition"))
  tt <- t.test(f, data = data, var.equal = FALSE)
  desc <- summarise_by_condition(data, var)

  m_direct  <- desc$mean[desc$condition == "direct_deliberation"]
  m_struct  <- desc$mean[desc$condition == "structured_pipeline"]
  sd_direct <- desc$sd[desc$condition   == "direct_deliberation"]
  sd_struct <- desc$sd[desc$condition   == "structured_pipeline"]
  n_direct  <- desc$n[desc$condition    == "direct_deliberation"]
  n_struct  <- desc$n[desc$condition    == "structured_pipeline"]

  # t.test returns Direct - Structured because direct is the first factor
  # level. Signs are flipped below so everything reads Structured - Direct.
  pooled_sd <- sqrt(((n_direct - 1) * sd_direct^2 + (n_struct - 1) * sd_struct^2) /
                      (n_direct + n_struct - 2))
  d <- (m_struct - m_direct) / pooled_sd

  tibble(
    variable                        = var,
    n_direct                        = n_direct,
    mean_direct                     = m_direct,
    sd_direct                       = sd_direct,
    n_structured                    = n_struct,
    mean_structured                 = m_struct,
    sd_structured                   = sd_struct,
    diff_structured_minus_direct    = m_struct - m_direct,
    t                               = unname(-tt$statistic),
    df                              = unname(tt$parameter),
    p                               = tt$p.value,
    ci_low_structured_minus_direct  = -tt$conf.int[2],
    ci_high_structured_minus_direct = -tt$conf.int[1],
    cohen_d                         = d
  )
}

# Paired within-condition pre/post test
paired_change_test <- function(data, pre, post) {
  data %>%
    group_by(condition) %>%
    group_modify(~{
      tt    <- t.test(.x[[post]], .x[[pre]], paired = TRUE)
      delta <- .x[[post]] - .x[[pre]]
      tibble(
        n            = sum(!is.na(delta)),
        mean_pre     = mean(.x[[pre]],  na.rm = TRUE),
        mean_post    = mean(.x[[post]], na.rm = TRUE),
        mean_delta   = mean(delta,      na.rm = TRUE),
        sd_delta     = sd(delta,        na.rm = TRUE),
        t            = unname(tt$statistic),
        df           = unname(tt$parameter),
        p            = tt$p.value,
        ci_low_delta = tt$conf.int[1],
        ci_high_delta= tt$conf.int[2]
      )
    }) %>%
    ungroup()
}

# Exact two-sided permutation test on the difference in means.
# With 10 runs per condition this enumerates all choose(20,10) = 184,756
# allocations, so the p-value is exact and no seed is required.
exact_perm_test <- function(data, var, group_var = "condition") {
  d <- data[!is.na(data[[var]]), ]
  y <- d[[var]]
  g <- droplevels(as.factor(d[[group_var]]))
  stopifnot(nlevels(g) == 2)

  n  <- length(y)
  n1 <- sum(g == levels(g)[2])           # structured
  obs <- mean(y[g == levels(g)[2]]) - mean(y[g == levels(g)[1]])

  idx    <- utils::combn(n, n1)          # columns = allocations
  total  <- ncol(idx)
  sum_all <- sum(y)

  # mean difference for every allocation, vectorised over columns
  sums1 <- colSums(matrix(y[idx], nrow = n1))
  diffs <- sums1 / n1 - (sum_all - sums1) / (n - n1)

  p_two <- mean(abs(diffs) >= abs(obs) - 1e-12)

  tibble(
    variable       = var,
    observed_diff  = obs,
    n_permutations = total,
    perm_p_twosided = p_two
  )
}

# Wilcoxon rank-sum (Mann-Whitney)
wilcox_test_tbl <- function(data, var) {
  f  <- as.formula(paste(var, "~ condition"))
  wt <- suppressWarnings(wilcox.test(f, data = data, exact = FALSE))
  tibble(variable = var, wilcoxon_W = unname(wt$statistic), wilcoxon_p = wt$p.value)
}

# HC3 coefficient table for a fitted lm
hc3_table <- function(fit, label = "") {
  ct <- lmtest::coeftest(fit, vcov. = sandwich::vcovHC(fit, type = "HC3"))
  ci <- lmtest::coefci(fit, vcov. = sandwich::vcovHC(fit, type = "HC3"))
  tibble(
    model     = label,
    term      = rownames(ct),
    estimate  = ct[, 1],
    std_error = ct[, 2],
    t         = ct[, 3],
    p         = ct[, 4],
    ci_low    = ci[, 1],
    ci_high   = ci[, 2]
  )
}


# -------------------------------------------------------------------------
# 3. Confirm sample structure
# -------------------------------------------------------------------------
cat("\n=========================================================\n")
cat(" 3. SAMPLE STRUCTURE\n")
cat("=========================================================\n")

cat("\n--- Group-level sample size ---\n")
print(group %>% count(condition))

cat("\n--- Individual-level sample size ---\n")
print(individual %>% count(condition))

cat("\n--- Individual rows by original run_id ---\n")
print(individual %>% count(condition, run_id), n = Inf)
cat("\nNote: if a run_id is duplicated in the individual file, all inferential\n",
    "tests below still use the group-level file. Run blocks in Section 6 are\n",
    "reconstructed positionally and validated before use.\n", sep = "")


# -------------------------------------------------------------------------
# 4. Table G1. Between-condition run-level Welch tests
# -------------------------------------------------------------------------
cat("\n=========================================================\n")
cat(" 4. TABLE G1 - Run-level Welch tests (Structured - Direct)\n")
cat("=========================================================\n")

main_vars <- c(
  "aqua_score",
  "participation_gini",
  "user_dri_pre",
  "user_dri_post",
  "user_dri_delta",
  "pre_mean_opinion",
  "post_mean_opinion",
  "opinion_delta",
  "pre_polarization_sd",
  "post_polarization_sd",
  "polarization_delta",
  "total_turns",
  "total_words"
)

missing_vars <- setdiff(main_vars, names(group))
if (length(missing_vars)) {
  stop("Missing expected group-level variables: ", paste(missing_vars, collapse = ", "))
}

main_results <- map_dfr(main_vars, ~welch_test(group, .x))
print(main_results, n = Inf, width = Inf)


# -------------------------------------------------------------------------
# 5. Table G2. Within-condition paired DRI tests
# -------------------------------------------------------------------------
cat("\n=========================================================\n")
cat(" 5. TABLE G2 - Within-condition paired pre/post User-DRI\n")
cat("=========================================================\n")

dri_paired <- paired_change_test(group, pre = "user_dri_pre", post = "user_dri_post")
print(dri_paired, width = Inf)

cat("\n--- Between-condition test of User-DRI change (central test) ---\n")
print(main_results %>% filter(variable == "user_dri_delta"), width = Inf)


# -------------------------------------------------------------------------
# 6. Tables G3, G4, G5. Ideology-opinion correlations
# -------------------------------------------------------------------------
cat("\n=========================================================\n")
cat(" 6. TABLES G3-G5 - Ideology-opinion correlations\n")
cat("=========================================================\n")

# G3: 
cat("\n--- Table G3. Pooled Pearson correlations (descriptive) ---\n")
pooled_cor <- individual %>%
  group_by(condition) %>%
  summarise(
    n_agents = sum(!is.na(political_ideology) & !is.na(final_score_resolved)),
    r        = cor(political_ideology, final_score_resolved, use = "complete.obs"),
    p_individual_level_only = cor.test(political_ideology, final_score_resolved)$p.value,
    .groups  = "drop"
  )
print(pooled_cor, width = Inf)


individual_blocks <- individual %>%
  group_by(condition) %>%
  mutate(.row = row_number(),
         run_block = ceiling(.row / 10)) %>%
  ungroup()

block_counts <- individual_blocks %>% count(condition, run_block)
cat("\n--- Check constructed run blocks ---\n")
print(block_counts, n = Inf)

if (!all(block_counts$n == 10)) {
  stop("Run blocks are not all of size 10. Inspect row ordering in the ",
       "individual-level file before interpreting Tables G4/G5.")
}

# G4: run-level correlations
run_level_cor <- individual_blocks %>%
  group_by(condition, run_block) %>%
  summarise(
    n        = n(),
    r        = cor(political_ideology, final_score_resolved, use = "complete.obs"),
    fisher_z = atanh(r),
    .groups  = "drop"
  )

cat("\n--- Table G4. Run-level ideology-opinion correlations ---\n")
print(run_level_cor, n = Inf, width = Inf)

cat("\n--- Summary of run-level correlations ---\n")
run_cor_summary <- run_level_cor %>%
  group_by(condition) %>%
  summarise(
    n_runs                  = n(),
    mean_r                  = mean(r, na.rm = TRUE),
    sd_r                    = sd(r, na.rm = TRUE),
    mean_fisher_z           = mean(fisher_z, na.rm = TRUE),
    sd_fisher_z             = sd(fisher_z, na.rm = TRUE),
    back_transformed_mean_r = tanh(mean(fisher_z, na.rm = TRUE)),
    .groups = "drop"
  )
print(run_cor_summary, width = Inf)

# G5: between-condition Welch test on Fisher z
cat("\n--- Table G5. Welch test on run-level Fisher-z correlations ---\n")
fisher_z_test <- welch_test(run_level_cor, "fisher_z")
print(fisher_z_test, width = Inf)


# -------------------------------------------------------------------------
# 7. Tables G6.1, G6.2. Baseline-adjusted DRI models (ANCOVA with HC3)
#    Reviewer point 1.
# -------------------------------------------------------------------------
cat("\n=========================================================\n")
cat(" 7. TABLES G6.1-G6.2 - Baseline-adjusted DRI (ANCOVA, HC3)\n")
cat("=========================================================\n")

# G6.2 first: baseline distributions and common support
cat("\n--- Table G6.2. Baseline User-DRI by condition ---\n")
baseline_desc <- group %>%
  group_by(condition) %>%
  summarise(
    n    = sum(!is.na(user_dri_pre)),
    mean = mean(user_dri_pre, na.rm = TRUE),
    sd   = sd(user_dri_pre,   na.rm = TRUE),
    min  = min(user_dri_pre,  na.rm = TRUE),
    max  = max(user_dri_pre,  na.rm = TRUE),
    .groups = "drop"
  )
print(baseline_desc, width = Inf)

overlap_low  <- max(baseline_desc$min)
overlap_high <- min(baseline_desc$max)
cat("\nBaseline overlap region: [",
    round(overlap_low, 3), ", ", round(overlap_high, 3), "]\n", sep = "")
if (overlap_low >= overlap_high) {
  cat("The baseline distributions do not overlap at all.\n")
} else {
  cat("The distributions overlap only in this narrow band; adjustment is\n",
      "therefore partly extrapolation.\n", sep = "")
}

# G6.1: post-DRI on condition + pre-DRI, HC3 standard errors.

fit_ancova <- lm(user_dri_post ~ condition + user_dri_pre, data = group)
ancova_tbl <- hc3_table(fit_ancova, "post ~ condition + pre (HC3)")

cat("\n--- Table G6.1. Post-deliberation User-DRI on condition and baseline ---\n")
print(ancova_tbl, width = Inf)

fit_delta <- lm(user_dri_delta ~ condition + user_dri_pre, data = group)
delta_tbl <- hc3_table(fit_delta, "delta ~ condition + pre (HC3)")

cat("\n--- Equivalent change-score specification (verification) ---\n")
print(delta_tbl, width = Inf)

cond_post  <- ancova_tbl %>% filter(grepl("condition", term))
cond_delta <- delta_tbl  %>% filter(grepl("condition", term))
cat("\nCondition estimates identical across specifications: ",
    isTRUE(all.equal(cond_post$estimate, cond_delta$estimate)), "\n", sep = "")
cat("Covariate coefficients differ by exactly 1: ",
    isTRUE(all.equal(
      unname(ancova_tbl$estimate[ancova_tbl$term == "user_dri_pre"] -
               delta_tbl$estimate[delta_tbl$term == "user_dri_pre"]),
      1)),
    "\n", sep = "")

unadj_delta <- main_results %>%
  filter(variable == "user_dri_delta") %>%
  pull(diff_structured_minus_direct)

cat("\nUnadjusted difference in change scores : ", round(unadj_delta, 3), "\n", sep = "")
cat("Baseline-adjusted architecture effect  : ",
    round(cond_post$estimate, 3), "\n", sep = "")
cat("Adjustment INCREASES the estimate. Because direct-deliberation runs\n",
    "begin lower they have more headroom, so raw change scores are if\n",
    "anything mechanically favourable to the direct condition and the\n",
    "unadjusted comparison is conservative.\n", sep = "")


# -------------------------------------------------------------------------
# 8. Table G7. Assumption-free tests and Holm correction
#    Reviewer point 3.
# -------------------------------------------------------------------------
cat("\n=========================================================\n")
cat(" 8. TABLE G7 - Wilcoxon, exact permutation, Holm correction\n")
cat("=========================================================\n")


principal_vars <- c("aqua_score", "user_dri_delta", "opinion_delta", "polarization_delta")

perm_results   <- map_dfr(principal_vars, ~exact_perm_test(group, .x))
wilcox_results <- map_dfr(principal_vars, ~wilcox_test_tbl(group, .x))

robustness <- main_results %>%
  filter(variable %in% principal_vars) %>%
  select(variable, mean_direct, mean_structured,
         diff_structured_minus_direct, df, welch_p = p) %>%
  left_join(wilcox_results, by = "variable") %>%
  left_join(perm_results %>% select(variable, n_permutations, perm_p = perm_p_twosided),
            by = "variable") %>%
  mutate(
    welch_p_holm = p.adjust(welch_p, method = "holm"),
    perm_p_holm  = p.adjust(perm_p,  method = "holm")
  ) %>%
  # keep the reporting order used in the paper
  mutate(variable = factor(variable, levels = principal_vars)) %>%
  arrange(variable)

print(robustness, n = Inf, width = Inf)

cat("\nNumber of enumerated allocations per test: ",
    unique(perm_results$n_permutations), "\n", sep = "")
cat("Expected for choose(20, 10): ", choose(20, 10), "\n", sep = "")

cat("\nDecision rule: where the parametric and assumption-free tests disagree,\n",
    "the assumption-free result is treated as authoritative.\n", sep = "")

disagree <- robustness %>%
  filter((welch_p < .05 & perm_p >= .05) | (welch_p >= .05 & perm_p < .05))
if (nrow(disagree)) {
  cat("\nOutcomes where the two procedures disagree at alpha = .05:\n")
  print(disagree %>% select(variable, welch_p, perm_p, wilcoxon_p, perm_p_holm),
        width = Inf)
} else {
  cat("\nNo outcome shows parametric / assumption-free disagreement.\n")
}


# -------------------------------------------------------------------------
# 9. Tables G8.1, G8.2, G8.3. Transcript length and AQuA
#    Reviewer point 2.
# -------------------------------------------------------------------------
cat("\n=========================================================\n")
cat(" 9. TABLES G8.1-G8.3 - Transcript length and AQuA\n")
cat("=========================================================\n")

# The number of AQuA scoring units equals the number of scored turns.
if (!"n_scored_comments" %in% names(group)) {
  group <- group %>% mutate(n_scored_comments = total_turns)
  cat("\nNote: n_scored_comments not present; using total_turns, since each\n",
      "agent turn is one AQuA scoring unit.\n", sep = "")
}

length_measures <- c("total_turns", "total_words", "n_scored_comments")

# G8.1: common support
cat("\n--- Table G8.1. Common support in transcript length ---\n")
common_support <- map_dfr(length_measures, function(v) {
  rng <- group %>%
    group_by(condition) %>%
    summarise(min = min(.data[[v]], na.rm = TRUE),
              max = max(.data[[v]], na.rm = TRUE), .groups = "drop")
  d_min <- rng$min[rng$condition == "direct_deliberation"]
  d_max <- rng$max[rng$condition == "direct_deliberation"]
  s_min <- rng$min[rng$condition == "structured_pipeline"]
  s_max <- rng$max[rng$condition == "structured_pipeline"]
  lo <- max(d_min, s_min); hi <- min(d_max, s_max)
  tibble(measure = v,
         direct_min = d_min, direct_max = d_max,
         struct_min = s_min, struct_max = s_max,
         overlap = if (lo > hi) "None" else paste0("[", lo, ", ", hi, "]"))
})
print(common_support, width = Inf)
cat("\nWhere overlap is None, any statistical adjustment for length is\n",
    "extrapolation rather than interpolation.\n", sep = "")

# G8.2: within-condition length-AQuA association (Spearman).

cat("\n--- Table G8.2. Within-condition length-AQuA association (Spearman) ---\n")
length_cor <- expand_grid(cond = lvls, measure = length_measures) %>%
  pmap_dfr(function(cond, measure) {
    d <- group %>% filter(condition == cond)
    x <- d[[measure]]; y <- d$aqua_score
    if (length(unique(x[!is.na(x)])) < 2) {
      tibble(condition = cond, measure = measure, n = sum(!is.na(y)),
             spearman_rho = NA_real_, p = NA_real_, note = "not estimable (constant)")
    } else {
      ct <- suppressWarnings(cor.test(x, y, method = "spearman", exact = FALSE))
      tibble(condition = cond, measure = measure, n = sum(!is.na(x) & !is.na(y)),
             spearman_rho = unname(ct$estimate), p = ct$p.value, note = "")
    }
  })
print(length_cor, n = Inf, width = Inf)

est <- length_cor %>% filter(!is.na(spearman_rho))
cat("\nEstimable coefficients: ", nrow(est),
    "; all negative: ", all(est$spearman_rho < 0), "\n", sep = "")
cat("A volume-driven artefact would require a POSITIVE association.\n")

# G8.3: AQuA on condition with and without length covariates (HC3).

group <- group %>%
  group_by(condition) %>%
  mutate(
    log_words_c  = log(total_words)    - mean(log(total_words),    na.rm = TRUE),
    n_scored_c   = n_scored_comments   - mean(n_scored_comments,   na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(log_words = log(total_words))

length_models <- list(
  "unadjusted"                 = lm(aqua_score ~ condition, data = group),
  "+ log total words"          = lm(aqua_score ~ condition + log_words, data = group),
  "+ scored comments"          = lm(aqua_score ~ condition + n_scored_comments, data = group),
  "+ within-cond log words"    = lm(aqua_score ~ condition + log_words_c, data = group),
  "+ within-cond comments"     = lm(aqua_score ~ condition + n_scored_c, data = group)
)

cat("\n--- Table G8.3. AQuA on condition with and without length covariates (HC3) ---\n")
length_model_tbl <- imap_dfr(length_models, ~hc3_table(.x, .y)) %>%
  filter(term != "(Intercept)")
print(length_model_tbl, n = Inf, width = Inf)

b_unadj <- length_model_tbl %>%
  filter(model == "unadjusted", grepl("condition", term)) %>% pull(estimate)
b_adj <- length_model_tbl %>%
  filter(model %in% c("+ log total words", "+ scored comments"),
         grepl("condition", term)) %>% pull(estimate)

cat("\nUnadjusted condition coefficient: ", round(b_unadj, 3), "\n", sep = "")
cat("Length-adjusted coefficients    : ", paste(round(b_adj, 3), collapse = ", "), "\n", sep = "")
cat("Adjustment does not attenuate the estimate toward zero: ",
    all(b_adj > b_unadj), "\n", sep = "")


# -------------------------------------------------------------------------
# 10. Stability statistics reported in Section 4.2.1
# -------------------------------------------------------------------------
cat("\n=========================================================\n")
cat(" 10. Cross-run stability (coefficient of variation)\n")
cat("=========================================================\n")

stability <- group %>%
  group_by(condition) %>%
  summarise(
    aqua_mean          = mean(aqua_score, na.rm = TRUE),
    aqua_sd            = sd(aqua_score,   na.rm = TRUE),
    aqua_cv_pct        = 100 * sd(aqua_score, na.rm = TRUE) / mean(aqua_score, na.rm = TRUE),
    dri_delta_positive = sum(user_dri_delta > 0, na.rm = TRUE),
    n_runs             = n(),
    .groups = "drop"
  )
print(stability, width = Inf)


# -------------------------------------------------------------------------
# 11. Export results and session information
# -------------------------------------------------------------------------
cat("\n=========================================================\n")
cat(" 11. EXPORT\n")
cat("=========================================================\n")

write.csv(main_results,     "G1_run_level_welch_tests.csv",             row.names = FALSE)
write.csv(dri_paired,       "G2_dri_paired_pre_post_tests.csv",         row.names = FALSE)
write.csv(pooled_cor,       "G3_pooled_ideology_correlations.csv",      row.names = FALSE)
write.csv(run_level_cor,    "G4_run_level_ideology_correlations.csv",   row.names = FALSE)
write.csv(fisher_z_test,    "G5_fisher_z_between_condition.csv",        row.names = FALSE)
write.csv(ancova_tbl,       "G6_1_ancova_hc3.csv",                      row.names = FALSE)
write.csv(baseline_desc,    "G6_2_baseline_dri_by_condition.csv",       row.names = FALSE)
write.csv(robustness,       "G7_assumption_free_and_holm.csv",          row.names = FALSE)
write.csv(common_support,   "G8_1_common_support.csv",                  row.names = FALSE)
write.csv(length_cor,       "G8_2_length_aqua_spearman.csv",            row.names = FALSE)
write.csv(length_model_tbl, "G8_3_length_adjusted_models.csv",          row.names = FALSE)
write.csv(stability,        "S4_2_1_stability_statistics.csv",          row.names = FALSE)

cat("\nCSV result files written to: ", getwd(), "\n", sep = "")

cat("\n--- Session information (reproducibility record) ---\n")
print(sessionInfo())

writeLines(capture.output(sessionInfo()), "sessionInfo.txt")

cat("\nDone.\n")
