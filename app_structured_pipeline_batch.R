# ============================================================
# Deliberation Simulator — Multi-Agent Shiny App (White UI)
# ============================================================


library(shiny)
library(httr2)
library(jsonlite)
library(shinyjs)
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(gridExtra)
library(grid)
library(tools)
library(DT)

HAS_PDFTOOLS   <- requireNamespace("pdftools", quietly = TRUE)
HAS_RMARKDOWN  <- requireNamespace("rmarkdown", quietly = TRUE)

# --- safe null-or-default that handles data frames ---
`%||%` <- function(a, b) {
  if (is.data.frame(a)) {
    if (nrow(a) > 0) a else b
  } else if (!is.null(a) && length(a) > 0 && !anyNA(a[1])) {
    a
  } else {
    b
  }
}

# ============================================================
# CONSTANTS
# ============================================================
DEFAULT_MODELS <- c(
  "openai/gpt-4o",
  "openai/gpt-4o-mini",
  "anthropic/claude-sonnet-4-5",
  "anthropic/claude-3-5-haiku",
  "google/gemini-2.0-flash-001",
  "google/gemini-pro-1.5",
  "meta-llama/llama-3.1-70b-instruct"
)

RECOMMENDED_MODELS <- c(
  "openai/gpt-4o",
  "openai/gpt-4o-mini",
  "anthropic/claude-sonnet-4-5",
  "anthropic/claude-3-5-haiku",
  "google/gemini-2.0-flash-001"
)

AQUA_DIMENSIONS <- list(
  Rationality = c("Relevance", "Fact", "Opinion", "Justification", "Solution Proposals", "Additional Knowledge", "Question"),
  Reciprocity = c("Referencing Users", "Referencing Medium", "Referencing Contents", "Referencing Personal", "Referencing Format"),
  Civility = c("Polite Form of Address", "Respect", "Screaming", "Vulgar", "Insults", "Sarcasm", "Discrimination"),
  Narrative = c("Storytelling")
)

AQUA_FORMULA_HTML <- paste0(
  "<div style='font-size:16px;line-height:1.8;'>",
  "<div><strong>Step 1 — LLM dimension scoring:</strong> f<sub>k</sub>(x) ∈ {0,1} for each of K=20 deliberative aspects, scored by LLM using exact definitions from Behrendt et al. (2024)</div>",
  "<div><strong>Step 2 — Weighted raw score:</strong> s(x) = Σ<sub>k=1</sub><sup>K</sup> w<sub>k</sub> f<sub>k</sub>(x), where w<sub>k</sub> are the published regression weights from Table 1</div>",
  "<div><strong>Step 3 — Normalized AQuA score:</strong> s<sub>AQuA</sub>(x) = 5 × (s(x) − s<sub>min</sub>) / (s<sub>max</sub> − s<sub>min</sub>)</div>",
  "<div class='small-note' style='margin-top:8px;'>",
  "Weights are taken verbatim from Behrendt et al. (2024) Table 1 — they reflect the correlation between each expert-coded aspect and non-expert perceptions of deliberativeness. ",
  "The original paper uses fine-tuned German-language adapter models (on top of a sentence transformer) to predict each f<sub>k</sub>(x). ",
  "This implementation uses the same LLM already running the simulation to apply the exact same binary question for each dimension, ",
  "with temperature=0 for deterministic scoring. The formula and weights are identical to the paper.",
  "</div>",
  "</div>"
)

STEPS <- list(
  list(id = 0,  label = "Setup",                icon = "\u2699"),
  list(id = 1,  label = "Generate Personas",    icon = "\U0001F465"),
  list(id = 2,  label = "Validate Personas",    icon = "\U0001F50D"),
  list(id = 3,  label = "Design Integrity",     icon = "\U0001F9ED"),
  list(id = 4,  label = "Assign Agents",        icon = "\U0001F916"),
  list(id = 5,  label = "Split Groups",         icon = "\u2696"),
  list(id = 6,  label = "Values Round",         icon = "\U0001F4A1"),
  list(id = 7,  label = "Master Values",        icon = "\U0001F4CB"),
  list(id = 8,  label = "Reshuffle Groups",     icon = "\U0001F500"),
  list(id = 9,  label = "Options Round",        icon = "\U0001F5C2"),
  list(id = 10, label = "Master Options",       icon = "\U0001F4CC"),
  list(id = 11, label = "Reshuffle Groups",     icon = "\U0001F500"),
  list(id = 12, label = "Evaluation Round",     icon = "\U0001F4CA"),
  list(id = 13, label = "Individual Rankings",  icon = "\U0001F3AF"),
  list(id = 14, label = "Final Recommendation", icon = "\U0001F3DB"),
  list(id = 15, label = "Quality Assessment",   icon = "\u2705")
)

QUALITY_DIMS <- c(
  "solid_information_base", "key_values", "broad_range_of_solutions",
  "pros_cons_tradeoffs", "participant_representation", "group_dynamics",
  "individual_dynamics", "quality_of_judgments"
)

QUALITY_LABELS <- c(
  "Information Base", "Key Values", "Range of Solutions",
  "Pros/Cons/Tradeoffs", "Representation", "Group Dynamics",
  "Individual Dynamics", "Quality of Judgments"
)

# ============================================================
# WHITE THEME
# ============================================================
APP_CSS <- "
body{background:#f8f8fb;color:#1f2330;font-family:Georgia,'Times New Roman',serif;margin:0;padding:0}
.container-fluid{padding:0!important}
#sidebar{position:fixed;top:0;left:0;width:245px;height:100vh;background:#ffffff;border-right:1px solid #dde1ea;overflow-y:auto;z-index:100}
#main-content{margin-left:245px;padding:32px 42px;max-width:1220px}
.sb-header{padding:22px 20px 18px;border-bottom:1px solid #e5e8ef;margin-bottom:6px}
.sb-sub{font-size:10px;letter-spacing:3px;color:#7d8596;text-transform:uppercase;margin-bottom:4px}
.sb-title{font-size:18px;font-weight:700;color:#946f00;margin:0}
.sb-section{font-size:9px;letter-spacing:2px;color:#7d8596;text-transform:uppercase;padding:14px 20px 4px}
.step-item,.out-item{display:flex;align-items:center;gap:10px;padding:9px 20px;border-left:3px solid transparent;transition:all .12s}
.step-item.done,.out-item{cursor:pointer}
.step-item.active{background:#fff8e1;border-left-color:#c8a12d}
.out-item.active{background:#eef7f2;border-left-color:#32936f}
.step-item.pending,.out-item.locked{opacity:.38;cursor:default;pointer-events:none}
.step-icon{font-size:13px;flex-shrink:0}
.step-label{font-size:12px;color:#4b5567;line-height:1.3;flex:1}
.step-item.active .step-label{color:#946f00}
.out-item.active .step-label{color:#237455}
.step-check{color:#1a8f5a;font-size:10px;margin-left:auto}
.api-bar{background:#ffffff;border:1px solid #dde1ea;border-radius:14px;padding:18px 24px;margin-bottom:22px;display:flex;gap:16px;align-items:flex-end;flex-wrap:wrap}
.form-control{background:#ffffff!important;border:1px solid #cfd6e1!important;border-radius:10px!important;color:#1f2330!important;font-size:14px!important;padding:12px 16px!important}
.form-control:focus{outline:1px solid rgba(200,161,45,.25);box-shadow:none!important}
label{color:#946f00!important;font-size:13px!important;margin-bottom:8px!important}
.control-label{font-size:10px!important;letter-spacing:2px!important;color:#7d8596!important;text-transform:uppercase!important}
textarea.form-control{min-height:120px;resize:vertical;line-height:1.7}
.step-num{font-size:10px;letter-spacing:3px;color:#7d8596;text-transform:uppercase;margin-bottom:6px}
.step-title{font-size:28px;font-weight:700;color:#946f00;margin-bottom:8px}
.step-desc{color:#5f6778;font-size:15px;line-height:1.6;margin-bottom:16px}
.out-title{font-size:24px;font-weight:700;color:#237455;margin-bottom:8px}
.out-desc{color:#5f6778;font-size:14px;line-height:1.6;margin-bottom:24px}
.result-box,.report-box{background:#ffffff;border:1px solid #dde1ea;border-radius:14px;padding:22px;margin-bottom:22px;overflow-y:auto;font-size:14px;line-height:1.8;color:#1f2330;white-space:pre-wrap;word-wrap:break-word}
.result-box{max-height:520px}
.report-box{max-height:760px}
.btn-run,.btn-next,.btn-rerun,.btn-ghost,.btn-start,.btn-output{border-radius:10px;padding:12px 24px;font-size:14px;cursor:pointer;font-family:Georgia,serif}
.btn-run,.btn-start{background:#c8a12d;color:#ffffff;border:none;font-weight:700}
.btn-next{background:#2f855a;color:#ffffff;border:1px solid #2f855a;font-weight:700}
.btn-rerun{background:#ffffff;color:#7a5a00;border:1px solid #d5c27a}
.btn-ghost{background:#ffffff;color:#677285;border:1px solid #cfd6e1}
.btn-output{background:#eef7f2;color:#237455;border:1px solid #b8dccd}
.error-box{background:#fff3f2;border:1px solid #e3b5b2;border-radius:10px;padding:14px 18px;color:#b33a2f;font-size:13px;margin-top:16px}
.info-box{background:#eff8f2;border:1px solid #c3e1d0;border-radius:10px;padding:14px 18px;color:#237455;font-size:13px;margin-bottom:16px}
.small-note{color:#6e7789;font-size:12px;line-height:1.5;margin-top:8px}
.plot-wrap{background:#ffffff;border:1px solid #dde1ea;border-radius:14px;padding:16px;margin-bottom:24px}
.kpi-grid{display:grid;grid-template-columns:repeat(4,minmax(140px,1fr));gap:12px;margin-bottom:18px}
.kpi{background:#ffffff;border:1px solid #dde1ea;border-radius:12px;padding:14px}
.kpi-value{font-size:24px;color:#946f00;font-weight:700}
.kpi-label{font-size:11px;color:#7d8596;text-transform:uppercase;letter-spacing:1px}
.missing{border:2px solid #d4735e!important;box-shadow:0 0 0 2px rgba(212,115,94,.12)!important}
hr{border:none;border-top:1px solid #e5e8ef}
::-webkit-scrollbar{width:8px}
::-webkit-scrollbar-track{background:#f1f3f8}
::-webkit-scrollbar-thumb{background:#c6cedb;border-radius:4px}
.demo-section{background:#ffffff;border:1px solid #dde1ea;border-radius:14px;padding:20px 24px;margin-bottom:18px}
.demo-section h4{color:#946f00;font-size:14px;margin:0 0 12px 0;font-weight:700}
.demo-grid{display:grid;grid-template-columns:repeat(auto-fill,minmax(120px,1fr));gap:8px 16px}
.demo-grid .form-group{margin-bottom:4px}
.demo-grid .control-label{font-size:9px!important}
.demo-grid .form-control{padding:8px 10px!important;font-size:13px!important}
.demo-warn{background:#fff8e1;border:1px solid #e5d7a0;border-radius:8px;padding:10px 14px;color:#7a5a00;font-size:12px;margin-top:8px}
.qa-card{background:#ffffff;border:1px solid #dde1ea;border-radius:12px;padding:16px 20px;margin-bottom:12px}
.qa-card h5{color:#946f00;margin:0 0 4px 0;font-size:14px}
.qa-card .qa-score{font-size:22px;font-weight:700;color:#237455;float:right;margin-top:-24px}
.qa-card .qa-notes{color:#5f6778;font-size:13px;line-height:1.6;margin-top:6px}
.dl-row{display:flex;gap:12px;flex-wrap:wrap;margin-top:16px;margin-bottom:16px}
.survey-section{background:#faf8f0;border:1px solid #e5d7a0;border-radius:14px;padding:20px 24px;margin-bottom:18px}
.survey-section h4{color:#7a5a00;font-size:14px;margin:0 0 8px 0;font-weight:700}
"

# ============================================================
# HELPERS
# ============================================================
clean_api_key <- function(x) {
  x <- trimws(x %||% "")
  gsub("[\r\n\t]", "", x)
}

scalar_text <- function(x) {
  paste(x %||% "", collapse = " ")
}

has_text <- function(x) {
  nzchar(trimws(scalar_text(x)))
}

truncate_text <- function(x, max_chars = 18000) {
  x <- scalar_text(x)
  if (nchar(x) <= max_chars) return(x)
  paste0(substr(x, 1, max_chars), "\n\n[TRUNCATED]")
}

extract_text_from_file <- function(path, filename) {
  ext <- tolower(file_ext(filename %||% path))
  out <- tryCatch({
    if (ext %in% c("txt", "md", "csv", "tsv", "json", "r", "py", "log")) {
      paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    } else if (ext == "pdf" && HAS_PDFTOOLS) {
      paste(pdftools::pdf_text(path), collapse = "\n\n")
    } else {
      paste0("[Unsupported file type for extraction: ", ext, "]")
    }
  }, error = function(e) {
    paste0("[Extraction failed for ", filename, ": ", conditionMessage(e), "]")
  })
  truncate_text(out, 16000)
}

build_evidence_block <- function(info_base, evidence_text, evidence_names) {
  parts <- c()
  if (has_text(info_base)) parts <- c(parts, paste0("USER BACKGROUND:\n", scalar_text(info_base)))
  if (has_text(evidence_text)) {
    nm <- if (length(evidence_names) > 0) paste(evidence_names, collapse = "; ") else "Uploaded evidence"
    parts <- c(parts, paste0("UPLOADED EVIDENCE [", nm, "]:\n", scalar_text(evidence_text)))
  }
  if (!length(parts)) return("No evidence supplied.")
  paste(parts, collapse = "\n\n")
}

split_groups_evenly <- function(n_people, n_groups) {
  ids <- seq_len(n_people)
  groups <- split(ids, rep(seq_len(n_groups), length.out = n_people))
  names(groups) <- paste0("Group ", seq_len(n_groups))
  groups
}

extract_json_text <- function(txt) {
  txt <- paste(txt %||% "", collapse = " ")
  txt <- gsub("```json", "", txt, fixed = TRUE)
  txt <- gsub("```", "", txt, fixed = TRUE)
  txt <- trimws(txt)
  if (grepl("###JSON_START###", txt, fixed = TRUE) && grepl("###JSON_END###", txt, fixed = TRUE)) {
    start <- regexpr("###JSON_START###", txt, fixed = TRUE)
    end <- regexpr("###JSON_END###", txt, fixed = TRUE)
    txt <- substr(txt, start + attr(start, "match.length"), end - 1)
    txt <- trimws(txt)
  }
  start <- regexpr("\\{", txt)
  if (start[1] == -1) stop("No JSON object found in model output.")
  txt <- substr(txt, start[1], nchar(txt))
  ends <- gregexpr("\\}", txt)[[1]]
  if (length(ends) >= 1 && ends[1] != -1) txt <- substr(txt, 1, max(ends))
  txt
}

salvage_personas_json <- function(txt) {
  loc <- regexpr('"personas"\\s*:\\s*\\[', txt, perl = TRUE)
  if (loc[1] == -1) return(txt)
  
  start <- loc[1] + attr(loc, "match.length")
  s <- substr(txt, start, nchar(txt))
  chars <- strsplit(s, "")[[1]]
  
  in_string <- FALSE
  escaped <- FALSE
  depth <- 0L
  obj_start <- NA_integer_
  objs <- character(0)
  
  for (i in seq_along(chars)) {
    ch <- chars[i]
    
    if (escaped) {
      escaped <- FALSE
      next
    }
    
    if (ch == "\\") {
      escaped <- TRUE
      next
    }
    
    if (ch == '"') {
      in_string <- !in_string
      next
    }
    
    if (in_string) next
    
    if (ch == "{") {
      if (depth == 0L) obj_start <- i
      depth <- depth + 1L
    } else if (ch == "}") {
      if (depth > 0L) depth <- depth - 1L
      if (depth == 0L && !is.na(obj_start)) {
        objs <- c(objs, paste(chars[obj_start:i], collapse = ""))
        obj_start <- NA_integer_
      }
    } else if (ch == "]" && depth == 0L) {
      break
    }
  }
  
  prefix <- substr(txt, 1, start - 1)
  paste0(prefix, "\n", paste(objs, collapse = ",\n"), "\n]}")
}
coerce_json <- function(txt) {
  json_txt <- extract_json_text(txt)
  result <- tryCatch(fromJSON(json_txt, simplifyVector = FALSE), error = function(e) NULL)
  if (!is.null(result)) return(result)
  repaired <- robust_json_repair(json_txt)
  result2 <- tryCatch(fromJSON(repaired, simplifyVector = FALSE), error = function(e) NULL)
  if (!is.null(result2)) return(result2)
  result3 <- tryCatch({
    salvaged <- salvage_json_array(json_txt)
    fromJSON(salvaged, simplifyVector = FALSE)
  }, error = function(e) NULL)
  if (!is.null(result3)) return(result3)
  result4 <- tryCatch({
    salvaged <- salvage_personas_json(json_txt)
    fromJSON(salvaged, simplifyVector = FALSE)
  }, error = function(e) NULL)
  if (!is.null(result4)) return(result4)
  stop(paste0("JSON parse failed even after repair.\nFirst 500 chars: ", substr(json_txt, 1, 500)))
}

# Find the last complete {...} objects inside a "responses" array and close everything
salvage_json_array <- function(txt) {
  # Strategy: find all positions of }, try to close the JSON after each one (from last to first)
  brace_positions <- gregexpr("\\}", txt)[[1]]
  if (length(brace_positions) == 0 || brace_positions[1] == -1) return(txt)
  
  # Try from the last } backwards
  for (i in rev(seq_along(brace_positions))) {
    candidate <- substr(txt, 1, brace_positions[i])
    
    # Close any open [ and {
    ob <- nchar(gsub("[^{]", "", candidate))
    cb <- nchar(gsub("[^}]", "", candidate))
    osq <- nchar(gsub("[^\\[]", "", candidate))
    csq <- nchar(gsub("[^\\]]", "", candidate))
    
    closing <- ""
    if (osq > csq) closing <- paste0(closing, paste(rep("]", osq - csq), collapse = ""))
    if (ob > cb)   closing <- paste0(closing, paste(rep("}", ob - cb), collapse = ""))
    
    attempt <- paste0(candidate, closing)
    parsed <- tryCatch(fromJSON(attempt, simplifyVector = FALSE), error = function(e) NULL)
    if (!is.null(parsed)) return(attempt)
  }
  txt
}

# Robust repair: trim back to last complete key:value pair, then close brackets
robust_json_repair <- function(txt) {
  # Step 1: trim everything after the last complete }, ] or quoted string value
  # Find last occurrence of "},  or "],  or "} or "]
  last_complete <- max(
    regexpr_last('\\},?\\s*', txt),
    regexpr_last('\\],?\\s*', txt),
    regexpr_last('"\\s*,?\\s*', txt),
    na.rm = TRUE
  )
  
  if (!is.na(last_complete) && last_complete > 10) {
    # Find the actual end of the match
    txt_trimmed <- substr(txt, 1, last_complete)
    # Remove trailing comma
    txt_trimmed <- sub(",\\s*$", "", txt_trimmed, perl = TRUE)
  } else {
    txt_trimmed <- txt
  }
  
  # Step 2: close brackets
  ob <- nchar(gsub("[^{]", "", txt_trimmed))
  cb <- nchar(gsub("[^}]", "", txt_trimmed))
  osq <- nchar(gsub("[^\\[]", "", txt_trimmed))
  csq <- nchar(gsub("[^\\]]", "", txt_trimmed))
  
  closing <- ""
  if (osq > csq) closing <- paste0(closing, paste(rep("]", osq - csq), collapse = ""))
  if (ob > cb)   closing <- paste0(closing, paste(rep("}", ob - cb), collapse = ""))
  
  paste0(txt_trimmed, closing)
}

# Helper: find last position of a pattern
regexpr_last <- function(pattern, txt) {
  m <- gregexpr(pattern, txt, perl = TRUE)[[1]]
  if (length(m) == 0 || m[1] == -1) return(NA_integer_)
  pos <- m[length(m)]
  len <- attr(m, "match.length")[length(m)]
  pos + len - 1L
}

safe_json <- function(x) {
  toJSON(x, auto_unbox = TRUE, pretty = TRUE, null = "null")
}

make_batch_sizes <- function(n, max_batch = 10L) {
  n <- as.integer(n %||% 0)
  max_batch <- as.integer(max_batch %||% 10)
  if (!is.finite(n) || n <= 0) return(integer())
  if (n <= max_batch) return(as.integer(n))
  k <- ceiling(n / max_batch)
  base <- rep(floor(n / k), k)
  remainder <- n - sum(base)
  if (remainder > 0) base[seq_len(remainder)] <- base[seq_len(remainder)] + 1L
  as.integer(base)
}

rebalance_persona_ideology <- function(df, input) {
  if (is.null(df) || nrow(df) == 0 || !"political_ideology" %in% names(df)) return(df)
  target_pct <- c(input$demo_ideo_ext_left %||% 10, input$demo_ideo_left %||% 20,
                  input$demo_ideo_centre %||% 40, input$demo_ideo_right %||% 20,
                  input$demo_ideo_ext_right %||% 10)
  target_pct[!is.finite(target_pct)] <- 0
  if (sum(target_pct) <= 0) return(df)
  target_counts <- floor(nrow(df) * target_pct / sum(target_pct))
  remainder <- nrow(df) - sum(target_counts)
  if (remainder > 0) {
    frac <- nrow(df) * target_pct / sum(target_pct) - target_counts
    add_idx <- order(frac, decreasing = TRUE)[seq_len(remainder)]
    target_counts[add_idx] <- target_counts[add_idx] + 1L
  }
  score_map <- c(1, 3, 5, 7, 9)
  ord <- order(df$political_ideology, na.last = TRUE)
  if (anyNA(df$political_ideology)) {
    fill_vals <- rep(score_map, times = pmax(target_counts, 0))
    fill_vals <- fill_vals[seq_len(min(length(fill_vals), length(ord)))]
    df$political_ideology[ord[seq_along(fill_vals)]] <- fill_vals
    ord <- order(df$political_ideology, na.last = TRUE)
  }
  new_scores <- numeric(0)
  for (i in seq_along(target_counts)) {
    if (target_counts[i] > 0) new_scores <- c(new_scores, rep(score_map[i], target_counts[i]))
  }
  if (length(new_scores) < nrow(df)) new_scores <- c(new_scores, rep(5, nrow(df) - length(new_scores)))
  df$political_ideology[ord[seq_len(nrow(df))]] <- new_scores[seq_len(nrow(df))]
  df
}

build_persona_json_prompt <- function(n_needed) {
  paste0('{"personas":[', paste(rep('{"id":"P1","name":"string","age":25,"gender":"string","education":"string","political_ideology":5,"income":"string","background":"string","community":"string","settlement":"urban","interests":"string","values":"string","blind_spots":"string","initial_position_text":"string","initial_score":3}', n_needed), collapse = ','), ']}')
}

extract_ideology_numeric <- function(x) {
  x_chr <- tolower(trimws(scalar_text(x)))
  num <- suppressWarnings(as.numeric(gsub("[^0-9.-]", "", x_chr)))
  if (is.finite(num)) return(max(0, min(10, round(num))))
  if (grepl("extreme right|far right", x_chr)) return(9)
  if (grepl("right", x_chr)) return(7)
  if (grepl("centre|center|moderate", x_chr)) return(5)
  if (grepl("left", x_chr) && !grepl("extreme|far", x_chr)) return(3)
  if (grepl("extreme left|far left", x_chr)) return(1)
  NA_real_
}

normalize_persona_df <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(df)
  required_cols <- c("id", "name", "age", "gender", "education", "political_ideology",
                     "income", "background", "community", "settlement", "interests",
                     "values", "blind_spots", "initial_position_text", "initial_score")
  for (nm in required_cols) if (!nm %in% names(df)) df[[nm]] <- NA
  df <- df[, required_cols, drop = FALSE]
  df$age <- suppressWarnings(as.numeric(df$age))
  df$political_ideology <- vapply(df$political_ideology, extract_ideology_numeric, numeric(1))
  df$initial_score <- suppressWarnings(as.numeric(df$initial_score))
  df$initial_score <- pmin(5, pmax(1, round(df$initial_score)))
  df$settlement <- ifelse(tolower(trimws(scalar_text_vec(df$settlement))) %in% c("urban", "rural"),
                          tolower(trimws(scalar_text_vec(df$settlement))), "urban")
  df$name <- scalar_text_vec(df$name)
  df$id <- ifelse(nzchar(trimws(scalar_text_vec(df$id))), scalar_text_vec(df$id), paste0("P", seq_len(nrow(df))))
  df
}

update_final_scores_from_turns <- function(personas_df, turns_df) {
  if (is.null(personas_df) || is.null(turns_df) || nrow(turns_df) == 0 || !"position_score" %in% names(turns_df)) return(personas_df)
  pos <- turns_df |> dplyr::filter(is.finite(position_score)) |> dplyr::group_by(speaker) |> dplyr::summarise(final_score = dplyr::last(position_score), .groups = "drop")
  if (nrow(pos) == 0) return(personas_df)
  out <- dplyr::left_join(personas_df, pos, by = c("name" = "speaker"), suffix = c("", "_turn"))
  if ("final_score_turn" %in% names(out)) {
    out$final_score <- ifelse(is.finite(out$final_score_turn), pmin(5, pmax(1, round(out$final_score_turn))), out$final_score)
    out$final_score_turn <- NULL
  }
  out
}

make_timestamp <- function() {
  format(Sys.time(), "%Y-%m-%d %H:%M:%S")
}

# ============================================================
# DEMOGRAPHIC COMPOSITION HELPERS
# ============================================================
build_demographic_instruction <- function(input) {
  parts <- c()
  pct_male <- input$demo_gender_male %||% 50
  parts <- c(parts, sprintf("Gender: ~%d%% male, ~%d%% female", pct_male, 100 - pct_male))
  parts <- c(parts, sprintf("Age: ~%d%% aged 16-25, ~%d%% aged 25-40, ~%d%% aged 40-65, ~%d%% aged 65+",
                            input$demo_age_16_25 %||% 25, input$demo_age_25_40 %||% 25, input$demo_age_40_65 %||% 25, input$demo_age_65p %||% 25))
  parts <- c(parts, sprintf(
    "Education: ~%d%% no formal education, ~%d%% primary school, ~%d%% apprenticeship/technical school, ~%d%% high school, ~%d%% BA/MA university",
    input$demo_edu_none %||% 5, input$demo_edu_primary %||% 10, input$demo_edu_apprentice %||% 20,
    input$demo_edu_highschool %||% 30, input$demo_edu_university %||% 35))
  pct_urban <- input$demo_urban %||% 50
  parts <- c(parts, sprintf("Settlement: ~%d%% urban, ~%d%% rural", pct_urban, 100 - pct_urban))
  parts <- c(parts, sprintf(
    "Ideology: ~%d%% extreme left, ~%d%% left, ~%d%% centre, ~%d%% right, ~%d%% extreme right",
    input$demo_ideo_ext_left %||% 10, input$demo_ideo_left %||% 20, input$demo_ideo_centre %||% 40,
    input$demo_ideo_right %||% 20, input$demo_ideo_ext_right %||% 10))
  paste0("DEMOGRAPHIC COMPOSITION TARGETS (approximate the percentages below as closely as possible given the number of participants):\n",
         paste(parts, collapse = "\n"))
}

# ============================================================
# DRI HELPERS
# ============================================================

safe_cor <- function(x, y, method = "spearman") {
  x <- as.numeric(x)
  y <- as.numeric(y)
  ok <- is.finite(x) & is.finite(y)
  x <- x[ok]; y <- y[ok]
  if (length(x) < 3 || length(unique(x)) < 2 || length(unique(y)) < 2) return(NA_real_)
  suppressWarnings(cor(x, y, method = method))
}

rank_to_borda <- function(ranks, option_ids) {
  # ranks: named numeric vector where names = option ids, values = rank (1 = best)
  # returns Borda-like preference score: higher = more preferred
  out <- setNames(rep(NA_real_, length(option_ids)), option_ids)
  n <- length(option_ids)
  for (oid in option_ids) {
    r <- ranks[[oid]]
    if (!is.null(r) && is.finite(r)) out[oid] <- n - r + 1
  }
  out
}

# ------------------------------------------------------------------
# STRICT / COMPLETE DRI RESPONSE HELPERS
# ------------------------------------------------------------------
normalize_dri_item_id <- function(id, valid_ids = character()) {
  id <- toupper(trimws(scalar_text(id %||% "")))
  id <- gsub("[^A-Z0-9]", "", id)
  if (!length(valid_ids) || id %in% valid_ids) return(id)
  # Accept C1 when survey uses C01, C001, etc.; same for P1/P01.
  m <- regexec("^([A-Z]+)0*([0-9]+)$", id, perl = TRUE)
  mm <- regmatches(id, m)[[1]]
  if (length(mm) == 3) {
    prefix <- mm[2]
    num <- suppressWarnings(as.integer(mm[3]))
    if (is.finite(num)) {
      candidates <- valid_ids[grepl(paste0("^", prefix, "0*", num, "$"), toupper(valid_ids))]
      if (length(candidates) == 1) return(candidates[1])
    }
  }
  id
}

dri_ids <- function(survey_obj) {
  list(
    c_ids = vapply(survey_obj$considerations, function(x) scalar_text(x$id), character(1)),
    p_ids = vapply(survey_obj$policy_options, function(x) scalar_text(x$id), character(1))
  )
}

extract_numeric_first <- function(x) {
  if (is.null(x)) return(NA_real_)
  if (is.numeric(x) || is.integer(x)) return(suppressWarnings(as.numeric(x)[1]))
  raw <- scalar_text(x)
  m <- regexpr("[-+]?[0-9]+(?:\\.[0-9]+)?", raw, perl = TRUE)
  if (m[1] < 0) return(NA_real_)
  suppressWarnings(as.numeric(regmatches(raw, m)[1]))
}

validate_dri_response <- function(resp, survey_obj, timing_expected = NULL) {
  ids <- dri_ids(survey_obj)
  c_ids <- ids$c_ids
  p_ids <- ids$p_ids
  
  c_seen <- setNames(rep(NA_real_, length(c_ids)), c_ids)
  p_seen <- setNames(rep(NA_real_, length(p_ids)), p_ids)
  
  if (!is.null(resp$considerations)) {
    for (x in resp$considerations) {
      id <- normalize_dri_item_id(x$id %||% "", c_ids)
      if (id %in% c_ids) {
        val <- extract_numeric_first(x$score %||% x$rating %||% x$value %||% x$answer_numeric %||% x$answer %||% NA)
        c_seen[id] <- val
      }
    }
  }
  
  if (!is.null(resp$preferences)) {
    for (x in resp$preferences) {
      id <- normalize_dri_item_id(x$id %||% "", p_ids)
      if (id %in% p_ids) {
        val <- extract_numeric_first(x$rank %||% x$ranking %||% x$value %||% x$answer_numeric %||% x$answer %||% NA)
        p_seen[id] <- val
      }
    }
  }
  
  c_min <- suppressWarnings(as.numeric(survey_obj$likert_scale$min %||% 1))
  c_max <- suppressWarnings(as.numeric(survey_obj$likert_scale$max %||% 5))
  if (!is.finite(c_min)) c_min <- 1
  if (!is.finite(c_max)) c_max <- 5
  
  c_complete <- all(is.finite(c_seen)) && all(c_seen >= c_min & c_seen <= c_max)
  p_complete <- all(is.finite(p_seen)) &&
    length(p_seen) == length(p_ids) &&
    setequal(as.integer(round(p_seen)), seq_along(p_ids)) &&
    !anyDuplicated(as.integer(round(p_seen)))
  
  list(
    ok = isTRUE(c_complete && p_complete),
    missing_considerations = names(c_seen)[!is.finite(c_seen) | c_seen < c_min | c_seen > c_max],
    missing_preferences = names(p_seen)[!is.finite(p_seen)],
    bad_preference_ranks = if (p_complete) character() else p_ids,
    n_considerations_expected = length(c_ids),
    n_considerations_valid = sum(is.finite(c_seen) & c_seen >= c_min & c_seen <= c_max),
    n_preferences_expected = length(p_ids),
    n_preferences_valid = sum(is.finite(p_seen)),
    consideration_scores = c_seen,
    preference_ranks = p_seen
  )
}

repair_dri_response_object <- function(resp, survey_obj, respondent = "unknown", timing = "pre") {
  ids <- dri_ids(survey_obj)
  v <- validate_dri_response(resp, survey_obj, timing)
  c_scores <- v$consideration_scores
  p_ranks <- v$preference_ranks
  
  # Keep only valid/normalised values; do NOT fabricate missing values.
  cons <- lapply(ids$c_ids, function(id) {
    val <- c_scores[[id]]
    list(id = id, score = if (is.finite(val)) as.numeric(val) else NA_real_)
  })
  prefs <- lapply(ids$p_ids, function(id) {
    val <- p_ranks[[id]]
    list(id = id, rank = if (is.finite(val)) as.numeric(round(val)) else NA_real_)
  })
  
  list(
    respondent = scalar_text(resp$respondent %||% respondent),
    timing = scalar_text(resp$timing %||% timing),
    considerations = cons,
    preferences = prefs
  )
}

compact_dri_survey_json <- function(survey_obj) {
  # Compact but complete: all IDs and texts are retained; non-essential fields are removed.
  compact <- list(
    title = scalar_text(survey_obj$title %||% "DRI survey"),
    likert_scale = survey_obj$likert_scale %||% list(min = 1, max = 5),
    considerations = lapply(survey_obj$considerations, function(x) {
      list(id = scalar_text(x$id), text = scalar_text(x$text))
    }),
    policy_options = lapply(survey_obj$policy_options, function(x) {
      list(id = scalar_text(x$id), text = scalar_text(x$text))
    })
  )
  safe_json(compact)
}

dri_response_prompt_strict <- function(persona, survey_obj, timing) {
  persona <- lapply(persona, scalar_text)
  ids <- dri_ids(survey_obj)
  paste0(
    "You are completing a Deliberative Reason Index (DRI) survey in character as the persona below.\n\n",
    "CRITICAL OUTPUT RULES:\n",
    "- You MUST answer EVERY consideration ID exactly once: ", paste(ids$c_ids, collapse = ", "), ".\n",
    "- You MUST rank EVERY policy option ID exactly once: ", paste(ids$p_ids, collapse = ", "), ".\n",
    "- Consideration scores must be integers from 1 to 5 only.\n",
    "- Preference ranks must be integers from 1 to ", length(ids$p_ids), " with no ties and no missing ranks.\n",
    "- Return ONLY valid JSON between ###JSON_START### and ###JSON_END###.\n",
    "- No explanation, no prose, no markdown.\n\n",
    "Persona:\n",
    "Name: ", persona$name, "\n",
    "Background: ", persona$background, "\n",
    "Community: ", persona$community, "\n",
    "Values: ", persona$values, "\n",
    "Blind spots: ", persona$blind_spots, "\n",
    "Ideology: ", persona$political_ideology, "/10\n",
    "Initial position: ", persona$initial_position_text, " (score ", persona$initial_score, "/5)\n\n",
    "Timing: ", timing, "-deliberation\n\n",
    "Survey JSON:\n", compact_dri_survey_json(survey_obj), "\n\n",
    "Required JSON schema:\n",
    "###JSON_START###\n",
    "{\"respondent\":\"", persona$name, "\",\"timing\":\"", timing, "\",",
    "\"considerations\":[", paste(sprintf("{\"id\":\"%s\",\"score\":3}", ids$c_ids), collapse = ","), "],",
    "\"preferences\":[", paste(sprintf("{\"id\":\"%s\",\"rank\":%d}", ids$p_ids, seq_along(ids$p_ids)), collapse = ","), "]}",
    "\n###JSON_END###"
  )
}

dri_repair_prompt <- function(persona, survey_obj, timing, bad_response, validation) {
  ids <- dri_ids(survey_obj)
  paste0(
    "Your previous DRI survey response was incomplete or invalid.\n",
    "You must now return ONE complete corrected JSON object only.\n\n",
    "Missing/invalid consideration IDs: ", paste(validation$missing_considerations, collapse = ", "), "\n",
    "Missing/invalid preference IDs: ", paste(unique(c(validation$missing_preferences, validation$bad_preference_ranks)), collapse = ", "), "\n\n",
    "Remember:\n",
    "- Include EVERY consideration exactly once: ", paste(ids$c_ids, collapse = ", "), ".\n",
    "- Include EVERY policy option exactly once: ", paste(ids$p_ids, collapse = ", "), ".\n",
    "- Consideration scores: integers 1-5 only.\n",
    "- Preference ranks: integers 1-", length(ids$p_ids), " exactly once each, no ties.\n",
    "- Return ONLY valid JSON between ###JSON_START### and ###JSON_END###.\n\n",
    "Persona and survey:\n",
    dri_response_prompt_strict(persona, survey_obj, timing), "\n\n",
    "Invalid previous response:\n", truncate_text(safe_json(bad_response), 5000)
  )
}

complete_dri_response <- function(api_key, model, persona, survey_obj, timing,
                                  temperature = 0.2, max_attempts = 4) {
  last_response <- NULL
  last_validation <- NULL
  respondent <- scalar_text(persona$name %||% "unknown")
  
  for (attempt in seq_len(max_attempts)) {
    prompt <- if (attempt == 1 || is.null(last_response)) {
      dri_response_prompt_strict(persona, survey_obj, timing)
    } else {
      dri_repair_prompt(persona, survey_obj, timing, last_response, last_validation)
    }
    
    txt <- call_openrouter(
      api_key, model,
      list(
        list(role = "system", content = paste(
          "You complete DRI surveys in character.",
          "You must return complete machine-readable JSON only.",
          "Never omit items. Never explain answers."
        )),
        list(role = "user", content = prompt)
      ),
      max_tokens = 7000,
      temperature = min(0.4, max(0, temperature)),
      max_retries = 3
    )
    
    parsed <- coerce_json(txt)
    parsed <- repair_dri_response_object(parsed, survey_obj, respondent = respondent, timing = timing)
    val <- validate_dri_response(parsed, survey_obj, timing)
    
    last_response <- parsed
    last_validation <- val
    
    if (isTRUE(val$ok)) return(parsed)
  }
  
  stop(
    "Incomplete DRI response for ", respondent, " (", timing, "). ",
    "Valid considerations: ", last_validation$n_considerations_valid, "/", last_validation$n_considerations_expected, "; ",
    "valid preferences: ", last_validation$n_preferences_valid, "/", last_validation$n_preferences_expected, ". ",
    "Missing considerations: ", paste(head(last_validation$missing_considerations, 12), collapse = ", "), ". ",
    "Missing/bad preferences: ", paste(head(unique(c(last_validation$missing_preferences, last_validation$bad_preference_ranks)), 12), collapse = ", "), "."
  )
}

dri_completion_summary <- function(responses, survey_obj) {
  if (is.null(responses) || is.null(survey_obj) || !length(responses)) {
    return(data.frame(n = 0, complete = 0, incomplete = 0, pct_complete = NA_real_))
  }
  vals <- lapply(responses, validate_dri_response, survey_obj = survey_obj)
  ok <- vapply(vals, function(x) isTRUE(x$ok), logical(1))
  data.frame(
    n = length(ok),
    complete = sum(ok),
    incomplete = sum(!ok),
    pct_complete = round(100 * mean(ok), 1),
    stringsAsFactors = FALSE
  )
}



extract_dri_response <- function(resp, survey_obj) {
  ids <- dri_ids(survey_obj)
  c_ids <- ids$c_ids
  p_ids <- ids$p_ids
  
  c_vec <- setNames(rep(NA_real_, length(c_ids)), c_ids)
  p_rank <- setNames(rep(NA_real_, length(p_ids)), p_ids)
  
  if (!is.null(resp$considerations)) {
    for (x in resp$considerations) {
      id <- normalize_dri_item_id(x$id %||% "", c_ids)
      if (id %in% c_ids) {
        c_vec[id] <- extract_numeric_first(x$score %||% x$rating %||% x$value %||% x$answer_numeric %||% x$answer %||% NA)
      }
    }
  }
  
  if (!is.null(resp$preferences)) {
    for (x in resp$preferences) {
      id <- normalize_dri_item_id(x$id %||% "", p_ids)
      if (id %in% p_ids) {
        p_rank[id] <- extract_numeric_first(x$rank %||% x$ranking %||% x$value %||% x$answer_numeric %||% x$answer %||% NA)
      }
    }
  }
  
  list(
    considerations = c_vec,
    preference_ranks = p_rank,
    preference_borda = rank_to_borda(p_rank, p_ids)
  )
}


read_uploaded_dri_survey <- function(path, filename = NULL) {
  ext <- tolower(tools::file_ext(filename %||% path))
  
  if (ext != "csv") {
    stop("The uploaded external DRI survey must be a CSV file.")
  }
  
  df <- tryCatch(
    read.csv(path, stringsAsFactors = FALSE, check.names = FALSE),
    error = function(e) stop("Could not read CSV file: ", conditionMessage(e))
  )
  
  names(df) <- trimws(names(df))
  req_cols <- c("section", "id", "text")
  miss <- setdiff(req_cols, names(df))
  if (length(miss) > 0) {
    stop("CSV is missing required columns: ", paste(miss, collapse = ", "))
  }
  
  df$section <- trimws(tolower(df$section))
  df$id <- trimws(as.character(df$id))
  df$text <- trimws(as.character(df$text))
  
  if ("theme" %in% names(df)) {
    df$theme <- trimws(as.character(df$theme))
  } else {
    df$theme <- ""
  }
  
  if ("direction" %in% names(df)) {
    df$direction <- trimws(as.character(df$direction))
  } else {
    df$direction <- ""
  }
  
  cons_df <- df[df$section %in% c("consideration", "considerations"), , drop = FALSE]
  opt_df  <- df[df$section %in% c("policy_option", "policy option", "option", "policy_options"), , drop = FALSE]
  
  if (nrow(cons_df) == 0) stop("CSV contains no consideration rows.")
  if (nrow(opt_df) == 0) stop("CSV contains no policy option rows.")
  
  considerations <- lapply(seq_len(nrow(cons_df)), function(i) {
    list(
      id = cons_df$id[i],
      text = cons_df$text[i],
      theme = cons_df$theme[i],
      direction = cons_df$direction[i]
    )
  })
  
  policy_options <- lapply(seq_len(nrow(opt_df)), function(i) {
    list(
      id = opt_df$id[i],
      text = opt_df$text[i]
    )
  })
  
  list(
    title = if (!is.null(filename)) filename else "Uploaded DRI Survey",
    considerations = considerations,
    policy_options = policy_options,
    likert_scale = list(
      min = 1,
      max = 5,
      labels = c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree")
    )
  )
}
dri_calc <- function(data, v1, v2){
  x <- as.numeric(data[[v1]])
  y <- as.numeric(data[[v2]])
  ok <- is.finite(x) & is.finite(y)
  x <- x[ok]
  y <- y[ok]
  
  if (length(x) == 0) return(NA_real_)
  
  lambda <- 1 - (sqrt(2) / 2)
  dri <- 2 * (((1 - mean(abs((x - y) / sqrt(2)), na.rm = TRUE)) - lambda) / (1 - lambda)) - 1
  return(max(-1, min(1, dri)))
}


compute_dri_from_responses <- function(response_list, survey_obj, timing = "pre") {
  if (is.null(response_list) || length(response_list) < 2 || is.null(survey_obj)) return(NULL)
  
  parsed <- lapply(response_list, extract_dri_response, survey_obj = survey_obj)
  respondent_names <- vapply(response_list, function(r) scalar_text(r$respondent %||% "unknown"), character(1))
  
  cons_mat <- do.call(rbind, lapply(parsed, function(x) x$considerations))
  pref_rank_mat <- do.call(rbind, lapply(parsed, function(x) x$preference_ranks))
  
  rownames(cons_mat) <- respondent_names
  rownames(pref_rank_mat) <- respondent_names
  
  if (nrow(cons_mat) < 2) return(NULL)
  
  pair_idx <- combn(seq_len(nrow(cons_mat)), 2, simplify = FALSE)
  
  pairwise <- bind_rows(lapply(pair_idx, function(ix) {
    i <- ix[1]
    j <- ix[2]
    
    q_cor <- safe_cor(cons_mat[i, ], cons_mat[j, ], method = "spearman")
    r_cor <- safe_cor(pref_rank_mat[i, ], pref_rank_mat[j, ], method = "kendall")
    
    ic_point <- if (is.finite(q_cor) && is.finite(r_cor)) {
      1 - abs((r_cor - q_cor) / sqrt(2))
    } else {
      NA_real_
    }
    
    data.frame(
      respondent_1 = rownames(cons_mat)[i],
      respondent_2 = rownames(cons_mat)[j],
      timing = timing,
      consideration_consistency = q_cor,
      preference_consistency = r_cor,
      IC_point = ic_point,
      stringsAsFactors = FALSE
    )
  }))
  
  group_dri <- dri_calc(pairwise, "preference_consistency", "consideration_consistency")
  
  individual <- bind_rows(lapply(respondent_names, function(nm) {
    sub <- pairwise[pairwise$respondent_1 == nm | pairwise$respondent_2 == nm, , drop = FALSE]
    
    data.frame(
      respondent = nm,
      timing = timing,
      avg_ic_point = mean(sub$IC_point, na.rm = TRUE),
      DRI = dri_calc(sub, "preference_consistency", "consideration_consistency"),
      stringsAsFactors = FALSE
    )
  }))
  
  list(
    individual = individual,
    pairwise = pairwise,
    group_dri = group_dri,
    consideration_matrix = cons_mat,
    preference_rank_matrix = pref_rank_mat
  )
}
compute_central_tendency_stats <- function(personas_df, model_baseline = NULL) {
  if (is.null(personas_df) || !"initial_score" %in% names(personas_df) || !"final_score" %in% names(personas_df)) return(NULL)
  
  pre <- as.numeric(personas_df$initial_score)
  post <- as.numeric(personas_df$final_score)
  
  baseline <- if (!is.null(model_baseline) && !is.null(model_baseline$baseline$position_score)) {
    as.numeric(model_baseline$baseline$position_score)
  } else NA_real_
  
  pre_mean <- mean(pre, na.rm = TRUE)
  post_mean <- mean(post, na.rm = TRUE)
  pre_sd <- sd(pre, na.rm = TRUE)
  post_sd <- sd(post, na.rm = TRUE)
  
  avg_pairwise <- function(x) {
    x <- x[is.finite(x)]
    if (length(x) < 2) return(NA_real_)
    mean(as.vector(dist(x)), na.rm = TRUE)
  }
  
  pre_pair <- avg_pairwise(pre)
  post_pair <- avg_pairwise(post)
  
  data.frame(
    baseline = baseline,
    pre_mean = pre_mean,
    post_mean = post_mean,
    pre_sd = pre_sd,
    post_sd = post_sd,
    pre_pairwise_distance = pre_pair,
    post_pairwise_distance = post_pair,
    shift_to_baseline = ifelse(is.finite(baseline),
                               abs(pre_mean - baseline) - abs(post_mean - baseline),
                               NA_real_),
    compression_sd = pre_sd - post_sd,
    compression_pairwise = pre_pair - post_pair,
    stringsAsFactors = FALSE
  )
}
# ============================================================
# OPENROUTER
# ============================================================
openrouter_headers <- function(api_key = NULL) {
  hdrs <- c("Content-Type" = "application/json",
            "HTTP-Referer" = Sys.getenv("OPENROUTER_SITE_URL", unset = "http://localhost"),
            "X-Title" = Sys.getenv("OPENROUTER_APP_NAME", unset = "Deliberation Simulator"))
  key <- clean_api_key(api_key)
  if (nzchar(key)) hdrs <- c(hdrs, "Authorization" = paste("Bearer", key))
  hdrs
}

call_openrouter <- function(api_key, model, messages, max_tokens = 2500, temperature = 0.7, max_retries = 2) {
  body <- list(model = unname(model), messages = messages, max_tokens = max_tokens, temperature = temperature)
  
  last_error <- NULL
  for (attempt in seq_len(max_retries)) {
    resp <- tryCatch({
      request("https://openrouter.ai/api/v1/chat/completions") |>
        req_headers(!!!openrouter_headers(api_key)) |>
        req_body_json(body, auto_unbox = TRUE) |>
        req_timeout(600) |>
        req_error(is_error = function(resp) FALSE) |>
        req_perform()
    }, error = function(e) {
      last_error <<- conditionMessage(e)
      NULL
    })
    
    if (is.null(resp)) {
      if (attempt < max_retries) { Sys.sleep(2); next }
      stop(paste("API request failed after", max_retries, "attempts:", last_error))
    }
    
    status <- resp_status(resp)
    txt <- resp_body_string(resp)
    js <- tryCatch(fromJSON(txt, simplifyVector = FALSE), error = function(e) NULL)
    
    if (status != 200) {
      msg <- tryCatch(js$error$message, error = function(e) txt)
      stop(paste("OpenRouter API error", status, ":", msg))
    }
    
    out <- tryCatch(js$choices[[1]]$message$content, error = function(e) NULL)
    out_txt <- paste(out %||% "", collapse = " ")
    
    if (is.null(out) || !nzchar(trimws(out_txt))) {
      raw_preview <- substr(txt, 1, 1000)
      if (attempt < max_retries) { Sys.sleep(1); next }
      stop(paste0(
        "OpenRouter returned an empty response.\n",
        "Model: ", model, "\n",
        "Raw response preview:\n", raw_preview
      ))
    }
    
    # Hallucination detection: check if output is gibberish
    if (is_hallucinated(out)) {
      if (attempt < max_retries) { Sys.sleep(1); next }
      stop(paste0("Model returned gibberish/hallucinated output. This usually means the model cannot handle structured JSON generation.\n",
                  "Try a more capable model: openai/gpt-4o, openai/gpt-4o-mini, anthropic/claude-sonnet-4-5, or google/gemini-2.0-flash-001.\n",
                  "First 200 chars: ", substr(out, 1, 200)))
    }
    
    return(out)
  }
}

# Detect hallucinated/gibberish output
is_hallucinated <- function(txt) {
  if (is.null(txt) || !nzchar(txt)) return(TRUE)
  txt <- trimws(txt)
  
  # Check 1: extremely high ratio of non-ASCII or unusual characters
  n_total <- nchar(txt)
  if (n_total < 10) return(TRUE)
  n_alnum <- nchar(gsub("[^a-zA-Z0-9 .,;:!?'\"{}\\[\\]\\-\\n]", "", txt))
  if (n_alnum / n_total < 0.5) return(TRUE)
  
  # Check 2: contains obvious gibberish patterns (camelCase soup, random tokens)
  gibberish_patterns <- c("\\b[A-Z][a-z]+[A-Z][a-z]+[A-Z]", "resize visualizing", "stackAudience",
                          "fucked}", "orbis erup", "_chunkWatching", "pursuantุง")
  for (pat in gibberish_patterns) {
    if (grepl(pat, txt, perl = TRUE)) return(TRUE)
  }
  
  # Check 3: if we expect JSON, the first non-whitespace char should be { or [
  # But only flag as hallucination if there's NO { anywhere in first 500 chars
  first500 <- substr(txt, 1, min(500, nchar(txt)))
  if (!grepl("\\{", first500) && !grepl("\\[", first500)) {
    # Could be a text response, not necessarily hallucination
    # But if it also has very low word-to-symbol ratio, it's gibberish
    words <- length(strsplit(first500, "\\s+")[[1]])
    if (words < 5) return(TRUE)
  }
  
  FALSE
}

fetch_openrouter_models <- function(api_key = NULL) {
  resp <- request("https://openrouter.ai/api/v1/models") |>
    req_headers(!!!openrouter_headers(api_key)) |>
    req_error(is_error = function(resp) FALSE) |>
    req_perform()
  status <- resp_status(resp)
  txt <- resp_body_string(resp)
  js <- tryCatch(fromJSON(txt, simplifyDataFrame = TRUE), error = function(e) NULL)
  if (status != 200 || is.null(js$data)) return(DEFAULT_MODELS)
  unique(sort(js$data$id))
}

safe_agent_deliberation <- function(
    personas_df,
    issue,
    policy_question,
    evidence_block,
    round_label,
    round_instruction,
    model,
    api_key,
    temperature,
    memory_df = NULL
){
  
  results <- list()
  
  for(i in seq_len(nrow(personas_df))){
    
    persona <- personas_df[i,]
    
    # -------- SHORT PERSONA --------
    persona_short <- list(
      name = persona$name,
      political_ideology = persona$political_ideology,
      initial_position_text = persona$initial_position_text,
      initial_score = persona$initial_score,
      values = persona$values
    )
    
    # -------- SHORT MEMORY --------
    mem_txt <- ""
    if(!is.null(memory_df) && nrow(memory_df) > 0){
      last_mem <- tail(memory_df$text, 6)
      mem_txt <- paste(last_mem, collapse="\n")
    }
    
    resp <- NULL
    
    for(k in 1:3){
      
      resp <- tryCatch(
        call_openrouter(
          api_key,
          model,
          list(
            list(
              role="system",
              content="You are one citizen participating in a democratic deliberation. Be concise."
            ),
            list(
              role="user",
              content = agent_turn_prompt(
                persona_short,
                issue,
                policy_question,
                evidence_block,
                mem_txt,
                round_instruction,
                persona$initial_score,
                persona$initial_position_text
              )
            )
          ),
          max_tokens = 1500,
          temperature = min(temperature,0.6),
          max_retries = 2
        ),
        error=function(e) NULL
      )
      
      if(!is.null(resp)) break
      
      Sys.sleep(1.2)
    }
    
    if(is.null(resp)){
      stop(paste("Deliberation failed:", persona$name, "in", round_label))
    }
    
    parsed <- tryCatch(coerce_json(resp), error=function(e) NULL)
    
    if(is.null(parsed)){
      stop(paste("JSON parsing failed:", persona$name, "in", round_label))
    }
    
    results[[i]] <- parsed
    
    Sys.sleep(0.6)
  }
  
  bind_rows(lapply(results, as.data.frame))
}

# ============================================================
# PROMPTS
# ============================================================
persona_generation_prompt <- function(issue, policy_question, n_participants, evidence_block,
                                      user_persona_note, demographic_instruction) {
  paste0(
    "We are running a deliberation simulation.\n\n",
    "Policy issue: ", scalar_text(issue), "\n",
    "Policy question: ", scalar_text(policy_question), "\n",
    "Number of participants required: ", n_participants, "\n\n",
    "Evidence base:\n", scalar_text(evidence_block), "\n\n",
    scalar_text(demographic_instruction), "\n\n",
    if (has_text(user_persona_note)) paste0("Additional user guidance on personas:\n", scalar_text(user_persona_note), "\n\n") else "",
    "You MUST generate exactly ", n_participants, " personas that match the demographic composition targets above as closely as possible.\n",
    "Keep every field compact so the JSON does not get truncated.\n",
    "Return ONLY valid JSON.\nStart your response with ###JSON_START###\nEnd your response with ###JSON_END###\nDo not write anything before or after the JSON.\n",
    build_persona_json_prompt(n_participants))
}

integrity_prompt <- function(issue, evidence_block, personas_json, group_n) {
  paste0(
    "Assess and improve the DESIGN INTEGRITY of this deliberation simulation.\n\n",
    "Issue: ", scalar_text(issue), "\nNumber of groups: ", group_n, "\n\n",
    "Evidence base:\n", scalar_text(evidence_block), "\n\n",
    "Current personas JSON:\n", scalar_text(personas_json), "\n\n",
    "Use these design integrity criteria: participant representativeness, unbiased framing, procedural design involvement.\n",
    "If there are critical perspectives or stakeholder groups NOT represented, list them under recommended_revisions.\n",
    "Return ONLY valid JSON.\nStart your response with ###JSON_START###\nEnd your response with ###JSON_END###\nDo not write anything before or after the JSON.\n",
    '{"integrity":{"participant_representativeness":{"score":0,"notes":"string","missing_perspectives":["string"]},"unbiased_framing":{"score":0,"notes":"string"},"procedural_design_involvement":{"score":0,"notes":"string"},"overall_score":0,"recommended_revisions":["string"]}}')
}

persona_validation_prompt <- function(issue, policy_question, evidence_block, personas_json, integrity_json) {
  paste0(
    "You are improving the personas for a deliberation simulation.\n\n",
    "Issue: ", scalar_text(issue), "\n",
    "Question: ", scalar_text(policy_question), "\n\n",
    "Current personas:\n", truncate_text(personas_json, 5000), "\n\n",
    "Design integrity review identified these gaps:\n", truncate_text(integrity_json, 2500), "\n\n",
    "Incorporate any missing perspectives or stakeholder groups identified in the review.\n",
    "It is okay for a persona to have more than one identity relevant to the discussion.\n",
    "Return the FULL revised set of personas with the SAME number of personas as input.\n",
    "Keep all fields short. Do not omit any field.\n",
    "Return ONLY valid JSON.\nStart your response with ###JSON_START###\nEnd your response with ###JSON_END###\nDo not write anything before or after the JSON.\n\n",
    "Rules:\n",
    "- age must be an integer\n",
    "- political_ideology must be an INTEGER from 0 to 10\n",
    "- initial_score must be an INTEGER from 1 to 5\n",
    "- settlement must be exactly 'urban' or 'rural'\n",
    "- initial_position_text max 18 words\n\n",
    '{"personas":[{"id":"P1","name":"string","age":25,"gender":"string","education":"string","political_ideology":5,"income":"string","background":"string","community":"string","settlement":"urban","interests":"string","values":"string","blind_spots":"string","initial_position_text":"string","initial_score":3}]}'
  )
}

ranking_prompt <- function(persona, issue, policy_question, master_values, master_options, evaluations_text) {
  persona <- lapply(persona, scalar_text)
  paste0(
    "You are ", persona$name, ". ", persona$background, ".\n",
    "Values: ", persona$values, ". Ideology: ", persona$political_ideology, "/10.\n\n",
    "Issue: ", scalar_text(issue), "\nQuestion: ", scalar_text(policy_question), "\n\n",
    "MASTER VALUES:\n", scalar_text(master_values), "\n\n",
    "MASTER OPTIONS:\n", scalar_text(master_options), "\n\n",
    "EVALUATION SUMMARY:\n", scalar_text(evaluations_text), "\n\n",
    "Based on your values and perspective, rank ALL the policy options from most preferred (rank 1) to least preferred.\n",
    "For each, give a 1-sentence justification connecting your ranking to your values.\n",
    "Also provide your final_position_score (1-5 scale: 1=strongly oppose the top option, 5=strongly support).\n\n",
    "Return ONLY valid JSON.\nStart your response with ###JSON_START###\nEnd your response with ###JSON_END###\nDo not write anything before or after the JSON.\n",
    '{"respondent":"', persona$name, '","final_position_score":0,"rankings":[{"option":"option text","rank":1,"justification":"reason"}]}')
}

# --- FIX 1: fully scalar-safe ---
agent_system_prompt <- function(persona) {
  persona <- lapply(persona, scalar_text)
  paste(
    "You are simulating exactly one deliberative participant.",
    paste0("You are ", persona$name, "."),
    paste0("Age: ", persona$age, ". Gender: ", persona$gender, ". Education: ", persona$education, "."),
    paste0("Ideology: ", persona$political_ideology, "/10. Income: ", persona$income, "."),
    paste0("Background: ", persona$background, ". Community: ", persona$community, "."),
    paste0("Settlement: ", persona$settlement %||% "unspecified", "."),
    paste0("Interests: ", persona$interests, ". Values: ", persona$values, ". Blind spots: ", persona$blind_spots, "."),
    paste0("Initial position: ", persona$initial_position_text, " (score ", persona$initial_score, "/5)."),
    "Stay consistent with this persona. Speak in first person. Give one concise but substantive intervention.")
}

# --- Persona-reinforced turn prompt (Taubenfeld et al. 2024 bias mitigation) ---
agent_turn_prompt <- function(persona, issue, policy_question, evidence_block, group_context, task_instruction,
                              current_score = NULL, current_position_text = NULL) {
  persona <- lapply(persona, scalar_text)
  group_context <- scalar_text(group_context)
  current_score <- suppressWarnings(as.numeric(current_score %||% persona$initial_score))
  if (!is.finite(current_score)) current_score <- suppressWarnings(as.numeric(persona$initial_score %||% NA))
  if (!is.finite(current_score)) current_score <- 3
  current_position_text <- scalar_text(current_position_text %||% persona$initial_position_text)
  
  paste0(
    "You are acting as this participant and must remain consistent with this profile.\n\n",
    "NAME: ", persona$name, "\n",
    "BACKGROUND: ", persona$background, "\n",
    "COMMUNITY: ", persona$community, "\n",
    "VALUES: ", persona$values, "\n",
    "BLIND SPOTS: ", persona$blind_spots, "\n",
    "IDEOLOGY: ", persona$political_ideology, "/10\n",
    "INITIAL POSITION: ", persona$initial_position_text, "\n",
    "INITIAL SCORE: ", persona$initial_score, "/5\n",
    "CURRENT POSITION SUMMARY: ", current_position_text, "\n",
    "CURRENT SCORE BEFORE THIS TURN: ", current_score, "/5\n\n",
    "Important instruction:\n",
    "- Stay consistent with this participant's profile and values.\n",
    "- Treat CURRENT SCORE BEFORE THIS TURN as your anchor for this intervention.\n",
    "- Do NOT drift toward a moderate consensus unless there is a clear reason from the evidence or discussion.\n",
    "- If you shift position, it must be gradual: normally no more than 1 point from CURRENT SCORE BEFORE THIS TURN.\n",
    "- If your position_score changes, shift_reason must explain what specific argument, value conflict, or evidence caused the movement.\n",
    "- position_score must be an integer from 1 to 5.\n",
    "- sentiment_score must be between -1 and 1.\n\n",
    "Issue: ", scalar_text(issue), "\n",
    "Policy question: ", scalar_text(policy_question), "\n\n",
    "Evidence base:\n", scalar_text(evidence_block), "\n\n",
    if (nzchar(trimws(group_context))) {
      paste0("What others have already said in this group:\n", group_context, "\n\n")
    } else "",
    "Task: ", scalar_text(task_instruction), "\n\n",
    "Return ONLY valid JSON.\nStart your response with ###JSON_START###\nEnd your response with ###JSON_END###\nDo not write anything before or after the JSON.\n",
    '{"speaker":"name","text":"one intervention","position_score":0,"sentiment_score":0,"shift_reason":"short reason or none"}'
  )
}

synthesis_prompt <- function(title, issue, policy_question, evidence_block, inputs, extra_instruction = "") {
  paste0(scalar_text(title), "\n\n", "Issue: ", scalar_text(issue), "\n",
         "Policy question: ", scalar_text(policy_question), "\n\n",
         "Evidence:\n", scalar_text(evidence_block), "\n\n",
         "Inputs to synthesize:\n", scalar_text(inputs), "\n\n", scalar_text(extra_instruction))
}

quality_prompt <- function(issue, evidence_block, transcript, final_report, participants_json) {
  paste0(
    "Assess the QUALITY of this GAI deliberation simulation.\n\nIssue: ", scalar_text(issue), "\n\n",
    "Evidence:\n", scalar_text(evidence_block), "\n\n",
    "Participants JSON:\n", scalar_text(participants_json), "\n\n",
    "Transcript:\n", truncate_text(transcript, 15000), "\n\n",
    "Final report:\n", truncate_text(final_report, 10000), "\n\n",
    "Use these criteria: solid information base, key values, broad range of solutions, pros/cons/tradeoffs, participant representation, group dynamics, individual dynamics, quality of judgments.\n",
    "Score each dimension 0-10 and provide substantive notes.\n",
    "Return ONLY valid JSON.\nStart your response with ###JSON_START###\nEnd your response with ###JSON_END###\nDo not write anything before or after the JSON.\n",
    '{"quality":{"solid_information_base":{"score":0,"notes":"string"},"key_values":{"score":0,"notes":"string"},"broad_range_of_solutions":{"score":0,"notes":"string"},"pros_cons_tradeoffs":{"score":0,"notes":"string"},"participant_representation":{"score":0,"notes":"string"},"group_dynamics":{"score":0,"notes":"string"},"individual_dynamics":{"score":0,"notes":"string"},"quality_of_judgments":{"score":0,"notes":"string"},"overall_score":0,"recommendations":["string"]}}')
}

survey_prompt <- function(persona, survey_text, timing) {
  persona <- lapply(persona, scalar_text)
  paste0(
    "You are ", persona$name, ". ", persona$background, ". ",
    "Age: ", persona$age, ". Gender: ", persona$gender, ". Education: ", persona$education, ". ",
    "Ideology: ", persona$political_ideology, "/10. Settlement: ", persona$settlement %||% "unspecified", ".\n\n",
    "This is a ", timing, "-deliberation survey. Answer the survey below IN CHARACTER as this persona.\n\n",
    "CRITICAL DATASET RULES:\n",
    "1. If a question gives a numeric scale, Likert scale, rating scale, or numbered response options, answer with the NUMBER ONLY in answer_numeric.\n",
    "2. In the answer field, write only the selected number or selected option label. Do NOT write sentences.\n",
    "3. Do NOT add explanations, justifications, or comments.\n",
    "4. If a question is open text with no numeric or categorical options, give a short categorical/text answer only.\n",
    "5. One response object per survey item/sub-item. For matrix questions, split each item into its own response.\n\n",
    "SURVEY:\n", scalar_text(survey_text), "\n\n",
    "Return ONLY valid JSON.\nStart your response with ###JSON_START###\nEnd your response with ###JSON_END###\nDo not write anything before or after the JSON.\n",
    "Use this EXACT structure. Do not include a justification key:\n",
    '{"respondent":"', persona$name, '","timing":"', timing, '","responses":[{"question":"exact survey item or sub-item","answer_numeric":7,"answer":"7"}]}')
}


baseline_bias_prompt <- function(issue, policy_question, evidence_block) {
  paste0(
    "Assess your baseline orientation toward this issue without role-playing any persona.\n\n",
    "Issue: ", scalar_text(issue), "\n",
    "Policy question: ", scalar_text(policy_question), "\n\n",
    "Evidence:\n", truncate_text(evidence_block, 6000), "\n\n",
    "Return ONLY valid JSON with this exact structure:\n",
    '{"baseline":{"position_score":0,"stance":"string","reasoning":"string"}}\n\n',
    "position_score must be on a 1-5 scale where:\n",
    "1 = strongly oppose, 2 = oppose, 3 = neutral/mixed, 4 = support, 5 = strongly support")
}

dri_survey_prompt <- function(issue, policy_question, evidence_block) {
  paste0(
    "You are designing a Deliberative Reason Index (DRI) survey for a policy issue.\n\n",
    "Issue: ", scalar_text(issue), "\n",
    "Policy question: ", scalar_text(policy_question), "\n\n",
    "Evidence:\n", truncate_text(evidence_block, 7000), "\n\n",
    "THEORETICAL FRAMING — Q-METHOD CONCOURSE:\n",
    "The consideration statements must constitute a *concourse* in the Q-method sense: a representative sample of the full universe of viewpoints, arguments, values, and concerns that currently circulate in public sphere discourse on this issue.",
    " The concourse should map the communicative space, not just the most salient positions.",
    " Include rarely-heard perspectives (e.g., implementation burdens on specific groups, distributional consequences, procedural legitimacy concerns) alongside dominant ones.",
    " Each statement should read as something a real person or public commentator might actually say in public debate — concrete, first-person-compatible, and not academic.\n\n",
    "Generate a survey with:\n",
    "- between 20 and 40 consideration statements covering the full concourse\n",
    "- between 4 and 7 distinct policy options that are concretely rankable\n",
    "- considerations must span: pro/con positions, values (liberty, equality, solidarity, efficiency), affected groups, feasibility, unintended consequences, procedural concerns\n",
    "- avoid duplicate concepts even if phrased differently\n",
    "- balance statement directions — not all statements should lean pro or con\n\n",
    "Return ONLY valid JSON.\nStart your response with ###JSON_START###\nEnd your response with ###JSON_END###\nDo not write anything before or after the JSON.\n",
    '{"title":"string",',
    '"considerations":[{"id":"C1","text":"string","theme":"string","direction":"pro/con/mixed"}],',
    '"policy_options":[{"id":"P1","text":"string"}],',
    '"likert_scale":{"min":1,"max":5,"labels":["Strongly disagree","Disagree","Neutral","Agree","Strongly agree"]}}'
  )
}

dri_response_prompt <- function(persona, survey_json, timing) {
  # Backward-compatible wrapper retained for existing code paths.
  # If a full survey object is passed, use the strict complete prompt.
  if (is.list(survey_json) && !is.null(survey_json$considerations) && !is.null(survey_json$policy_options)) {
    return(dri_response_prompt_strict(persona, survey_json, timing))
  }
  persona <- lapply(persona, scalar_text)
  paste0(
    "You are completing a deliberative reasoning survey in character as the persona below.\n\n",
    "Persona:\n",
    "Name: ", persona$name, "\n",
    "Background: ", persona$background, "\n",
    "Community: ", persona$community, "\n",
    "Values: ", persona$values, "\n",
    "Blind spots: ", persona$blind_spots, "\n",
    "Ideology: ", persona$political_ideology, "/10\n",
    "Initial position: ", persona$initial_position_text, " (score ", persona$initial_score, "/5)\n\n",
    "Timing: ", timing, "-deliberation\n\n",
    "Survey:\n", scalar_text(survey_json), "\n\n",
    "Instructions:\n",
    "1. Rate EVERY consideration item on the Likert scale.\n",
    "2. Rank ALL policy options from 1 (most preferred) to N (least preferred). No ties allowed.\n",
    "3. Use the EXACT item IDs from the survey.\n",
    "4. Return ONLY valid complete JSON — no preamble, no explanation.\n\n",
    "Start with ###JSON_START### and end with ###JSON_END###\n",
    "{\"respondent\":\"", persona$name, "\",\"timing\":\"", timing, "\",",
    "\"considerations\":[{\"id\":\"C01\",\"score\":3}],",
    "\"preferences\":[{\"id\":\"P01\",\"rank\":1}]}"
  )
}


compute_drift_table <- function(personas_df, turns_df, baseline_bias = NULL, threshold = 2) {
  if (is.null(personas_df) || is.null(turns_df) || nrow(turns_df) == 0) return(NULL)
  if (!"position_score" %in% names(turns_df)) return(NULL)
  initial_map <- personas_df |> dplyr::select(name, initial_score)
  drift_df <- turns_df |>
    dplyr::filter(!is.na(position_score), speaker %in% initial_map$name) |>
    dplyr::left_join(initial_map, by = c("speaker" = "name")) |>
    dplyr::mutate(
      position_score = as.numeric(position_score),
      initial_score = as.numeric(initial_score),
      abs_drift = abs(position_score - initial_score),
      drift_flag = abs_drift >= threshold)
  if (!is.null(baseline_bias) && !is.null(baseline_bias$baseline$position_score)) {
    b <- as.numeric(baseline_bias$baseline$position_score)
    drift_df <- drift_df |> dplyr::mutate(
      dist_to_baseline = abs(position_score - b),
      initial_dist_to_baseline = abs(initial_score - b),
      toward_baseline = dist_to_baseline < initial_dist_to_baseline)
  } else {
    drift_df$toward_baseline <- NA
  }
  drift_df
}

# ============================================================
# PLOTS
# ============================================================
plot_theme <- function() {
  theme_minimal(base_size = 11) +
    theme(plot.background = element_rect(fill = "#ffffff", color = NA),
          panel.background = element_rect(fill = "#fbfcfe", color = NA),
          panel.grid.major = element_line(color = "#e8edf4", linewidth = 0.4),
          panel.grid.minor = element_blank(),
          text = element_text(color = "#1f2330"),
          axis.text = element_text(color = "#657084"),
          axis.title = element_text(color = "#946f00"),
          plot.title = element_text(color = "#946f00", face = "bold"),
          legend.background = element_rect(fill = "#ffffff", color = NA),
          legend.text = element_text(color = "#4f596b"),
          legend.title = element_text(color = "#946f00"))
}

make_opinion_change <- function(df) {
  df_long <- df |> select(name, initial_score, final_score) |>
    pivot_longer(c(initial_score, final_score), names_to = "phase", values_to = "score") |>
    mutate(phase = factor(phase, levels = c("initial_score", "final_score"), labels = c("Before", "After")), name = factor(name))
  ggplot(df_long, aes(x = phase, y = score, group = name, color = name)) +
    geom_line(linewidth = 1.1, alpha = 0.85) + geom_point(size = 3.2) +
    scale_y_continuous(limits = c(0.8, 5.2), breaks = 1:5) +
    labs(title = "Opinion Change", x = NULL, y = "Likert score", color = "Participant") + plot_theme()
}

make_polarization <- function(df) {
  pol_df <- data.frame(stage = c("Before", "After"),
                       sd = c(sd(df$initial_score, na.rm = TRUE), sd(df$final_score, na.rm = TRUE)))
  ggplot(pol_df, aes(x = stage, y = sd, fill = stage)) + geom_col() +
    labs(title = "Polarization (SD of positions)", x = NULL, y = "Standard deviation") +
    plot_theme() + theme(legend.position = "none")
}

make_sentiment <- function(turns) {
  if (is.null(turns) || !is.data.frame(turns) || nrow(turns) == 0) {
    return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "No sentiment data available", color = "#7d8596", size = 5) +
             theme_void() + theme(plot.background = element_rect(fill = "#ffffff", color = NA)))
  }
  ggplot(turns, aes(x = turn, y = sentiment_score, color = phase, group = 1)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "#8b94a7") +
    geom_line() + geom_point() +
    labs(title = "Sentiment trajectory", x = "Turn", y = "Sentiment") + plot_theme()
}

# Position drift tracking (per Taubenfeld et al. 2024)
make_position_drift <- function(turns_df, personas_df, model_baseline = NULL) {
  if (is.null(turns_df) || nrow(turns_df) == 0 || !"position_score" %in% names(turns_df)) {
    return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "No position data available", color = "#7d8596", size = 5) +
             theme_void() + theme(plot.background = element_rect(fill = "#ffffff", color = NA)))
  }
  # Filter out NA position scores
  df <- turns_df[!is.na(turns_df$position_score), ]
  if (nrow(df) == 0) {
    return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "No valid position scores", color = "#7d8596", size = 5) +
             theme_void() + theme(plot.background = element_rect(fill = "#ffffff", color = NA)))
  }
  
  p <- ggplot(df, aes(x = turn, y = position_score, color = speaker)) +
    geom_line(alpha = 0.7) + geom_point(size = 2) +
    scale_y_continuous(limits = c(0, 5.5), breaks = 1:5)
  
  # Add model baseline reference line (dashed, like Taubenfeld et al. Figure 4)
  if (!is.null(model_baseline) && !is.null(model_baseline$baseline$position_score) && !is.na(as.numeric(model_baseline$baseline$position_score))) {
    bl <- as.numeric(model_baseline$baseline$position_score)
    p <- p + geom_hline(yintercept = bl, linetype = "dashed", color = "#d4735e", linewidth = 1) +
      annotate("text", x = max(df$turn), y = bl + 0.2, label = "Model default bias", color = "#d4735e", hjust = 1, size = 3)
  }
  
  # Add initial position reference lines per speaker (thin dotted)
  if (!is.null(personas_df)) {
    for (i in seq_len(nrow(personas_df))) {
      name <- scalar_text(personas_df$name[i])
      init <- as.numeric(personas_df$initial_score[i])
      if (!is.na(init) && name %in% df$speaker) {
        p <- p + geom_hline(yintercept = init, linetype = "dotted", color = "#a0aec0", linewidth = 0.4)
      }
    }
  }
  
  p + labs(title = "Position Drift Monitor",
           subtitle = "Tracks position_score per turn vs initial position (dotted) and model default bias (dashed red)",
           x = "Turn", y = "Position score (1-5)", color = "Speaker") + plot_theme()
}

# Drift magnitude summary
make_drift_summary <- function(turns_df, personas_df) {
  if (is.null(turns_df) || nrow(turns_df) == 0 || !"position_score" %in% names(turns_df) || is.null(personas_df)) {
    return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "No drift data", color = "#7d8596", size = 5) +
             theme_void() + theme(plot.background = element_rect(fill = "#ffffff", color = NA)))
  }
  df <- turns_df[!is.na(turns_df$position_score), ]
  if (nrow(df) == 0) return(ggplot() + theme_void())
  
  # Get last position per speaker
  last_pos <- df |> group_by(speaker) |> summarise(last_score = tail(position_score, 1), .groups = "drop")
  # Merge with initial scores
  init_map <- data.frame(speaker = scalar_text_vec(personas_df$name),
                         initial = as.numeric(personas_df$initial_score), stringsAsFactors = FALSE)
  merged <- inner_join(last_pos, init_map, by = "speaker")
  if (nrow(merged) == 0) return(ggplot() + theme_void())
  
  merged$drift <- merged$last_score - merged$initial
  merged$direction <- ifelse(merged$drift > 0.5, "Shifted up", ifelse(merged$drift < -0.5, "Shifted down", "Stable"))
  merged$speaker <- factor(merged$speaker, levels = merged$speaker[order(abs(merged$drift), decreasing = TRUE)])
  
  ggplot(merged, aes(x = speaker, y = drift, fill = direction)) +
    geom_col(alpha = 0.8) + geom_hline(yintercept = 0, color = "#4b5567") +
    scale_fill_manual(values = c("Shifted up" = "#c8a12d", "Shifted down" = "#5b8abf", "Stable" = "#a0aec0")) +
    labs(title = "Attitude Drift per Participant",
         subtitle = "Difference between last position_score and initial_score (>0.5 = potential bias drift)",
         x = NULL, y = "Drift (last - initial)", fill = NULL) +
    plot_theme() + theme(axis.text.x = element_text(angle = 40, hjust = 1, size = 8))
}

make_quality_radar <- function(quality_obj) {
  scores <- sapply(QUALITY_DIMS, function(d) {
    s <- quality_obj$quality[[d]]$score
    if (is.null(s)) 0 else as.numeric(s)
  })
  df <- data.frame(dimension = factor(QUALITY_LABELS, levels = QUALITY_LABELS), score = scores)
  ggplot(df, aes(x = dimension, y = score)) +
    geom_col(fill = "#c8a12d", alpha = 0.7, width = 0.7) +
    geom_text(aes(label = score), vjust = -0.5, color = "#946f00", size = 3.5) +
    scale_y_continuous(limits = c(0, 11), breaks = seq(0, 10, 2)) +
    labs(title = "Quality Assessment Scores", x = NULL, y = "Score (0\u201310)") +
    plot_theme() + theme(axis.text.x = element_text(angle = 35, hjust = 1, size = 9))
}

# ============================================================
# COMPOSITION PIE CHARTS
# ============================================================
pie_theme <- function() {
  theme_void(base_size = 11) +
    theme(plot.background = element_rect(fill = "#ffffff", color = NA),
          plot.title = element_text(color = "#946f00", face = "bold", size = 13, hjust = 0.5),
          legend.text = element_text(color = "#4f596b", size = 9),
          legend.title = element_blank())
}

make_pie <- function(df, col, title, custom_palette = NULL) {
  if (is.null(df) || !col %in% names(df)) {
    return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "No data", color = "#7d8596", size = 5) +
             theme_void() + theme(plot.background = element_rect(fill = "#ffffff", color = NA)))
  }
  vals <- scalar_text_vec(df[[col]])
  tbl <- as.data.frame(table(vals), stringsAsFactors = FALSE)
  names(tbl) <- c("category", "n")
  tbl$pct <- round(100 * tbl$n / sum(tbl$n))
  tbl$label <- paste0(tbl$category, " (", tbl$pct, "%)")
  
  pal <- if (!is.null(custom_palette)) custom_palette else
    c("#c8a12d", "#2f855a", "#5b8abf", "#d4735e", "#8b6fcf", "#3d9970", "#e07c4e", "#6ba3d6", "#c45b8a", "#7dc87d")
  
  ggplot(tbl, aes(x = "", y = n, fill = label)) +
    geom_col(width = 1, color = "white", linewidth = 0.5) +
    coord_polar("y") +
    scale_fill_manual(values = pal[seq_len(nrow(tbl))]) +
    labs(title = title) + pie_theme()
}

scalar_text_vec <- function(x) {
  vapply(x, function(v) paste(v %||% "unknown", collapse = " "), character(1))
}

make_ideology_pie <- function(df) {
  if (is.null(df) || !"political_ideology" %in% names(df)) return(NULL)
  ideo <- suppressWarnings(as.numeric(scalar_text_vec(df$political_ideology)))
  ideo <- ideo[is.finite(ideo)]
  if (!length(ideo)) {
    return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "No ideology data", color = "#7d8596", size = 5) +
             theme_void() + theme(plot.background = element_rect(fill = "#ffffff", color = NA)))
  }
  levs <- c("Extreme Left (0-2)", "Left (3-4)", "Centre (5)", "Right (6-7)", "Extreme Right (8-10)")
  bins <- cut(ideo, breaks = c(-Inf, 2.5, 4.5, 5.5, 7.5, Inf), labels = levs)
  tbl <- as.data.frame(table(bins), stringsAsFactors = FALSE)
  names(tbl) <- c("category", "n")
  tbl <- tbl[tbl$n > 0, ]
  tbl$pct <- round(100 * tbl$n / sum(tbl$n))
  tbl$label <- paste0(tbl$category, " (", tbl$pct, "%)")
  pal <- c("Extreme Left (0-2)" = "#c53030", "Left (3-4)" = "#e07c4e", "Centre (5)" = "#a0aec0", "Right (6-7)" = "#5b8abf", "Extreme Right (8-10)" = "#2b4c7e")
  ggplot(tbl, aes(x = "", y = n, fill = category)) +
    geom_col(width = 1, color = "white", linewidth = 0.5) +
    coord_polar("y") +
    scale_fill_manual(values = pal, labels = setNames(tbl$label, tbl$category), drop = FALSE) +
    labs(title = "Political Ideology (realized personas)", fill = NULL) + pie_theme()
}
make_ideology_target_table <- function(input, df) {
  realized <- suppressWarnings(as.numeric(scalar_text_vec(df$political_ideology)))
  realized <- realized[is.finite(realized)]
  cats <- c("Extreme left", "Left", "Centre", "Right", "Extreme right")
  if (length(realized)) {
    bins <- cut(realized, breaks = c(-Inf, 2.5, 4.5, 5.5, 7.5, Inf), labels = cats)
    real_tbl <- as.data.frame(table(bins), stringsAsFactors = FALSE)
    names(real_tbl) <- c("category", "realized_n")
    real_tbl$realized_pct <- round(100 * real_tbl$realized_n / sum(real_tbl$realized_n), 1)
  } else {
    real_tbl <- data.frame(category = cats, realized_n = 0, realized_pct = 0)
  }
  target_tbl <- data.frame(
    category = cats,
    target_pct = c(input$demo_ideo_ext_left %||% 0, input$demo_ideo_left %||% 0, input$demo_ideo_centre %||% 0,
                   input$demo_ideo_right %||% 0, input$demo_ideo_ext_right %||% 0)
  )
  out <- left_join(target_tbl, real_tbl, by = "category")
  out$realized_n[is.na(out$realized_n)] <- 0
  out$realized_pct[is.na(out$realized_pct)] <- 0
  out
}
make_ideology_alignment_plot <- function(input, df) {
  tbl <- make_ideology_target_table(input, df)
  tbl_long <- tidyr::pivot_longer(tbl, cols = c("target_pct", "realized_pct"), names_to = "type", values_to = "pct")
  tbl_long$type <- factor(tbl_long$type, levels = c("target_pct", "realized_pct"), labels = c("Target", "Realized"))
  ggplot(tbl_long, aes(x = category, y = pct, fill = type)) +
    geom_col(position = "dodge") +
    labs(title = "Ideology alignment: target vs realized", x = NULL, y = "% of personas", fill = NULL) +
    plot_theme() + theme(axis.text.x = element_text(angle = 25, hjust = 1))
}

# AQuA weights from Behrendt et al. (2024) Table 1 — exact values
AQUA_WEIGHTS <- c(
  Relevance             =  0.20908452,
  Fact                  =  0.18285757,
  Opinion               = -0.11069402,
  Justification         =  0.29000763,
  `Solution Proposals`  =  0.39535126,
  `Additional Knowledge`=  0.14655912,
  Question              = -0.07331445,
  `Referencing Users`   = -0.03768367,
  `Referencing Medium`  =  0.07019062,
  `Referencing Contents`= -0.02847408,
  `Referencing Personal`=  0.21126469,
  `Referencing Format`  = -0.02674237,
  `Polite Form of Address`= 0.01482095,
  Respect               =  0.00732909,
  Screaming             = -0.01900971,
  Vulgar                = -0.04995486,
  Insults               = -0.05884586,
  Sarcasm               = -0.15170863,
  Discrimination        =  0.02934227,
  Storytelling          =  0.10628146
)

# ============================================================
# AQuA — LLM-BASED IMPLEMENTATION (Behrendt et al. 2024)
# Replaces regex proxy with LLM scoring on the exact 20
# binary dimension definitions from the paper.
# Each comment is scored by the LLM on all 20 aspects (0/1),
# then combined with the exact weights from Table 1 and
# normalised to 0–5 exactly as in the paper formula.
# ============================================================

AQUA_DIMENSION_PROMPTS <- list(
  Relevance              = "Does the comment have relevance for the discussed topic? (1=yes, 0=no)",
  Fact                   = "Is there at least one fact-claiming statement in the comment — i.e. a claim presented as objective fact, citing data, statistics, research, or documented events? (1=yes, 0=no)",
  Opinion                = "Is there a subjective statement made in the comment — i.e. a personal view, evaluation, or normative stance expressed as the author's own opinion? (1=yes, 0=no)",
  Justification          = "Is at least one statement justified in the comment — i.e. does the author provide reasons, evidence, or arguments to support a claim? (1=yes, 0=no)",
  `Solution Proposals`   = "Does the comment contain a proposal for how an issue could be solved or an action that should be taken? (1=yes, 0=no)",
  `Additional Knowledge` = "Does the comment contain additional knowledge — i.e. information, context, or background that goes beyond restating the question and enriches the discussion? (1=yes, 0=no)",
  Question               = "Does the comment include a genuine, non-rhetorical question directed at other participants or at the issue? (1=yes, 0=no)",
  `Referencing Users`    = "Does the comment refer to at least one other user by name, pronoun, or role, or refer to all users in the community (e.g. 'we', 'everyone here')? (1=yes, 0=no)",
  `Referencing Medium`   = "Does the comment refer to the medium, the editorial team, or the moderation team (e.g. the news outlet, the platform, the organiser)? (1=yes, 0=no)",
  `Referencing Contents` = "Does the comment refer to content, arguments, or positions expressed in other comments in this discussion? (1=yes, 0=no)",
  `Referencing Personal` = "Does the comment refer to the person or personal characteristics of other users (e.g. their background, profession, or individual traits as described in their previous posts)? (1=yes, 0=no)",
  `Referencing Format`   = "Does the comment refer to the tone, language, spelling, or other formal or stylistic criteria of other comments? (1=yes, 0=no)",
  `Polite Form of Address` = "Does the comment contain welcome or farewell phrases, or other polite forms of address directed at other participants? (1=yes, 0=no)",
  Respect                = "Does the comment contain explicit expressions of respect, gratitude, or acknowledgement of the value of another participant's contribution? (1=yes, 0=no)",
  Screaming              = "Does the comment contain clusters of punctuation (e.g. '!!!', '???') or excessive capitalisation intended to imply shouting or aggressive emphasis? (1=yes, 0=no)",
  Vulgar                 = "Does the comment contain language that is inappropriate for civil discourse — e.g. profanity, obscenities, or crude expressions? (1=yes, 0=no)",
  Insults                = "Does the comment contain insults or personal attacks directed at one or more people? (1=yes, 0=no)",
  Sarcasm                = "Does the comment contain biting mockery or sarcasm aimed at devaluing another participant or their argument? (1=yes, 0=no)",
  Discrimination         = "Does the comment explicitly or implicitly contain unfair treatment, prejudice, or stereotyping of groups or individuals based on characteristics such as ethnicity, gender, religion, or social status? (1=yes, 0=no)",
  Storytelling           = "Does the commenter include a personal story, anecdote, or personal experience to illustrate their point? (1=yes, 0=no)"
)

build_aqua_scoring_prompt <- function(comment_text) {
  dims_block <- paste(
    mapply(function(name, question) {
      paste0('  "', name, '": <0 or 1>  // ', question)
    }, names(AQUA_DIMENSION_PROMPTS), AQUA_DIMENSION_PROMPTS),
    collapse = "\n"
  )
  paste0(
    "You are an expert coder of deliberative discourse quality.\n",
    "Score the following discussion comment on each of the 20 binary dimensions below.\n",
    "For each dimension, return exactly 0 (absent) or 1 (present) based on the definition provided.\n",
    "Apply the definitions strictly — do not infer intent; only code what is explicitly present in the text.\n\n",
    "COMMENT:\n", comment_text, "\n\n",
    "Return ONLY valid JSON with this exact structure (no extra keys, no explanation):\n",
    "###JSON_START###\n",
    "{\n",
    dims_block, "\n",
    "}\n",
    "###JSON_END###"
  )
}

score_single_comment_aqua <- function(comment_text, api_key, model, temperature = 0.0) {
  # Returns a named numeric vector of 20 binary scores (0/1), or NA vector on failure
  dim_names <- names(AQUA_DIMENSION_PROMPTS)
  na_result <- setNames(rep(NA_real_, length(dim_names)), dim_names)
  
  if (!nzchar(trimws(comment_text %||% ""))) return(na_result)
  
  result <- tryCatch({
    raw <- call_openrouter(
      api_key, model,
      list(
        list(role = "system", content = "You are a precise binary coder of deliberative discourse. Return only JSON."),
        list(role = "user",   content = build_aqua_scoring_prompt(comment_text))
      ),
      max_tokens = 400,
      temperature = temperature,
      max_retries = 2
    )
    parsed <- coerce_json(raw)
    scores <- vapply(dim_names, function(d) {
      v <- parsed[[d]]
      if (is.null(v)) return(NA_real_)
      as.numeric(v[[1]])
    }, numeric(1))
    # Clamp to 0/1
    scores <- pmax(0, pmin(1, round(scores)))
    scores
  }, error = function(e) na_result)
  
  result
}

compute_aqua_llm <- function(turns_df, api_key, model, temperature = 0.0) {
  if (is.null(turns_df) || !is.data.frame(turns_df) || nrow(turns_df) == 0 || !"text" %in% names(turns_df)) return(NULL)
  if (!nzchar(clean_api_key(api_key))) return(NULL)
  
  df <- turns_df
  n <- nrow(df)
  dim_names <- names(AQUA_WEIGHTS)
  
  # Score each comment via LLM
  scores_mat <- matrix(NA_real_, nrow = n, ncol = length(dim_names),
                       dimnames = list(NULL, dim_names))
  
  for (i in seq_len(n)) {
    txt <- scalar_text(df$text[i] %||% "")
    if (nzchar(trimws(txt))) {
      scores_mat[i, ] <- score_single_comment_aqua(txt, api_key, model, temperature)
    }
    # Brief pause to avoid rate-limiting
    if (i < n) Sys.sleep(0.3)
  }
  
  ind <- as.data.frame(scores_mat)
  
  # Handle any NA rows by falling back to 0 (conservative: absent)
  for (d in dim_names) {
    ind[[d]] <- ifelse(is.na(ind[[d]]), 0, ind[[d]])
  }
  
  # Apply Behrendt et al. formula: weighted sum -> min-max normalise to 0-5
  raw   <- as.matrix(ind[, dim_names]) %*% AQUA_WEIGHTS
  smin  <- sum(AQUA_WEIGHTS[AQUA_WEIGHTS < 0])
  smax  <- sum(AQUA_WEIGHTS[AQUA_WEIGHTS > 0])
  score <- 5 * (raw - smin) / (smax - smin)
  score <- as.numeric(pmin(5, pmax(0, score)))
  
  comments <- cbind(
    df[, intersect(c("turn", "speaker", "phase", "group", "text"), names(df)), drop = FALSE],
    ind
  )
  comments$aqua_score <- score
  
  dim_means <- data.frame(
    dimension     = dim_names,
    weight        = as.numeric(AQUA_WEIGHTS),
    mean_presence = vapply(ind[, dim_names, drop = FALSE], mean, numeric(1), na.rm = TRUE),
    stringsAsFactors = FALSE
  )
  dim_means$contribution <- dim_means$weight * dim_means$mean_presence
  
  by_speaker <- comments |>
    dplyr::group_by(speaker) |>
    dplyr::summarise(mean_aqua = mean(aqua_score, na.rm = TRUE), n_comments = dplyr::n(), .groups = "drop")
  
  by_phase <- comments |>
    dplyr::group_by(phase) |>
    dplyr::summarise(mean_aqua = mean(aqua_score, na.rm = TRUE), n_comments = dplyr::n(), .groups = "drop")
  
  list(
    comments   = comments,
    dimensions = dim_means,
    by_speaker = by_speaker,
    by_phase   = by_phase,
    overview   = data.frame(
      mean_aqua   = mean(score, na.rm = TRUE),
      median_aqua = median(score, na.rm = TRUE),
      n_comments  = length(score),
      stringsAsFactors = FALSE
    )
  )
}

make_aqua_speaker_plot <- function(aqua_obj) {
  df <- aqua_obj$by_speaker
  if (is.null(df) || nrow(df) == 0) return(ggplot() + theme_void())
  df$speaker <- factor(df$speaker, levels = df$speaker[order(df$mean_aqua, decreasing = TRUE)])
  ggplot(df, aes(x = speaker, y = mean_aqua)) + geom_col() + coord_flip() +
    labs(title = "AQuA by participant", x = NULL, y = "Mean AQuA score (0–5)") + plot_theme()
}

make_aqua_phase_plot <- function(aqua_obj) {
  df <- aqua_obj$by_phase
  if (is.null(df) || nrow(df) == 0) return(ggplot() + theme_void())
  ggplot(df, aes(x = phase, y = mean_aqua, fill = phase)) + geom_col() +
    labs(title = "AQuA by deliberation phase", x = NULL, y = "Mean AQuA score (0–5)") + plot_theme() + theme(legend.position = "none")
}

make_aqua_dimension_plot <- function(aqua_obj) {
  df <- aqua_obj$dimensions
  if (is.null(df) || nrow(df) == 0) return(ggplot() + theme_void())
  df$dimension <- factor(df$dimension, levels = df$dimension[order(df$contribution, decreasing = TRUE)])
  ggplot(df, aes(x = dimension, y = contribution, fill = contribution > 0)) + geom_col() + coord_flip() +
    labs(title = "AQuA dimension contributions", x = NULL, y = "Weight × prevalence") + plot_theme() + theme(legend.position = "none")
}

make_age_pie <- function(df) {
  if (is.null(df) || !"age" %in% names(df)) return(NULL)
  ages <- as.numeric(scalar_text_vec(df$age))
  bins <- cut(ages, breaks = c(-Inf, 25, 40, 65, Inf),
              labels = c("16-25", "26-40", "41-65", "65+"))
  tbl <- as.data.frame(table(bins), stringsAsFactors = FALSE)
  names(tbl) <- c("category", "n")
  tbl <- tbl[tbl$n > 0, ]
  tbl$pct <- round(100 * tbl$n / sum(tbl$n))
  tbl$label <- paste0(tbl$category, " (", tbl$pct, "%)")
  pal <- c("#7dc87d", "#5b8abf", "#c8a12d", "#d4735e")
  ggplot(tbl, aes(x = "", y = n, fill = label)) +
    geom_col(width = 1, color = "white", linewidth = 0.5) +
    coord_polar("y") + scale_fill_manual(values = pal[seq_len(nrow(tbl))]) +
    labs(title = "Age Distribution") + pie_theme()
}

# ============================================================
# PROCESS ANALYTICS
# ============================================================
make_wordcount_bar <- function(turns_df) {
  if (is.null(turns_df) || nrow(turns_df) == 0 || !"word_count" %in% names(turns_df)) {
    return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "No process data", color = "#7d8596", size = 5) +
             theme_void() + theme(plot.background = element_rect(fill = "#ffffff", color = NA)))
  }
  summary_df <- turns_df |> group_by(speaker) |>
    summarise(total_words = sum(word_count, na.rm = TRUE), n_turns = n(), avg_words = round(total_words / n_turns), .groups = "drop") |>
    arrange(desc(total_words))
  summary_df$speaker <- factor(summary_df$speaker, levels = summary_df$speaker)
  ggplot(summary_df, aes(x = speaker, y = total_words)) +
    geom_col(fill = "#5b8abf", alpha = 0.8) +
    geom_text(aes(label = total_words), vjust = -0.4, color = "#2b4c7e", size = 3) +
    labs(title = "Total Words per Participant", x = NULL, y = "Total word count") +
    plot_theme() + theme(axis.text.x = element_text(angle = 40, hjust = 1, size = 8))
}

make_turns_per_phase <- function(turns_df) {
  if (is.null(turns_df) || nrow(turns_df) == 0) {
    return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "No process data", color = "#7d8596", size = 5) +
             theme_void() + theme(plot.background = element_rect(fill = "#ffffff", color = NA)))
  }
  summary_df <- turns_df |> group_by(speaker, phase) |>
    summarise(total_words = sum(word_count, na.rm = TRUE), .groups = "drop")
  summary_df$phase <- factor(summary_df$phase, levels = c("Values", "Options", "Evaluation"))
  ggplot(summary_df, aes(x = speaker, y = total_words, fill = phase)) +
    geom_col(position = "stack", alpha = 0.85) +
    scale_fill_manual(values = c("Values" = "#c8a12d", "Options" = "#5b8abf", "Evaluation" = "#2f855a")) +
    labs(title = "Contribution by Phase", x = NULL, y = "Word count", fill = "Phase") +
    plot_theme() + theme(axis.text.x = element_text(angle = 40, hjust = 1, size = 8))
}

make_participation_equity <- function(turns_df) {
  if (is.null(turns_df) || nrow(turns_df) == 0 || !"word_count" %in% names(turns_df)) {
    return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "No data", color = "#7d8596", size = 5) +
             theme_void() + theme(plot.background = element_rect(fill = "#ffffff", color = NA)))
  }
  by_speaker <- turns_df |> group_by(speaker) |> summarise(total = sum(word_count, na.rm = TRUE), .groups = "drop")
  ideal <- sum(by_speaker$total) / nrow(by_speaker)
  by_speaker$deviation <- round(100 * (by_speaker$total - ideal) / ideal)
  by_speaker$color <- ifelse(by_speaker$deviation >= 0, "above", "below")
  by_speaker$speaker <- factor(by_speaker$speaker, levels = by_speaker$speaker[order(by_speaker$deviation)])
  ggplot(by_speaker, aes(x = speaker, y = deviation, fill = color)) +
    geom_col(alpha = 0.8) + geom_hline(yintercept = 0, color = "#4b5567") +
    scale_fill_manual(values = c("above" = "#c8a12d", "below" = "#5b8abf"), guide = "none") +
    labs(title = "Participation Equity", subtitle = "% deviation from equal share of total words",
         x = NULL, y = "% deviation from equal") +
    plot_theme() + theme(axis.text.x = element_text(angle = 40, hjust = 1, size = 8))
}

make_cross_reference_heatmap <- function(turns_df) {
  if (is.null(turns_df) || nrow(turns_df) == 0 || !"text" %in% names(turns_df)) {
    return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "No data", color = "#7d8596", size = 5) +
             theme_void() + theme(plot.background = element_rect(fill = "#ffffff", color = NA)))
  }
  speakers <- unique(turns_df$speaker)
  mat <- matrix(0, nrow = length(speakers), ncol = length(speakers),
                dimnames = list(speakers, speakers))
  for (i in seq_len(nrow(turns_df))) {
    from <- turns_df$speaker[i]
    txt <- tolower(turns_df$text[i] %||% "")
    for (to in speakers) {
      if (to != from && grepl(tolower(to), txt, fixed = TRUE)) {
        mat[from, to] <- mat[from, to] + 1
      }
      # Also check first names
      first_name <- strsplit(to, " ")[[1]][1]
      if (to != from && nchar(first_name) > 2 && grepl(tolower(first_name), txt, fixed = TRUE)) {
        mat[from, to] <- mat[from, to] + 1
      }
    }
  }
  ref_df <- expand.grid(from = speakers, to = speakers, stringsAsFactors = FALSE)
  ref_df$mentions <- vapply(seq_len(nrow(ref_df)), function(i) mat[ref_df$from[i], ref_df$to[i]], numeric(1))
  if (max(ref_df$mentions) == 0) {
    return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "No cross-references detected", color = "#7d8596", size = 5) +
             theme_void() + theme(plot.background = element_rect(fill = "#ffffff", color = NA)))
  }
  ggplot(ref_df, aes(x = to, y = from, fill = mentions)) +
    geom_tile(color = "white") +
    geom_text(aes(label = ifelse(mentions > 0, mentions, "")), size = 3, color = "#1f2330") +
    scale_fill_gradient(low = "#f7f7fb", high = "#c8a12d") +
    labs(title = "Cross-References Between Speakers", subtitle = "How often each speaker mentioned another",
         x = "Referenced", y = "Speaker", fill = "Mentions") +
    plot_theme() + theme(axis.text.x = element_text(angle = 40, hjust = 1, size = 8))
}

compute_process_stats <- function(turns_df, n_participants) {
  if (is.null(turns_df) || nrow(turns_df) == 0 || !"word_count" %in% names(turns_df)) {
    return(list(total_turns = 0, total_words = 0, avg_words_per_turn = 0,
                gini = 0, most_active = "-", least_active = "-"))
  }
  by_sp <- turns_df |> group_by(speaker) |> summarise(tw = sum(word_count, na.rm = TRUE), .groups = "drop")
  total_words <- sum(by_sp$tw)
  # Gini coefficient for participation equity
  sorted <- sort(by_sp$tw)
  n <- length(sorted)
  gini <- if (n > 1 && total_words > 0) {
    round(sum((2 * seq_along(sorted) - n - 1) * sorted) / (n * total_words), 2)
  } else 0
  list(
    total_turns = nrow(turns_df),
    total_words = total_words,
    avg_words_per_turn = round(total_words / nrow(turns_df)),
    gini = gini,
    most_active = by_sp$speaker[which.max(by_sp$tw)],
    least_active = by_sp$speaker[which.min(by_sp$tw)]
  )
}

make_survey_comparison <- function(pre_survey, post_survey) {
  if (is.null(pre_survey) || is.null(post_survey)) {
    return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "Survey data not available", color = "#7d8596", size = 5) +
             theme_void() + theme(plot.background = element_rect(fill = "#ffffff", color = NA)))
  }
  safe_extract <- function(survey_list) {
    bind_rows(lapply(survey_list, function(r) {
      resps <- r$responses
      if (is.null(resps) || length(resps) == 0) return(data.frame(question = character(), answer = character(), stringsAsFactors = FALSE))
      bind_rows(lapply(resps, function(q)
        data.frame(question = scalar_text(q$question %||% ""), answer = scalar_text(q$answer %||% ""), stringsAsFactors = FALSE)))
    }))
  }
  pre_df  <- safe_extract(pre_survey)
  post_df <- safe_extract(post_survey)
  if (nrow(pre_df) == 0 || nrow(post_df) == 0) {
    return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "Could not parse survey responses", color = "#7d8596", size = 5) +
             theme_void() + theme(plot.background = element_rect(fill = "#ffffff", color = NA)))
  }
  pre_s  <- pre_df  |> group_by(question) |> summarise(n_unique = n_distinct(answer), .groups = "drop") |> mutate(timing = "Pre")
  post_s <- post_df |> group_by(question) |> summarise(n_unique = n_distinct(answer), .groups = "drop") |> mutate(timing = "Post")
  comb <- bind_rows(pre_s, post_s)
  comb$question <- make.unique(substr(comb$question, 1, 50), sep = " ")
  ggplot(comb, aes(x = question, y = n_unique, fill = timing)) +
    geom_col(position = "dodge") +
    labs(title = "Answer Diversity: Pre vs Post Deliberation", x = NULL, y = "Distinct answers", fill = "Timing") +
    plot_theme() + theme(axis.text.x = element_text(angle = 35, hjust = 1, size = 8))
}

# --- Survey report helper: extract full labelled data frame ---
survey_to_df <- function(survey_list, timing, persona_names = NULL) {
  bind_rows(lapply(seq_along(survey_list), function(idx) {
    r <- survey_list[[idx]]
    nm <- scalar_text(r$respondent %||% "")
    if (!nzchar(trimws(nm)) || nm == "name")
      nm <- if (!is.null(persona_names) && idx <= length(persona_names)) persona_names[idx] else paste0("Agent ", idx)
    resps <- r$responses
    if (is.null(resps) || length(resps) == 0)
      return(data.frame(respondent = nm, timing = timing, question = "[none]", answer = "", justification = "", stringsAsFactors = FALSE))
    bind_rows(lapply(resps, function(q) data.frame(
      respondent = nm, timing = timing,
      question = scalar_text(q$question %||% ""),
      answer = scalar_text(q$answer %||% ""),
      justification = scalar_text(q$justification %||% ""),
      stringsAsFactors = FALSE)))
  }))
}

# Plot: per-respondent answer change count (how many answers changed pre->post)
make_survey_change_per_respondent <- function(pre_df, post_df) {
  merged <- inner_join(
    pre_df |> select(respondent, question, answer_pre = answer),
    post_df |> select(respondent, question, answer_post = answer),
    by = c("respondent", "question"))
  if (nrow(merged) == 0) {
    return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "Cannot match pre/post responses", color = "#7d8596", size = 5) +
             theme_void() + theme(plot.background = element_rect(fill = "#ffffff", color = NA)))
  }
  merged$changed <- merged$answer_pre != merged$answer_post
  summary_df <- merged |> group_by(respondent) |>
    summarise(total = n(), changed = sum(changed), pct_changed = round(100 * changed / total), .groups = "drop") |>
    arrange(desc(pct_changed))
  summary_df$respondent <- factor(summary_df$respondent, levels = summary_df$respondent)
  ggplot(summary_df, aes(x = respondent, y = pct_changed)) +
    geom_col(fill = "#c8a12d", alpha = 0.8) +
    geom_text(aes(label = paste0(pct_changed, "%")), vjust = -0.4, color = "#946f00", size = 3) +
    scale_y_continuous(limits = c(0, 105)) +
    labs(title = "Opinion Shift per Participant", subtitle = "% of survey answers that changed after deliberation",
         x = NULL, y = "% answers changed") +
    plot_theme() + theme(axis.text.x = element_text(angle = 40, hjust = 1, size = 8))
}

# Plot: per-question consensus change (did answers converge or diverge?)
make_survey_consensus_shift <- function(pre_df, post_df) {
  pre_q  <- pre_df  |> group_by(question) |> summarise(n_unique_pre  = n_distinct(answer), .groups = "drop")
  post_q <- post_df |> group_by(question) |> summarise(n_unique_post = n_distinct(answer), .groups = "drop")
  merged <- inner_join(pre_q, post_q, by = "question")
  if (nrow(merged) == 0) {
    return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "Cannot match pre/post questions", color = "#7d8596", size = 5) +
             theme_void() + theme(plot.background = element_rect(fill = "#ffffff", color = NA)))
  }
  merged$shift <- merged$n_unique_post - merged$n_unique_pre
  merged$direction <- ifelse(merged$shift < 0, "Convergence", ifelse(merged$shift > 0, "Divergence", "No change"))
  merged$qlabel <- paste0("Q", seq_len(nrow(merged)), ": ", substr(merged$question, 1, 40))
  merged$qlabel <- factor(merged$qlabel, levels = merged$qlabel[order(merged$shift)])
  ggplot(merged, aes(x = qlabel, y = shift, fill = direction)) +
    geom_col(alpha = 0.8) +
    geom_hline(yintercept = 0, linetype = "solid", color = "#4b5567") +
    scale_fill_manual(values = c("Convergence" = "#2f855a", "Divergence" = "#c53030", "No change" = "#a0aec0")) +
    labs(title = "Consensus Shift per Question", subtitle = "Negative = opinions converged; Positive = opinions diverged",
         x = NULL, y = "Change in distinct answers", fill = NULL) +
    plot_theme() + theme(axis.text.x = element_text(angle = 40, hjust = 1, size = 8))
}

# Plot: overall summary KPIs for survey
compute_survey_summary <- function(pre_df, post_df) {
  merged <- inner_join(
    pre_df |> select(respondent, question, answer_pre = answer),
    post_df |> select(respondent, question, answer_post = answer),
    by = c("respondent", "question"))
  if (nrow(merged) == 0) return(list(n_respondents = 0, n_questions = 0, pct_changed = 0, convergence = 0))
  n_respondents <- n_distinct(merged$respondent)
  n_questions <- n_distinct(merged$question)
  pct_changed <- round(100 * mean(merged$answer_pre != merged$answer_post))
  pre_diversity <- pre_df |> group_by(question) |> summarise(d = n_distinct(answer), .groups = "drop")
  post_diversity <- post_df |> group_by(question) |> summarise(d = n_distinct(answer), .groups = "drop")
  avg_shift <- mean(post_diversity$d, na.rm = TRUE) - mean(pre_diversity$d, na.rm = TRUE)
  convergence <- ifelse(avg_shift < 0, "Yes", ifelse(avg_shift > 0, "No", "Neutral"))
  list(n_respondents = n_respondents, n_questions = n_questions, pct_changed = pct_changed, convergence = convergence)
}
# ============================================================
# DRI & CENTRAL TENDENCY PLOTS
# ============================================================

make_dri_scatter <- function(pairwise_df, title = "DRI Plot", group_dri = NA_real_) {
  if (is.null(pairwise_df) || nrow(pairwise_df) == 0) {
    return(
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "No DRI data available", color = "#7d8596", size = 5) +
        theme_void()
    )
  }
  
  ggplot(pairwise_df, aes(x = consideration_consistency, y = preference_consistency)) +
    xlim(-1.01, 1.01) + ylim(-1.01, 1.01) +
    geom_jitter(width = 0.03, height = 0.03, alpha = 0.65, size = 1.8) +
    geom_abline(intercept = 0, slope = 1, colour = "#c8a12d", linewidth = 1) +
    geom_hline(yintercept = 0, color = "#d9dee8", linewidth = 0.5) +
    geom_vline(xintercept = 0, color = "#d9dee8", linewidth = 0.5) +
    geom_density_2d_filled(alpha = 0.40) +
    annotate(
      "text",
      x = -0.95, y = 0.95, hjust = 0,
      label = paste0("DRI = ", round(group_dri, 2)),
      color = "#b33a2f", size = 5, fontface = "italic"
    ) +
    labs(
      title = title,
      x = "Consistency - Considerations",
      y = "Consistency - Preferences"
    ) +
    plot_theme() +
    theme(legend.position = "none")
}
compute_dri_prepost_wilcox <- function(pre_scores, post_scores) {
  if (is.null(pre_scores) || is.null(post_scores)) return(NULL)
  if (is.null(pre_scores$pairwise) || is.null(post_scores$pairwise)) return(NULL)
  
  merged <- inner_join(
    pre_scores$pairwise[, c("respondent_1", "respondent_2", "IC_point")],
    post_scores$pairwise[, c("respondent_1", "respondent_2", "IC_point")],
    by = c("respondent_1", "respondent_2"),
    suffix = c("_pre", "_post")
  )
  
  merged <- merged[
    is.finite(merged$IC_point_pre) & is.finite(merged$IC_point_post),
    ,
    drop = FALSE
  ]
  
  if (nrow(merged) == 0) return(NULL)
  
  one_tailed <- tryCatch(
    wilcox.test(merged$IC_point_post, merged$IC_point_pre, paired = TRUE, alternative = "greater"),
    error = function(e) NULL
  )
  
  two_sided <- tryCatch(
    wilcox.test(merged$IC_point_post, merged$IC_point_pre, paired = TRUE, alternative = "two.sided"),
    error = function(e) NULL
  )
  
  list(
    n_pairs = nrow(merged),
    p_greater = if (!is.null(one_tailed)) one_tailed$p.value else NA_real_,
    p_twosided = if (!is.null(two_sided)) two_sided$p.value else NA_real_,
    merged = merged
  )
}
make_dri_change_plot <- function(pre_df, post_df) {
  if (is.null(pre_df) || is.null(post_df)) {
    return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "No pre/post DRI data", color = "#7d8596", size = 5) +
             theme_void())
  }
  
  merged <- inner_join(
    pre_df[, c("respondent", "DRI")],
    post_df[, c("respondent", "DRI")],
    by = "respondent",
    suffix = c("_pre", "_post")
  )
  
  if (nrow(merged) == 0) {
    return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "Cannot match pre/post DRI", color = "#7d8596", size = 5) +
             theme_void())
  }
  
  merged$delta <- merged$DRI_post - merged$DRI_pre
  merged$respondent <- factor(merged$respondent, levels = merged$respondent[order(merged$delta)])
  
  ggplot(merged, aes(x = respondent, y = delta)) +
    geom_col(fill = "#5b8abf", alpha = 0.85) +
    geom_hline(yintercept = 0, color = "#4b5567") +
    labs(
      title = "Change in DRI by Participant",
      x = NULL,
      y = "Post - Pre DRI"
    ) +
    plot_theme() +
    theme(axis.text.x = element_text(angle = 40, hjust = 1, size = 8))
}
make_dri_opinion_boxplot <- function(response_list, survey_obj, title = "DRI Consideration Scores") {
  df <- dri_raw_to_df(response_list, survey_obj)
  df <- df[df$section == "consideration" & is.finite(df$value), , drop = FALSE]
  
  if (nrow(df) == 0) {
    return(
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "No DRI consideration data", color = "#7d8596", size = 5) +
        theme_void()
    )
  }
  
  ggplot(df, aes(x = timing, y = value)) +
    geom_boxplot(alpha = 0.5, outlier.alpha = 0.4) +
    labs(
      title = title,
      x = NULL,
      y = "Agreement score"
    ) +
    plot_theme()
}

make_dri_preference_barplot <- function(response_list, survey_obj, title = "DRI Preference Rankings") {
  df <- dri_raw_to_df(response_list, survey_obj)
  df <- df[df$section == "preference" & is.finite(df$value), , drop = FALSE]
  
  if (nrow(df) == 0) {
    return(
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "No DRI ranking data", color = "#7d8596", size = 5) +
        theme_void()
    )
  }
  
  summ <- df |>
    group_by(timing, text) |>
    summarise(mean_rank = mean(value, na.rm = TRUE), .groups = "drop")
  
  ggplot(summ, aes(x = reorder(text, mean_rank), y = mean_rank, fill = timing)) +
    geom_col(position = "dodge", alpha = 0.85) +
    coord_flip() +
    labs(
      title = title,
      x = NULL,
      y = "Average rank (lower = more preferred)",
      fill = NULL
    ) +
    plot_theme()
}
make_central_tendency_plot <- function(personas_df, baseline = NA_real_) {
  if (is.null(personas_df) || !"initial_score" %in% names(personas_df) || !"final_score" %in% names(personas_df)) {
    return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "No central tendency data", color = "#7d8596", size = 5) +
             theme_void())
  }
  
  df <- bind_rows(
    data.frame(stage = "Pre", score = as.numeric(personas_df$initial_score)),
    data.frame(stage = "Post", score = as.numeric(personas_df$final_score))
  )
  
  p <- ggplot(df, aes(x = score, fill = stage)) +
    geom_density(alpha = 0.35) +
    scale_x_continuous(limits = c(1, 5), breaks = 1:5) +
    labs(
      title = "Central Tendency / Distribution Compression",
      x = "Position score",
      y = "Density",
      fill = NULL
    ) +
    plot_theme()
  
  if (is.finite(baseline)) {
    p <- p + geom_vline(xintercept = baseline, linetype = "dashed", color = "#d4735e", linewidth = 1) +
      annotate("text", x = baseline, y = Inf, label = "Model baseline", vjust = 1.2, color = "#d4735e", size = 3.5)
  }
  
  p
}

make_mean_shift_plot <- function(ct_df) {
  if (is.null(ct_df) || nrow(ct_df) == 0) {
    return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "No central tendency summary", color = "#7d8596", size = 5) +
             theme_void())
  }
  
  df <- data.frame(
    point = c("Pre Mean", "Post Mean", "Model Baseline"),
    value = c(ct_df$pre_mean[1], ct_df$post_mean[1], ct_df$baseline[1])
  )
  df <- df[is.finite(df$value), ]
  
  ggplot(df, aes(x = point, y = value)) +
    geom_point(size = 4) +
    geom_line(aes(group = 1), linewidth = 1) +
    scale_y_continuous(limits = c(1, 5), breaks = 1:5) +
    labs(
      title = "Collective Mean Shift Toward Baseline",
      x = NULL,
      y = "Position score"
    ) +
    plot_theme()
}
# ============================================================
# UI
# ============================================================
ui <- fluidPage(
  useShinyjs(),
  tags$head(tags$title("Deliberation Simulator"), tags$style(HTML(APP_CSS))),
  tags$div(id = "sidebar",
           tags$div(class = "sb-header", tags$div(class = "sb-sub", "Deliberation"), tags$h2(class = "sb-title", "Simulator")),
           tags$div(class = "sb-section", "Workflow"), uiOutput("sidebar_steps"),
           tags$div(class = "sb-section", "Outputs"), uiOutput("sidebar_outputs")),
  tags$div(id = "main-content",
           tags$div(class = "api-bar",
                    tags$div(style = "flex:2;min-width:220px;", tags$label("OpenRouter API Key *"), passwordInput("api_key", label = NULL, placeholder = "sk-or-...")),
                    tags$div(style = "flex:1;min-width:240px;", tags$label("OpenRouter Model"), uiOutput("model_ui")),
                    tags$div(style = "min-width:160px;", numericInput("temperature", "Temperature", value = 0.3, min = 0, max = 1.0, step = 0.05)),
                    tags$div(style = "min-width:170px;", tags$button(class = "btn-output", onclick = "Shiny.setInputValue('refresh_models', Math.random(), {priority:'event'})", "Refresh Models")),
                    tags$div(style = "min-width:150px;", tags$button(class = "btn-output", onclick = "Shiny.setInputValue('test_key_click', Math.random(), {priority:'event'})", "Test Key"))),
           tags$div(class = "small-note", style = "margin:-14px 0 16px 0; padding:0 24px;",
                    "Recommended models for reliable JSON output: openai/gpt-4o, openai/gpt-4o-mini, anthropic/claude-sonnet-4-5, anthropic/claude-3-5-haiku, google/gemini-2.0-flash-001. Smaller or free-tier models may produce gibberish."),
           uiOutput("status_ui"), uiOutput("error_ui"), uiOutput("main_ui"))
)

# ============================================================
# SERVER
# ============================================================
server <- function(input, output, session) {
  
  rv <- reactiveValues(
    models = DEFAULT_MODELS, view = "step", step = 0, loading = FALSE,
    error = "", status = "", evidence_text = "", evidence_names = character(0),
    survey_text = "", survey_enabled = FALSE,
    personas = NULL, integrity = NULL, agent_cards = NULL, groups = NULL,
    group_values = NULL, master_values = NULL, group_options = NULL, master_options = NULL,
    evaluations = NULL, final_report = NULL, appendix_report = NULL,
    quality = NULL, transcript = NULL, turns = NULL,
    rankings = NULL, persona_validated = FALSE,
    model_baseline = NULL,
    drift_table = NULL,
    pre_survey = NULL, post_survey = NULL,
    auto_dri_survey = NULL,
    user_dri_survey = NULL,
    
    auto_dri_pre = NULL,
    auto_dri_post = NULL,
    user_dri_pre = NULL,
    user_dri_post = NULL,
    
    auto_dri_pre_scores = NULL,
    auto_dri_post_scores = NULL,
    user_dri_pre_scores = NULL,
    user_dri_post_scores = NULL,
    
    auto_dri_comparison = NULL,
    user_dri_comparison = NULL,
    central_tendency = NULL,
    aqua = NULL,
    batch_individual = data.frame(), batch_group = data.frame(),
    batch_running = FALSE, batch_status = "", batch_run_counter = 0L,
    replog = data.frame(timestamp = character(), stage = character(), model = character(),
                        temperature = numeric(), participants = integer(), groups = integer(), stringsAsFactors = FALSE))
  
  get_turns <- function() {
    t <- rv$turns
    if (is.null(t) || !is.data.frame(t) || nrow(t) == 0)
      data.frame(turn = integer(), speaker = character(), sentiment_score = numeric(),
                 position_score = numeric(), shift_reason = character(),
                 phase = character(), group = character(),
                 text = character(), word_count = integer(), stringsAsFactors = FALSE)
    else t
  }
  
  add_replog <- function(stage_name) {
    rv$replog <- bind_rows(rv$replog, data.frame(
      timestamp = make_timestamp(), stage = stage_name, model = input$model %||% "",
      temperature = as.numeric(input$temperature %||% 0.3),
      participants = as.integer(input$n_participants %||% NA),
      groups = as.integer(input$n_groups %||% NA), stringsAsFactors = FALSE))
  }
  
  output$model_ui <- renderUI({ selectInput("model", label = NULL, choices = rv$models, selected = rv$models[1]) })
  
  observeEvent(input$refresh_models, {
    tryCatch({
      mods <- fetch_openrouter_models(input$api_key); rv$models <- mods
      updateSelectInput(session, "model", choices = mods, selected = mods[1])
      rv$status <- paste0("Loaded ", length(mods), " models."); rv$error <- ""
    }, error = function(e) {
      rv$models <- DEFAULT_MODELS; updateSelectInput(session, "model", choices = DEFAULT_MODELS, selected = DEFAULT_MODELS[1])
      rv$error <- conditionMessage(e)
    })
  })
  
  observeEvent(input$test_key_click, {
    rv$error <- ""; rv$status <- ""
    tryCatch({
      ans <- call_openrouter(clean_api_key(input$api_key), input$model,
                             list(list(role = "system", content = "You are a test assistant."), list(role = "user", content = "Reply with exactly OK")),
                             max_tokens = 10, temperature = as.numeric(input$temperature %||% 0))
      rv$status <- paste0("OpenRouter connection successful: ", ans)
    }, error = function(e) { rv$error <- conditionMessage(e) })
  })
  
  # Sidebar
  output$sidebar_steps <- renderUI({
    tagList(lapply(STEPS, function(s) {
      sid <- s$id; done <- if (sid == 0) rv$step > 0 else sid < rv$step
      active <- rv$view == "step" && rv$step == sid
      cls <- paste("step-item", if (active) "active" else if (done) "done" else "pending")
      tags$div(class = cls,
               onclick = if (done || sid == 0) paste0("Shiny.setInputValue('goto_step',", sid, ",{priority:'event'})") else NULL,
               tags$span(class = "step-icon", s$icon), tags$span(class = "step-label", s$label),
               if (done && !active) tags$span(class = "step-check", "\u2713") else NULL)
    }))
  })
  
  output$sidebar_outputs <- renderUI({
    outs <- list(list(id = "transcript", icon = "\U0001F4C4", label = "Transcript"),
                 list(id = "report", icon = "\U0001F4DD", label = "Final Report"),
                 list(id = "appendix", icon = "\U0001F4D1", label = "Appendix"),
                 list(id = "dashboard", icon = "\U0001F4CA", label = "Dashboard"),
                 list(id = "dri_analysis", icon = "\U0001F4C8", label = "DRI Analysis"),
                 list(id = "aqua_analysis", icon = "\U0001F4D8", label = "AQuA Analysis"))
    if (!is.null(rv$pre_survey) || !is.null(rv$post_survey))
      outs <- c(outs, list(list(id = "survey_report", icon = "\U0001F4CB", label = "Survey Report")))
    tagList(lapply(outs, function(o) {
      active <- rv$view == o$id; locked <- is.null(rv$final_report)
      cls <- paste("out-item", if (active) "active", if (locked) "locked")
      tags$div(class = cls,
               onclick = if (!locked) paste0("Shiny.setInputValue('goto_output','", o$id, "',{priority:'event'})") else NULL,
               tags$span(class = "step-icon", o$icon), tags$span(class = "step-label", o$label))
    }))
  })
  
  observeEvent(input$goto_step,   { rv$view <- "step"; rv$step <- input$goto_step })
  observeEvent(input$goto_output, { rv$view <- input$goto_output })
  
  output$status_ui <- renderUI({ if (nzchar(rv$status)) tags$div(class = "info-box", rv$status) })
  output$error_ui  <- renderUI({ if (nzchar(rv$error))  tags$div(class = "error-box", rv$error) })
  
  # Evidence & survey uploads
  observeEvent(input$evidence_files, {
    files <- input$evidence_files
    if (is.null(files) || nrow(files) == 0) { rv$evidence_text <- ""; rv$evidence_names <- character(0); return() }
    txts <- vapply(seq_len(nrow(files)), function(i) extract_text_from_file(files$datapath[i], files$name[i]), character(1))
    rv$evidence_text <- paste(paste0("FILE: ", files$name, "\n", txts), collapse = "\n\n----------------\n\n")
    rv$evidence_names <- files$name; rv$status <- paste0("Loaded ", nrow(files), " evidence file(s).")
  })
  
  observeEvent(input$survey_file, {
    f <- input$survey_file; if (is.null(f)) { rv$survey_text <- ""; return() }
    rv$survey_text <- extract_text_from_file(f$datapath, f$name); rv$survey_enabled <- TRUE
    rv$status <- paste0("Survey loaded: ", f$name)
  })
  
  # Required fields
  required_missing <- reactive({
    list(api_key = !nzchar(clean_api_key(input$api_key)),
         issue = !nzchar(trimws(input$policy_issue %||% "")),
         question = !nzchar(trimws(input$policy_question %||% "")))
  })
  highlight_required <- function() {
    miss <- required_missing()
    if (isTRUE(miss$api_key)) addClass("api_key", "missing") else removeClass("api_key", "missing")
    if (isTRUE(miss$issue)) addClass("policy_issue", "missing") else removeClass("policy_issue", "missing")
    if (isTRUE(miss$question)) addClass("policy_question", "missing") else removeClass("policy_question", "missing")
  }
  observe({ if (rv$step == 0) highlight_required() })
  
  # Demo warnings
  output$demo_age_warn <- renderUI({
    s <- sum(input$demo_age_16_25 %||% 25, input$demo_age_25_40 %||% 25, input$demo_age_40_65 %||% 25, input$demo_age_65p %||% 25)
    if (s != 100) tags$div(class = "demo-warn", paste0("Age percentages sum to ", s, "% (should be 100%)")) })
  output$demo_edu_warn <- renderUI({
    s <- sum(input$demo_edu_none %||% 5, input$demo_edu_primary %||% 10, input$demo_edu_apprentice %||% 20, input$demo_edu_highschool %||% 30, input$demo_edu_university %||% 35)
    if (s != 100) tags$div(class = "demo-warn", paste0("Education percentages sum to ", s, "% (should be 100%)")) })
  output$demo_ideo_warn <- renderUI({
    s <- sum(input$demo_ideo_ext_left %||% 10, input$demo_ideo_left %||% 20, input$demo_ideo_centre %||% 40, input$demo_ideo_right %||% 20, input$demo_ideo_ext_right %||% 10)
    if (s != 100) tags$div(class = "demo-warn", paste0("Ideology percentages sum to ", s, "% (should be 100%)")) })
  output$demo_gender_female_label <- renderText({ paste0("Female: ", 100 - (input$demo_gender_male %||% 50), "%") })
  output$demo_rural_label <- renderText({ paste0("Rural: ", 100 - (input$demo_urban %||% 50), "%") })
  
  output$main_ui <- renderUI({
    if (rv$view == "transcript") return(build_transcript_ui())
    if (rv$view == "report") return(build_report_ui())
    if (rv$view == "appendix") return(build_appendix_ui())
    if (rv$view == "dashboard") return(build_dashboard_ui())
    if (rv$view == "dri_analysis") return(build_dri_analysis_ui())
    if (rv$view == "aqua_analysis") return(build_aqua_analysis_ui())
    if (rv$view == "survey_report") return(build_survey_report_ui())
    build_step_ui()
  })
  
  # ====== STEP UI ======
  build_step_ui <- function() {
    s <- rv$step
    if (s == 0) {
      return(tagList(
        tags$div(class = "step-num", "Step 0"),
        tags$h1(class = "step-title", "Configure the Deliberation"),
        tags$p(class = "step-desc", "Set policy issue, participants, groups, temperature, evidence, demographics, and optionally a pre/post survey."),
        textInput("policy_issue", "Policy Issue *", placeholder = "e.g., banning social media under 16"),
        textInput("policy_question", "Policy Question *", placeholder = "e.g., Should the government ban social media for under-16s?"),
        
        tags$div(
          class = "demo-section",
          tags$h4("\U0001F3AF Deliberation Output Type"),
          tags$p(class = "small-note", "Choose the type of output this deliberation should produce. This shapes the final report and generates a detailed appendix."),
          radioButtons(
            "output_type", NULL,
            choices = c(
              "Policy Recommendation" = "policy_recommendation",
              "Existing Policy Support / Opposition" = "policy_support_opposition",
              "Idea Generation" = "idea_generation"
            ),
            selected = "policy_recommendation"
          ),
          conditionalPanel(
            condition = "input.output_type == 'policy_support_opposition'",
            textAreaInput(
              "existing_policy",
              "Describe the existing policy to evaluate",
              rows = 4,
              placeholder = "Describe the current policy, regulation, or legislation that the assembly should assess..."
            )
          )
        ),
        
        numericInput("n_participants", "Number of Participants", value = 10, min = 4, max = 60, step = 1),
        numericInput("n_groups", "Number of Groups (2\u201310)", value = 2, min = 2, max = 10, step = 1),
        
        tags$hr(),
        tags$h3(style = "color:#946f00;margin-bottom:4px;", "Socio-Demographic Composition"),
        tags$p(class = "small-note", "Set approximate %. Multi-category dimensions should sum to 100%."),
        
        tags$div(
          class = "demo-section",
          tags$h4("Gender"),
          tags$div(class = "demo-grid",
                   sliderInput("demo_gender_male", "% Male", min = 0, max = 100, value = 50, step = 5, width = "100%")),
          tags$p(class = "small-note", style = "margin-top:2px;", textOutput("demo_gender_female_label", inline = TRUE))
        ),
        
        tags$div(
          class = "demo-section",
          tags$h4("Age"),
          tags$div(
            class = "demo-grid",
            numericInput("demo_age_16_25", "% 16\u201325", value = 25, min = 0, max = 100, step = 5),
            numericInput("demo_age_25_40", "% 25\u201340", value = 25, min = 0, max = 100, step = 5),
            numericInput("demo_age_40_65", "% 40\u201365", value = 25, min = 0, max = 100, step = 5),
            numericInput("demo_age_65p", "% 65+", value = 25, min = 0, max = 100, step = 5)
          ),
          uiOutput("demo_age_warn")
        ),
        
        tags$div(
          class = "demo-section",
          tags$h4("Education"),
          tags$div(
            class = "demo-grid",
            numericInput("demo_edu_none", "% No formal", value = 5, min = 0, max = 100, step = 5),
            numericInput("demo_edu_primary", "% Primary school", value = 10, min = 0, max = 100, step = 5),
            numericInput("demo_edu_apprentice", "% Apprenticeship", value = 20, min = 0, max = 100, step = 5),
            numericInput("demo_edu_highschool", "% High school", value = 30, min = 0, max = 100, step = 5),
            numericInput("demo_edu_university", "% BA/MA university", value = 35, min = 0, max = 100, step = 5)
          ),
          uiOutput("demo_edu_warn")
        ),
        
        tags$div(
          class = "demo-section",
          tags$h4("Settlement"),
          tags$div(class = "demo-grid",
                   sliderInput("demo_urban", "% Urban", min = 0, max = 100, value = 50, step = 5, width = "100%")),
          tags$p(class = "small-note", style = "margin-top:2px;", textOutput("demo_rural_label", inline = TRUE))
        ),
        
        tags$div(
          class = "demo-section",
          tags$h4("Political Ideology"),
          tags$div(
            class = "demo-grid",
            numericInput("demo_ideo_ext_left", "% Extreme left", value = 10, min = 0, max = 100, step = 5),
            numericInput("demo_ideo_left", "% Left", value = 20, min = 0, max = 100, step = 5),
            numericInput("demo_ideo_centre", "% Centre", value = 40, min = 0, max = 100, step = 5),
            numericInput("demo_ideo_right", "% Right", value = 20, min = 0, max = 100, step = 5),
            numericInput("demo_ideo_ext_right", "% Extreme right", value = 10, min = 0, max = 100, step = 5)
          ),
          uiOutput("demo_ideo_warn")
        ),
        
        tags$hr(),
        
        tags$div(
          class = "survey-section",
          tags$h4("📋 Optional: Pre/Post Deliberation Survey"),
          tags$p(
            class = "small-note",
            "Upload a survey as PDF. Each agent will complete it before and after deliberation, enabling pre/post comparison on the Dashboard."
          ),
          fileInput("survey_file", "Upload survey (PDF)", accept = ".pdf"),
          
          tags$hr(),
          
          tags$h4("🧠 Optional: Independent DRI Survey"),
          tags$p(
            class = "small-note",
            "Upload an external DRI survey in CSV. If provided, the app will show both automated DRI results and uploaded-survey DRI results."
          ),
          tags$p(
            class = "small-note",
            "CSV must contain columns: section, id, text. Rows with 'consideration' will be rated on a 1–7 Likert scale. Rows with 'policy_option' will be ranked."
          ),
          tags$a(
            href = "DRI_survey_template.csv",
            "Download CSV template",
            target = "_blank"
          ),
          fileInput("dri_survey_file", "Upload independent DRI survey (CSV)", accept = c(".csv"))
        ),
        
        tags$hr(),
        
        textAreaInput(
          "persona_note",
          "Additional persona guidance (optional)",
          rows = 3,
          placeholder = "e.g., include rural parents, teenagers, school counsellors..."
        ),
        textInput(
          "info_type",
          "Type of Background Information",
          placeholder = "e.g., evidence summary, expert testimony"
        ),
        textAreaInput(
          "info_base",
          "Typed Background Information",
          rows = 8,
          placeholder = "Paste the evidence or context here..."
        ),
        fileInput(
          "evidence_files",
          "Upload supporting documents / evidence",
          multiple = TRUE,
          accept = c(".txt", ".md", ".csv", ".tsv", ".json", ".pdf")
        ),
        tags$div(
          class = "small-note",
          "Replicability is improved through explicit model selection, stored temperature, participant/group counts, and a run log."
        ),
        tags$button(
          class = "btn-start",
          onclick = "Shiny.setInputValue('begin_click', Math.random(), {priority:'event'})",
          "Begin Deliberation \u2192"
        ),
        tags$hr(),
        tags$div(
          class = "survey-section",
          tags$h4("Batch runs / concrete replications"),
          tags$p(class = "small-note", "Run the full app multiple times with the current settings. The app stores two cumulative CSV datasets: individual-level rows and group-level rows, with run_id identifying each replication."),
          numericInput("batch_n_runs", "Number of concrete runs", value = 1, min = 1, max = 50, step = 1),
          tags$div(class = "dl-row", actionButton("run_batch", "Run batch", class = "btn-output"), actionButton("clear_batch", "Clear batch data", class = "btn-ghost")),
          textOutput("batch_status_text"),
          tags$div(class = "dl-row", downloadButton("dl_batch_individual", "Download individual-level CSV"), downloadButton("dl_batch_group", "Download group-level CSV"), downloadButton("dl_batch_errors", "Download failed-runs log"))
        )
      ))
    }
    
    # Generic step rendering
    step_cfgs <- list(
      "1" = list(t = "Generate Personas", d = "Generate stakeholder personas matching your demographic targets.", btn = "Generate Personas"),
      "2" = list(t = "Validate Personas", d = "Check for missing perspectives or stakeholder groups and regenerate personas to fill gaps (per Rountree & Gastil).", btn = "Validate & Regenerate Personas"),
      "3" = list(t = "Design Integrity Review", d = "Assess representativeness, unbiased framing, and procedural design involvement.", btn = "Run Design Integrity Check"),
      "4" = list(t = "Assign Independent Agents", d = "Each persona becomes an agent card for independent model calls.", btn = "Assign Agents"),
      "5" = list(t = "Split Groups", d = "Split participants into groups for the values round.", btn = "Split Groups"),
      "6" = list(t = "Values Round", d = "Each agent contributes independently to identify values at stake.", btn = "Run Values Round"),
      "7" = list(t = "Master Values", d = "Merge group value lists into one master list.", btn = "Merge Values"),
      "8" = list(t = "Reshuffle Groups", d = "Change up participants in each group before the options round (per Rountree & Gastil).", btn = "Reshuffle Groups"),
      "9" = list(t = "Options Round", d = "Groups brainstorm policy options using evidence and master values.", btn = "Run Options Round"),
      "10" = list(t = "Master Options", d = "Merge all group option lists.", btn = "Merge Options"),
      "11" = list(t = "Reshuffle Groups", d = "Change up participants in each group before evaluation (per Rountree & Gastil).", btn = "Reshuffle Groups"),
      "12" = list(t = "Evaluation Round", d = "Groups evaluate options with pros, cons, tradeoffs linked to values.", btn = "Run Evaluation Round"),
      "13" = list(t = "Individual Rankings", d = "Each participant ranks options individually based on their values and perspective.", btn = "Run Individual Rankings"),
      "14" = list(t = "Final Recommendation", d = "Synthesize the final recommendation with ranked options.", btn = "Generate Final Recommendation"),
      "15" = list(t = "Quality Assessment", d = "Assess the simulation quality.", btn = "Run Quality Assessment"))
    
    cfg <- step_cfgs[[as.character(s)]]; if (is.null(cfg)) return(NULL)
    
    # Content
    cb <- NULL
    if (s == 1 && !is.null(rv$personas)) cb <- tagList(tags$div(class = "info-box", paste0(nrow(rv$personas), " personas generated.")), DTOutput("personas_table"))
    else if (s == 2 && rv$persona_validated) cb <- tagList(tags$div(class = "info-box", "Personas validated and regenerated with missing perspectives."), DTOutput("personas_table"))
    else if (s == 3 && !is.null(rv$integrity)) cb <- tags$div(class = "report-box", safe_json(rv$integrity))
    else if (s == 4 && !is.null(rv$agent_cards)) cb <- tags$div(class = "report-box", paste(vapply(rv$agent_cards, function(x) paste0(scalar_text(x$id), ": ", scalar_text(x$name), " \u2014 ", scalar_text(x$background)), character(1)), collapse = "\n\n"))
    else if (s == 5 && !is.null(rv$groups)) cb <- tags$div(class = "report-box", paste(vapply(names(rv$groups), function(g) paste0(g, ": ", paste(rv$personas$name[rv$groups[[g]]], collapse = ", ")), character(1)), collapse = "\n\n"))
    else if (s == 6 && !is.null(rv$group_values)) cb <- tags$div(class = "report-box", paste(vapply(names(rv$group_values), function(g) paste0(g, "\n", rv$group_values[[g]]$summary), character(1)), collapse = "\n\n---\n\n"))
    else if (s == 7 && !is.null(rv$master_values)) cb <- tags$div(class = "report-box", rv$master_values)
    else if (s %in% c(8, 11) && !is.null(rv$groups)) cb <- tagList(tags$div(class = "info-box", "Groups reshuffled."), tags$div(class = "report-box", paste(vapply(names(rv$groups), function(g) paste0(g, ": ", paste(rv$personas$name[rv$groups[[g]]], collapse = ", ")), character(1)), collapse = "\n\n")))
    else if (s == 9 && !is.null(rv$group_options)) cb <- tags$div(class = "report-box", paste(vapply(names(rv$group_options), function(g) paste0(g, "\n", rv$group_options[[g]]$summary), character(1)), collapse = "\n\n---\n\n"))
    else if (s == 10 && !is.null(rv$master_options)) cb <- tags$div(class = "report-box", rv$master_options)
    else if (s == 12 && !is.null(rv$evaluations)) cb <- tags$div(class = "report-box", paste(vapply(names(rv$evaluations), function(g) paste0(g, "\n", rv$evaluations[[g]]$summary), character(1)), collapse = "\n\n---\n\n"))
    else if (s == 13 && !is.null(rv$rankings)) cb <- tags$div(class = "report-box", safe_json(rv$rankings))
    else if (s == 14 && !is.null(rv$final_report)) cb <- tags$div(class = "report-box", rv$final_report)
    else if (s == 15 && !is.null(rv$quality)) {
      cb <- tagList(
        tags$div(class = "plot-wrap", plotOutput("plot_quality_step", height = "360px")),
        tagList(lapply(seq_along(QUALITY_DIMS), function(i) {
          d <- QUALITY_DIMS[i]; q <- rv$quality$quality[[d]]
          sc <- if (is.null(q$score)) "?" else scalar_text(q$score)
          nt <- if (is.null(q$notes)) "" else scalar_text(q$notes)
          tags$div(class = "qa-card", tags$h5(QUALITY_LABELS[i]), tags$span(class = "qa-score", sc), tags$div(class = "qa-notes", nt))
        })),
        if (!is.null(rv$quality$quality$recommendations))
          tags$div(class = "qa-card", tags$h5("Recommendations"),
                   tags$div(class = "qa-notes", paste("\u2022", vapply(rv$quality$quality$recommendations, scalar_text, character(1)), collapse = "\n"))))
    }
    
    done <- switch(as.character(s),
                   "1" = !is.null(rv$personas), "2" = rv$persona_validated, "3" = !is.null(rv$integrity),
                   "4" = !is.null(rv$agent_cards), "5" = !is.null(rv$groups) && is.null(rv$group_values),
                   "6" = !is.null(rv$group_values), "7" = !is.null(rv$master_values),
                   "8" = !is.null(rv$group_options), "9" = !is.null(rv$group_options),
                   "10" = !is.null(rv$master_options), "11" = !is.null(rv$evaluations),
                   "12" = !is.null(rv$evaluations), "13" = !is.null(rv$rankings),
                   "14" = !is.null(rv$final_report), "15" = !is.null(rv$quality), FALSE)
    
    btn <- if (!done) tags$button(class = "btn-run", onclick = paste0("Shiny.setInputValue('run_current',", s, ",{priority:'event'})"), cfg$btn)
    else if (s < 15) tags$button(class = "btn-next", onclick = paste0("Shiny.setInputValue('goto_step',", s + 1, ",{priority:'event'})"), "Next Step \u2192")
    else tags$button(class = "btn-output", onclick = "Shiny.setInputValue('goto_output','report',{priority:'event'})", "View Outputs \u2192")
    
    tagList(tags$div(class = "step-num", paste("Step", s)), tags$h1(class = "step-title", cfg$t), tags$p(class = "step-desc", cfg$d), cb, btn)
  }
  
  # ====== OUTPUT UIs ======
  build_transcript_ui <- function() {
    tagList(tags$div(class = "step-num", "OUTPUT"), tags$h1(class = "out-title", "\U0001F4C4 Transcript"),
            tags$p(class = "out-desc", "Full multi-agent transcript."),
            if (!is.null(rv$transcript)) tags$div(class = "report-box", rv$transcript),
            tags$h3("Replicability Log"), DTOutput("replog_table"),
            tags$div(class = "dl-row", downloadButton("dl_transcript_pdf", "Download Transcript PDF"), downloadButton("dl_csv", "Download CSV")))
  }
  
  build_report_ui <- function() {
    turns_df <- get_turns()
    proc <- compute_process_stats(turns_df, if (!is.null(rv$personas)) nrow(rv$personas) else 0)
    tagList(tags$div(class = "step-num", "OUTPUT"), tags$h1(class = "out-title", "\U0001F4DD Final Report"),
            tags$p(class = "out-desc", "Final recommendation, assembly composition, process analytics, design integrity, quality assessment."),
            if (!is.null(rv$final_report)) tags$div(class = "report-box", rv$final_report),
            
            # Assembly Composition in report
            if (!is.null(rv$personas)) tagList(
              tags$h3(style = "color:#946f00; margin-top:24px;", "Assembly Composition"),
              tags$div(style = "display:grid; grid-template-columns: repeat(auto-fill, minmax(280px, 1fr)); gap:16px;",
                       tags$div(class = "plot-wrap", plotOutput("pie_gender_rpt", height = "280px")),
                       tags$div(class = "plot-wrap", plotOutput("pie_age_rpt", height = "280px")),
                       tags$div(class = "plot-wrap", plotOutput("pie_education_rpt", height = "280px")),
                       tags$div(class = "plot-wrap", plotOutput("pie_settlement_rpt", height = "280px")),
                       tags$div(class = "plot-wrap", plotOutput("pie_ideology_rpt", height = "280px")),
                       tags$div(class = "plot-wrap", plotOutput("plot_ideology_alignment_rpt", height = "320px")))),
            
            # Process Analytics in report
            if (proc$total_turns > 0) tagList(
              tags$h3(style = "color:#946f00; margin-top:24px;", "Process Analytics"),
              tags$div(class = "kpi-grid",
                       tags$div(class = "kpi", tags$div(class = "kpi-value", proc$total_turns), tags$div(class = "kpi-label", "total turns")),
                       tags$div(class = "kpi", tags$div(class = "kpi-value", proc$total_words), tags$div(class = "kpi-label", "total words")),
                       tags$div(class = "kpi", tags$div(class = "kpi-value", proc$avg_words_per_turn), tags$div(class = "kpi-label", "avg words/turn")),
                       tags$div(class = "kpi", tags$div(class = "kpi-value", proc$gini), tags$div(class = "kpi-label", "gini (equity)"))),
              tags$div(class = "kpi-grid",
                       tags$div(class = "kpi", tags$div(class = "kpi-value", proc$most_active), tags$div(class = "kpi-label", "most active")),
                       tags$div(class = "kpi", tags$div(class = "kpi-value", proc$least_active), tags$div(class = "kpi-label", "least active"))),
              tags$div(class = "plot-wrap", plotOutput("plot_wordcount_rpt", height = "360px")),
              tags$div(class = "plot-wrap", plotOutput("plot_turns_phase_rpt", height = "360px")),
              tags$div(class = "plot-wrap", plotOutput("plot_equity_rpt", height = "340px")),
              tags$div(class = "plot-wrap", plotOutput("plot_crossref_rpt", height = "400px"))),
            # DRI analysis in report
            if (!is.null(rv$auto_dri_pre_scores) && !is.null(rv$auto_dri_post_scores)) tagList(
              tags$h3(style = "color:#946f00; margin-top:24px;", "Automated DRI"),
              tags$div(class = "kpi-grid",
                       tags$div(class = "kpi", tags$div(class = "kpi-value", round(rv$auto_dri_pre_scores$group_dri, 2)), tags$div(class = "kpi-label", "auto pre")),
                       tags$div(class = "kpi", tags$div(class = "kpi-value", round(rv$auto_dri_post_scores$group_dri, 2)), tags$div(class = "kpi-label", "auto post")),
                       tags$div(class = "kpi", tags$div(class = "kpi-value", round(rv$auto_dri_post_scores$group_dri - rv$auto_dri_pre_scores$group_dri, 2)), tags$div(class = "kpi-label", "delta")),
                       tags$div(class = "kpi", tags$div(class = "kpi-value",
                                                        if (!is.null(rv$auto_dri_wilcox)) signif(rv$auto_dri_wilcox$p_greater, 3) else NA),
                                tags$div(class = "kpi-label", "IC Wilcoxon p"))),
              tags$div(class = "plot-wrap", plotOutput("plot_auto_dri_pre", height = "360px")),
              tags$div(class = "plot-wrap", plotOutput("plot_auto_dri_post", height = "360px")),
              tags$div(class = "plot-wrap", plotOutput("plot_auto_dri_change", height = "340px")),
              tags$div(class = "plot-wrap", plotOutput("plot_auto_dri_opinion_box", height = "320px")),
              tags$div(class = "plot-wrap", plotOutput("plot_auto_dri_pref_bar", height = "420px")),
              DTOutput("auto_dri_table_dt")
            ),
            
            if (!is.null(rv$user_dri_pre_scores) && !is.null(rv$user_dri_post_scores)) tagList(
              tags$h3(style = "color:#946f00; margin-top:24px;", "Uploaded Independent DRI"),
              tags$div(class = "kpi-grid",
                       tags$div(class = "kpi", tags$div(class = "kpi-value", round(rv$user_dri_pre_scores$group_dri, 2)), tags$div(class = "kpi-label", "user pre")),
                       tags$div(class = "kpi", tags$div(class = "kpi-value", round(rv$user_dri_post_scores$group_dri, 2)), tags$div(class = "kpi-label", "user post")),
                       tags$div(class = "kpi", tags$div(class = "kpi-value", round(rv$user_dri_post_scores$group_dri - rv$user_dri_pre_scores$group_dri, 2)), tags$div(class = "kpi-label", "delta")),
                       tags$div(class = "kpi", tags$div(class = "kpi-value",
                                                        if (!is.null(rv$user_dri_wilcox)) signif(rv$user_dri_wilcox$p_greater, 3) else NA),
                                tags$div(class = "kpi-label", "IC Wilcoxon p"))),
              tags$div(class = "plot-wrap", plotOutput("plot_user_dri_pre", height = "360px")),
              tags$div(class = "plot-wrap", plotOutput("plot_user_dri_post", height = "360px")),
              tags$div(class = "plot-wrap", plotOutput("plot_user_dri_change", height = "340px")),
              tags$div(class = "plot-wrap", plotOutput("plot_user_dri_opinion_box", height = "320px")),
              tags$div(class = "plot-wrap", plotOutput("plot_user_dri_pref_bar", height = "420px")),
              DTOutput("user_dri_table_dt")
            ),
            # Design integrity
            if (!is.null(rv$integrity)) tags$div(class = "report-box", paste0("DESIGN INTEGRITY\n\n", safe_json(rv$integrity))),
            
            # Bias monitoring in report
            if (!is.null(rv$model_baseline)) tagList(
              tags$h3(style = "color:#946f00; margin-top:24px;", "\U0001F6A8 Model Bias Analysis"),
              tags$div(class = "kpi-grid",
                       tags$div(class = "kpi", tags$div(class = "kpi-value", scalar_text(rv$model_baseline$baseline$position_score %||% "?")), tags$div(class = "kpi-label", "model default bias")),
                       tags$div(class = "kpi", tags$div(class = "kpi-value", input$model %||% "?"), tags$div(class = "kpi-label", "model used"))),
              tags$div(class = "plot-wrap", plotOutput("plot_position_drift_rpt", height = "380px")),
              tags$div(class = "plot-wrap", plotOutput("plot_drift_summary_rpt", height = "340px"))),
            
            # Quality
            if (!is.null(rv$quality)) tagList(
              tags$h3(style = "color:#946f00; margin-top:24px;", "Quality Assessment"),
              tags$div(class = "plot-wrap", plotOutput("plot_quality_report", height = "360px")),
              tagList(lapply(seq_along(QUALITY_DIMS), function(i) {
                d <- QUALITY_DIMS[i]; q <- rv$quality$quality[[d]]
                sc <- if (is.null(q$score)) "?" else scalar_text(q$score)
                nt <- if (is.null(q$notes)) "" else scalar_text(q$notes)
                tags$div(class = "qa-card", tags$h5(QUALITY_LABELS[i]), tags$span(class = "qa-score", sc), tags$div(class = "qa-notes", nt))
              }))),
            
            tags$h3("Replicability Log"), DTOutput("replog_table_report"),
            
            # Appendix inline in report
            if (!is.null(rv$appendix_report)) tagList(
              tags$hr(),
              tags$h3(style = "color:#946f00; margin-top:24px;", "\U0001F4D1 Appendix"),
              tags$div(class = "report-box", rv$appendix_report)),
            
            tags$div(class = "dl-row", downloadButton("dl_report_pdf", "Download Report PDF"),
                     downloadButton("dl_full_report_pdf2", "Download Full Report + Appendix PDF"),
                     downloadButton("dl_csv2", "Download CSV"),
                     downloadButton("dl_plots_pdf", "Download Plots PDF")))
  }
  
  build_appendix_ui <- function() {
    output_type <- input$output_type %||% "policy_recommendation"
    type_label <- switch(output_type,
                         "policy_recommendation" = "Policy Recommendation",
                         "policy_support_opposition" = "Policy Support / Opposition",
                         "idea_generation" = "Idea Generation")
    tagList(
      tags$div(class = "step-num", "OUTPUT"),
      tags$h1(class = "out-title", "\U0001F4D1 Appendix"),
      tags$p(class = "out-desc", paste0("Detailed appendix for deliberation type: ", type_label, ".")),
      tags$div(class = "kpi-grid",
               tags$div(class = "kpi", tags$div(class = "kpi-value", type_label), tags$div(class = "kpi-label", "output type")),
               tags$div(class = "kpi", tags$div(class = "kpi-value", if (!is.null(rv$personas)) nrow(rv$personas) else "-"), tags$div(class = "kpi-label", "participants")),
               tags$div(class = "kpi", tags$div(class = "kpi-value", input$n_groups %||% "-"), tags$div(class = "kpi-label", "groups"))),
      if (!is.null(rv$appendix_report)) tags$div(class = "report-box", rv$appendix_report)
      else tags$div(class = "info-box", "Appendix will be generated after Step 10 (Final Recommendation)."),
      tags$div(class = "dl-row",
               downloadButton("dl_appendix_pdf", "Download Appendix PDF"),
               downloadButton("dl_full_report_pdf", "Download Full Report + Appendix PDF")))
  }
  
  build_dashboard_ui <- function() {
    req(!is.null(rv$personas))
    qual_score <- if (is.null(rv$quality) || is.null(rv$quality$quality$overall_score)) "-" else scalar_text(rv$quality$quality$overall_score)
    turns_df <- get_turns()
    proc <- compute_process_stats(turns_df, nrow(rv$personas))
    tagList(tags$div(class = "step-num", "OUTPUT"), tags$h1(class = "out-title", "\U0001F4CA Dashboard"),
            tags$p(class = "out-desc", "Assembly composition, deliberation process analytics, opinion dynamics, and quality."),
            
            # KPI row
            tags$div(class = "kpi-grid",
                     tags$div(class = "kpi", tags$div(class = "kpi-value", nrow(rv$personas)), tags$div(class = "kpi-label", "participants")),
                     tags$div(class = "kpi", tags$div(class = "kpi-value", input$n_groups), tags$div(class = "kpi-label", "groups")),
                     tags$div(class = "kpi", tags$div(class = "kpi-value", proc$total_turns), tags$div(class = "kpi-label", "total turns")),
                     tags$div(class = "kpi", tags$div(class = "kpi-value", proc$total_words), tags$div(class = "kpi-label", "total words"))),
            
            tags$div(class = "kpi-grid",
                     tags$div(class = "kpi", tags$div(class = "kpi-value", proc$avg_words_per_turn), tags$div(class = "kpi-label", "avg words/turn")),
                     tags$div(class = "kpi", tags$div(class = "kpi-value", proc$gini), tags$div(class = "kpi-label", "gini (equity)")),
                     tags$div(class = "kpi", tags$div(class = "kpi-value", input$temperature), tags$div(class = "kpi-label", "temperature")),
                     tags$div(class = "kpi", tags$div(class = "kpi-value", qual_score), tags$div(class = "kpi-label", "quality score"))),
            
            # Assembly Composition
            tags$h3(style = "color:#946f00; margin-top:24px;", "Assembly Composition"),
            tags$div(style = "display:grid; grid-template-columns: repeat(auto-fill, minmax(280px, 1fr)); gap:16px;",
                     tags$div(class = "plot-wrap", plotOutput("pie_gender", height = "280px")),
                     tags$div(class = "plot-wrap", plotOutput("pie_age", height = "280px")),
                     tags$div(class = "plot-wrap", plotOutput("pie_education", height = "280px")),
                     tags$div(class = "plot-wrap", plotOutput("pie_settlement", height = "280px")),
                     tags$div(class = "plot-wrap", plotOutput("pie_ideology", height = "280px")),
                     tags$div(class = "plot-wrap", plotOutput("plot_ideology_alignment", height = "320px")),
                     tags$div(class = "plot-wrap", DTOutput("table_ideology_alignment"))),
            
            # Process Analytics
            tags$h3(style = "color:#946f00; margin-top:24px;", "Process Analytics"),
            tags$div(class = "plot-wrap", plotOutput("plot_wordcount", height = "360px"),
                     tags$div(class = "dl-row", style = "margin-top:6px;", downloadButton("dl_plot_wordcount", "⬇ PNG", class = "btn-ghost"))),
            tags$div(class = "plot-wrap", plotOutput("plot_turns_phase", height = "360px")),
            tags$div(class = "plot-wrap", plotOutput("plot_equity", height = "340px"),
                     tags$div(class = "dl-row", style = "margin-top:6px;", downloadButton("dl_plot_equity", "⬇ PNG", class = "btn-ghost"))),
            tags$div(class = "plot-wrap", plotOutput("plot_crossref", height = "400px"),
                     tags$div(class = "dl-row", style = "margin-top:6px;", downloadButton("dl_plot_crossref", "⬇ PNG", class = "btn-ghost"))),
            
            # Opinion Dynamics
            tags$h3(style = "color:#946f00; margin-top:24px;", "Opinion Dynamics"),
            tags$div(class = "plot-wrap", plotOutput("plot_opinion", height = "360px"),
                     tags$div(class = "dl-row", style = "margin-top:6px;", downloadButton("dl_plot_opinion", "⬇ PNG", class = "btn-ghost"))),
            tags$div(class = "plot-wrap", plotOutput("plot_polar", height = "320px"),
                     tags$div(class = "dl-row", style = "margin-top:6px;", downloadButton("dl_plot_polar", "⬇ PNG", class = "btn-ghost"))),
            tags$div(class = "plot-wrap", plotOutput("plot_sentiment", height = "320px"),
                     tags$div(class = "dl-row", style = "margin-top:6px;", downloadButton("dl_plot_sentiment", "⬇ PNG", class = "btn-ghost"))),
            # Central tendency / Taubenfeld
            tags$h3(style = "color:#946f00; margin-top:24px;", "Central Tendency & Bias Drift"),
            tags$p(class = "small-note", style = "margin-bottom:12px;",
                   "This section makes Taubenfeld-style convergence visible: are agents compressing toward a common center, and is that center moving toward the model's default baseline?"),
            if (!is.null(rv$central_tendency)) tags$div(
              class = "kpi-grid",
              tags$div(class = "kpi", tags$div(class = "kpi-value", round(rv$central_tendency$baseline[1], 2)), tags$div(class = "kpi-label", "model baseline")),
              tags$div(class = "kpi", tags$div(class = "kpi-value", round(rv$central_tendency$pre_mean[1], 2)), tags$div(class = "kpi-label", "pre mean")),
              tags$div(class = "kpi", tags$div(class = "kpi-value", round(rv$central_tendency$post_mean[1], 2)), tags$div(class = "kpi-label", "post mean")),
              tags$div(class = "kpi", tags$div(class = "kpi-value", round(rv$central_tendency$shift_to_baseline[1], 2)), tags$div(class = "kpi-label", "shift to baseline"))
            ),
            if (!is.null(rv$central_tendency)) tags$div(
              class = "kpi-grid",
              tags$div(class = "kpi", tags$div(class = "kpi-value", round(rv$central_tendency$pre_sd[1], 2)), tags$div(class = "kpi-label", "pre sd")),
              tags$div(class = "kpi", tags$div(class = "kpi-value", round(rv$central_tendency$post_sd[1], 2)), tags$div(class = "kpi-label", "post sd")),
              tags$div(class = "kpi", tags$div(class = "kpi-value", round(rv$central_tendency$compression_sd[1], 2)), tags$div(class = "kpi-label", "sd compression")),
              tags$div(class = "kpi", tags$div(class = "kpi-value", round(rv$central_tendency$compression_pairwise[1], 2)), tags$div(class = "kpi-label", "pairwise compression"))
            ),
            tags$div(class = "plot-wrap", plotOutput("plot_central_tendency", height = "340px"),
                     tags$div(class = "dl-row", style = "margin-top:6px;", downloadButton("dl_plot_central_tendency", "⬇ PNG", class = "btn-ghost"))),
            tags$div(class = "plot-wrap", plotOutput("plot_mean_shift", height = "300px")),
            # Bias Monitoring (per Taubenfeld et al. 2024)
            tags$h3(style = "color:#946f00; margin-top:24px;", "\U0001F6A8 Bias Monitor (Taubenfeld et al. 2024)"),
            tags$p(class = "small-note", style = "margin-bottom:12px;",
                   "LLM agents tend to converge toward the model's inherent bias regardless of assigned persona. The dashed red line shows the model's default position (probed without persona). Watch for agents drifting toward this line."),
            if (!is.null(rv$model_baseline)) tags$div(class = "kpi-grid",
                                                      tags$div(class = "kpi", tags$div(class = "kpi-value", scalar_text(rv$model_baseline$baseline$position_score %||% "?")), tags$div(class = "kpi-label", "model default bias")),
                                                      tags$div(class = "kpi", tags$div(class = "kpi-value", scalar_text(rv$model_baseline$baseline$reasoning %||% "")), tags$div(class = "kpi-label", "reasoning"))),
            tags$div(class = "plot-wrap", plotOutput("plot_position_drift", height = "380px"),
                     tags$div(class = "dl-row", style = "margin-top:6px;", downloadButton("dl_plot_position_drift", "⬇ PNG", class = "btn-ghost"))),
            tags$div(class = "plot-wrap", plotOutput("plot_drift_summary", height = "340px"),
                     tags$div(class = "dl-row", style = "margin-top:6px;", downloadButton("dl_plot_drift_summary", "⬇ PNG", class = "btn-ghost"))),
            if (!is.null(rv$drift_table) && nrow(rv$drift_table) > 0) tagList(
              tags$h4(style = "color:#946f00;", "Drift Detail Table"),
              DTOutput("drift_table_dt")),
            
            # Quality
            if (!is.null(rv$quality)) tagList(
              tags$h3(style = "color:#946f00; margin-top:24px;", "Quality Assessment"),
              tags$div(class = "plot-wrap", plotOutput("plot_quality_dash", height = "360px"))),
            # DRI
            # DRI
            tags$h3(style = "color:#946f00; margin-top:24px;", "Deliberative Reasoning Index (DRI)"),
            tags$p(
              class = "small-note",
              style = "margin-bottom:12px;",
              "This section shows the automated DRI survey generated by the app and, if uploaded, the independent DRI survey. Both question sets and both result sets are displayed."
            ),
            
            # -------------------------
            # AUTOMATED DRI SURVEY
            # -------------------------
            if (!is.null(rv$auto_dri_survey)) tagList(
              tags$h4(style = "color:#946f00;", "Automated DRI Survey Questions"),
              DTOutput("auto_dri_survey_table")
            ),
            
            if (!is.null(rv$auto_dri_pre_scores) && !is.null(rv$auto_dri_post_scores)) tagList(
              tags$h4(style = "color:#946f00; margin-top:18px;", "Automated DRI Results"),
              tags$div(
                class = "kpi-grid",
                tags$div(class = "kpi",
                         tags$div(class = "kpi-value", round(rv$auto_dri_pre_scores$group_dri, 2)),
                         tags$div(class = "kpi-label", "auto dri pre")),
                tags$div(class = "kpi",
                         tags$div(class = "kpi-value", round(rv$auto_dri_post_scores$group_dri, 2)),
                         tags$div(class = "kpi-label", "auto dri post")),
                tags$div(class = "kpi",
                         tags$div(class = "kpi-value", round(rv$auto_dri_post_scores$group_dri - rv$auto_dri_pre_scores$group_dri, 2)),
                         tags$div(class = "kpi-label", "auto delta")),
                tags$div(class = "kpi",
                         tags$div(class = "kpi-value",
                                  if (!is.null(rv$auto_dri_wilcox)) signif(rv$auto_dri_wilcox$p_greater, 3) else NA),
                         tags$div(class = "kpi-label", "IC Wilcoxon p")),
              ),
              tags$div(class = "plot-wrap", plotOutput("plot_auto_dri_pre", height = "360px")),
              tags$div(class = "plot-wrap", plotOutput("plot_auto_dri_post", height = "360px")),
              tags$div(class = "plot-wrap", plotOutput("plot_auto_dri_change", height = "340px")),
              tags$div(class = "plot-wrap", plotOutput("plot_auto_dri_opinion_box", height = "320px")),
              tags$div(class = "plot-wrap", plotOutput("plot_auto_dri_pref_bar", height = "420px")),
              tags$h4(style = "color:#946f00;", "Automated DRI Detail Table"),
              DTOutput("auto_dri_table_dt")
            ),
            tags$div(
              class = "dl-row",
              downloadButton("dl_auto_dri_questions", "Download Auto DRI Questions"),
              downloadButton("dl_auto_dri_pre_raw", "Download Auto DRI Pre Raw"),
              downloadButton("dl_auto_dri_post_raw", "Download Auto DRI Post Raw")
            ),
            # -------------------------
            # UPLOADED USER DRI SURVEY
            # -------------------------
            if (!is.null(rv$user_dri_survey)) tagList(
              tags$h4(style = "color:#946f00; margin-top:24px;", "Uploaded Independent DRI Survey Questions"),
              DTOutput("user_dri_survey_table")
            ),
            
            if (!is.null(rv$user_dri_pre_scores) && !is.null(rv$user_dri_post_scores)) tagList(
              tags$h4(style = "color:#946f00; margin-top:18px;", "Uploaded Independent DRI Results"),
              tags$div(
                class = "kpi-grid",
                tags$div(class = "kpi",
                         tags$div(class = "kpi-value", round(rv$user_dri_pre_scores$group_dri, 2)),
                         tags$div(class = "kpi-label", "user dri pre")),
                tags$div(class = "kpi",
                         tags$div(class = "kpi-value", round(rv$user_dri_post_scores$group_dri, 2)),
                         tags$div(class = "kpi-label", "user dri post")),
                tags$div(class = "kpi",
                         tags$div(class = "kpi-value", round(rv$user_dri_post_scores$group_dri - rv$user_dri_pre_scores$group_dri, 2)),
                         tags$div(class = "kpi-label", "user delta")),
                tags$div(class = "kpi",
                         tags$div(class = "kpi-value",
                                  if (!is.null(rv$user_dri_wilcox)) signif(rv$user_dri_wilcox$p_greater, 3) else NA),
                         tags$div(class = "kpi-label", "IC Wilcoxon p")),
              ),
              tags$div(class = "plot-wrap", plotOutput("plot_user_dri_pre", height = "360px")),
              tags$div(class = "plot-wrap", plotOutput("plot_user_dri_post", height = "360px")),
              tags$div(class = "plot-wrap", plotOutput("plot_user_dri_change", height = "340px")),
              tags$div(class = "plot-wrap", plotOutput("plot_user_dri_opinion_box", height = "320px")),
              tags$div(class = "plot-wrap", plotOutput("plot_user_dri_pref_bar", height = "420px")),
              tags$h4(style = "color:#946f00;", "Uploaded DRI Detail Table"),
              DTOutput("user_dri_table_dt")
            ),
            tags$div(
              class = "dl-row",
              downloadButton("dl_user_dri_questions", "Download Uploaded DRI Questions"),
              downloadButton("dl_user_dri_pre_raw", "Download Uploaded DRI Pre Raw"),
              downloadButton("dl_user_dri_post_raw", "Download Uploaded DRI Post Raw")
            ),
            # Survey
            if (!is.null(rv$pre_survey) && !is.null(rv$post_survey)) tagList(
              tags$h3(style = "color:#946f00; margin-top:24px;", "Survey Pre/Post"),
              tags$div(class = "plot-wrap", plotOutput("plot_survey", height = "360px"))),
            if (!is.null(rv$pre_survey)) tagList(tags$h3("Pre-Deliberation Survey"), DTOutput("pre_survey_table")),
            if (!is.null(rv$post_survey)) tagList(tags$h3("Post-Deliberation Survey"), DTOutput("post_survey_table")),
            
            tags$div(class = "dl-row", downloadButton("dl_report_pdf2", "Download Report PDF"),
                     downloadButton("dl_transcript_pdf2", "Download Transcript PDF"), downloadButton("dl_csv3", "Download CSV")))
  }
  
  build_dri_analysis_ui <- function() {
    tagList(
      tags$div(class = "step-num", "OUTPUT"),
      tags$h1(class = "out-title", "📈 DRI Analysis"),
      tags$p(class = "out-desc", "Dedicated DRI page with automated and uploaded DRI plots, tables, and CSV downloads."),
      if (!is.null(rv$auto_dri_survey)) tagList(
        tags$h3(style = "color:#946f00;", "Automated DRI Survey Questions"),
        DTOutput("auto_dri_survey_table")
      ),
      tags$h3(style = "color:#946f00; margin-top:24px;", "Automated DRI"),
      if (!is.null(rv$auto_dri_pre_scores)) tagList(
        tags$div(class = "kpi-grid",
                 tags$div(class = "kpi", tags$div(class = "kpi-value", round(rv$auto_dri_pre_scores$group_dri, 2)), tags$div(class = "kpi-label", "auto pre")),
                 tags$div(class = "kpi", tags$div(class = "kpi-value", if (!is.null(rv$auto_dri_post_scores)) round(rv$auto_dri_post_scores$group_dri, 2) else "NA"), tags$div(class = "kpi-label", "auto post")),
                 tags$div(class = "kpi", tags$div(class = "kpi-value", if (!is.null(rv$auto_dri_post_scores)) round(rv$auto_dri_post_scores$group_dri - rv$auto_dri_pre_scores$group_dri, 2) else "NA"), tags$div(class = "kpi-label", "delta")),
                 tags$div(class = "kpi", tags$div(class = "kpi-value", if (!is.null(rv$auto_dri_wilcox)) signif(rv$auto_dri_wilcox$p_greater, 3) else NA), tags$div(class = "kpi-label", "IC Wilcoxon p")))
      ),
      tags$div(class = "plot-wrap", plotOutput("plot_auto_dri_pre", height = "360px"),
               tags$div(class = "dl-row", style = "margin-top:8px;", downloadButton("dl_plot_auto_dri_pre", "⬇ PNG", class = "btn-ghost"))),
      tags$div(class = "plot-wrap", plotOutput("plot_auto_dri_post", height = "360px"),
               tags$div(class = "dl-row", style = "margin-top:8px;", downloadButton("dl_plot_auto_dri_post", "⬇ PNG", class = "btn-ghost"))),
      tags$div(class = "plot-wrap", plotOutput("plot_auto_dri_change", height = "340px"),
               tags$div(class = "dl-row", style = "margin-top:8px;", downloadButton("dl_plot_auto_dri_change", "⬇ PNG", class = "btn-ghost"))),
      tags$div(class = "plot-wrap", plotOutput("plot_auto_dri_opinion_box", height = "320px")),
      tags$div(class = "plot-wrap", plotOutput("plot_auto_dri_pref_bar", height = "420px")),
      if (!is.null(rv$auto_dri_comparison)) tagList(
        tags$h4(style = "color:#946f00;", "Automated DRI Detail Table"),
        DTOutput("auto_dri_table_dt")
      ),
      tags$div(class = "dl-row",
               downloadButton("dl_auto_dri_questions", "Download Auto DRI Questions"),
               downloadButton("dl_auto_dri_pre_raw", "Download Auto DRI Pre Raw"),
               downloadButton("dl_auto_dri_post_raw", "Download Auto DRI Post Raw"),
               downloadButton("dl_auto_dri_comparison", "Download Auto DRI Comparison"),
               downloadButton("dl_auto_dri_all", "Download Auto DRI All"),
               downloadButton("dl_auto_dri_wide", "📥 Download DRI Wide CSV (name/stage/C1..Cn/P)")),
      tags$hr(),
      if (!is.null(rv$user_dri_survey)) tagList(
        tags$h3(style = "color:#946f00; margin-top:24px;", "Uploaded Independent DRI Survey Questions"),
        DTOutput("user_dri_survey_table")
      ),
      tags$h3(style = "color:#946f00; margin-top:24px;", "Uploaded Independent DRI"),
      if (!is.null(rv$user_dri_pre_scores)) tagList(
        tags$div(class = "kpi-grid",
                 tags$div(class = "kpi", tags$div(class = "kpi-value", round(rv$user_dri_pre_scores$group_dri, 2)), tags$div(class = "kpi-label", "user pre")),
                 tags$div(class = "kpi", tags$div(class = "kpi-value", if (!is.null(rv$user_dri_post_scores)) round(rv$user_dri_post_scores$group_dri, 2) else "NA"), tags$div(class = "kpi-label", "user post")),
                 tags$div(class = "kpi", tags$div(class = "kpi-value", if (!is.null(rv$user_dri_post_scores)) round(rv$user_dri_post_scores$group_dri - rv$user_dri_pre_scores$group_dri, 2) else "NA"), tags$div(class = "kpi-label", "delta")),
                 tags$div(class = "kpi", tags$div(class = "kpi-value", if (!is.null(rv$user_dri_wilcox)) signif(rv$user_dri_wilcox$p_greater, 3) else NA), tags$div(class = "kpi-label", "IC Wilcoxon p")))
      ),
      tags$div(class = "plot-wrap", plotOutput("plot_user_dri_pre", height = "360px"),
               tags$div(class = "dl-row", style = "margin-top:8px;", downloadButton("dl_plot_user_dri_pre", "⬇ PNG", class = "btn-ghost"))),
      tags$div(class = "plot-wrap", plotOutput("plot_user_dri_post", height = "360px"),
               tags$div(class = "dl-row", style = "margin-top:8px;", downloadButton("dl_plot_user_dri_post", "⬇ PNG", class = "btn-ghost"))),
      tags$div(class = "plot-wrap", plotOutput("plot_user_dri_change", height = "340px"),
               tags$div(class = "dl-row", style = "margin-top:8px;", downloadButton("dl_plot_user_dri_change", "⬇ PNG", class = "btn-ghost"))),
      tags$div(class = "plot-wrap", plotOutput("plot_user_dri_opinion_box", height = "320px")),
      tags$div(class = "plot-wrap", plotOutput("plot_user_dri_pref_bar", height = "420px")),
      if (!is.null(rv$user_dri_comparison)) tagList(
        tags$h4(style = "color:#946f00;", "Uploaded DRI Detail Table"),
        DTOutput("user_dri_table_dt")
      ),
      tags$div(class = "dl-row",
               downloadButton("dl_user_dri_questions", "Download Uploaded DRI Questions"),
               downloadButton("dl_user_dri_pre_raw", "Download Uploaded DRI Pre Raw"),
               downloadButton("dl_user_dri_post_raw", "Download Uploaded DRI Post Raw"),
               downloadButton("dl_user_dri_comparison", "Download Uploaded DRI Comparison"),
               downloadButton("dl_user_dri_wide", "📥 Download DRI Wide CSV (name/stage/C1..Cn/P)"))
    )
  }
  
  build_aqua_analysis_ui <- function() {
    dim_cards <- lapply(names(AQUA_DIMENSIONS), function(group_name) {
      dims <- AQUA_DIMENSIONS[[group_name]]
      tags$div(
        class = "card",
        style = "margin-bottom:16px;",
        tags$h3(style = "margin-top:0;", group_name),
        tags$ul(style = "margin-bottom:0;", lapply(dims, tags$li))
      )
    })
    
    pdf_candidates <- c(file.path(getwd(), "www", "aqua_analysis.pdf"), file.path(getwd(), "aqua_analysis.pdf"))
    pdf_exists <- any(file.exists(pdf_candidates))
    pdf_src <- if (file.exists(file.path(getwd(), "www", "aqua_analysis.pdf"))) "aqua_analysis.pdf" else NULL
    
    tagList(
      tags$div(class = "step-num", "AQuA"),
      tags$h1(class = "step-title", "AQuA Analysis"),
      tags$p(class = "step-desc", "AQuA scores are computed by asking the LLM to classify each comment on the 20 binary deliberative quality dimensions defined by Behrendt et al. (2024), then applying their exact published weights (Table 1) and 0–5 normalisation formula. This replaces the original paper's German-language adapter models with the same LLM driving the simulation, using temperature=0 for deterministic scoring."),
      if (!is.null(rv$aqua)) tags$div(
        class = "kpi-grid",
        tags$div(class = "kpi", tags$div(class = "kpi-value", round(rv$aqua$overview$mean_aqua[1], 2)), tags$div(class = "kpi-label", "mean AQuA")),
        tags$div(class = "kpi", tags$div(class = "kpi-value", round(rv$aqua$overview$median_aqua[1], 2)), tags$div(class = "kpi-label", "median AQuA")),
        tags$div(class = "kpi", tags$div(class = "kpi-value", rv$aqua$overview$n_comments[1]), tags$div(class = "kpi-label", "comments scored")),
        tags$div(class = "kpi", tags$div(class = "kpi-value", length(unique(rv$aqua$comments$speaker))), tags$div(class = "kpi-label", "participants"))
      ),
      tags$div(class = "card", tags$h3(style = "margin-top:0;", "What the AQuA score does"), tags$p("AQuA is an additive deliberative-quality score for individual comments. It combines predictions on 20 deliberative aspects, weights them by how strongly each aspect aligns with non-expert perceptions of deliberativeness, and normalizes the result to a 0–5 scale."), HTML(AQUA_FORMULA_HTML)),
      if (!is.null(rv$aqua)) tagList(
        tags$div(class = "dl-row",
                 downloadButton("dl_aqua_report_pdf", "Download AQuA Report PDF", class = "btn-output"),
                 downloadButton("dl_aqua_csv", "Download AQuA CSV", class = "btn-output"),
                 if (pdf_exists) downloadButton("dl_aqua_pdf", "Download AQuA PDF", class = "btn-output")),
        tags$div(class = "plot-wrap", plotOutput("plot_aqua_speaker", height = "340px"),
                 tags$div(class = "dl-row", style = "margin-top:6px;", downloadButton("dl_plot_aqua_speaker", "⬇ PNG", class = "btn-ghost"))),
        tags$div(class = "plot-wrap", plotOutput("plot_aqua_phase", height = "320px"),
                 tags$div(class = "dl-row", style = "margin-top:6px;", downloadButton("dl_plot_aqua_phase", "⬇ PNG", class = "btn-ghost"))),
        tags$div(class = "plot-wrap", plotOutput("plot_aqua_dimensions", height = "420px"),
                 tags$div(class = "dl-row", style = "margin-top:6px;", downloadButton("dl_plot_aqua_dimensions", "⬇ PNG", class = "btn-ghost"))),
        tags$h3(style = "color:#946f00; margin-top:20px;", "AQuA dimension table"),
        DTOutput("table_aqua_dimensions"),
        tags$h3(style = "color:#946f00; margin-top:20px;", "Comment-level AQuA scores"),
        DTOutput("table_aqua_comments")
      ) else tags$div(class = "info-box", "Run at least one deliberation round to generate AQuA outputs."),
      tags$div(style = "display:grid;grid-template-columns:repeat(auto-fit,minmax(220px,1fr));gap:16px;", dim_cards),
      tags$div(class = "card", tags$h3(style = "margin-top:0;", "Interpretation notes"), tags$ul(
        tags$li("Higher AQuA scores indicate more deliberative comments on the paper's weighted multidimensional conception of deliberativeness."),
        tags$li("Dimension scoring uses the same LLM running the simulation, prompted with the exact binary definitions from Behrendt et al. (2024), at temperature=0 for determinism."),
        tags$li("The original paper uses fine-tuned German-language adapters (Falk & Lapesa 2023 architecture) trained on human-annotated data. This implementation applies the same dimension definitions and formula via LLM prompting — a closer approximation than keyword rules, but still not identical to trained adapter predictions."),
        tags$li("Weights are taken verbatim from Table 1 of the paper: positive weights (Justification, Solution Proposals, Relevance) increase the score; negative weights (Sarcasm, Opinion, Question) decrease it.")
      )),
      tags$div(class = "card",
               tags$div(style = "display:flex;justify-content:space-between;align-items:center;gap:12px;flex-wrap:wrap;", tags$h3(style = "margin:0;", "Embedded AQuA paper")),
               if (isTRUE(pdf_exists) && !is.null(pdf_src)) {
                 tags$iframe(src = pdf_src, style = "width:100%;height:1000px;border:1px solid #d8dce6;border-radius:14px;background:white;")
               } else {
                 tags$div(class = "small-note", "The AQuA PDF was not found in a displayable app directory. The AQuA report still works, but embed view requires aqua_analysis.pdf in a www folder.")
               })
    )
  }
  
  build_survey_report_ui <- function() {
    pn <- if (!is.null(rv$personas)) rv$personas$name else NULL
    has_pre  <- !is.null(rv$pre_survey)
    has_post <- !is.null(rv$post_survey)
    has_both <- has_pre && has_post
    
    # Build data frames for plots
    pre_df  <- if (has_pre)  survey_to_df(rv$pre_survey, "Pre", pn)  else NULL
    post_df <- if (has_post) survey_to_df(rv$post_survey, "Post", pn) else NULL
    summary <- if (has_both) compute_survey_summary(pre_df, post_df) else NULL
    
    tagList(
      tags$div(class = "step-num", "OUTPUT"),
      tags$h1(class = "out-title", "\U0001F4CB Survey Report"),
      tags$p(class = "out-desc", "Pre/post deliberation survey analysis: opinion shifts, consensus changes, and full response comparison."),
      
      # KPI row
      if (!is.null(summary)) tags$div(class = "kpi-grid",
                                      tags$div(class = "kpi", tags$div(class = "kpi-value", summary$n_respondents), tags$div(class = "kpi-label", "respondents")),
                                      tags$div(class = "kpi", tags$div(class = "kpi-value", summary$n_questions), tags$div(class = "kpi-label", "questions")),
                                      tags$div(class = "kpi", tags$div(class = "kpi-value", paste0(summary$pct_changed, "%")), tags$div(class = "kpi-label", "answers changed")),
                                      tags$div(class = "kpi", tags$div(class = "kpi-value", summary$convergence), tags$div(class = "kpi-label", "convergence?"))
      ),
      
      # Answer diversity plot (existing)
      if (has_both) tagList(
        tags$h3(style = "color:#946f00;", "Answer Diversity: Pre vs Post"),
        tags$div(class = "plot-wrap", plotOutput("plot_survey_diversity_sr", height = "380px"))
      ),
      
      # Opinion shift per respondent
      if (has_both) tagList(
        tags$h3(style = "color:#946f00;", "Opinion Shift per Participant"),
        tags$div(class = "plot-wrap", plotOutput("plot_survey_change_sr", height = "380px"))
      ),
      
      # Consensus shift per question
      if (has_both) tagList(
        tags$h3(style = "color:#946f00;", "Consensus Shift per Question"),
        tags$div(class = "plot-wrap", plotOutput("plot_survey_consensus_sr", height = "380px"))
      ),
      
      # Pre-survey table
      if (has_pre) tagList(
        tags$h3(style = "color:#946f00;", "Pre-Deliberation Responses"),
        DTOutput("pre_survey_table_sr")
      ),
      
      # Post-survey table
      if (has_post) tagList(
        tags$h3(style = "color:#946f00;", "Post-Deliberation Responses"),
        DTOutput("post_survey_table_sr")
      ),
      
      # Combined comparison table
      if (has_both) tagList(
        tags$h3(style = "color:#946f00;", "Side-by-Side Comparison"),
        DTOutput("survey_comparison_table_sr")
      ),
      
      # Downloads
      tags$div(class = "dl-row",
               downloadButton("dl_survey_pdf", "Download Survey Report PDF"),
               downloadButton("dl_survey_csv", "Download Survey Data CSV"))
    )
  }
  
  # ====== TABLES & PLOTS ======
  output$personas_table <- renderDT({ req(rv$personas); datatable(rv$personas, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE) })
  output$replog_table <- renderDT({ datatable(rv$replog, options = list(pageLength = 20, scrollX = TRUE), rownames = FALSE) })
  output$replog_table_report <- renderDT({ datatable(rv$replog, options = list(pageLength = 20, scrollX = TRUE), rownames = FALSE) })
  # ============================================================
  # DRI / CENTRAL TENDENCY OUTPUTS
  # ============================================================
  
  # ============================================================
  # AUTO DRI OUTPUTS
  # ============================================================
  
  output$plot_auto_dri_pre <- renderPlot({
    pairwise <- if (!is.null(rv$auto_dri_pre_scores)) rv$auto_dri_pre_scores$pairwise else NULL
    group_dri <- if (!is.null(rv$auto_dri_pre_scores)) rv$auto_dri_pre_scores$group_dri else NA_real_
    make_dri_scatter(pairwise, "Automated DRI — Pre Deliberation", group_dri)
  }, bg = "#ffffff")
  
  output$plot_auto_dri_post <- renderPlot({
    pairwise <- if (!is.null(rv$auto_dri_post_scores)) rv$auto_dri_post_scores$pairwise else NULL
    group_dri <- if (!is.null(rv$auto_dri_post_scores)) rv$auto_dri_post_scores$group_dri else NA_real_
    make_dri_scatter(pairwise, "Automated DRI — Post Deliberation", group_dri)
  }, bg = "#ffffff")
  
  output$plot_user_dri_pre <- renderPlot({
    pairwise <- if (!is.null(rv$user_dri_pre_scores)) rv$user_dri_pre_scores$pairwise else NULL
    group_dri <- if (!is.null(rv$user_dri_pre_scores)) rv$user_dri_pre_scores$group_dri else NA_real_
    make_dri_scatter(pairwise, "Uploaded DRI — Pre Deliberation", group_dri)
  }, bg = "#ffffff")
  
  output$plot_user_dri_post <- renderPlot({
    pairwise <- if (!is.null(rv$user_dri_post_scores)) rv$user_dri_post_scores$pairwise else NULL
    group_dri <- if (!is.null(rv$user_dri_post_scores)) rv$user_dri_post_scores$group_dri else NA_real_
    make_dri_scatter(pairwise, "Uploaded DRI — Post Deliberation", group_dri)
  }, bg = "#ffffff")
  
  output$plot_auto_dri_change <- renderPlot({
    pre_df <- if (!is.null(rv$auto_dri_pre_scores)) rv$auto_dri_pre_scores$individual else NULL
    post_df <- if (!is.null(rv$auto_dri_post_scores)) rv$auto_dri_post_scores$individual else NULL
    make_dri_change_plot(pre_df, post_df)
  }, bg = "#ffffff")
  
  
  
  output$auto_dri_table_dt <- renderDT({
    req(rv$auto_dri_comparison)
    datatable(rv$auto_dri_comparison, options = list(pageLength = 15, scrollX = TRUE), rownames = FALSE)
  })
  
  output$auto_dri_survey_table <- renderDT({
    req(rv$auto_dri_survey)
    datatable(dri_questions_to_df(rv$auto_dri_survey), options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })
  
  output$plot_auto_dri_opinion_box <- renderPlot({
    survey <- rv$auto_dri_survey
    if (is.null(survey)) {
      ggplot() + annotate("text", x = 0.5, y = 0.5, label = "No DRI survey data", color = "#7d8596", size = 5) + theme_void()
    } else {
      pre_df  <- if (!is.null(rv$auto_dri_pre))  dri_raw_to_df(rv$auto_dri_pre,  survey, "Pre")  else data.frame()
      post_df <- if (!is.null(rv$auto_dri_post)) dri_raw_to_df(rv$auto_dri_post, survey, "Post") else data.frame()
      combined <- dplyr::bind_rows(pre_df, post_df)
      combined <- combined[combined$section == "consideration" & is.finite(combined$value), , drop = FALSE]
      if (nrow(combined) == 0) {
        ggplot() + annotate("text", x = 0.5, y = 0.5, label = "No DRI consideration data", color = "#7d8596", size = 5) + theme_void()
      } else {
        ggplot(combined, aes(x = timing, y = value, fill = timing)) +
          geom_boxplot(alpha = 0.55, outlier.alpha = 0.4) +
          scale_fill_manual(values = c("Pre" = "#5b8abf", "Post" = "#c8a12d")) +
          labs(title = "Automated DRI — Consideration Agreement Levels Pre/Post", x = NULL, y = "Agreement score (1–5)", fill = NULL) +
          plot_theme()
      }
    }
  }, bg = "#ffffff")
  
  output$plot_auto_dri_pref_bar <- renderPlot({
    survey <- rv$auto_dri_survey
    if (is.null(survey)) {
      ggplot() + annotate("text", x = 0.5, y = 0.5, label = "No DRI survey data", color = "#7d8596", size = 5) + theme_void()
    } else {
      pre_df  <- if (!is.null(rv$auto_dri_pre))  dri_raw_to_df(rv$auto_dri_pre,  survey, "Pre")  else data.frame()
      post_df <- if (!is.null(rv$auto_dri_post)) dri_raw_to_df(rv$auto_dri_post, survey, "Post") else data.frame()
      combined <- dplyr::bind_rows(pre_df, post_df)
      combined <- combined[combined$section == "preference" & is.finite(combined$value), , drop = FALSE]
      if (nrow(combined) == 0) {
        ggplot() + annotate("text", x = 0.5, y = 0.5, label = "No DRI ranking data", color = "#7d8596", size = 5) + theme_void()
      } else {
        summ <- combined |> dplyr::group_by(timing, text) |> dplyr::summarise(mean_rank = mean(value, na.rm = TRUE), .groups = "drop")
        ggplot(summ, aes(x = reorder(text, mean_rank), y = mean_rank, fill = timing)) +
          geom_col(position = "dodge", alpha = 0.85) + coord_flip() +
          scale_fill_manual(values = c("Pre" = "#5b8abf", "Post" = "#c8a12d")) +
          labs(title = "Automated DRI — Preference Rankings Pre/Post", x = NULL, y = "Average rank (lower = more preferred)", fill = NULL) +
          plot_theme()
      }
    }
  }, bg = "#ffffff")
  
  output$plot_user_dri_opinion_box <- renderPlot({
    survey <- rv$user_dri_survey
    if (is.null(survey)) {
      ggplot() + annotate("text", x = 0.5, y = 0.5, label = "No uploaded DRI survey", color = "#7d8596", size = 5) + theme_void()
    } else {
      pre_df  <- if (!is.null(rv$user_dri_pre))  dri_raw_to_df(rv$user_dri_pre,  survey, "Pre")  else data.frame()
      post_df <- if (!is.null(rv$user_dri_post)) dri_raw_to_df(rv$user_dri_post, survey, "Post") else data.frame()
      combined <- dplyr::bind_rows(pre_df, post_df)
      combined <- combined[combined$section == "consideration" & is.finite(combined$value), , drop = FALSE]
      if (nrow(combined) == 0) {
        ggplot() + annotate("text", x = 0.5, y = 0.5, label = "No DRI consideration data", color = "#7d8596", size = 5) + theme_void()
      } else {
        ggplot(combined, aes(x = timing, y = value, fill = timing)) +
          geom_boxplot(alpha = 0.55, outlier.alpha = 0.4) +
          scale_fill_manual(values = c("Pre" = "#5b8abf", "Post" = "#c8a12d")) +
          labs(title = "Uploaded DRI — Consideration Agreement Levels Pre/Post", x = NULL, y = "Agreement score (1–5)", fill = NULL) +
          plot_theme()
      }
    }
  }, bg = "#ffffff")
  
  output$plot_user_dri_pref_bar <- renderPlot({
    survey <- rv$user_dri_survey
    if (is.null(survey)) {
      ggplot() + annotate("text", x = 0.5, y = 0.5, label = "No uploaded DRI survey", color = "#7d8596", size = 5) + theme_void()
    } else {
      pre_df  <- if (!is.null(rv$user_dri_pre))  dri_raw_to_df(rv$user_dri_pre,  survey, "Pre")  else data.frame()
      post_df <- if (!is.null(rv$user_dri_post)) dri_raw_to_df(rv$user_dri_post, survey, "Post") else data.frame()
      combined <- dplyr::bind_rows(pre_df, post_df)
      combined <- combined[combined$section == "preference" & is.finite(combined$value), , drop = FALSE]
      if (nrow(combined) == 0) {
        ggplot() + annotate("text", x = 0.5, y = 0.5, label = "No DRI ranking data", color = "#7d8596", size = 5) + theme_void()
      } else {
        summ <- combined |> dplyr::group_by(timing, text) |> dplyr::summarise(mean_rank = mean(value, na.rm = TRUE), .groups = "drop")
        ggplot(summ, aes(x = reorder(text, mean_rank), y = mean_rank, fill = timing)) +
          geom_col(position = "dodge", alpha = 0.85) + coord_flip() +
          scale_fill_manual(values = c("Pre" = "#5b8abf", "Post" = "#c8a12d")) +
          labs(title = "Uploaded DRI — Preference Rankings Pre/Post", x = NULL, y = "Average rank (lower = more preferred)", fill = NULL) +
          plot_theme()
      }
    }
  }, bg = "#ffffff")
  
  # ============================================================
  # USER DRI OUTPUTS
  # ============================================================
  
  
  
  output$plot_user_dri_change <- renderPlot({
    pre_df <- if (!is.null(rv$user_dri_pre_scores)) rv$user_dri_pre_scores$individual else NULL
    post_df <- if (!is.null(rv$user_dri_post_scores)) rv$user_dri_post_scores$individual else NULL
    make_dri_change_plot(pre_df, post_df)
  }, bg = "#ffffff")
  
  
  
  output$user_dri_table_dt <- renderDT({
    req(rv$user_dri_comparison)
    datatable(rv$user_dri_comparison, options = list(pageLength = 15, scrollX = TRUE), rownames = FALSE)
  })
  
  output$user_dri_survey_table <- renderDT({
    req(rv$user_dri_survey)
    datatable(dri_questions_to_df(rv$user_dri_survey), options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })
  
  outputOptions(output, "plot_auto_dri_pre", suspendWhenHidden = FALSE)
  outputOptions(output, "plot_auto_dri_post", suspendWhenHidden = FALSE)
  outputOptions(output, "plot_auto_dri_change", suspendWhenHidden = FALSE)
  outputOptions(output, "plot_auto_dri_opinion_box", suspendWhenHidden = FALSE)
  outputOptions(output, "plot_auto_dri_pref_bar", suspendWhenHidden = FALSE)
  outputOptions(output, "plot_user_dri_pre", suspendWhenHidden = FALSE)
  outputOptions(output, "plot_user_dri_post", suspendWhenHidden = FALSE)
  outputOptions(output, "plot_user_dri_change", suspendWhenHidden = FALSE)
  outputOptions(output, "plot_user_dri_opinion_box", suspendWhenHidden = FALSE)
  outputOptions(output, "plot_user_dri_pref_bar", suspendWhenHidden = FALSE)
  
  output$plot_central_tendency <- renderPlot({
    baseline <- if (!is.null(rv$central_tendency)) rv$central_tendency$baseline[1] else NA_real_
    make_central_tendency_plot(rv$personas, baseline)
  }, bg = "#ffffff")
  
  output$plot_mean_shift <- renderPlot({
    req(rv$central_tendency)
    make_mean_shift_plot(rv$central_tendency)
  }, bg = "#ffffff")
  output$plot_opinion <- renderPlot({ req(rv$personas); df <- rv$personas; if (!"final_score" %in% names(df)) df$final_score <- df$initial_score; make_opinion_change(df) }, bg = "#ffffff")
  output$plot_polar <- renderPlot({ req(rv$personas); df <- rv$personas; if (!"final_score" %in% names(df)) df$final_score <- df$initial_score; make_polarization(df) }, bg = "#ffffff")
  output$plot_sentiment <- renderPlot({ make_sentiment(get_turns()) }, bg = "#ffffff")
  
  # Bias monitoring plots (Taubenfeld et al. 2024)
  output$plot_position_drift <- renderPlot({
    make_position_drift(get_turns(), rv$personas, rv$model_baseline)
  }, bg = "#ffffff")
  output$plot_drift_summary <- renderPlot({
    make_drift_summary(get_turns(), rv$personas)
  }, bg = "#ffffff")
  
  # Bias monitoring plots — Report page
  output$plot_position_drift_rpt <- renderPlot({
    make_position_drift(get_turns(), rv$personas, rv$model_baseline)
  }, bg = "#ffffff")
  output$plot_drift_summary_rpt <- renderPlot({
    make_drift_summary(get_turns(), rv$personas)
  }, bg = "#ffffff")
  
  output$drift_table_dt <- renderDT({
    req(rv$drift_table)
    datatable(rv$drift_table, options = list(pageLength = 15, scrollX = TRUE), rownames = FALSE)
  })
  output$plot_quality_step <- renderPlot({ req(rv$quality); make_quality_radar(rv$quality) }, bg = "#ffffff")
  output$plot_quality_report <- renderPlot({ req(rv$quality); make_quality_radar(rv$quality) }, bg = "#ffffff")
  output$plot_quality_dash <- renderPlot({ req(rv$quality); make_quality_radar(rv$quality) }, bg = "#ffffff")
  output$plot_survey <- renderPlot({ req(rv$pre_survey, rv$post_survey); make_survey_comparison(rv$pre_survey, rv$post_survey) }, bg = "#ffffff")
  
  # Composition pies — Dashboard
  output$pie_gender     <- renderPlot({ req(rv$personas); make_pie(rv$personas, "gender", "Gender") }, bg = "#ffffff")
  output$pie_age        <- renderPlot({ req(rv$personas); make_age_pie(rv$personas) }, bg = "#ffffff")
  output$pie_education  <- renderPlot({ req(rv$personas); make_pie(rv$personas, "education", "Education") }, bg = "#ffffff")
  output$pie_settlement <- renderPlot({ req(rv$personas); make_pie(rv$personas, "settlement", "Settlement") }, bg = "#ffffff")
  output$pie_ideology   <- renderPlot({ req(rv$personas); make_ideology_pie(rv$personas) }, bg = "#ffffff")
  
  # Composition pies — Report
  output$pie_gender_rpt     <- renderPlot({ req(rv$personas); make_pie(rv$personas, "gender", "Gender") }, bg = "#ffffff")
  output$pie_age_rpt        <- renderPlot({ req(rv$personas); make_age_pie(rv$personas) }, bg = "#ffffff")
  output$pie_education_rpt  <- renderPlot({ req(rv$personas); make_pie(rv$personas, "education", "Education") }, bg = "#ffffff")
  output$pie_settlement_rpt <- renderPlot({ req(rv$personas); make_pie(rv$personas, "settlement", "Settlement") }, bg = "#ffffff")
  output$pie_ideology_rpt   <- renderPlot({ req(rv$personas); make_ideology_pie(rv$personas) }, bg = "#ffffff")
  output$plot_ideology_alignment <- renderPlot({ req(rv$personas); make_ideology_alignment_plot(input, rv$personas) }, bg = "#ffffff")
  output$plot_ideology_alignment_rpt <- renderPlot({ req(rv$personas); make_ideology_alignment_plot(input, rv$personas) }, bg = "#ffffff")
  output$table_ideology_alignment <- renderDT({ req(rv$personas); datatable(make_ideology_target_table(input, rv$personas), options = list(dom = 't', pageLength = 5), rownames = FALSE) })
  output$plot_aqua_speaker <- renderPlot({ req(rv$aqua); make_aqua_speaker_plot(rv$aqua) }, bg = "#ffffff")
  output$plot_aqua_phase <- renderPlot({ req(rv$aqua); make_aqua_phase_plot(rv$aqua) }, bg = "#ffffff")
  output$plot_aqua_dimensions <- renderPlot({ req(rv$aqua); make_aqua_dimension_plot(rv$aqua) }, bg = "#ffffff")
  output$table_aqua_dimensions <- renderDT({ req(rv$aqua); datatable(rv$aqua$dimensions, options = list(pageLength = 20, scrollX = TRUE), rownames = FALSE) })
  output$table_aqua_comments <- renderDT({ req(rv$aqua); datatable(rv$aqua$comments[, intersect(c("turn","speaker","phase","group","aqua_score","text"), names(rv$aqua$comments)), drop = FALSE], options = list(pageLength = 15, scrollX = TRUE), rownames = FALSE) })
  
  # Process analytics — Dashboard
  output$plot_wordcount   <- renderPlot({ make_wordcount_bar(get_turns()) }, bg = "#ffffff")
  output$plot_turns_phase <- renderPlot({ make_turns_per_phase(get_turns()) }, bg = "#ffffff")
  output$plot_equity      <- renderPlot({ make_participation_equity(get_turns()) }, bg = "#ffffff")
  output$plot_crossref    <- renderPlot({ make_cross_reference_heatmap(get_turns()) }, bg = "#ffffff")
  
  # Process analytics — Report
  output$plot_wordcount_rpt   <- renderPlot({ make_wordcount_bar(get_turns()) }, bg = "#ffffff")
  output$plot_turns_phase_rpt <- renderPlot({ make_turns_per_phase(get_turns()) }, bg = "#ffffff")
  output$plot_equity_rpt      <- renderPlot({ make_participation_equity(get_turns()) }, bg = "#ffffff")
  output$plot_crossref_rpt    <- renderPlot({ make_cross_reference_heatmap(get_turns()) }, bg = "#ffffff")
  
  flatten_survey <- function(survey_list, persona_names = NULL) {
    bind_rows(lapply(seq_along(survey_list), function(idx) {
      r <- survey_list[[idx]]
      # Use respondent from JSON, fall back to persona name, fall back to index
      nm <- scalar_text(r$respondent %||% "")
      if (!nzchar(trimws(nm)) || nm == "name") {
        nm <- if (!is.null(persona_names) && idx <= length(persona_names)) persona_names[idx] else paste0("Agent ", idx)
      }
      resps <- r$responses
      if (is.null(resps) || length(resps) == 0) {
        return(data.frame(Respondent = nm, Question = "[no responses parsed]", Answer = "", Justification = "", stringsAsFactors = FALSE))
      }
      bind_rows(lapply(resps, function(q) data.frame(
        Respondent = nm,
        Question = scalar_text(q$question %||% ""),
        Answer = scalar_text(q$answer %||% ""),
        Justification = scalar_text(q$justification %||% ""),
        stringsAsFactors = FALSE)))
    }))
  }
  flatten_dri_survey <- function(survey_obj) {
    if (is.null(survey_obj)) return(data.frame())
    
    cons_df <- if (!is.null(survey_obj$considerations) && length(survey_obj$considerations) > 0) {
      bind_rows(lapply(survey_obj$considerations, function(x) {
        data.frame(
          section = "Consideration",
          id = scalar_text(x$id %||% ""),
          text = scalar_text(x$text %||% x$consideration %||% ""),
          stringsAsFactors = FALSE
        )
      }))
    } else data.frame()
    
    opt_df <- if (!is.null(survey_obj$policy_options) && length(survey_obj$policy_options) > 0) {
      bind_rows(lapply(survey_obj$policy_options, function(x) {
        data.frame(
          section = "Policy option",
          id = scalar_text(x$id %||% ""),
          text = scalar_text(x$text %||% x$option %||% ""),
          stringsAsFactors = FALSE
        )
      }))
    } else data.frame()
    
    bind_rows(cons_df, opt_df)
  }
  dri_questions_to_df <- function(survey_obj) {
    if (is.null(survey_obj)) return(data.frame())
    
    cons <- if (!is.null(survey_obj$considerations)) {
      bind_rows(lapply(survey_obj$considerations, function(x) {
        data.frame(
          section = "consideration",
          id = scalar_text(x$id %||% ""),
          text = scalar_text(x$text %||% ""),
          theme = scalar_text(x$theme %||% ""),
          direction = scalar_text(x$direction %||% ""),
          stringsAsFactors = FALSE
        )
      }))
    } else data.frame()
    
    prefs <- if (!is.null(survey_obj$policy_options)) {
      bind_rows(lapply(survey_obj$policy_options, function(x) {
        data.frame(
          section = "policy_option",
          id = scalar_text(x$id %||% ""),
          text = scalar_text(x$text %||% ""),
          theme = "",
          direction = "",
          stringsAsFactors = FALSE
        )
      }))
    } else data.frame()
    
    bind_rows(cons, prefs)
  }
  
  dri_raw_to_df <- function(response_list, survey_obj, timing = "pre") {
    if (is.null(response_list) || is.null(survey_obj)) return(data.frame())
    
    cons_text <- setNames(
      vapply(survey_obj$considerations, function(x) scalar_text(x$text %||% ""), character(1)),
      vapply(survey_obj$considerations, function(x) scalar_text(x$id %||% ""), character(1))
    )
    
    pref_text <- setNames(
      vapply(survey_obj$policy_options, function(x) scalar_text(x$text %||% ""), character(1)),
      vapply(survey_obj$policy_options, function(x) scalar_text(x$id %||% ""), character(1))
    )
    
    bind_rows(lapply(response_list, function(resp) {
      nm <- scalar_text(resp$respondent %||% "unknown")
      
      cons_df <- if (!is.null(resp$considerations) && length(resp$considerations) > 0) {
        bind_rows(lapply(resp$considerations, function(x) {
          id <- scalar_text(x$id %||% "")
          data.frame(
            respondent = nm,
            timing = timing,
            section = "consideration",
            id = id,
            text = cons_text[id] %||% "",
            value = as.numeric(x$score %||% NA),
            stringsAsFactors = FALSE
          )
        }))
      } else data.frame()
      
      pref_df <- if (!is.null(resp$preferences) && length(resp$preferences) > 0) {
        bind_rows(lapply(resp$preferences, function(x) {
          id <- scalar_text(x$id %||% "")
          data.frame(
            respondent = nm,
            timing = timing,
            section = "preference",
            id = id,
            text = pref_text[id] %||% "",
            value = as.numeric(x$rank %||% NA),
            stringsAsFactors = FALSE
          )
        }))
      } else data.frame()
      
      bind_rows(cons_df, pref_df)
    }))
  }
  output$pre_survey_table <- renderDT({ req(rv$pre_survey); pn <- if (!is.null(rv$personas)) rv$personas$name else NULL; datatable(flatten_survey(rv$pre_survey, pn), options = list(pageLength = 20, scrollX = TRUE), rownames = FALSE) })
  output$post_survey_table <- renderDT({ req(rv$post_survey); pn <- if (!is.null(rv$personas)) rv$personas$name else NULL; datatable(flatten_survey(rv$post_survey, pn), options = list(pageLength = 20, scrollX = TRUE), rownames = FALSE) })
  output$auto_dri_survey_table <- renderDT({
    req(rv$auto_dri_survey)
    datatable(
      flatten_dri_survey(rv$auto_dri_survey),
      options = list(pageLength = 15, scrollX = TRUE),
      rownames = FALSE
    )
  })
  
  output$user_dri_survey_table <- renderDT({
    req(rv$user_dri_survey)
    datatable(
      flatten_dri_survey(rv$user_dri_survey),
      options = list(pageLength = 15, scrollX = TRUE),
      rownames = FALSE
    )
  })
  # --- Survey Report renderers ---
  output$plot_survey_diversity_sr <- renderPlot({
    req(rv$pre_survey, rv$post_survey)
    make_survey_comparison(rv$pre_survey, rv$post_survey)
  }, bg = "#ffffff")
  
  output$plot_survey_change_sr <- renderPlot({
    req(rv$pre_survey, rv$post_survey, rv$personas)
    pn <- rv$personas$name
    pre_df  <- survey_to_df(rv$pre_survey, "Pre", pn)
    post_df <- survey_to_df(rv$post_survey, "Post", pn)
    make_survey_change_per_respondent(pre_df, post_df)
  }, bg = "#ffffff")
  
  output$plot_survey_consensus_sr <- renderPlot({
    req(rv$pre_survey, rv$post_survey, rv$personas)
    pn <- rv$personas$name
    pre_df  <- survey_to_df(rv$pre_survey, "Pre", pn)
    post_df <- survey_to_df(rv$post_survey, "Post", pn)
    make_survey_consensus_shift(pre_df, post_df)
  }, bg = "#ffffff")
  
  output$pre_survey_table_sr <- renderDT({
    req(rv$pre_survey)
    pn <- if (!is.null(rv$personas)) rv$personas$name else NULL
    datatable(flatten_survey(rv$pre_survey, pn), options = list(pageLength = 15, scrollX = TRUE), rownames = FALSE)
  })
  
  output$post_survey_table_sr <- renderDT({
    req(rv$post_survey)
    pn <- if (!is.null(rv$personas)) rv$personas$name else NULL
    datatable(flatten_survey(rv$post_survey, pn), options = list(pageLength = 15, scrollX = TRUE), rownames = FALSE)
  })
  
  output$survey_comparison_table_sr <- renderDT({
    req(rv$pre_survey, rv$post_survey, rv$personas)
    pn <- rv$personas$name
    pre_df  <- survey_to_df(rv$pre_survey, "Pre", pn) |> select(respondent, question, answer_pre = answer)
    post_df <- survey_to_df(rv$post_survey, "Post", pn) |> select(respondent, question, answer_post = answer)
    merged <- inner_join(pre_df, post_df, by = c("respondent", "question"))
    merged$changed <- ifelse(merged$answer_pre != merged$answer_post, "\u2714", "")
    names(merged) <- c("Respondent", "Question", "Pre-Deliberation", "Post-Deliberation", "Changed")
    datatable(merged, options = list(pageLength = 20, scrollX = TRUE), rownames = FALSE)
  })
  
  # ====== BEGIN ======
  observeEvent(input$begin_click, {
    miss <- required_missing(); highlight_required()
    if (miss$api_key || miss$issue || miss$question) { rv$error <- "Please complete all compulsory fields."; rv$status <- ""; return() }
    rv$error <- ""; rv$status <- "Deliberation started. Go to Step 1."; rv$step <- 1; rv$view <- "step"; add_replog("begin")
  })
  
  observeEvent(input$dri_survey_file, {
    f <- input$dri_survey_file
    if (is.null(f)) {
      rv$user_dri_survey <- NULL
      rv$status <- "Uploaded independent DRI survey cleared."
      return()
    }
    
    tryCatch({
      rv$user_dri_survey <- read_uploaded_dri_survey(f$datapath, f$name)
      rv$status <- paste0(
        "Uploaded independent DRI survey loaded: ",
        f$name,
        " (",
        length(rv$user_dri_survey$considerations),
        " considerations; ",
        length(rv$user_dri_survey$policy_options),
        " options)"
      )
      rv$error <- ""
    }, error = function(e) {
      rv$user_dri_survey <- NULL
      rv$error <- conditionMessage(e)
    })
  })
  # ====== WORKFLOW ENGINE ======
  run_workflow_step <- function(step_id_override = NULL) {
    req(!rv$loading); rv$loading <- TRUE; rv$error <- ""; rv$status <- ""
    issue <- input$policy_issue %||% ""; question <- input$policy_question %||% ""
    n_participants <- as.integer(input$n_participants %||% 10)
    n_groups <- max(2, min(10, as.integer(input$n_groups %||% 2)))
    temp <- min(1.0, max(0, as.numeric(input$temperature %||% 0.3)))
    evidence_block <- build_evidence_block(input$info_base %||% "", rv$evidence_text, rv$evidence_names)
    demographic_instruction <- build_demographic_instruction(input)
    
    tryCatch({
      step_id <- as.integer(step_id_override %||% input$run_current)
      
      if (step_id == 1) {
        withProgress(message = "Probing model baseline bias...", value = 0.1, {
          # Bias probe: ask the model as a neutral "default agent" (per Taubenfeld et al. 2024)
          baseline_probe <- tryCatch({
            probe_txt <- call_openrouter(input$api_key, input$model,
                                         list(list(role = "system", content = "You assess baseline policy stance without persona role-play. Return JSON only."),
                                              list(role = "user", content = baseline_bias_prompt(issue, question, evidence_block))),
                                         max_tokens = 400, temperature = 0)
            coerce_json(probe_txt)
          }, error = function(e) list(baseline = list(position_score = NA, stance = "Probe failed", reasoning = conditionMessage(e))))
          rv$model_baseline <- baseline_probe
          incProgress(0.1, detail = "Generating personas...")
          
          incProgress(0.4, detail = "Parsing...")
          persona_tokens <- min(16000, max(5000, 900 + 425 * n_participants))
          js <- NULL
          use_batches <- n_participants > 12 || temp >= 0.65
          if (!use_batches) {
            for (attempt in 1:4) {
              out <- call_openrouter(input$api_key, input$model,
                                     list(list(role = "system", content = "You generate diverse deliberative personas. Return JSON only."),
                                          list(role = "user", content = persona_generation_prompt(issue, question, n_participants, evidence_block, input$persona_note %||% "", demographic_instruction))),
                                     max_tokens = persona_tokens, temperature = temp, max_retries = 3)
              parsed <- tryCatch(coerce_json(out), error = function(e) NULL)
              if (!is.null(parsed) && !is.null(parsed$personas) && length(parsed$personas) >= n_participants) { js <- parsed; break }
            }
          }
          if (is.null(js) || is.null(js$personas)) {
            batch_sizes <- make_batch_sizes(n_participants, max_batch = if (temp >= 0.8) 8L else 10L)
            persona_list <- list()
            for (b in seq_along(batch_sizes)) {
              out <- call_openrouter(input$api_key, input$model,
                                     list(list(role = "system", content = "You generate diverse deliberative personas. Return JSON only."),
                                          list(role = "user", content = persona_generation_prompt(issue, question, batch_sizes[b], evidence_block, input$persona_note %||% "", demographic_instruction))),
                                     max_tokens = min(9000, max(3500, 1100 + 500 * batch_sizes[b])), temperature = min(temp, 0.75), max_retries = 3)
              parsed <- tryCatch(coerce_json(out), error = function(e) NULL)
              if (is.null(parsed) || is.null(parsed$personas) || !length(parsed$personas)) stop("Persona generation failed during batched fallback.")
              persona_list <- c(persona_list, parsed$personas)
            }
            js <- list(personas = persona_list)
          }
          if (is.null(js) || is.null(js$personas)) stop("Persona generation failed: the model kept returning truncated JSON. Try a stronger model or slightly fewer participants.")
          df <- bind_rows(lapply(js$personas, as.data.frame))
          if (nrow(df) > n_participants) df <- df[seq_len(n_participants), , drop = FALSE]
          df <- normalize_persona_df(df)
          df$id <- paste0("P", seq_len(nrow(df)))
          df <- rebalance_persona_ideology(df, input)
          rv$personas <- df; incProgress(0.3)
        })
        rv$step <- 2; rv$status <- paste0("Generated ", nrow(rv$personas), " personas. Proceed to validate."); add_replog("generate_personas")
      }
      
      
      if (step_id == 2) {
        withProgress(message = "Validating personas...", value = 0.1, {
          pj <- safe_json(list(personas = split(rv$personas, seq_len(nrow(rv$personas)))))
          
          # First: integrity check to find missing perspectives
          out_int <- call_openrouter(
            input$api_key, input$model,
            list(
              list(role = "system", content = "You assess deliberative design integrity. Return JSON only."),
              list(role = "user", content = integrity_prompt(issue, evidence_block, pj, n_groups))
            ),
            max_tokens = 1800,
            temperature = 0.2
          )
          
          integrity_result <- coerce_json(out_int)
          incProgress(0.2, detail = "Regenerating with missing perspectives...")
          
          # ------------------------------------------------------------
          # SAFER PERSONA REGENERATION
          # ------------------------------------------------------------
          persona_tokens <- min(12000, max(4000, 700 + 325 * n_participants))
          js <- NULL
          
          # First try: whole set, smaller and safer
          for (attempt in 1:3) {
            out_regen <- tryCatch(
              call_openrouter(
                input$api_key, input$model,
                list(
                  list(role = "system", content = "You regenerate deliberative personas to fill representation gaps. Return JSON only."),
                  list(role = "user", content = persona_validation_prompt(issue, question, evidence_block, pj, safe_json(integrity_result)))
                ),
                max_tokens = persona_tokens,
                temperature = min(temp, 0.6),
                max_retries = 2
              ),
              error = function(e) NULL
            )
            
            if (!is.null(out_regen)) {
              parsed <- tryCatch(coerce_json(out_regen), error = function(e) NULL)
              if (!is.null(parsed) && !is.null(parsed$personas) && length(parsed$personas) >= nrow(rv$personas)) {
                js <- parsed
                break
              }
            }
          }
          
          # Fallback: validate in small batches
          if (is.null(js) || is.null(js$personas)) {
            batch_sizes <- make_batch_sizes(nrow(rv$personas), max_batch = 4L)
            persona_list <- list()
            
            for (b in seq_along(batch_sizes)) {
              idx_start <- if (b == 1) 1 else sum(batch_sizes[seq_len(b - 1)]) + 1
              idx_end   <- sum(batch_sizes[seq_len(b)])
              chunk_df  <- rv$personas[idx_start:idx_end, , drop = FALSE]
              chunk_pj  <- safe_json(list(personas = split(chunk_df, seq_len(nrow(chunk_df)))))
              
              out_chunk <- tryCatch(
                call_openrouter(
                  input$api_key, input$model,
                  list(
                    list(role = "system", content = "You regenerate deliberative personas to fill representation gaps. Return JSON only."),
                    list(role = "user", content = persona_validation_prompt(issue, question, evidence_block, chunk_pj, safe_json(integrity_result)))
                  ),
                  max_tokens = min(5000, max(2200, 1000 + 350 * nrow(chunk_df))),
                  temperature = min(temp, 0.6),
                  max_retries = 2
                ),
                error = function(e) NULL
              )
              
              if (is.null(out_chunk)) {
                stop(paste("Persona validation failed in batch", b, "- empty model output."))
              }
              
              parsed_chunk <- tryCatch(coerce_json(out_chunk), error = function(e) NULL)
              if (is.null(parsed_chunk) || is.null(parsed_chunk$personas) || !length(parsed_chunk$personas)) {
                stop(paste("Persona validation failed in batch", b, "- JSON parse failed."))
              }
              
              persona_list <- c(persona_list, parsed_chunk$personas)
              incProgress(0.2 / length(batch_sizes), detail = paste("Validated batch", b, "of", length(batch_sizes)))
            }
            
            js <- list(personas = persona_list)
          }
          
          if (is.null(js) || is.null(js$personas)) {
            stop("Persona validation failed: OpenRouter returned empty or unparseable output.")
          }
          
          df <- bind_rows(lapply(js$personas, as.data.frame))
          if (nrow(df) > nrow(rv$personas)) df <- df[seq_len(nrow(rv$personas)), , drop = FALSE]
          df <- normalize_persona_df(df)
          df$id <- paste0("P", seq_len(nrow(df)))
          df <- rebalance_persona_ideology(df, input)
          
          if (all(is.na(df$political_ideology)) || all(is.na(df$initial_score))) {
            rv$status <- "Persona regeneration returned incomplete ideology/position fields; keeping original personas."
          } else {
            rv$personas <- df
          }
          
          rv$persona_validated <- TRUE
          incProgress(0.1, detail = "Personas validated.")
        })
        
        # ------------------------------------------------------------
        # AUTO-GENERATED DRI SURVEY + PRE-DLIB SURVEY ADMINISTRATION
        # ------------------------------------------------------------
        withProgress(message = "Preparing DRI surveys...", value = 0, {
          
          auto_dri_txt <- call_openrouter(
            input$api_key, input$model,
            list(
              list(role = "system", content = "You design balanced deliberative reasoning surveys. Return JSON only."),
              list(role = "user", content = dri_survey_prompt(issue, question, evidence_block))
            ),
            max_tokens = 3500,
            temperature = 0.2
          )
          
          rv$auto_dri_survey <- coerce_json(auto_dri_txt)
          incProgress(0.15, detail = "Auto DRI survey generated...")
          
          cards <- lapply(seq_len(nrow(rv$personas)), function(i) as.list(rv$personas[i, ]))
          
          # -------------------------
          # AUTO DRI PRE
          # -------------------------
          auto_pre_results <- list()
          for (i in seq_along(cards)) {
            incProgress(0.40 / length(cards), detail = paste0("Auto DRI pre: Agent ", i, "/", length(cards)))
            auto_pre_results[[i]] <- tryCatch({
              complete_dri_response(input$api_key, input$model, cards[[i]], rv$auto_dri_survey, "pre", temperature = temp)
            }, error = function(e) {
              stop(conditionMessage(e))
            })
          }
          
          rv$auto_dri_pre <- auto_pre_results
          rv$auto_dri_pre_scores <- compute_dri_from_responses(rv$auto_dri_pre, rv$auto_dri_survey, timing = "pre")
          incProgress(0.10, detail = "Auto DRI pre completed...")
          
          # -------------------------
          # USER DRI PRE (OPTIONAL)
          # -------------------------
          if (!is.null(rv$user_dri_survey)) {
            user_pre_results <- list()
            for (i in seq_along(cards)) {
              incProgress(0.35 / length(cards), detail = paste0("User DRI pre: Agent ", i, "/", length(cards)))
              user_pre_results[[i]] <- tryCatch({
                complete_dri_response(input$api_key, input$model, cards[[i]], rv$user_dri_survey, "pre", temperature = temp)
              }, error = function(e) {
                list(
                  respondent = scalar_text(cards[[i]]$name),
                  timing = "pre",
                  considerations = list(),
                  preferences = list()
                )
              })
            }
            
            rv$user_dri_pre <- user_pre_results
            rv$user_dri_pre_scores <- compute_dri_from_responses(rv$user_dri_pre, rv$user_dri_survey, timing = "pre")
            incProgress(0.05, detail = "User DRI pre completed...")
          }
        })
        
        add_replog("auto_dri_generate")
        add_replog("auto_dri_pre")
        if (!is.null(rv$user_dri_survey)) add_replog("user_dri_pre")
        
        # ------------------------------------------------------------
        # OPTIONAL PRE-DELIBERATION SURVEY
        # ------------------------------------------------------------
        if (rv$survey_enabled && nzchar(rv$survey_text)) {
          withProgress(message = "Pre-deliberation survey...", value = 0, {
            cards <- lapply(seq_len(nrow(rv$personas)), function(i) as.list(rv$personas[i, ]))
            pre_results <- list()
            
            for (i in seq_along(cards)) {
              incProgress(1 / length(cards), detail = paste0("Agent ", i, "/", length(cards)))
              pre_results[[i]] <- tryCatch({
                txt <- call_openrouter(
                  input$api_key, input$model,
                  list(
                    list(role = "system", content = "You complete surveys in character. Return ONLY valid JSON. Keep each answer to 1-2 sentences and each justification to 1 sentence. You MUST close all brackets and braces."),
                    list(role = "user", content = survey_prompt(cards[[i]], rv$survey_text, "pre"))
                  ),
                  max_tokens = 4000,
                  temperature = temp
                )
                coerce_json(txt)
              }, error = function(e) {
                list(
                  respondent = scalar_text(cards[[i]]$name),
                  timing = "pre",
                  responses = list(
                    list(
                      question = "[PARSE ERROR]",
                      answer = conditionMessage(e),
                      justification = ""
                    )
                  )
                )
              })
            }
            
            rv$pre_survey <- pre_results
          })
          
          add_replog("pre_survey")
        }
        
        rv$step <- 3
        rv$status <- "Personas validated and regenerated."
        add_replog("validate_personas")
      }
      
      if (step_id == 3) {
        withProgress(message = "Design integrity...", value = 0.3, {
          pj <- safe_json(list(personas = split(rv$personas, seq_len(nrow(rv$personas)))))
          out <- call_openrouter(input$api_key, input$model,
                                 list(list(role = "system", content = "You assess deliberative design integrity. Return JSON only."),
                                      list(role = "user", content = integrity_prompt(issue, evidence_block, pj, n_groups))), max_tokens = 1800, temperature = 0.2)
          rv$integrity <- coerce_json(out); incProgress(0.7)
        }); rv$step <- 4; rv$status <- "Design integrity completed."; add_replog("design_integrity")
      }
      
      if (step_id == 4) {
        req(rv$personas)
        rv$agent_cards <- lapply(seq_len(nrow(rv$personas)), function(i) as.list(rv$personas[i, ]))
        rv$step <- 5; rv$status <- paste0("Assigned ", length(rv$agent_cards), " agents."); add_replog("assign_agents")
      }
      
      if (step_id == 5) {
        req(rv$personas); rv$groups <- split_groups_evenly(nrow(rv$personas), n_groups)
        rv$step <- 6; rv$status <- paste0("Split into ", n_groups, " groups."); add_replog("split_groups")
      }
      
      # Helper for group rounds
      run_group_round <- function(phase_label, task_text, extra_evidence = "", max_tok = 500) {
        total <- sum(lengths(rv$groups)) + length(rv$groups)
        all_turns <- get_turns(); group_out <- list()
        turn_counter <- if (nrow(all_turns) > 0) max(all_turns$turn) + 1 else 1; call_n <- 0
        ev <- if (nzchar(extra_evidence)) paste(evidence_block, extra_evidence) else evidence_block
        current_scores <- setNames(as.numeric(vapply(rv$agent_cards, function(x) x$initial_score %||% NA, numeric(1))), vapply(rv$agent_cards, function(x) scalar_text(x$name), character(1)))
        current_positions <- setNames(vapply(rv$agent_cards, function(x) scalar_text(x$initial_position_text %||% ""), character(1)), vapply(rv$agent_cards, function(x) scalar_text(x$name), character(1)))
        if (nrow(all_turns) > 0 && "position_score" %in% names(all_turns)) {
          for (nm in names(current_scores)) {
            prev <- all_turns[all_turns$speaker == nm & is.finite(all_turns$position_score), , drop = FALSE]
            if (nrow(prev) > 0) current_scores[[nm]] <- tail(prev$position_score, 1)
            prev_txt <- all_turns[all_turns$speaker == nm & nzchar(trimws(all_turns$text)), , drop = FALSE]
            if (nrow(prev_txt) > 0) current_positions[[nm]] <- tail(prev_txt$text, 1)
          }
        }
        for (g in names(rv$groups)) {
          ids <- rv$groups[[g]]; gc <- ""; interventions <- c()
          for (idx in ids) {
            call_n <- call_n + 1; incProgress(1/total, detail = paste0("Agent ", call_n, "/", total))
            persona <- rv$agent_cards[[idx]]
            speaker_name <- scalar_text(persona$name)
            score_before <- current_scores[[speaker_name]] %||% as.numeric(persona$initial_score %||% NA)
            txt <- call_openrouter(input$api_key, input$model,
                                   list(list(role = "system", content = agent_system_prompt(persona)),
                                        list(role = "user", content = agent_turn_prompt(persona, issue, question, ev, gc, task_text,
                                                                                        current_score = score_before,
                                                                                        current_position_text = current_positions[[speaker_name]] %||% persona$initial_position_text))),
                                   max_tokens = max_tok, temperature = temp)
            turn <- coerce_json(txt)
            turn_text <- scalar_text(turn$text %||% "")
            score_after <- suppressWarnings(as.numeric(turn$position_score %||% score_before))
            if (!is.finite(score_after)) score_after <- score_before
            if (is.finite(score_before) && is.finite(score_after) && abs(score_after - score_before) > 1) {
              score_after <- score_before + sign(score_after - score_before)
            }
            current_scores[[speaker_name]] <- score_after
            if (nzchar(trimws(turn_text))) current_positions[[speaker_name]] <- turn_text
            interventions <- c(interventions, paste0(speaker_name, ": ", turn_text, " [score ", score_after, "/5]"))
            all_turns <- bind_rows(all_turns, data.frame(
              turn = turn_counter, speaker = speaker_name,
              sentiment_score = as.numeric(turn$sentiment_score %||% 0),
              position_score = score_after,
              score_before_turn = suppressWarnings(as.numeric(score_before)),
              shift_reason = scalar_text(turn$shift_reason %||% ""),
              phase = phase_label, group = g,
              text = turn_text,
              word_count = length(strsplit(trimws(turn_text), "\\s+")[[1]]),
              stringsAsFactors = FALSE))
            turn_counter <- turn_counter + 1; gc <- paste(interventions, collapse = "\n")
          }
          call_n <- call_n + 1; incProgress(1/total, detail = paste0("Synthesis ", g))
          synth_inst <- switch(phase_label,
                               "Values" = "Provide a short transcript summary followed by a list of 8-12 values.",
                               "Options" = "Provide a short transcript summary followed by 8-10 policy options.",
                               "Evaluation" = "For each policy option, summarize pros, cons, and tradeoffs. Connect each pro, con, or tradeoff to a key value mentioned previously in the discussion.")
          sumtxt <- call_openrouter(input$api_key, input$model,
                                    list(list(role = "system", content = paste0("You synthesize ", tolower(phase_label), " outputs.")),
                                         list(role = "user", content = synthesis_prompt(paste0("Summarize ", tolower(phase_label), " for ", g),
                                                                                        issue, question, evidence_block, paste(interventions, collapse = "\n"), synth_inst))),
                                    max_tokens = 1200, temperature = 0.3)
          group_out[[g]] <- list(transcript = paste(interventions, collapse = "\n"), summary = sumtxt)
        }
        list(turns = all_turns, groups = group_out)
      }
      
      if (step_id == 6) {
        req(rv$groups, rv$agent_cards)
        withProgress(message = "Values round...", value = 0, {
          res <- run_group_round("Values", "Discuss the key values at stake in this policy issue.", max_tok = 500)
          rv$group_values <- res$groups; rv$turns <- res$turns
          rv$personas <- update_final_scores_from_turns(rv$personas, rv$turns)
          rv$drift_table <- compute_drift_table(rv$personas, rv$turns, rv$model_baseline)
        }); rv$step <- 7; rv$status <- "Values round completed."; add_replog("values_round")
      }
      
      if (step_id == 7) {
        withProgress(message = "Merging values...", value = 0.5, {
          inputs <- paste(vapply(names(rv$group_values), function(g) paste0(g, "\n", rv$group_values[[g]]$summary), character(1)), collapse = "\n\n")
          rv$master_values <- call_openrouter(input$api_key, input$model,
                                              list(list(role = "system", content = "You merge group value lists."),
                                                   list(role = "user", content = synthesis_prompt("Merge all group values", issue, question, evidence_block, inputs, "Create one clean master values list."))),
                                              max_tokens = 1000, temperature = 0.2)
        }); rv$step <- 8; rv$status <- "Master values created."; add_replog("master_values")
      }
      
      # Reshuffle groups before options round (per Rountree & Gastil)
      if (step_id == 8) {
        req(rv$personas)
        rv$groups <- split_groups_evenly(nrow(rv$personas), n_groups)
        # Shuffle to get different composition
        set.seed(as.numeric(Sys.time()))
        shuffled_ids <- sample(seq_len(nrow(rv$personas)))
        rv$groups <- split(shuffled_ids, rep(seq_len(n_groups), length.out = nrow(rv$personas)))
        names(rv$groups) <- paste0("Group ", seq_len(n_groups))
        rv$step <- 9; rv$status <- "Groups reshuffled for options round."; add_replog("reshuffle_options")
      }
      
      if (step_id == 9) {
        withProgress(message = "Options round...", value = 0, {
          res <- run_group_round("Options", "Brainstorm policy options to answer the policy question.",
                                 extra_evidence = paste0("\n\nMASTER VALUES:\n", rv$master_values), max_tok = 550)
          rv$group_options <- res$groups; rv$turns <- res$turns
          rv$personas <- update_final_scores_from_turns(rv$personas, rv$turns)
          rv$drift_table <- compute_drift_table(rv$personas, rv$turns, rv$model_baseline)
        }); rv$step <- 10; rv$status <- "Options round completed."; add_replog("options_round")
      }
      
      if (step_id == 10) {
        withProgress(message = "Merging options...", value = 0.5, {
          inputs <- paste(vapply(names(rv$group_options), function(g) paste0(g, "\n", rv$group_options[[g]]$summary), character(1)), collapse = "\n\n")
          rv$master_options <- call_openrouter(input$api_key, input$model,
                                               list(list(role = "system", content = "You merge policy option lists."),
                                                    list(role = "user", content = synthesis_prompt("Merge all policy options", issue, question, evidence_block, inputs, "Create a master list of distinct policy options."))),
                                               max_tokens = 1300, temperature = 0.2)
        }); rv$step <- 11; rv$status <- "Master options created."; add_replog("master_options")
      }
      
      # Reshuffle groups before evaluation round (per Rountree & Gastil)
      if (step_id == 11) {
        req(rv$personas)
        set.seed(as.numeric(Sys.time()) + 1)
        shuffled_ids <- sample(seq_len(nrow(rv$personas)))
        rv$groups <- split(shuffled_ids, rep(seq_len(n_groups), length.out = nrow(rv$personas)))
        names(rv$groups) <- paste0("Group ", seq_len(n_groups))
        rv$step <- 12; rv$status <- "Groups reshuffled for evaluation round."; add_replog("reshuffle_evaluation")
      }
      
      if (step_id == 12) {
        withProgress(message = "Evaluation round...", value = 0, {
          res <- run_group_round("Evaluation", "Evaluate each of the policy options. For each option, discuss pros, cons, and tradeoffs. Connect each pro, con, or tradeoff to a key value from the master values list.",
                                 extra_evidence = paste0("\n\nMASTER VALUES:\n", rv$master_values, "\n\nMASTER OPTIONS:\n", rv$master_options), max_tok = 850)
          rv$evaluations <- res$groups; rv$turns <- res$turns
          rv$personas <- update_final_scores_from_turns(rv$personas, rv$turns)
          rv$drift_table <- compute_drift_table(rv$personas, rv$turns, rv$model_baseline)
        }); rv$step <- 13; rv$status <- "Evaluation round completed."; add_replog("evaluation_round")
      }
      
      # Individual Rankings (per Rountree & Gastil)
      if (step_id == 13) {
        req(rv$agent_cards, rv$master_values, rv$master_options, rv$evaluations)
        withProgress(message = "Individual rankings...", value = 0, {
          eval_text <- paste(vapply(names(rv$evaluations), function(g) paste0(g, "\n", rv$evaluations[[g]]$summary), character(1)), collapse = "\n\n")
          ranking_results <- list()
          for (i in seq_along(rv$agent_cards)) {
            incProgress(1/length(rv$agent_cards), detail = paste0("Agent ", i, "/", length(rv$agent_cards)))
            ranking_results[[i]] <- tryCatch({
              txt <- call_openrouter(input$api_key, input$model,
                                     list(list(role = "system", content = "You rank policy options in character. Return ONLY valid JSON."),
                                          list(role = "user", content = ranking_prompt(rv$agent_cards[[i]], issue, question, rv$master_values, rv$master_options, eval_text))),
                                     max_tokens = 2000, temperature = temp)
              coerce_json(txt)
            }, error = function(e) {
              list(respondent = scalar_text(rv$agent_cards[[i]]$name), final_position_score = NA,
                   rankings = list(list(option = "[PARSE ERROR]", rank = NA, justification = conditionMessage(e))))
            })
          }
          rv$rankings <- ranking_results
          rv$personas <- update_final_scores_from_turns(rv$personas, rv$turns)
          # Update final scores from rankings
          for (i in seq_along(ranking_results)) {
            fs <- ranking_results[[i]]$final_position_score
            if (!is.null(fs) && !is.na(as.numeric(fs))) {
              rv$personas$final_score[i] <- as.numeric(fs)
            }
          }
        });
        if (isTRUE(rv$batch_running)) {
          rv$view <- "step"; rv$step <- 0L; rv$status <- "Batch individual rankings completed; staying on setup/export screen."
        } else {
          rv$step <- 14; rv$status <- "Individual rankings completed."
        }
        add_replog("individual_rankings")
      }
      
      if (step_id == 14) {
        withProgress(message = "Final recommendation...", value = 0.1, {
          output_type <- input$output_type %||% "policy_recommendation"
          existing_policy <- input$existing_policy %||% ""
          
          inputs <- paste("PERSONAS:\n", safe_json(split(rv$personas, seq_len(nrow(rv$personas)))), "\n\n",
                          "MASTER VALUES:\n", rv$master_values, "\n\n", "MASTER OPTIONS:\n", rv$master_options, "\n\n",
                          "EVALUATIONS:\n", paste(vapply(names(rv$evaluations), function(g) paste0(g, "\n", rv$evaluations[[g]]$summary), character(1)), collapse = "\n\n"),
                          if (!is.null(rv$rankings)) paste0("\n\nINDIVIDUAL RANKINGS:\n", safe_json(rv$rankings)) else "")
          
          # Shape the main report instruction based on output type
          report_instruction <- switch(output_type,
                                       "policy_recommendation" = paste0(
                                         "Write a POLICY RECOMMENDATION report that includes:\n",
                                         "(A) Problem statement and context\n",
                                         "(B) The assembly's recommended policy (the group's preferred option)\n",
                                         "(C) 6 key reasons supporting the recommendation\n",
                                         "(D) 6 key reasons against or risks identified\n",
                                         "(E) Implementation considerations and conditions\n",
                                         "(F) Initial and final positions of each participant with scores 1-5"),
                                       "policy_support_opposition" = paste0(
                                         "Write a POLICY ASSESSMENT report evaluating the following existing policy:\n\n",
                                         "EXISTING POLICY: ", scalar_text(existing_policy), "\n\n",
                                         "The report must include:\n",
                                         "(A) Summary of the existing policy under review\n",
                                         "(B) The assembly's overall verdict: SUPPORT, OPPOSE, or CONDITIONAL SUPPORT\n",
                                         "(C) Key arguments FOR the existing policy raised by participants\n",
                                         "(D) Key arguments AGAINST the existing policy raised by participants\n",
                                         "(E) Proposed amendments or conditions for support\n",
                                         "(F) Dissenting positions and minority views\n",
                                         "(G) Initial and final positions of each participant with scores 1-5"),
                                       "idea_generation" = paste0(
                                         "Write an IDEA GENERATION report that includes:\n",
                                         "(A) The challenge or opportunity being addressed\n",
                                         "(B) Comprehensive catalogue of all ideas generated (grouped thematically)\n",
                                         "(C) Most innovative or unconventional ideas highlighted\n",
                                         "(D) Ideas with broadest consensus across participants\n",
                                         "(E) Ideas with strongest opposition and why\n",
                                         "(F) Feasibility and impact assessment for the top 5 ideas\n",
                                         "(G) Suggested next steps and ideas for further development\n",
                                         "(H) Initial and final positions of each participant with scores 1-5"))
          
          rv$final_report <- call_openrouter(input$api_key, input$model,
                                             list(list(role = "system", content = "You write a final deliberation report."),
                                                  list(role = "user", content = synthesis_prompt("Write the final deliberation report", issue, question, evidence_block, inputs, report_instruction))),
                                             max_tokens = 3500, temperature = 0.25)
          incProgress(0.3, detail = "Generating appendix...")
          
          # Generate the detailed appendix based on output type
          appendix_instruction <- switch(output_type,
                                         "policy_recommendation" = paste0(
                                           "Write a DETAILED APPENDIX for a policy recommendation deliberation. Include ALL of the following sections:\n\n",
                                           "1. STAKEHOLDER IMPACT ANALYSIS: For each major stakeholder group, describe how the recommended policy would affect them.\n",
                                           "2. IMPLEMENTATION ROADMAP: Outline a phased implementation plan with timeline, milestones, and responsible actors.\n",
                                           "3. COST-BENEFIT ANALYSIS: Summarise the expected costs vs benefits as discussed by participants.\n",
                                           "4. RISK REGISTER: List the top 10 risks identified during deliberation with likelihood and mitigation strategies.\n",
                                           "5. COMPARATIVE ANALYSIS: Compare the recommended policy against the 2-3 runner-up options with a structured comparison.\n",
                                           "6. VALUE TENSIONS: Describe the key value conflicts that emerged and how they were navigated.\n",
                                           "7. MINORITY REPORT: Document dissenting views that could not be reconciled, with their reasoning.\n",
                                           "8. EVIDENCE GAPS: List areas where participants noted insufficient evidence or uncertainty."),
                                         "policy_support_opposition" = paste0(
                                           "Write a DETAILED APPENDIX for a policy support/opposition assessment. Include ALL of the following sections:\n\n",
                                           "EXISTING POLICY: ", scalar_text(existing_policy), "\n\n",
                                           "1. CLAUSE-BY-CLAUSE ANALYSIS: Break down the key provisions of the existing policy and the assembly's view on each.\n",
                                           "2. AFFECTED POPULATIONS: Detail how different demographic groups are affected by the policy.\n",
                                           "3. AMENDMENT PROPOSALS: List every proposed amendment or modification with rationale and level of support.\n",
                                           "4. UNINTENDED CONSEQUENCES: Document unintended effects identified during deliberation.\n",
                                           "5. ENFORCEMENT AND COMPLIANCE: Assess the enforceability of the policy as discussed.\n",
                                           "6. COMPARATIVE EVIDENCE: Summarise any comparisons to similar policies in other jurisdictions.\n",
                                           "7. VALUE ALIGNMENT: Analyse how the policy aligns or conflicts with the values identified in the values round.\n",
                                           "8. LEGITIMACY ASSESSMENT: Evaluate the democratic legitimacy and public acceptability of the policy."),
                                         "idea_generation" = paste0(
                                           "Write a DETAILED APPENDIX for an idea generation deliberation. Include ALL of the following sections:\n\n",
                                           "1. COMPLETE IDEA CATALOGUE: List EVERY idea generated, organised by theme, with the originating participant(s).\n",
                                           "2. IDEA CLUSTERING MAP: Group ideas into clusters showing how they relate to each other.\n",
                                           "3. FEASIBILITY-IMPACT MATRIX: Rate each major idea on feasibility (low/medium/high) and potential impact (low/medium/high).\n",
                                           "4. QUICK WINS: Identify ideas that are both highly feasible and could be implemented immediately.\n",
                                           "5. MOONSHOTS: Highlight ambitious, transformative ideas that require significant resources or time.\n",
                                           "6. SYNERGIES: Identify combinations of ideas that could reinforce each other.\n",
                                           "7. RESOURCE REQUIREMENTS: For the top 10 ideas, estimate resources needed (financial, human, institutional).\n",
                                           "8. PRIORITISATION RATIONALE: Explain the reasoning behind participant preferences and rankings."))
          
          rv$appendix_report <- call_openrouter(input$api_key, input$model,
                                                list(list(role = "system", content = "You write detailed analytical appendices for deliberation reports."),
                                                     list(role = "user", content = synthesis_prompt("Write the appendix for this deliberation", issue, question, evidence_block, inputs, appendix_instruction))),
                                                max_tokens = 4000, temperature = 0.3)
          add_replog("appendix")
          incProgress(0.2, detail = "Building transcript...")
          
          # Final scores: use rankings if available, otherwise simple heuristic
          if (!is.null(rv$rankings) && !"final_score" %in% names(rv$personas)) {
            for (i in seq_along(rv$rankings)) {
              fs <- rv$rankings[[i]]$final_position_score
              if (!is.null(fs) && !is.na(as.numeric(fs))) rv$personas$final_score[i] <- as.numeric(fs)
            }
          }
          rv$personas <- update_final_scores_from_turns(rv$personas, rv$turns)
          if (!"final_score" %in% names(rv$personas) || all(!is.finite(rv$personas$final_score))) {
            rv$personas$final_score <- rv$personas$initial_score
          }
          # ------------------------------------------------------------
          # POST-DELIBERATION AUTO DRI
          # ------------------------------------------------------------
          if (!is.null(rv$auto_dri_survey)) {
            incProgress(0.03, detail = "Post-deliberation Auto DRI...")
            cards <- lapply(seq_len(nrow(rv$personas)), function(i) as.list(rv$personas[i, ]))
            auto_post_results <- list()
            
            for (i in seq_along(cards)) {
              auto_post_results[[i]] <- tryCatch({
                complete_dri_response(input$api_key, input$model, cards[[i]], rv$auto_dri_survey, "post", temperature = temp)
              }, error = function(e) {
                list(
                  respondent = scalar_text(cards[[i]]$name),
                  timing = "post",
                  considerations = list(),
                  preferences = list()
                )
              })
            }
            
            rv$auto_dri_post <- auto_post_results
            rv$auto_dri_post_scores <- compute_dri_from_responses(rv$auto_dri_post, rv$auto_dri_survey, timing = "post")
            rv$auto_dri_wilcox <- compute_dri_prepost_wilcox(rv$auto_dri_pre_scores, rv$auto_dri_post_scores)
            if (!is.null(rv$auto_dri_pre_scores) && !is.null(rv$auto_dri_post_scores)) {
              rv$auto_dri_comparison <- full_join(
                rv$auto_dri_pre_scores$individual,
                rv$auto_dri_post_scores$individual,
                by = "respondent",
                suffix = c("_pre", "_post")
              )
            }
          }
          
          # ------------------------------------------------------------
          # POST-DELIBERATION USER DRI (OPTIONAL)
          # ------------------------------------------------------------
          if (!is.null(rv$user_dri_survey)) {
            incProgress(0.03, detail = "Post-deliberation User DRI...")
            cards <- lapply(seq_len(nrow(rv$personas)), function(i) as.list(rv$personas[i, ]))
            user_post_results <- list()
            
            for (i in seq_along(cards)) {
              user_post_results[[i]] <- tryCatch({
                complete_dri_response(input$api_key, input$model, cards[[i]], rv$user_dri_survey, "post", temperature = temp)
              }, error = function(e) {
                list(
                  respondent = scalar_text(cards[[i]]$name),
                  timing = "post",
                  considerations = list(),
                  preferences = list()
                )
              })
            }
            
            rv$user_dri_post <- user_post_results
            rv$user_dri_post_scores <- compute_dri_from_responses(rv$user_dri_post, rv$user_dri_survey, timing = "post")
            rv$user_dri_wilcox <- compute_dri_prepost_wilcox(rv$user_dri_pre_scores, rv$user_dri_post_scores)
            if (!is.null(rv$user_dri_pre_scores) && !is.null(rv$user_dri_post_scores)) {
              rv$user_dri_comparison <- full_join(
                rv$user_dri_pre_scores$individual,
                rv$user_dri_post_scores$individual,
                by = "respondent",
                suffix = c("_pre", "_post")
              )
            }
          }
          
          # Central tendency remains separate
          rv$central_tendency <- compute_central_tendency_stats(rv$personas, rv$model_baseline)
          add_replog("auto_dri_post")
          if (!is.null(rv$user_dri_survey)) add_replog("user_dri_post")
          add_replog("central_tendency")
        }); rv$step <- 15; rv$status <- "Final recommendation and appendix created."; add_replog("final_recommendation")
      }
      
      if (step_id == 15) {
        withProgress(message = "Quality assessment + AQuA scoring...", value = 0.1, {
          pj <- safe_json(split(rv$personas, seq_len(nrow(rv$personas))))
          out <- call_openrouter(input$api_key, input$model,
                                 list(list(role = "system", content = "You assess deliberative quality. Return JSON only."),
                                      list(role = "user", content = quality_prompt(issue, evidence_block, rv$transcript, rv$final_report, pj))),
                                 max_tokens = 2200, temperature = 0.2)
          rv$quality <- coerce_json(out)
          incProgress(0.35, detail = "Quality assessment done. Running AQuA LLM scoring...")
          
          # AQuA — LLM-based scoring (Behrendt et al. 2024)
          # Each comment is scored by the LLM on 20 binary dimensions,
          # then combined with the exact weights from Table 1 and normalised to 0-5.
          turns_for_aqua <- get_turns()
          if (!is.null(turns_for_aqua) && nrow(turns_for_aqua) > 0) {
            n_comments <- nrow(turns_for_aqua)
            incProgress(0, detail = paste0("AQuA: scoring ", n_comments, " comments via LLM..."))
            rv$aqua <- tryCatch(
              compute_aqua_llm(turns_for_aqua, input$api_key, input$model, temperature = 0.0),
              error = function(e) {
                rv$status <- paste0("AQuA LLM scoring failed: ", conditionMessage(e), ". Quality assessment still saved.")
                NULL
              }
            )
          }
          incProgress(0.55, detail = "AQuA scoring complete.")
        }); rv$status <- "Quality assessment and AQuA scoring completed. Outputs are ready."; add_replog("quality_assessment"); add_replog("aqua_llm_scoring")
      }
      
      rv$error <- ""
    }, error = function(e) { rv$error <- conditionMessage(e) })
    rv$loading <- FALSE
  }
  
  observeEvent(input$run_current, {
    run_workflow_step(as.integer(input$run_current))
  })
  
  
  # ====== BATCH RUNS AND CSV EXPORTS ======
  reset_run_state_for_batch <- function() {
    keep_user_dri <- rv$user_dri_survey
    keep_evidence_text <- rv$evidence_text
    keep_evidence_names <- rv$evidence_names
    rv$view <- "step"; rv$step <- 1; rv$error <- ""; rv$status <- ""
    rv$personas <- NULL; rv$integrity <- NULL; rv$agent_cards <- NULL; rv$groups <- NULL
    rv$group_values <- NULL; rv$master_values <- NULL; rv$group_options <- NULL; rv$master_options <- NULL
    rv$evaluations <- NULL; rv$final_report <- NULL; rv$appendix_report <- NULL; rv$quality <- NULL
    rv$transcript <- NULL; rv$turns <- NULL; rv$rankings <- NULL; rv$persona_validated <- FALSE
    rv$model_baseline <- NULL; rv$drift_table <- NULL; rv$pre_survey <- NULL; rv$post_survey <- NULL
    rv$auto_dri_survey <- NULL; rv$auto_dri_pre <- NULL; rv$auto_dri_post <- NULL
    rv$user_dri_pre <- NULL; rv$user_dri_post <- NULL; rv$auto_dri_pre_scores <- NULL; rv$auto_dri_post_scores <- NULL
    rv$user_dri_pre_scores <- NULL; rv$user_dri_post_scores <- NULL; rv$auto_dri_comparison <- NULL; rv$user_dri_comparison <- NULL
    rv$central_tendency <- NULL; rv$aqua <- NULL
    rv$user_dri_survey <- keep_user_dri; rv$evidence_text <- keep_evidence_text; rv$evidence_names <- keep_evidence_names
  }
  
  flatten_regular_survey <- function(responses, prefix) {
    extract_numeric_answer <- function(x) {
      if (is.null(x)) return(NA_real_)
      if (is.numeric(x)) return(as.numeric(x)[1])
      raw <- scalar_text(x)
      m <- regexpr("[-+]?[0-9]+(?:\\.[0-9]+)?", raw, perl = TRUE)
      if (m[1] < 0) return(NA_real_)
      suppressWarnings(as.numeric(regmatches(raw, m)[1]))
    }
    out <- list()
    if (is.null(responses)) return(data.frame(respondent = character(), stringsAsFactors = FALSE))
    for (r in responses) {
      nm <- scalar_text(r$respondent %||% r$name %||% "unknown")
      row <- list(respondent = nm)
      qs <- r$responses %||% list()
      if (length(qs)) {
        for (j in seq_along(qs)) {
          q <- qs[[j]]
          lab <- paste0(prefix, "_Q", j)
          ans_raw <- scalar_text(q$answer %||% "")
          ans_num <- extract_numeric_answer(q$answer_numeric %||% q$answer %||% NA)
          row[[paste0(lab, "_question")]] <- scalar_text(q$question %||% "")
          row[[paste0(lab, "_answer_numeric")]] <- ans_num
          row[[paste0(lab, "_answer")]] <- ans_raw
        }
      }
      out[[length(out)+1]] <- as.data.frame(row, stringsAsFactors = FALSE)
    }
    if (!length(out)) data.frame(respondent = character(), stringsAsFactors = FALSE) else dplyr::bind_rows(out)
  }
  
  
  flatten_dri_responses <- function(responses, prefix) {
    out <- list()
    if (is.null(responses)) return(data.frame(respondent = character(), stringsAsFactors = FALSE))
    for (r in responses) {
      nm <- scalar_text(r$respondent %||% r$name %||% "unknown")
      row <- list(respondent = nm)
      cons <- r$considerations %||% list()
      if (length(cons)) for (j in seq_along(cons)) {
        x <- cons[[j]]; id <- scalar_text(x$id %||% paste0("C", j))
        row[[paste0(prefix, "_", id, "_rating")]] <- suppressWarnings(as.numeric(x$score %||% x$rating %||% x$value %||% x$answer_numeric %||% x$answer %||% NA))
        row[[paste0(prefix, "_", id, "_reason")]] <- scalar_text(x$reason %||% x$justification %||% "")
      }
      prefs <- r$preferences %||% list()
      if (length(prefs)) for (j in seq_along(prefs)) {
        x <- prefs[[j]]; id <- scalar_text(x$id %||% paste0("P", j))
        row[[paste0(prefix, "_", id, "_rank")]] <- suppressWarnings(as.numeric(x$rank %||% x$ranking %||% x$value %||% NA))
        row[[paste0(prefix, "_", id, "_reason")]] <- scalar_text(x$reason %||% x$justification %||% "")
      }
      out[[length(out)+1]] <- as.data.frame(row, stringsAsFactors = FALSE)
    }
    if (!length(out)) data.frame(respondent = character(), stringsAsFactors = FALSE) else dplyr::bind_rows(out)
  }
  
  latest_scores_by_speaker <- function() {
    t <- get_turns()
    if (is.null(t) || !nrow(t) || !"position_score" %in% names(t)) return(data.frame(name = character(), latest_position_score = numeric()))
    t |> dplyr::filter(!is.na(position_score), nzchar(speaker)) |> dplyr::group_by(speaker) |> dplyr::slice_tail(n = 1) |> dplyr::ungroup() |> dplyr::transmute(name = speaker, latest_position_score = as.numeric(position_score))
  }
  
  collect_individual_batch_rows <- function(run_id, condition_label = "structured_pipeline") {
    if (is.null(rv$personas)) return(data.frame())
    
    individual_dri_wide <- function(score_obj, prefix) {
      if (is.null(score_obj) || is.null(score_obj$individual) || !is.data.frame(score_obj$individual) || !nrow(score_obj$individual)) {
        return(data.frame(respondent = character(), stringsAsFactors = FALSE))
      }
      x <- as.data.frame(score_obj$individual, stringsAsFactors = FALSE)
      if (!"respondent" %in% names(x)) x$respondent <- NA_character_
      out <- data.frame(respondent = x$respondent, stringsAsFactors = FALSE)
      out[[paste0(prefix, "_DRI")]] <- suppressWarnings(as.numeric(x$DRI %||% NA))
      out[[paste0(prefix, "_avg_ic_point")]] <- suppressWarnings(as.numeric(x$avg_ic_point %||% NA))
      out
    }
    
    df <- as.data.frame(rv$personas, stringsAsFactors = FALSE)
    if (!"name" %in% names(df)) df$name <- paste0("P", seq_len(nrow(df)))
    if (!"id" %in% names(df)) df$id <- paste0("P", seq_len(nrow(df)))
    latest <- latest_scores_by_speaker()
    df <- dplyr::left_join(df, latest, by = "name")
    if (!"final_score" %in% names(df)) df$final_score <- NA_real_
    df$initial_score <- suppressWarnings(as.numeric(df$initial_score %||% NA))
    df$final_score_resolved <- dplyr::coalesce(suppressWarnings(as.numeric(df$final_score)), suppressWarnings(as.numeric(df$latest_position_score)))
    df$final_score <- df$final_score_resolved
    df$opinion_delta <- df$final_score - df$initial_score
    df$run_id <- run_id; df$condition <- condition_label; df$treatment <- condition_label; df$run <- run_id
    df$model <- input$model %||% ""; df$temperature <- as.numeric(input$temperature %||% NA)
    base <- df |> dplyr::relocate(run_id, condition, treatment, run, model, temperature, id, name)
    
    pieces <- list(
      base,
      flatten_regular_survey(rv$pre_survey, "survey_pre"),
      flatten_regular_survey(rv$post_survey, "survey_post"),
      flatten_dri_responses(rv$auto_dri_pre, "auto_dri_pre"),
      flatten_dri_responses(rv$auto_dri_post, "auto_dri_post"),
      flatten_dri_responses(rv$user_dri_pre, "user_dri_pre"),
      flatten_dri_responses(rv$user_dri_post, "user_dri_post"),
      individual_dri_wide(rv$auto_dri_pre_scores, "auto_dri_pre"),
      individual_dri_wide(rv$auto_dri_post_scores, "auto_dri_post"),
      individual_dri_wide(rv$user_dri_pre_scores, "user_dri_pre"),
      individual_dri_wide(rv$user_dri_post_scores, "user_dri_post")
    )
    merged <- pieces[[1]]
    for (p in pieces[-1]) {
      if (is.data.frame(p) && nrow(p) > 0 && "respondent" %in% names(p)) merged <- dplyr::left_join(merged, p, by = c("name" = "respondent"))
    }
    
    # DRI_pooled-compatible columns: prefer uploaded/user DRI when available; otherwise use automated DRI.
    col_num_or_na <- function(dat, nm) {
      if (nm %in% names(dat)) suppressWarnings(as.numeric(dat[[nm]])) else rep(NA_real_, nrow(dat))
    }
    merged$DRI_pre <- dplyr::coalesce(col_num_or_na(merged, "user_dri_pre_DRI"), col_num_or_na(merged, "auto_dri_pre_DRI"))
    merged$DRI_post <- dplyr::coalesce(col_num_or_na(merged, "user_dri_post_DRI"), col_num_or_na(merged, "auto_dri_post_DRI"))
    merged$delta <- merged$DRI_post - merged$DRI_pre
    merged
  }
  
  
  score_from_quality <- function(dim) {
    suppressWarnings(as.numeric(rv$quality$quality[[dim]]$score %||% NA))
  }
  
  collect_group_batch_row <- function(run_id, condition_label = "structured_pipeline") {
    proc <- compute_process_stats(get_turns(), if (!is.null(rv$personas)) nrow(rv$personas) else 0)
    init <- if (!is.null(rv$personas) && "initial_score" %in% names(rv$personas)) suppressWarnings(as.numeric(rv$personas$initial_score)) else NA_real_
    fin <- if (!is.null(rv$personas) && "final_score" %in% names(rv$personas)) suppressWarnings(as.numeric(rv$personas$final_score)) else NA_real_
    if (all(is.na(fin))) {
      ls <- latest_scores_by_speaker()
      if (!is.null(rv$personas) && nrow(ls)) fin <- ls$latest_position_score[match(rv$personas$name, ls$name)]
    }
    aqua_overall <- NA_real_
    aqua_median <- NA_real_
    aqua_n_comments <- NA_real_
    if (!is.null(rv$aqua)) {
      # compute_aqua_llm() returns overview$mean_aqua, not overall_score.
      aqua_overall <- suppressWarnings(as.numeric(rv$aqua$overview$mean_aqua %||% rv$aqua$mean_aqua %||% rv$aqua$overall_score %||% NA))
      aqua_median <- suppressWarnings(as.numeric(rv$aqua$overview$median_aqua %||% NA))
      aqua_n_comments <- suppressWarnings(as.numeric(rv$aqua$overview$n_comments %||% NA))
      if (is.na(aqua_overall) && !is.null(rv$aqua$comments) && is.data.frame(rv$aqua$comments) && "aqua_score" %in% names(rv$aqua$comments)) {
        aqua_overall <- mean(as.numeric(rv$aqua$comments$aqua_score), na.rm = TRUE)
      }
    }
    
    data.frame(
      run_id = run_id, condition = condition_label, model = input$model %||% "", temperature = as.numeric(input$temperature %||% NA),
      n_participants = if (!is.null(rv$personas)) nrow(rv$personas) else NA_integer_, n_groups = as.integer(input$n_groups %||% NA),
      pre_mean_opinion = mean(init, na.rm = TRUE), post_mean_opinion = mean(fin, na.rm = TRUE), opinion_delta = mean(fin, na.rm = TRUE) - mean(init, na.rm = TRUE),
      pre_polarization_sd = sd(init, na.rm = TRUE), post_polarization_sd = sd(fin, na.rm = TRUE), polarization_delta = sd(fin, na.rm = TRUE) - sd(init, na.rm = TRUE),
      total_turns = proc$total_turns, total_words = proc$total_words, avg_words_per_turn = proc$avg_words_per_turn, participation_gini = proc$gini,
      auto_dri_pre = suppressWarnings(as.numeric(rv$auto_dri_pre_scores$group_dri %||% NA)), auto_dri_post = suppressWarnings(as.numeric(rv$auto_dri_post_scores$group_dri %||% NA)),
      auto_dri_delta = suppressWarnings(as.numeric(rv$auto_dri_post_scores$group_dri %||% NA) - as.numeric(rv$auto_dri_pre_scores$group_dri %||% NA)),
      user_dri_pre = suppressWarnings(as.numeric(rv$user_dri_pre_scores$group_dri %||% NA)), user_dri_post = suppressWarnings(as.numeric(rv$user_dri_post_scores$group_dri %||% NA)),
      user_dri_delta = suppressWarnings(as.numeric(rv$user_dri_post_scores$group_dri %||% NA) - as.numeric(rv$user_dri_pre_scores$group_dri %||% NA)),
      aqua_score = aqua_overall, aqua_median = aqua_median, aqua_n_comments = aqua_n_comments,
      q_inclusion = score_from_quality("inclusion"), q_equality = score_from_quality("equality"), q_reason_giving = score_from_quality("reason_giving"),
      q_respect = score_from_quality("respect"), q_reflection = score_from_quality("reflection"), q_common_good = score_from_quality("common_good"),
      stringsAsFactors = FALSE
    )
  }
  
  
  
  run_batch_post_metrics_no_report <- function(condition_label = "batch") {
    # Batch mode intentionally skips final report and appendix generation.
    # It only creates post-deliberation respondent data and group-level metrics.
    temp <- min(1.0, max(0, as.numeric(input$temperature %||% 0.3)))
    
    if (!is.null(rv$personas)) {
      rv$personas <- tryCatch(update_final_scores_from_turns(rv$personas, get_turns()), error = function(e) rv$personas)
      if (!"final_score" %in% names(rv$personas) || all(!is.finite(suppressWarnings(as.numeric(rv$personas$final_score))))) {
        rv$personas$final_score <- rv$personas$initial_score
      }
    }
    
    cards <- if (!is.null(rv$personas)) lapply(seq_len(nrow(rv$personas)), function(i) as.list(rv$personas[i, ])) else list()
    
    if (length(cards) > 0 && !is.null(rv$auto_dri_survey)) {
      auto_post_results <- vector("list", length(cards))
      for (i in seq_along(cards)) {
        auto_post_results[[i]] <- tryCatch({
          complete_dri_response(input$api_key, input$model, cards[[i]], rv$auto_dri_survey, "post", temperature = temp)
        }, error = function(e) stop(conditionMessage(e)))
      }
      rv$auto_dri_post <- auto_post_results
      rv$auto_dri_post_scores <- tryCatch(compute_dri_from_responses(rv$auto_dri_post, rv$auto_dri_survey, timing = "post"), error = function(e) NULL)
      rv$auto_dri_wilcox <- tryCatch(compute_dri_prepost_wilcox(rv$auto_dri_pre_scores, rv$auto_dri_post_scores), error = function(e) NULL)
      if (!is.null(rv$auto_dri_pre_scores) && !is.null(rv$auto_dri_post_scores)) {
        rv$auto_dri_comparison <- tryCatch(full_join(rv$auto_dri_pre_scores$individual, rv$auto_dri_post_scores$individual, by = "respondent", suffix = c("_pre", "_post")), error = function(e) NULL)
      }
      add_replog("auto_dri_post_batch_no_report")
    }
    
    if (length(cards) > 0 && !is.null(rv$user_dri_survey)) {
      user_post_results <- vector("list", length(cards))
      for (i in seq_along(cards)) {
        user_post_results[[i]] <- tryCatch({
          complete_dri_response(input$api_key, input$model, cards[[i]], rv$user_dri_survey, "post", temperature = temp)
        }, error = function(e) stop(conditionMessage(e)))
      }
      rv$user_dri_post <- user_post_results
      rv$user_dri_post_scores <- tryCatch(compute_dri_from_responses(rv$user_dri_post, rv$user_dri_survey, timing = "post"), error = function(e) NULL)
      rv$user_dri_wilcox <- tryCatch(compute_dri_prepost_wilcox(rv$user_dri_pre_scores, rv$user_dri_post_scores), error = function(e) NULL)
      if (!is.null(rv$user_dri_pre_scores) && !is.null(rv$user_dri_post_scores)) {
        rv$user_dri_comparison <- tryCatch(full_join(rv$user_dri_pre_scores$individual, rv$user_dri_post_scores$individual, by = "respondent", suffix = c("_pre", "_post")), error = function(e) NULL)
      }
      add_replog("user_dri_post_batch_no_report")
    }
    
    if (length(cards) > 0 && isTRUE(rv$survey_enabled) && nzchar(rv$survey_text %||% "")) {
      post_results <- vector("list", length(cards))
      for (i in seq_along(cards)) {
        post_results[[i]] <- tryCatch({
          txt <- call_openrouter(
            input$api_key, input$model,
            list(
              list(role = "system", content = "You complete surveys in character. Return ONLY valid JSON."),
              list(role = "user", content = survey_prompt(cards[[i]], rv$survey_text, "post"))
            ),
            max_tokens = 2500, temperature = temp
          )
          coerce_json(txt)
        }, error = function(e) list(respondent = scalar_text(cards[[i]]$name), timing = "post", responses = list(list(question = "[ERROR]", answer = conditionMessage(e), justification = ""))))
      }
      rv$post_survey <- post_results
      add_replog("post_survey_batch_no_report")
    }
    
    rv$central_tendency <- tryCatch(compute_central_tendency_stats(rv$personas, rv$model_baseline), error = function(e) NULL)
    
    # Keep AQuA in batch because it is a group-level dataset variable. This does not generate a report.
    turns_for_aqua <- tryCatch(get_turns(), error = function(e) NULL)
    if (!is.null(turns_for_aqua) && nrow(turns_for_aqua) > 0) {
      rv$aqua <- tryCatch(compute_aqua_llm(turns_for_aqua, input$api_key, input$model, temperature = 0.0), error = function(e) NULL)
      add_replog("aqua_llm_scoring_batch_no_report")
    }
    
    rv$final_report <- NULL
    rv$appendix_report <- NULL
    rv$quality <- NULL
    rv$status <- paste0("Batch no-report metrics completed for ", condition_label, ".")
  }
  
  
  # ---- Robust batch checkpoint helpers ----
  sanitize_batch_value <- function(x) {
    if (is.null(x)) return(NA)
    if (is.data.frame(x)) return(paste(capture.output(utils::str(x, max.level = 1)), collapse = " | "))
    if (is.list(x)) return(jsonlite::toJSON(x, auto_unbox = TRUE, null = "null"))
    x
  }
  
  safe_batch_df <- function(x) {
    if (is.null(x) || !is.data.frame(x)) return(data.frame())
    for (nm in names(x)) {
      if (is.list(x[[nm]]) || is.data.frame(x[[nm]])) {
        x[[nm]] <- vapply(x[[nm]], function(z) scalar_text(sanitize_batch_value(z)), character(1))
      }
    }
    x
  }
  
  batch_dir <- function() {
    d <- file.path(tempdir(), paste0("deliberation_batch_", session$token))
    if (!dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
    d
  }
  
  checkpoint_batch <- function(kind, new_rows = NULL) {
    kind <- match.arg(kind, c("individual", "group", "errors"))
    if (kind == "individual") {
      current <- safe_batch_df(rv$batch_individual)
      if (!is.null(new_rows) && is.data.frame(new_rows) && nrow(new_rows)) current <- dplyr::bind_rows(current, safe_batch_df(new_rows))
      rv$batch_individual <- current
      utils::write.csv(current, file.path(batch_dir(), "batch_individual_checkpoint.csv"), row.names = FALSE, na = "")
    } else if (kind == "group") {
      current <- safe_batch_df(rv$batch_group)
      if (!is.null(new_rows) && is.data.frame(new_rows) && nrow(new_rows)) current <- dplyr::bind_rows(current, safe_batch_df(new_rows))
      rv$batch_group <- current
      utils::write.csv(current, file.path(batch_dir(), "batch_group_checkpoint.csv"), row.names = FALSE, na = "")
    } else {
      current <- safe_batch_df(rv$batch_errors %||% data.frame())
      if (!is.null(new_rows) && is.data.frame(new_rows) && nrow(new_rows)) current <- dplyr::bind_rows(current, safe_batch_df(new_rows))
      rv$batch_errors <- current
      utils::write.csv(current, file.path(batch_dir(), "batch_errors_checkpoint.csv"), row.names = FALSE, na = "")
    }
  }
  
  restore_batch_checkpoints <- function() {
    ind <- file.path(batch_dir(), "batch_individual_checkpoint.csv")
    grp <- file.path(batch_dir(), "batch_group_checkpoint.csv")
    err <- file.path(batch_dir(), "batch_errors_checkpoint.csv")
    if (file.exists(ind)) rv$batch_individual <- tryCatch(utils::read.csv(ind, stringsAsFactors = FALSE, check.names = FALSE), error = function(e) rv$batch_individual)
    if (file.exists(grp)) rv$batch_group <- tryCatch(utils::read.csv(grp, stringsAsFactors = FALSE, check.names = FALSE), error = function(e) rv$batch_group)
    if (file.exists(err)) rv$batch_errors <- tryCatch(utils::read.csv(err, stringsAsFactors = FALSE, check.names = FALSE), error = function(e) rv$batch_errors)
  }
  
  output$batch_status_text <- renderText({ rv$batch_status %||% "" })
  observeEvent(input$clear_batch, {
    rv$batch_individual <- data.frame(); rv$batch_group <- data.frame(); rv$batch_errors <- data.frame()
    rv$batch_run_counter <- 0L; rv$batch_status <- "Batch data cleared."
    unlink(file.path(batch_dir(), c("batch_individual_checkpoint.csv", "batch_group_checkpoint.csv", "batch_errors_checkpoint.csv")), force = TRUE)
  })
  
  observeEvent(input$run_batch, {
    req(!rv$batch_running)
    rv$batch_running <- TRUE
    on.exit({ rv$batch_running <- FALSE; gc(verbose = FALSE) }, add = TRUE)
    
    restore_batch_checkpoints()
    n_runs <- max(1L, min(50L, as.integer(input$batch_n_runs %||% 1L)))
    steps <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13)  # dataset-only batch skips report/appendix and full quality report generation
    
    withProgress(message = paste0("Running ", n_runs, " concrete replication(s)..."), value = 0, {
      for (rr in seq_len(n_runs)) {
        rv$batch_run_counter <- rv$batch_run_counter + 1L
        run_id <- rv$batch_run_counter
        rv$batch_status <- paste0("Running batch replication ", rr, " of ", n_runs, " (run_id=", run_id, ")")
        
        run_error <- NULL
        run_result <- tryCatch({
          reset_run_state_for_batch()
          for (st in steps) {
            incProgress(0.70 / (n_runs * length(steps)), detail = paste0("run_id=", run_id, ", step=", st))
            rv$error <- ""
            run_workflow_step(st)
            if (nzchar(rv$error %||% "")) stop(paste0("step ", st, ": ", rv$error))
          }
          
          incProgress(0.15 / n_runs, detail = paste0("run_id=", run_id, ", post metrics only; no report"))
          run_batch_post_metrics_no_report("structured_pipeline")
          
          ind_rows <- collect_individual_batch_rows(run_id, "structured_pipeline")
          grp_rows <- collect_group_batch_row(run_id, "structured_pipeline")
          list(individual = ind_rows, group = grp_rows)
        }, error = function(e) {
          run_error <<- conditionMessage(e)
          NULL
        })
        
        if (!is.null(run_result)) {
          checkpoint_batch("individual", run_result$individual)
          checkpoint_batch("group", run_result$group)
          rv$batch_status <- paste0("Completed run_id=", run_id, ". Checkpoint saved. Individual rows=", nrow(rv$batch_individual), "; group rows=", nrow(rv$batch_group), ".")
        } else {
          err_row <- data.frame(run_id = run_id, condition = "structured_pipeline", model = input$model %||% "", error = run_error %||% "unknown error", stringsAsFactors = FALSE)
          checkpoint_batch("errors", err_row)
          rv$batch_status <- paste0("Run_id=", run_id, " failed but batch continued: ", run_error)
        }
        
        rv$final_report <- NULL; rv$appendix_report <- NULL; rv$quality <- NULL
        rv$transcript <- NULL; rv$turns <- NULL; rv$groups <- NULL
        rv$group_values <- NULL; rv$group_options <- NULL; rv$evaluations <- NULL
        gc(verbose = FALSE)
        incProgress(0.15 / n_runs, detail = paste0("run_id=", run_id, ", checkpoint complete"))
      }
    })
    
    restore_batch_checkpoints()
    rv$final_report <- NULL
    rv$appendix_report <- NULL
    rv$quality <- NULL
    rv$view <- "step"
    rv$step <- 0L
    n_err <- if (is.data.frame(rv$batch_errors)) nrow(rv$batch_errors) else 0L
    rv$status <- "Batch completed. Download the individual-level and group-level CSV files below."
    rv$batch_status <- paste0("Batch completed. Stored ", nrow(rv$batch_individual), " individual rows and ", nrow(rv$batch_group), " group rows. Failed runs: ", n_err, ". No report was generated.")
  })
  
  output$dl_batch_individual <- downloadHandler(filename = function() paste0("structured_pipeline_individual_level_batch.csv"), content = function(file) { restore_batch_checkpoints(); utils::write.csv(safe_batch_df(rv$batch_individual), file, row.names = FALSE, na = "") })
  output$dl_batch_group <- downloadHandler(filename = function() paste0("structured_pipeline_group_level_batch.csv"), content = function(file) { restore_batch_checkpoints(); utils::write.csv(safe_batch_df(rv$batch_group), file, row.names = FALSE, na = "") })
  
  output$dl_batch_errors <- downloadHandler(filename = function() paste0("structured_pipeline_failed_runs.csv"), content = function(file) { restore_batch_checkpoints(); utils::write.csv(safe_batch_df(rv$batch_errors %||% data.frame()), file, row.names = FALSE, na = "") })
  
  # ====== DOWNLOADS ======
  make_report_rmd <- function() {
    proc <- compute_process_stats(get_turns(), if (!is.null(rv$personas)) nrow(rv$personas) else 0)
    paste0("---\ntitle: \"Deliberation Report\"\noutput: pdf_document\n---\n\n",
           "# Final Report\n\n", gsub("\n", "\n\n", rv$final_report %||% ""), "\n\n",
           "# Assembly Composition\n\n",
           if (!is.null(rv$personas)) paste0(
             "- **Participants:** ", nrow(rv$personas), "\n",
             "- **Gender:** ", paste(names(table(rv$personas$gender)), table(rv$personas$gender), sep = ": ", collapse = ", "), "\n",
             "- **Education:** ", paste(names(table(rv$personas$education)), table(rv$personas$education), sep = ": ", collapse = ", "), "\n",
             if ("settlement" %in% names(rv$personas)) paste0("- **Settlement:** ", paste(names(table(rv$personas$settlement)), table(rv$personas$settlement), sep = ": ", collapse = ", "), "\n") else "",
             "\n") else "",
           "# Process Analytics\n\n",
           "- **Total turns:** ", proc$total_turns, "\n",
           "- **Total words:** ", proc$total_words, "\n",
           "- **Avg words/turn:** ", proc$avg_words_per_turn, "\n",
           "- **Gini coefficient (equity):** ", proc$gini, " (0 = perfectly equal, 1 = one person dominated)\n",
           "- **Most active:** ", proc$most_active, "\n",
           "- **Least active:** ", proc$least_active, "\n\n",
           "# Design Integrity\n\n```json\n", safe_json(rv$integrity), "\n```\n\n",
           "# Model Bias Analysis (Taubenfeld et al. 2024)\n\n",
           if (!is.null(rv$model_baseline)) paste0(
             "- **Model used:** ", input$model %||% "unknown", "\n",
             "- **Model default bias score:** ", scalar_text(rv$model_baseline$baseline$position_score %||% "N/A"), "/5\n",
             "- **Reasoning:** ", scalar_text(rv$model_baseline$baseline$reasoning %||% ""), "\n",
             "- **Note:** LLM agents tend to converge toward the model's inherent bias regardless of assigned persona (Taubenfeld et al. 2024). Monitor position drift charts for evidence of this pattern.\n\n"
           ) else "",
           "# Quality Assessment\n\n```json\n", safe_json(rv$quality), "\n```\n\n",
           "# Replicability Log\n\n```json\n", safe_json(rv$replog), "\n```\n")
  }
  make_transcript_rmd <- function() {
    paste0("---\ntitle: \"Deliberation Transcript\"\noutput: pdf_document\n---\n\n",
           gsub("\n", "\n\n", rv$transcript %||% ""), "\n\n",
           "# Replicability Log\n\n```json\n", safe_json(rv$replog), "\n```\n")
  }
  render_rmd_to_pdf <- function(rmd_text, file) {
    if (!HAS_RMARKDOWN) stop("Please install the rmarkdown package to export PDF.")
    tmp <- tempfile(fileext = ".Rmd"); writeLines(rmd_text, tmp)
    rmarkdown::render(tmp, output_file = basename(file), output_dir = dirname(file), quiet = TRUE, envir = new.env(parent = globalenv()))
  }
  output$dl_report_pdf <- downloadHandler(filename = function() "deliberation_report.pdf", content = function(f) render_rmd_to_pdf(make_report_rmd(), f))
  output$dl_report_pdf2 <- downloadHandler(filename = function() "deliberation_report.pdf", content = function(f) render_rmd_to_pdf(make_report_rmd(), f))
  output$dl_transcript_pdf <- downloadHandler(filename = function() "deliberation_transcript.pdf", content = function(f) render_rmd_to_pdf(make_transcript_rmd(), f))
  output$dl_transcript_pdf2 <- downloadHandler(filename = function() "deliberation_transcript.pdf", content = function(f) render_rmd_to_pdf(make_transcript_rmd(), f))
  
  # Appendix PDF
  make_appendix_rmd <- function() {
    output_type <- input$output_type %||% "policy_recommendation"
    type_label <- switch(output_type,
                         "policy_recommendation" = "Policy Recommendation",
                         "policy_support_opposition" = "Policy Support / Opposition",
                         "idea_generation" = "Idea Generation")
    paste0("---\ntitle: \"Deliberation Appendix\"\nsubtitle: \"", type_label, "\"\noutput: pdf_document\n---\n\n",
           gsub("\n", "\n\n", rv$appendix_report %||% "[Appendix not yet generated]"), "\n")
  }
  output$dl_appendix_pdf <- downloadHandler(filename = function() "deliberation_appendix.pdf",
                                            content = function(f) render_rmd_to_pdf(make_appendix_rmd(), f))
  
  # Full report + appendix combined PDF
  make_full_report_rmd <- function() {
    paste0(make_report_rmd(),
           "\n\n\\newpage\n\n",
           "# APPENDIX\n\n",
           gsub("\n", "\n\n", rv$appendix_report %||% "[Appendix not yet generated]"), "\n")
  }
  output$dl_full_report_pdf <- downloadHandler(filename = function() "deliberation_full_report.pdf",
                                               content = function(f) render_rmd_to_pdf(make_full_report_rmd(), f))
  output$dl_full_report_pdf2 <- downloadHandler(filename = function() "deliberation_full_report.pdf",
                                                content = function(f) render_rmd_to_pdf(make_full_report_rmd(), f))
  make_csv <- function(file) {
    req(rv$personas); out <- rv$personas
    if (!is.null(rv$replog) && nrow(rv$replog) > 0) { out$run_model <- input$model %||% ""; out$run_temperature <- as.numeric(input$temperature %||% 0.3); out$run_groups <- as.integer(input$n_groups %||% 2) }
    write.csv(out, file, row.names = FALSE)
  }
  output$dl_csv <- downloadHandler(filename = function() "deliberation_outputs.csv", content = make_csv)
  output$dl_csv2 <- downloadHandler(filename = function() "deliberation_outputs.csv", content = make_csv)
  output$dl_csv3 <- downloadHandler(filename = function() "deliberation_outputs.csv", content = make_csv)
  # ============================================================
  # DRI WIDE-FORMAT EXPORT HELPER
  # Columns: name, stage (Pre/Post), C1...Cn (consideration scores), P (ranked option, as "P1>P2>P3")
  # ============================================================
  dri_to_wide_csv <- function(response_list_pre, response_list_post, survey_obj) {
    if (is.null(survey_obj)) return(data.frame(message = "No DRI survey available"))
    
    c_ids  <- vapply(survey_obj$considerations, function(x) scalar_text(x$id  %||% ""), character(1))
    p_ids  <- vapply(survey_obj$policy_options, function(x) scalar_text(x$id  %||% ""), character(1))
    c_lbls <- vapply(survey_obj$considerations, function(x) scalar_text(x$text %||% x$id %||% ""), character(1))
    p_lbls <- vapply(survey_obj$policy_options, function(x) scalar_text(x$text %||% x$id %||% ""), character(1))
    
    build_rows <- function(resp_list, stage_label) {
      if (is.null(resp_list) || length(resp_list) == 0) return(data.frame())
      bind_rows(lapply(resp_list, function(resp) {
        nm <- scalar_text(resp$respondent %||% "unknown")
        
        # Consideration scores
        c_scores <- setNames(rep(NA_real_, length(c_ids)), c_ids)
        if (!is.null(resp$considerations)) {
          for (x in resp$considerations) {
            id <- scalar_text(x$id %||% "")
            if (id %in% c_ids) c_scores[[id]] <- as.numeric(x$score %||% NA)
          }
        }
        
        # Preference ranking — encode as ordered string "P1>P3>P2"
        p_ranks <- setNames(rep(NA_real_, length(p_ids)), p_ids)
        if (!is.null(resp$preferences)) {
          for (x in resp$preferences) {
            id <- scalar_text(x$id %||% "")
            if (id %in% p_ids) p_ranks[[id]] <- as.numeric(x$rank %||% NA)
          }
        }
        rank_str <- if (all(is.na(p_ranks))) NA_character_ else {
          ord <- order(p_ranks, na.last = TRUE)
          paste(p_ids[ord], collapse = ">")
        }
        
        row <- data.frame(name = nm, stage = stage_label, stringsAsFactors = FALSE)
        for (i in seq_along(c_ids)) row[[c_ids[i]]] <- c_scores[[c_ids[i]]]
        row[["P"]] <- rank_str
        row
      }))
    }
    
    pre_rows  <- build_rows(response_list_pre,  "Pre")
    post_rows <- build_rows(response_list_post, "Post")
    out <- dplyr::bind_rows(pre_rows, post_rows)
    
    # Add readable labels as comment row header? No — keep machine-readable; add a metadata sheet approach:
    # Prepend a legend as the first N rows would break CSV; instead rename columns with short label
    # Map C1->C1 (keep IDs) for machine-readability; add attr for reference
    out
  }
  
  output$dl_auto_dri_wide <- downloadHandler(
    filename = function() "auto_dri_respondents_wide.csv",
    contentType = "text/csv; charset=UTF-8",
    content = function(file) {
      out <- dri_to_wide_csv(rv$auto_dri_pre, rv$auto_dri_post, rv$auto_dri_survey)
      write.csv(out, file, row.names = FALSE, na = "")
    }
  )
  
  output$dl_user_dri_wide <- downloadHandler(
    filename = function() "user_dri_respondents_wide.csv",
    contentType = "text/csv; charset=UTF-8",
    content = function(file) {
      out <- dri_to_wide_csv(rv$user_dri_pre, rv$user_dri_post, rv$user_dri_survey)
      write.csv(out, file, row.names = FALSE, na = "")
    }
  )
  
  output$dl_auto_dri_questions <- downloadHandler(
    filename = function() "auto_dri_questions.csv",
    contentType = "text/csv; charset=UTF-8",
    content = function(file) {
      out <- if (!is.null(rv$auto_dri_survey)) dri_questions_to_df(rv$auto_dri_survey) else data.frame(message = "No automated DRI survey available")
      write.csv(out, file, row.names = FALSE)
    }
  )
  
  output$dl_auto_dri_pre_raw <- downloadHandler(
    filename = function() "auto_dri_pre_raw.csv",
    contentType = "text/csv; charset=UTF-8",
    content = function(file) {
      out <- if (!is.null(rv$auto_dri_pre) && !is.null(rv$auto_dri_survey)) dri_raw_to_df(rv$auto_dri_pre, rv$auto_dri_survey, "pre") else data.frame(message = "No automated pre DRI data available")
      write.csv(out, file, row.names = FALSE)
    }
  )
  
  output$dl_auto_dri_post_raw <- downloadHandler(
    filename = function() "auto_dri_post_raw.csv",
    contentType = "text/csv; charset=UTF-8",
    content = function(file) {
      out <- if (!is.null(rv$auto_dri_post) && !is.null(rv$auto_dri_survey)) dri_raw_to_df(rv$auto_dri_post, rv$auto_dri_survey, "post") else data.frame(message = "No automated post DRI data available")
      write.csv(out, file, row.names = FALSE)
    }
  )
  
  output$dl_auto_dri_comparison <- downloadHandler(
    filename = function() "auto_dri_comparison.csv",
    contentType = "text/csv; charset=UTF-8",
    content = function(file) {
      out <- rv$auto_dri_comparison %||% data.frame(message = "No automated DRI comparison available")
      write.csv(out, file, row.names = FALSE)
    }
  )
  
  output$dl_auto_dri_all <- downloadHandler(
    filename = function() "auto_dri_all.csv",
    contentType = "text/csv; charset=UTF-8",
    content = function(file) {
      pre_df <- if (!is.null(rv$auto_dri_pre) && !is.null(rv$auto_dri_survey)) dri_raw_to_df(rv$auto_dri_pre, rv$auto_dri_survey, "pre") else data.frame()
      post_df <- if (!is.null(rv$auto_dri_post) && !is.null(rv$auto_dri_survey)) dri_raw_to_df(rv$auto_dri_post, rv$auto_dri_survey, "post") else data.frame()
      cmp_df <- rv$auto_dri_comparison %||% data.frame()
      if (nrow(pre_df) == 0 && nrow(post_df) == 0 && nrow(cmp_df) == 0) {
        out <- data.frame(message = "No automated DRI data available")
      } else {
        if (nrow(pre_df) > 0) pre_df$export_block <- "auto_pre_raw"
        if (nrow(post_df) > 0) post_df$export_block <- "auto_post_raw"
        if (nrow(cmp_df) > 0) cmp_df$export_block <- "auto_comparison"
        out <- dplyr::bind_rows(pre_df, post_df, cmp_df)
      }
      write.csv(out, file, row.names = FALSE)
    }
  )
  
  output$dl_user_dri_questions <- downloadHandler(
    filename = function() "uploaded_dri_questions.csv",
    contentType = "text/csv; charset=UTF-8",
    content = function(file) {
      out <- if (!is.null(rv$user_dri_survey)) dri_questions_to_df(rv$user_dri_survey) else data.frame(message = "No uploaded DRI survey available")
      write.csv(out, file, row.names = FALSE)
    }
  )
  
  output$dl_user_dri_pre_raw <- downloadHandler(
    filename = function() "uploaded_dri_pre_raw.csv",
    contentType = "text/csv; charset=UTF-8",
    content = function(file) {
      out <- if (!is.null(rv$user_dri_pre) && !is.null(rv$user_dri_survey)) dri_raw_to_df(rv$user_dri_pre, rv$user_dri_survey, "pre") else data.frame(message = "No uploaded pre DRI data available")
      write.csv(out, file, row.names = FALSE)
    }
  )
  
  output$dl_user_dri_post_raw <- downloadHandler(
    filename = function() "uploaded_dri_post_raw.csv",
    contentType = "text/csv; charset=UTF-8",
    content = function(file) {
      out <- if (!is.null(rv$user_dri_post) && !is.null(rv$user_dri_survey)) dri_raw_to_df(rv$user_dri_post, rv$user_dri_survey, "post") else data.frame(message = "No uploaded post DRI data available")
      write.csv(out, file, row.names = FALSE)
    }
  )
  
  output$dl_user_dri_comparison <- downloadHandler(
    filename = function() "uploaded_dri_comparison.csv",
    contentType = "text/csv; charset=UTF-8",
    content = function(file) {
      out <- rv$user_dri_comparison %||% data.frame(message = "No uploaded DRI comparison available")
      write.csv(out, file, row.names = FALSE)
    }
  )
  # --- Survey Report Downloads ---
  make_survey_rmd <- function() {
    pn <- if (!is.null(rv$personas)) rv$personas$name else NULL
    pre_df  <- if (!is.null(rv$pre_survey))  survey_to_df(rv$pre_survey, "Pre", pn)  else NULL
    post_df <- if (!is.null(rv$post_survey)) survey_to_df(rv$post_survey, "Post", pn) else NULL
    summary <- if (!is.null(pre_df) && !is.null(post_df)) compute_survey_summary(pre_df, post_df) else NULL
    
    rmd <- paste0("---\ntitle: \"Deliberation Survey Report\"\noutput: pdf_document\n---\n\n")
    
    if (!is.null(summary)) {
      rmd <- paste0(rmd,
                    "# Summary\n\n",
                    "- **Respondents:** ", summary$n_respondents, "\n",
                    "- **Questions:** ", summary$n_questions, "\n",
                    "- **Answers changed:** ", summary$pct_changed, "%\n",
                    "- **Overall convergence:** ", summary$convergence, "\n\n")
    }
    
    if (!is.null(pre_df)) {
      rmd <- paste0(rmd, "# Pre-Deliberation Responses\n\n")
      for (nm in unique(pre_df$respondent)) {
        rmd <- paste0(rmd, "## ", nm, "\n\n")
        sub <- pre_df[pre_df$respondent == nm, ]
        for (j in seq_len(nrow(sub))) {
          rmd <- paste0(rmd, "**Q:** ", sub$question[j], "\n\n",
                        "**A:** ", sub$answer[j], "\n\n",
                        "*Justification:* ", sub$justification[j], "\n\n")
        }
      }
    }
    
    if (!is.null(post_df)) {
      rmd <- paste0(rmd, "# Post-Deliberation Responses\n\n")
      for (nm in unique(post_df$respondent)) {
        rmd <- paste0(rmd, "## ", nm, "\n\n")
        sub <- post_df[post_df$respondent == nm, ]
        for (j in seq_len(nrow(sub))) {
          rmd <- paste0(rmd, "**Q:** ", sub$question[j], "\n\n",
                        "**A:** ", sub$answer[j], "\n\n",
                        "*Justification:* ", sub$justification[j], "\n\n")
        }
      }
    }
    
    if (!is.null(pre_df) && !is.null(post_df)) {
      rmd <- paste0(rmd, "# Side-by-Side Comparison\n\n")
      merged <- inner_join(
        pre_df |> select(respondent, question, answer_pre = answer),
        post_df |> select(respondent, question, answer_post = answer),
        by = c("respondent", "question"))
      merged$changed <- ifelse(merged$answer_pre != merged$answer_post, "YES", "")
      for (j in seq_len(nrow(merged))) {
        rmd <- paste0(rmd, "**", merged$respondent[j], " \u2014 ", merged$question[j], "**\n\n",
                      "- Pre: ", merged$answer_pre[j], "\n",
                      "- Post: ", merged$answer_post[j], "\n",
                      if (nzchar(merged$changed[j])) "- **CHANGED**\n" else "",
                      "\n")
      }
    }
    rmd
  }
  
  output$dl_aqua_pdf <- downloadHandler(
    filename = function() "aqua_analysis.pdf",
    content = function(f) {
      src1 <- file.path(getwd(), "www", "aqua_analysis.pdf")
      src2 <- file.path(getwd(), "aqua_analysis.pdf")
      src <- if (file.exists(src1)) src1 else src2
      if (!file.exists(src)) stop("aqua_analysis.pdf not found.")
      file.copy(src, f, overwrite = TRUE)
    })
  
  # ============================================================
  # PER-PLOT DOWNLOAD HANDLERS
  # ============================================================
  save_ggplot_png <- function(plot_fn, file, width = 10, height = 7) {
    tmp <- tempfile(fileext = ".png")
    ggplot2::ggsave(tmp, plot = plot_fn(), width = width, height = height, dpi = 150, bg = "white")
    file.copy(tmp, file, overwrite = TRUE)
  }
  
  plot_dl <- function(output_id, filename, plot_fn, width = 10, height = 7) {
    output[[paste0("dl_plot_", output_id)]] <- downloadHandler(
      filename = function() filename,
      content  = function(f) save_ggplot_png(plot_fn, f, width, height)
    )
  }
  
  plot_dl("auto_dri_pre",       "auto_dri_pre.png",       function() make_dri_scatter(if (!is.null(rv$auto_dri_pre_scores)) rv$auto_dri_pre_scores$pairwise else NULL, "Automated DRI — Pre", if (!is.null(rv$auto_dri_pre_scores)) rv$auto_dri_pre_scores$group_dri else NA_real_))
  plot_dl("auto_dri_post",      "auto_dri_post.png",      function() make_dri_scatter(if (!is.null(rv$auto_dri_post_scores)) rv$auto_dri_post_scores$pairwise else NULL, "Automated DRI — Post", if (!is.null(rv$auto_dri_post_scores)) rv$auto_dri_post_scores$group_dri else NA_real_))
  plot_dl("auto_dri_change",    "auto_dri_change.png",    function() make_dri_change_plot(if (!is.null(rv$auto_dri_pre_scores)) rv$auto_dri_pre_scores$individual else NULL, if (!is.null(rv$auto_dri_post_scores)) rv$auto_dri_post_scores$individual else NULL))
  plot_dl("user_dri_pre",       "user_dri_pre.png",       function() make_dri_scatter(if (!is.null(rv$user_dri_pre_scores)) rv$user_dri_pre_scores$pairwise else NULL, "Uploaded DRI — Pre", if (!is.null(rv$user_dri_pre_scores)) rv$user_dri_pre_scores$group_dri else NA_real_))
  plot_dl("user_dri_post",      "user_dri_post.png",      function() make_dri_scatter(if (!is.null(rv$user_dri_post_scores)) rv$user_dri_post_scores$pairwise else NULL, "Uploaded DRI — Post", if (!is.null(rv$user_dri_post_scores)) rv$user_dri_post_scores$group_dri else NA_real_))
  plot_dl("user_dri_change",    "user_dri_change.png",    function() make_dri_change_plot(if (!is.null(rv$user_dri_pre_scores)) rv$user_dri_pre_scores$individual else NULL, if (!is.null(rv$user_dri_post_scores)) rv$user_dri_post_scores$individual else NULL))
  plot_dl("opinion",            "opinion_change.png",     function() { df <- rv$personas; if (is.null(df)) ggplot() + theme_void() else { if (!"final_score" %in% names(df)) df$final_score <- df$initial_score; make_opinion_change(df) }})
  plot_dl("polar",              "polarization.png",       function() { df <- rv$personas; if (is.null(df)) ggplot() + theme_void() else { if (!"final_score" %in% names(df)) df$final_score <- df$initial_score; make_polarization(df) }})
  plot_dl("sentiment",          "sentiment.png",          function() make_sentiment(get_turns()))
  plot_dl("position_drift",     "position_drift.png",     function() make_position_drift(get_turns(), rv$personas, rv$model_baseline))
  plot_dl("drift_summary",      "drift_summary.png",      function() make_drift_summary(get_turns(), rv$personas))
  plot_dl("central_tendency",   "central_tendency.png",   function() { bl <- if (!is.null(rv$central_tendency)) rv$central_tendency$baseline[1] else NA_real_; make_central_tendency_plot(rv$personas, bl) })
  plot_dl("quality_radar",      "quality_radar.png",      function() { req(rv$quality); make_quality_radar(rv$quality) })
  plot_dl("aqua_speaker",       "aqua_speaker.png",       function() { req(rv$aqua); make_aqua_speaker_plot(rv$aqua) })
  plot_dl("aqua_phase",         "aqua_phase.png",         function() { req(rv$aqua); make_aqua_phase_plot(rv$aqua) })
  plot_dl("aqua_dimensions",    "aqua_dimensions.png",    function() { req(rv$aqua); make_aqua_dimension_plot(rv$aqua) })
  plot_dl("wordcount",          "wordcount.png",          function() make_wordcount_bar(get_turns()))
  plot_dl("equity",             "participation_equity.png", function() make_participation_equity(get_turns()))
  plot_dl("crossref",           "cross_references.png",   function() make_cross_reference_heatmap(get_turns()))
  
  save_plot_pdf <- function(plot_obj, file, width = 10, height = 7) {
    grDevices::pdf(file, width = width, height = height)
    on.exit(grDevices::dev.off(), add = TRUE)
    print(plot_obj)
  }
  
  output$dl_plots_pdf <- downloadHandler(
    filename = function() "deliberation_plots.pdf",
    content = function(file) {
      grDevices::pdf(file, width = 10, height = 7)
      on.exit(grDevices::dev.off(), add = TRUE)
      if (!is.null(rv$personas)) {
        p <- rv$personas
        if (!"final_score" %in% names(p)) p$final_score <- p$initial_score
        print(make_pie(p, "gender", "Gender"))
        print(make_age_pie(p))
        print(make_pie(p, "education", "Education"))
        print(make_pie(p, "settlement", "Settlement"))
        print(make_ideology_pie(p))
        print(make_ideology_alignment_plot(input, p))
        print(make_opinion_change(p))
        print(make_polarization(p))
      }
      print(make_sentiment(get_turns()))
      print(make_position_drift(get_turns(), rv$personas, rv$model_baseline))
      print(make_drift_summary(get_turns(), rv$personas))
      if (!is.null(rv$quality)) print(make_quality_radar(rv$quality))
      if (!is.null(rv$aqua)) {
        print(make_aqua_speaker_plot(rv$aqua))
        print(make_aqua_phase_plot(rv$aqua))
        print(make_aqua_dimension_plot(rv$aqua))
      }
    }
  )
  
  make_aqua_rmd <- function() {
    if (is.null(rv$aqua)) stop("No AQuA analysis available yet.")
    ov <- rv$aqua$overview
    dim_txt <- paste(apply(rv$aqua$dimensions, 1, function(r) {
      paste0("- **", r[["dimension"]], "**: prevalence = ", round(as.numeric(r[["mean_presence"]]), 3), ", contribution = ", round(as.numeric(r[["contribution"]]), 3))
    }), collapse = "\n")
    top_comments <- head(rv$aqua$comments[order(rv$aqua$comments$aqua_score, decreasing = TRUE), intersect(c("speaker","phase","aqua_score","text"), names(rv$aqua$comments)), drop = FALSE], 10)
    top_txt <- paste(apply(top_comments, 1, function(r) {
      paste0("- **", r[["speaker"]], "** (", r[["phase"]], ", ", round(as.numeric(r[["aqua_score"]]), 2), "): ", r[["text"]])
    }), collapse = "\n")
    paste0(
      "---\ntitle: \"AQuA Analysis Report\"\noutput: pdf_document\n---\n\n",
      "## Overview\n\n",
      "- Mean AQuA score: **", round(ov$mean_aqua[1], 2), "**\n",
      "- Median AQuA score: **", round(ov$median_aqua[1], 2), "**\n",
      "- Comments scored: **", ov$n_comments[1], "**\n\n",
      "## Interpretation\n\n",
      "This report uses the LLM-based AQuA implementation. Each comment was scored on the 20 deliberative quality dimensions of Behrendt et al. (2024) by prompting the simulation LLM with the exact binary definitions from the paper (temperature=0). Scores are combined with the published Table 1 weights and normalised to 0-5 using the paper formula.\n\n",
      "## Dimension contributions\n\n", dim_txt, "\n\n",
      "## Highest-scoring comments\n\n", top_txt, "\n"
    )
  }
  
  render_aqua_report_pdf <- function(file) {
    if (is.null(rv$aqua)) stop("No AQuA analysis available yet.")
    grDevices::pdf(file, width = 10, height = 7)
    on.exit(grDevices::dev.off(), add = TRUE)
    grid::grid.newpage()
    ov <- rv$aqua$overview
    title_gp <- grid::gpar(fontsize = 20, fontface = "bold")
    body_gp <- grid::gpar(fontsize = 11)
    grid::grid.text("AQuA Analysis Report", x = 0.5, y = 0.94, gp = title_gp)
    summary_txt <- paste0(
      "Mean AQuA score: ", round(ov$mean_aqua[1], 2), "\n",
      "Median AQuA score: ", round(ov$median_aqua[1], 2), "\n",
      "Comments scored: ", ov$n_comments[1], "\n\n",
      "This PDF summarizes the LLM-based AQuA scoring (Behrendt et al. 2024). Each comment scored on 20 binary dimensions via LLM at temperature=0, combined with exact Table 1 weights, normalised to 0-5."
    )
    grid::grid.text(summary_txt, x = 0.08, y = 0.78, just = c("left", "top"), gp = body_gp)
    top_dims <- head(rv$aqua$dimensions[order(rv$aqua$dimensions$contribution, decreasing = TRUE), ], 8)
    dim_txt <- paste(apply(top_dims, 1, function(r) paste0(r[["dimension"]], ": ", round(as.numeric(r[["contribution"]]), 3))), collapse = "\n")
    grid::grid.text(paste0("Top dimensions\n", dim_txt), x = 0.08, y = 0.48, just = c("left", "top"), gp = body_gp)
    print(make_aqua_speaker_plot(rv$aqua))
    print(make_aqua_phase_plot(rv$aqua))
    print(make_aqua_dimension_plot(rv$aqua))
  }
  
  output$dl_aqua_report_pdf <- downloadHandler(
    filename = function() "aqua_analysis_report.pdf",
    content = function(f) {
      render_aqua_report_pdf(f)
    }
  )
  
  output$dl_aqua_csv <- downloadHandler(
    filename = function() "aqua_comment_scores.csv",
    content = function(f) {
      write.csv(rv$aqua$comments, f, row.names = FALSE, na = "")
    }
  )
  
  output$dl_survey_pdf <- downloadHandler(
    filename = function() "deliberation_survey_report.pdf",
    content = function(f) {
      render_rmd_to_pdf(make_survey_rmd(), f)
    }
  )
  
  output$dl_survey_csv <- downloadHandler(
    filename = function() "deliberation_survey_data.csv",
    content = function(f) {
      pn <- if (!is.null(rv$personas)) rv$personas$name else NULL
      pre_df  <- if (!is.null(rv$pre_survey))  survey_to_df(rv$pre_survey, "Pre", pn)  else data.frame()
      post_df <- if (!is.null(rv$post_survey)) survey_to_df(rv$post_survey, "Post", pn) else data.frame()
      out <- bind_rows(pre_df, post_df)
      write.csv(out, f, row.names = FALSE)
    }
  )
}
shinyApp(ui = ui, server = server)
