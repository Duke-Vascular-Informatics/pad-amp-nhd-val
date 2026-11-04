#!/usr/bin/env Rscript
# =============================================================================
# tests/regression/test_subgroup_ece.R
#
# PURPOSE
# -------
# Guards the 2026-11-04 harmonisation of ECE: the subgroup analysis
# (compute_subgroup_bias) must use the SAME ECE definition as the overall metric
# (compute_ece: ten quantile bins), so a subgroup that happens to be the whole
# test set reproduces the overall ECE exactly. Before the change, the subgroups
# used a separate equal-width-bin calc_ece(), which made subgroup and overall
# ECE (and the forest-plot reference line) non-comparable.
#
# No database needed: the demographic lookups are stubbed. Simulated predictions
# are deliberately miscalibrated and skewed so quantile and equal-width bins
# give visibly different ECE values (so the test would catch a regression).
#
# USAGE   Rscript tests/regression/test_subgroup_ece.R     (from the study folder)
# =============================================================================

suppressMessages(source("R/risk_score_pipeline.R"))

failures <- character()
fail <- function(msg) { failures <<- c(failures, msg); cat("  FAIL:", msg, "\n") }
ok   <- function(msg) cat("  ok  :", msg, "\n")

# ---- simulated test partition ------------------------------------------------
set.seed(1)
n <- 400L
p <- plogis(rnorm(n, -1.5, 1.2))                 # skewed predicted risks
y <- rbinom(n, 1, pmin(1, p * 1.4))              # truth is riskier than predicted
person_level <- data.frame(
  subject_id = seq_len(n),
  outcome = y,
  predicted_risk_recalibrated = p,
  split_set = "test"
)

# ---- stub the DB-backed demographic lookups ----------------------------------
# One subgroup level ("All") that contains every patient, plus a two-level
# variable so the loop has something to do besides the whole-cohort level.
fetch_subgroup_labels <- function(connection, config) {
  data.frame(subject_id = seq_len(n),
             sex = "All",
             race = ifelse(seq_len(n) %% 2 == 0, "A", "B"),
             stringsAsFactors = FALSE)
}
fetch_proc_type_labels <- function(connection, config) NULL

res <- suppressMessages(compute_subgroup_bias(person_level, connection = NULL, config = list(),
                                              B = 50L, min_events = 10L))
if (is.null(res)) stop("compute_subgroup_bias() returned NULL")
cat("result columns:", paste(names(res), collapse = ", "), "\n")

# ---- 1. whole-cohort subgroup equals the overall ECE -------------------------
overall <- compute_ece(y, p)
whole <- res[res$subgroup_var == "sex" & res$subgroup_level == "All", ]
ece_col <- intersect(c("ece", "ece_observed", "ECE"), names(whole))[1]
if (nrow(whole) != 1 || is.na(ece_col)) {
  fail("could not locate the whole-cohort subgroup row / ECE column")
} else if (abs(as.numeric(whole[[ece_col]]) - overall) <= 1e-4) {   # the output rounds ECE to 4 decimals
  ok(sprintf("whole-cohort subgroup ECE (%.5f) equals overall compute_ece()", overall))
} else {
  fail(sprintf("subgroup ECE %.5f != overall ECE %.5f", as.numeric(whole[[ece_col]]), overall))
}

# ---- 2. the retired equal-width definition would have disagreed ---------------
equal_width <- local({
  b <- cut(p, seq(0, 1, length.out = 11), include.lowest = TRUE, labels = FALSE)
  sum(vapply(unique(b), function(k) { i <- b == k; mean(i) * abs(mean(p[i]) - mean(y[i])) }, 0))
})
if (abs(equal_width - overall) > 1e-3) {
  ok(sprintf("test data separates the two definitions (equal-width %.5f vs quantile %.5f)", equal_width, overall))
} else {
  fail("test data does not distinguish quantile from equal-width ECE; strengthen it")
}

# ---- 3. the old function is gone ----------------------------------------------
if (!exists("calc_ece")) {
  ok("calc_ece() no longer defined")
} else {
  fail("calc_ece() still defined")
}

if (length(failures)) { cat("\nSUBGROUP ECE TEST FAILED (", length(failures), ")\n", sep = ""); quit(status = 1) }
cat("\nSUBGROUP ECE TEST PASSED\n")
