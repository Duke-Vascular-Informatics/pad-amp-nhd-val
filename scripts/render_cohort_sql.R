#!/usr/bin/env Rscript
# =============================================================================
# scripts/render_cohort_sql.R
#
# Render inst/cohorts/<id>.json -> inst/sql/sql_server/<id>.sql with CirceR, for
# every cohort in inst/Cohorts.csv EXCEPT those on the skip list below.
#
# WHY A SKIP LIST EXISTS
# ----------------------
# Cohort 9100001 (non-home discharge) is hand-authored SQL. Its JSON is a
# deliberately under-specified placeholder that exists only because
# CohortGenerator::getCohortDefinitionSet() requires a JSON per manifest row.
# Rendering that placeholder would overwrite the real NHD logic with "all
# inpatient visits" -- an enormous outcome rate that still produces
# plausible-looking metrics. This script refuses to touch it, and
# CreateStrategusAnalysisSpecification.R carries a second, independent sentinel
# guard in case someone renders it by other means.
#
# USAGE
#   Rscript scripts/render_cohort_sql.R
# =============================================================================

# Cohorts whose SQL is hand-authored and must never be regenerated from JSON.
HAND_AUTHORED <- c(9100001L)

manifest <- read.csv("inst/Cohorts.csv", stringsAsFactors = FALSE)
stopifnot(!any(duplicated(manifest$cohort_id)))

rendered <- 0L
skipped  <- character(0)

for (i in seq_len(nrow(manifest))) {
  cid <- as.integer(manifest$cohort_id[i])
  json_path <- file.path("inst", "cohorts", paste0(cid, ".json"))
  sql_path  <- file.path("inst", "sql", "sql_server", paste0(cid, ".sql"))

  if (cid %in% HAND_AUTHORED) {
    skipped <- c(skipped, sprintf("%d (hand-authored SQL)", cid))
    next
  }
  if (!file.exists(json_path)) {
    stop("Missing cohort JSON for id ", cid, ": ", json_path)
  }

  # Reused ATLAS cohorts ship with CirceR-rendered SQL already; re-rendering
  # them is harmless but pointless, and risks a CirceR-version diff showing up
  # as noise in the repo. Only render when the SQL is absent.
  if (file.exists(sql_path)) {
    skipped <- c(skipped, sprintf("%d (SQL already present)", cid))
    next
  }

  json <- paste(readLines(json_path, warn = FALSE), collapse = "\n")
  sql  <- CirceR::buildCohortQuery(
    CirceR::cohortExpressionFromJson(json),
    options = CirceR::createGenerateOptions(generateStats = FALSE)
  )
  writeLines(sql, sql_path)
  rendered <- rendered + 1L
  cat(sprintf("  rendered %s -> %s (%d lines)\n", json_path, sql_path,
              length(strsplit(sql, "\n")[[1]])))
}

cat("\nRendered:", rendered, "cohort(s).\n")
if (length(skipped)) cat("Skipped :", paste(skipped, collapse = ", "), "\n")
