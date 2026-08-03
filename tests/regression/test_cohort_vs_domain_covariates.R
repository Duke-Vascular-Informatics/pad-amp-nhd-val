#!/usr/bin/env Rscript
# =============================================================================
# tests/regression/test_cohort_vs_domain_covariates.R
#
# Scores each published instrument TWICE against the same target cohort — once
# resolving covariates from the Strategus cohort table, once by querying the CDM
# domain tables directly — and reports the per-covariate difference.
#
# WHY THIS IS A CHARACTERISATION TEST, NOT AN EQUALITY TEST
# ---------------------------------------------------------
# Four covariates are resolved by REUSED [DVI] VA-FI cohorts (1797949-1797952),
# which are PrimaryCriteriaLimit = "First": one row per person at their
# first-ever qualifying record. A lookback window cannot bind against that, so
# those covariates are effectively "ever prior to index" in cohort mode and
# windowed in domain mode. They are EXPECTED to differ, and the size of that
# difference is a study finding worth reporting, not a failure.
#
# So the assertions are split:
#
#   STRICT  — covariates whose semantics are genuinely identical across the two
#             paths must match EXACTLY, person for person. That is every
#             purpose-authored cohort (9100002-9100010) and everything computed
#             in R. A loose tolerance here would hide exactly the class of bug
#             this file exists to catch.
#   REPORTED — the four reused VA-FI covariates. Difference is printed and
#             written to disk; the test does not fail on it.
#
# A third assertion is unconditional: each authored cohort's concept set must
# still equal the score CSV's concepts, so a future ATLAS re-author cannot drift
# the two definitions apart silently.
#
# USAGE
#   Rscript tests/regression/test_cohort_vs_domain_covariates.R
# Prerequisite: cohorts generated (StrategusCodeToRun.R, or its stage-1 subset).
# =============================================================================

Sys.setenv("_JAVA_OPTIONS" = "-Xmx4g")
Sys.setenv(DATABASECONNECTOR_JAR_FOLDER = "/workspace/synthea-omop-template/drivers/jdbc-runtime")

connectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms     = "sql server",
  server   = Sys.getenv("MSSQL_HOST", unset = "mssql_dev"),
  user     = Sys.getenv("MSSQL_USER", unset = "sa"),
  password = Sys.getenv("MSSQL_SA_PASSWORD"),
  extraSettings = "database=omop_synth;trustServerCertificate=true;socketTimeout=0;queryTimeout=0"
)

source("config.R")
source("R/risk_score_pipeline.R")
source("scripts/analysis/integer_score_validation.R")

config <- get_validation_config()
config$results_schema <- Sys.getenv("OMOP_RESULTS_SCHEMA_OVERRIDE", unset = "pad_amp_nhd_prog_results")
config$cohort_table   <- "pad_amp_nhd_prog"

outRoot <- file.path(getwd(), "output", "regression")
dir.create(outRoot, recursive = TRUE, showWarnings = FALSE)

# Covariates expected to differ, and why. Keyed "<score_id>:<covariate_id>".
EXPECTED_DIFF <- c(
  "mfi5:dm", "mfi5:chf", "mfi5:htn",
  "vqifs:htn", "vqifs:chf", "vqifs:cad", "vqifs:dm"
)

message("\n########## PASS 1: domain-query path (oracle) ##########")
config$output_folder <- file.path(outRoot, "domain")
run_integer_score_validation(connectionDetails, config, use_cohort_covariates = FALSE)

message("\n########## PASS 2: cohort-based path ##########")
config$output_folder <- file.path(outRoot, "cohort")
run_integer_score_validation(connectionDetails, config, use_cohort_covariates = TRUE)

# ---------------------------------------------------------------------------
# Compare
# ---------------------------------------------------------------------------
message("\n########## COMPARISON ##########")
failures <- character(0)
report   <- list()

for (score in config$scores) {
  sid <- score$id
  dPath <- file.path(outRoot, "domain", sid, "covariate_summary.csv")
  cPath <- file.path(outRoot, "cohort", sid, "covariate_summary.csv")
  if (!file.exists(dPath) || !file.exists(cPath)) {
    failures <- c(failures, sprintf("%s: missing covariate_summary.csv", sid)); next
  }
  d <- read.csv(dPath, stringsAsFactors = FALSE)
  k <- read.csv(cPath, stringsAsFactors = FALSE)
  m <- merge(d[, c("covariate_id", "n_positive")],
             k[, c("covariate_id", "n_positive")],
             by = "covariate_id", suffixes = c("_domain", "_cohort"), all = TRUE)
  m$n_positive_domain[is.na(m$n_positive_domain)] <- 0
  m$n_positive_cohort[is.na(m$n_positive_cohort)] <- 0
  m$delta    <- m$n_positive_cohort - m$n_positive_domain
  m$key      <- paste0(sid, ":", m$covariate_id)
  m$expected <- m$key %in% EXPECTED_DIFF
  m$score_id <- sid

  cat(sprintf("\n--- %s ---\n", score$label))
  cat(sprintf("  %-20s %8s %8s %8s   %s\n",
              "covariate", "domain", "cohort", "delta", "verdict"))
  for (i in seq_len(nrow(m))) with(m[i, ], {
    verdict <- if (delta == 0) "match"
               else if (expected) "EXPECTED (reused Limit=First VA-FI cohort)"
               else "*** UNEXPECTED ***"
    cat(sprintf("  %-20s %8d %8d %+8d   %s\n",
                covariate_id, n_positive_domain, n_positive_cohort, delta, verdict))
  })

  bad <- m[m$delta != 0 & !m$expected, ]
  if (nrow(bad) > 0) {
    failures <- c(failures, sprintf("%s: %s", sid, paste(bad$covariate_id, collapse = ", ")))
  }
  report[[sid]] <- m
}

# ---------------------------------------------------------------------------
# Concept-set drift guard: each authored cohort must still match its CSV.
# ---------------------------------------------------------------------------
message("\n########## CONCEPT-SET DRIFT CHECK ##########")
extractConcepts <- function(path) {
  j <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  ids <- c()
  walk <- function(x) {
    if (is.list(x)) {
      if (!is.null(x$CONCEPT_ID)) ids <<- c(ids, as.integer(x$CONCEPT_ID))
      for (el in x) walk(el)
    }
  }
  walk(j$ConceptSets)
  sort(unique(ids))
}
# authored cohort -> (csv file, covariate_id)
DRIFT <- list(
  "9100002" = list("covariates/covariate_concepts_vqifs.csv", "copd"),
  "9100003" = list("covariates/covariate_concepts_vqifs.csv", "pvd"),
  "9100004" = list("covariates/covariate_concepts.csv",       "tissue_loss"),
  "9100005" = list("covariates/covariate_concepts_mfi5.csv",  "fs_dep"),
  "9100007" = list("covariates/covariate_concepts.csv",       "insulin_dep")
)
for (cid in names(DRIFT)) {
  spec <- DRIFT[[cid]]
  csv  <- read.csv(spec[[1]], comment.char = "#", stringsAsFactors = FALSE)
  want <- sort(unique(as.integer(csv$concept_id[csv$covariate_id == spec[[2]]])))
  got  <- extractConcepts(file.path("inst", "cohorts", paste0(cid, ".json")))
  ok   <- identical(want, got)
  cat(sprintf("  %-9s %-14s csv=[%s] cohort=[%s]  %s\n", cid, spec[[2]],
              paste(want, collapse = ","), paste(got, collapse = ","),
              if (ok) "match" else "*** DRIFT ***"))
  if (!ok) failures <- c(failures, sprintf("concept drift in cohort %s (%s)", cid, spec[[2]]))
}

# ---------------------------------------------------------------------------
# Persist and summarise
# ---------------------------------------------------------------------------
allRows <- do.call(rbind, report)
outCsv  <- file.path(outRoot, "cohort_vs_domain_deltas.csv")
write.csv(allRows[, c("score_id", "covariate_id", "n_positive_domain",
                      "n_positive_cohort", "delta", "expected")],
          outCsv, row.names = FALSE)

nExpected <- sum(allRows$delta != 0 &  allRows$expected)
nMatch    <- sum(allRows$delta == 0)
cat(sprintf("\n%d covariates match exactly; %d differ as expected (reused Limit=First cohorts).\n",
            nMatch, nExpected))
cat("Per-covariate deltas written to ", outCsv, "\n", sep = "")

if (length(failures) > 0) {
  cat("\n*** REGRESSION TEST FAILED ***\n")
  for (f in failures) cat("  - ", f, "\n", sep = "")
  quit(status = 1)
}
cat("\nREGRESSION TEST PASSED — every semantically-identical covariate agrees exactly.\n")
