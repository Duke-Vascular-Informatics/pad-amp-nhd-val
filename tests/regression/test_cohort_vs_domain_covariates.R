#!/usr/bin/env Rscript
# =============================================================================
# tests/regression/test_cohort_vs_domain_covariates.R
#
# Scores each published instrument TWICE against the same target cohort — once
# resolving covariates from the Strategus cohort table, once by querying the CDM
# domain tables directly — and reports the per-covariate difference.
#
# WHAT THIS ASSERTS
# -----------------
# Every covariate cohort in this repo is Limit=All with EndStrategy StartDate+0
# (cohorts 9100002-9100028), so a lookback window binds identically in both
# paths and the two must agree PERSON FOR PERSON. Until 2026-10-05 this was a
# characterisation test: four covariates (dm, htn, chf, cad) were resolved by
# REUSED [DVI] VA-FI cohorts (1797949-1797952) that are PrimaryCriteriaLimit =
# "First", so their windows could not bind and they were EXPECTED to differ. Those
# four were replaced by Limit=All cohorts 9100021-9100024, EXPECTED_DIFF is now
# empty, and ANY delta is a failure. A loose tolerance would hide exactly the
# class of bug this file exists to catch.
#
# Includes the one multi-arm item: mFI-5 copd = COPD (9100002, 365 d) OR
# pneumonia (9100025, 30 d), which the domain path expresses with per-row window
# overrides in covariate_concepts_mfi5.csv.
#
# A third assertion is unconditional: each authored cohort's concept set must
# still equal the score CSV's concepts, so a future ATLAS re-author cannot drift
# the two definitions apart silently.
#
# USAGE
#   Rscript tests/regression/test_cohort_vs_domain_covariates.R
#
# The test GENERATES THE COHORTS ITSELF (see below) rather than assuming a
# populated cohort table, so it is self-contained and cannot be misled by stale
# data. That is not a convenience: an earlier version read whatever happened to
# be in the cohort table, and when a covariate cohort definition was edited it
# compared the NEW domain-query result against the OLD cohort rows and reported
# a spurious failure. Regenerating costs a few seconds and removes the entire
# class of problem.
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
config$results_schema <- Sys.getenv("OMOP_RESULTS_SCHEMA_OVERRIDE", unset = "pad_amp_nhd_val_results")
config$cohort_table   <- "pad_amp_nhd_val"

outRoot <- file.path(getwd(), "output", "regression")
dir.create(outRoot, recursive = TRUE, showWarnings = FALSE)

# Covariates expected to differ, keyed "<score_id>:<covariate_id>". Empty since
# 2026-10-05, when the Limit=First VA-FI cohorts were replaced; see the header.
EXPECTED_DIFF <- character(0)

# ---------------------------------------------------------------------------
# Regenerate every cohort from the definitions currently on disk, so both passes
# below are compared against the same, current definitions. See the note in the
# header for why this is not optional.
#
# The NHD outcome is regenerated from its hand-authored SQL afterwards, because
# CohortGenerator would otherwise render its placeholder JSON (all inpatient
# visits) — the same reason StrategusCodeToRun.R calls generate_nhd_cohort().
# ---------------------------------------------------------------------------
message("\n########## PASS 0: regenerate cohorts from current definitions ##########")
local({
  omopDb  <- Sys.getenv("OMOP_DATABASE", unset = "omop_synth")
  cdmTwo  <- paste0(omopDb, ".", Sys.getenv("OMOP_CDM_SCHEMA_OVERRIDE",
                                            unset = "pad_amp_nhd_val_cdm_test"))
  workTwo <- paste0(omopDb, ".", config$results_schema)

  cds <- CohortGenerator::getCohortDefinitionSet(
    settingsFileName = "inst/Cohorts.csv", jsonFolder = "inst/cohorts",
    sqlFolder = "inst/sql/sql_server", packageName = NULL)
  nm <- CohortGenerator::getCohortTableNames(cohortTable = config$cohort_table)
  CohortGenerator::createCohortTables(
    connectionDetails = connectionDetails,
    cohortDatabaseSchema = workTwo, cohortTableNames = nm)
  CohortGenerator::generateCohortSet(
    connectionDetails = connectionDetails,
    cdmDatabaseSchema = cdmTwo, cohortDatabaseSchema = workTwo,
    cohortTableNames = nm, cohortDefinitionSet = cds, incremental = FALSE)

  source("R/generate_nhd_cohort.R")
  conn <- DatabaseConnector::connect(connectionDetails)
  on.exit(DatabaseConnector::disconnect(conn), add = TRUE)
  generate_nhd_cohort(connection = conn, cdmDatabaseSchema = cdmTwo,
                      cohortDatabaseSchema = workTwo,
                      cohortTable = config$cohort_table)

  # Applied here too, same reason as generate_nhd_cohort() above and in the
  # same order both real pipelines use it: StrategusCodeToRun.R and
  # duke-prcc-deploy's run_analysis.R both restrict to the study period
  # before excluding facility admissions, so PASS 1/PASS 2 below must run
  # against that same (post-restriction) population -- otherwise this test
  # would silently keep comparing the domain-query oracle against a cohort
  # that still includes out-of-period patients while every real run uses
  # the restricted one.
  source("R/restrict_to_study_period.R")
  restrict_to_study_period(connection = conn, cohortDatabaseSchema = workTwo,
                           cohortTable = config$cohort_table,
                           studyStartDate = config$study_start_date,
                           studyEndDate = config$study_end_date)

  # Applied here too, same reason as generate_nhd_cohort() above: both
  # StrategusCodeToRun.R and duke-prcc-deploy's run_analysis.R call this
  # after generating 9100011, so PASS 1/PASS 2 below must run against the
  # same (post-exclusion) population both real pipelines actually use --
  # otherwise this test would silently keep comparing the domain-query
  # oracle against an unfiltered cohort while every real run uses the
  # filtered one.
  source("R/exclude_facility_admissions.R")
  exclude_facility_admissions(connection = conn, cdmDatabaseSchema = cdmTwo,
                              cohortDatabaseSchema = workTwo,
                              cohortTable = config$cohort_table)
})

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
               else if (expected) "EXPECTED"
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
# Concept ids of a cohort's concept sets, split by circe's isExcluded flag so an
# exclusion in the cohort must be matched by an exclusion in the CSV (and vice versa).
extractConcepts <- function(path, excluded = FALSE, sets = NULL) {
  j <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  # sets: optional vector of concept-set ids to read (cohort 9100007 also carries a visit concept set that is not
  # part of the insulin concepts)
  if (!is.null(sets)) j$ConceptSets <- Filter(function(cs) cs$id %in% sets, j$ConceptSets)
  ids <- integer(0)  # not c(): an empty result must be integer(0), not NULL, or identical() against an empty CSV side is FALSE
  walk <- function(x) {
    if (is.list(x)) {
      if (!is.null(x$concept$CONCEPT_ID) && identical(isTRUE(x$isExcluded), excluded)) {
        ids <<- c(ids, as.integer(x$concept$CONCEPT_ID))
      }
      for (el in x) walk(el)
    }
  }
  walk(j$ConceptSets)
  sort(unique(ids))
}
# One entry per (cohort, score-CSV covariate) it must match. window = "default"
# compares the covariate's rows WITHOUT a per-row window override; "override"
# compares the rows that carry one (mFI-5's pneumonia arm). Files without those
# columns are treated as all-default.
DRIFT <- list(
  list(cid = "9100002", csv = "covariates/covariate_concepts_vqifs.csv", cov = "copd",             window = "default"),
  list(cid = "9100002", csv = "covariates/covariate_concepts_mfi5.csv",  cov = "copd",             window = "default"),
  list(cid = "9100003", csv = "covariates/covariate_concepts_vqifs.csv", cov = "pvd",              window = "default"),
  list(cid = "9100004", csv = "covariates/covariate_concepts.csv",       cov = "tissue_loss",      window = "default"),
  list(cid = "9100007", csv = "covariates/covariate_concepts.csv",       cov = "insulin_dep",      window = "default", sets = 0L),
  list(cid = "9100021", csv = "covariates/covariate_concepts_mfi5.csv",  cov = "dm",               window = "default"),
  list(cid = "9100021", csv = "covariates/covariate_concepts_vqifs.csv", cov = "dm",               window = "default"),
  # mFI-5 htn since 2026-11-06: its non-diagnosis rows are the antihypertensive drugs (cohort 9100032); its
  # diagnosis_required row is checked against cohort 9100022 below.
  list(cid = "9100032", csv = "covariates/covariate_concepts_mfi5.csv",  cov = "htn",              window = "default", sets = 0L),
  list(cid = "9100022", csv = "covariates/covariate_concepts_vqifs.csv", cov = "htn",              window = "default"),
  list(cid = "9100023", csv = "covariates/covariate_concepts_mfi5.csv",  cov = "chf",              window = "default"),
  list(cid = "9100023", csv = "covariates/covariate_concepts_vqifs.csv", cov = "chf",              window = "default"),
  list(cid = "9100024", csv = "covariates/covariate_concepts_vqifs.csv", cov = "cad",              window = "default"),
  list(cid = "9100025", csv = "covariates/covariate_concepts_mfi5.csv",  cov = "copd",             window = "override"),
  list(cid = "9100026", csv = "covariates/covariate_concepts_mfi5.csv",  cov = "fs_dep",           window = "default"),
  list(cid = "9100027", csv = "covariates/covariate_concepts.csv",       cov = "ambu_deficit",     window = "default"),
  list(cid = "9100028", csv = "covariates/covariate_concepts_vqifs.csv", cov = "nonambulatory",    window = "default")
)
for (spec in DRIFT) {
  csv  <- read.csv(spec$csv, comment.char = "#", stringsAsFactors = FALSE)
  rows <- csv[csv$covariate_id == spec$cov, , drop = FALSE]
  if ("lookback_start_day" %in% names(rows)) {
    has_override <- !is.na(suppressWarnings(as.integer(rows$lookback_start_day)))
    rows <- rows[if (spec$window == "override") has_override else !has_override, , drop = FALSE]
  }
  is_excl_row <- !is.na(rows$concept_role) & tolower(trimws(rows$concept_role)) == "exclude"
  # 'diagnosis_required' rows are the diagnosis half of a drug-AND-diagnosis item (insulin_dep: diabetes; mFI-5 htn:
  # hypertension), not drug concepts; they are checked against cohorts 9100021 / 9100022 below.
  is_dx_row   <- !is.na(rows$concept_role) & tolower(trimws(rows$concept_role)) == "diagnosis_required"
  rows        <- rows[!is_dx_row, , drop = FALSE]
  is_excl_row <- is_excl_row[!is_dx_row]
  want     <- sort(unique(as.integer(rows$concept_id[!is_excl_row])))
  want_exc <- sort(unique(as.integer(rows$concept_id[is_excl_row])))
  cj       <- file.path("inst", "cohorts", paste0(spec$cid, ".json"))
  got      <- extractConcepts(cj, sets = spec$sets)
  got_exc  <- extractConcepts(cj, excluded = TRUE, sets = spec$sets)
  ok   <- identical(want, got) && identical(want_exc, got_exc)
  cat(sprintf("  %-9s %-14s %-8s csv=[%s]%s cohort=[%s]%s  %s\n", spec$cid, spec$cov, spec$window,
              paste(want, collapse = ","),
              if (length(want_exc)) sprintf(" excl=[%s]", paste(want_exc, collapse = ",")) else "",
              paste(got, collapse = ","),
              if (length(got_exc)) sprintf(" excl=[%s]", paste(got_exc, collapse = ",")) else "",
              if (ok) "match" else "*** DRIFT ***"))
  if (!ok) failures <- c(failures, sprintf("concept drift in cohort %s (%s, %s)", spec$cid, spec$cov, spec$csv))
}

# ONE DIABETES DEFINITION (2026-11-05): the diabetes requirement of the Iannuzzi insulin item (diagnosis_required rows)
# and both score dm items must equal cohort 9100021, which also drives Table 1.
local({
  dm_cohort <- extractConcepts(file.path("inst", "cohorts", "9100021.json"))
  ins <- read.csv("covariates/covariate_concepts.csv", comment.char = "#", stringsAsFactors = FALSE)
  ins_dx <- sort(unique(as.integer(ins$concept_id[ins$covariate_id == "insulin_dep" & !is.na(ins$concept_role) & ins$concept_role == "diagnosis_required"])))
  ok <- identical(ins_dx, dm_cohort)
  cat(sprintf("  insulin_dep diagnosis_required=[%s] vs cohort 9100021=[%s]  %s\n", paste(ins_dx, collapse = ","), paste(dm_cohort, collapse = ","), if (ok) "match" else "*** DRIFT ***"))
  if (!ok) failures <<- c(failures, "insulin_dep diagnosis requirement differs from the diabetes cohort 9100021")
})

# HYPERTENSION REQUIRING MEDICATION (2026-11-06): the diagnosis half of the mFI-5 htn item must equal cohort 9100022,
# the same hypertension definition the sVQI-FS htn item uses.
local({
  htn_cohort <- extractConcepts(file.path("inst", "cohorts", "9100022.json"))
  m5 <- read.csv("covariates/covariate_concepts_mfi5.csv", comment.char = "#", stringsAsFactors = FALSE)
  htn_dx <- sort(unique(as.integer(m5$concept_id[m5$covariate_id == "htn" & !is.na(m5$concept_role) & m5$concept_role == "diagnosis_required"])))
  ok <- identical(htn_dx, htn_cohort)
  cat(sprintf("  mfi5 htn diagnosis_required=[%s] vs cohort 9100022=[%s]  %s\n", paste(htn_dx, collapse = ","), paste(htn_cohort, collapse = ","), if (ok) "match" else "*** DRIFT ***"))
  if (!ok) failures <<- c(failures, "mFI-5 htn diagnosis requirement differs from the hypertension cohort 9100022")
})

# ---------------------------------------------------------------------------
# PASS 3: Table 1 phenotype rows read from cohorts must equal an independent direct count from the CDM
#
# R/extract_report_inputs.R builds the Table 1 indication rows (PAD, diabetes, lower-extremity wound) and the
# amputation-level rows (above-knee, below-knee, other) from ATLAS cohorts (.table1_indication_sql() and
# .table1_amp_level_sql()). Here the same numbers are recomputed straight from the cohort JSON concept sets
# against the CDM (descendants expanded, excluded concepts removed), with no cohort table involved, and the two
# must match exactly. This is the Table 1 analogue of the score-component comparison above.
# ---------------------------------------------------------------------------
message("\n########## PASS 3: Table 1 phenotypes, cohort-based vs direct CDM ##########")
source("R/extract_report_inputs.R")
local({
  omopDb  <- Sys.getenv("OMOP_DATABASE", unset = "omop_synth")
  cdmTwo  <- paste0(omopDb, ".", Sys.getenv("OMOP_CDM_SCHEMA_OVERRIDE", unset = "pad_amp_nhd_val_cdm_test"))
  workTwo <- paste0(omopDb, ".", config$results_schema)
  conn <- DatabaseConnector::connect(connectionDetails)
  on.exit(DatabaseConnector::disconnect(conn), add = TRUE)
  q <- function(sql) { r <- DatabaseConnector::querySql(conn, SqlRender::translate(sql, "sql server")); names(r) <- tolower(names(r)); r }

  # concept ids of a cohort's single concept set after expansion and exclusion
  ids_of <- function(cid) {
    j <- jsonlite::fromJSON(file.path("inst", "cohorts", paste0(cid, ".json")), simplifyVector = FALSE)
    items <- j$ConceptSets[[1]]$expression$items
    pick <- function(excl) Filter(function(i) isTRUE(i$isExcluded) == excl, items)
    expand <- function(its) {
      if (!length(its)) return(integer(0))
      desc <- vapply(its, function(i) isTRUE(i$includeDescendants), logical(1))
      ids  <- vapply(its, function(i) as.integer(i$concept$CONCEPT_ID), integer(1))
      out <- ids[!desc]
      if (any(desc)) out <- c(out, q(sprintf("SELECT DISTINCT descendant_concept_id AS id FROM %s.concept_ancestor WHERE ancestor_concept_id IN (%s)",
                                             cdmTwo, paste(ids[desc], collapse = ",")))$id)
      unique(out)
    }
    setdiff(expand(pick(FALSE)), expand(pick(TRUE)))
  }
  in_list <- function(v) paste(v, collapse = ",")
  tgt <- sprintf("(SELECT subject_id, cohort_start_date, ISNULL(cohort_end_date, cohort_start_date) AS cohort_end_date FROM %s.%s WHERE cohort_definition_id = %d)",
                 workTwo, config$cohort_table, config$target_cohort_id)

  direct_ind <- function(cid) q(sprintf(
    "SELECT COUNT(DISTINCT t.subject_id) AS n FROM %s t INNER JOIN %s.condition_occurrence co ON co.person_id = t.subject_id
       AND co.condition_start_date <= t.cohort_start_date AND co.condition_concept_id IN (%s)", tgt, cdmTwo, in_list(ids_of(cid))))$n
  direct_proc <- function(cid) q(sprintf(
    "SELECT DISTINCT t.subject_id FROM %s t INNER JOIN %s.procedure_occurrence po ON po.person_id = t.subject_id
       AND po.procedure_date BETWEEN t.cohort_start_date AND t.cohort_end_date AND po.procedure_concept_id IN (%s)", tgt, cdmTwo, in_list(ids_of(cid))))$subject_id

  ind <- q(.table1_indication_sql(workTwo, config$cohort_table, config$target_cohort_id))
  lvl <- q(.table1_amp_level_sql(workTwo, config$cohort_table, config$target_cohort_id))
  n_target <- q(sprintf("SELECT COUNT(*) AS n FROM %s.%s WHERE cohort_definition_id = %d", workTwo, config$cohort_table, config$target_cohort_id))$n

  aka <- direct_proc(.TABLE1_COHORTS[["aka"]]); bka <- direct_proc(.TABLE1_COHORTS[["bka"]])
  want <- c("PAD" = direct_ind(.TABLE1_COHORTS[["pad"]]),
            "Diabetes mellitus" = direct_ind(.TABLE1_COHORTS[["dm"]]),
            "LE wound / gangrene" = direct_ind(.TABLE1_COHORTS[["wound"]]),
            "Above-knee amputation (AKA)" = length(aka),
            "Below-knee amputation (BKA)" = length(setdiff(bka, aka)),
            "Other amputation" = n_target - length(union(aka, bka)))
  got <- stats::setNames(c(ind$n, lvl$n), c(ind$category, lvl$category))
  cat(sprintf("  %-30s %8s %8s  %s\n", "Table 1 row", "cohort", "direct", "verdict"))
  for (k in names(want)) {
    ok <- identical(as.numeric(got[[k]]), as.numeric(want[[k]]))
    cat(sprintf("  %-30s %8s %8s  %s\n", k, got[[k]], want[[k]], if (ok) "match" else "*** DIFFERS ***"))
    if (!ok) failures <<- c(failures, sprintf("Table 1 row '%s': cohort-based %s vs direct %s", k, got[[k]], want[[k]]))
  }
  tot <- sum(got[c("Above-knee amputation (AKA)", "Below-knee amputation (BKA)", "Other amputation")])
  if (tot != n_target) failures <<- c(failures, sprintf("amputation levels sum to %d, target cohort has %d", tot, n_target))
  cat(sprintf("  amputation levels sum to %d of %d target patients\n", tot, n_target))
})

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
cat(sprintf("\n%d covariates match exactly; %d differ as expected.\n",
            nMatch, nExpected))
cat("Per-covariate deltas written to ", outCsv, "\n", sep = "")

if (length(failures) > 0) {
  cat("\n*** REGRESSION TEST FAILED ***\n")
  for (f in failures) cat("  - ", f, "\n", sep = "")
  quit(status = 1)
}
cat("\nREGRESSION TEST PASSED — every semantically-identical covariate agrees exactly.\n")
