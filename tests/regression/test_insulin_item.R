#!/usr/bin/env Rscript
# =============================================================================
# tests/regression/test_insulin_item.R
#
# PURPOSE
# -------
# Proves the Iannuzzi insulin_dep item does what its definition says, on hand-built patients where the right
# answer is known, through BOTH implementations:
#   cohort path  - cohort 9100007 (insulin outside an inpatient stay) AND cohort 9100021 (diabetes, any time
#                  before index) combined by cohort_map.csv and_cohort_id
#   domain path  - query_insulin_covariate_counts() (direct CDM query, the oracle)
#
# DEFINITION (2026-11-05): "who with diabetes is currently on insulin" = an OUTPATIENT insulin exposure in the 365
# days before index (day -365 through -1) AND a diabetes diagnosis at any time before index. Outpatient = no
# inpatient visit (9201, 262) spans the exposure date, start and end dates inclusive.
#
# WHY A FIXTURE: the synthetic dataset has no insulin older than 365 days and none during an inpatient stay, so
# test_cohort_vs_domain_covariates.R agrees on it without exercising the window edge or the inpatient exclusion.
# This test builds a tiny CDM (schema omop_synth.insulin_fx) with 16 patients covering every edge, so a wrong
# window, a wrong overlap rule, or a wrong diabetes requirement fails here.
#
# USAGE   Rscript tests/regression/test_insulin_item.R     (from the study folder; needs the dev SQL Server)
# =============================================================================

Sys.setenv("_JAVA_OPTIONS" = "-Xmx2g")
Sys.setenv(DATABASECONNECTOR_JAR_FOLDER = "/workspace/synthea-omop-template/drivers/jdbc-runtime")
connectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = "sql server", server = Sys.getenv("MSSQL_HOST", unset = "mssql_dev"),
  user = Sys.getenv("MSSQL_USER", unset = "sa"), password = Sys.getenv("MSSQL_SA_PASSWORD"),
  extraSettings = "database=omop_synth;trustServerCertificate=true;socketTimeout=0;queryTimeout=0")

source("config.R"); source("R/risk_score_pipeline.R")
FX <- "insulin_fx"; SRC <- "pad_amp_nhd_val_cdm_test"; COHORT_TABLE <- "ins_fx"; IDX <- as.Date("2024-01-01")
conn <- DatabaseConnector::connect(connectionDetails); on.exit(DatabaseConnector::disconnect(conn), add = TRUE)
run <- function(sql) DatabaseConnector::executeSql(conn, sql, progressBar = FALSE, reportOverallTime = FALSE)
q   <- function(sql) { r <- DatabaseConnector::querySql(conn, sql); names(r) <- tolower(names(r)); r }

# ---- fixture CDM -----------------------------------------------------------------------------------------------
run(sprintf("IF SCHEMA_ID('%s') IS NOT NULL BEGIN
  DECLARE @sql NVARCHAR(MAX) = '';
  SELECT @sql += 'DROP VIEW [%s].[' + name + '];' FROM sys.views WHERE schema_id = SCHEMA_ID('%s');
  SELECT @sql += 'DROP TABLE [%s].[' + name + '];' FROM sys.tables WHERE schema_id = SCHEMA_ID('%s');
  EXEC sp_executesql @sql; END ELSE EXEC('CREATE SCHEMA %s');", FX, FX, FX, FX, FX, FX))
for (t in c("person", "observation_period", "visit_occurrence", "drug_exposure", "condition_occurrence"))
  run(sprintf("SELECT TOP 0 * INTO %s.%s FROM %s.%s", FX, t, SRC, t))
for (v in c("concept", "concept_ancestor", "concept_relationship"))
  run(sprintf("CREATE VIEW %s.%s AS SELECT * FROM omop_vocab.%s", FX, v, v))

# one insulin concept (a standard Drug concept under ATC 21600713) and type 2 diabetes (201826, under 201820)
INSULIN <- q("SELECT TOP 1 c.concept_id FROM omop_vocab.concept_ancestor a JOIN omop_vocab.concept c ON c.concept_id = a.descendant_concept_id
              WHERE a.ancestor_concept_id = 21600713 AND c.standard_concept = 'S' AND c.domain_id = 'Drug' ORDER BY c.concept_id")$concept_id
DM_T2 <- 201826L

d <- function(offset) format(IDX + offset)      # date at index + offset days
# id, label, insulin offset (NA = none), visit spec, diabetes offset (NA = none), expected positive
S <- list(
  list(1,  "outpatient insulin 100 d before, diabetes earlier",           -100, list(c(9202, -100, -100)), -2000, TRUE),
  list(2,  "insulin 400 d before (outside 365 d)",                        -400, list(),                    -2000, FALSE),
  list(3,  "insulin during an inpatient stay (mid-stay)",                 -100, list(c(9201, -103, -98)),  -2000, FALSE),
  list(4,  "inpatient stay ended the day BEFORE the insulin",             -100, list(c(9201, -112, -101)), -2000, TRUE),
  list(5,  "outpatient insulin, NO diabetes diagnosis",                   -100, list(),                    NA,    FALSE),
  list(6,  "outpatient insulin, diabetes 8 years before",                 -100, list(),                    -3000, TRUE),
  list(7,  "outpatient insulin, diabetes first coded AFTER index",        -100, list(),                    60,    FALSE),
  list(8,  "inpatient stay STARTS on the insulin date",                   -100, list(c(9201, -100, -93)),  -2000, FALSE),
  list(9,  "insulin with no visit link at all",                           -100, list(),                    -2000, TRUE),
  list(10, "insulin on the LAST day of an inpatient stay",                -100, list(c(9201, -105, -100)), -2000, FALSE),
  list(11, "ER-to-inpatient visit (262) spans the insulin date",          -100, list(c(262, -102, -98)),   -2000, FALSE),
  list(12, "ED-only visit (9203) on the insulin date",                    -100, list(c(9203, -100, -100)), -2000, TRUE),
  list(13, "insulin on day -1 (window edge, inclusive)",                  -1,   list(),                    -2000, TRUE),
  list(14, "insulin on the index date (day 0, outside window)",           0,    list(),                    -2000, FALSE),
  list(15, "insulin on day -365 (window edge, inclusive)",                -365, list(),                    -2000, TRUE),
  list(16, "insulin on day -366 (outside window)",                        -366, list(),                    -2000, FALSE)
)
vid <- 0L; drug_id <- 0L; cond_id <- 0L; rows <- list()
for (s in S) {
  pid <- s[[1]]
  run(sprintf("INSERT INTO %s.person (person_id, gender_concept_id, year_of_birth, race_concept_id, ethnicity_concept_id) VALUES (%d, 8507, 1950, 8527, 38003564)", FX, pid))
  run(sprintf("INSERT INTO %s.observation_period (observation_period_id, person_id, observation_period_start_date, observation_period_end_date, period_type_concept_id) VALUES (%d, %d, '2000-01-01', '2030-12-31', 44814724)", FX, pid, pid))
  link <- "NULL"
  for (v in s[[4]]) {
    vid <- vid + 1L
    run(sprintf("INSERT INTO %s.visit_occurrence (visit_occurrence_id, person_id, visit_concept_id, visit_start_date, visit_end_date, visit_type_concept_id) VALUES (%d, %d, %d, '%s', '%s', 44818517)", FX, vid, pid, as.integer(v[1]), d(v[2]), d(v[3])))
    if (as.integer(v[1]) == 9202 || length(s[[4]]) == 1L) link <- as.character(vid)   # link the drug to its (only) visit
  }
  drug_id <- drug_id + 1L
  run(sprintf("INSERT INTO %s.drug_exposure (drug_exposure_id, person_id, drug_concept_id, drug_exposure_start_date, drug_exposure_end_date, drug_type_concept_id, visit_occurrence_id) VALUES (%d, %d, %d, '%s', '%s', 38000177, %s)", FX, drug_id, pid, INSULIN, d(s[[3]]), d(s[[3]]), link))
  if (!is.na(s[[5]])) { cond_id <- cond_id + 1L
    run(sprintf("INSERT INTO %s.condition_occurrence (condition_occurrence_id, person_id, condition_concept_id, condition_start_date, condition_type_concept_id) VALUES (%d, %d, %d, '%s', 32020)", FX, cond_id, pid, DM_T2, d(s[[5]]))) }
}

# ---- cohorts: the target (index = 2024-01-01 for everyone) and the two that make up the item ------------------
cds <- CohortGenerator::getCohortDefinitionSet(settingsFileName = "inst/Cohorts.csv", jsonFolder = "inst/cohorts",
                                               sqlFolder = "inst/sql/sql_server", packageName = NULL)
cds <- cds[cds$cohortId %in% c(9100007, 9100021), ]
work <- paste0("omop_synth.", FX); nm <- CohortGenerator::getCohortTableNames(cohortTable = COHORT_TABLE)
CohortGenerator::createCohortTables(connectionDetails = connectionDetails, cohortDatabaseSchema = work, cohortTableNames = nm)
for (s in S) run(sprintf("INSERT INTO %s.%s (cohort_definition_id, subject_id, cohort_start_date, cohort_end_date) VALUES (9100011, %d, '%s', '%s')", work, COHORT_TABLE, as.integer(s[[1]]), format(IDX), format(IDX + 9)))
CohortGenerator::generateCohortSet(connectionDetails = connectionDetails, cdmDatabaseSchema = work, cohortDatabaseSchema = work,
                                   cohortTableNames = nm, cohortDefinitionSet = cds, incremental = FALSE)

# ---- the two paths ----------------------------------------------------------------------------------------------
config <- get_validation_config()
config$cdm_schema <- FX; config$results_schema <- FX; config$cohort_table <- COHORT_TABLE
config$score_id <- "iannuzzi"; config$use_cohort_covariates <- TRUE
config$covariate_definitions_file <- "covariates/covariates.csv"; config$covariate_concepts_file <- "covariates/covariate_concepts.csv"
specs <- read_score_specs(config)
cov  <- specs$covariates[specs$covariates$covariate_id == "insulin_dep", , drop = FALSE]
cons <- specs$concepts[specs$concepts$covariate_id == "insulin_dep", , drop = FALSE]
cov <- as.list(cov[1, ])

positives <- function(df) { names(df) <- tolower(names(df)); sort(unique(as.integer(df$subject_id[df$event_count > 0]))) }
mappings <- lookup_cohort_map(config, "insulin_dep")
via_cohort <- positives(or_combine_counts(lapply(mappings, function(m) query_cohort_covariate_counts(conn, config, cov, m))))
via_domain <- positives(query_insulin_covariate_counts(conn, config, cov, cons))
expected   <- sort(vapply(Filter(function(s) isTRUE(s[[6]]), S), function(s) as.integer(s[[1]]), integer(1)))

cat(sprintf("  %-4s %-60s %-9s %-7s %-7s\n", "id", "scenario", "expected", "cohort", "direct"))
failures <- character(0)
for (s in S) {
  e <- isTRUE(s[[6]]); c1 <- s[[1]] %in% via_cohort; c2 <- s[[1]] %in% via_domain
  bad <- (c1 != e) || (c2 != e)
  cat(sprintf("  %-4d %-60s %-9s %-7s %-7s %s\n", s[[1]], s[[2]], e, c1, c2, if (bad) "*** WRONG ***" else ""))
  if (bad) failures <- c(failures, sprintf("patient %d (%s): expected %s, cohort %s, direct %s", s[[1]], s[[2]], e, c1, c2))
}
cat(sprintf("\n  expected positives: [%s]\n  cohort path:        [%s]\n  direct query:       [%s]\n",
            paste(expected, collapse = ","), paste(via_cohort, collapse = ","), paste(via_domain, collapse = ",")))
if (length(failures)) { cat("\nINSULIN ITEM TEST FAILED\n"); for (f in failures) cat("  - ", f, "\n", sep = ""); quit(status = 1) }
cat("\nINSULIN ITEM TEST PASSED: both paths give the expected answer for all 16 patients.\n")
