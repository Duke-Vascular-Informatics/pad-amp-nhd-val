#!/usr/bin/env Rscript
# =============================================================================
# tests/regression/test_cohort_freshness.R
#
# Proves R/cohort_freshness.R does what it claims, against the synthetic CDM and
# the study's REAL cohort definitions (9100003 PAOD, small; 9100011 target):
#
#   1. A first generation passes the consistency check.
#   2. Simulating the post-steps (rows deleted from the target cohort) makes the
#      check FAIL -- this is the 2026-10-15 failure: a reused, already-narrowed copy.
#   3. prepare_cohort_generation() deletes ONLY the target's generation record, so
#      the next incremental run regenerates the target (rows restored) and REUSES
#      the untouched cohort (its generation timestamp does not move).
#   4. The check then passes, and reports the target as regenerated, the other reused.
#   5. A changed CDM/study fingerprint makes prepare_cohort_generation() clear every
#      generation record, so both cohorts regenerate.
#   6. .extract_consort_flow() refuses to write a funnel whose stages do not chain
#      (the same 880 -> 587 -> 587 shape the misattributed PRCC export had), and
#      writes one that does.
#
# Uses its own cohort table (cf_test) so it cannot disturb the other regression
# test's cohort table. USAGE:  Rscript tests/regression/test_cohort_freshness.R
# =============================================================================

Sys.setenv("_JAVA_OPTIONS" = "-Xmx4g")
Sys.setenv(DATABASECONNECTOR_JAR_FOLDER = "/workspace/synthea-omop-template/drivers/jdbc-runtime")

connectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = "sql server",
  server   = Sys.getenv("MSSQL_HOST", unset = "mssql_dev"),
  user     = Sys.getenv("MSSQL_USER", unset = "sa"),
  password = Sys.getenv("MSSQL_SA_PASSWORD"),
  extraSettings = "database=omop_synth;trustServerCertificate=true;socketTimeout=0;queryTimeout=0"
)

source("config.R")
source("R/risk_score_pipeline.R")      # .suppress_counts(), %||%
source("R/cohort_freshness.R")
source("R/extract_report_inputs.R")    # .extract_consort_flow()

config   <- get_validation_config()
omopDb   <- Sys.getenv("OMOP_DATABASE", unset = "omop_synth")
cdmTwo   <- paste0(omopDb, ".", Sys.getenv("OMOP_CDM_SCHEMA_OVERRIDE", unset = "pad_amp_nhd_val_cdm_test"))
workTwo  <- paste0(omopDb, ".", Sys.getenv("OMOP_RESULTS_SCHEMA_OVERRIDE", unset = "pad_amp_nhd_val_results"))
TARGET   <- as.integer(config$target_cohort_id)
OTHER    <- 9100003L
tbl      <- "cf_test"
nm       <- CohortGenerator::getCohortTableNames(cohortTable = tbl)
failures <- character(0)
check <- function(ok, label) {
  cat(sprintf("  [%s] %s\n", if (isTRUE(ok)) "PASS" else "FAIL", label))
  if (!isTRUE(ok)) failures <<- c(failures, label)
}

cds <- CohortGenerator::getCohortDefinitionSet(
  settingsFileName = "inst/Cohorts.csv", jsonFolder = "inst/cohorts",
  sqlFolder = "inst/sql/sql_server", packageName = NULL)
cds <- cds[cds$cohortId %in% c(TARGET, OTHER), ]
stopifnot(nrow(cds) == 2L)

conn <- DatabaseConnector::connect(connectionDetails)
on.exit({
  for (t in unlist(nm)) try(.cf_exec(conn, "IF OBJECT_ID('@sch.@tb','U') IS NOT NULL DROP TABLE @sch.@tb;", sch = workTwo, tb = t), silent = TRUE)
  try(.cf_exec(conn, "IF OBJECT_ID('@sch.@tb','U') IS NOT NULL DROP TABLE @sch.@tb;", sch = workTwo, tb = paste0(tbl, "_run_fingerprint")), silent = TRUE)
  try(DatabaseConnector::disconnect(conn), silent = TRUE)
}, add = TRUE)

generate <- function() {
  suppressMessages(CohortGenerator::generateCohortSet(
    connection = conn, cdmDatabaseSchema = cdmTwo, cohortDatabaseSchema = workTwo,
    cohortTableNames = nm, cohortDefinitionSet = cds, incremental = TRUE))
}
live <- function() {
  r <- DatabaseConnector::querySql(conn, SqlRender::translate(SqlRender::render(
    "SELECT cohort_definition_id, COUNT(DISTINCT subject_id) AS n FROM @sch.@tb GROUP BY cohort_definition_id",
    sch = workTwo, tb = tbl), "sql server"))
  setNames(r[[2]], r[[1]])
}
start_times <- function() {
  r <- DatabaseConnector::querySql(conn, SqlRender::translate(SqlRender::render(
    "SELECT cohort_definition_id, start_time FROM @sch.@tb", sch = workTwo, tb = nm$cohortChecksumTable), "sql server"))
  setNames(r[[2]], r[[1]])
}
assert_now <- function(attrition, runStart) {
  assert_cohort_generation_consistent(
    conn, workTwo, tbl, cdmTwo, strategusOutputDir = tempdir(), expectedCohortIds = c(TARGET, OTHER),
    runStart = runStart, studyStartDate = config$study_start_date, studyEndDate = config$study_end_date,
    attrition = attrition)
}

# ---- 1. first generation -------------------------------------------------------
cat("\n[1] first generation\n")
suppressMessages(CohortGenerator::createCohortTables(connectionDetails = connectionDetails,
  cohortDatabaseSchema = workTwo, cohortTableNames = nm))
t0 <- Sys.time()
generate()
n1 <- live()
cat("    cohort subject counts:", paste(names(n1), n1, sep = "=", collapse = ", "), "\n")
check(all(c(TARGET, OTHER) %in% as.integer(names(n1))) && n1[as.character(TARGET)] > 10,
      "both cohorts generated; target has enough patients to test with")
attr1 <- data.frame(cohort_definition_id = as.integer(names(n1)), expected = as.integer(n1))
ok1 <- tryCatch({ suppressMessages(assert_now(attr1, t0)); TRUE }, error = function(e) FALSE)
check(ok1, "consistency check passes on a fresh generation")

# ---- 2. simulate the post-steps editing the target ------------------------------
cat("\n[2] post-steps edit the target cohort in place\n")
.cf_exec(conn, "DELETE FROM @sch.@tb WHERE cohort_definition_id = @id AND subject_id IN
  (SELECT TOP 5 subject_id FROM @sch.@tb WHERE cohort_definition_id = @id ORDER BY subject_id);",
  sch = workTwo, tb = tbl, id = TARGET)
n2 <- live()
check(n2[as.character(TARGET)] < n1[as.character(TARGET)], "target now has fewer subjects than generated")
msg <- tryCatch({ suppressMessages(assert_now(attr1, t0)); "NO ERROR" }, error = function(e) conditionMessage(e))
check(grepl("does not match the cohorts just generated", msg) && grepl(as.character(TARGET), msg),
      "consistency check FAILS and names the edited cohort")

# ---- 3. prepare forces only the target ------------------------------------------
cat("\n[3] prepare_cohort_generation() + incremental re-run\n")
st_before <- start_times()
Sys.sleep(2)  # so a regenerated cohort's start_time is distinguishable
t1 <- Sys.time()
suppressMessages(prepare_cohort_generation(conn, workTwo, tbl, cdmTwo,
  alwaysRegenerateIds = TARGET, studyStartDate = config$study_start_date, studyEndDate = config$study_end_date))
rec <- start_times()
check(!(as.character(TARGET) %in% names(rec)) && (as.character(OTHER) %in% names(rec)),
      "only the target's generation record was removed")
generate()
st_after <- start_times()
check(st_after[as.character(TARGET)] > st_before[as.character(TARGET)], "target was regenerated (new start_time)")
check(isTRUE(all.equal(unname(st_after[as.character(OTHER)]), unname(st_before[as.character(OTHER)]))),
      "untouched cohort was REUSED (start_time unchanged)")
check(live()[as.character(TARGET)] == n1[as.character(TARGET)], "target rows restored to the generated count")

# ---- 4. check now passes and reports regenerated vs reused ----------------------
cat("\n[4] consistency check after regeneration\n")
out <- character(0)
ok4 <- tryCatch({ withCallingHandlers(assert_now(attr1, t1),
        message = function(m) { out <<- c(out, conditionMessage(m)); invokeRestart("muffleMessage") }); TRUE },
        error = function(e) FALSE)
check(ok4, "consistency check passes")
check(any(grepl(paste0("Regenerated this run \\(1\\): ", TARGET), out)) &&
      any(grepl(paste0("Reused unchanged +\\(1\\): ", OTHER), out)),
      "reports the target as regenerated and the other cohort as reused")
if (!any(grepl(paste0("Regenerated this run \\(1\\): ", TARGET), out))) {
  cat("    reported:\n"); cat(paste0("      ", out), sep = "\n")
  cat("    start_time (epoch s) per cohort:", paste(names(start_times()), format(start_times(), nsmall = 1), sep = "=", collapse = ", "),
      "| t1 =", format(as.numeric(t1), nsmall = 1), "\n")
}

# ---- 5. data drift clears everything --------------------------------------------
cat("\n[5] CDM/study fingerprint change\n")
.cf_write_fingerprint(conn, workTwo, paste0(tbl, "_run_fingerprint"), "cdm_release_date=1999-01-01|stale")
suppressMessages(prepare_cohort_generation(conn, workTwo, tbl, cdmTwo, alwaysRegenerateIds = TARGET,
  studyStartDate = config$study_start_date, studyEndDate = config$study_end_date))
check(length(start_times()) == 0L, "a changed fingerprint cleared every generation record")
st_b <- NULL; Sys.sleep(2); generate(); st_c <- start_times()
check(all(c(TARGET, OTHER) %in% as.integer(names(st_c))), "both cohorts regenerated after the drift reset")

# ---- 6. CONSORT chain check ------------------------------------------------------
cat("\n[6] .extract_consort_flow() chain check\n")
sd <- file.path(tempdir(), "strategus_out", "CohortGeneratorModule"); dir.create(sd, recursive = TRUE, showWarnings = FALSE)
write.csv(data.frame(database_id = "x", cohort_definition_id = TARGET, mode_id = 0L, cohort_entry = c(1,0,0,0,0),
                     rule_sequence = c(-1, 0, 1, 2, 3), person_count = c(1034, 1013, 960, 911, 880)),
          file.path(sd, "cg_cohort_attrition.csv"), row.names = FALSE)
write.csv(data.frame(database_id = "x", cohort_definition_id = TARGET, rule_sequence = 0:3,
                     name = c("a", "b", "c", "d")), file.path(sd, "cg_cohort_inclusion.csv"), row.names = FALSE)
cfg <- list(target_cohort_id = TARGET, study_start_date = "2017-01-01", study_end_date = "2025-12-31")
so <- file.path(tempdir(), "strategus_out")
bad_dir <- file.path(tempdir(), "ri_bad"); dir.create(bad_dir)
r_bad <- suppressMessages(.extract_consort_flow(cfg, so, list(before = 587, after = 587),
                                                list(before = 587, after = 587), bad_dir, 5L))
check(length(r_bad) == 0L && !file.exists(file.path(bad_dir, "agg_consort_flow.csv")),
      "refuses a funnel that starts the study-period step at 587 after generating 880")
good_dir <- file.path(tempdir(), "ri_good"); dir.create(good_dir)
r_good <- suppressMessages(.extract_consort_flow(cfg, so, list(before = 880, after = 849),
                                                 list(before = 849, after = 587), good_dir, 5L))
fl <- if (length(r_good)) read.csv(file.path(good_dir, "agg_consort_flow.csv")) else NULL
check(!is.null(fl) && identical(as.integer(fl$n), c(1034L, 1013L, 960L, 911L, 880L, 849L, 587L)),
      "writes the correct 880 -> 849 -> 587 funnel when the stages chain")

if (length(failures) > 0) {
  cat("\n*** COHORT-FRESHNESS TEST FAILED ***\n"); for (f in failures) cat("  - ", f, "\n", sep = ""); quit(status = 1)
}
cat("\nCOHORT-FRESHNESS TEST PASSED\n")
