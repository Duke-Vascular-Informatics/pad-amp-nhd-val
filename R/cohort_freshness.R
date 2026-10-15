# =============================================================================
# cohort_freshness.R — keep the cohort table honest across re-runs
#
# PURPOSE
#   Strategus/CohortGenerator runs in incremental mode: it records a checksum per
#   cohort definition in <cohortTable>_checksum and SKIPS any cohort whose
#   definition is unchanged. That is right for a cohort nobody touches, and wrong
#   for two situations this study has:
#
#   1. POST-GENERATION MUTATION. Cohort 9100011 (the target) is edited in place
#      after generation by restrict_to_study_period() and
#      exclude_facility_admissions(); 9100001 (the outcome) is overwritten by
#      generate_nhd_cohort(). A re-run sees an unchanged checksum, skips both, and
#      the post-steps then run AGAIN on the already-narrowed rows. Their counts
#      collapse to "0 excluded", the CONSORT funnel misattributes every exclusion
#      (observed on PRCC 2026-10-15: 31 outside-period + 262 facility exclusions
#      shown as 293 + 0), and nothing errors.
#   2. DATA DRIFT. The definition has not changed but the CDM has been refreshed
#      (or the study dates moved). The checksum cannot see that.
#
#   This file replaces the manual reset_incremental_state step for those cases:
#
#     prepare_cohort_generation()          BEFORE Strategus::execute()
#        - forces the mutated cohorts to regenerate every run (their checksum
#          rows are deleted; CohortGenerator then treats them as never generated)
#        - compares a CDM/study "fingerprint" with the one recorded after the last
#          run and, if it differs, forces EVERY cohort to regenerate
#        - cohorts whose DEFINITION changed need no help: CohortGenerator already
#          regenerates a cohort whose checksum differs
#
#     assert_cohort_generation_consistent() AFTER Strategus::execute(), BEFORE any
#     post-step
#        - proves the cohort table is a fresh generation by the current
#          definitions: each cohort's live subject count must equal the final-stage
#          count CohortGenerator recorded while generating it, and every cohort in
#          the specification must have been generated
#        - prints which cohorts were regenerated this run and which were reused
#        - records the fingerprint for the next run
#
#   reset_incremental_state still exists for a fully clean slate (e.g. changing the
#   physical CDM schema), but is no longer needed after a cohort edit.
#
# SCHEMA NOTE
#   Everything here is plain SQL through SqlRender/querySql/executeSql, never
#   DatabaseConnector::insertTable: Duke PRCC results schemas look like
#   [dhe\netid], and a backslash schema crashes insertTable.
# =============================================================================

# -----------------------------------------------------------------------------
# .cf_table_exists()
# TRUE when <schema>.<table> exists. OBJECT_ID accepts the three-part
# database.[bracketed schema].table form the PRCC schema needs.
# -----------------------------------------------------------------------------
.cf_table_exists <- function(connection, schema, table) {
  sql <- SqlRender::render(
    "SELECT CASE WHEN OBJECT_ID('@schema.@table', 'U') IS NULL THEN 0 ELSE 1 END AS e",
    schema = schema, table = table)
  isTRUE(DatabaseConnector::querySql(
    connection, SqlRender::translate(sql, targetDialect = "sql server"))[[1]][1] == 1)
}

.cf_exec <- function(connection, sql, ...) {
  DatabaseConnector::executeSql(
    connection, SqlRender::translate(SqlRender::render(sql, ...), targetDialect = "sql server"),
    progressBar = FALSE, reportOverallTime = FALSE)
}

# -----------------------------------------------------------------------------
# .cf_fingerprint()
# One string describing the data the cohort table was built from: the CDM's own
# release dates and vocabulary version (best effort -- NA when cdm_source is
# unreadable) plus the study window. Any change forces a full regeneration.
# -----------------------------------------------------------------------------
.cf_fingerprint <- function(connection, cdmDatabaseSchema, studyStartDate, studyEndDate) {
  src <- tryCatch(DatabaseConnector::querySql(connection, SqlRender::translate(SqlRender::render(
    "SELECT TOP 1 CAST(cdm_release_date AS VARCHAR(30)) AS a,
                  CAST(source_release_date AS VARCHAR(30)) AS b,
                  CAST(vocabulary_version AS VARCHAR(100)) AS c
     FROM @cdm.cdm_source", cdm = cdmDatabaseSchema), targetDialect = "sql server")),
    error = function(e) NULL)
  v <- if (is.null(src) || nrow(src) == 0L) c(NA, NA, NA) else as.character(unlist(src[1, 1:3]))
  paste0("cdm_release_date=", v[1], "|source_release_date=", v[2],
         "|vocabulary_version=", v[3],
         "|study=", as.character(studyStartDate), "..", as.character(studyEndDate))
}

.cf_read_fingerprint <- function(connection, schema, table) {
  if (!.cf_table_exists(connection, schema, table)) return(NA_character_)
  r <- DatabaseConnector::querySql(connection, SqlRender::translate(SqlRender::render(
    "SELECT TOP 1 fingerprint FROM @schema.@table", schema = schema, table = table),
    targetDialect = "sql server"))
  if (nrow(r) == 0L) NA_character_ else as.character(r[[1]][1])
}

.cf_write_fingerprint <- function(connection, schema, table, fingerprint) {
  if (!.cf_table_exists(connection, schema, table)) {
    .cf_exec(connection,
      "CREATE TABLE @schema.@table (fingerprint VARCHAR(1000) NOT NULL, recorded_at VARCHAR(30));",
      schema = schema, table = table)
  }
  .cf_exec(connection, "DELETE FROM @schema.@table;", schema = schema, table = table)
  .cf_exec(connection,
    "INSERT INTO @schema.@table (fingerprint, recorded_at) VALUES ('@fp', '@ts');",
    schema = schema, table = table,
    fp = gsub("'", "''", fingerprint), ts = format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
}

# -----------------------------------------------------------------------------
# prepare_cohort_generation()
#
# Call BEFORE Strategus::execute(), with an open connection.
#
# @param connection            open DatabaseConnector connection
# @param cohortDatabaseSchema  results schema holding the cohort table (two-part)
# @param cohortTable           base cohort table name
# @param cdmDatabaseSchema     CDM schema (for the cdm_source fingerprint)
# @param alwaysRegenerateIds   cohorts mutated after generation: the target and
#                              the outcome. Regenerated every run, cheaply
#                              (target ~5 min, outcome ~1 min on PRCC, versus
#                              ~1.2 h for the whole set).
# @param studyStartDate,studyEndDate  part of the fingerprint
# @return invisibly, list(regenerate_all, forced_ids, reason)
# Side effects: deletes rows from <cohortTable>_checksum. Never touches the cohort
# table itself -- CohortGenerator clears a cohort's rows when it regenerates it.
# -----------------------------------------------------------------------------
prepare_cohort_generation <- function(connection, cohortDatabaseSchema, cohortTable,
                                      cdmDatabaseSchema, alwaysRegenerateIds,
                                      studyStartDate, studyEndDate) {
  tn       <- CohortGenerator::getCohortTableNames(cohortTable = cohortTable)
  checksum <- tn$cohortChecksumTable
  fp_table <- paste0(cohortTable, "_run_fingerprint")

  if (!.cf_table_exists(connection, cohortDatabaseSchema, checksum)) {
    message("[cohort-freshness] No prior generation recorded (", checksum,
            " absent): every cohort will be generated.")
    return(invisible(list(regenerate_all = TRUE, forced_ids = integer(0), reason = "first run")))
  }

  # --- 1. data drift: CDM release / vocabulary / study window changed? -------
  now_fp  <- .cf_fingerprint(connection, cdmDatabaseSchema, studyStartDate, studyEndDate)
  last_fp <- .cf_read_fingerprint(connection, cohortDatabaseSchema, fp_table)
  regenerate_all <- FALSE; reason <- NA_character_
  if (is.na(last_fp)) {
    message("[cohort-freshness] No fingerprint recorded for the last run (first run with this ",
            "check). Data drift cannot be assessed; definition changes and the always-regenerate ",
            "cohorts are still handled.")
  } else if (!identical(last_fp, now_fp)) {
    regenerate_all <- TRUE
    reason <- paste0("CDM/study fingerprint changed.\n    was: ", last_fp, "\n    now: ", now_fp)
    message("[cohort-freshness] ", reason, "\n  -> clearing ALL generation records; every cohort regenerates.")
    .cf_exec(connection, "DELETE FROM @schema.@table;", schema = cohortDatabaseSchema, table = checksum)
  } else {
    message("[cohort-freshness] CDM/study fingerprint unchanged since the last run.")
  }

  # --- 2. mutated cohorts: always regenerate ---------------------------------
  if (!regenerate_all) {
    ids <- paste(as.integer(alwaysRegenerateIds), collapse = ",")
    .cf_exec(connection,
      "DELETE FROM @schema.@table WHERE cohort_definition_id IN (@ids);",
      schema = cohortDatabaseSchema, table = checksum, ids = ids)
    message("[cohort-freshness] Forcing regeneration of ", ids, ": these cohorts are modified ",
            "after generation (study-period / facility exclusions, hand-authored outcome), so a ",
            "reused copy would already carry those edits.")
  }
  message("[cohort-freshness] CohortGenerator will also regenerate any cohort whose definition ",
          "checksum changed; the rest are reused.")
  invisible(list(regenerate_all = regenerate_all, forced_ids = alwaysRegenerateIds, reason = reason))
}

# -----------------------------------------------------------------------------
# .cf_attrition_final_counts()
# Reads CohortGenerator's cg_cohort_attrition.csv and returns, per cohort, the
# person count at the LAST rule_sequence (mode_id 0) -- i.e. the size CohortGenerator
# computed for the cohort at generation time, after every inclusion rule.
# NULL when the file is absent.
# -----------------------------------------------------------------------------
.cf_attrition_final_counts <- function(strategusOutputDir) {
  f <- file.path(strategusOutputDir, "CohortGeneratorModule", "cg_cohort_attrition.csv")
  if (!file.exists(f)) return(NULL)
  a <- utils::read.csv(f, stringsAsFactors = FALSE)
  a <- a[a$mode_id == 0, , drop = FALSE]
  if (nrow(a) == 0L) return(NULL)
  do.call(rbind, lapply(split(a, a$cohort_definition_id), function(g) {
    g <- g[which.max(g$rule_sequence), ]
    data.frame(cohort_definition_id = g$cohort_definition_id[1], expected = g$person_count[1])
  }))
}

# -----------------------------------------------------------------------------
# assert_cohort_generation_consistent()
#
# Call right AFTER Strategus::execute() and BEFORE any post-step.
#
# Verifies the cohort table is a fresh, unmodified generation by the current
# definitions:
#   (a) every cohort in the specification has a generation record;
#   (b) for every cohort CohortGenerator wrote attrition for, the live distinct
#       subject count equals the final-stage count it recorded at generation. A
#       cohort that was reused after being narrowed (the 2026-10-15 failure) shows
#       up here as live < expected.
# Then prints which cohorts were regenerated this run vs reused, and records the
# fingerprint for the next run.
#
# @param connection            open DatabaseConnector connection
# @param cohortDatabaseSchema, cohortTable, cdmDatabaseSchema  as above
# @param strategusOutputDir    Strategus results folder (has CohortGeneratorModule/)
# @param expectedCohortIds     ids in the analysis specification
# @param runStart              Sys.time() captured BEFORE prepare_cohort_generation()
# @param studyStartDate,studyEndDate  for the fingerprint
# @param attrition             optional data.frame(cohort_definition_id, expected)
#                              overriding the CSV (used by the unit test)
# @return invisibly, the comparison data.frame; stops on any inconsistency.
# -----------------------------------------------------------------------------
assert_cohort_generation_consistent <- function(connection, cohortDatabaseSchema, cohortTable,
                                                cdmDatabaseSchema, strategusOutputDir,
                                                expectedCohortIds, runStart,
                                                studyStartDate, studyEndDate,
                                                attrition = NULL) {
  tn <- CohortGenerator::getCohortTableNames(cohortTable = cohortTable)
  problems <- character(0)

  # (a) generation records
  rec <- DatabaseConnector::querySql(connection, SqlRender::translate(SqlRender::render(
    "SELECT cohort_definition_id, start_time FROM @schema.@table",
    schema = cohortDatabaseSchema, table = tn$cohortChecksumTable), targetDialect = "sql server"))
  names(rec) <- tolower(names(rec))
  missing_rec <- setdiff(as.integer(expectedCohortIds), as.integer(rec$cohort_definition_id))
  if (length(missing_rec) > 0L)
    problems <- c(problems, paste0("no generation record for cohort(s): ", paste(missing_rec, collapse = ", ")))

  # (b) live count vs generation-time count
  if (is.null(attrition)) attrition <- .cf_attrition_final_counts(strategusOutputDir)
  cmp <- NULL
  if (is.null(attrition)) {
    message("[cohort-freshness] cg_cohort_attrition.csv not found under ", strategusOutputDir,
            " -- live-vs-generated count check skipped.")
  } else {
    live <- DatabaseConnector::querySql(connection, SqlRender::translate(SqlRender::render(
      "SELECT cohort_definition_id, COUNT(DISTINCT subject_id) AS live
       FROM @schema.@table GROUP BY cohort_definition_id",
      schema = cohortDatabaseSchema, table = cohortTable), targetDialect = "sql server"))
    names(live) <- tolower(names(live))
    cmp <- merge(attrition, live, by = "cohort_definition_id", all.x = TRUE)
    cmp$live[is.na(cmp$live)] <- 0L
    cmp$ok <- cmp$live == cmp$expected
    for (i in which(!cmp$ok)) {
      problems <- c(problems, sprintf(
        "cohort %s: %s subject(s) in the cohort table but CohortGenerator generated %s (a stale copy that was edited after generation, or an interrupted run)",
        cmp$cohort_definition_id[i], format(cmp$live[i], big.mark = ","),
        format(cmp$expected[i], big.mark = ",")))
    }
  }

  if (length(problems) > 0L) {
    stop("[cohort-freshness] The cohort table does not match the cohorts just generated:\n  - ",
         paste(problems, collapse = "\n  - "),
         "\n\nContinuing would score and report from a population that no current definition ",
         "produced. prepare_cohort_generation() should have prevented this; if it still fails, ",
         "run reset_incremental_state for a clean slate and re-run.", call. = FALSE)
  }

  # Which cohorts were regenerated this run? CohortGenerator 1.1.0 stores start_time as
  # epoch MILLISECONDS (~1.8e12); older versions stored seconds (~1.8e9). Normalise rather
  # than assume -- comparing ms against seconds marked every cohort "regenerated".
  start_s <- ifelse(rec$start_time > 1e11, rec$start_time / 1000, rec$start_time)
  regenerated <- as.integer(rec$cohort_definition_id[start_s >= as.numeric(runStart) - 1])
  reused      <- setdiff(as.integer(rec$cohort_definition_id), regenerated)
  message("[cohort-freshness] Regenerated this run (", length(regenerated), "): ",
          paste(sort(regenerated), collapse = ", "))
  message("[cohort-freshness] Reused unchanged     (", length(reused), "): ",
          if (length(reused)) paste(sort(reused), collapse = ", ") else "none")
  if (!is.null(cmp)) message("[cohort-freshness] Verified ", nrow(cmp),
    " cohort(s): live subject count equals the count CohortGenerator generated.")

  .cf_write_fingerprint(connection, cohortDatabaseSchema, paste0(cohortTable, "_run_fingerprint"),
    .cf_fingerprint(connection, cdmDatabaseSchema, studyStartDate, studyEndDate))
  invisible(cmp)
}
