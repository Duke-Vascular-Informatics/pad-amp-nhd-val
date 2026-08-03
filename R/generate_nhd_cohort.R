# =============================================================================
# R/generate_nhd_cohort.R
#
# Generate the non-home discharge (NHD) outcome cohort from HAND-AUTHORED SQL,
# replacing whatever Strategus produced for cohort_definition_id 9100001.
#
# WHY THIS FILE HAS TO EXIST
# -------------------------
# Strategus does NOT execute inst/sql/sql_server/9100001.sql. Its analysis
# specification stores each cohort as a single `cohortDefinition` JSON string
# (verified: sharedResources[0]$cohortDefinitions rows carry only cohortId,
# cohortName, cohortDefinition — there is no sql field), and the
# CohortGeneratorModule re-renders SQL from that JSON with CirceR at execution
# time. The .sql file on disk is used by a direct
# CohortGenerator::generateCohortSet() call, which is how the smoke test read
# it, but never by Strategus.
#
# So for cohort 9100001 — whose JSON is a deliberate placeholder, because circe
# cannot express a discharge-disposition criterion — Strategus generates the
# PLACEHOLDER definition: every inpatient visit. Measured: 919 persons / 3190
# events, against 3245 inpatient visits in the CDM. Every one of the 195 target
# patients then has an "outcome", the outcome becomes constant, and AUROC is
# undefined. That is exactly how this was found: Table 4 came back all NA.
#
# The build-time sentinel guard in CreateStrategusAnalysisSpecification.R was
# guarding the wrong artefact — it checks a file Strategus never reads. It is
# retained (the file IS authoritative for the direct-generation path) but the
# real protection is the behavioural assertion at the bottom of this function,
# which compares the regenerated cohort against the inpatient-visit total and
# fails if the placeholder's signature reappears.
#
# CALLED BY: StrategusCodeToRun.R, after Strategus::execute() and before scoring.
# =============================================================================

# -----------------------------------------------------------------------------
# generate_nhd_cohort()
#
#   connection        an open DatabaseConnector connection
#   cdmDatabaseSchema schema the SQL reads clinical + vocabulary tables from.
#                     Must be the Strategus view-overlay, because the SQL needs
#                     `concept` and `concept_relationship` alongside
#                     `visit_occurrence`.
#   cohortDatabaseSchema / cohortTable  where the cohort rows are written
#   cohortId          9100001
#   sqlPath           inst/sql/sql_server/9100001.sql
#
# Returns a one-row data frame (persons, events) for the regenerated cohort.
# -----------------------------------------------------------------------------
generate_nhd_cohort <- function(connection,
                                cdmDatabaseSchema,
                                cohortDatabaseSchema,
                                cohortTable,
                                cohortId = 9100001L,
                                sqlPath  = file.path("inst", "sql", "sql_server", "9100001.sql")) {

  if (!file.exists(sqlPath)) {
    stop("Hand-authored NHD SQL not found: ", sqlPath)
  }
  sql <- paste(readLines(sqlPath, warn = FALSE), collapse = "\n")

  # Guard the SQL itself before running it. Cheap, and it catches the case where
  # someone has re-rendered the placeholder over this file.
  if (!grepl("UB04 Pt dis status", sql, fixed = TRUE)) {
    stop("*** ", sqlPath, " has lost its UB04 home-discharge CTE — it appears to ",
         "have been overwritten by a CirceR re-render of the placeholder JSON. ",
         "Restore it from git; do not regenerate it. ***")
  }

  countInpatient <- DatabaseConnector::querySql(connection, SqlRender::translate(
    SqlRender::render("SELECT COUNT(*) AS n FROM @cdm.visit_occurrence WHERE visit_concept_id = 9201",
                      cdm = cdmDatabaseSchema), "sql server"))[1, 1]

  message("[nhd] Regenerating cohort ", cohortId, " from hand-authored SQL ...")
  rendered <- SqlRender::render(
    sql,
    cdm_database_schema    = cdmDatabaseSchema,
    target_database_schema = cohortDatabaseSchema,
    target_cohort_table    = cohortTable,
    target_cohort_id       = cohortId
  )
  DatabaseConnector::executeSql(connection, SqlRender::translate(rendered, "sql server"),
                                progressBar = FALSE, reportOverallTime = FALSE)

  after <- DatabaseConnector::querySql(connection, SqlRender::translate(
    SqlRender::render(
      "SELECT COUNT(DISTINCT subject_id) AS persons, COUNT(*) AS events
       FROM @schema.@table WHERE cohort_definition_id = @id",
      schema = cohortDatabaseSchema, table = cohortTable, id = cohortId),
    "sql server"))
  names(after) <- tolower(names(after))

  message("[nhd] Cohort ", cohortId, ": ", after$persons, " persons / ",
          after$events, " events (", countInpatient, " inpatient visits in the CDM)")

  # --- BEHAVIOURAL ASSERTION -------------------------------------------------
  # The placeholder definition is "every inpatient visit". If the cohort's event
  # count approaches the inpatient-visit total, the placeholder logic is what
  # ran, regardless of what any file contains. A text check cannot catch this;
  # only the numbers can.
  if (after$events >= 0.5 * countInpatient) {
    stop("*** Cohort ", cohortId, " has ", after$events, " events against ",
         countInpatient, " inpatient visits in the CDM (>= 50%).\n",
         "    That is the signature of the PLACEHOLDER definition ('all inpatient\n",
         "    visits'), not of non-home discharge. The hand-authored SQL did not\n",
         "    take effect. Do not trust any downstream metric from this run. ***")
  }
  if (after$events == 0) {
    stop("*** Cohort ", cohortId, " is empty. The hand-authored NHD SQL ran but ",
         "matched nothing — check that visit_occurrence.discharged_to_concept_id ",
         "is populated in ", cdmDatabaseSchema, ". ***")
  }

  invisible(after)
}
