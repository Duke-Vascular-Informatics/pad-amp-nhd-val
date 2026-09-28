# =============================================================================
# R/restrict_to_study_period.R
#
# Restricts the target cohort (9100011) to patients whose index event
# (cohort_start_date) falls within the study's configured date range --
# config$study_start_date / config$study_end_date, set in study_params.yaml.
# Runs as a post-execute step, deleting rows from the results-schema cohort
# table -- NOT a circe criterion, because circe's cohort JSON has no
# primary-event date-range field. CensorWindow (the one circe construct that
# takes literal dates) only bounds cohort END dates via end-strategy
# calculations, not entry events, and this cohort's CensorWindow is empty in
# any case (confirmed by inspecting inst/cohorts/9100011.json).
#
# WHY THIS EXISTS -- A REGRESSION FROM THE STRATEGUS PORT
# -----------------------------------------------------------------------
# study_start_date/study_end_date have been configured since this study's
# original synthea-omop-template incarnation. That template's R/cohorts.R
# applied them as a literal
#   cohort_start_date >= study_start_date AND cohort_start_date <= study_end_date
# filter directly on the materialized cohort table, immediately after
# CohortGenerator populated it. When this study moved to the Strategus
# architecture (pad-amp-nhd-prog, later frozen as pad-amp-nhd-val), that SQL
# step was never carried forward -- CohortGenerator now applies no date bound
# at all, and the two config fields were silently reduced to report-narrative
# text only (see extract_report_inputs.R's report_config$study_end_date,
# which only feeds the Methods section's prose).
#
# Confirmed as a LIVE problem, not theoretical: a 2026-09-16 PRCC run's
# agg_nhd_by_year.csv had a genuine "2026" row (24 patients, most with an
# outcome) even though study_end_date is "2025-12-31" -- Duke's live CDM
# keeps accruing amputation encounters past any date fixed at
# protocol-writing time, and nothing was stopping them from entering a
# cohort meant to be frozen for publication at a fixed historical cutoff.
# The lower bound (study_start_date, "2017-01-01") is not currently a live
# problem -- no pre-2017 rows have been observed in real data -- but is
# enforced symmetrically anyway, since nothing guarantees that stays true.
#
# WHY THIS DOES NOT NEED THE 9100001-STYLE ESCAPE-HATCH MACHINERY
# -----------------------------------------------------------------------
# Same reasoning as R/exclude_facility_admissions.R's identical header
# section: cohort 9100011's own circe JSON is CORRECT and stays untouched;
# this only narrows an already-correctly-generated cohort table afterward,
# which is not the class of problem (a circe definition that cannot express
# its own criterion, forcing a placeholder JSON) that machinery guards
# against.
#
# ORDER: run this AFTER generate_nhd_cohort() (independent -- that function
# computes NHD from raw visit_occurrence data with no dependency on 9100011
# membership) and BEFORE exclude_facility_admissions(), so the CONSORT
# funnel's stage order (see R/extract_report_inputs.R's
# .extract_consort_flow()) matches the pipeline's actual execution order:
# temporal study-period scope is a more fundamental entry criterion than the
# facility-admission narrowing, so it is presented as the earlier stage.
# The two filters are independent predicates on the same materialized table,
# so their execution order does not change the final cohort -- only which
# stage's "excluded n" absorbs a patient who happens to match both.
# =============================================================================

#' Delete patients outside the study's configured date range from the target cohort
#'
#' @param connection            An OPEN DatabaseConnector connection.
#' @param cohortDatabaseSchema  Results schema holding the cohort table.
#' @param cohortTable           Cohort table name.
#' @param studyStartDate        "YYYY-MM-DD" string (config$study_start_date),
#'                              inclusive lower bound on cohort_start_date.
#' @param studyEndDate          "YYYY-MM-DD" string (config$study_end_date),
#'                              inclusive upper bound on cohort_start_date.
#' @param targetCohortId        Defaults to 9100011L.
#' @return Invisibly, a list(before, after, excluded) patient count.
restrict_to_study_period <- function(connection, cohortDatabaseSchema, cohortTable,
                                     studyStartDate, studyEndDate,
                                     targetCohortId = 9100011L) {

  countSql <- SqlRender::render(
    "SELECT COUNT(DISTINCT subject_id) AS n
     FROM @cohort_schema.@cohort_table
     WHERE cohort_definition_id = @target_id",
    cohort_schema = cohortDatabaseSchema,
    cohort_table  = cohortTable,
    target_id     = targetCohortId
  )
  before <- DatabaseConnector::querySql(
    connection, SqlRender::translate(countSql, targetDialect = "sql server")
  )$n[1]

  deleteSql <- SqlRender::render(
    "DELETE FROM @cohort_schema.@cohort_table
     WHERE cohort_definition_id = @target_id
       AND (CAST(cohort_start_date AS DATE) < CAST('@study_start_date' AS DATE)
            OR CAST(cohort_start_date AS DATE) > CAST('@study_end_date' AS DATE))",
    cohort_schema     = cohortDatabaseSchema,
    cohort_table      = cohortTable,
    target_id         = targetCohortId,
    study_start_date  = studyStartDate,
    study_end_date    = studyEndDate
  )
  DatabaseConnector::executeSql(
    connection, SqlRender::translate(deleteSql, targetDialect = "sql server")
  )

  after <- DatabaseConnector::querySql(
    connection, SqlRender::translate(countSql, targetDialect = "sql server")
  )$n[1]

  excluded <- before - after
  message(sprintf(
    "[restrict_to_study_period] Cohort %d: %d -> %d patients (%d excluded, outside %s to %s).",
    targetCohortId, before, after, excluded, studyStartDate, studyEndDate
  ))
  # Sanity check, not a hard stop: an unexpectedly large exclusion fraction
  # here would mean the study period itself is misconfigured (or the cohort's
  # date range is wildly different from what was assumed), which should be
  # visible rather than silently accepted.
  if (before > 0 && excluded / before > 0.5) {
    warning(sprintf(
      "[restrict_to_study_period] Excluded %.0f%% of the cohort -- unexpectedly high; verify study_start_date/study_end_date before trusting this run.",
      100 * excluded / before
    ))
  }

  invisible(list(before = before, after = after, excluded = excluded))
}
