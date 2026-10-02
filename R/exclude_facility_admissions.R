# =============================================================================
# R/exclude_facility_admissions.R
#
# Excludes patients from the target cohort (9100011) whose index-visit
# admission source indicates transfer from another hospital or a skilled
# nursing facility, per the study team's request to restrict the analysis
# to patients admitted from home. Runs as a post-execute step, deleting
# rows from the results-schema cohort table -- NOT a circe InclusionRule,
# because circe has no admitted_from_concept_id/admitted_from_source_value
# attribute on VisitOccurrence (confirmed by class inspection, same
# limitation already documented for discharged_to_concept_id in
# https://github.com/Duke-Vascular-Informatics/strategus-study-template/blob/main/docs/CIRCE_ESCAPE_HATCH.md). CohortGenerator's
# native "cohort subset" feature does not help either -- its three operator
# types (demographic, limit, cohort-in-cohort) do not reach a visit-level
# admission-source predicate; building a helper cohort to subset against
# would hit the identical circe wall.
#
# WHY THIS DOES NOT NEED THE 9100001-STYLE ESCAPE-HATCH MACHINERY
# -----------------------------------------------------------------------
# Cohort 9100001 (the NHD outcome) is a full REPLACEMENT: its circe JSON is
# a deliberate placeholder ("every inpatient visit") because circe cannot
# express its own defining criterion at all, so Strategus would otherwise
# generate the WRONG cohort outright -- hence the placeholder-JSON +
# sentinel-guard + HAND_AUTHORED-registration + behavioural-assertion
# apparatus in CreateStrategusAnalysisSpecification.R / R/generate_nhd_cohort.R
# / scripts/render_cohort_sql.R.
#
# This is different in kind: cohort 9100011's own circe JSON is CORRECT and
# stays untouched -- Strategus generates the right population for it today,
# exactly as it always has. This function only deletes a few rows from the
# ALREADY-CORRECTLY-GENERATED cohort table afterward -- a narrowing, not a
# replacement. None of the placeholder-JSON guards apply because there is
# no placeholder here for CirceR to silently overwrite, and no risk of
# Strategus building "a cohort that is not the one you meant" for 9100011
# itself. (Investigated and confirmed 2026-09-15 before writing this file
# -- see that date's plan/PR for the full reasoning.)
#
# ADMISSION-SOURCE CODE MAPPING (verified against a real Duke export,
# supp_admission_source.csv, via a live vocabulary query -- [vocab query])
# -----------------------------------------------------------------------
#   IP -> 38004515 "Hospital" (Medicare Specialty)      -- EXCLUDED
#   IH -> 38004515 "Hospital" (Medicare Specialty)      -- EXCLUDED
#   SN -> 8863     "Skilled Nursing Facility" (CMS PoS)  -- EXCLUDED
#   HO -> unmapped (concept_id 0), but the dominant code (~58% of the
#         cohort) and matches this CDM's own convention for discharge (HO =
#         Home there too, per R/extract_report_inputs.R's
#         supp_discharge_destinations query)             -- KEPT (home)
#   AV -> 38004207 "Clinic/Center" (NUCC) -- an outpatient clinic referral
#         for a scheduled admission, not evidence of prior facility
#         residence                                       -- KEPT
#   OT, NI -> unmapped, negligible n (<1% combined), no evidence of
#         facility origin                                 -- KEPT
#
# KNOWN LIMITATION -- Strategus's own CharacterizationModule baseline
# characterization of cohort 9100011 (output/strategusOutput/
# CharacterizationModule/, see CreateStrategusAnalysisSpecification.R's
# CharacterizationModule config) runs INSIDE Strategus::execute(), before
# this function ever runs -- so it will describe the PRE-exclusion (larger)
# population. duke-prcc-deploy's export_results_for_review.R ships those
# files in the PRCC review package. The Word report and every custom-step
# analysis (this file's caller runs before all of them) are unaffected --
# they query the cohort table live, after this function has run -- but
# anyone reviewing the raw Strategus module output in a results-export
# package should not expect its counts to match the manuscript's. Milder
# than 9100001's placeholder scenario (a superset population, not
# meaningless data), so no Characterization sub-analysis is disabled here.
#
# CALL THIS after generate_nhd_cohort() (order between the two does not
# matter -- generate_nhd_cohort() computes NHD independently from raw
# visit_occurrence data with no dependency on 9100011 membership) and
# before run_integer_score_validation() (the first step that reads cohort
# 9100011's membership for anything analytic).
# =============================================================================

#' Delete facility-admitted patients from the target cohort
#'
#' @param connection            An OPEN DatabaseConnector connection.
#' @param cdmDatabaseSchema     CDM schema (the vocab-union overlay, same
#'                              schema generate_nhd_cohort() uses -- has
#'                              visit_occurrence).
#' @param cohortDatabaseSchema  Results schema holding the cohort table.
#' @param cohortTable           Cohort table name.
#' @param targetCohortId        Defaults to 9100011L.
#' @return Invisibly, a list(before, after, excluded) patient count.
exclude_facility_admissions <- function(connection, cdmDatabaseSchema,
                                        cohortDatabaseSchema, cohortTable,
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

  # Same join shape as the already-verified supp_admission_source diagnostic
  # query in R/extract_report_inputs.R: index visit = inpatient
  # (visit_concept_id = 9201) bracketing the target cohort's own
  # cohort_start_date. Reused, not reinvented.
  deleteSql <- SqlRender::render(
    "DELETE oc
     FROM @cohort_schema.@cohort_table oc
     INNER JOIN @cdm_schema.visit_occurrence vo
       ON vo.person_id        = oc.subject_id
      AND vo.visit_concept_id = 9201
      AND CAST(vo.visit_start_date AS DATE) <= CAST(oc.cohort_start_date AS DATE)
      AND CAST(COALESCE(vo.visit_end_date, vo.visit_start_date) AS DATE)
          >= CAST(oc.cohort_start_date AS DATE)
     WHERE oc.cohort_definition_id = @target_id
       AND vo.admitted_from_source_value IN ('IP', 'IH', 'SN')",
    cohort_schema = cohortDatabaseSchema,
    cohort_table  = cohortTable,
    cdm_schema    = cdmDatabaseSchema,
    target_id     = targetCohortId
  )
  DatabaseConnector::executeSql(
    connection, SqlRender::translate(deleteSql, targetDialect = "sql server")
  )

  after <- DatabaseConnector::querySql(
    connection, SqlRender::translate(countSql, targetDialect = "sql server")
  )$n[1]

  excluded <- before - after
  message(sprintf(
    "[exclude_facility_admissions] Cohort %d: %d -> %d patients (%d excluded, IP/IH/SN admission).",
    targetCohortId, before, after, excluded
  ))
  # Sanity check, not a hard stop: this determines a publication cohort's
  # population, so an unexpectedly large exclusion fraction (real Duke data
  # showed ~31%) should be visible, not silently accepted, if a future
  # ETL/vocabulary change shifts how these codes map.
  if (before > 0 && excluded / before > 0.5) {
    warning(sprintf(
      "[exclude_facility_admissions] Excluded %.0f%% of the cohort -- unexpectedly high; verify the admission-source join before trusting this run.",
      100 * excluded / before
    ))
  }

  invisible(list(before = before, after = after, excluded = excluded))
}
