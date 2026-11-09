# =============================================================================
# R/cohort_demographics.R
#
# Subgroup label lookups used by compute_subgroup_bias() in
# R/risk_score_pipeline.R.
#
# PARTIAL PORT. pad-amp-nhd-val's version of this file also defined
# calculate_cohort_summary() and build_combined_feature_table(). Both are
# omitted here: neither has a call site anywhere in that repo either (only a
# mention in its R/README.md), so carrying them forward would import dead code
# into a new repo on day one. Recoverable from git history if ever needed.
#
# Both functions below read the Strategus-generated cohort table and the
# PHYSICAL CDM schema (config$cdm_schema), not the Strategus view-overlay.
# =============================================================================

# -----------------------------------------------------------------------------
# fetch_subgroup_labels()
#
# Queries the OMOP CDM person table to assign demographic subgroup labels to
# each subject in the target cohort.  Returns a data frame with one row per
# subject_id and five label columns used by compute_subgroup_bias():
#
#   sex        — "Female" / "Male"  (OMOP gender_concept_id: 8532 = Female)
#   race       — "White" / "Black" / "Other"
#                (OMOP race_concept_id: 8527 = White, 8516 = Black)
#   ethnicity  — "Hispanic" / "Non-Hispanic"
#                (OMOP ethnicity_concept_id: 38003563 = Hispanic or Latino)
#   age_group  — "<65" / "65-74" / ">=75"  (age at index date in years)
#
# Note: surgical indication subgroup is derived from the score component
# column score_indicationClaudication in person_level (already computed by
# the risk score pipeline), so it is NOT included here.
#
# Returns NULL (with a warning) if the SQL query fails or returns no rows.
# -----------------------------------------------------------------------------
fetch_subgroup_labels <- function(connection, config) {

  # ---------------------------------------------------------------------------
  # Query person demographics + age at index date for all target cohort members.
  # FLOOR(DATEDIFF / 365.25) replicates the age calculation used elsewhere in
  # the pipeline (consistent with SQL FLOOR(days/365.25) convention).
  # ---------------------------------------------------------------------------
  sql <- SqlRender::render(
    "SELECT
       c.subject_id,
       FLOOR(DATEDIFF(day, p.birth_datetime, c.cohort_start_date) / 365.25)
         AS age_at_index,
       p.gender_concept_id,
       p.race_concept_id,
       p.ethnicity_concept_id
     FROM @results_schema.@cohort_table c
     INNER JOIN @cdm_schema.person p
       ON c.subject_id = p.person_id
     WHERE c.cohort_definition_id = @target_id",
    results_schema = config$results_schema,
    cohort_table   = config$cohort_table,
    cdm_schema     = config$cdm_schema,
    target_id      = config$target_cohort_id
  )

  demog <- tryCatch(
    DatabaseConnector::querySql(
      connection,
      SqlRender::translate(sql, targetDialect = "sql server")
    ),
    error = function(e) {
      warning("[fetch_subgroup_labels] Demographics query failed: ", conditionMessage(e))
      NULL
    }
  )

  if (is.null(demog) || nrow(demog) == 0) {
    warning("[fetch_subgroup_labels] No rows returned — subgroup labels unavailable.")
    return(NULL)
  }

  # Normalise column names: DatabaseConnector may return UPPER or mixed case.
  names(demog) <- tolower(names(demog))

  # ---------------------------------------------------------------------------
  # Map concept IDs to human-readable subgroup labels.
  # ---------------------------------------------------------------------------

  # Sex: 8532 = FEMALE; all others treated as Male.
  demog$sex <- ifelse(
    as.integer(demog$gender_concept_id) == 8532L,
    "Female",
    "Male"
  )

  # Race: 8527 = White, 8516 = Black; all others → "Other".
  demog$race <- dplyr::case_when(
    as.integer(demog$race_concept_id) == 8527L ~ "White",
    as.integer(demog$race_concept_id) == 8516L ~ "Black",
    TRUE                                        ~ "Other"
  )

  # Ethnicity: 38003563 = Hispanic or Latino; all others → "Non-Hispanic".
  demog$ethnicity <- ifelse(
    as.integer(demog$ethnicity_concept_id) == 38003563L,
    "Hispanic",
    "Non-Hispanic"
  )

  # Age group: three clinically meaningful bands.
  age <- as.numeric(demog$age_at_index)
  demog$age_group <- dplyr::case_when(
    age <  65 ~ "<65",
    age <  75 ~ "65-74",
    !is.na(age) ~ ">=75",
    TRUE        ~ NA_character_
  )

  # Return only the columns needed downstream.
  demog[, c("subject_id", "sex", "race", "ethnicity", "age_group")]
}

# -----------------------------------------------------------------------------
# fetch_amp_level_labels()
#
# Assigns each subject in the target cohort an AMPUTATION LEVEL for the subgroup bias assessment, using the same
# definition as Table 1's amputation-level rows (R/extract_report_inputs.R .table1_amp_level_sql): an entry of
# cohort 9100030 (above-knee) or 9100031 (below-knee) on the index visit, hierarchical (above-knee first, then
# below-knee, everyone else "Other").
#
# REPLACES fetch_proc_type_labels() (removed 2026-11-09). That function came from the bypass-surgery template and
# labelled patients by BYPASS type (extra-anatomic, aortobifemoral, femoral-popliteal, ...), which is meaningless in
# an amputation cohort: it matched only the few patients with a concurrent bypass, so every level was suppressed
# for having fewer than 10 events and the amputation-level subgroup the Methods describe never appeared.
#
# Returns a data frame with columns subject_id, amp_level ("Above-knee" / "Below-knee" / "Other"), or NULL (with a
# warning) if the query fails.
# -----------------------------------------------------------------------------
fetch_amp_level_labels <- function(connection, config, aka_id = 9100030L, bka_id = 9100031L) {

  sql <- SqlRender::render(
    "WITH target AS (
       SELECT subject_id,
              cohort_start_date,
              ISNULL(cohort_end_date, cohort_start_date) AS cohort_end_date
       FROM @results_schema.@cohort_table
       WHERE cohort_definition_id = @target_id
     ),
     lvl AS (
       SELECT t.subject_id,
              MAX(CASE WHEN c.cohort_definition_id = @aka_id THEN 1 ELSE 0 END) AS aka,
              MAX(CASE WHEN c.cohort_definition_id = @bka_id THEN 1 ELSE 0 END) AS bka
       FROM target t
       LEFT JOIN @results_schema.@cohort_table c
         ON c.subject_id = t.subject_id
        AND c.cohort_definition_id IN (@aka_id, @bka_id)
        AND c.cohort_start_date BETWEEN t.cohort_start_date AND t.cohort_end_date
       GROUP BY t.subject_id
     )
     SELECT subject_id,
            CASE WHEN aka = 1 THEN 'Above-knee'
                 WHEN bka = 1 THEN 'Below-knee'
                 ELSE 'Other' END AS amp_level
     FROM lvl",
    results_schema = config$results_schema,
    cohort_table   = config$cohort_table,
    target_id      = config$target_cohort_id,
    aka_id         = aka_id,
    bka_id         = bka_id
  )

  result <- tryCatch(
    DatabaseConnector::querySql(
      connection,
      SqlRender::translate(sql, targetDialect = "sql server")
    ),
    error = function(e) {
      warning("[fetch_amp_level_labels] Query failed: ", conditionMessage(e))
      NULL
    }
  )

  if (is.null(result) || nrow(result) == 0) {
    warning("[fetch_amp_level_labels] No rows returned.")
    return(NULL)
  }

  names(result) <- tolower(names(result))
  result[, c("subject_id", "amp_level")]
}
