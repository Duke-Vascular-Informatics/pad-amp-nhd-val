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
# fetch_proc_type_labels()
#
# Queries the OMOP CDM to assign a single procedure type label to each subject
# in the target cohort based on the qualifying procedure at the index visit.
# Priority order (highest to lowest): Extra-anatomic bypass, Aortobifemoral,
# Femoral-popliteal, Femorotibial, Femoral endarterectomy, Other.
#
# Returns a data frame with columns: subject_id, proc_type
# Returns NULL (with a warning) if the query fails.
# -----------------------------------------------------------------------------
fetch_proc_type_labels <- function(connection, config) {

  sql <- SqlRender::render(
    "WITH target AS (
       SELECT subject_id,
              cohort_start_date,
              ISNULL(cohort_end_date, cohort_start_date) AS cohort_end_date
       FROM @results_schema.@cohort_table
       WHERE cohort_definition_id = @target_id
     ),
     proc_hits AS (
       SELECT t.subject_id,
         MAX(CASE WHEN ca.ancestor_concept_id = 4050281 THEN 1 ELSE 0 END) AS is_extraanat,
         MAX(CASE WHEN ca.ancestor_concept_id = 4231680 THEN 1 ELSE 0 END) AS is_aortobif,
         MAX(CASE WHEN ca.ancestor_concept_id = 4012936 THEN 1 ELSE 0 END) AS is_fempop,
         MAX(CASE WHEN ca.ancestor_concept_id = 4166196 THEN 1 ELSE 0 END) AS is_femtib,
         MAX(CASE WHEN ca.ancestor_concept_id = 4040974 THEN 1 ELSE 0 END) AS is_endar
       FROM target t
       INNER JOIN @cdm_schema.procedure_occurrence po
         ON po.person_id = t.subject_id
        AND po.procedure_date BETWEEN t.cohort_start_date AND t.cohort_end_date
       INNER JOIN @cdm_schema.concept_ancestor ca
         ON ca.descendant_concept_id = po.procedure_concept_id
        AND ca.ancestor_concept_id IN (4050281, 4231680, 4012936, 4166196, 4040974)
       GROUP BY t.subject_id
     )
     SELECT subject_id,
       CASE
         WHEN is_extraanat = 1 THEN 'Extra-anatomic bypass'
         WHEN is_aortobif  = 1 THEN 'Aortobifemoral bypass'
         WHEN is_fempop    = 1 THEN 'Femoral-popliteal bypass'
         WHEN is_femtib    = 1 THEN 'Femorotibial bypass'
         WHEN is_endar     = 1 THEN 'Femoral endarterectomy'
         ELSE 'Other'
       END AS proc_type
     FROM proc_hits",
    results_schema = config$results_schema,
    cohort_table   = config$cohort_table,
    cdm_schema     = config$cdm_schema,
    target_id      = config$target_cohort_id
  )

  result <- tryCatch(
    DatabaseConnector::querySql(
      connection,
      SqlRender::translate(sql, targetDialect = "sql server")
    ),
    error = function(e) {
      warning("[fetch_proc_type_labels] Query failed: ", conditionMessage(e))
      NULL
    }
  )

  if (is.null(result) || nrow(result) == 0) {
    warning("[fetch_proc_type_labels] No rows returned.")
    return(NULL)
  }

  names(result) <- tolower(names(result))
  result[, c("subject_id", "proc_type")]
}
