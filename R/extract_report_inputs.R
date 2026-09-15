# =============================================================================
# extract_report_inputs.R — the EXTRACT half of the report pipeline
#
# PURPOSE
#   Runs every live CDM query the manuscript report needs and writes the results
#   to plain CSV artifacts. This is the only file in the report path that talks
#   to a database.
#
#   The report itself (R/report_prognostic.R) reads those CSVs and never opens a
#   connection. That split is deliberate and is enforced structurally, not by
#   convention: .report_prognostic() no longer accepts a connection_details
#   argument, so it *cannot* reach a database even by accident.
#
#     extract  (needs a database; runs where the data lives — PRCC, or locally
#      |        against a synthetic CDM)
#      |        writes aggregate CSVs
#      v
#     output/report_inputs/*.csv
#      |
#      v
#     render   (needs ONLY files — no DB, no VPN, no credentials)
#
#   Motivation and full design: docs/MIGRATION_PLAN_REPO_SPLIT.md, Phase 0.
#   The render half is destined for the shared `omop-report-toolkit` repo; this
#   half stays with the study, because the SQL is study-specific.
#
# INPUTS
#   config             — study config from get_validation_config()
#   connection_details — DatabaseConnector connection details
#
# OUTPUTS  (all under <output_folder>/report_inputs/, unless noted)
#   demographics_sex.csv             concept_id / category / n
#   demographics_race.csv            concept_id / category / n
#   demographics_ethnicity.csv       concept_id / category / n
#   demographics_indication.csv      category / n   (PAD, DM, LE wound)
#   demographics_procedure_type.csv  category / n   (AKA, BKA, Other)
#   nhd_outcomes_scalars.csv         one row: n_nhd, median_los, los_p25,
#                                    los_p75, n_readmission, n_death
#   nhd_outcomes_destinations.csv    destination / n
#   cdm_source_metadata.csv          cdm_version / vocabulary_version (methods text)
#   supp_cdm_source.csv              full cdm_source row (Supplemental Table S1)
#   supp_cpt_codes.csv               CPT codes by subgroup (Supplemental Table S3)
#   supp_discharge_destinations.csv  discharge source codes (Supplemental Table S4)
#   supp_admission_source.csv        admitted_from source codes/concepts, raw
#                                    distribution -- diagnostic only, added
#                                    2026-09-15 to check whether the target
#                                    cohort should be restricted to
#                                    home-admitted patients. Not yet consumed
#                                    by the report; no classification column
#                                    (unlike supp_discharge_destinations) since
#                                    Duke's admission-source coding is not yet
#                                    known -- see this file's own query comment.
#   _report_config.yaml              the ~9 config$ fields report_prognostic.R
#                                    actually reads (narrative dates, DCA axis
#                                    bound, score_type routing — no schema
#                                    names, no cohort ids). Lets the render
#                                    half run from this directory alone, with
#                                    no duplicated study_params.yaml in the
#                                    report repo to drift out of sync.
#   _manifest.csv                    what was written, when, and against which
#                                    schemas — so a stale render is detectable
#
#   NOT written to disk at all (returned in-memory instead — see RETURN VALUE):
#   age at index, subject_id -> discharge_type
#
#   Written OUTSIDE report_inputs/, into the study output folder:
#   pad_amp_nhd_edge_<date>.csv      PHI — see section 5. Not a render input.
#
# RETURN VALUE (REVISED 2026-08-19 — see DISCLOSURE BOUNDARY below for why)
#   A list: `dir` (the report_inputs_dir path, as before), `demog_age`
#   (the age-at-index data frame — one row per patient, from `demog$age`),
#   `discharge_types` (subject_id/discharge_type — one row per patient).
#   StrategusCodeToRun.R must pass `demog_age`/`discharge_types` straight into
#   `aggregate_report_inputs()`, which turns them into `agg_age_summary.csv` /
#   `agg_age_groups.csv` / `agg_nhd_by_year.csv` — see that file's header.
#   These two frames are NEVER written to a file anywhere in this pipeline.
#
# DISCLOSURE BOUNDARY — READ BEFORE ADDING AN OUTPUT
#   Everything above is aggregate. Age-at-index and discharge-type used to be
#   written here as `demographics_age.csv` / `discharge_types.csv` — one row
#   per patient each, pseudonymous (keyed by `subject_id`, an OMOP person_id,
#   not an MRN) but not aggregate — then read back and aggregated by
#   `aggregate_report_inputs.R`, which deleted the two files afterward. That
#   two-step "write, then delete" pattern left a real window (a crash between
#   the two steps, or a run with `keep_row_level = TRUE`) where patient-level
#   data sat in `report_inputs/`. Per explicit instruction (2026-08-19), no
#   patient-level file is written at all now: both frames are returned
#   in-memory and consumed directly by `aggregate_report_inputs()` in the same
#   R session — see RETURN VALUE above.
#
#   What must NEVER be written here: direct identifiers. In particular
#   output/pad_amp_nhd_edge_<date>.csv contains MRN, age, and procedure date.
#   That file is PHI. It stays on PRCC, it is not written by this script, and it
#   must never become an input to render. If you add an output that carries a
#   source_value, a name, a date of birth, or an MRN, it does not belong here.
#
# ASSUMPTIONS
#   - Column-name casing is preserved exactly as DatabaseConnector returned it.
#     DatabaseConnector >= 6.0 stopped auto-uppercasing, and the render half
#     does case-insensitive lookups, so the CSVs must round-trip names verbatim
#     (hence check.names = FALSE on the read side).
#   - A query that fails writes NO file. The reader maps a missing file back to
#     NULL, which is exactly what the old in-report fetchers returned on
#     failure, so downstream `is.null()` guards keep working unchanged.
# =============================================================================


# =============================================================================
# 1. FETCHERS  (moved verbatim from R/report_prognostic.R, Phase 0)
#
# These three functions were closures inside .report_prognostic(). They are
# unchanged apart from dedenting — deliberately, so this refactor cannot alter
# a single query result. Do not "tidy" them here; any behaviour change must be
# a separate, reviewable commit.
# =============================================================================

fetch_demographics_from_omop <- function(config, connection_details) {
  if (is.null(config) || is.null(connection_details)) {
    return(NULL)
  }

  conn <- NULL
  out <- NULL
  try({
    conn <- DatabaseConnector::connect(connection_details)

    # Defensive deduplication: one row per person_id in case of ETL re-runs.
    # Uses the row with the most-frequent person_source_value (= most recent run)
    # and breaks ties by taking the first row per person_id within that group.
    dedup_person_cte <-
      "dedup_person AS (
         SELECT p2.*
         FROM (
           SELECT p3.*,
             ROW_NUMBER() OVER (
               PARTITION BY p3.person_id
               ORDER BY src_freq.n DESC, p3.person_source_value DESC, p3.year_of_birth DESC
             ) AS _rn
           FROM @cdm_schema.person p3
           INNER JOIN (
             SELECT person_id, person_source_value, COUNT(*) AS n
             FROM @cdm_schema.person
             GROUP BY person_id, person_source_value
           ) src_freq
             ON src_freq.person_id      = p3.person_id
            AND src_freq.person_source_value = p3.person_source_value
         ) p2
         WHERE p2._rn = 1
       )"

    # Age = FLOOR((procedure_date - birth_date) / 365.25), where birth_date
    # is constructed from the three OMOP person fields year_of_birth,
    # month_of_birth, day_of_birth (ETLSyntheaBuilder populates all three;
    # birth_datetime is optional in CDM 5.4 and may be NULL).
    # DATEFROMPARTS defaults to mid-year (Jul 1) when month/day are missing
    # so that any residual imprecision is symmetric rather than biased.
    # The DATEDIFF(DAY, ...) / 365.25 division is done in floating-point
    # so that fractional years are preserved before FLOOR rounds down to the
    # last completed year — matching the user-specified formula exactly.
    # WHERE-clause year_of_birth guard is load-bearing, not cosmetic: on real
    # (non-Synthea) CDMs, person.year_of_birth can carry a garbage sentinel
    # value for a small number of records (e.g. 0, or another out-of-range
    # year). DATEFROMPARTS() returns NULL for a NULL input, but THROWS for a
    # non-NULL out-of-range year -- and unlike a WHERE-clause NULL comparison,
    # a single bad row's arithmetic error aborts the entire batch with zero
    # rows returned and no exception message ever reaching R (SQL Server
    # evaluates the expression before any per-row filtering elsewhere).
    # Confirmed this way against a real Duke PRCC run: no "[report] Age query
    # failed" message ever printed, yet agg_age_summary.csv was never written
    # -- consistent with the query executing "successfully" but as zero rows,
    # not with a caught exception. Explicit CAST(...AS INT) guards against a
    # column-type surprise on a real CDM (Synthea's ETL always writes plain
    # integers; a real site's ETL is not guaranteed to).
    sql_age <- SqlRender::render(
      paste0(
        "WITH ", dedup_person_cte, "
         SELECT
           CAST(
             FLOOR(
               CAST(DATEDIFF(DAY,
                 DATEFROMPARTS(
                   CAST(p.year_of_birth AS INT),
                   COALESCE(CAST(p.month_of_birth AS INT), 7),
                   COALESCE(CAST(p.day_of_birth AS INT),   1)
                 ),
                 t.cohort_start_date
               ) AS FLOAT) / 365.25
             )
           AS FLOAT) AS age_at_index
         FROM @results_schema.@cohort_table t
         INNER JOIN dedup_person p ON p.person_id = t.subject_id
         WHERE t.cohort_definition_id = @target_id
           AND p.year_of_birth BETWEEN 1900 AND YEAR(GETDATE())"
      ),
      results_schema = results_schema_prefix(config),
      cohort_table   = config$cohort_table,
      cdm_schema     = config$cdm_schema,
      target_id      = config$target_cohort_id
    )
    age_df <- tryCatch(
      DatabaseConnector::querySql(
        conn,
        SqlRender::translate(sql_age, targetDialect = "sql server")
      ),
      error = function(e) {
        message("[report] Age query failed: ", conditionMessage(e))
        NULL
      }
    )
    # A 0-row (but non-NULL) result is a distinct failure mode from a caught
    # exception -- it means the query executed but every candidate row was
    # excluded (e.g. an out-of-range year_of_birth, or a subject_id absent
    # from dedup_person). Surface it explicitly rather than silently letting
    # agg_age_summary.csv go unwritten downstream with no log trace at all.
    if (!is.null(age_df) && nrow(age_df) == 0) {
      message("[report] Age query returned 0 rows -- agg_age_summary.csv will not be written.")
    }

    # Returns concept_id, category (concept_name), and n per group so that
    # downstream lookups can match on concept_id rather than on concept_name
    # strings (which are fragile to vocabulary version changes and regex errors).
    distribution_sql <- function(concept_col) {
      SqlRender::render(
        paste0(
          "WITH ", dedup_person_cte, "
           SELECT
             COALESCE(p.@concept_col, 0)                        AS concept_id,
             COALESCE(NULLIF(c.concept_name, ''), 'Unknown')    AS category,
             COUNT(DISTINCT t.subject_id)                       AS n
           FROM @results_schema.@cohort_table t
           INNER JOIN dedup_person p ON p.person_id = t.subject_id
           LEFT  JOIN @cdm_schema.concept c ON c.concept_id = p.@concept_col
           WHERE t.cohort_definition_id = @target_id
           GROUP BY COALESCE(p.@concept_col, 0),
                    COALESCE(NULLIF(c.concept_name, ''), 'Unknown')
           ORDER BY n DESC, category"
        ),
        results_schema = results_schema_prefix(config),
        cohort_table   = config$cohort_table,
        cdm_schema     = config$cdm_schema,
        concept_col    = concept_col,
        target_id      = config$target_cohort_id
      )
    }

    sex_df <- DatabaseConnector::querySql(
      conn,
      SqlRender::translate(distribution_sql("gender_concept_id"), targetDialect = "sql server")
    )
    race_df <- DatabaseConnector::querySql(
      conn,
      SqlRender::translate(distribution_sql("race_concept_id"), targetDialect = "sql server")
    )
    ethnicity_df <- DatabaseConnector::querySql(
      conn,
      SqlRender::translate(distribution_sql("ethnicity_concept_id"), targetDialect = "sql server")
    )

    # ---- Indication categories (condition_ancestor rollup, any time before index date) ----
    # PAD   : ancestor 317309  (Peripheral vascular disease, [vocab query])
    # DM    : ancestor 201820  (Diabetes mellitus, [vocab query])
    # Wound : 25 LE wound ancestors from target_amputation.sql ([vocab query])
    sql_indication <- SqlRender::render(
      "WITH target AS (
         SELECT subject_id, cohort_start_date
         FROM @results_schema.@cohort_table
         WHERE cohort_definition_id = @target_id
       ),
       pad AS (
         SELECT DISTINCT co.person_id
         FROM @cdm_schema.condition_occurrence co
         INNER JOIN @cdm_schema.concept_ancestor ca
           ON ca.descendant_concept_id = co.condition_concept_id
          AND ca.ancestor_concept_id   = 317309
         INNER JOIN target t ON t.subject_id = co.person_id
           AND co.condition_start_date <= t.cohort_start_date
       ),
       dm AS (
         SELECT DISTINCT co.person_id
         FROM @cdm_schema.condition_occurrence co
         INNER JOIN @cdm_schema.concept_ancestor ca
           ON ca.descendant_concept_id = co.condition_concept_id
          AND ca.ancestor_concept_id   = 201820
         INNER JOIN target t ON t.subject_id = co.person_id
           AND co.condition_start_date <= t.cohort_start_date
       ),
       wound AS (
         SELECT DISTINCT co.person_id
         FROM @cdm_schema.condition_occurrence co
         INNER JOIN @cdm_schema.concept_ancestor ca
           ON ca.descendant_concept_id = co.condition_concept_id
          AND ca.ancestor_concept_id   IN (
              197304, 4097962, 4054067,
              4108371, 433696, 4226354, 4263116, 4291464, 4112159,
              4111843, 317577, 4256119, 4111732, 40480503, 46273172,
              46273522, 46270361,
              42709838, 4028237, 4320944, 133566,
              133853, 1246044,
              4087682, 4159742
            )
         INNER JOIN target t ON t.subject_id = co.person_id
           AND co.condition_start_date <= t.cohort_start_date
       )
       SELECT 'PAD'   AS category, COUNT(*) AS n FROM pad
       UNION ALL
       SELECT 'Diabetes mellitus', COUNT(*)      FROM dm
       UNION ALL
       SELECT 'LE wound / gangrene', COUNT(*)    FROM wound",
      results_schema = results_schema_prefix(config),
      cohort_table   = config$cohort_table,
      cdm_schema     = config$cdm_schema,
      target_id      = config$target_cohort_id
    )
    indication_df <- tryCatch(
      DatabaseConnector::querySql(
        conn, SqlRender::translate(sql_indication, targetDialect = "sql server")
      ),
      error = function(e) NULL
    )

    # ---- Amputation level (qualifying procedure at index visit) --------------
    # Concept ancestor rollup by amputation level (mutually exclusive priority:
    # AKA → BKA → Other). Uses OMOP procedure_concept_id at the index visit.
    # Ancestor IDs (SNOMED, verified [vocab query]):
    #   4195136 = Amputation above knee (AKA)
    #   4338257 = Amputation below knee (BKA) / through tibia and fibula
    #   Other   = through-knee (4143795), hip disarticulation (4242396),
    #             ankle disarticulation (4264289), hemipelvectomy (36675618)
    sql_proc_type <- SqlRender::render(
      "WITH target AS (
         SELECT subject_id,
                cohort_start_date,
                ISNULL(cohort_end_date, cohort_start_date) AS cohort_end_date
         FROM @results_schema.@cohort_table
         WHERE cohort_definition_id = @target_id
       ),
       amp_level AS (
         SELECT t.subject_id,
                CASE
                  WHEN MAX(CASE WHEN ca.ancestor_concept_id = 4195136 THEN 1 ELSE 0 END) = 1
                    THEN 'Above-knee amputation (AKA)'
                  WHEN MAX(CASE WHEN ca.ancestor_concept_id = 4338257 THEN 1 ELSE 0 END) = 1
                    THEN 'Below-knee amputation (BKA)'
                  ELSE 'Other amputation'
                END AS category
         FROM target t
         INNER JOIN @cdm_schema.procedure_occurrence po
           ON po.person_id = t.subject_id
          AND po.procedure_date BETWEEN t.cohort_start_date AND t.cohort_end_date
         INNER JOIN @cdm_schema.concept_ancestor ca
           ON ca.descendant_concept_id = po.procedure_concept_id
          AND ca.ancestor_concept_id   IN (4195136, 4338257, 4143795,
                                           4242396, 4264289, 36675618)
         GROUP BY t.subject_id
       )
       SELECT 'Above-knee amputation (AKA)' AS category,
              COUNT(DISTINCT subject_id)     AS n
       FROM amp_level WHERE category = 'Above-knee amputation (AKA)'
       UNION ALL
       SELECT 'Below-knee amputation (BKA)', COUNT(DISTINCT subject_id)
       FROM amp_level WHERE category = 'Below-knee amputation (BKA)'
       UNION ALL
       SELECT 'Other amputation',            COUNT(DISTINCT subject_id)
       FROM amp_level WHERE category = 'Other amputation'",
      results_schema = results_schema_prefix(config),
      cohort_table   = config$cohort_table,
      cdm_schema     = config$cdm_schema,
      target_id      = config$target_cohort_id
    )
    proc_type_df <- tryCatch(
      DatabaseConnector::querySql(
        conn, SqlRender::translate(sql_proc_type, targetDialect = "sql server")
      ),
      error = function(e) NULL
    )

    out <- list(
      age            = age_df,
      sex            = sex_df,
      race           = race_df,
      ethnicity      = ethnicity_df,
      indication     = indication_df,
      procedure_type = proc_type_df
    )
  }, silent = TRUE)

  if (!is.null(conn)) {
    try(DatabaseConnector::disconnect(conn), silent = TRUE)
  }

  out
}

# ---------------------------------------------------------------------------
# fetch_nhd_outcomes_from_omop()
#
# Queries post-discharge outcome statistics for patients with non-home
# discharge (NHD) at the end of their index hospitalization:
#   1. NHD patient count (denominator)
#   2. Discharge destination breakdown from visit_occurrence.discharged_to_concept_id:
#      SNF, IRF, LTAC, Hospice, and other NHD destinations, grouped dynamically
#      from the concept table (no hardcoded destination concept IDs)
#   3. Index hospitalization length of stay: median (IQR)
#   4. 90-day readmission: inpatient visit (concept 9201) starting after
#      index discharge and within 90 days of the index date
#   5. 90-day mortality: death record within 90 days of index date
#
# Returns a named list; all counts are NA when the corresponding query fails.
# ---------------------------------------------------------------------------
fetch_nhd_outcomes_from_omop <- function(config, connection_details) {
  if (is.null(config) || is.null(connection_details)) return(NULL)

  conn <- NULL
  out  <- NULL
  try({
    conn <- DatabaseConnector::connect(connection_details)

    # Shared CTE: NHD patients (outcome cohort) joined to their index date
    # from the target cohort. Each patient appears once.
    #
    # PREDICTION-WINDOW FILTER (added 2026-07-26): this must match
    # get_outcomes()'s definition of the binary NHD flag EXACTLY —
    # `o.cohort_start_date BETWEEN t.index_date AND
    # index_date + prediction_window_days` — or this function's NHD count
    # (which drives Table 2 and its narrative text) can silently diverge
    # from Table 1's NHD count (sum(person_level$outcome)), which is exactly
    # what happened previously: an outcome-cohort record with no window
    # bound here counted as NHD in Table 2 even when it fell outside the
    # prediction window get_outcomes() uses, producing a 79-vs-80 mismatch
    # between the two tables describing the same cohort.
    nhd_cte <- "nhd_pts AS (
      SELECT
        o.subject_id,
        t.cohort_start_date AS index_date
      FROM @results_schema.@cohort_table o
      INNER JOIN @results_schema.@cohort_table t
        ON  t.subject_id           = o.subject_id
        AND t.cohort_definition_id = @target_id
      WHERE o.cohort_definition_id = @outcome_id
        AND o.cohort_start_date >= t.cohort_start_date
        AND o.cohort_start_date <= DATEADD(DAY, @prediction_window_days, t.cohort_start_date)
    )"

    # 1. NHD patient count
    sql_count <- SqlRender::render(
      paste0(
        "WITH ", nhd_cte, "
         SELECT COUNT(DISTINCT subject_id) AS n_nhd
         FROM nhd_pts"
      ),
      results_schema = results_schema_prefix(config),
      cohort_table   = config$cohort_table,
      target_id      = config$target_cohort_id,
      outcome_id     = config$outcome_cohort_id,
      prediction_window_days = config$prediction_window_days
    )
    count_raw <- tryCatch({
      r <- DatabaseConnector::querySql(
        conn, SqlRender::translate(sql_count, targetDialect = "sql server")
      )
      names(r) <- tolower(names(r))
      r
    }, error = function(e) {
      message("[report] NHD count query failed: ", conditionMessage(e))
      NULL
    })

    # 2. Discharge destination breakdown — dynamically grouped by concept name.
    #    Joins visit_occurrence (index visit: visit containing the index date) to
    #    omop_vocab.concept to get the human-readable discharge destination label.
    #    Groups with fewer than 11 patients are suppressed (small-cell privacy rule).
    sql_dest <- SqlRender::render(
      paste0(
        "WITH ", nhd_cte, ",
         index_visit AS (
           SELECT
             np.subject_id,
             np.index_date,
             vo.discharged_to_concept_id,
             vo.discharged_to_source_value,
             DATEDIFF(DAY, vo.visit_start_date,
               COALESCE(vo.visit_end_date, vo.visit_start_date)) AS los_days,
             CAST(COALESCE(vo.visit_end_date, vo.visit_start_date) AS DATE) AS discharge_date
           FROM nhd_pts np
           INNER JOIN @cdm_schema.visit_occurrence vo
             ON  vo.person_id        = np.subject_id
             AND vo.visit_concept_id = 9201
             AND CAST(vo.visit_start_date AS DATE) <= np.index_date
             AND CAST(COALESCE(vo.visit_end_date, vo.visit_start_date) AS DATE) >= np.index_date
           WHERE
             -- Defensive home-source filter: strip non-printing characters before
             -- checking source_value so that HO/AM/01/06 with embedded CHAR(13)/
             -- CHAR(10)/CHAR(9) are still caught. Mirrors the post-INSERT DELETE
             -- in outcome_nhd.sql. NULL source value = destination unknown from
             -- source (concept_id determines classification), so it is kept.
             vo.discharged_to_source_value IS NULL
             OR UPPER(REPLACE(REPLACE(REPLACE(
                  LTRIM(RTRIM(vo.discharged_to_source_value)),
                  CHAR(13), ''), CHAR(10), ''), CHAR(9), ''))
                NOT IN ('01', '06', 'HO', 'HH', 'HM', 'AM', 'HOME')
         )
         SELECT
           COALESCE(
             c.concept_name,
             CASE UPPER(LTRIM(RTRIM(iv.discharged_to_source_value)))
               WHEN '03' THEN 'Skilled nursing facility (SNF)'
               WHEN 'SN' THEN 'Skilled nursing facility (SNF)'
               WHEN 'NH' THEN 'Skilled nursing facility (SNF)'
               WHEN '62' THEN 'Inpatient rehab facility (IRF)'
               WHEN 'RH' THEN 'Inpatient rehab facility (IRF)'
               WHEN '50' THEN 'Hospice'
               WHEN '51' THEN 'Hospice'
               WHEN '52' THEN 'Hospice'
               WHEN 'HS' THEN 'Hospice'
               WHEN '04' THEN 'Long-term acute care (LTAC)'
               WHEN '41' THEN 'Long-term acute care (LTAC)'
               WHEN 'EX' THEN 'Long-term acute care (LTAC)'
               WHEN 'IP' THEN 'Other institutional (IP)'
               WHEN 'OT' THEN 'Other NHD'
               ELSE COALESCE(iv.discharged_to_source_value, 'Unknown')
             END
           ) AS destination,
           COUNT(DISTINCT iv.subject_id) AS n
         FROM index_visit iv
         LEFT JOIN @vocab_schema.concept c
           ON  c.concept_id = iv.discharged_to_concept_id
          AND  c.concept_name NOT IN ('No matching concept', 'Unknown concept')
         GROUP BY
           COALESCE(
             c.concept_name,
             CASE UPPER(LTRIM(RTRIM(iv.discharged_to_source_value)))
               WHEN '03' THEN 'Skilled nursing facility (SNF)'
               WHEN 'SN' THEN 'Skilled nursing facility (SNF)'
               WHEN 'NH' THEN 'Skilled nursing facility (SNF)'
               WHEN '62' THEN 'Inpatient rehab facility (IRF)'
               WHEN 'RH' THEN 'Inpatient rehab facility (IRF)'
               WHEN '50' THEN 'Hospice'
               WHEN '51' THEN 'Hospice'
               WHEN '52' THEN 'Hospice'
               WHEN 'HS' THEN 'Hospice'
               WHEN '04' THEN 'Long-term acute care (LTAC)'
               WHEN '41' THEN 'Long-term acute care (LTAC)'
               WHEN 'EX' THEN 'Long-term acute care (LTAC)'
               WHEN 'IP' THEN 'Other institutional (IP)'
               WHEN 'OT' THEN 'Other NHD'
               ELSE COALESCE(iv.discharged_to_source_value, 'Unknown')
             END
           )
         HAVING COUNT(DISTINCT iv.subject_id) >= 11
         ORDER BY n DESC"
      ),
      results_schema = results_schema_prefix(config),
      cohort_table   = config$cohort_table,
      cdm_schema     = config$cdm_schema,
      vocab_schema   = config$vocab_schema %||% "omop_vocab",
      target_id      = config$target_cohort_id,
      outcome_id     = config$outcome_cohort_id,
      prediction_window_days = config$prediction_window_days
    )
    dest_raw <- tryCatch({
      r <- DatabaseConnector::querySql(
        conn, SqlRender::translate(sql_dest, targetDialect = "sql server")
      )
      names(r) <- tolower(names(r))
      r
    }, error = function(e) {
      message("[report] Discharge destination query failed: ", conditionMessage(e))
      NULL
    })

    # 3. Index hospitalization LOS (all NHD patients, not just those with
    #    a matched destination) — PERCENTILE_CONT requires OVER () in SQL Server.
    sql_los <- SqlRender::render(
      paste0(
        "WITH ", nhd_cte, ",
         index_visit AS (
           SELECT
             np.subject_id,
             DATEDIFF(DAY, vo.visit_start_date,
               COALESCE(vo.visit_end_date, vo.visit_start_date)) AS los_days
           FROM nhd_pts np
           INNER JOIN @cdm_schema.visit_occurrence vo
             ON  vo.person_id        = np.subject_id
             AND vo.visit_concept_id = 9201
             AND CAST(vo.visit_start_date AS DATE) <= np.index_date
             AND CAST(COALESCE(vo.visit_end_date, vo.visit_start_date) AS DATE) >= np.index_date
         )
         SELECT TOP 1
           COUNT(*) OVER ()                                                         AS n_los,
           CAST(PERCENTILE_CONT(0.25)
             WITHIN GROUP (ORDER BY CAST(los_days AS FLOAT)) OVER () AS FLOAT)     AS p25,
           CAST(PERCENTILE_CONT(0.5)
             WITHIN GROUP (ORDER BY CAST(los_days AS FLOAT)) OVER () AS FLOAT)     AS median_los,
           CAST(PERCENTILE_CONT(0.75)
             WITHIN GROUP (ORDER BY CAST(los_days AS FLOAT)) OVER () AS FLOAT)     AS p75
         FROM index_visit"
      ),
      results_schema = results_schema_prefix(config),
      cohort_table   = config$cohort_table,
      cdm_schema     = config$cdm_schema,
      target_id      = config$target_cohort_id,
      outcome_id     = config$outcome_cohort_id,
      prediction_window_days = config$prediction_window_days
    )
    los_raw <- tryCatch({
      r <- DatabaseConnector::querySql(
        conn, SqlRender::translate(sql_los, targetDialect = "sql server")
      )
      names(r) <- tolower(names(r))
      r
    }, error = function(e) {
      message("[report] LOS query failed: ", conditionMessage(e))
      NULL
    })

    # Shared sub-CTE for items 4-5: the index hospitalization's discharge
    # date. Reused so 90-day windows can be anchored on discharge — the
    # clinically meaningful start of "post-discharge" follow-up — instead of
    # the admission date.
    #
    # ANCHOR FIX (2026-07-26): items 4 and 5 previously anchored their 90-day
    # windows on index_date (admission), not discharge_date. With a median
    # index-hospitalization LOS of ~14 days, that shrinks the true
    # post-discharge follow-up window to ~76 days for a typical patient and
    # LESS for anyone with a longer stay — an immortal-time-style distortion
    # where patients who stayed in hospital longer get a shorter effective
    # window to be readmitted or die post-discharge. discharge_date uses the
    # same COALESCE(visit_end_date, visit_start_date + 1 day) fallback as
    # the discharge-destination query above (item 2), for the same reason:
    # synthetic data can have a null visit_end_date.
    nhd_discharge_cte <- ",
         nhd_discharge AS (
           SELECT np.subject_id,
                  np.index_date,
                  CAST(COALESCE(vo.visit_end_date,
                    DATEADD(DAY, 1, vo.visit_start_date)) AS DATE) AS discharge_date
           FROM nhd_pts np
           INNER JOIN @cdm_schema.visit_occurrence vo
             ON  vo.person_id        = np.subject_id
             AND vo.visit_concept_id = 9201
             AND CAST(vo.visit_start_date AS DATE) <= np.index_date
             AND CAST(COALESCE(vo.visit_end_date, vo.visit_start_date) AS DATE) >= np.index_date
         )"

    # 4. 90-day readmission: inpatient visit (visit_concept_id 9201) starting
    #    after the index hospitalization's discharge date and within 90 days
    #    of that discharge date (see nhd_discharge_cte note above).
    sql_readm <- SqlRender::render(
      paste0(
        "WITH ", nhd_cte, nhd_discharge_cte, "
         SELECT COUNT(DISTINCT nd.subject_id) AS n_readmission
         FROM nhd_discharge nd
         INNER JOIN @cdm_schema.visit_occurrence vo
           ON  vo.person_id        = nd.subject_id
           AND vo.visit_concept_id = 9201
           AND CAST(vo.visit_start_date AS DATE) > nd.discharge_date
           AND CAST(vo.visit_start_date AS DATE) <= DATEADD(DAY, 90, nd.discharge_date)"
      ),
      results_schema = results_schema_prefix(config),
      cohort_table   = config$cohort_table,
      cdm_schema     = config$cdm_schema,
      target_id      = config$target_cohort_id,
      outcome_id     = config$outcome_cohort_id,
      prediction_window_days = config$prediction_window_days
    )
    readm_raw <- tryCatch({
      r <- DatabaseConnector::querySql(
        conn, SqlRender::translate(sql_readm, targetDialect = "sql server")
      )
      names(r) <- tolower(names(r))
      message("[report] 90-day readmission n = ", r$n_readmission[1])
      r
    }, error = function(e) {
      message("[report] 90-day readmission query failed: ", conditionMessage(e))
      NULL
    })

    # 5. 90-day mortality: death record within 90 days of the index
    #    hospitalization's discharge date (see nhd_discharge_cte note above;
    #    previously anchored on index_date/admission).
    sql_death <- SqlRender::render(
      paste0(
        "WITH ", nhd_cte, nhd_discharge_cte, "
         SELECT COUNT(DISTINCT nd.subject_id) AS n_death
         FROM nhd_discharge nd
         INNER JOIN @cdm_schema.death d
           ON  d.person_id = nd.subject_id
           AND CAST(d.death_date AS DATE) >= nd.discharge_date
           AND CAST(d.death_date AS DATE) <= DATEADD(DAY, 90, nd.discharge_date)"
      ),
      results_schema = results_schema_prefix(config),
      cohort_table   = config$cohort_table,
      cdm_schema     = config$cdm_schema,
      target_id      = config$target_cohort_id,
      outcome_id     = config$outcome_cohort_id,
      prediction_window_days = config$prediction_window_days
    )
    death_raw <- tryCatch({
      r <- DatabaseConnector::querySql(
        conn, SqlRender::translate(sql_death, targetDialect = "sql server")
      )
      names(r) <- tolower(names(r))
      message("[report] 90-day mortality n = ", r$n_death[1])
      r
    }, error = function(e) {
      message("[report] 90-day mortality query failed: ", conditionMessage(e))
      NULL
    })

    out <- list(
      n_nhd        = if (!is.null(count_raw)) as.integer(count_raw$n_nhd[1])        else NA_integer_,
      dest_df      = dest_raw,   # data frame: destination / n (may be NULL)
      median_los   = if (!is.null(los_raw))   as.numeric(los_raw$median_los[1])     else NA_real_,
      los_p25      = if (!is.null(los_raw))   as.numeric(los_raw$p25[1])            else NA_real_,
      los_p75      = if (!is.null(los_raw))   as.numeric(los_raw$p75[1])            else NA_real_,
      n_readmission = if (!is.null(readm_raw)) as.integer(readm_raw$n_readmission[1]) else NA_integer_,
      n_death      = if (!is.null(death_raw)) as.integer(death_raw$n_death[1])      else NA_integer_
    )
  }, silent = TRUE)

  if (!is.null(conn)) {
    try(DatabaseConnector::disconnect(conn), silent = TRUE)
  }

  out
}

# ---------------------------------------------------------------------------
# fetch_discharge_types_from_omop()
#
# Queries visit_occurrence.discharged_to_source_value for target-cohort
# patients' index inpatient visits and maps UB-04 discharge codes to the
# five NHD disposition categories used in the Figure 1 stacked chart:
#   SNF (03, SN, NH), IRF (62, RH), Hospice (50, 51, 52, HS),
#   LTAC (04, 41, EX), Other NHD (any non-home, non-missing code not above).
#
# Returns a data frame with columns subject_id and discharge_type, one row
# per patient. Patients with no matching visit or unmapped code get NA.
# ---------------------------------------------------------------------------
fetch_discharge_types_from_omop <- function(config, connection_details) {
  if (is.null(config) || is.null(connection_details)) return(NULL)

  conn <- NULL
  out  <- NULL
  try({
    conn <- DatabaseConnector::connect(connection_details)
    sql_dtype <- SqlRender::render(
      "WITH target AS (
         SELECT subject_id, cohort_start_date AS index_date
         FROM @results_schema.@cohort_table
         WHERE cohort_definition_id = @target_id
       )
       SELECT
         t.subject_id,
         CASE UPPER(LTRIM(RTRIM(vo.discharged_to_source_value)))
           WHEN '03' THEN 'SNF'
           WHEN 'SN' THEN 'SNF'
           WHEN 'NH' THEN 'SNF'
           WHEN '62' THEN 'IRF'
           WHEN 'RH' THEN 'IRF'
           WHEN '50' THEN 'Hospice'
           WHEN '51' THEN 'Hospice'
           WHEN '52' THEN 'Hospice'
           WHEN 'HS' THEN 'Hospice'
           WHEN '04' THEN 'LTAC'
           WHEN '41' THEN 'LTAC'
           WHEN 'EX' THEN 'LTAC'
           WHEN 'IP' THEN 'Other NHD'
           WHEN 'OT' THEN 'Other NHD'
           ELSE 'Other NHD'
         END AS discharge_type
       FROM target t
       INNER JOIN @cdm_schema.visit_occurrence vo
         ON  vo.person_id        = t.subject_id
         AND vo.visit_concept_id = 9201
         AND CAST(vo.visit_start_date AS DATE) <= t.index_date
         AND CAST(COALESCE(vo.visit_end_date, vo.visit_start_date) AS DATE) >= t.index_date
       WHERE vo.discharged_to_source_value IS NOT NULL
         AND UPPER(LTRIM(RTRIM(vo.discharged_to_source_value)))
             NOT IN ('01','06','07','08','09','20','HO','HH','HM','AM','HOME')",
      results_schema = results_schema_prefix(config),
      cohort_table   = config$cohort_table,
      cdm_schema     = config$cdm_schema,
      target_id      = config$target_cohort_id
    )
    raw <- tryCatch({
      r <- DatabaseConnector::querySql(
        conn, SqlRender::translate(sql_dtype, targetDialect = "sql server")
      )
      names(r) <- tolower(names(r))
      r
    }, error = function(e) {
      message("[report] discharge_type lookup failed: ", conditionMessage(e))
      NULL
    })
    if (!is.null(raw) && nrow(raw) > 0) {
      out <- raw[!duplicated(raw$subject_id), c("subject_id", "discharge_type")]
    }
  }, silent = TRUE)
  if (!is.null(conn)) try(DatabaseConnector::disconnect(conn), silent = TRUE)
  out
}

# =============================================================================
# 2. WRITERS
# =============================================================================

# Same guard R/report_extended.R uses. This file must be sourceable on its own
# (the extract step runs before the report layer is loaded), so it cannot rely
# on another file having defined %||% first.
if (!exists("%||%", mode = "function")) `%||%` <- function(x, y) if (is.null(x)) y else x

# Write one data frame to <dir>/<name>.csv.
#
# A NULL input writes NOTHING and returns FALSE. That is the load-bearing
# convention of this whole file: "query failed" is represented by the ABSENCE
# of a file, so the reader can map it back to NULL and every downstream
# is.null() guard in the report keeps its original meaning. Writing an empty
# file instead would silently turn a failed query into a legitimate zero.
#
# A zero-row data frame IS written (header only) — that is a successful query
# that found nothing, which is a different fact.
.write_report_input <- function(df, dir, name) {
  if (is.null(df)) {
    message("[extract] ", name, ": no data (query failed or returned NULL) — file not written")
    return(FALSE)
  }
  path <- file.path(dir, paste0(name, ".csv"))
  utils::write.csv(df, path, row.names = FALSE, na = "NA")
  message("[extract] ", name, ": ", nrow(df), " row(s) -> ", basename(path))
  TRUE
}

# =============================================================================
# 3. ENTRY POINT
# =============================================================================

#' Run every database query the manuscript report needs and write CSV artifacts.
#'
#' @param config             Study config from get_validation_config().
#' @param connection_details DatabaseConnector connection details.
#' @param inputs_dir         Destination directory. Defaults to
#'                           <config$output_folder>/report_inputs.
#' @return The directory written to, invisibly. Called for its side effects.
#'
#' Side effects: creates `inputs_dir` and writes the CSVs listed in the file
#' header, plus `_manifest.csv`. Existing files are overwritten; a query that
#' fails leaves its file absent (see .write_report_input()).
extract_report_inputs <- function(config,
                                  connection_details,
                                  inputs_dir = file.path(config$output_folder, "report_inputs")) {

  if (is.null(config))             stop("extract_report_inputs(): config is required")
  if (is.null(connection_details)) stop("extract_report_inputs(): connection_details is required")

  if (!dir.exists(inputs_dir)) dir.create(inputs_dir, recursive = TRUE)
  message("[extract] Writing report inputs to ", inputs_dir)

  written <- character(0)
  note    <- function(ok, name) if (isTRUE(ok)) written <<- c(written, name)

  # ---- Demographics (Table 1) ------------------------------------------------
  # Returns a named list of six data frames, any of which may be NULL.
  # "age" is patient-level (one row per patient) and is deliberately NOT
  # written to disk — see the file header's RETURN VALUE / DISCLOSURE
  # BOUNDARY sections. It is returned in-memory for aggregate_report_inputs()
  # to consume directly.
  demog <- fetch_demographics_from_omop(config, connection_details)
  for (part in c("sex", "race", "ethnicity", "indication", "procedure_type")) {
    nm <- paste0("demographics_", part)
    note(.write_report_input(demog[[part]], inputs_dir, nm), nm)
  }

  # ---- NHD outcomes (Table 2) ------------------------------------------------
  # Six scalars plus one data frame. The scalars are flattened into a single
  # one-row CSV so the reader can rebuild the original list shape exactly,
  # including any NA that came from a sub-query that failed.
  nhd <- fetch_nhd_outcomes_from_omop(config, connection_details)
  if (!is.null(nhd)) {
    scalars <- data.frame(
      n_nhd         = nhd$n_nhd,
      median_los    = nhd$median_los,
      los_p25       = nhd$los_p25,
      los_p75       = nhd$los_p75,
      n_readmission = nhd$n_readmission,
      n_death       = nhd$n_death,
      stringsAsFactors = FALSE
    )
    note(.write_report_input(scalars, inputs_dir, "nhd_outcomes_scalars"), "nhd_outcomes_scalars")
    note(.write_report_input(nhd$dest_df, inputs_dir, "nhd_outcomes_destinations"),
         "nhd_outcomes_destinations")
  } else {
    message("[extract] nhd_outcomes: fetch returned NULL — no files written")
  }

  # ---- Discharge dispositions (Figure 1) -------------------------------------
  # Person-level: subject_id + discharge_type. Pseudonymous, and — as of
  # 2026-08-19 — deliberately NOT written to disk; see the file header's
  # RETURN VALUE / DISCLOSURE BOUNDARY sections before adding a write call
  # back here.
  dtypes <- fetch_discharge_types_from_omop(config, connection_details)

  # ---- Supplemental tables + cdm_source metadata -----------------------------
  # Defined in section 4. These were inline DatabaseConnector calls inside the
  # report's Supplemental Material section, not fetch_*_from_omop() helpers,
  # which is why they are easy to miss — see the section 4 header.
  written <- c(written, .extract_supplemental(config, connection_details, inputs_dir))

  # ---- Report config ----------------------------------------------------------
  # The narrative/parameter fields the render half actually reads — verified by
  # grepping every config$ access across R/report_prognostic.R, R/report_extended.R,
  # and R/report_helpers.R, not assumed. Nothing here implies database access:
  # no schema names, no cohort ids, no credentials. Written here rather than
  # duplicated in a second study_params.yaml inside the report repo, so there is
  # exactly one place these values can drift from what the analysis actually
  # ran with — this file. A report author changing dca_threshold_max_pct or a
  # study date range edits study_params.yaml here and re-runs extract (cheap,
  # no Strategus re-run needed), not a copy living in another repo.
  report_config <- list(
    cdm_database_name          = config$cdm_database_name,
    dca_threshold_max_pct      = config$dca_threshold_max_pct,
    min_prior_observation_days = config$min_prior_observation_days,
    prediction_window_days     = config$prediction_window_days,
    score_type                 = config$score_type,
    study_design               = config$study_design,
    study_end_date             = config$study_end_date,
    study_start_date           = config$study_start_date,
    var_imp_file               = config$var_imp_file
  )
  yaml::write_yaml(report_config, file.path(inputs_dir, "_report_config.yaml"))
  written <- c(written, "_report_config.yaml")

  # ---- Manifest --------------------------------------------------------------
  # Lets a reviewer tell at a glance whether a rendered report came from the
  # intended CDM and how old the extract is. Rendering does not require this
  # file; it is provenance, not input.
  manifest <- data.frame(
    extracted_at   = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
    cdm_schema     = config$cdm_schema     %||% NA_character_,
    results_schema = tryCatch(results_schema_prefix(config), error = function(e) NA_character_),
    cohort_table   = config$cohort_table   %||% NA_character_,
    target_cohort_id  = config$target_cohort_id  %||% NA_integer_,
    outcome_cohort_id = config$outcome_cohort_id %||% NA_integer_,
    files_written  = paste(written, collapse = ";"),
    stringsAsFactors = FALSE
  )
  utils::write.csv(manifest, file.path(inputs_dir, "_manifest.csv"), row.names = FALSE)

  message("[extract] Done — ", length(written), " file(s) written to ", inputs_dir)

  # Return shape changed 2026-08-19: was invisible(inputs_dir) (a bare path).
  # StrategusCodeToRun.R's `report_inputs_dir <- extract_report_inputs(...)`
  # is the only caller and never used that value again, so this is safe, but
  # any NEW caller must use `$dir`, not the return value directly as a path.
  invisible(list(
    dir             = inputs_dir,
    demog_age       = demog$age,
    discharge_types = dtypes
  ))
}


# =============================================================================
# 4. SUPPLEMENTAL + METADATA QUERIES
#
# These four were NOT wrapped in fetch_*_from_omop() helpers — they were inline
# DatabaseConnector calls buried in the report's Supplemental Material section
# and in its methods-text setup. That is precisely why they survived the first
# pass of this refactor: grepping for the helper name does not find them.
#
# If you add a query to the report, add it HERE and give it an artifact. A
# query that renders directly is a query that cannot run off a results export.
#
# SQL is moved verbatim from R/report_prognostic.R; only indentation changed.
# =============================================================================

#' Query the supplemental tables and the cdm_source metadata line.
#'
#' @param config             Study config.
#' @param connection_details DatabaseConnector connection details.
#' @param inputs_dir         Destination directory (created by the caller).
#' @return Character vector of artifact names successfully written.
#'
#' Each query is independently wrapped: one failure does not suppress the
#' others, and a failed query simply leaves its artifact absent, which the
#' render half reads back as NULL.
.extract_supplemental <- function(config, connection_details, inputs_dir) {
  written <- character(0)
  conn <- NULL
  try({
    conn <- DatabaseConnector::connect(connection_details)

    run <- function(sql, name) {
      tryCatch({
        r <- DatabaseConnector::querySql(
          conn, SqlRender::translate(sql, targetDialect = "sql server")
        )
        names(r) <- tolower(names(r))
        if (.write_report_input(r, inputs_dir, name)) written <<- c(written, name)
      }, error = function(e) {
        message("[extract] ", name, ": query failed — ", conditionMessage(e))
      })
    }

    # ---- cdm_source (two shapes) ---------------------------------------------
    # The methods text needs only cdm_version + vocabulary_version; Supplemental
    # Table S1 needs the full row. Both come from one table, so query it once
    # for each shape rather than making render re-derive one from the other.

  sql_cdm_src <- SqlRender::render(
    "SELECT cdm_source_name, cdm_source_abbreviation, cdm_holder,
            source_release_date, cdm_release_date, cdm_version,
            vocabulary_version
     FROM @cdm_schema.cdm_source",
    cdm_schema = config$cdm_schema
  )
  run(sql_cdm_src, "supp_cdm_source")

    # The methods-text subset. Same table, narrower projection — kept separate
    # so a schema change to cdm_source cannot silently break the methods line.
    sql_cdm_meta <- SqlRender::render(
      "SELECT cdm_version, vocabulary_version FROM @cdm_schema.cdm_source",
      cdm_schema = config$cdm_schema
    )
    run(sql_cdm_meta, "cdm_source_metadata")

    # ---- Supplemental Table S3 — CPT codes by procedure subgroup -------------

  sql_cpt <- SqlRender::render(
    "WITH target_population AS (
       SELECT c.subject_id,
              CAST(c.cohort_start_date AS DATE) AS index_date
       FROM @results_schema.@cohort_table c
       WHERE c.cohort_definition_id = @target_id
     )
     SELECT combined.proc_group,
            combined.cpt_code,
            combined.cpt_description,
            COUNT(DISTINCT po.person_id) AS case_count
     FROM (
       -- Branch 1: CPT4 codes that ARE in concept_ancestor as descendants
       -- (covers CPT4s with standard_concept = 'S' in this vocabulary)
       SELECT grp.proc_group,
              c.concept_id   AS standard_concept_id,
              c.concept_code AS cpt_code,
              c.concept_name AS cpt_description
       FROM (
         -- [vocab query] Amputation-level ancestor concept IDs
         SELECT 'AKA (transfemoral)'    AS proc_group, 4195136 AS ancestor_id
         UNION ALL SELECT 'BKA (transtibial)',          4338257
         UNION ALL SELECT 'Knee disarticulation',       4143795
         UNION ALL SELECT 'Hip disarticulation',        4242396
         UNION ALL SELECT 'Ankle disarticulation',      4264289
         UNION ALL SELECT 'Hemipelvectomy',             36675618
       ) grp
       INNER JOIN @vocab_schema.concept_ancestor ca
         ON ca.ancestor_concept_id = grp.ancestor_id
       INNER JOIN @vocab_schema.concept c
         ON c.concept_id    = ca.descendant_concept_id
        AND c.vocabulary_id IN ('CPT4','HCPCS')

       UNION

       -- Branch 2: CPT4 source codes that map TO standard SNOMED descendants
       -- via concept_relationship (catches CPT4s not in concept_ancestor)
       SELECT grp.proc_group,
              cr.concept_id_1 AS standard_concept_id,
              c.concept_code  AS cpt_code,
              c.concept_name  AS cpt_description
       FROM (
         -- [vocab query] Amputation-level ancestor concept IDs
         SELECT 'AKA (transfemoral)'    AS proc_group, 4195136 AS ancestor_id
         UNION ALL SELECT 'BKA (transtibial)',          4338257
         UNION ALL SELECT 'Knee disarticulation',       4143795
         UNION ALL SELECT 'Hip disarticulation',        4242396
         UNION ALL SELECT 'Ankle disarticulation',      4264289
         UNION ALL SELECT 'Hemipelvectomy',             36675618
       ) grp
       INNER JOIN @vocab_schema.concept_ancestor ca
         ON ca.ancestor_concept_id = grp.ancestor_id
       INNER JOIN @vocab_schema.concept_relationship cr
         ON cr.concept_id_2    = ca.descendant_concept_id
        AND cr.relationship_id = 'Maps to'
        AND cr.invalid_reason  IS NULL
       INNER JOIN @vocab_schema.concept c
         ON c.concept_id    = cr.concept_id_1
        AND c.vocabulary_id IN ('CPT4','HCPCS')
     ) combined
     LEFT JOIN @cdm_schema.procedure_occurrence po
       ON po.person_id IN (SELECT subject_id FROM target_population)
      AND EXISTS (
            SELECT 1 FROM target_population tp
            WHERE tp.subject_id = po.person_id
              AND tp.index_date = CAST(po.procedure_date AS DATE)
          )
      AND (
            po.procedure_source_concept_id = combined.standard_concept_id
            OR po.procedure_source_value    = combined.cpt_code
          )
     GROUP BY combined.proc_group, combined.cpt_code, combined.cpt_description
     ORDER BY combined.proc_group, combined.cpt_code",
    results_schema = results_schema_prefix(config),
    cohort_table   = config$cohort_table,
    target_id      = config$target_cohort_id,
    vocab_schema   = config$vocab_schema,
    cdm_schema     = config$cdm_schema
  )
    run(sql_cpt, "supp_cpt_codes")

    # ---- Supplemental Table S4 — Discharge destination source codes ----------

  sql_dest_supp <- SqlRender::render(
    "SELECT
       COALESCE(vo.discharged_to_source_value, '(none / NULL)')  AS nubc_code,
       vo.discharged_to_concept_id                               AS omop_concept_id,
       COALESCE(MAX(c.concept_name), 'Unknown')                  AS concept_name,
       CASE vo.discharged_to_source_value
         -- Numeric UB-04 codes (CMS / synthetic data)
         WHEN '01' THEN 'Home'
         WHEN '06' THEN 'Home (home health)'
         WHEN '07' THEN 'Left AMA'
         WHEN '20' THEN 'Deceased in hospital'
         WHEN '03' THEN 'SNF'
         WHEN '62' THEN 'IRF'
         WHEN '50' THEN 'Hospice'
         WHEN '51' THEN 'Hospice'
         WHEN '04' THEN 'LTAC'
         WHEN '41' THEN 'LTAC'
         -- Alphabetic site-specific codes (PRCC / Duke Health OMOP CDM)
         WHEN 'AM'  THEN 'Against Medical Advice (home)'
         WHEN 'HO'  THEN 'Home (excluded if ETL maps to non-home concept)'
         WHEN 'HH'  THEN 'Home with services'
         WHEN 'SN'  THEN 'SNF'
         WHEN 'NH'  THEN 'SNF'
         WHEN 'RH'  THEN 'IRF'
         WHEN 'HS'  THEN 'Hospice'
         WHEN 'EX'  THEN 'LTAC'
         WHEN 'IP'  THEN 'Other NHD'
         WHEN 'OT'  THEN 'Other NHD'
         ELSE 'Other / Unknown'
       END                                                        AS classification,
       COUNT(*)                                                   AS visit_count,
       COUNT(DISTINCT vo.person_id)                              AS person_count
     FROM @results_schema.@cohort_table oc
     JOIN @cdm_schema.visit_occurrence vo
       ON vo.person_id        = oc.subject_id
      AND vo.visit_concept_id = 9201
      AND CAST(vo.visit_start_date AS DATE) <= CAST(oc.cohort_start_date AS DATE)
      AND CAST(COALESCE(vo.visit_end_date, vo.visit_start_date) AS DATE)
          >= CAST(oc.cohort_start_date AS DATE)
     LEFT JOIN @vocab_schema.concept c
       ON c.concept_id = vo.discharged_to_concept_id
     WHERE oc.cohort_definition_id = @target_id
     GROUP BY vo.discharged_to_source_value, vo.discharged_to_concept_id
     ORDER BY person_count DESC",
    results_schema = results_schema_prefix(config),
    cohort_table   = config$cohort_table,
    cdm_schema     = config$cdm_schema,
    vocab_schema   = config$vocab_schema %||% "omop_vocab",
    target_id      = config$target_cohort_id
  )
    run(sql_dest_supp, "supp_discharge_destinations")

    # ---- Supplemental (diagnostic) — Admission source codes ------------------
    #
    # Added 2026-09-15 to answer a study-design question: should the target
    # cohort be restricted to patients admitted from home (excluding SNF/
    # hospital-transfer admissions)? Two things are unknown before writing
    # that filter: (1) whether the real Duke CDM populates
    # visit_occurrence.admitted_from_concept_id / admitted_from_source_value
    # at all -- in the synthetic CDM this study runs against, both are
    # always 0/NULL (the ETL's discharge-disposition fix-up has no
    # admission-source equivalent), so writing a filter now risks the exact
    # failure CIRCE_ESCAPE_HATCH.md warns about: silently excluding everyone
    # (if "home" requires a positive match on an unpopulated field) or no
    # one (if only recognized facility codes are excluded); (2) what Duke's
    # actual admission-source coding is -- there is no documented UB-04-style
    # mapping for admission source anywhere in this workspace, unlike
    # discharge (see sql_dest_supp above). This query is diagnostic only: it
    # reports the raw distribution so a filter can be designed from real
    # values, per Rule 1 (no concept IDs guessed here). Deliberately no
    # classification column (unlike supp_discharge_destinations) since the
    # coding scheme isn't known yet.
    #
    # Same join shape as sql_dest_supp above (this study's index visit:
    # inpatient, visit_concept_id 9201, bracketing the target cohort's
    # cohort_start_date) so the two distributions describe the same visits.
    sql_admit_supp <- SqlRender::render(
      "SELECT
         COALESCE(vo.admitted_from_source_value, '(none / NULL)') AS admission_source_code,
         vo.admitted_from_concept_id                              AS omop_concept_id,
         COALESCE(MAX(c.concept_name), 'Unknown')                 AS concept_name,
         COUNT(*)                                                 AS visit_count,
         COUNT(DISTINCT vo.person_id)                             AS person_count
       FROM @results_schema.@cohort_table oc
       JOIN @cdm_schema.visit_occurrence vo
         ON vo.person_id        = oc.subject_id
        AND vo.visit_concept_id = 9201
        AND CAST(vo.visit_start_date AS DATE) <= CAST(oc.cohort_start_date AS DATE)
        AND CAST(COALESCE(vo.visit_end_date, vo.visit_start_date) AS DATE)
            >= CAST(oc.cohort_start_date AS DATE)
       LEFT JOIN @vocab_schema.concept c
         ON c.concept_id = vo.admitted_from_concept_id
       WHERE oc.cohort_definition_id = @target_id
       GROUP BY vo.admitted_from_source_value, vo.admitted_from_concept_id
       ORDER BY person_count DESC",
      results_schema = results_schema_prefix(config),
      cohort_table   = config$cohort_table,
      cdm_schema     = config$cdm_schema,
      vocab_schema   = config$vocab_schema %||% "omop_vocab",
      target_id      = config$target_cohort_id
    )
    run(sql_admit_supp, "supp_admission_source")
  }, silent = TRUE)

  if (!is.null(conn)) try(DatabaseConnector::disconnect(conn), silent = TRUE)
  written
}


# =============================================================================
# 5. EDGE-CASE EXPORT — MOVED, 2026-08-11
#
# export_edge_cases() (PHI-producing: writes MRN, age, procedure date) moved
# to duke-prcc-deploy/studies/pad-amp-nhd-val/edge_case_export.R — see
# studies.yaml's pad-amp-nhd-val entry (`edge_case_export` block) and that
# file's own header for the full history. This repo is the confirmed
# long-term shareable analysis-core citizen; PHI-producing, Duke/PRCC-only
# logic does not belong here.
#
# StrategusCodeToRun.R no longer calls this function directly — see its own
# tail for the pointer message it prints instead.
# =============================================================================
