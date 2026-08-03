-- =============================================================================
-- cohorts/outcome_nhd.sql
-- OUTCOME COHORT — Non-home discharge from index hospitalisation
--
-- This file is specific to the PAD / OLER Non-home Discharge (NHD) validation
-- study. Unlike the generic outcome_ssi.sql template (condition_occurrence
-- based), non-home discharge is derived from visit_occurrence.discharged_to_concept_id
-- and therefore takes no outcome_concept_ids parameter.
-- Set outcome.ancestor_concept_ids: [] in study_params.yaml to disable the
-- R/cohorts.R concept-ID guard for this outcome.
--
-- PARAMETERS (injected from study_params.yaml via R/cohorts.R):
--   @cdm_database_schema      CDM schema. Also resolves `concept` and
--                             `concept_relationship`, because the Strategus
--                             overlay unions omop_vocab into one schema
--                             (createCdmExecutionSettings has no separate
--                             vocabulary-schema parameter).
--   @target_database_schema   Cohort/results schema
--   @target_cohort_table      Cohort table name
--   @target_cohort_id         Cohort definition ID (9100001)
--
-- All four are supplied by CohortGenerator::generateCohort().
--
-- COHORT LOGIC:
--   OUTCOME EVENT : any inpatient visit (visit_concept_id = 9201) whose
--                   discharged_to_concept_id is non-null, non-zero, and NOT
--                   in the home discharge concept set.
--   INDEX DATE    : visit_end_date (discharge date). When visit_end_date is
--                   NULL, visit_start_date + 1 day is substituted.
--   COHORT EXIT   : same as index date (point event, duration = 0 days).
--   ONE ROW PER VISIT: one cohort entry per qualifying inpatient stay, not per
--                   person. This is intentional: PatientLevelPrediction (PLP)
--                   matches target cohort entries (procedures) to outcome
--                   events within a prediction window; one outcome row per
--                   visit ensures each procedure episode is matched correctly.
--
-- HOME DISCHARGE CONCEPT SET (resolved dynamically from the live vocabulary):
--   The home set is derived at execution time via a CTE rather than a
--   hard-coded list. This keeps the query correct across vocabulary versions
--   without code edits. Three anchor groups are unioned:
--
--   1. UB-04 / NUBC patient discharge codes in vocabulary_id 'UB04 Pt dis status':
--        "01" — Discharged to home or self-care (routine discharge)
--        "06" — Discharged/transferred to home under care of organised home health
--              service (Medicare home health)
--      These are the codes emitted by the Synthea-PAD fork and by most US
--      claims-based ETLs.
--
--   2. CMS Place of Service standard concept:
--        8536  — Home  (domain: Visit, vocabulary: CMS Place of Service)
--      Used by ETLs that write OMOP-suggested standard concepts directly.
--
--   3. All 'Maps to' targets of the UB04 anchors above.
--      In OMOP vocabulary v5.0 2024-10-01+, UB04 code 01 maps to
--      Visit concept 581476 ("Home Visit") and code 06 to 8536.
--      Including both anchors AND their targets makes the classification
--      robust whether the ETL stored the raw NUBC concept, the standard
--      CMS PoS concept, or a Maps-to-derived target.
--
--   Non-home = discharged_to_concept_id is NOT NULL, NOT 0, NOT in the home
--              set above, AND discharged_to_source_value (when present) is not
--              a recognised home source code (01, 06, HO, HH, HM, AM, HOME).
--              The source-value guard catches sites where the ETL has not mapped
--              alphabetic home codes to the correct standard concept ID.
--   Unknown  = NULL or 0 — excluded from this cohort (not counted as outcome).
--
-- IANNUZZI 2020 REFERENCE:
--   This outcome definition matches the primary endpoint of the Iannuzzi 2020
--   risk score for non-home discharge following open lower extremity
--   revascularization (OLER). The same definition was used as a secondary
--   outcome in the PAD / OLER SSI validation study (pad-oler-ssi-val).
-- =============================================================================

DELETE FROM @target_database_schema.@target_cohort_table
WHERE cohort_definition_id = @target_cohort_id;

-- Resolve the home discharge concept set from the live vocabulary.
-- Unioning all three anchor groups into a flat list of concept IDs that are
-- treated as "home" for the purpose of classifying discharge disposition.
WITH home_concepts AS (

  -- Anchor 1: UB-04 NUBC patient discharge codes 01 and 06
  SELECT concept_id AS home_concept_id
  FROM @cdm_database_schema.concept
  WHERE vocabulary_id    = 'UB04 Pt dis status'
    AND concept_code    IN ('01', '06')
    AND invalid_reason  IS NULL

  UNION

  -- Anchor 2: CMS Place of Service standard concept "Home"
  SELECT 8536 AS home_concept_id

  UNION

  -- Anchor 3: 'Maps to' targets of the UB04 anchors above.
  -- In vocab v5.0 2024-10-01+: 01 -> 581476 (Home Visit), 06 -> 8536 (Home).
  -- Including these ensures the NOT IN filter works whether the ETL stored
  -- the raw NUBC source concept or the Maps-to standard target.
  SELECT cr.concept_id_2 AS home_concept_id
  FROM @cdm_database_schema.concept_relationship cr
  INNER JOIN @cdm_database_schema.concept anchor
    ON  anchor.concept_id    = cr.concept_id_1
    AND anchor.vocabulary_id = 'UB04 Pt dis status'
    AND anchor.concept_code IN ('01', '06')
    AND anchor.invalid_reason IS NULL
  WHERE cr.relationship_id = 'Maps to'
    AND cr.invalid_reason  IS NULL

)

INSERT INTO @target_database_schema.@target_cohort_table (
  cohort_definition_id,
  subject_id,
  cohort_start_date,
  cohort_end_date
)
SELECT
  @target_cohort_id                                                              AS cohort_definition_id,
  vo.person_id                                                                    AS subject_id,

  -- Discharge date = cohort start. Substitute visit_start_date + 1 when
  -- visit_end_date is NULL (edge case in synthetic data).
  CAST(COALESCE(vo.visit_end_date,
                DATEADD(DAY, 1, vo.visit_start_date)) AS DATE)                  AS cohort_start_date,

  -- Point event: cohort_end_date = cohort_start_date (duration = 0 days).
  -- PLP only needs the start date to evaluate within-window occurrence.
  CAST(COALESCE(vo.visit_end_date,
                DATEADD(DAY, 1, vo.visit_start_date)) AS DATE)                  AS cohort_end_date

FROM @cdm_database_schema.visit_occurrence vo

WHERE
  -- Inpatient stays only (visit_concept_id = 9201, Inpatient Visit).
  -- Matches the same visit type filter used for the target cohort.
  vo.visit_concept_id = 9201

  -- NOTE: pad-amp-nhd-val's version filtered on study_start_date /
  -- study_end_date parameters here. CohortGenerator supplies no such
  -- parameters and renders with warnOnMissingParameters = FALSE, so leaving
  -- them in would send the un-substituted parameter text straight to SQL
  -- Server as a syntax error. Dropped rather than
  -- hardcoded: the target cohort already bounds the analysis window, and this
  -- cohort is only ever evaluated relative to that target.

  -- Known discharge disposition only: NULL and 0 are excluded.
  -- Unknown disposition is NOT counted as non-home discharge.
  AND vo.discharged_to_concept_id IS NOT NULL
  AND vo.discharged_to_concept_id <> 0

  -- Non-home: discharged_to_concept_id must NOT be in the home set.
  AND vo.discharged_to_concept_id NOT IN (
    SELECT home_concept_id FROM home_concepts
  )

  -- Require the concept to resolve to a named, valid entry in the vocabulary.
  -- Visits where discharged_to_concept_id maps to 'No matching concept',
  -- 'Unknown concept', or an invalid/deprecated concept are excluded because
  -- the true destination is uncertain.  These visits could be home discharges
  -- miscoded as non-home, and including them would inflate the NHD rate.
  AND EXISTS (
    SELECT 1
    FROM @cdm_database_schema.concept valid_c
    WHERE valid_c.concept_id   = vo.discharged_to_concept_id
      AND valid_c.invalid_reason IS NULL
      AND valid_c.concept_name NOT IN ('No matching concept', 'Unknown concept')
  )

  -- Source-value guard: treat discharged_to_source_value as authoritative for
  -- home classification. Any visit whose source code indicates Home is excluded
  -- from the NHD outcome, even when the ETL has assigned a conflicting non-home
  -- concept_id (e.g. HO source value + SNF concept_id = ETL mapping error;
  -- source value wins → excluded from NHD).
  --
  -- Home codes:     01, 06 (numeric UB-04); HO, HH, HM, HOME (alphabetic).
  --   HO is treated as Home: it is the most common alphabetic home discharge
  --   code in Duke Health OMOP and is consistent with UB-04 code 01 semantics.
  -- AM (against medical advice): patient self-discharged and returned home;
  --   classified as a home discharge and excluded from the NHD outcome.
  --
  -- NOTE: LTRIM/RTRIM alone may not strip embedded non-printing characters
  -- (CHAR(9) tab, CHAR(10) LF, CHAR(13) CR). The post-INSERT DELETE below
  -- applies REPLACE-based stripping as a second pass to catch these cases.
  AND (
    vo.discharged_to_source_value IS NULL
    OR UPPER(LTRIM(RTRIM(vo.discharged_to_source_value)))
       NOT IN ('01', '06', 'HO', 'HH', 'HM', 'AM', 'HOME')
  );

-- =============================================================================
-- Post-instantiation cleanup: remove any NHD cohort entries whose corresponding
-- inpatient visit has a home or home-equivalent source value.
--
-- This second pass uses REPLACE to strip non-printing characters
-- (CHAR(9) = tab, CHAR(10) = LF, CHAR(13) = CR) that may be embedded in
-- discharged_to_source_value and survive LTRIM/RTRIM. Such hidden characters
-- can cause the WHERE-clause guard in the INSERT above to miss home visits,
-- resulting in false-positive NHD entries (e.g. 'HO\r' passing the NOT IN test).
--
-- The join uses person_id + visit_concept_id + computed discharge date to
-- re-identify the visit that produced each cohort row without adding a
-- visit_occurrence_id column to the cohort table.
-- =============================================================================
DELETE nhd
FROM @target_database_schema.@target_cohort_table nhd
INNER JOIN @cdm_database_schema.visit_occurrence vo
  ON  vo.person_id        = nhd.subject_id
  AND vo.visit_concept_id = 9201   -- Inpatient Visit only
  AND CAST(COALESCE(vo.visit_end_date,
                    DATEADD(DAY, 1, vo.visit_start_date)) AS DATE)
      = nhd.cohort_start_date
WHERE nhd.cohort_definition_id = @target_cohort_id
  -- Strip CHAR(13)/CHAR(10)/CHAR(9) before comparing, then check against
  -- the full list of home/home-equivalent source codes.
  AND UPPER(REPLACE(REPLACE(REPLACE(
        LTRIM(RTRIM(ISNULL(vo.discharged_to_source_value, ''))),
        CHAR(13), ''), CHAR(10), ''), CHAR(9), ''))
      IN ('01', '06', 'HO', 'HH', 'HM', 'AM', 'HOME');
