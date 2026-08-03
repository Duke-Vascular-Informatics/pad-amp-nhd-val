# =============================================================================
# R/report_prognostic.R
#
# Parameterized Word report template for prognostic model validation studies.
# Handles both integer risk scores and LASSO models within a single template,
# routing on config$score_type.
#
# This file is sourced by R/report_extended.R (the dispatcher).
# Do NOT source R/report_helpers.R here — the dispatcher handles that.
#
# Entry points:
#   .report_prognostic(output_dir, score_output_dir, mfi5_output_dir,
#                      connection_details, config, citations)
#     — full manuscript-format Word report; called by generate_manuscript_report()
#       in R/report_extended.R.
#   .report_word_simple(output_dir, score_output_dir)
#     — lightweight report (no live CDM queries); called by generate_word_report()
#       in R/report_extended.R.
#
# Parameterization branches (inside .report_prognostic()):
#   Branch 1 — Table 2 predictor definitions:
#     lasso  → .covariate_table_data_lasso(config)
#     integer → .covariate_table_data()
#   Branch 2 — Table 3 covariate activation table:
#     lasso  → .build_combined_covariate_table(covariate_summary)
#     integer → .build_combined_component_table(covariate_summary)
#   Branch 3 — Annual / monthly outcome rate plots:
#     lasso  → .save_macce_rate_by_year_plot() / .save_macce_rate_by_month_plot()
#     integer → .save_nhd_rate_by_year_plot() / .save_nhd_rate_by_month_plot()
#   Branch 4 — Methods §2.2 narrative text:
#     lasso  → LASSO-specific paragraph
#     integer → integer-score-specific paragraph
#
# Dependencies:
#   R/report_helpers.R  — sourced by dispatcher before this file
#   dplyr               — used by .covariate_table_data_lasso()
#   readr               — used by .report_prognostic() for subgroup_bias.csv
# =============================================================================

# ---------------------------------------------------------------------------
# Integer score functions (lineage: the SSI validation study this was ported from)
# ---------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# .covariate_table_data()
#
# Returns a static data frame defining the PAD risk score covariates.
# Used as Table 2 in the Word report for integer score_type.
# Each row contains covariate_id, variable, points, lookback, omop_domain, derivation.
# This table is static (not queried from the CDM) — if covariates.csv is updated,
# this function must be kept in sync manually.
# -----------------------------------------------------------------------------
.covariate_table_data <- function() {
  data.frame(
    covariate_id = c(
      "female",
      "overweight",
      "obese",
      "urgnt",
      "abi_35",
      "prrevasc_any",
      "prolong_abx",
      "optime4h",
      "mFI_high",
      "indicationClaudication"
    ),
    variable = c(
      "Female sex",
      "Overweight (BMI 25 to <30)",
      "Obese (BMI ≥30)",
      "Urgent / emergency case",
      "Low ankle-brachial index (ABI ≤0.35)",
      "Prior revascularization (any)",
      "Prolonged antibiotic exposure",
      "Operative time ≥4 hours",
      "High modified Frailty Index (mFI)",
      "Indication: claudication"
    ),
    points = c(
      "+1", "+1", "+3", "+1", "+1",
      "+1", "+2", "+1", "+1", "−1"
    ),
    lookback = c(
      "Any time",
      "365 days",
      "365 days",
      "30 days",
      "365 days",
      "10 years",
      "90 days",
      "Index date",
      "365 days",
      "365 days"
    ),
    omop_domain = c(
      "Person",
      "Measurement",
      "Measurement",
      "Observation / Visit",
      "Measurement",
      "Procedure",
      "Drug Exposure",
      "Procedure",
      "Condition (composite)",
      "Condition"
    ),
    derivation = c(
      paste0(
        "Concept 8532 (Female) matched to person.gender_concept_id. ",
        "No lookback required; demographic attribute."
      ),
      paste0(
        "BMI resolved with three-tier priority: ",
        "(1) direct BMI measurement (LOINC 3038553, 36304833); ",
        "(2) computed from weight (LOINC 3025315, 3013762, 3011054, 3026600) ",
        "and height (LOINC 3036277, 3023540, 3015514) as weight_kg / height_m\u00b2. ",
        "Flagged when 25 \u2264 BMI < 30."
      ),
      paste0(
        "Same BMI resolution as Overweight (direct preferred, weight/height fallback). ",
        "Flagged when BMI \u2265 30. Mutually exclusive with Overweight."
      ),
      paste0(
        "Concepts 4158569 (Emergency procedure) and 4250892 (Urgent procedure), ",
        "plus all descendants via concept_ancestor, in procedure_occurrence or ",
        "observation within 30 days before or on the index date."
      ),
      paste0(
        "Concepts 40489833 and 46237026 (ABI measurement), plus descendants, in ",
        "the measurement table. Record is counted when value_as_number < 0.35."
      ),
      paste0(
        "Concepts 4236706 (Arterial bypass of lower limb artery) and 4225375 ",
        "(Endarterectomy of lower limb artery) and all descendants in procedure_occurrence. ",
        "Captures any prior lower-extremity arterial bypass or endarterectomy within a 10-year lookback."
      ),
      paste0(
        "Concept 21603553 (systemic antibiotic agent) and descendants in ",
        "drug_exposure. Counted when drug_exposure_start_date ≤ index − 1 day ",
        "and total exposure duration > 2 days (non-prophylactic heuristic)."
      ),
      paste0(
        "Operative duration derived from procedure_occurrence: ",
        "DATEDIFF(MINUTE, procedure_start_datetime, procedure_end_datetime) > 240. ",
        "Supplemented by measurement-table operative-time concepts when available."
      ),
      paste0(
        "Composite index of 5 sub-components: diabetes (201820), COPD (255573), ",
        "congestive heart failure (316139), hypertension (316866), and functional ",
        "status impairment (4215267), each with descendants in condition_occurrence. ",
        "Flagged when ≥2 conditions are present (mFI score > 0.25)."
      ),
      paste0(
        "Concept 442774 (Intermittent claudication) and descendants in ",
        "condition_occurrence. Negative point value — claudication as the ",
        "operative indication is a protective factor for post-operative non-home discharge (NHD)."
      )
    ),
    stringsAsFactors = FALSE
  )
}

# -----------------------------------------------------------------------------
# .build_combined_component_table()
#
# Merges the static covariate definitions with observed prevalence counts from
# the pipeline's covariate_summary.csv to produce a combined flextable for
# Table 3 in the Word report (integer score_type).
# Renamed from .build_combined_covariate_table() in the original monolith.
# -----------------------------------------------------------------------------
.build_combined_component_table <- function(covariate_summary_df) {
  # Build combined covariate table with definitions and prevalence
  # Input: covariate_summary dataframe with columns: covariate_name, n_positive, n_total

  border_h  <- officer::fp_border(color = "#BFBFBF", width = 0.5)
  border_out <- officer::fp_border(color = "#1F3864", width = 1.5)

  # Map covariates to their definitions and derivation methods.
  # Components: Iannuzzi JC et al. J Vasc Surg 2020;71:889-95 (NHD integer score).
  # Point values and component names match covariates/covariates.csv exactly.
  covariate_defs <- list(
    "Age 60-69 years at index" = list(
      points = "+2", definition = "Age 60–69 years at the index amputation date",
      omop_concept = "person.year_of_birth (no OMOP concept ID required)",
      derivation = "Computed from person.year_of_birth; age 60–69 at index date"
    ),
    "Age 70-79 years at index" = list(
      points = "+4", definition = "Age 70–79 years at the index amputation date",
      omop_concept = "person.year_of_birth (no OMOP concept ID required)",
      derivation = "Computed from person.year_of_birth; age 70–79 at index date"
    ),
    "Age 80 or more years at index" = list(
      points = "+6", definition = "Age ≥80 years at the index amputation date",
      omop_concept = "person.year_of_birth (no OMOP concept ID required)",
      derivation = "Computed from person.year_of_birth; age ≥80 at index date"
    ),
    "Female sex" = list(
      points = "+1", definition = "Female gender recorded in OMOP person table",
      omop_concept = "Concept 8532 (Female) [vocab query]",
      derivation = "person.gender_concept_id = 8532"
    ),
    "Non-White race" = list(
      points = "+2", definition = "Non-White race recorded in OMOP person table",
      omop_concept = "Concept 8527 (White) as reference [vocab query]; non-White = race_concept_id ≠ 8527",
      derivation = "person.race_concept_id ≠ 8527 and ≠ 0 (unknown)"
    ),
    "Ambulatory deficit (use of any ambulatory device)" = list(
      points = "+3", definition = "Use of any ambulatory assistive device (cane, walker, wheelchair, etc.)",
      omop_concept = "4012645, 437643, 439405, 36714126 + descendants [vocab query]",
      derivation = "observation_occurrence with ambulatory device concepts within 365 days before index"
    ),
    "Tissue loss (CLI indication — wound ulcer or gangrene)" = list(
      points = "+3", definition = "Critical limb ischaemia indication: wound, ulcer, or gangrene",
      omop_concept = "4029926, 4291464 + descendants [vocab query]; see catalog entry tissue_loss",
      derivation = "condition_occurrence within 365 days before or on the index amputation date"
    ),
    "Anemia (Hgb less than 10 g/dL)" = list(
      points = "+2", definition = "Pre-operative anemia: hemoglobin < 10 g/dL",
      omop_concept = "3000963 (LOINC 718-7, Hgb) [vocab query]; see covariate_concepts.csv :: anemia",
      derivation = "Most recent Hgb in measurement table within 365 days before index; g/L values converted"
    ),
    "Insulin-dependent diabetes mellitus" = list(
      points = "+2", definition = "Insulin use as proxy for insulin-dependent diabetes mellitus",
      omop_concept = "21600713 (ATC: Insulins and analogues) + descendants [vocab query]",
      derivation = "drug_exposure with qualifying insulin ancestor within 365 days before index"
    )
  )
  
  combined_data <- data.frame(
    Component = character(),
    Points = character(),
    Definition = character(),
    OMOP_Concept = character(),
    Count = integer(),
    Total = integer(),
    Prevalence = character(),
    stringsAsFactors = FALSE
  )
  
  for (i in seq_len(nrow(covariate_summary_df))) {
    cov_name <- covariate_summary_df$covariate_name[i]
    cov_def <- covariate_defs[[cov_name]]

    if (is.null(cov_def)) {
      cov_def <- list(
        points = "—", definition = cov_name, omop_concept = "—", derivation = "—"
      )
    }

    combined_data <- rbind(combined_data, data.frame(
      Component = cov_name,
      Points = cov_def$points,
      Definition = cov_def$definition,
      OMOP_Concept = cov_def$omop_concept,
      Count = covariate_summary_df$n_positive[i],
      Total = covariate_summary_df$n_total[i],
      Prevalence = paste0(
        round(100 * covariate_summary_df$n_positive[i] / covariate_summary_df$n_total[i], 1), "%"
      ),
      stringsAsFactors = FALSE
    ))
  }
  
  ft <- flextable(combined_data) |>
    set_header_labels(
      Component = "Component",
      Points = "Points",
      Definition = "Definition",
      OMOP_Concept = "OMOP Standard Concept ID(s)",
      Count = "Count",
      Total = "Total",
      Prevalence = "Prevalence %"
    ) |>
    bold(part = "header") |>
    fontsize(size = 9, part = "all") |>
    font(fontname = "Calibri", part = "all") |>
    width(j = "Component", width = 1.8) |>
    width(j = "Points", width = 0.5) |>
    width(j = "Definition", width = 1.8) |>
    width(j = "OMOP_Concept", width = 1.8) |>
    width(j = "Count", width = 0.6) |>
    width(j = "Total", width = 0.6) |>
    width(j = "Prevalence", width = 0.8) |>
    align(j = c("Points", "Count", "Total", "Prevalence"), align = "center", part = "all") |>
    bg(part = "header", bg = "#1F3864") |>
    color(part = "header", color = "white") |>
    hline(border = border_h, part = "body") |>
    border_outer(border = border_out, part = "all") |>
    set_table_properties(layout = "fixed") |>
    padding(padding = 3, part = "all")
  
  ft
}


# -----------------------------------------------------------------------------
# .build_mfi5_component_table()
#
# Builds a flextable for Table 3a: Subramaniam 2018 mFI-5 components with
# point values, OMOP definitions, and observed activation in the cohort.
# Input: covariate_summary dataframe (covariate_name, n_positive, n_total)
#        from the mFI-5 pipeline run.
# -----------------------------------------------------------------------------
.build_mfi5_component_table <- function(covariate_summary_df) {

  border_h   <- officer::fp_border(color = "#BFBFBF", width = 0.5)
  border_out <- officer::fp_border(color = "#1F3864", width = 1.5)

  # Component definitions: Subramaniam S et al. J Am Coll Surg 2018;226(2):173-181.
  # Each item = 1 point; mFI-5 = sum of positive items (range 0–5).
  covariate_defs <- list(
    "Dependent functional status (frailty / impaired mobility)" = list(
      points = "+1",
      definition = "Partial or total dependence for ADLs, frailty, or impaired mobility",
      omop_concept = "Concept 4086506 (Frailty, SNOMED 248279007) + 4306934 (Impaired mobility, SNOMED 82971005) + descendants [vocab query]",
      derivation = "observation_occurrence within 10 years before index (lookback_start_day = −3650)"
    ),
    "Diabetes mellitus (any type)" = list(
      points = "+1",
      definition = "Any type of diabetes mellitus",
      omop_concept = "Concept 201820 (Diabetes mellitus, SNOMED 73211009) + descendants [vocab query]",
      derivation = "condition_occurrence within 10 years before index"
    ),
    "Chronic obstructive pulmonary disease" = list(
      points = "+1",
      definition = "COPD (current-pneumonia sub-component of mFI-5 omitted — documented limitation in EHR data)",
      omop_concept = "Concept 255573 (COPD, SNOMED 13645005) + descendants [vocab query]",
      derivation = "condition_occurrence within 10 years before index"
    ),
    "Congestive heart failure (within 30 days prior)" = list(
      points = "+1",
      definition = "Congestive heart failure within 30 days before the index amputation date",
      omop_concept = "Concept 316139 (Heart failure, SNOMED 84114007) + descendants [vocab query]",
      derivation = "condition_occurrence within 30 days before index (lookback_start_day = −30)"
    ),
    "Hypertension" = list(
      points = "+1",
      definition = "Hypertensive disorder (medication requirement not enforced — documented limitation)",
      omop_concept = "Concept 316866 (Hypertensive disorder, SNOMED 38341003) + descendants [vocab query]",
      derivation = "condition_occurrence within 10 years before index"
    )
  )

  combined_data <- data.frame(
    Component    = character(),
    Points       = character(),
    Definition   = character(),
    OMOP_Concept = character(),
    Count        = integer(),
    Total        = integer(),
    Prevalence   = character(),
    stringsAsFactors = FALSE
  )

  for (i in seq_len(nrow(covariate_summary_df))) {
    cov_name <- covariate_summary_df$covariate_name[i]
    cov_def  <- covariate_defs[[cov_name]]
    if (is.null(cov_def)) {
      cov_def <- list(points = "—", definition = cov_name, omop_concept = "—", derivation = "—")
    }
    combined_data <- rbind(combined_data, data.frame(
      Component    = cov_name,
      Points       = cov_def$points,
      Definition   = cov_def$definition,
      OMOP_Concept = cov_def$omop_concept,
      Count        = covariate_summary_df$n_positive[i],
      Total        = covariate_summary_df$n_total[i],
      Prevalence   = paste0(
        round(100 * covariate_summary_df$n_positive[i] / covariate_summary_df$n_total[i], 1), "%"
      ),
      stringsAsFactors = FALSE
    ))
  }

  ft <- flextable(combined_data) |>
    set_header_labels(
      Component    = "Component",
      Points       = "Points",
      Definition   = "Definition",
      OMOP_Concept = "OMOP Standard Concept ID(s)",
      Count        = "Count",
      Total        = "Total",
      Prevalence   = "Prevalence %"
    ) |>
    bold(part = "header") |>
    fontsize(size = 9, part = "all") |>
    font(fontname = "Calibri", part = "all") |>
    width(j = "Component",    width = 1.8) |>
    width(j = "Points",       width = 0.5) |>
    width(j = "Definition",   width = 1.8) |>
    width(j = "OMOP_Concept", width = 1.8) |>
    width(j = "Count",        width = 0.6) |>
    width(j = "Total",        width = 0.6) |>
    width(j = "Prevalence",   width = 0.8) |>
    align(j = c("Points", "Count", "Total", "Prevalence"), align = "center", part = "all") |>
    bg(part = "header", bg = "#1F3864") |>
    color(part = "header", color = "white") |>
    hline(border = border_h, part = "body") |>
    border_outer(border = border_out, part = "all") |>
    set_table_properties(layout = "fixed") |>
    padding(padding = 3, part = "all")

  ft
}

# -----------------------------------------------------------------------------
# .build_vqifs_component_table()
#
# Builds a flextable for Table 3c: Kraiss 2022 sVQI-FS components with
# point values, OMOP definitions, and observed activation in the cohort.
# Input: covariate_summary dataframe (covariate_name, n_positive, n_total)
#        from the sVQI-FS pipeline run.
# -----------------------------------------------------------------------------
.build_vqifs_component_table <- function(covariate_summary_df) {

  border_h   <- officer::fp_border(color = "#BFBFBF", width = 0.5)
  border_out <- officer::fp_border(color = "#1F3864", width = 1.5)

  # Component definitions: Kraiss LW et al. J Vasc Surg 2022;76:1325-34.
  # Each item = 1 point; sVQI-FS (implemented) = sum of positive items (range 0–10).
  # 10 of the paper's 11 items — non-home residence omitted (see covariates_vqifs.csv header).
  covariate_defs <- list(
    "Hypertension" = list(
      points = "+1",
      definition = "Hypertensive disorder",
      omop_concept = "Concept 316866 (Hypertensive disorder, SNOMED 38341003) + descendants [vocab query]",
      derivation = "condition_occurrence within 10 years before index (lookback_start_day = −3650)"
    ),
    "Congestive heart failure" = list(
      points = "+1",
      definition = "Congestive heart failure",
      omop_concept = "Concept 316139 (Heart failure, SNOMED 84114007) + descendants [vocab query]",
      derivation = "condition_occurrence within 10 years before index"
    ),
    "Coronary artery disease" = list(
      points = "+1",
      definition = "Coronary artery disease",
      omop_concept = "Concept 4185932 (Coronary arteriosclerosis) + descendants [vocab query]",
      derivation = "condition_occurrence within 10 years before index"
    ),
    "Peripheral vascular disease" = list(
      points = "+1",
      definition = "Peripheral arterial occlusive disease (diagnosis proxy; paper uses ABI <0.7 or prior arterial intervention/amputation)",
      omop_concept = "Concept 317309 (Peripheral arterial occlusive disease, SNOMED 399957001) + descendants [vocab query]",
      derivation = "condition_occurrence within 10 years before index"
    ),
    "Diabetes mellitus (any type)" = list(
      points = "+1",
      definition = "Any type of diabetes mellitus",
      omop_concept = "Concept 201820 (Diabetes mellitus, SNOMED 73211009) + descendants [vocab query]",
      derivation = "condition_occurrence within 10 years before index"
    ),
    "Chronic obstructive pulmonary disease" = list(
      points = "+1",
      definition = "COPD",
      omop_concept = "Concept 255573 (COPD, SNOMED 13645005) + descendants [vocab query]",
      derivation = "condition_occurrence within 10 years before index"
    ),
    # RENAL IMPAIRMENT — key updated 2026-07-26 to match the covariate_name in
    # covariates_vqifs.csv after the definition itself changed. The dict key
    # is looked up by exact covariate_name match below (cov_def <-
    # covariate_defs[[cov_name]]); leaving the OLD key here after
    # covariates_vqifs.csv was updated would have silently fallen through to
    # the generic "—" placeholder for every field, since a missed lookup
    # returns NULL rather than an error.
    "Renal impairment (creatinine > 1.8 mg/dL or dialysis)" = list(
      points = "+1",
      definition = "Creatinine > 1.8 mg/dL or dialysis, the paper's own lab-value definition (no longer a diagnosis proxy — CORRECTED 2026-07-26 after the prior CKD-diagnosis proxy activated in 224/225 patients, 99.6%, due to an over-broad concept-ancestor expansion)",
      omop_concept = "Concept 3016723 (Creatinine [Mass/volume] in Serum or Plasma, LOINC) thresholded at >1.8 mg/dL, OR Concepts 4146536 (Renal dialysis) / 4032243 (Dialysis procedure) [vocab query]",
      derivation = "query_renal_impairment_covariate_counts() in R/risk_score_pipeline.R: measurement value threshold OR procedure/condition dialysis presence, within 10 years before index"
    ),
    # ANEMIA — key updated 2026-07-26, same reason as renal impairment above.
    "Anemia (sex-specific Hgb threshold: <13 g/dL M / <12 g/dL F)" = list(
      points = "+1",
      definition = "Hgb < 13 g/dL (male) / < 12 g/dL (female), the paper's own sex-specific lab threshold (no longer a diagnosis proxy — CORRECTED 2026-07-26 after the prior anemia-diagnosis proxy activated in only 3/225 patients, 1.3%, under-capturing relative to the lab threshold)",
      omop_concept = "Concept 3000963 (Hemoglobin [Mass/volume] in Blood, LOINC 718-7) — the same measurement Iannuzzi's single-threshold anemia component uses [vocab query]",
      derivation = "query_anemia_covariate_counts(sex_specific = TRUE) in R/risk_score_pipeline.R: measurement value threshold, sex resolved from person.gender_concept_id, within 10 years before index; missing_is_negative = FALSE (unmeasured Hgb is unknown, not \"not anaemic\")"
    ),
    "Underweight (BMI < 18.5)" = list(
      points = "+1",
      definition = "BMI < 18.5, the paper's own cut-point (direct measurement preferred; weight/height fallback)",
      omop_concept = "Concepts 3038553, 36304833 (BMI direct); 3025315, 3013762, 3011054, 3026600 (weight); 3036277, 3023540, 3015514 (height) [vocab query]",
      derivation = "measurement within 10 years before index; missing_is_negative = FALSE (unmeasured BMI is unknown, not \"not underweight\")"
    ),
    "Non-ambulatory / impaired ambulation status" = list(
      points = "+1",
      definition = "Impaired ambulation, walking aid use, or gait abnormality",
      omop_concept = paste0(
        "Concepts 4012645, 437643, 439405, 36714126 (Observation/Clinical Finding) + ",
        "4085915 (Provision of long cane, Procedure) + 4044714 (Using wheelchair, Observation) + ",
        "4240470 (Wheelchair, Device) [vocab query]; see phenotype_library/catalog.yaml :: ",
        "ambulatory_deficit for per-concept domain notes"
      ),
      derivation = paste0(
        "query_auto_domain_covariate_counts() in R/risk_score_pipeline.R (domain = \"auto\" in ",
        "covariates_vqifs.csv — CHANGED 2026-07-26 from \"observation\", which found 0/225 ",
        "patients despite qualifying Synthea events, because 3 of the 7 concepts above resolve ",
        "to Procedure/Device rather than Observation in this vocabulary build): unions across ",
        "every clinical-event domain table, within 10 years before index"
      )
    )
  )

  combined_data <- data.frame(
    Component    = character(),
    Points       = character(),
    Definition   = character(),
    OMOP_Concept = character(),
    Count        = integer(),
    Total        = integer(),
    Prevalence   = character(),
    stringsAsFactors = FALSE
  )

  for (i in seq_len(nrow(covariate_summary_df))) {
    cov_name <- covariate_summary_df$covariate_name[i]
    cov_def  <- covariate_defs[[cov_name]]
    if (is.null(cov_def)) {
      cov_def <- list(points = "—", definition = cov_name, omop_concept = "—", derivation = "—")
    }
    combined_data <- rbind(combined_data, data.frame(
      Component    = cov_name,
      Points       = cov_def$points,
      Definition   = cov_def$definition,
      OMOP_Concept = cov_def$omop_concept,
      Count        = covariate_summary_df$n_positive[i],
      Total        = covariate_summary_df$n_total[i],
      Prevalence   = paste0(
        round(100 * covariate_summary_df$n_positive[i] / covariate_summary_df$n_total[i], 1), "%"
      ),
      stringsAsFactors = FALSE
    ))
  }

  ft <- flextable(combined_data) |>
    set_header_labels(
      Component    = "Component",
      Points       = "Points",
      Definition   = "Definition",
      OMOP_Concept = "OMOP Standard Concept ID(s)",
      Count        = "Count",
      Total        = "Total",
      Prevalence   = "Prevalence %"
    ) |>
    bold(part = "header") |>
    fontsize(size = 9, part = "all") |>
    font(fontname = "Calibri", part = "all") |>
    width(j = "Component",    width = 1.8) |>
    width(j = "Points",       width = 0.5) |>
    width(j = "Definition",   width = 1.8) |>
    width(j = "OMOP_Concept", width = 1.8) |>
    width(j = "Count",        width = 0.6) |>
    width(j = "Total",        width = 0.6) |>
    width(j = "Prevalence",   width = 0.8) |>
    align(j = c("Points", "Count", "Total", "Prevalence"), align = "center", part = "all") |>
    bg(part = "header", bg = "#1F3864") |>
    color(part = "header", color = "white") |>
    hline(border = border_h, part = "body") |>
    border_outer(border = border_out, part = "all") |>
    set_table_properties(layout = "fixed") |>
    padding(padding = 3, part = "all")

  ft
}


# -----------------------------------------------------------------------------
# .save_nhd_rate_by_year_plot()
#
# Stacked area chart of NHD rate (%) by procedure year with each non-home
# discharge disposition type (SNF, IRF, Hospice, LTAC, Other NHD) as a
# distinct filled band. Requires discharge_type column in person_level_df
# (populated by fetch_discharge_types_from_omop before this is called).
# Returns the output file path, or NULL if the plot cannot be generated.
# -----------------------------------------------------------------------------
.save_nhd_rate_by_year_plot <- function(person_level_df, output_folder) {
  if (!all(c("index_date", "outcome", "discharge_type") %in% names(person_level_df))) {
    message("[report] NHD-by-year plot skipped: required columns missing (index_date, outcome, discharge_type).")
    return(NULL)
  }

  year_val <- tryCatch(
    as.integer(format(as.Date(person_level_df$index_date), "%Y")),
    error = function(e) NA_integer_
  )

  nhd_levels <- c("SNF", "IRF", "Hospice", "LTAC", "Other NHD")
  fill_colours <- c(
    "SNF"       = "#5B8DB8",
    "IRF"       = "#2E7D5E",
    "Hospice"   = "#C0392B",
    "LTAC"      = "#8E44AD",
    "Other NHD" = "#BDC3C7"
  )

  df_yr <- data.frame(
    year           = year_val,
    outcome        = as.integer(person_level_df$outcome),
    discharge_type = as.character(person_level_df$discharge_type),
    stringsAsFactors = FALSE
  )
  df_yr <- df_yr[!is.na(df_yr$year), ]

  all_years <- sort(unique(df_yr$year))
  if (length(all_years) < 2) {
    message("[report] NHD-by-year plot skipped: fewer than 2 years of data.")
    return(NULL)
  }

  yr_type_tbl <- do.call(rbind, lapply(all_years, function(y) {
    sub_yr <- df_yr[df_yr$year == y, ]
    n_yr   <- nrow(sub_yr)
    if (n_yr < 11L) return(NULL)
    do.call(rbind, lapply(nhd_levels, function(tp) {
      events <- sum(sub_yr$discharge_type == tp, na.rm = TRUE)
      data.frame(
        year           = y,
        n              = n_yr,
        discharge_type = tp,
        events         = events,
        nhd_rate       = 100 * events / n_yr,
        stringsAsFactors = FALSE
      )
    }))
  }))

  if (is.null(yr_type_tbl) || nrow(yr_type_tbl) == 0) {
    message("[report] NHD-by-year plot skipped: no year with >= 11 procedures.")
    return(NULL)
  }

  # Drop disposition categories with ZERO events across every year before
  # plotting (added 2026-07-26) — see the identical fix and rationale in the
  # sibling NHD-by-year function above this one in this file.
  present_levels <- nhd_levels[
    vapply(nhd_levels, function(tp) sum(yr_type_tbl$events[yr_type_tbl$discharge_type == tp]) > 0,
           logical(1))
  ]
  if (length(present_levels) == 0) present_levels <- nhd_levels
  yr_type_tbl <- yr_type_tbl[yr_type_tbl$discharge_type %in% present_levels, ]
  yr_type_tbl$discharge_type <- factor(yr_type_tbl$discharge_type, levels = present_levels)
  total_procs <- sum(yr_type_tbl$n[!duplicated(yr_type_tbl$year)])
  total_years <- length(unique(yr_type_tbl$year))

  p <- ggplot2::ggplot(
      yr_type_tbl,
      ggplot2::aes(x = year, y = nhd_rate,
                   fill = discharge_type, colour = discharge_type,
                   group = discharge_type)
    ) +
    ggplot2::geom_area(position = "stack", alpha = 0.45, linewidth = 0.3) +
    ggplot2::geom_line(
      ggplot2::aes(y = nhd_rate),
      position = ggplot2::position_stack(),
      linewidth = 1.0
    ) +
    ggplot2::geom_point(
      position = ggplot2::position_stack(),
      size = 2.5, shape = 21, fill = "white", stroke = 1.2
    ) +
    ggplot2::scale_fill_manual(values = fill_colours, breaks = present_levels,
                               name = "Discharge disposition") +
    ggplot2::scale_colour_manual(values = fill_colours, breaks = present_levels,
                                 name = "Discharge disposition") +
    ggplot2::scale_x_continuous(breaks = sort(unique(yr_type_tbl$year))) +
    ggplot2::scale_y_continuous(limits = c(0, NA),
                                labels = function(x) paste0(round(x, 1), "%")) +
    ggplot2::labs(
      title    = "Non-Home Discharge Rate by Disposition Type and Amputation Year",
      subtitle = "Top line = overall NHD rate; bands show contribution of each disposition",
      x        = "Year of index amputation",
      y        = "NHD rate (%)",
      caption  = paste0("N = ", total_procs, " patients across ", total_years,
                        " years; years with < 11 patients suppressed.")
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      axis.text.x      = ggplot2::element_text(angle = 45, hjust = 1),
      plot.caption     = ggplot2::element_text(size = 8),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position  = "bottom"
    )

  out_file <- file.path(output_folder, "nhd_rate_by_year.png")
  tryCatch({
    ggplot2::ggsave(out_file, p, width = 7, height = 5, dpi = 150)
    out_file
  }, error = function(e) {
    message("[report] Could not save NHD-by-year plot: ", conditionMessage(e))
    NULL
  })
}

# -----------------------------------------------------------------------------
# .save_nhd_rate_by_month_plot()
#
# Bar chart of 90-day outcome rate (%) by calendar month (Jan-Dec), pooled across
# all years.  Used for integer score_type studies.
# -----------------------------------------------------------------------------
.save_nhd_rate_by_month_plot <- function(person_level_df, output_folder) {
  if (!all(c("index_date", "outcome") %in% names(person_level_df))) {
    message("[report] outcome-by-month plot skipped: index_date or outcome column missing.")
    return(NULL)
  }

  month_val <- tryCatch(
    as.integer(format(as.Date(person_level_df$index_date), "%m")),
    error = function(e) NA_integer_
  )

  df_mo <- data.frame(
    month   = month_val,
    outcome = as.integer(person_level_df$outcome),
    stringsAsFactors = FALSE
  )
  df_mo <- df_mo[!is.na(df_mo$month), ]

  # Aggregate across all months 1–12 (keep all so x-axis is always Jan–Dec).
  # 95% CI via Wilson score interval (stable when events = 0 or n = events).
  mo_tbl <- do.call(rbind, lapply(1:12, function(m) {
    sub    <- df_mo[df_mo$month == m, ]
    n      <- nrow(sub)
    events <- sum(sub$outcome, na.rm = TRUE)
    if (n >= 5) {
      rate   <- 100 * events / n
      ci_obj <- tryCatch(
        prop.test(events, n, conf.level = 0.95, correct = FALSE)$conf.int,
        error = function(e) c(NA_real_, NA_real_)
      )
      ci_lo  <- 100 * ci_obj[1]
      ci_hi  <- 100 * ci_obj[2]
    } else {
      rate   <- NA_real_
      ci_lo  <- NA_real_
      ci_hi  <- NA_real_
    }
    data.frame(
      month    = m,
      n        = n,
      events   = events,
      ssi_rate = rate,
      ci_lo    = ci_lo,
      ci_hi    = ci_hi,
      stringsAsFactors = FALSE
    )
  }))

  mo_tbl$month_label <- factor(
    mo_tbl$month,
    levels = 1:12,
    labels = c("Jan","Feb","Mar","Apr","May","Jun",
               "Jul","Aug","Sep","Oct","Nov","Dec")
  )

  if (all(is.na(mo_tbl$ssi_rate))) {
    message("[report] NHD-by-month plot skipped: all months suppressed (< 5 procedures each).")
    return(NULL)
  }

  p <- ggplot2::ggplot(mo_tbl, ggplot2::aes(x = month_label, y = ssi_rate)) +
    ggplot2::geom_col(fill = "#1F3864", width = 0.7, na.rm = TRUE) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = ci_lo, ymax = ci_hi),
      width = 0.25, colour = "grey30", na.rm = TRUE
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = ifelse(!is.na(ssi_rate), paste0("n=", n), "")),
      vjust = -0.4, size = 2.8, colour = "grey30", na.rm = TRUE
    ) +
    ggplot2::scale_y_continuous(
      limits = c(0, NA),
      expand = ggplot2::expansion(mult = c(0, 0.15)),
      labels = function(x) paste0(round(x, 1), "%")
    ) +
    ggplot2::labs(
      title   = "NHD Rate by Month of Amputation",
      x       = "Month of index amputation",
      y       = "NHD rate (%)",
      caption = paste0("Pooled across all study years. ",
                       "Error bars = 95% Wilson CI. ",
                       "Months with < 5 patients suppressed.")
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      plot.caption     = ggplot2::element_text(size = 8),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor   = ggplot2::element_blank()
    )

  out_file <- file.path(output_folder, "ssi_rate_by_month.png")
  tryCatch({
    ggplot2::ggsave(out_file, p, width = 7, height = 4.5, dpi = 150)
    out_file
  }, error = function(e) {
    message("[report] Could not save NHD-by-month plot: ", conditionMessage(e))
    NULL
  })
}


# ---------------------------------------------------------------------------
# LASSO model functions (from MACCE validation study)
# ---------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# .covariate_table_data_lasso()
#
# Reads included LASSO predictors from model/varImp.rds and returns a data frame
# for inclusion in the Word report as Table 2 (lasso score_type).
# Renamed from .covariate_table_data() in the original MACCE monolith.
# Now accepts config as first argument and reads config$var_imp_file instead of
# the hardcoded default path.
# -----------------------------------------------------------------------------
.covariate_table_data_lasso <- function(config, n_top = Inf) {
  varImp_path <- config$var_imp_file %||% "model/varImp.rds"
  if (!file.exists(varImp_path)) {
    stop("varImp.rds not found at: ", varImp_path,
         "\nSet varImp_path= or ensure the model artifact exists before generating the report.")
  }

  vi <- readRDS(varImp_path)

  # Retain only model-included covariates (included == 1 flags the LASSO-selected set).
  vi <- vi[!is.na(vi$included) & vi$included == 1, ]

  if (nrow(vi) == 0) {
    stop("No covariates with included == 1 found in ", varImp_path)
  }

  # Derive OMOP domain from analysisId encoding used by FeatureExtraction:
  #   2xx = condition_era, 4xx = drug_era, 9xx = measurement value,
  #   5xx = demographics,  7xx = visit,    other = miscellaneous
  get_domain <- function(aid) {
    ifelse(aid >= 200L & aid < 300L, "Condition",
    ifelse(aid >= 400L & aid < 500L, "Drug",
    ifelse(aid >= 900L & aid < 1000L, "Measurement",
    ifelse(aid >= 500L & aid < 600L, "Demographics",
    ifelse(aid >= 700L & aid < 800L, "Visit",
    "Other")))))
  }

  # Derive human-readable lookback window from analysisId.
  # Window codes follow FeatureExtraction default temporal analysis settings.
  get_lookback <- function(aid) {
    dplyr::case_when(
      aid %in% c(210L, 410L) ~ "365 to 3 days before index",
      aid %in% c(211L, 411L) ~ "180 to 3 days before index",
      aid %in% c(212L, 412L) ~ "30 to 3 days before index",
      aid %in% c(501L, 502L, 503L, 504L) ~ "At index (demographic)",
      aid %in% c(706L, 707L, 708L) ~ "365 days before index (visit)",
      aid %in% c(901L, 904L) ~ "Most recent value in 365 days before index",
      aid %in% c(998L, 999L) ~ "Aggregated risk score",
      TRUE ~ paste0("analysisId=", aid)
    )
  }

  vi$domain   <- get_domain(vi$analysisId)
  vi$lookback <- get_lookback(vi$analysisId)

  # Strip the FeatureExtraction time-window prefix from covariateName.
  # E.g. "condition_era group during day -365 through -3 days relative to index: Angina pectoris"
  # becomes "Angina pectoris".
  vi$variable <- sub("^[^:]+:\\s*", "", vi$covariateName)

  # Derivation note: OMOP CDM table and concept hierarchy source.
  vi$derivation <- ifelse(
    vi$domain == "Condition",
    paste0("Concept ", vi$conceptId, " + descendants in condition_occurrence (condition_era)."),
    ifelse(vi$domain == "Drug",
      paste0("Concept ", vi$conceptId, " + descendants in drug_exposure (drug_era)."),
      ifelse(vi$domain == "Measurement",
        paste0("Concept ", vi$conceptId, " in measurement; most recent value in lookback window."),
        paste0("FeatureExtraction analysisId=", vi$analysisId, ", conceptId=", vi$conceptId, ".")
      )
    )
  )

  # Sort by absolute coefficient value descending so the most predictive
  # covariates appear first.
  vi <- vi[order(-abs(vi$covariateValue)), ]

  # Trim to top n_top rows if requested.
  if (is.finite(n_top) && n_top < nrow(vi)) {
    vi <- vi[seq_len(as.integer(n_top)), ]
  }

  # Coerce covariateId to plain numeric so that merge() with covariate_summary
  # (read from CSV via read.csv(), which returns numeric) produces matches.
  # PLP RDS files store covariateId as bit64::integer64; read.csv() returns
  # numeric (double).  R's merge() treats these as different types and finds
  # zero intersections without explicit coercion.
  data.frame(
    covariate_id = as.numeric(vi$covariateId),
    variable     = vi$variable,
    weight       = vi$covariateValue,
    lookback     = vi$lookback,
    domain       = vi$domain,
    derivation   = vi$derivation,
    stringsAsFactors = FALSE
  )
}

# -----------------------------------------------------------------------------
# .build_combined_covariate_table()
#
# Builds a combined covariate table for LASSO score_type studies (PLP mode).
# Shows top predictors by |coefficient| with prevalence counts.
# Copied verbatim from pad_oler_macce_val/R/report_extended.R.
# -----------------------------------------------------------------------------
.build_combined_covariate_table <- function(covariate_summary_df) {
  # Build combined covariate table with definitions and prevalence.
  # Input: covariate_summary dataframe.
  # PLP mode: detected when "feature_importance" column is present.
  #   Columns: Feature, Importance (LASSO weight), Count, Prevalence.
  # Integer-score mode: covariate_name, n_positive, n_total, no feature_importance.
  #   Columns: Component, Points, Definition, OMOP_Concept, Count, Total, Prevalence.

  border_h  <- officer::fp_border(color = "#BFBFBF", width = 0.5)
  border_out <- officer::fp_border(color = "#1F3864", width = 1.5)

  is_plp_mode <- "feature_importance" %in% names(covariate_summary_df)

  if (is_plp_mode) {
    # ---- PLP mode: show top features by |LASSO weight| with prevalence ----
    n_total <- if ("n_total" %in% names(covariate_summary_df))
                 covariate_summary_df$n_total[1] else NA_integer_

    # Strip FeatureExtraction time-window prefix from covariate name for display.
    # E.g. "condition_era group during day -365 through -3 days relative to index: Angina"
    # becomes "Angina".
    display_name <- sub("^[^:]+:\\s*", "", covariate_summary_df$covariate_name)

    plp_data <- data.frame(
      Feature      = display_name,
      Importance   = round(covariate_summary_df$feature_importance, 4),
      Count        = as.integer(covariate_summary_df$n_positive),
      Prevalence   = paste0(
        ifelse(!is.na(n_total) & n_total > 0,
               round(100 * covariate_summary_df$n_positive / n_total, 1),
               NA_real_), "%"),
      stringsAsFactors = FALSE
    )

    ft <- flextable::flextable(plp_data) |>
      flextable::set_header_labels(
        Feature    = "Feature (top predictors by |coefficient|)",
        Importance = "LASSO\nCoefficient",
        Count      = "Count\n(n positive)",
        Prevalence = "Prevalence %"
      ) |>
      flextable::bold(part = "header") |>
      flextable::fontsize(size = 9, part = "all") |>
      flextable::font(fontname = "Calibri", part = "all") |>
      flextable::width(j = "Feature",    width = 3.5) |>
      flextable::width(j = "Importance", width = 0.8) |>
      flextable::width(j = "Count",      width = 0.7) |>
      flextable::width(j = "Prevalence", width = 0.8) |>
      flextable::align(j = c("Importance", "Count", "Prevalence"),
                       align = "center", part = "all") |>
      flextable::bg(part = "header", bg = "#1F3864") |>
      flextable::color(part = "header", color = "white") |>
      flextable::hline(border = border_h, part = "body") |>
      flextable::border_outer(border = border_out, part = "all") |>
      flextable::set_table_properties(layout = "fixed") |>
      flextable::padding(padding = 3, part = "all")

    return(ft)
  }

  # ---- Integer-score mode: full covariate definition table ----

  # Map covariates to their definitions and derivation methods
  covariate_defs <- list(
    "Female sex" = list(
      points = "+1",
      definition = "Female gender",
      omop_concept = "Concept 8532 (Female)",
      derivation = "person.gender_concept_id matches concept 8532"
    ),
    "Overweight (BMI 25 to <30)" = list(
      points = "+1",
      definition = "BMI between 25 and <30 kg/m\u00b2",
      omop_concept = paste0("Direct BMI: 3038553, 36304833; ",
                            "Weight: 3025315, 3013762, 3011054, 3026600; ",
                            "Height: 3036277, 3023540, 3015514"),
      derivation = paste0("Direct BMI measurement preferred (LOINC 39156-5 / 59574-4); ",
                          "computed from weight/height as fallback. 25 \u2264 BMI < 30.")
    ),
    "Obese (BMI \u226530)" = list(
      points = "+3",
      definition = "BMI \u2265 30 kg/m\u00b2",
      omop_concept = paste0("Direct BMI: 3038553, 36304833; ",
                            "Weight: 3025315, 3013762, 3011054, 3026600; ",
                            "Height: 3036277, 3023540, 3015514"),
      derivation = paste0("Direct BMI measurement preferred (LOINC 39156-5 / 59574-4); ",
                          "computed from weight/height as fallback. BMI \u2265 30.")
    ),
    "Urgent / emergency case" = list(
      points = "+1",
      definition = "Urgent or emergency procedure",
      omop_concept = "Concepts 4158569, 4250892 + descendants",
      derivation = "Procedure types in procedure_occurrence or observation within 30 days"
    ),
    "Low ankle-brachial index (ABI ≤0.35)" = list(
      points = "+1",
      definition = "ABI ≤ 0.35",
      omop_concept = "Concepts 40489833, 46237026 (ABI measurement)",
      derivation = "ABI measurement value < 0.35 in measurement table"
    ),
    "Prior revascularization (any)" = list(
      points = "+1",
      definition = "Any prior lower-extremity revascularization procedure",
      omop_concept = "Concepts 4236706 + 4225375 + descendants",
      derivation = "Procedure_occurrence within 10-year lookback"
    ),
    "Prolonged antibiotic exposure" = list(
      points = "+2",
      definition = "Non-prophylactic antibiotic exposure >2 days",
      omop_concept = "Concept 21603553 (systemic antibiotic) + descendants",
      derivation = "drug_exposure duration > 2 days within 90 days before index date"
    ),
    "Operative time ≥4 hours" = list(
      points = "+1",
      definition = "Operative duration ≥ 240 minutes",
      omop_concept = "procedure_occurrence timestamps (procedure_start/end_datetime)",
      derivation = "DATEDIFF(MINUTE, start, end) > 240"
    ),
    "High modified Frailty Index (mFI)" = list(
      points = "+1",
      definition = "Modified Frailty Index > 0.25 (≥2 of 5 conditions)",
      omop_concept = "Concepts 201820, 255573, 316139, 316866, 4215267",
      derivation = "Condition_occurrence: diabetes, COPD, CHF, hypertension, functional impairment"
    ),
    "Indication: claudication" = list(
      points = "−1",
      definition = "Intermittent claudication as operative indication",
      omop_concept = "Concept 442774 + descendants",
      derivation = "Condition_occurrence within 365 days"
    )
  )
  
  combined_data <- data.frame(
    Component = character(),
    Points = character(),
    Definition = character(),
    OMOP_Concept = character(),
    Count = integer(),
    Total = integer(),
    Prevalence = character(),
    stringsAsFactors = FALSE
  )
  
  for (i in seq_len(nrow(covariate_summary_df))) {
    cov_name <- covariate_summary_df$covariate_name[i]
    cov_def <- covariate_defs[[cov_name]]

    if (is.null(cov_def)) {
      cov_def <- list(
        points = "—", definition = cov_name, omop_concept = "—", derivation = "—"
      )
    }

    combined_data <- rbind(combined_data, data.frame(
      Component = cov_name,
      Points = cov_def$points,
      Definition = cov_def$definition,
      OMOP_Concept = cov_def$omop_concept,
      Count = covariate_summary_df$n_positive[i],
      Total = covariate_summary_df$n_total[i],
      Prevalence = paste0(
        round(100 * covariate_summary_df$n_positive[i] / covariate_summary_df$n_total[i], 1), "%"
      ),
      stringsAsFactors = FALSE
    ))
  }
  
  ft <- flextable(combined_data) |>
    set_header_labels(
      Component = "Component",
      Points = "Points",
      Definition = "Definition",
      OMOP_Concept = "OMOP Standard Concept ID(s)",
      Count = "Count",
      Total = "Total",
      Prevalence = "Prevalence %"
    ) |>
    bold(part = "header") |>
    fontsize(size = 9, part = "all") |>
    font(fontname = "Calibri", part = "all") |>
    width(j = "Component", width = 1.8) |>
    width(j = "Points", width = 0.5) |>
    width(j = "Definition", width = 1.8) |>
    width(j = "OMOP_Concept", width = 1.8) |>
    width(j = "Count", width = 0.6) |>
    width(j = "Total", width = 0.6) |>
    width(j = "Prevalence", width = 0.8) |>
    align(j = c("Points", "Count", "Total", "Prevalence"), align = "center", part = "all") |>
    bg(part = "header", bg = "#1F3864") |>
    color(part = "header", color = "white") |>
    hline(border = border_h, part = "body") |>
    border_outer(border = border_out, part = "all") |>
    set_table_properties(layout = "fixed") |>
    padding(padding = 3, part = "all")
  
  ft
}

# -----------------------------------------------------------------------------
# .save_macce_rate_by_year_plot()
#
# Single-line trend of annual outcome rate (%) by procedure year.
# Used for lasso score_type studies (e.g. MACCE outcome).
# Copied verbatim from pad_oler_macce_val/R/report_extended.R.
# -----------------------------------------------------------------------------
.save_macce_rate_by_year_plot <- function(person_level_df, output_folder) {
  if (!all(c("index_date", "outcome") %in% names(person_level_df))) {
    message("[report] MACCE-by-year plot skipped: index_date or outcome column missing.")
    return(NULL)
  }

  year_val <- tryCatch(
    as.integer(format(as.Date(person_level_df$index_date), "%Y")),
    error = function(e) NA_integer_
  )

  df_yr <- data.frame(
    year    = year_val,
    outcome = as.integer(person_level_df$outcome),
    stringsAsFactors = FALSE
  )
  df_yr <- df_yr[!is.na(df_yr$year), ]

  all_years <- sort(unique(df_yr$year))
  yr_tbl <- do.call(rbind, lapply(all_years, function(y) {
    sub_yr <- df_yr[df_yr$year == y, ]
    n_yr   <- nrow(sub_yr)
    events <- sum(sub_yr$outcome, na.rm = TRUE)
    data.frame(
      year       = y,
      n          = n_yr,
      events     = events,
      macce_rate = if (n_yr >= 10L) 100 * events / n_yr else NA_real_,
      stringsAsFactors = FALSE
    )
  }))

  yr_tbl <- yr_tbl[!is.na(yr_tbl$macce_rate), ]

  if (nrow(yr_tbl) < 2L) {
    message("[report] MACCE-by-year plot skipped: fewer than 2 years with >= 10 procedures.")
    return(NULL)
  }

  total_procs <- sum(yr_tbl$n)
  total_years <- nrow(yr_tbl)

  p <- ggplot2::ggplot(yr_tbl, ggplot2::aes(x = year, y = macce_rate)) +
    ggplot2::geom_line(colour = "#1F3864", linewidth = 0.9) +
    ggplot2::geom_point(colour = "#1F3864", size = 2.5, shape = 21,
                        fill = "white", stroke = 1.2) +
    ggplot2::scale_x_continuous(breaks = yr_tbl$year) +
    ggplot2::scale_y_continuous(
      limits = c(0, NA),
      labels = function(x) paste0(round(x, 1), "%")
    ) +
    ggplot2::labs(
      title   = "90-Day MACCE Rate by Procedure Year",
      x       = "Year of procedure",
      y       = "30-day MACCE rate (%)",
      caption = paste0("N = ", total_procs, " procedures across ", total_years,
                       " years; years with < 10 procedures suppressed.")
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      axis.text.x      = ggplot2::element_text(angle = 45, hjust = 1),
      plot.caption     = ggplot2::element_text(size = 8),
      panel.grid.minor = ggplot2::element_blank()
    )

  out_file <- file.path(output_folder, "macce_rate_by_year.png")
  tryCatch({
    ggplot2::ggsave(out_file, p, width = 7, height = 5, dpi = 150)
    out_file
  }, error = function(e) {
    message("[report] Could not save MACCE-by-year plot: ", conditionMessage(e))
    NULL
  })
}

# -----------------------------------------------------------------------------
# .save_macce_rate_by_month_plot()
#
# Bar chart of monthly outcome rate (%) by calendar month.
# Used for lasso score_type studies.
# Copied verbatim from pad_oler_macce_val/R/report_extended.R.
# -----------------------------------------------------------------------------
.save_macce_rate_by_month_plot <- function(person_level_df, output_folder) {
  if (!all(c("index_date", "outcome") %in% names(person_level_df))) {
    message("[report] MACCE-by-month plot skipped: index_date or outcome column missing.")
    return(NULL)
  }

  month_val <- tryCatch(
    as.integer(format(as.Date(person_level_df$index_date), "%m")),
    error = function(e) NA_integer_
  )

  df_mo <- data.frame(
    month   = month_val,
    outcome = as.integer(person_level_df$outcome),
    stringsAsFactors = FALSE
  )
  df_mo <- df_mo[!is.na(df_mo$month), ]

  mo_tbl <- do.call(rbind, lapply(1:12, function(m) {
    sub    <- df_mo[df_mo$month == m, ]
    n      <- nrow(sub)
    events <- sum(sub$outcome, na.rm = TRUE)
    data.frame(
      month      = m,
      n          = n,
      events     = events,
      macce_rate = if (n >= 5L) 100 * events / n else NA_real_,
      stringsAsFactors = FALSE
    )
  }))

  mo_tbl$month_label <- factor(
    mo_tbl$month,
    levels = 1:12,
    labels = c("Jan","Feb","Mar","Apr","May","Jun",
               "Jul","Aug","Sep","Oct","Nov","Dec")
  )

  if (all(is.na(mo_tbl$macce_rate))) {
    message("[report] MACCE-by-month plot skipped: all months suppressed (< 5 procedures each).")
    return(NULL)
  }

  p <- ggplot2::ggplot(mo_tbl, ggplot2::aes(x = month_label, y = macce_rate)) +
    ggplot2::geom_col(fill = "#1F3864", width = 0.7, na.rm = TRUE) +
    ggplot2::geom_text(
      ggplot2::aes(label = ifelse(!is.na(macce_rate), paste0("n=", n), "")),
      vjust = -0.4, size = 2.8, colour = "grey30", na.rm = TRUE
    ) +
    ggplot2::scale_y_continuous(
      limits = c(0, NA),
      expand = ggplot2::expansion(mult = c(0, 0.15)),
      labels = function(x) paste0(round(x, 1), "%")
    ) +
    ggplot2::labs(
      title   = "90-Day MACCE Rate by Month of Procedure",
      x       = "Month of index procedure",
      y       = "30-day MACCE rate (%)",
      caption = "Pooled across all study years. Months with < 5 procedures suppressed."
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      plot.caption       = ggplot2::element_text(size = 8),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor   = ggplot2::element_blank()
    )

  out_file <- file.path(output_folder, "macce_rate_by_month.png")
  tryCatch({
    ggplot2::ggsave(out_file, p, width = 7, height = 4.5, dpi = 150)
    out_file
  }, error = function(e) {
    message("[report] Could not save MACCE-by-month plot: ", conditionMessage(e))
    NULL
  })
}


# ===========================================================================
# .report_word_simple()
#
# Lightweight report that reads pipeline CSV outputs and assembles a Word
# document.  No live CDM queries.  Called by generate_word_report() in
# R/report_extended.R.
#
# Renamed from generate_word_report() in the original synthea-omop-template monolith.
# Body is verbatim — no parameterization applied to the simple report.
# ===========================================================================
.report_word_simple <- function(output_dir = "output/risk_score_eval",
                                 score_output_dir = "output/risk_score_eval") {

  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

  # Load pipeline outputs if available
  person_level <- NULL
  covariate_summary <- NULL
  metrics <- NULL
  calibration_plot_files <- list()
  roc_plot_file <- NULL

  if (file.exists(file.path(score_output_dir, "person_level_scores.csv"))) {
    person_level <- read.csv(file.path(score_output_dir, "person_level_scores.csv"), stringsAsFactors = FALSE)
  }
  if (file.exists(file.path(score_output_dir, "covariate_summary.csv"))) {
    covariate_summary <- read.csv(file.path(score_output_dir, "covariate_summary.csv"), stringsAsFactors = FALSE)
  }
  if (file.exists(file.path(score_output_dir, "metrics.csv"))) {
    metrics <- read.csv(file.path(score_output_dir, "metrics.csv"), stringsAsFactors = FALSE)
  }

  # Ensure calibration PNGs exist for report insertion by backfilling from CSV
  # tables (preferred) or person-level predictions (fallback).
  lookup_plot_path <- file.path(score_output_dir, "calibration_lookup.png")
  recal_plot_path <- file.path(score_output_dir, "calibration_recalibrated.png")
  lookup_table_path <- file.path(score_output_dir, "calibration_table_lookup.csv")
  recal_table_path <- file.path(score_output_dir, "calibration_table_recalibrated.csv")

  if (!file.exists(lookup_plot_path) && file.exists(lookup_table_path)) {
    .save_calibration_plot_from_table(
      calibration_table_path = lookup_table_path,
      output_folder = score_output_dir,
      file_name = "calibration_lookup.png",
      plot_title = "Calibration Plot: Lookup Model"
    )
  }
  if (!file.exists(recal_plot_path) && file.exists(recal_table_path)) {
    .save_calibration_plot_from_table(
      calibration_table_path = recal_table_path,
      output_folder = score_output_dir,
      file_name = "calibration_recalibrated.png",
      plot_title = "Calibration Plot: Recalibrated Model"
    )
  }

  if (!file.exists(lookup_plot_path) && !is.null(person_level) &&
      all(c("outcome", "predicted_risk_lookup") %in% names(person_level))) {
    .save_calibration_plot_from_vectors(
      y = person_level$outcome,
      p = person_level$predicted_risk_lookup,
      output_folder = score_output_dir,
      file_name = "calibration_lookup.png",
      plot_title = "Calibration Plot: Lookup Model"
    )
  }
  if (!file.exists(recal_plot_path) && !is.null(person_level) &&
      all(c("outcome", "predicted_risk_recalibrated") %in% names(person_level))) {
    .save_calibration_plot_from_vectors(
      y = person_level$outcome,
      p = person_level$predicted_risk_recalibrated,
      output_folder = score_output_dir,
      file_name = "calibration_recalibrated.png",
      plot_title = "Calibration Plot: Recalibrated Model"
    )
  }
  
  # Check for calibration plot files
  cal_files <- list.files(score_output_dir, pattern = "^calibration_.*\\.png$", full.names = TRUE)
  if (length(cal_files) > 0) {
    calibration_plot_files <- setNames(cal_files, 
                                         gsub(".*calibration_|\\.png$", "", cal_files))
  }
  
  # Generate ROC plot if we have the data
  if (!is.null(person_level)) {
    roc_plot_file <- .save_roc_plot(
      y = person_level$outcome,
      p = if ("predicted_risk_recalibrated" %in% names(person_level)) 
          person_level$predicted_risk_recalibrated
        else person_level$total_score / max(person_level$total_score, na.rm = TRUE),
      output_folder = score_output_dir
    )
  }

  doc <- read_docx()

  # ---- Title ---------------------------------------------------------------
  today_str <- format(Sys.Date(), "%B %d, %Y")
  doc <- body_add_par(doc,
    paste0("PAD / Major Lower-Extremity Amputation — Non-Home Discharge Risk Score"),
    style = "heading 1")
  doc <- body_add_par(doc,
    paste0("External Validation Report: Iannuzzi 2020 and Subramaniam mFI-5"),
    style = "heading 1")
  doc <- body_add_par(doc, paste("Report Generated:", today_str), style = "heading 2")
  doc <- body_add_par(doc, "", style = "Normal")

  # ---- 1. Executive Summary ------------------------------------------------
  doc <- body_add_par(doc, "1.  Executive Summary", style = "heading 2")
  
  if (!is.null(person_level)) {
    n_patients   <- length(unique(person_level$subject_id))
    n_procedures <- nrow(person_level)
    n_nhd_events <- sum(person_level$outcome, na.rm = TRUE)
    nhd_rate     <- round(100 * n_nhd_events / n_procedures, 1)
    mean_score   <- round(mean(person_level$total_score, na.rm = TRUE), 2)

    doc <- body_add_par(doc,
      paste0(
        "This study externally validated two published integer risk scores for non-home ",
        "discharge (NHD) following major lower-extremity amputation in patients with ",
        "peripheral arterial disease, diabetes mellitus, or a lower-extremity wound indication. ",
        "The scores evaluated were the Iannuzzi 2020 NHD integer score (9 components, 0–18 points) ",
        "and the Subramaniam 2018 modified Frailty Index — 5-item (mFI-5, 0–5 points). ",
        "The analysis was performed on ",
        config$cdm_database_name %||% "the validation database",
        " (OMOP CDM v5.4) containing ", n_patients, " unique patients with ",
        n_procedures, " eligible amputation encounters. ",
        "Overall NHD incidence was ", n_nhd_events, " events (", nhd_rate, "%). ",
        "The mean Iannuzzi integer score was ", mean_score, " points."
      ),
      style = "Normal"
    )
  } else {
    doc <- body_add_par(doc,
      paste0(
        "This study externally validated two published integer risk scores for non-home ",
        "discharge (NHD) following major lower-extremity amputation: the Iannuzzi 2020 ",
        "NHD integer score (9 components, 0–18 points) and the Subramaniam 2018 mFI-5 ",
        "(5 components, 0–5 points)."
      ),
      style = "Normal"
    )
  }
  
  doc <- body_add_par(doc, "", style = "Normal")

  # ---- 2. Methods ----------------------------------------------------------
  doc <- body_add_par(doc, "2.  Methods", style = "heading 2")

  # 2.1 Study Population and Data Source
  doc <- body_add_par(doc, "2.1  Study Population and Data Source", style = "heading 3")
  doc <- body_add_par(doc,
    paste0(
      "We conducted an external validation study of two published integer risk scores for ",
      "non-home discharge (NHD) following major lower-extremity amputation using the ",
      config$cdm_database_name %||% "validation", " database, which contains records mapped ",
      "to the Observational Medical Outcomes Partnership (OMOP) Common Data Model version 5.4. ",
      "The study period spanned ", config$study_start_date %||% "2017-01-01",
      " through ", config$study_end_date %||% "2025-12-31", "."
    ),
    style = "Normal"
  )
  doc <- body_add_par(doc,
    paste0(
      "The target cohort comprised adults aged 18 years or older who underwent an inpatient ",
      "major lower-extremity amputation. The index event was defined by six OMOP procedure ",
      "concept ancestors and all descendants via the concept_ancestor table: above-knee ",
      "amputation (concept 4195136), amputation through tibia and fibula / below-knee ",
      "(concept 4338257), through-knee amputation (concept 4143795), hip disarticulation ",
      "(concept 4242396), ankle disarticulation (concept 4264289), and hemipelvectomy ",
      "(concept 36675618). At least one of the following indications was required to be ",
      "documented on or before the index date: peripheral arterial disease (SNOMED 399957001 ",
      "and descendants), diabetes mellitus (SNOMED 73211009 and descendants), or a ",
      "lower-extremity wound (ulcers, open wounds, gangrene, soft-tissue infection, ",
      "osteomyelitis, or diabetic foot — 25 SNOMED ancestor concept IDs). These eligibility ",
      "criteria exclude traumatic, burn, and oncologic amputations. Patients were required to ",
      "have at least ", config$min_prior_observation_days %||% 365,
      " days of prior observation before the index date. ",
      "All patients were enrolled from inpatient visits (OMOP visit_concept_id 9201) only."
    ),
    style = "Normal"
  )
  doc <- body_add_par(doc,
    paste0(
      "The outcome was non-home discharge at the end of the index hospitalization, defined as ",
      "any discharge destination other than home (UB-04 codes 01/06 and CMS place-of-service ",
      "code 8536 = “home”). Discharge destination was ascertained from ",
      "visit_occurrence.discharged_to_concept_id using a dynamic vocabulary query resolved ",
      "at runtime. The outcome was attributed to the index visit when discharge occurred within ",
      config$prediction_window_days %||% 90,
      " days of the index date (prediction_window_days). The dataset contained ",
      if (!is.null(person_level)) length(unique(person_level$subject_id)) else "N",
      " patients with at least one qualifying amputation within the study window."
    ),
    style = "Normal"
  )

  # 2.2 Risk Score Computation
  doc <- body_add_par(doc, "2.2  Risk Score Computation", style = "heading 3")
  doc <- body_add_par(doc,
    paste0(
      "Two published integer risk scores were evaluated. ",
      "For each score, covariates were extracted from the OMOP CDM using component-specific ",
      "lookback windows relative to the index date. A patient meeting the minimum event ",
      "threshold for a given component received the full integer point value; those below ",
      "threshold received zero. Missing data were treated as zero evidence (absence of ",
      "component). The total score is the arithmetic sum of all component values."
    ),
    style = "Normal"
  )
  doc <- body_add_par(doc,
    paste0(
      "Iannuzzi 2020 non-home discharge integer risk score (score range 0–18 points). ",
      "This nine-component score was originally derived and internally validated in patients ",
      "undergoing open lower-extremity revascularisation for peripheral artery disease. ",
      "Components and point values are: age 60–69 years (+2), age 70–79 years (+4), ",
      "age ≥80 years (+6), female sex (+1; OMOP concept 8532), non-White race (+2; ",
      "concept 8527 [White] as reference), ambulatory deficit — use of any ambulatory ",
      "assistive device (+3; concepts 4012645, 437643, 439405, 36714126 and descendants), ",
      "tissue loss / critical limb ischaemia indication — wound, ulcer, or gangrene (+3; ",
      "concepts 4029926, 4291464 and descendants), anemia (hemoglobin <10 g/dL; +2; ",
      "LOINC 718-7, OMOP concept 3000963), and insulin-dependent diabetes mellitus (+2; ",
      "ATC insulins and analogues, OMOP concept 21600713 and descendants). The score is ",
      "mapped to predicted NHD risk probabilities via a refitted score-to-risk lookup table ",
      "(see Performance Evaluation section below for refitting rationale)."
    ),
    style = "Normal"
  )
  doc <- body_add_par(doc,
    paste0(
      "Subramaniam 2018 modified Frailty Index — 5-item (mFI-5; score range 0–5 points). ",
      "This five-component frailty index was derived in a large surgical registry (ACS NSQIP) ",
      "and assigns one point for each of: dependent functional status or impaired mobility ",
      "(OMOP concepts 4086506 and 4306934 and descendants), diabetes mellitus of any type ",
      "(concept 201820 and descendants), chronic obstructive pulmonary disease (concept ",
      "255573 and descendants), congestive heart failure within 30 days prior to the index ",
      "date (concept 316139 and descendants), and hypertension (concept 316866 and ",
      "descendants). No published NHD probability mapping exists for the mFI-5 in this ",
      "population; the raw integer score is used directly for discrimination analysis."
    ),
    style = "Normal"
  )

  # 2.3 Performance Evaluation
  doc <- body_add_par(doc, "2.3  Performance Evaluation", style = "heading 3")
  doc <- body_add_par(doc,
    paste0(
      "Three model specifications were evaluated. ",
      "(1) Iannuzzi 2020 — refitted lookup: the published Iannuzzi lookup table ",
      "(Iannuzzi et al. 2020, Table III) exhibits a non-monotonic inversion in its tail ",
      "(score 17 = 50%% NHD risk, below score 16 = 87.5%%), a small-sample artefact from ",
      "the derivation cohort. To ensure higher scores map to higher predicted risk, we refitted ",
      "the lookup as a monotone isotonic regression (using both derivation and validation ",
      "series) via stats::isoreg(). This produces a mathematically monotone curve while ",
      "incorporating all available observed rates. Predicted NHD probabilities are drawn from ",
      "this refitted table. ",
      "(2) Iannuzzi 2020 — temporal recalibration: a logistic regression of the total ",
      "integer score on the observed binary NHD outcome was fitted on the chronologically ",
      "earlier half of the cohort (training set) and all reported metrics were evaluated ",
      "on the later half (test set). This specification quantifies the improvement in ",
      "calibration obtainable by re-anchoring the score’s probability scale to the local ",
      "event rate. ",
      "(3) mFI-5 — raw integer score: because no published NHD probability mapping exists ",
      "for the mFI-5, discrimination was assessed from the raw score only; calibration ",
      "metrics are not applicable and are reported as N/A."
    ),
    style = "Normal"
  )
  doc <- body_add_par(doc,
    paste0(
      "Discrimination was quantified using the area under the receiver operating ",
      "characteristic curve (AUROC) computed with pROC and the area under the ",
      "precision-recall curve (AUPRC) computed with PRROC. Calibration was assessed ",
      "using four metrics: (a) Brier score (mean squared error of predicted probabilities); ",
      "(b) expected calibration error (ECE) — the probability-weighted mean absolute ",
      "difference between mean predicted probability and observed event rate across ",
      "10 equal-frequency bins; (c) calibration intercept; and (d) calibration slope, ",
      "derived from a logistic regression of the observed outcome on the log-odds of the ",
      "predicted probability. Perfect calibration corresponds to intercept = 0 and slope = 1. ",
      "All point estimates are accompanied by 95% bootstrap percentile confidence intervals ",
      "(B = 500 resamples)."
    ),
    style = "Normal"
  )
  doc <- body_add_par(doc,
    paste0(
      "Temporal train/test split. Patients were sorted chronologically by index date and ",
      "divided at the midpoint (floor(n / 2) training rows). The recalibration logistic ",
      "regression was fitted exclusively on the training set (earlier half) and all ",
      "discrimination and calibration metrics — for all three model specifications — ",
      "were evaluated on the test set (later half) only. This approach prevents optimistic ",
      "bias arising from evaluating a locally fitted model on the data used to fit it, and ",
      "provides an estimate of prospective performance on future patients from the same source."
    ),
    style = "Normal"
  )
  # Append model-specific split sample sizes when split_info.csv was found.
  for (si_pair in list(
    list(si = split_info_iannuzzi, label = "Iannuzzi 2020"),
    list(si = split_info_mfi5,     label = "mFI-5")
  )) {
    sent <- split_sentence(si_pair$si, si_pair$label)
    if (!is.null(sent)) {
      doc <- body_add_par(doc, sent, style = "Normal")
    }
  }
  doc <- body_add_par(doc, "", style = "Normal")

  # ---- 3. Table 1: Cohort Summary ------------------------------------------
  doc <- body_add_par(doc, "3.  Study Cohort Characteristics", style = "heading 2")
  
  if (!is.null(person_level)) {
    doc <- body_add_par(doc,
      paste0(
        "Table 1 presents baseline demographic and clinical characteristics of the validation cohort. ",
        "Variables are summarized across all eligible procedures (N = ", nrow(person_level), ")."
      ),
      style = "Normal"
    )
    doc <- body_add_par(doc,
      "Table 1.  Cohort characteristics (baseline demographics and clinical features).",
      style = "Normal"
    )
    
    cohort_summary_df <- .build_cohort_summary_table(person_level)
    doc <- body_add_flextable(doc, cohort_summary_df)
  }
  doc <- body_add_par(doc, "", style = "Normal")

  # ---- 4. Table 2: Risk Score Components -----------------------------------
  doc <- body_add_par(doc, "", style = "Normal")
  doc <- body_add_par(doc, "4.  Risk Model Variables", style = "heading 2")
  doc <- body_add_par(doc,
    paste0(
      "Table 2 lists the components of the integer risk score, the point value assigned to each, ",
      "the lookback window applied, and the OMOP concept-based derivation method used in this validation."
    ),
    style = "Normal"
  )
  doc <- body_add_par(doc,
    "Table 2.  Risk score components, point values, and OMOP CDM derivation method.",
    style = "Normal"
  )
  doc <- body_add_flextable(doc, .build_table1(.covariate_table_data()))
  doc <- body_add_par(doc, "", style = "Normal")

  # ---- Figure 1. ROC Curve (placed immediately after Tables 1-2) ----------
  if (!is.null(roc_plot_file) && file.exists(roc_plot_file)) {
    doc <- body_add_par(doc,
      "Figure 1.  Receiver operating characteristic (ROC) curve for non-home discharge (NHD) risk prediction.",
      style = "Normal"
    )
    doc <- body_add_img(doc, src = roc_plot_file, width = 5, height = 3.5)
    doc <- body_add_par(doc, "", style = "Normal")
  }

  # ---- 5. Table 3: Covariate Summary & Cohort Counts ----------------------
  if (!is.null(covariate_summary)) {
    doc <- body_add_par(doc, "", style = "Normal")
    doc <- body_add_par(doc, "", style = "Normal")
    doc <- body_add_par(doc, "5.  Covariate Prevalence in the Validation Cohort", style = "heading 2")
    doc <- body_add_par(doc,
      paste0(
        "Table 3 displays the prevalence of each risk score covariate in the validation cohort, ",
        "alongside the covariate definitions and OMOP concept derivation. ",
        "Covariate counts and prevalence percentages are computed across all eligible procedures."
      ),
      style = "Normal"
    )
    doc <- body_add_par(doc,
      "Table 3.  Risk score covariates with OMOP derivation and prevalence in the validation cohort.",
      style = "Normal"
    )

    # Build combined flextable for covariate summary with definitions
    combined_cov_df <- .build_combined_covariate_table(covariate_summary)
    doc <- body_add_flextable(doc, combined_cov_df)
    doc <- body_add_par(doc, "", style = "Normal")
  }

  # ---- 6. Results & Performance Metrics ------------------------------------
  if (!is.null(metrics) && nrow(metrics) > 0) {
    doc <- body_add_par(doc, "", style = "Normal")
    doc <- body_add_par(doc, "", style = "Normal")
    doc <- body_add_par(doc, "6.  Discrimination and Calibration Metrics", style = "heading 2")
    
    doc <- body_add_par(doc,
      paste0(
        "Table 4 presents the discrimination (AUROC, AUPRC, Brier score) and calibration ",
        "(calibration-in-the-large intercept and slope) metrics across three model specifications: ",
        "lookup-based score-to-risk table, and recalibrated logistic regression."
      ),
      style = "Normal"
    )
    doc <- body_add_par(doc,
      "Table 4.  Discrimination and calibration metrics.",
      style = "Normal"
    )
    
    # Build a formatted "value (95% CI)" string per metric-model cell, then
    # pivot wide so each model becomes one column.  Falls back to just the
    # point estimate when ci_lower / ci_upper are absent (legacy CSV format).
    has_ci <- all(c("ci_lower", "ci_upper") %in% names(metrics)) &&
              any(!is.na(metrics$ci_lower))

    fmt3 <- function(x) format(round(as.numeric(x), 3), nsmall = 3, trim = TRUE)

    metrics_disp <- metrics
    metrics_disp$cell <- if (has_ci) {
      ifelse(
        !is.na(metrics$ci_lower) & !is.na(metrics$ci_upper),
        paste0(fmt3(metrics$value),
               " (", fmt3(metrics$ci_lower), "\u2013", fmt3(metrics$ci_upper), ")"),
        fmt3(metrics$value)
      )
    } else {
      fmt3(metrics$value)
    }

    metrics_wide <- metrics_disp[, c("metric", "cell", "model")]
    metrics_wide <- reshape(metrics_wide, idvar = "metric", timevar = "model", direction = "wide")
    names(metrics_wide) <- gsub("cell\\.", "", names(metrics_wide))

    ft_metrics <- flextable(metrics_wide) |>
      bold(part = "header") |>
      fontsize(size = 10, part = "all") |>
      font(fontname = "Calibri", part = "all") |>
      bg(part = "header", bg = "#1F3864") |>
      color(part = "header", color = "white") |>
      align(j = seq_along(names(metrics_wide))[-1], align = "center", part = "all") |>
      set_header_labels(
        metric       = "Metric",
        score_only   = "Score only",
        lookup       = "Lookup (published)",
        recalibrated = "Recalibrated"
      )

    if (has_ci) {
      ft_metrics <- add_footer_lines(ft_metrics,
        "Values shown as point estimate (95% bootstrap percentile CI, B\u2009=\u2009500 resamples).")
      ft_metrics <- fontsize(ft_metrics, size = 8, part = "footer")
      ft_metrics <- font(ft_metrics, fontname = "Calibri", part = "footer")
    }

    doc <- body_add_flextable(doc, ft_metrics)
    doc <- body_add_par(doc, "", style = "Normal")
    
    # Interpretation
    auroc_lookup <- metrics$value[metrics$metric == "AUROC" & metrics$model == "lookup"]
    if (length(auroc_lookup) > 0 && !is.na(auroc_lookup)) {
      interp <- if (auroc_lookup > 0.8) "excellent" 
                else if (auroc_lookup > 0.7) "good" 
                else if (auroc_lookup > 0.6) "fair" 
                else "poor"
      doc <- body_add_par(doc,
        paste0(
          "The model demonstrates an AUROC of ", round(auroc_lookup, 3), 
          " when using lookup-based probabilities, indicating ", interp, 
          " discriminative ability."
        ),
        style = "Normal"
      )
    }
    doc <- body_add_par(doc, "", style = "Normal")
  }

  # ---- 8. Calibration Plots -----------------------------------------------
  if (length(calibration_plot_files) > 0) {
    doc <- body_add_par(doc, "", style = "Normal")
    doc <- body_add_par(doc, "", style = "Normal")
    doc <- body_add_par(doc, "8.  Calibration: Observed vs. Predicted Risk", style = "heading 2")
    doc <- body_add_par(doc,
      paste0(
        "Figures 2+ display calibration plots for the lookup-based and recalibrated model specifications. ",
        "The solid line represents perfect calibration (predicted = observed risk). Points above the line ",
        "indicate overprediction; points below indicate underprediction."
      ),
      style = "Normal"
    )
    
    fig_num <- 2
    for (model_name in names(calibration_plot_files)) {
      plot_file <- calibration_plot_files[[model_name]]
      if (file.exists(plot_file)) {
        cap <- paste0(
          "Figure ", fig_num, ".  Calibration plot (",
          gsub("_", " ", model_name), " model). Points represent deciles of predicted risk, ",
          "with error bars showing 95% confidence intervals around the observed event rate."
        )
        doc <- body_add_par(doc, cap, style = "Normal")
        doc <- body_add_img(doc, src = plot_file, width = 5, height = 3.5)
        doc <- body_add_par(doc, "", style = "Normal")
        fig_num <- fig_num + 1
      }
    }
  }

  # ---- 9. Expected Calibration Error (ECE) --------------------------------
  if (!is.null(person_level) && "predicted_risk_recalibrated" %in% names(person_level)) {
    doc <- body_add_par(doc, "", style = "Normal")
    doc <- body_add_par(doc, "", style = "Normal")
    doc <- body_add_par(doc, "9.  Expected Calibration Error", style = "heading 2")
    
    # Compute ECE
    y <- person_level$outcome
    p <- person_level$predicted_risk_recalibrated
    ece_result <- .compute_ece(y, p, n_bins = 10)
    ece_value <- ece_result$ece
    
    doc <- body_add_par(doc,
      paste0(
        "Expected calibration error (ECE) quantifies the average absolute difference between predicted ",
        "and observed risk probabilities across deciles of risk. For the recalibrated model, ECE = ",
        round(ece_value, 4), ", indicating ",
        if (ece_value < 0.05) "excellent" else if (ece_value < 0.10) "good" else "moderate",
        " calibration."
      ),
      style = "Normal"
    )
    doc <- body_add_par(doc, "", style = "Normal")
  }

  # ---- 10. Discussion & Conclusion -----------------------------------------
  doc <- body_add_par(doc, "", style = "Normal")
  doc <- body_add_par(doc, "", style = "Normal")
  doc <- body_add_par(doc, "10.  Discussion and Conclusion", style = "heading 2")
  doc <- body_add_par(doc,
    paste0(
      "This external validation demonstrates the applicability of the Iannuzzi 2020 NHD integer risk score ",
      "and the Subramaniam mFI-5 to an OMOP CDM v5.4 dataset. All score components were successfully mapped ",
      "to OMOP standard concept IDs using transparent, scriptable SQL against the concept_ancestor and concept ",
      "tables. The target cohort comprised adult patients undergoing inpatient major lower-extremity amputation, ",
      "and the non-home discharge (NHD) outcome was ascertained from visit_occurrence.discharged_to_concept_id ",
      "using a dynamic vocabulary query at runtime. Performance metrics indicate ",
      if (!is.null(metrics)) {
        auroc <- metrics$value[metrics$metric == "AUROC" & metrics$model == "lookup"]
        if (length(auroc) > 0 && !is.na(auroc[1])) {
          if (auroc[1] > 0.75) "promising discriminative and calibration properties"
          else if (auroc[1] > 0.60) "moderate discriminative and calibration properties"
          else "modest discriminative properties that warrant further investigation"
        } else "good performance"
      } else "reasonable",
      ", supporting the continued evaluation of these scores as perioperative clinical decision-support ",
      "tools for patients undergoing major lower-extremity amputation."
    ),
    style = "Normal"
  )

  doc <- body_add_par(doc, "", style = "Normal")
  doc <- body_add_par(doc,
    paste0(
      "The fully reproducible workflow — implemented as executable R scripts with SqlRender-parameterised ",
      "cohort SQL — enables validation teams to audit all cohort inclusion criteria, concept mappings, ",
      "lookback windows, and statistical calculations end-to-end. This transparency aligns with OHDSI ",
      "best practices for network studies and external validation. Future steps include applying this ",
      "validated pipeline to de-identified real-world vascular surgery registry data."
    ),
    style = "Normal"
  )

  # ---- Write output -------------------------------------------------------
  out_path <- file.path(output_dir, "ssi_validation_report.docx")
  print(doc, target = out_path)
  message("Report written to: ", normalizePath(out_path))
  invisible(out_path)
}


# ===========================================================================
# .report_prognostic()
#
# Full manuscript-format Word report with live CDM queries for Table 1.
# Parameterized by config$score_type ("integer" | "lasso") with 4 branches.
# Called by generate_manuscript_report() in R/report_extended.R.
#
# Renamed from generate_manuscript_report() in the original synthea-omop-template
# monolith.  All 4 parameterization branches have been applied; the rest of
# the body is verbatim.
# ===========================================================================
.report_prognostic <- function(output_dir        = "output",
                                       score_output_dir   = "output",
                                       mfi5_output_dir    = NULL,
                                       vqifs_output_dir   = NULL,
                                       connection_details = NULL,
                                       config             = NULL,
                                       citations          = NULL) {
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

  temp_figure_dir <- tempfile("report_figures_")
  dir.create(temp_figure_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(temp_figure_dir, recursive = TRUE, force = TRUE), add = TRUE)

  # ---------------------------------------------------------------------------
  # Section and supplemental-label counters.
  #
  # Section numbers used to be hardcoded into each heading string, which drifted
  # out of order as sections were added (the decision-curve section rendered as
  # "2.8" between "2.5" and "2.6") and double-numbered against the Word
  # template's own automatic heading numbering. Supplemental labels had the same
  # problem: "Supplemental Figure S6" was hardcoded inside a loop, so two
  # figures shared the label and a third got none.
  #
  # These closures are the single source of numbering for the whole document.
  # Call them in render order; never hardcode a number in a heading or caption.
  # ---------------------------------------------------------------------------
  .sec_major <- 0L   # level-2 heading counter  (1. Methods, 2. Results, ...)
  .sec_minor <- 0L   # level-3 heading counter, reset by each major section
  .supp_tbl  <- 0L   # Supplemental Table  S<n>
  .supp_fig  <- 0L   # Supplemental Figure S<n>

  # Start a new major section: "1. Methods". Resets the minor counter.
  section_major <- function(title) {
    .sec_major <<- .sec_major + 1L
    .sec_minor <<- 0L
    paste0(.sec_major, ". ", title)
  }
  # Next subsection within the current major section: "1.3. Risk score evaluation".
  section_num <- function(title) {
    .sec_minor <<- .sec_minor + 1L
    paste0(.sec_major, ".", .sec_minor, ". ", title)
  }
  # ---------------------------------------------------------------------------
  # Supplemental label plan.
  #
  # Supplemental tables and figures share ONE S-number sequence (S1 = a table,
  # S5 = a figure, ...), matching the established convention for this report.
  #
  # Labels are pre-allocated here rather than counted at render time for two
  # reasons: the Methods section cross-references subgroup tables/figures that
  # are not rendered until much later, and the subgroup sections physically
  # render before the "Supplemental Material" section even though readers
  # expect S1 to be the CDM metadata table. A pre-computed plan gives unique,
  # stable labels that both the cross-reference and the render site agree on.
  #
  # Which entries exist depends on which score pipelines supplied output, so the
  # plan is built after the pipeline outputs are loaded (see .init_supp_labels()
  # below). Look up a label with supp("<key>").
  # ---------------------------------------------------------------------------
  .supp_labels <- list()

  .init_supp_labels <- function(has_mfi5, has_vqifs) {
    keys <- c(
      # "Supplemental Material" section, in render order
      "cdm_metadata", "model_descriptions", "cpt_codes", "discharge_codes",
      "nhd_rate_by_month", "score_dist_iannuzzi",
      if (has_mfi5)  "score_dist_mfi5",
      if (has_vqifs) "score_dist_vqifs",
      # Subgroup bias sections (table then forest plot per model)
      "subgroup_tbl_iannuzzi", "subgroup_fig_iannuzzi",
      if (has_mfi5)  c("subgroup_tbl_mfi5",  "subgroup_fig_mfi5"),
      if (has_vqifs) c("subgroup_tbl_vqifs", "subgroup_fig_vqifs")
    )
    .supp_labels <<- stats::setNames(paste0("S", seq_along(keys)), keys)
  }

  # Return the bare label ("S7") or a prefixed one ("Supplemental Table S7").
  supp <- function(key, kind = NULL) {
    id <- .supp_labels[[key]]
    if (is.null(id)) id <- "S?"   # never abort a report over a label
    if (is.null(kind)) id else paste0("Supplemental ", kind, " ", id)
  }
  supp_table  <- function(key) supp(key, "Table")
  supp_figure <- function(key) supp(key, "Figure")

  make_doc_run <- function(text, bold = FALSE, font_size = 10) {
    officer::ftext(
      text,
      officer::fp_text(bold = bold, font.size = font_size, font.family = "Calibri")
    )
  }

  add_doc_caption <- function(doc, title, details = NULL) {
    caption_runs <- list(make_doc_run(title, bold = TRUE, font_size = 10))
    if (!is.null(details) && nzchar(details)) {
      caption_runs[[length(caption_runs) + 1L]] <- make_doc_run(
        paste0(" ", details), bold = FALSE, font_size = 10
      )
    }
    caption_par <- do.call(
      officer::fpar,
      c(caption_runs, list(fp_p = officer::fp_par(text.align = "left")))
    )
    officer::body_add_fpar(doc, value = caption_par, style = "Normal")
  }

  person_level_path <- file.path(score_output_dir, "person_level_scores.csv")
  covariate_summary_path <- file.path(score_output_dir, "covariate_summary.csv")
  metrics_path <- file.path(score_output_dir, "metrics.csv")
  lookup_calibration_plot <- file.path(score_output_dir, "calibration_lookup.png")
  recalibrated_calibration_plot <- file.path(score_output_dir, "calibration_recalibrated.png")
  calibration_table_lookup_path <- file.path(score_output_dir, "calibration_table_lookup.csv")
  calibration_table_recalibrated_path <- file.path(score_output_dir, "calibration_table_recalibrated.csv")
  lookup_calibration_plot_temp <- file.path(temp_figure_dir, "calibration_lookup.png")
  recalibrated_calibration_plot_temp <- file.path(temp_figure_dir, "calibration_recalibrated.png")

  if (!file.exists(person_level_path) || !file.exists(covariate_summary_path) || !file.exists(metrics_path)) {
    stop("Missing one or more required pipeline outputs in ", score_output_dir)
  }

  person_level <- read.csv(person_level_path, stringsAsFactors = FALSE)
  covariate_summary <- read.csv(covariate_summary_path, stringsAsFactors = FALSE)
  metrics <- read.csv(metrics_path, stringsAsFactors = FALSE)
  names(metrics) <- tolower(names(metrics))

  # --- mFI-5 pipeline outputs (loaded when mfi5_output_dir is supplied) ------
  person_level_mfi5      <- NULL
  covariate_summary_mfi5 <- NULL
  metrics_mfi5           <- NULL

  if (!is.null(mfi5_output_dir) && dir.exists(mfi5_output_dir)) {
    mfi5_pl_path  <- file.path(mfi5_output_dir, "person_level_scores.csv")
    mfi5_cs_path  <- file.path(mfi5_output_dir, "covariate_summary.csv")
    mfi5_met_path <- file.path(mfi5_output_dir, "metrics.csv")
    if (file.exists(mfi5_pl_path))  person_level_mfi5      <- read.csv(mfi5_pl_path,  stringsAsFactors = FALSE)
    if (file.exists(mfi5_cs_path))  covariate_summary_mfi5 <- read.csv(mfi5_cs_path,  stringsAsFactors = FALSE)
    if (file.exists(mfi5_met_path)) {
      metrics_mfi5 <- read.csv(mfi5_met_path, stringsAsFactors = FALSE)
      if (!"ci_lower" %in% names(metrics_mfi5)) metrics_mfi5$ci_lower <- NA_real_
      if (!"ci_upper" %in% names(metrics_mfi5)) metrics_mfi5$ci_upper <- NA_real_
    }
    message("[report] mFI-5 pipeline outputs loaded from: ", mfi5_output_dir)
  }

  # --- sVQI-FS pipeline outputs (loaded when vqifs_output_dir is supplied) ---
  person_level_vqifs      <- NULL
  covariate_summary_vqifs <- NULL
  metrics_vqifs           <- NULL

  if (!is.null(vqifs_output_dir) && dir.exists(vqifs_output_dir)) {
    vqifs_pl_path  <- file.path(vqifs_output_dir, "person_level_scores.csv")
    vqifs_cs_path  <- file.path(vqifs_output_dir, "covariate_summary.csv")
    vqifs_met_path <- file.path(vqifs_output_dir, "metrics.csv")
    if (file.exists(vqifs_pl_path))  person_level_vqifs      <- read.csv(vqifs_pl_path,  stringsAsFactors = FALSE)
    if (file.exists(vqifs_cs_path))  covariate_summary_vqifs <- read.csv(vqifs_cs_path,  stringsAsFactors = FALSE)
    if (file.exists(vqifs_met_path)) {
      metrics_vqifs <- read.csv(vqifs_met_path, stringsAsFactors = FALSE)
      if (!"ci_lower" %in% names(metrics_vqifs)) metrics_vqifs$ci_lower <- NA_real_
      if (!"ci_upper" %in% names(metrics_vqifs)) metrics_vqifs$ci_upper <- NA_real_
    }
    message("[report] sVQI-FS pipeline outputs loaded from: ", vqifs_output_dir)
  }

  # Now that we know which score pipelines produced output, fix the supplemental
  # S-number plan. Everything downstream refers to labels via supp()/supp_table()
  # /supp_figure() so Methods cross-references and render sites cannot diverge.
  .init_supp_labels(
    has_mfi5  = !is.null(person_level_mfi5),
    has_vqifs = !is.null(person_level_vqifs)
  )

  # --- Temporal split metadata (written by evaluate_integer_risk_score()) ----
  # split_info.csv contains split_date, n_train, n_test for each model.
  # Used to populate the methods sentence and Table 4 caption.
  split_info_iannuzzi <- NULL
  split_info_mfi5     <- NULL
  split_info_vqifs    <- NULL
  si_path_iannuzzi    <- file.path(score_output_dir, "split_info.csv")
  if (file.exists(si_path_iannuzzi)) {
    split_info_iannuzzi <- read.csv(si_path_iannuzzi, stringsAsFactors = FALSE)
  }
  if (!is.null(mfi5_output_dir)) {
    si_path_mfi5 <- file.path(mfi5_output_dir, "split_info.csv")
    if (file.exists(si_path_mfi5)) {
      split_info_mfi5 <- read.csv(si_path_mfi5, stringsAsFactors = FALSE)
    }
  }
  if (!is.null(vqifs_output_dir)) {
    si_path_vqifs <- file.path(vqifs_output_dir, "split_info.csv")
    if (file.exists(si_path_vqifs)) {
      split_info_vqifs <- read.csv(si_path_vqifs, stringsAsFactors = FALSE)
    }
  }

  # Helper: build a one-sentence split description for the methods section.
  # Returns NULL when no split was applied (pre-existing outputs without split).
  # Build the split-sample-size sentence. model_label = NULL renders the shared
  # form: the temporal split is identical across all scores (same patients, same
  # index dates), so the report states it once rather than repeating a
  # near-verbatim sentence per model.
  split_sentence <- function(si, model_label = NULL) {
    if (is.null(si) || nrow(si) == 0) return(NULL)
    prefix <- if (is.null(model_label)) "\tPatients" else sprintf("\tFor %s, patients", model_label)
    sprintf(
      paste0("%s were sorted chronologically by index date and ",
             "divided into a training set (n = %d; through %s) ",
             "used only to fit the recalibration models and a test set ",
             "(n = %d; after %s) on which the recalibrated and raw-score ",
             "specifications were evaluated. The same split applies to every score."),
      prefix,
      si$n_train[1], si$split_date[1],
      si$n_test[1],  si$split_date[1]
    )
  }

  # Backfill ECE if it is not present in the metrics file.
  compute_ece <- function(y, p, n_bins = 10) {
    ok <- !(is.na(y) | is.na(p))
    y <- as.numeric(y[ok])
    p <- as.numeric(p[ok])
    if (length(y) == 0) {
      return(NA_real_)
    }

    eps <- 1e-6
    p[p < eps] <- eps
    p[p > (1 - eps)] <- 1 - eps

    probs <- unique(stats::quantile(p, probs = seq(0, 1, length.out = n_bins + 1), na.rm = TRUE))
    if (length(probs) < 3) {
      probs <- c(0, 1)
    }
    bins <- cut(p, breaks = probs, include.lowest = TRUE)
    bin_n <- as.numeric(table(bins))
    if (length(bin_n) == 0) {
      return(NA_real_)
    }

    pred_mean <- tapply(p, bins, mean)
    obs_mean <- tapply(y, bins, mean)
    as.numeric(sum(abs(pred_mean - obs_mean) * bin_n) / sum(bin_n))
  }

  if (!any(metrics$metric == "ECE")) {
    ece_rows <- data.frame(
      metric = character(), value = numeric(),
      ci_lower = numeric(), ci_upper = numeric(),
      model = character(), stringsAsFactors = FALSE
    )

    if ("predicted_risk_lookup" %in% names(person_level)) {
      keep_lookup <- !is.na(person_level$predicted_risk_lookup)
      if (any(keep_lookup)) {
        ece_rows <- rbind(
          ece_rows,
          data.frame(
            metric   = "ECE",
            value    = compute_ece(person_level$outcome[keep_lookup], person_level$predicted_risk_lookup[keep_lookup]),
            ci_lower = NA_real_,
            ci_upper = NA_real_,
            model    = "lookup",
            stringsAsFactors = FALSE
          )
        )
      }
    }

    if ("predicted_risk_recalibrated" %in% names(person_level)) {
      ece_rows <- rbind(
        ece_rows,
        data.frame(
          metric   = "ECE",
          value    = compute_ece(person_level$outcome, person_level$predicted_risk_recalibrated),
          ci_lower = NA_real_,
          ci_upper = NA_real_,
          model    = "recalibrated",
          stringsAsFactors = FALSE
        )
      )
    }

    if (nrow(ece_rows) > 0) {
      # Ensure metrics has ci_lower/ci_upper before binding, so column sets match.
      if (!"ci_lower" %in% names(metrics)) metrics$ci_lower <- NA_real_
      if (!"ci_upper" %in% names(metrics)) metrics$ci_upper <- NA_real_
      metrics <- rbind(metrics, ece_rows)
    }
  } else {
    # Ensure ci_lower/ci_upper exist even when ECE was already present in the CSV.
    if (!"ci_lower" %in% names(metrics)) metrics$ci_lower <- NA_real_
    if (!"ci_upper" %in% names(metrics)) metrics$ci_upper <- NA_real_
  }

  # add_doc_page_break(): inserts a manual Word page break.
  add_doc_page_break <- function(doc) {
    officer::body_add_break(doc)
  }

  fmt <- function(x, digits = 3) {
    format(round(as.numeric(x), digits), nsmall = digits)
  }

  metric_value <- function(metric_name, model_name) {
    row <- metrics[metrics$metric == metric_name & metrics$model == model_name, , drop = FALSE]
    if (nrow(row) == 0) {
      return(NA_real_)
    }
    as.numeric(row$value[1])
  }

  # Returns a formatted "(lower–upper)" CI string for a given metric/model pair.
  # Pulls ci_lower and ci_upper from the metrics data frame (present when
  # compute_bootstrap_cis() was run during the pipeline).  Returns "—" when the
  # columns are absent or the values are NA (e.g. legacy metrics.csv files).
  metric_ci <- function(metric_name, model_name) {
    row <- metrics[metrics$metric == metric_name & metrics$model == model_name, , drop = FALSE]
    if (nrow(row) == 0) return("\u2014")
    if (!all(c("ci_lower", "ci_upper") %in% names(row))) return("\u2014")
    lo <- as.numeric(row$ci_lower[1])
    hi <- as.numeric(row$ci_upper[1])
    if (is.na(lo) || is.na(hi)) return("\u2014")
    paste0("(", fmt(lo), "\u2013", fmt(hi), ")")
  }

  # Metric lookup helpers for the mFI-5 model (reads from metrics_mfi5).
  metric_value_mfi5 <- function(metric_name, model_name) {
    if (is.null(metrics_mfi5)) return(NA_real_)
    row <- metrics_mfi5[metrics_mfi5$metric == metric_name & metrics_mfi5$model == model_name, , drop = FALSE]
    if (nrow(row) == 0) return(NA_real_)
    as.numeric(row$value[1])
  }

  metric_ci_mfi5 <- function(metric_name, model_name) {
    if (is.null(metrics_mfi5)) return("\u2014")
    row <- metrics_mfi5[metrics_mfi5$metric == metric_name & metrics_mfi5$model == model_name, , drop = FALSE]
    if (nrow(row) == 0) return("\u2014")
    if (!all(c("ci_lower", "ci_upper") %in% names(row))) return("\u2014")
    lo <- as.numeric(row$ci_lower[1])
    hi <- as.numeric(row$ci_upper[1])
    if (is.na(lo) || is.na(hi)) return("\u2014")
    paste0("(", fmt(lo), "\u2013", fmt(hi), ")")
  }

  # Metric lookup helpers for the sVQI-FS model (reads from metrics_vqifs).
  metric_value_vqifs <- function(metric_name, model_name) {
    if (is.null(metrics_vqifs)) return(NA_real_)
    row <- metrics_vqifs[metrics_vqifs$metric == metric_name & metrics_vqifs$model == model_name, , drop = FALSE]
    if (nrow(row) == 0) return(NA_real_)
    as.numeric(row$value[1])
  }

  metric_ci_vqifs <- function(metric_name, model_name) {
    if (is.null(metrics_vqifs)) return("\u2014")
    row <- metrics_vqifs[metrics_vqifs$metric == metric_name & metrics_vqifs$model == model_name, , drop = FALSE]
    if (nrow(row) == 0) return("\u2014")
    if (!all(c("ci_lower", "ci_upper") %in% names(row))) return("\u2014")
    lo <- as.numeric(row$ci_lower[1])
    hi <- as.numeric(row$ci_upper[1])
    if (is.na(lo) || is.na(hi)) return("\u2014")
    paste0("(", fmt(lo), "\u2013", fmt(hi), ")")
  }

  n_target <- nrow(person_level)
  n_outcome <- sum(person_level$outcome, na.rm = TRUE)
  outcome_prev <- if (n_target > 0) 100 * n_outcome / n_target else NA_real_

  results_tbl <- data.frame(
    Metric = c(
      "AUROC",
      "AUPRC",
      "Brier score",
      "Estimated calibration error",
      "Calibration intercept",
      "Calibration slope"
    ),
    Value = c(
      fmt(metric_value("AUROC", "lookup")),
      fmt(metric_value("AUPRC", "lookup")),
      fmt(metric_value("Brier", "lookup")),
      fmt(metric_value("ECE", "lookup")),
      fmt(metric_value("CalibrationIntercept", "lookup")),
      fmt(metric_value("CalibrationSlope", "lookup"))
    ),
    "95% CI" = c(
      metric_ci("AUROC",                "lookup"),
      metric_ci("AUPRC",                "lookup"),
      metric_ci("Brier",                "lookup"),
      metric_ci("ECE",                  "lookup"),
      metric_ci("CalibrationIntercept", "lookup"),
      metric_ci("CalibrationSlope",     "lookup")
    ),
    check.names     = FALSE,
    stringsAsFactors = FALSE
  )

  normalize_label <- function(x) {
    x <- tolower(trimws(as.character(x)))
    x <- gsub("[()\\[\\]\\{\\}]", " ", x)
    x <- gsub("[^a-z0-9]+", " ", x)
    x <- gsub("\\s+", " ", x)
    trimws(x)
  }

  # Branch 1 — Table 2 covariate/predictor definitions.
  # LASSO: read from varImp.rds artifact via .covariate_table_data_lasso().
  # Integer: use the static component specification table .covariate_table_data().
  tbl2_data <- if (identical(config$score_type, "lasso")) {
    .covariate_table_data_lasso(config)
  } else {
    .covariate_table_data()
  }
  # Build predictor_ref from whichever table was selected above.
  # Column names differ: integer has "points"; LASSO has "weight".
  if ("points" %in% names(tbl2_data) && "covariate_id" %in% names(tbl2_data)) {
    predictor_ref <- tbl2_data[, c("covariate_id", "variable", "points", "lookback", "derivation")]
    names(predictor_ref) <- c("covariate_id", "Predictor", "Points", "Lookback", "Definition")
  } else if ("weight" %in% names(tbl2_data) && "covariate_id" %in% names(tbl2_data)) {
    predictor_ref <- tbl2_data[, c("covariate_id", "variable", "weight", "lookback", "derivation")]
    names(predictor_ref) <- c("covariate_id", "Predictor", "Points", "Lookback", "Definition")
  } else {
    predictor_ref <- .covariate_table_data()[, c("covariate_id", "variable", "points", "lookback", "derivation")]
    names(predictor_ref) <- c("covariate_id", "Predictor", "Points", "Lookback", "Definition")
  }

  has_missing_col <- "n_missing" %in% names(covariate_summary)

  if ("covariate_id" %in% names(covariate_summary)) {
    keep_cols <- c("covariate_id", "n_positive", "mean_points",
                   if (has_missing_col) "n_missing")
    covariate_act <- covariate_summary[, keep_cols, drop = FALSE]
    predictor_tbl <- merge(predictor_ref, covariate_act, by = "covariate_id", all.x = TRUE, sort = FALSE)
  } else {
    predictor_ref$key <- normalize_label(predictor_ref$Predictor)
    keep_cols <- c("covariate_name", "n_positive", "mean_points",
                   if (has_missing_col) "n_missing")
    covariate_act <- covariate_summary[, keep_cols, drop = FALSE]
    covariate_act$key <- normalize_label(covariate_act$covariate_name)
    covariate_act <- covariate_act[, c("key", "n_positive", "mean_points",
                                       if (has_missing_col) "n_missing"), drop = FALSE]
    predictor_tbl <- merge(predictor_ref, covariate_act, by = "key", all.x = TRUE, sort = FALSE)
  }

  predictor_tbl$n_positive[is.na(predictor_tbl$n_positive)] <- 0
  predictor_tbl$mean_points[is.na(predictor_tbl$mean_points)] <- 0
  if (has_missing_col) predictor_tbl$n_missing[is.na(predictor_tbl$n_missing)] <- 0

  base_cols <- c("Predictor", "Points", "Lookback", "Definition", "n_positive", "mean_points")
  if (has_missing_col) base_cols <- c(base_cols, "n_missing")
  predictor_tbl <- predictor_tbl[, base_cols]

  if (has_missing_col) {
    names(predictor_tbl) <- c("Predictor", "Points", "Lookback", "Definition",
                               "PositiveCount", "MeanPoints", "MissingCount")
    predictor_tbl$MissingCount <- as.integer(predictor_tbl$MissingCount)
    predictor_tbl$MissingPct   <- paste0(
      round(100 * predictor_tbl$MissingCount / max(n_target, 1), 1), "%")
  } else {
    names(predictor_tbl) <- c("Predictor", "Points", "Lookback", "Definition",
                               "PositiveCount", "MeanPoints")
  }
  predictor_tbl$PositiveCount <- as.integer(predictor_tbl$PositiveCount)
  predictor_tbl$MeanPoints <- round(as.numeric(predictor_tbl$MeanPoints), 4)

  fmt_n_pct <- function(n, denom, digits = 1) {
    n <- suppressWarnings(as.numeric(n))
    denom <- suppressWarnings(as.numeric(denom))
    if (is.na(n) || is.na(denom) || denom <= 0) {
      return("0 (0.0%)")
    }
    paste0(format(round(n, 0), scientific = FALSE, trim = TRUE),
           " (", format(round(100 * n / denom, digits), nsmall = digits, trim = TRUE), "%)")
  }

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
      sql_age <- SqlRender::render(
        paste0(
          "WITH ", dedup_person_cte, "
           SELECT
             CAST(
               FLOOR(
                 CAST(DATEDIFF(DAY,
                   DATEFROMPARTS(
                     p.year_of_birth,
                     COALESCE(p.month_of_birth, 7),
                     COALESCE(p.day_of_birth,   1)
                   ),
                   t.cohort_start_date
                 ) AS FLOAT) / 365.25
               )
             AS FLOAT) AS age_at_index
           FROM @results_schema.@cohort_table t
           INNER JOIN dedup_person p ON p.person_id = t.subject_id
           WHERE t.cohort_definition_id = @target_id"
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

  append_distribution_rows <- function(tbl, dist_df, label_prefix, denom, max_rows = 6L) {
    if (is.null(dist_df) || nrow(dist_df) == 0) {
      return(tbl)
    }

    n_col <- names(dist_df)[tolower(names(dist_df)) == "n"][1]
    cat_col <- names(dist_df)[tolower(names(dist_df)) == "category"][1]
    if (is.na(n_col) || is.na(cat_col)) {
      return(tbl)
    }

    dist_df$n_value <- as.numeric(dist_df[[n_col]])
    dist_df$cat_value <- as.character(dist_df[[cat_col]])
    keep_n <- min(nrow(dist_df), max_rows)
    shown <- dist_df[seq_len(keep_n), , drop = FALSE]

    for (i in seq_len(nrow(shown))) {
      tbl <- rbind(
        tbl,
        data.frame(
          Item = paste0(label_prefix, " - ", shown$cat_value[i]),
          Value = fmt_n_pct(shown$n_value[i], denom),
          Definition = paste0(
            "Distribution among target-cohort patients based on OMOP person ",
            if (label_prefix == "Race") "race_concept_id" else if (label_prefix == "Ethnicity") "ethnicity_concept_id" else "gender_concept_id",
            "."
          ),
          stringsAsFactors = FALSE
        )
      )
    }

    if (nrow(dist_df) > keep_n) {
      other_n <- sum(as.numeric(dist_df$n_value[(keep_n + 1):nrow(dist_df)]), na.rm = TRUE)
      tbl <- rbind(
        tbl,
        data.frame(
          Item = paste0(label_prefix, " - Other"),
          Value = fmt_n_pct(other_n, denom),
          Definition = "Combined frequency of remaining categories not shown individually.",
          stringsAsFactors = FALSE
        )
      )
    }

    tbl
  }

  build_table1_cohort <- function(person_level, config, connection_details) {
    n_target  <- nrow(person_level)
    n_outcome <- sum(as.numeric(person_level$outcome), na.rm = TRUE)

    # Helper: build one row; is_header=TRUE makes the row a section label
    row1 <- function(char, val = "", header = FALSE) {
      data.frame(
        Characteristic = char,
        Value          = val,
        is_header      = header,
        stringsAsFactors = FALSE
      )
    }
    sub_row <- function(label, n, denom) {
      row1(paste0("    ", label), fmt_n_pct(n, denom))
    }
    # Look up count for a fixed OMOP concept_id in a distribution data frame
    # returned by distribution_sql() (columns: CONCEPT_ID, CATEGORY, N).
    # All matching is done on the integer concept_id — no string/regex logic.
    lookup_concept <- function(df, cid) {
      if (is.null(df) || nrow(df) == 0) return(0L)
      n_col  <- names(df)[toupper(names(df)) == "N"][1]
      id_col <- names(df)[toupper(names(df)) == "CONCEPT_ID"][1]
      if (is.na(n_col) || is.na(id_col)) return(0L)
      idx <- which(as.integer(df[[id_col]]) == as.integer(cid))
      if (length(idx) == 0) return(0L)
      sum(as.integer(df[[n_col]][idx]), na.rm = TRUE)
    }
    # Look up count by exact category label (used for indication/procedure rows
    # where the SQL itself sets the category string, so it is stable).
    lookup_n <- function(df, category_value) {
      if (is.null(df) || nrow(df) == 0) return(0L)
      n_col   <- names(df)[toupper(names(df)) == "N"][1]
      cat_col <- names(df)[toupper(names(df)) == "CATEGORY"][1]
      if (is.na(n_col) || is.na(cat_col)) return(0L)
      idx <- which(trimws(df[[cat_col]]) == category_value)
      if (length(idx) == 0) return(0L)
      as.integer(df[[n_col]][idx[1]])
    }

    demog <- fetch_demographics_from_omop(config, connection_details)

    # ---- Age ------------------------------------------------------------------
    age_row <- row1("Age, median (IQR), years", "N/A")
    if (!is.null(demog$age) && nrow(demog$age) > 0) {
      # DatabaseConnector >= 6.0 stopped auto-uppercasing column names, so use
      # a case-insensitive lookup instead of the hard-coded AGE_AT_INDEX name.
      .age_col <- names(demog$age)[toupper(names(demog$age)) == "AGE_AT_INDEX"][1]
      ages <- if (!is.na(.age_col)) as.numeric(demog$age[[.age_col]]) else numeric(0)
      ages <- ages[!is.na(ages)]
      if (length(ages) > 0) {
        q <- stats::quantile(ages, probs = c(0.25, 0.75), na.rm = TRUE)
        age_row <- row1(
          "Age, median (IQR), years",
          paste0(
            as.character(as.integer(floor(stats::median(ages)))),
            " (",
            as.character(as.integer(floor(q[[1]]))),
            "\u2013",
            as.character(as.integer(floor(q[[2]]))),
            ")"
          )
        )
      }
    }

    # ---- Sex ------------------------------------------------------------------
    # Standard OMOP Gender domain concept IDs (vocabulary_id = 'Gender'):
    #   8507 = MALE
    #   8532 = FEMALE
    male_n   <- lookup_concept(demog$sex, 8507L)
    female_n <- lookup_concept(demog$sex, 8532L)

    # ---- Race / Ethnicity (four requested categories) ------------------------
    # Standard OMOP Race domain concept IDs (vocabulary_id = 'Race'):
    #   8527 = White
    #   8516 = Black or African American
    #   8515 = Asian
    # Standard OMOP Ethnicity domain concept IDs (vocabulary_id = 'Ethnicity'):
    #   38003563 = Hispanic or Latino
    #   38003564 = Not Hispanic or Latino
    #
    # RACE AND ETHNICITY ARE SEPARATE OMOP DOMAINS AND MUST NOT SHARE A TABLE
    # SECTION (fixed 2026-07-26): Hispanic/Latino (ethnicity_concept_id) was
    # previously rendered as a fourth row under the "Race" header alongside
    # White/Black/Asian (race_concept_id). Since a patient can be e.g. both
    # White (race) AND Hispanic (ethnicity), summing all four rows' percentages
    # as if they were one mutually-exclusive block made the "Race" section sum
    # to well over 100% — not a data error, just a table-layout error that
    # conflated two independent OMOP fields. latino_n now renders in its own
    # "Ethnicity" section below, matching how Race/Ethnicity are reported in
    # Supplemental Table S7 (subgroup bias), which already treats them as
    # distinct subgroup variables.
    white_n   <- lookup_concept(demog$race,      8527L)
    black_n   <- lookup_concept(demog$race,      8516L)
    asian_n   <- lookup_concept(demog$race,      8515L)
    latino_n  <- lookup_concept(demog$ethnicity, 38003563L)
    non_latino_n <- lookup_concept(demog$ethnicity, 38003564L)

    # ---- Indication (PAD/DM/wound, from concept-ancestor rollup before index date) -----
    ind_df  <- demog$indication
    pad_n   <- lookup_n(ind_df, "PAD")
    dm_n    <- lookup_n(ind_df, "Diabetes mellitus")
    wound_n <- lookup_n(ind_df, "LE wound / gangrene")

    # ---- Amputation level -----------------------------------------------------
    pt_df       <- demog$procedure_type
    aka_n       <- lookup_n(pt_df, "Above-knee amputation (AKA)")
    bka_n       <- lookup_n(pt_df, "Below-knee amputation (BKA)")
    other_amp_n <- lookup_n(pt_df, "Other amputation")

    # ---- Assemble table -------------------------------------------------------
    tbl <- rbind(
      age_row,
      # Sex
      row1("Sex", header = TRUE),
      sub_row("Male",   male_n,   n_target),
      sub_row("Female", female_n, n_target),
      # Race (mutually exclusive OMOP race_concept_id categories)
      row1("Race", header = TRUE),
      sub_row("White",             white_n,  n_target),
      sub_row("Black",             black_n,  n_target),
      sub_row("Asian",             asian_n,  n_target),
      # Ethnicity — a SEPARATE OMOP field from race (a patient may be e.g. both
      # White and Hispanic); rendered as its own section rather than folded into
      # Race, which previously made the Race section's percentages sum to over
      # 100% (see the note above white_n/black_n/asian_n/latino_n).
      row1("Ethnicity", header = TRUE),
      sub_row("Hispanic / Latino",     latino_n,     n_target),
      sub_row("Not Hispanic / Latino", non_latino_n, n_target),
      # Indication (PAD, DM, and/or LE wound documented before index; not mutually exclusive)
      row1("Indication (not mutually exclusive)", header = TRUE),
      sub_row("Peripheral arterial disease (PAD)", pad_n,   n_target),
      sub_row("Diabetes mellitus",                  dm_n,    n_target),
      sub_row("Lower-extremity wound / gangrene",   wound_n, n_target),
      # Amputation level (mutually exclusive: AKA > BKA > Other)
      row1("Amputation level", header = TRUE),
      sub_row("Above-knee (AKA)", aka_n,       n_target),
      sub_row("Below-knee (BKA)", bka_n,       n_target),
      sub_row("Other",            other_amp_n, n_target),
      # Outcome
      row1("Outcome", header = TRUE),
      sub_row("Non-home discharge (NHD)", n_outcome, n_target),
      # Total (last row)
      row1("Total cohort", fmt_n_pct(n_target, n_target))
    )

    tbl
  }

  # Use passed-in config / connection_details; fall back to get_validation_config()
  # for callers that do not supply them explicitly.
  if (is.null(config) && exists("get_validation_config", mode = "function")) {
    config <- tryCatch(get_validation_config(), error = function(e) NULL)
  }
  if (is.null(connection_details) && !is.null(config) &&
      exists("build_connection_details", mode = "function")) {
    connection_details <- tryCatch(build_connection_details(config), error = function(e) NULL)
  }
  cohort_tbl <- build_table1_cohort(person_level, config, connection_details)

  simple_ft <- function(df) {
    flextable(df) |>
      bold(part = "header") |>
      fontsize(size = 10, part = "all") |>
      font(fontname = "Calibri", part = "all") |>
      bg(part = "header", bg = "#1F3864") |>
      color(part = "header", color = "white") |>
      padding(padding = 4, part = "all") |>
      autofit()
  }

  # Two-column Table 1 formatter.
  # Expects df with columns: Characteristic, Value, is_header (logical).
  # is_header rows are rendered bold with no indentation; sub-rows are indented.
  # The is_header column is dropped before the flextable is built.
  table1_ft <- function(df) {
    if (is.null(df) || nrow(df) == 0) {
      return(flextable(data.frame(Characteristic = character(), Value = character())))
    }

    header_rows <- which(df$is_header)
    sub_rows    <- which(!df$is_header)
    total_row   <- nrow(df)   # last row is always "Total cohort"

    # Strip the helper column before passing to flextable
    display_df <- df[, c("Characteristic", "Value"), drop = FALSE]

    ft <- flextable(display_df) |>
      bold(part = "header") |>
      fontsize(size = 10, part = "all") |>
      font(fontname = "Calibri", part = "all") |>
      bg(part = "header", bg = "#1F3864") |>
      color(part = "header", color = "white") |>
      align(align = "left",  part = "all") |>
      valign(valign = "top", part = "all") |>
      padding(padding = 3, part = "all") |>
      # Section-label rows: bold, no left indent, light grey background
      bold(i = header_rows, part = "body") |>
      bg(i = header_rows, bg = "#F2F2F2", part = "body") |>
      # Sub-rows: extra left padding to simulate indent
      padding(i = sub_rows, j = "Characteristic", padding.left = 18, part = "body") |>
      # Total row: bold
      bold(i = total_row, part = "body") |>
      # Column widths
      width(j = "Characteristic", width = 2.8) |>
      width(j = "Value",          width = 1.4) |>
      set_table_properties(layout = "fixed")

    ft
  }

  wrapped_definition_ft <- function(df) {
    ft <- flextable(df) |>
      bold(part = "header") |>
      fontsize(size = 10, part = "all") |>
      font(fontname = "Calibri", part = "all") |>
      bg(part = "header", bg = "#1F3864") |>
      color(part = "header", color = "white") |>
      padding(padding = 4, part = "all") |>
      align(align = "left", part = "all") |>
      valign(valign = "top", part = "all") |>
      width(j = "Item", width = 1.4) |>
      width(j = "Value", width = 0.9) |>
      width(j = "Definition", width = 4.7) |>
      set_table_properties(layout = "fixed")

    ft
  }

  wrapped_predictor_ft <- function(df) {
    has_missing <- "MissingPct" %in% names(df)
    ft <- flextable(df) |>
      bold(part = "header") |>
      fontsize(size = 9, part = "all") |>
      font(fontname = "Calibri", part = "all") |>
      bg(part = "header", bg = "#1F3864") |>
      color(part = "header", color = "white") |>
      padding(padding = 3, part = "all") |>
      align(align = "left", part = "all") |>
      valign(valign = "top", part = "all") |>
      width(j = "Predictor",    width = 1.3) |>
      width(j = "Points",       width = 0.5) |>
      width(j = "Lookback",     width = 0.75) |>
      width(j = "Definition",   width = if (has_missing) 3.0 else 3.5) |>
      width(j = "PositiveCount", width = 0.75) |>
      width(j = "MeanPoints",   width = 0.7)
    if (has_missing) {
      ft <- ft |>
        width(j = "MissingCount", width = 0.65) |>
        width(j = "MissingPct",   width = 0.65) |>
        set_header_labels(
          MissingCount = "No CDM\nRecord\nn",
          MissingPct   = "No CDM\nRecord\n%"
        )
    }
    ft |> set_table_properties(layout = "fixed")
  }

  # ---------------------------------------------------------------------------
  # save_subgroup_forest_plot()
  #
  # Builds a forest plot of ECE (95% CI) by subgroup from the subgroup_bias
  # data frame.  Each subgroup variable is drawn as a labelled section (via
  # ggplot2 faceting on subgroup_var).  A dashed vertical reference line shows
  # the overall ECE for the lookup model (read from metrics.csv).
  #
  # Arguments:
  #   bias_df      — data frame from subgroup_bias.csv
  #   overall_ece  — numeric overall ECE (lookup model) for the reference line
  #   output_folder — directory where the PNG will be written
  #
  # Returns the path to the saved PNG, or NULL on failure.
  # ---------------------------------------------------------------------------
  save_subgroup_forest_plot <- function(bias_df, overall_ece, output_folder,
                                        file_name = "subgroup_forest_plot.png") {

    if (is.null(bias_df) || nrow(bias_df) == 0) return(NULL)

    # Impose Table 1 ordering on subgroup facets.
    var_order <- c("age_group", "sex", "race", "ethnicity",
                   "indication", "proc_type", "year")
    present_vars <- var_order[var_order %in% bias_df$subgroup_var]
    extra_vars   <- setdiff(unique(bias_df$subgroup_var), present_vars)
    ordered_vars <- c(present_vars, extra_vars)
    bias_df$subgroup_var <- factor(bias_df$subgroup_var, levels = ordered_vars)

    # Build a combined label: "Sex: Female", "Race: Black", etc.
    bias_df$label <- paste0(
      tools::toTitleCase(gsub("_", " ", as.character(bias_df$subgroup_var))),
      ": ",
      bias_df$subgroup_level
    )

    # Order labels within each facet by ECE (ascending) for readability.
    bias_df$label <- factor(
      bias_df$label,
      levels = bias_df$label[order(bias_df$subgroup_var, bias_df$ece)]
    )

    # Facet labels: capitalise the subgroup variable name for display.
    facet_labels <- setNames(
      tools::toTitleCase(gsub("_", " ", levels(bias_df$subgroup_var))),
      levels(bias_df$subgroup_var)
    )

    p <- ggplot2::ggplot(bias_df,
           ggplot2::aes(x = ece, y = label)) +
      ggplot2::geom_point(size = 2, colour = "black") +
      ggplot2::geom_errorbar(
        ggplot2::aes(xmin = ci_lower, xmax = ci_upper),
        width = 0.25, colour = "grey40", orientation = "y"
      ) +
      ggplot2::geom_vline(
        xintercept = overall_ece,
        linetype   = "dashed",
        colour     = "black"
      ) +
      ggplot2::facet_grid(
        subgroup_var ~ .,
        scales   = "free_y",
        space    = "free_y",
        labeller = ggplot2::as_labeller(facet_labels)
      ) +
      ggplot2::labs(
        x     = "Expected Calibration Error (95% CI)",
        y     = NULL,
        title = "Subgroup Calibration (ECE)",
        caption = paste0(
          "Dashed line = overall ECE (", round(overall_ece, 3), "). ",
          "Groups with < 10 events suppressed. ",
          "CIs from 200 bootstrap resamples."
        )
      ) +
      ggplot2::theme_bw(base_size = 11) +
      ggplot2::theme(
        panel.grid.major.y = ggplot2::element_blank(),
        strip.text         = ggplot2::element_text(face = "bold"),
        plot.caption       = ggplot2::element_text(size = 8)
      )

    out_path <- file.path(output_folder, file_name)
    tryCatch({
      ggplot2::ggsave(out_path, p,
                      width  = 7,
                      height = max(4, nrow(bias_df) * 0.35 + 1.5),
                      dpi    = 150)
      out_path
    }, error = function(e) {
      message("[report] Could not save subgroup forest plot: ", conditionMessage(e))
      NULL
    })
  }

  # ---------------------------------------------------------------------------
  # save_score_distribution_plot()
  # Stacked count histogram of integer risk scores by outcome (NHD vs not).
  # model_label is shown in the plot title to identify which score is plotted.
  # ---------------------------------------------------------------------------
  save_score_distribution_plot <- function(person_level_df, output_folder,
                                           model_label = "Integer risk score") {
    if (!all(c("total_score", "outcome") %in% names(person_level_df))) return(NULL)
    df <- person_level_df[, c("total_score", "outcome")]
    df$Outcome <- ifelse(df$outcome == 1, "NHD", "No NHD")
    df$Outcome <- factor(df$Outcome, levels = c("No NHD", "NHD"))

    score_range <- range(df$total_score, na.rm = TRUE)
    p <- ggplot2::ggplot(df, ggplot2::aes(x = total_score, fill = Outcome)) +
      ggplot2::geom_histogram(
        binwidth = 1, position = "stack", colour = "white"
      ) +
      ggplot2::scale_fill_manual(values = c("No NHD" = "#4472C4", "NHD" = "#C00000")) +
      ggplot2::scale_x_continuous(
        breaks = seq(floor(score_range[1]), ceiling(score_range[2]), by = 1)
      ) +
      ggplot2::labs(
        title   = paste0("Score Distribution by Outcome — ", model_label),
        x       = "Integer risk score",
        y       = "Count",
        fill    = NULL,
        caption = paste0(
          "N = ", nrow(df), " patients. ",
          "NHD = non-home discharge. Bars stacked: No NHD (blue) below, NHD (red) above."
        )
      ) +
      ggplot2::theme_bw(base_size = 11) +
      ggplot2::theme(
        legend.position  = "top",
        plot.caption     = ggplot2::element_text(size = 8),
        panel.grid.minor = ggplot2::element_blank()
      )

    out_path <- file.path(output_folder, paste0("score_distribution_",
                          gsub("[^A-Za-z0-9]", "_", tolower(model_label)), ".png"))
    tryCatch({
      ggplot2::ggsave(out_path, p, width = 6, height = 3.8, dpi = 150)
      out_path
    }, error = function(e) {
      message("[report] Could not save score distribution plot: ", conditionMessage(e))
      NULL
    })
  }

  # ---------------------------------------------------------------------------
  # save_dca_plot()
  # Decision curve analysis: net benefit vs. threshold probability.
  # Accepts a named list of predicted-risk vectors (models) so that multiple
  # model curves can be overlaid. Threshold range 1-70% for NHD studies.
  #
  # Parameters:
  #   y            — integer vector of observed outcomes (0/1)
  #   p            — numeric vector (single model) OR named list of numeric
  #                  vectors (multiple models); e.g.:
  #                  list("Iannuzzi Lookup" = v1, "Iannuzzi Recal" = v2,
  #                       "mFI-5 Recal" = v3)
  #   output_folder — directory for the PNG file
  # ---------------------------------------------------------------------------
  save_dca_plot <- function(y, p, output_folder) {
    if (length(y) == 0 || length(p) == 0) return(NULL)

    # Normalise single-vector input to a 1-element named list.
    if (!is.list(p)) {
      p <- list("Lookup model" = p)
    }

    thresholds <- seq(0.01, 0.99, by = 0.005)
    n    <- length(y)
    prev <- mean(y, na.rm = TRUE)

    # Build a net-benefit row for each model at each threshold.
    model_rows <- do.call(rbind, lapply(names(p), function(nm) {
      pv <- p[[nm]]
      nb <- vapply(thresholds, function(pt) {
        pp <- pv >= pt
        tp <- sum(pp & y == 1, na.rm = TRUE)
        fp <- sum(pp & y == 0, na.rm = TRUE)
        tp / n - fp / n * (pt / (1 - pt))
      }, numeric(1L))
      data.frame(threshold = thresholds, net_benefit = nb,
                 Strategy = nm, stringsAsFactors = FALSE)
    }))

    # Reference strategies: treat-all and treat-none.
    ref_rows <- data.frame(
      threshold   = rep(thresholds, 2),
      net_benefit = c(pmax(prev - (1 - prev) * (thresholds / (1 - thresholds)), 0),
                      rep(0, length(thresholds))),
      Strategy    = rep(c("Treat all", "Treat none"), each = length(thresholds)),
      stringsAsFactors = FALSE
    )

    dca_df <- rbind(model_rows, ref_rows)

    # Colour / linetype palettes for up to 4 model slots (Iannuzzi Lookup,
    # Iannuzzi Recal., mFI-5 Recal., sVQI-FS Recal. — see call site). Colours
    # are the first four slots of the validated categorical palette
    # (blue/orange/aqua/yellow); this order clears the CVD/normal-vision
    # adjacent-pair checks for a line chart (dataviz skill, validate_palette.js).
    # Previously only 3 colours/linetypes were defined, so the 4th model
    # (sVQI-FS) silently got NA for both and its line never rendered.
    model_names    <- names(p)
    model_colours  <- c("#2a78d6", "#eb6834", "#1baf7a", "#eda100")
    model_lty      <- c("solid", "longdash", "dotdash", "twodash")
    all_strategies <- c(model_names, "Treat all", "Treat none")
    all_colours    <- c(model_colours[seq_along(model_names)], "#888888", "grey50")
    all_lty        <- c(model_lty[seq_along(model_names)], "dashed", "dotted")
    names(all_colours) <- all_strategies
    names(all_lty)     <- all_strategies
    dca_df$Strategy    <- factor(dca_df$Strategy, levels = all_strategies)

    p_dca <- ggplot2::ggplot(dca_df,
        ggplot2::aes(x = threshold * 100, y = net_benefit,
                     colour = Strategy, linetype = Strategy)) +
      ggplot2::geom_line(linewidth = 0.9) +
      ggplot2::scale_colour_manual(values = all_colours) +
      ggplot2::scale_linetype_manual(values = all_lty) +
      ggplot2::scale_x_continuous(
        limits = c(0, 100),
        breaks = seq(0, 100, by = 10),
        labels = function(x) paste0(x, "%")
      ) +
      ggplot2::labs(
        title   = "Decision Curve Analysis",
        x       = "Threshold probability (%)",
        y       = "Net benefit",
        colour  = NULL, linetype = NULL,
        caption = paste0(
          "Net benefit = TP/N − FP/N × (pₜ / (1−pₜ)). ",
          "Treat-all and treat-none are reference strategies. ",
          "Threshold range 1–99%."
        )
      ) +
      ggplot2::theme_bw(base_size = 11) +
      ggplot2::theme(
        legend.position  = "top",
        plot.caption     = ggplot2::element_text(size = 8),
        panel.grid.minor = ggplot2::element_blank()
      )

    out_path <- file.path(output_folder, "decision_curve.png")
    tryCatch({
      ggplot2::ggsave(out_path, p_dca, width = 6.5, height = 4.5, dpi = 150)
      out_path
    }, error = function(e) {
      message("[report] Could not save DCA plot: ", conditionMessage(e))
      NULL
    })
  }

  # ---------------------------------------------------------------------------
  # next_report_file()
  #
  # Returns the target .docx path for the new report.  archive_old_reports()
  # always runs before this, so the dated filename is free to use directly.
  # ---------------------------------------------------------------------------
  next_report_file <- function(output_dir, base_name) {
    file.path(output_dir, paste0(base_name, ".docx"))
  }

  # ---------------------------------------------------------------------------
  # archive_old_reports()
  #
  # Moves all existing .docx files in output_dir to output_dir/archive/ before
  # each report run so only the latest report is visible at the top level.
  # Files are renamed with an incrementing suffix (_2, _3 …) when an archive
  # entry with the same name already exists (e.g. two runs on the same date).
  # ---------------------------------------------------------------------------
  archive_old_reports <- function(output_dir) {
    existing <- list.files(output_dir, pattern = "\\.docx$",
                           full.names = TRUE, all.files = FALSE)
    if (length(existing) == 0L) return(invisible(NULL))

    archive_dir <- file.path(output_dir, "archive")
    dir.create(archive_dir, recursive = TRUE, showWarnings = FALSE)

    for (f in existing) {
      nm   <- basename(f)
      dest <- file.path(archive_dir, nm)

      # Avoid silently overwriting an existing archive entry on same-day re-runs.
      if (file.exists(dest)) {
        base <- tools::file_path_sans_ext(nm)
        ext  <- tools::file_ext(nm)
        i    <- 2L
        repeat {
          dest <- file.path(archive_dir, paste0(base, "_", i, ".", ext))
          if (!file.exists(dest)) break
          i <- i + 1L
        }
      }

      file.rename(f, dest)
    }

    invisible(NULL)
  }

  roc_y <- person_level$outcome
  if ("predicted_risk_lookup" %in% names(person_level)) {
    keep_lookup <- !is.na(person_level$predicted_risk_lookup)
    roc_y <- person_level$outcome[keep_lookup]
    roc_p <- person_level$predicted_risk_lookup[keep_lookup]
  } else if ("predicted_risk_recalibrated" %in% names(person_level)) {
    roc_p <- person_level$predicted_risk_recalibrated
  } else {
    roc_p <- person_level$total_score / max(person_level$total_score, na.rm = TRUE)
  }

  roc_plot_file <- .save_roc_plot(
    y = roc_y,
    p = roc_p,
    output_folder = temp_figure_dir,
    auc_override = metric_value("AUROC", "lookup")
  )

  # Branch 3 — Annual and monthly outcome rate plots.
  # LASSO (MACCE-style): single-line trend via .save_macce_rate_by_year_plot().
  # NHD integer: stacked area chart by disposition type via .save_nhd_rate_by_year_plot().
  if (identical(config$score_type, "lasso")) {
    nhd_year_plot_file  <- .save_macce_rate_by_year_plot(person_level, temp_figure_dir)
    nhd_month_plot_file <- .save_macce_rate_by_month_plot(person_level, temp_figure_dir)
  } else {
    # Fetch discharge disposition type — required by .save_nhd_rate_by_year_plot().
    discharge_types_df <- fetch_discharge_types_from_omop(config, connection_details)
    if (!is.null(discharge_types_df) && nrow(discharge_types_df) > 0) {
      person_level <- merge(person_level, discharge_types_df,
                            by = "subject_id", all.x = TRUE)
      message("[report] discharge_type joined: ",
              sum(!is.na(person_level$discharge_type)), " of ",
              nrow(person_level), " patients have a mapped disposition.")
    }
    nhd_year_plot_file  <- .save_nhd_rate_by_year_plot(person_level, temp_figure_dir)
    nhd_month_plot_file <- .save_nhd_rate_by_month_plot(person_level, temp_figure_dir)
  }

  # Score distribution plots — one supplemental figure per model
  score_dist_plot_file_iannuzzi <- save_score_distribution_plot(
    person_level, temp_figure_dir, model_label = "Iannuzzi 2020")
  score_dist_plot_file_mfi5 <- if (!is.null(person_level_mfi5)) {
    save_score_distribution_plot(
      person_level_mfi5, temp_figure_dir, model_label = "mFI-5")
  } else NULL
  score_dist_plot_file_vqifs <- if (!is.null(person_level_vqifs)) {
    save_score_distribution_plot(
      person_level_vqifs, temp_figure_dir, model_label = "sVQI-FS")
  } else NULL
  score_dist_plot_file <- score_dist_plot_file_iannuzzi  # keep backward compat name

  # Returns a logical index selecting the temporal test partition of a
  # person_level data frame (split_set == "test"); TRUE for every row when no
  # split was applied. Shared by the DCA plot below and the risk-tier table
  # further down, so every place that needs "the evaluation rows for a
  # recalibrated specification" uses the same definition.
  .test_rows <- function(df) {
    if (is.null(df)) return(logical(0))
    if ("split_set" %in% names(df)) df$split_set == "test" else rep(TRUE, nrow(df))
  }

  # Decision curve analysis plot (Figure 4) — pass every available model's
  # predictions as a named list so all curves the report actually has (up to
  # six: Iannuzzi lookup/recal, mFI-5 raw/recal, sVQI-FS raw/recal) appear on
  # the same plot, driven from the SAME model list Table 4 uses (previously
  # Figure 4 and Table 4 iterated independent lists and drifted — Table 4
  # included sVQI-FS while Figure 4 silently omitted it).
  #
  # EVALUATION SET: restricted to the temporal test partition, matching Table 5
  # (recalibrated tiers) and the subgroup ECE analysis — every curve on this
  # plot compares net benefit across the same patients. The published Iannuzzi
  # lookup is included on this same test-set population for comparability with
  # the other curves on this specific figure, even though Table 4/Table 5 report
  # it over the full cohort elsewhere (it requires no fitting, so evaluating it
  # on a subset introduces no leakage — only fewer events).
  dca_plot_file <- tryCatch({
    if ("predicted_risk_lookup" %in% names(person_level)) {
      test_pl  <- .test_rows(person_level)
      pl_test  <- person_level[test_pl, , drop = FALSE]
      keep_dca <- !is.na(pl_test$predicted_risk_lookup)
      y_dca    <- pl_test$outcome[keep_dca]
      p_list   <- list(
        "Iannuzzi (Lookup)" = pl_test$predicted_risk_lookup[keep_dca]
      )
      if ("predicted_risk_recalibrated" %in% names(pl_test)) {
        keep_recal <- !is.na(pl_test$predicted_risk_recalibrated[keep_dca])
        if (sum(keep_recal) > 10L) {
          p_list[["Iannuzzi (Recal.)"]] <- pl_test$predicted_risk_recalibrated[keep_dca]
        }
      }
      subj_dca <- pl_test$subject_id[keep_dca]

      # Helper: align another score's recalibrated predictions (which may come
      # from a different person_level frame with its own row order/split) onto
      # the subject_id order established above, restricted to ITS OWN test rows.
      .align_recal <- function(pl_other, label) {
        if (is.null(pl_other) || !"predicted_risk_recalibrated" %in% names(pl_other)) return(NULL)
        other_test <- pl_other[.test_rows(pl_other), , drop = FALSE]
        aligned <- other_test$predicted_risk_recalibrated[match(subj_dca, other_test$subject_id)]
        if (all(is.na(aligned))) return(NULL)
        aligned
      }
      mfi5_aligned  <- .align_recal(person_level_mfi5,  "mFI-5")
      vqifs_aligned <- .align_recal(person_level_vqifs, "sVQI-FS")
      if (!is.null(mfi5_aligned))  p_list[["mFI-5 (Recal.)"]]   <- mfi5_aligned
      if (!is.null(vqifs_aligned)) p_list[["sVQI-FS (Recal.)"]] <- vqifs_aligned

      save_dca_plot(y_dca, p_list, temp_figure_dir)
    } else NULL
  }, error = function(e) {
    message("[report] DCA plot failed: ", conditionMessage(e))
    NULL
  })

  if (file.exists(lookup_calibration_plot)) {
    file.copy(lookup_calibration_plot, lookup_calibration_plot_temp, overwrite = TRUE)
  } else if (file.exists(calibration_table_lookup_path)) {
    lookup_generated <- .save_calibration_plot_from_table(
      calibration_table_path = calibration_table_lookup_path,
      output_folder = temp_figure_dir,
      file_name = "calibration_lookup.png",
      plot_title = "Calibration Plot: Lookup Model"
    )
    if (!is.null(lookup_generated) && file.exists(lookup_generated)) {
      lookup_calibration_plot_temp <- lookup_generated
    }
  }

  if (file.exists(recalibrated_calibration_plot)) {
    file.copy(recalibrated_calibration_plot, recalibrated_calibration_plot_temp, overwrite = TRUE)
  } else if (file.exists(calibration_table_recalibrated_path)) {
    recal_generated <- .save_calibration_plot_from_table(
      calibration_table_path = calibration_table_recalibrated_path,
      output_folder = temp_figure_dir,
      file_name = "calibration_recalibrated.png",
      plot_title = "Calibration Plot: Recalibrated Model"
    )
    if (!is.null(recal_generated) && file.exists(recal_generated)) {
      recalibrated_calibration_plot_temp <- recal_generated
    }
  }

  if (!file.exists(lookup_calibration_plot_temp) &&
      all(c("outcome", "predicted_risk_lookup") %in% names(person_level))) {
    lookup_generated <- .save_calibration_plot_from_vectors(
      y = person_level$outcome,
      p = person_level$predicted_risk_lookup,
      output_folder = temp_figure_dir,
      file_name = "calibration_lookup.png",
      plot_title = "Calibration Plot: Lookup Model"
    )
    if (!is.null(lookup_generated) && file.exists(lookup_generated)) {
      lookup_calibration_plot_temp <- lookup_generated
    }
  }

  if (!file.exists(recalibrated_calibration_plot_temp) &&
      all(c("outcome", "predicted_risk_recalibrated") %in% names(person_level))) {
    recal_generated <- .save_calibration_plot_from_vectors(
      y = person_level$outcome,
      p = person_level$predicted_risk_recalibrated,
      output_folder = temp_figure_dir,
      file_name = "calibration_recalibrated.png",
      plot_title = "Calibration Plot: Recalibrated Model"
    )
    if (!is.null(recal_generated) && file.exists(recal_generated)) {
      recalibrated_calibration_plot_temp <- recal_generated
    }
  }

  project_name <- basename(normalizePath(getwd(), winslash = "/", mustWork = FALSE))
  project_name <- gsub("[^A-Za-z0-9_-]", "_", project_name)
  report_base_name <- paste(project_name, "report", format(Sys.Date(), "%Y%m%d"), sep = "_")

  archive_old_reports(output_dir)

  report_file <- next_report_file(output_dir, report_base_name)

  # Query CDM source metadata early so values are available for the methods text.
  cdm_version_str        <- ""
  vocabulary_version_str <- ""
  if (!is.null(connection_details) && !is.null(config)) {
    tryCatch({
      conn_meta <- DatabaseConnector::connect(connection_details)
      meta_raw  <- DatabaseConnector::querySql(
        conn_meta,
        SqlRender::translate(
          SqlRender::render(
            "SELECT cdm_version, vocabulary_version FROM @cdm_schema.cdm_source",
            cdm_schema = config$cdm_schema
          ),
          targetDialect = "sql server"
        )
      )
      DatabaseConnector::disconnect(conn_meta)
      names(meta_raw) <- tolower(names(meta_raw))
      if (nrow(meta_raw) > 0) {
        cdm_version_str        <- as.character(meta_raw$cdm_version[1])
        vocabulary_version_str <- as.character(meta_raw$vocabulary_version[1])
      }
    }, error = function(e) NULL)
  }

  doc <- read_docx()
  doc <- officer::body_set_default_section(
    doc,
    officer::prop_section(
      page_margins = officer::page_mar(
        top = 0.5, bottom = 0.5, left = 0.5, right = 0.5,
        header = 0.3, footer = 0.3, gutter = 0
      )
    )
  )
  doc <- body_add_par(doc, "Manuscript Draft: Methods and Results", style = "heading 1")
  doc <- body_add_par(doc, paste0(
    "PAD Major Lower-Extremity Amputation and Non-Home Discharge: ",
    "External Validation of the Iannuzzi 2020 Score, the Subramaniam mFI-5, ",
    "and the Kraiss 2022 sVQI-FS"
  ), style = "Normal")
  doc <- body_add_par(doc, paste("Date:", format(Sys.Date(), "%Y-%m-%d")), style = "Normal")
  doc <- body_add_par(doc, "", style = "Normal")

  doc <- body_add_par(doc, section_major("Methods"), style = "heading 2")
  doc <- body_add_par(doc, section_num("Data source"), style = "heading 3")
  doc <- body_add_par(doc, paste0(
    "\tThis analysis used patient-level data mapped to the Observational Medical Outcomes Partnership ",
    "Common Data Model (OMOP CDM",
    if (nzchar(cdm_version_str))        paste0("; CDM version: ", cdm_version_str)        else "",
    if (nzchar(vocabulary_version_str)) paste0("; vocabulary release: ", vocabulary_version_str) else "",
    "). The study window spanned ",
    if (!is.null(config$study_start_date)) format(as.Date(config$study_start_date), "%B %d, %Y") else "N/A",
    " to ",
    if (!is.null(config$study_end_date))   format(as.Date(config$study_end_date),   "%B %d, %Y") else "N/A",
    ". Full data source metadata are reported in Supplemental Table S1; the analytic ",
    "implementation and its portability to other OMOP CDM v5 sources are described under ",
    "Implementation and code deployment, below."
  ), style = "Normal")
  doc <- body_add_par(doc, section_num("Target and outcome cohort definitions"), style = "heading 3")
  doc <- body_add_par(doc, paste0(
    "\tThe target cohort comprised adults aged 18 years or older who underwent an inpatient ",
    "major lower-extremity amputation. The index event was defined by six OMOP procedure ",
    "concept ancestors and all descendants via the concept_ancestor table: above-knee ",
    "amputation (concept 4195136), amputation through tibia and fibula / below-knee ",
    "(concept 4338257), through-knee amputation (concept 4143795), hip disarticulation ",
    "(concept 4242396), ankle disarticulation (concept 4264289), and hemipelvectomy ",
    "(concept 36675618). At least one of the following indications was required to be ",
    "documented on or before the index date: peripheral arterial disease (SNOMED 399957001 ",
    "and descendants), diabetes mellitus (SNOMED 73211009 and descendants), or a ",
    "lower-extremity wound (ulcers, open wounds, gangrene, soft-tissue infection, ",
    "osteomyelitis, or diabetic foot). These eligibility criteria exclude traumatic, ",
    "burn, and oncologic amputations. Patients were required to have at least ",
    config$min_prior_observation_days %||% 1,
    " day(s) of prior observation before the index date ",
    "(a minimal requirement that accommodates emergency presentations). ",
    "All patients were enrolled from inpatient visits (OMOP visit_concept_id 9201) only."
  ), style = "Normal")
  doc <- body_add_par(doc, paste0(
    "\tThe outcome was non-home discharge (NHD) at the end of the index hospitalization, defined as ",
    "any unambiguously non-home discharge destination. Discharge destination was ascertained from ",
    "visit_occurrence.discharged_to_concept_id using a dynamic vocabulary query resolved at runtime. ",
    "Non-home destinations include skilled nursing facility (SNF), inpatient rehabilitation ",
    "facility (IRF), long-term acute care (LTAC), hospice, and other institutional care. Hospice is ",
    "classified as a single non-home category regardless of setting; a known limitation is that ",
    "this does not distinguish home hospice (UB-04 NUBC code 50) from facility-based hospice ",
    "(code 51), which the Iannuzzi 2020 score's own NHD definition (rehabilitation or SNF only, ",
    "excluding hospice and LTAC entirely — see Supplemental Table S2) does not need to resolve; ",
    "this distinction was not implemented because the current synthetic cohort does not generate ",
    "hospice discharges to validate the split against, and should be addressed before this ",
    "classification is relied on with a real-data cohort that does include hospice discharges. ",
    "NHD is a complete-case classification, not an exclusion: every patient in the target cohort ",
    "is retained in both the NHD rate's numerator and denominator, and is classified as NOT ",
    "non-home discharge (i.e., presumed home) whenever the destination cannot be confirmed as ",
    "non-home. Concretely: patients with null, zero, or unresolved discharge_to_concept_id ",
    "(mapping to 'No matching concept' or equivalent in the local vocabulary) are classified as ",
    "not-NHD because the true discharge destination could not be confirmed non-home — this is a ",
    "conservative default that may under-count true NHD events among these patients, and is noted ",
    "as a limitation. Patients whose UB-04 source code was 'HO' (Home) are classified as home ",
    "discharges (not-NHD); where the ETL had additionally assigned a conflicting non-home ",
    "concept_id to these visits, the source code is treated as authoritative. Patients with source ",
    "code 'AM' (against medical advice) are likewise classified as home discharges (not-NHD). ",
    "The outcome was attributed to the index hospitalization. The dataset contained ",
    if (!is.null(person_level)) length(unique(person_level$subject_id)) else "N",
    " patients with at least one qualifying amputation within the study window."
  ), style = "Normal")

  # ---------------------------------------------------------------------------
  # Methods: implementation and code deployment.
  #
  # Added when this study was ported to Strategus. The Methods previously
  # described only the statistical procedure, which left a reader unable to tell
  # which artefacts are network-portable OHDSI cohort definitions and which are
  # local SQL — a distinction that matters for reproducing this analysis at
  # another site, and one this study cannot avoid because its outcome is not
  # expressible in Circe.
  # ---------------------------------------------------------------------------
  doc <- body_add_par(doc, section_num("Implementation and code deployment"), style = "heading 3")
  doc <- body_add_par(doc, paste0(
    "\tThe analysis was implemented as an OHDSI Strategus (v1.5.0) study package, so that the ",
    "same code and cohort definitions can be executed unmodified at any site holding an OMOP CDM ",
    "v5 instance. Cohort construction, cohort diagnostics, and baseline characterization are ",
    "delegated to standard HADES modules — CohortGenerator, CohortDiagnostics, and ",
    "Characterization — invoked from a single declarative analysis specification. Cohorts are ",
    "specified as OHDSI Circe JSON expressions and are rendered to dialect-specific SQL by CirceR ",
    "at execution time, so no site-specific SQL editing is required. All schema and table names ",
    "are supplied at run time; no credentials or institution-specific identifiers are embedded in ",
    "the analytic code."
  ), style = "Normal")
  doc <- body_add_par(doc, paste0(
    "\tPredictor ascertainment is likewise cohort-based rather than expressed as ad hoc queries ",
    "against the CDM domain tables. Each component of each risk score is resolved to a named ",
    "cohort definition, and a single mapping table links every published score item to the cohort ",
    "that operationalizes it, together with that item's point value and its lookback window. ",
    "Where an equivalent cohort definition already existed in the investigators' shared phenotype ",
    "library it was reused unchanged; the remainder were authored for this study and are named and ",
    "versioned for later contribution to that library. Two consequences of this design are ",
    "reported transparently in the Limitations, because they affect what the scores measure: some ",
    "reused definitions are broader than the corresponding published score item, and some cannot ",
    "carry a lookback window."
  ), style = "Normal")
  doc <- body_add_par(doc, paste0(
    "\tThree elements fall outside what the Strategus modules can express and are implemented as ",
    "documented extensions. First, the non-home discharge outcome is derived from ",
    "visit_occurrence.discharged_to_concept_id, an attribute the Circe cohort-definition schema ",
    "does not support; it is therefore implemented as hand-authored, parameterized SQL that is ",
    "applied to the generated cohort table before any analysis reads it, and is protected by an ",
    "automated check that halts the pipeline if the definition is ever silently replaced. Second, ",
    "application of the three published scoring rules is performed by a study-specific step, ",
    "because the Strategus prediction module fits new models rather than applying fixed published ",
    "weights. Third, three predictors requiring arithmetic on measurement values — body mass index ",
    "computed from height and weight, and unit normalization of laboratory results — are computed ",
    "in analytic code rather than as cohort definitions. Every such deviation is recorded in the ",
    "study repository alongside the definition it applies to."
  ), style = "Normal")
  doc <- body_add_par(doc, paste0(
    "\tTo verify that moving predictor ascertainment from direct CDM queries to cohort definitions ",
    "did not change what was measured, both implementations are retained and an automated ",
    "regression test scores every instrument twice — once by each route — and compares the ",
    "resulting per-patient predictor values. Predictors whose definitions are semantically ",
    "identical between the two routes are required to agree exactly; predictors that are expected ",
    "to differ, because the reused cohort definition cannot carry a lookback window, are quantified ",
    "rather than suppressed, and the magnitude of each difference is reported. A second automated ",
    "check confirms that each authored cohort's concept set still matches the concept set recorded ",
    "for that score component, so that a later revision of a shared definition cannot silently ",
    "change this study's predictors."
  ), style = "Normal")

  doc <- body_add_par(doc, section_num("Risk score evaluation"), style = "heading 3")

  # --- Transportability framing ---------------------------------------------
  # Peer review flagged that "external validation" overstates what is being done
  # for two of the three scores: only Iannuzzi 2020 predicts NHD, and none was
  # derived in an amputation population. This paragraph states the actual aim.
  doc <- body_add_par(doc, paste0(
    "\tNo published risk score exists for non-home discharge after major lower-extremity ",
    "amputation. Each instrument evaluated here was derived for a different outcome, a ",
    "different population, or both: the Iannuzzi 2020 score predicts NHD but was derived in ",
    "patients undergoing elective lower extremity bypass; the Subramaniam mFI-5 is a general ",
    "frailty index derived to predict mortality, postoperative complications, and unplanned ",
    "readmission rather than discharge destination; and the Kraiss 2022 simple VQI Frailty Score ",
    "(sVQI-FS) was derived to predict 9-month mortality in non-emergent vascular surgery cases. ",
    "We therefore evaluated whether any of these existing instruments transports to NHD ",
    "prediction in amputation patients. This is a screening step, undertaken as a benchmark ",
    "before committing to de novo model development; it is not an external validation of the ",
    "scores against the outcomes and populations for which they were designed."
  ), style = "Normal")

  n_scores <- 1L + (!is.null(person_level_mfi5)) + (!is.null(person_level_vqifs))
  score_word <- c("One", "Two", "Three")[min(n_scores, 3)]

  doc <- body_add_par(doc, paste0(
    "\t", score_word, " published integer risk scores were evaluated. ",
    "The Iannuzzi 2020 NHD integer risk score (range 0–18 points) predicts non-home discharge ",
    "using age, the sex/race interaction, ambulatory status, tissue loss, anemia, and ",
    "insulin-dependent diabetes. Its published sex/race term is a four-level interaction ",
    "(white male 0, white female +1, nonwhite male +2, nonwhite female +3); because those ",
    "integer weights are exactly additive, we implement them as two independent binary ",
    "components (female +1, non-White +2), which reproduces the published point totals exactly ",
    "rather than ignoring the interaction. ",
    if (!is.null(person_level_mfi5)) paste0(
      "The Subramaniam 2018 modified Frailty Index — 5-item (mFI-5, range 0–5 points) assigns ",
      "one point each for diabetes mellitus, COPD, congestive heart failure, hypertension ",
      "requiring medication, and dependent functional status. "
    ) else "",
    if (!is.null(person_level_vqifs)) paste0(
      # NOTE: this list must match covariates/covariates_vqifs.csv exactly — ten items,
      # NOT the paper's eleven. Non-home residence is deliberately omitted (near-circular
      # with the NHD outcome; no clean standard concept in this vocabulary build). An
      # earlier revision of this paragraph listed all eleven and so contradicted both
      # Table 3c and Supplemental Table S2, which are generated from the CSV.
      "The Kraiss 2022 simple VQI Frailty Score (sVQI-FS, implemented here as a 0–10 ",
      "integer point sum) assigns one point each for hypertension, congestive heart ",
      "failure, coronary artery disease, peripheral vascular disease, diabetes, COPD, ",
      "renal impairment, anemia, underweight status, and non-ambulatory status. ",
      # Equal-weighting disclosure: the source authors explicitly disavow the equally
      # weighted form for THEIR outcome (9-month mortality), so naming the score without
      # this clause would let a reader assume the differentially weighted VQI-FS.
      "The eleventh published item — non-home residence — is omitted because it is ",
      "near-circular with the non-home discharge outcome and has no clean standard ",
      "concept in this vocabulary build. We evaluate the equally weighted form of the ",
      "score; the authors' preferred differentially weighted VQI-FS could not be applied ",
      "because it requires nomogram regression coefficients together with a ",
      "procedure-specific risk term whose categories (carotid endarterectomy, carotid ",
      "artery stenting, endovascular and open abdominal aortic aneurysm repair, ",
      "peripheral vascular intervention, suprainguinal and infrainguinal bypass) do not ",
      "include major amputation. The implications of both departures are addressed in ",
      "the Limitations discussion. "
    ) else "",
    "For each score, covariates were extracted from the OMOP CDM using component-specific ",
    "lookback windows relative to the index amputation date (Tables 3a–3c). ",
    "A patient meeting the minimum event threshold for a given component received the full integer ",
    "point value; those below threshold received zero. Absent records were treated as zero ",
    "evidence for that component.",
    # The source publication suppresses the score when fewer than five of its frailty
    # domains have data; we score every patient instead, so this must be disclosed rather
    # than left implicit in the missing_is_negative columns.
    if (!is.null(person_level_vqifs)) paste0(
      " The source publication does not compute the sVQI-FS when fewer than five frailty ",
      "domains have available data; because component ascertainment here is based on the ",
      "presence or absence of coded records rather than on registry fields with an explicit ",
      "missing state, no equivalent suppression rule was applied and every patient in the ",
      "cohort received a score."
    ) else ""
  ), style = "Normal")

  # Model specifications: two per score (raw/lookup + recalibrated).
  spec_n    <- if (!is.null(person_level_vqifs)) 6L else if (!is.null(person_level_mfi5)) 4L else 2L
  spec_word <- c("2" = "Two", "4" = "Four", "6" = "Six")[[as.character(spec_n)]]

  doc <- body_add_par(doc, paste0(
    "\t", spec_word, " model specifications were evaluated. ",
    "(1) Iannuzzi 2020 — published lookup: predicted NHD probabilities drawn from the original ",
    "score-to-risk table without any local refitting. Because the published table's tail is ",
    "non-monotonic in the sparse high-score cells, the mapping used here is a monotone ",
    "(isotonic) fit pooling the derivation and validation columns of the source publication's ",
    "Table III; both raw columns are retained in covariates/risk_lookup.csv for provenance. ",
    "(2) Iannuzzi 2020 — temporal recalibration: a logistic regression of the total integer ",
    "score on the observed binary NHD outcome, fitted on the chronologically earlier half of the ",
    "cohort (training set) and evaluated on the later half (test set). This quantifies the ",
    "improvement in calibration obtainable by re-anchoring the score's probability scale to the ",
    "local event rate. ",
    if (!is.null(person_level_mfi5)) paste0(
      "(3) mFI-5 — raw score and (4) mFI-5 — temporal recalibration: the same procedure applied ",
      "to the mFI-5 raw integer score. Because no published NHD probability mapping exists for ",
      "the mFI-5, recalibration is required to obtain predicted probabilities. "
    ) else "",
    if (!is.null(person_level_vqifs)) paste0(
      "(5) sVQI-FS — raw score and (6) sVQI-FS — temporal recalibration: likewise applied to the ",
      "sVQI-FS raw integer score, for which no published NHD probability mapping exists either. "
    ) else "",
    "Because logistic recalibration of a single predictor is a strictly monotone transform, the ",
    "raw-score and recalibrated specifications of a given score share the same discrimination by ",
    "construction; AUROC and AUPRC are therefore computed once per score and reported in both ",
    "columns."
  ), style = "Normal")

  doc <- body_add_par(doc, paste0(
    "\tPatients were sorted chronologically by index amputation date and divided at the midpoint ",
    "(floor(n / 2) training rows). Each recalibration logistic regression was fitted exclusively ",
    "on the training set (earlier half), and every specification that involves local fitting — ",
    "the recalibrated and raw-score models — was evaluated on the test set (later half) only. ",
    "This prevents optimistic bias from evaluating a locally fitted model on the data used to fit ",
    "it, and provides an estimate of prospective performance on future patients from the same ",
    "source. The Iannuzzi published-lookup specification is the exception: it is a fixed external ",
    "mapping with no parameters estimated from these data, so restricting it to the test half ",
    "would discard half the available events without removing any bias. It is therefore evaluated ",
    "over the full cohort. Table 4 states the evaluation set for every column."
  ), style = "Normal")
  # Append the split sample sizes when split_info.csv was found. The split is
  # identical across scores by construction (same patients, same index dates),
  # so report it once rather than repeating a near-identical sentence per model.
  split_sent <- NULL
  for (si in list(split_info_iannuzzi, split_info_mfi5, split_info_vqifs)) {
    if (is.null(split_sent)) split_sent <- split_sentence(si, NULL)
  }
  if (!is.null(split_sent)) doc <- body_add_par(doc, split_sent, style = "Normal")
  doc <- body_add_par(doc, paste0(
    "\tDiscrimination was summarised using the area under the receiver operating characteristic curve ",
    "(AUROC) and the area under the precision-recall curve (AUPRC), each with 95% bootstrap ",
    "percentile confidence intervals (B = 500 resamples). Calibration was assessed using ",
    "the Brier score, expected calibration error (ECE), calibration intercept, and calibration slope, ",
    "each with 95% bootstrap percentile CIs. ECE was computed as the probability-weighted mean ",
    "absolute difference between mean predicted and observed NHD rates across quantile-based bins. ",
    "Calibration plots compare mean predicted risk versus observed event rate within each bin; the ",
    "dashed diagonal represents perfect calibration."
  ), style = "Normal")
  doc <- body_add_par(doc, section_num("Subgroup analysis and bias assessment"), style = "heading 3")
  doc <- body_add_par(doc, paste0(
    "\tModel calibration was assessed across prespecified patient subgroups to identify populations ",
    "in which the recalibrated risk scores may systematically over- or underestimate observed non-home ",
    "discharge risk. ",
    "Subgroups evaluated included biological sex, race, ethnicity, age group (<65, 65–74, ",
    "≥75 years), amputation level (above-knee, below-knee, other), and calendar year of the ",
    "index procedure. Expected calibration error (ECE) was computed within each subgroup as the ",
    "weighted mean absolute difference between grouped predicted and observed event rates across ",
    "quantile-based bins. Uncertainty was quantified using 200 bootstrap resamples (percentile ",
    "95% CI). Subgroup levels with fewer than 10 observed NHD events were suppressed to avoid ",
    "unreliable estimates. Subgroup ECE is computed on the same temporal test partition as the ",
    "overall ECE reported in Table 4, so the two are directly comparable. ",
    "Results are presented separately for the ",
    paste(c(
      paste0("Iannuzzi 2020 (Recalibrated) model in Supplemental Table ",
             supp("subgroup_tbl_iannuzzi"), " and Supplemental Figure ",
             supp("subgroup_fig_iannuzzi")),
      if (!is.null(person_level_mfi5))
        paste0("the mFI-5 (Recalibrated) model in Supplemental Table ",
               supp("subgroup_tbl_mfi5"), " and Supplemental Figure ",
               supp("subgroup_fig_mfi5")),
      if (!is.null(person_level_vqifs))
        paste0("the sVQI-FS (Recalibrated) model in Supplemental Table ",
               supp("subgroup_tbl_vqifs"), " and Supplemental Figure ",
               supp("subgroup_fig_vqifs"))
    ), collapse = "; "),
    "."
  ), style = "Normal")

  doc <- add_doc_page_break(doc)
  doc <- body_add_par(doc, section_major("Results"), style = "heading 2")

  # ---- Table 1: Demographics -----------------------------------------------
  doc <- body_add_par(doc, section_num("Cohort characteristics"), style = "heading 3")
  doc <- body_add_par(doc, paste0(
    "The final target cohort included ", n_target, " patients, of whom ",
    n_outcome, " experienced non-home discharge (NHD), corresponding to an observed ",
    # 1 decimal place, matching every other percentage in this report (Table 3b/3c
    # prevalence, Table 5 tier rates, etc. all use round(..., 1)) — this line
    # previously used fmt(outcome_prev, 2), producing "35.11%" here against
    # "35.1%" everywhere else describing the identical rate.
    "NHD rate of ", fmt(outcome_prev, 1), "%."
  ), style = "Normal")
  doc <- body_add_flextable(doc, table1_ft(cohort_tbl))
  doc <- add_doc_caption(doc,
    "Table 1. Demographics and clinical characteristics of the validation cohort.",
    paste0(
      "Values are n (%) unless stated. Age is summarised as median (IQR). ",
      "Race and ethnicity are derived from OMOP person table concept fields. ",
      "Indication rows are not mutually exclusive; a patient may have more than one. ",
      "PAD = peripheral arterial disease (ancestor 317309); ",
      "diabetes mellitus (ancestor 201820); ",
      "lower-extremity wound / gangrene (25 SNOMED ancestors — ulcers, open wounds, gangrene, ",
      "soft-tissue infection, osteomyelitis, diabetic foot). ",
      "Amputation level is assigned hierarchically: AKA (ancestor 4195136), then BKA (4338257), ",
      "then Other (through-knee 4143795, hip disarticulation 4242396, ankle disarticulation 4264289, ",
      "hemipelvectomy 36675618). Amputation level rows are mutually exclusive."
    )
  )
  doc <- body_add_par(doc, "", style = "Normal")

  # ---- Table 2: NHD Patient Outcomes ----------------------------------------
  doc <- add_doc_page_break(doc)
  nhd_outcomes <- fetch_nhd_outcomes_from_omop(config, connection_details)

  # Defensive consistency check: Table 2's NHD count (from
  # fetch_nhd_outcomes_from_omop()'s own SQL, run against the outcome cohort
  # table with the same prediction_window_days bound as get_outcomes()) must
  # equal Table 1's NHD count (sum(person_level$outcome), from
  # calculate_scores()/get_outcomes()). These previously disagreed (79 vs 80)
  # because one query lacked the prediction-window bound the other applied.
  # Both queries now apply that bound identically, so they SHOULD always
  # agree going forward — this warning exists so a future edit to either
  # query that reintroduces a divergence is caught immediately rather than
  # discovered by a peer reviewer reading the generated report.
  if (!is.null(nhd_outcomes) && !is.na(nhd_outcomes$n_nhd) && !is.null(person_level)) {
    n_nhd_table1 <- sum(person_level$outcome, na.rm = TRUE)
    if (nhd_outcomes$n_nhd != n_nhd_table1) {
      warning(sprintf(
        paste0("[report] Table 1 and Table 2 NHD counts disagree (Table 1 = %d, ",
               "Table 2 = %d). These must come from the same population — check that ",
               "fetch_nhd_outcomes_from_omop()'s nhd_cte and get_outcomes() apply an ",
               "identical prediction_window_days bound."),
        n_nhd_table1, nhd_outcomes$n_nhd
      ), call. = FALSE)
    }
  }

  if (!is.null(nhd_outcomes) && !is.na(nhd_outcomes$n_nhd) && nhd_outcomes$n_nhd > 0) {
    n_nhd_denom <- nhd_outcomes$n_nhd

    los_str <- if (!is.na(nhd_outcomes$median_los)) {
      paste0(as.integer(round(nhd_outcomes$median_los)), " days",
             " (IQR: ", as.integer(round(nhd_outcomes$los_p25)),
             "\u2013", as.integer(round(nhd_outcomes$los_p75)), ")")
    } else "N/A"

    # Build static rows for NHD outcome table
    nhd_static_rows <- data.frame(
      Outcome = c(
        "Index hospitalization length of stay, median (IQR)",
        "Discharge destination",
        "90-day readmission, n (%)",
        "90-day mortality, n (%)"
      ),
      Value = c(
        los_str,
        "",
        fmt_n_pct(nhd_outcomes$n_readmission, n_nhd_denom),
        fmt_n_pct(nhd_outcomes$n_death,        n_nhd_denom)
      ),
      stringsAsFactors = FALSE
    )

    # Insert dynamic discharge destination rows after the "Discharge destination" header row.
    # Destinations are returned sorted descending by n from the SQL query.
    if (!is.null(nhd_outcomes$dest_df) && nrow(nhd_outcomes$dest_df) > 0) {
      dest_rows <- data.frame(
        Outcome = paste0("    ", nhd_outcomes$dest_df$destination),
        Value   = vapply(nhd_outcomes$dest_df$n, function(x) fmt_n_pct(x, n_nhd_denom),
                         character(1L)),
        stringsAsFactors = FALSE
      )
      # Splice: rows 1-2 | destination sub-rows | rows 3-4
      nhd_outcome_tbl <- rbind(
        nhd_static_rows[1:2, ],
        dest_rows,
        nhd_static_rows[3:4, ]
      )
    } else {
      nhd_outcome_tbl <- nhd_static_rows
    }

    # Row formatting indices
    nhd_header_rows <- which(nhd_outcome_tbl$Outcome == "Discharge destination")
    nhd_indent_rows <- grep("^    ", nhd_outcome_tbl$Outcome)

    nhd_out_ft <- flextable::flextable(nhd_outcome_tbl) |>
      flextable::bold(part = "header") |>
      flextable::fontsize(size = 10, part = "all") |>
      flextable::font(fontname = "Calibri", part = "all") |>
      flextable::bg(part = "header", bg = "#1F3864") |>
      flextable::color(part = "header", color = "white") |>
      flextable::align(align = "left", part = "all") |>
      flextable::padding(padding = 4, part = "all") |>
      flextable::bold(i = nhd_header_rows, part = "body") |>
      flextable::bg(i = nhd_header_rows, bg = "#F2F2F2", part = "body") |>
      flextable::padding(i = nhd_indent_rows, j = "Outcome",
                         padding.left = 18, part = "body") |>
      flextable::width(j = "Outcome", width = 3.5) |>
      flextable::width(j = "Value",   width = 1.5) |>
      flextable::set_table_properties(layout = "fixed")

    doc <- body_add_par(doc, section_num("NHD patient outcomes"), style = "heading 3")
    doc <- body_add_par(doc,
      paste0(
        "Among the ", n_nhd_denom, " patients who experienced non-home discharge (NHD) at ",
        "the end of the index hospitalization, Table 2 summarises key post-discharge outcomes."
      ),
      style = "Normal"
    )
    doc <- body_add_flextable(doc, nhd_out_ft)
    doc <- add_doc_caption(doc,
      "Table 2. NHD patient outcomes at the index hospitalization and within 90 days.",
      paste0(
        "Denominator is all patients with non-home discharge at the index ",
        "hospitalization (n\u00a0=\u00a0", n_nhd_denom, "). ",
        "Index hospitalization LOS: length of the inpatient visit (visit_concept_id 9201) ",
        "containing the index procedure date, calculated as DATEDIFF(DAY, visit_start_date, ",
        "COALESCE(visit_end_date, visit_start_date)). ",
        "Discharge destination: destination concept name from visit_occurrence.discharged_to_concept_id ",
        "joined to the vocabulary concept table; destinations with fewer than 11 patients are suppressed. ",
        "90-day readmission: any inpatient visit (visit_concept_id 9201) starting after the index ",
        "hospitalization's discharge date and within 90 days of that discharge date (anchored on ",
        "discharge, not admission, so patients with a longer index stay are not given a shorter ",
        "effective follow-up window). ",
        "90-day mortality: any death record on or after the discharge date and within 90 days of it."
      )
    )
    doc <- body_add_par(doc, "", style = "Normal")
    message("[report] Table 2 (NHD outcomes) added.")
  }

  # ---- Figure 1: NHD rate by year (placed after Tables 1-2) ---------------
  if (!is.null(nhd_year_plot_file) && file.exists(nhd_year_plot_file)) {
    doc <- body_add_img(doc, src = nhd_year_plot_file, width = 5.5, height = 3.5)
    doc <- add_doc_caption(doc,
      "Figure 1. Non-home discharge (NHD) rate by disposition type and amputation year.",
      paste0(
        "Non-home discharge (NHD) rate (%) by calendar year of index amputation, ",
        "stratified by discharge disposition (SNF = Skilled Nursing Facility; ",
        "IRF = Inpatient Rehabilitation Facility; LTAC = Long-term Acute Care; Hospice; Other NHD). ",
        "Stacked bands show the cumulative NHD rate by disposition; the top edge of the uppermost ",
        "band equals the overall NHD rate for that year. ",
        "Points and lines trace the top edge of each disposition's contribution. ",
        "Years with fewer than 11 patients are suppressed (small-cell privacy rule, matching ",
        "Table 2's discharge-destination suppression threshold). ",
        "Disposition is mapped from UB-04 discharged_to_source_value codes."
      )
    )
    doc <- body_add_par(doc, "", style = "Normal")
    message("[report] Figure 1 (NHD rate by year) added.")
  }

  # ---- Table 3a: mFI-5 component activation --------------------------------
  doc <- add_doc_page_break(doc)
  doc <- body_add_par(doc, section_num("Predictor activation"), style = "heading 3")
  if (!is.null(covariate_summary_mfi5) && nrow(covariate_summary_mfi5) > 0) {
    doc <- body_add_flextable(doc, .build_mfi5_component_table(covariate_summary_mfi5))
    doc <- add_doc_caption(doc,
      "Table 3a. Subramaniam mFI-5: component definitions and activation summary.",
      paste0(
        "The five Subramaniam 2018 mFI-5 components are listed with their ",
        "point value (each = 1 point, range 0–5), OMOP-based definition, and observed ",
        "activation rate in the validation cohort. All components use a 10-year lookback ",
        "window except congestive heart failure (30-day lookback per the original publication). ",
        "Absent records are treated as true negatives (missing_is_negative = TRUE for all components)."
      )
    )
    doc <- body_add_par(doc, "", style = "Normal")
    message("[report] Table 3a (mFI-5 components) added.")
  }

  # ---- Table 3b: Iannuzzi NHD component activation --------------------------
  tbl3b_data <- if (identical(config$score_type, "lasso")) {
    .build_combined_covariate_table(covariate_summary)
  } else {
    .build_combined_component_table(covariate_summary)
  }
  doc <- body_add_flextable(doc, tbl3b_data)
  doc <- add_doc_caption(doc,
    "Table 3b. Iannuzzi 2020 NHD score: component definitions and activation summary.",
    paste0(
      "The nine components of the Iannuzzi 2020 non-home discharge integer risk score ",
      "are listed with point values (range 0–18), OMOP-based definitions, and observed ",
      "activation rates in the validation cohort. ",
      "For the anemia component (Hgb < 10 g/dL), absent measurement records are treated as ",
      "unknown (missing_is_negative = FALSE). All other components treat absent records as ",
      "true negatives."
    )
  )
  doc <- body_add_par(doc, "", style = "Normal")
  message("[report] Table 3b (Iannuzzi components) added.")

  # ---- Table 3c: sVQI-FS component activation --------------------------------
  if (!is.null(covariate_summary_vqifs) && nrow(covariate_summary_vqifs) > 0) {
    doc <- body_add_flextable(doc, .build_vqifs_component_table(covariate_summary_vqifs))
    doc <- add_doc_caption(doc,
      "Table 3c. Kraiss 2022 sVQI-FS: component definitions and activation summary.",
      paste0(
        "Ten of the eleven Kraiss 2022 simple VQI Frailty Score components are listed ",
        "with their point value (each = 1 point, range 0–10 as implemented here; ",
        "non-home residence is omitted, see covariates/covariates_vqifs.csv header), ",
        "OMOP-based definition, and observed activation rate in the validation cohort. ",
        # Updated to match the 2026-07-26 switch from diagnosis proxies to the paper's
        # own lab thresholds (query_renal_impairment_covariate_counts() and
        # query_anemia_covariate_counts(sex_specific = TRUE) in R/risk_score_pipeline.R).
        # This footnote previously still claimed diagnosis proxies, understating fidelity.
        "All components use a 10-year lookback window. Renal impairment (serum creatinine ",
        "> 1.8 mg/dL or dialysis) and anemia (hemoglobin < 13 g/dL in males, < 12 g/dL in ",
        "females) apply the source publication's own laboratory thresholds rather than ",
        "diagnosis proxies; underweight uses a direct BMI < 18.5 measurement threshold. ",
        "Peripheral vascular disease remains a diagnosis-based proxy rather than the ",
        "paper's ankle-brachial index < 0.7 or prior arterial intervention definition. ",
        "Absent records are treated as true ",
        "negatives (missing_is_negative = TRUE) for all components except underweight and ",
        "anemia, where an unmeasured BMI or hemoglobin value is treated as unknown rather ",
        "than a true negative."
      )
    )
    doc <- body_add_par(doc, "", style = "Normal")
    message("[report] Table 3c (sVQI-FS components) added.")
  }

  # ---- Table 4: Dual-model performance comparison ---------------------------
  doc <- add_doc_page_break(doc)
  doc <- body_add_par(doc, section_num("Model performance"), style = "heading 3")
  # Build a dynamic caption note about the temporal split if metadata is available.
  split_note_iannuzzi <- if (!is.null(split_info_iannuzzi) && nrow(split_info_iannuzzi) > 0) {
    sprintf("Iannuzzi 2020: metrics reported on the test set (n\u2009=\u2009%d; after %s).",
            split_info_iannuzzi$n_test[1], split_info_iannuzzi$split_date[1])
  } else NULL
  split_note_mfi5 <- if (!is.null(split_info_mfi5) && nrow(split_info_mfi5) > 0) {
    sprintf("mFI-5: metrics reported on the test set (n\u2009=\u2009%d; after %s).",
            split_info_mfi5$n_test[1], split_info_mfi5$split_date[1])
  } else NULL
  split_note_vqifs <- if (!is.null(split_info_vqifs) && nrow(split_info_vqifs) > 0) {
    sprintf("sVQI-FS: metrics reported on the test set (n\u2009=\u2009%d; after %s).",
            split_info_vqifs$n_test[1], split_info_vqifs$split_date[1])
  } else NULL
  split_caption <- paste(Filter(Negate(is.null), list(split_note_iannuzzi, split_note_mfi5, split_note_vqifs)),
                         collapse = " ")

  perf_tbl <- data.frame(
    Metric = c(
      "AUROC",
      "AUPRC",
      "Brier score",
      "Expected calibration error",
      "Calibration intercept",
      "Calibration slope"
    ),
    "Iannuzzi Lookup" = c(
      paste0(fmt(metric_value("AUROC",                "lookup")), " ", metric_ci("AUROC",                "lookup")),
      paste0(fmt(metric_value("AUPRC",                "lookup")), " ", metric_ci("AUPRC",                "lookup")),
      paste0(fmt(metric_value("Brier",                "lookup")), " ", metric_ci("Brier",                "lookup")),
      paste0(fmt(metric_value("ECE",                  "lookup")), " ", metric_ci("ECE",                  "lookup")),
      paste0(fmt(metric_value("CalibrationIntercept", "lookup")), " ", metric_ci("CalibrationIntercept", "lookup")),
      paste0(fmt(metric_value("CalibrationSlope",     "lookup")), " ", metric_ci("CalibrationSlope",     "lookup"))
    ),
    "Iannuzzi Recalibrated" = c(
      paste0(fmt(metric_value("AUROC",                "recalibrated")), " ", metric_ci("AUROC",                "recalibrated")),
      paste0(fmt(metric_value("AUPRC",                "recalibrated")), " ", metric_ci("AUPRC",                "recalibrated")),
      paste0(fmt(metric_value("Brier",                "recalibrated")), " ", metric_ci("Brier",                "recalibrated")),
      paste0(fmt(metric_value("ECE",                  "recalibrated")), " ", metric_ci("ECE",                  "recalibrated")),
      paste0(fmt(metric_value("CalibrationIntercept", "recalibrated")), " ", metric_ci("CalibrationIntercept", "recalibrated")),
      paste0(fmt(metric_value("CalibrationSlope",     "recalibrated")), " ", metric_ci("CalibrationSlope",     "recalibrated"))
    ),
    "mFI-5 Raw" = if (!is.null(metrics_mfi5)) c(
      paste0(fmt(metric_value_mfi5("AUROC", "score_only")), " ", metric_ci_mfi5("AUROC", "score_only")),
      paste0(fmt(metric_value_mfi5("AUPRC", "score_only")), " ", metric_ci_mfi5("AUPRC", "score_only")),
      "N/A",
      "N/A",
      "N/A",
      "N/A"
    ) else rep("—", 6),
    "mFI-5 Recalibrated" = if (!is.null(metrics_mfi5)) c(
      paste0(fmt(metric_value_mfi5("AUROC",                "recalibrated")), " ", metric_ci_mfi5("AUROC",                "recalibrated")),
      paste0(fmt(metric_value_mfi5("AUPRC",                "recalibrated")), " ", metric_ci_mfi5("AUPRC",                "recalibrated")),
      paste0(fmt(metric_value_mfi5("Brier",                "recalibrated")), " ", metric_ci_mfi5("Brier",                "recalibrated")),
      paste0(fmt(metric_value_mfi5("ECE",                  "recalibrated")), " ", metric_ci_mfi5("ECE",                  "recalibrated")),
      paste0(fmt(metric_value_mfi5("CalibrationIntercept", "recalibrated")), " ", metric_ci_mfi5("CalibrationIntercept", "recalibrated")),
      paste0(fmt(metric_value_mfi5("CalibrationSlope",     "recalibrated")), " ", metric_ci_mfi5("CalibrationSlope",     "recalibrated"))
    ) else rep("—", 6),
    "sVQI-FS Raw" = if (!is.null(metrics_vqifs)) c(
      paste0(fmt(metric_value_vqifs("AUROC", "score_only")), " ", metric_ci_vqifs("AUROC", "score_only")),
      paste0(fmt(metric_value_vqifs("AUPRC", "score_only")), " ", metric_ci_vqifs("AUPRC", "score_only")),
      "N/A",
      "N/A",
      "N/A",
      "N/A"
    ) else rep("—", 6),
    "sVQI-FS Recalibrated" = if (!is.null(metrics_vqifs)) c(
      paste0(fmt(metric_value_vqifs("AUROC",                "recalibrated")), " ", metric_ci_vqifs("AUROC",                "recalibrated")),
      paste0(fmt(metric_value_vqifs("AUPRC",                "recalibrated")), " ", metric_ci_vqifs("AUPRC",                "recalibrated")),
      paste0(fmt(metric_value_vqifs("Brier",                "recalibrated")), " ", metric_ci_vqifs("Brier",                "recalibrated")),
      paste0(fmt(metric_value_vqifs("ECE",                  "recalibrated")), " ", metric_ci_vqifs("ECE",                  "recalibrated")),
      paste0(fmt(metric_value_vqifs("CalibrationIntercept", "recalibrated")), " ", metric_ci_vqifs("CalibrationIntercept", "recalibrated")),
      paste0(fmt(metric_value_vqifs("CalibrationSlope",     "recalibrated")), " ", metric_ci_vqifs("CalibrationSlope",     "recalibrated"))
    ) else rep("—", 6),
    check.names     = FALSE,
    stringsAsFactors = FALSE
  )

  has_vqifs_metrics <- !is.null(metrics_vqifs)

  perf_ft <- flextable::flextable(perf_tbl) |>
    flextable::set_header_labels(
      Metric                   = "Metric",
      "Iannuzzi Lookup"        = "Iannuzzi 2020\n(Lookup, 95% CI)",
      "Iannuzzi Recalibrated"  = "Iannuzzi 2020\n(Recalibrated, 95% CI)",
      "mFI-5 Raw"              = "mFI-5\n(Raw score, 95% CI)",
      "mFI-5 Recalibrated"     = "mFI-5\n(Recalibrated, 95% CI)",
      "sVQI-FS Raw"            = "sVQI-FS\n(Raw score, 95% CI)",
      "sVQI-FS Recalibrated"   = "sVQI-FS\n(Recalibrated, 95% CI)"
    ) |>
    flextable::bold(part = "header") |>
    flextable::fontsize(size = 9, part = "all") |>
    flextable::font(fontname = "Calibri", part = "all") |>
    flextable::bg(part = "header", bg = "#1F3864") |>
    flextable::color(part = "header", color = "white") |>
    flextable::padding(padding = 3, part = "all") |>
    flextable::width(j = "Metric",                  width = 1.3) |>
    flextable::width(j = "Iannuzzi Lookup",         width = 1.0) |>
    flextable::width(j = "Iannuzzi Recalibrated",   width = 1.0) |>
    flextable::width(j = "mFI-5 Raw",               width = 0.9) |>
    flextable::width(j = "mFI-5 Recalibrated",      width = 1.0) |>
    flextable::width(j = "sVQI-FS Raw",             width = 0.9) |>
    flextable::width(j = "sVQI-FS Recalibrated",    width = 1.0) |>
    flextable::align(j = c("Iannuzzi Lookup", "Iannuzzi Recalibrated",
                           "mFI-5 Raw", "mFI-5 Recalibrated",
                           "sVQI-FS Raw", "sVQI-FS Recalibrated"),
                     align = "center", part = "all") |>
    flextable::set_table_properties(layout = "fixed")

  doc <- body_add_flextable(doc, perf_ft)
  doc <- add_doc_caption(doc,
    paste0(
      "Table 4. Model performance: discrimination and calibration metrics for ",
      "Iannuzzi 2020 (published lookup), Subramaniam mFI-5 (raw integer score), and ",
      "Kraiss 2022 sVQI-FS (raw integer score)."
    ),
    paste0(
      if (has_vqifs_metrics) "Six model specifications are compared. " else "Four model specifications are compared. ",
      "Iannuzzi 2020 (Lookup): predicted probabilities drawn from a refitted ",
      "score-to-risk lookup (monotone isotonic fit, resolving the non-monotonic inversion in ",
      "the published Table III). ",
      "Iannuzzi 2020 (Recalibrated): logistic regression of the total Iannuzzi integer score on the ",
      "observed NHD outcome, fitted on the chronologically earlier half of the cohort ",
      "(training set) and evaluated on the later half (test set). ",
      "mFI-5 (Raw score): discrimination from the raw integer score (0–5) only; ",
      "no published NHD probability mapping exists so calibration metrics are N/A. ",
      "mFI-5 (Recalibrated): logistic recalibration of the mFI-5 raw score on the same ",
      "temporal training split as above; all four metrics reported on the test set. ",
      if (has_vqifs_metrics) paste0(
        "sVQI-FS (Raw score): discrimination from the raw integer score only; ",
        "no published NHD probability mapping exists so calibration metrics are N/A. ",
        "sVQI-FS (Recalibrated): logistic recalibration of the sVQI-FS raw score on the same ",
        "temporal training split as above; all four metrics reported on the test set. "
      ) else "",
      if (nchar(split_caption) > 0) paste0(split_caption, " ") else "",
      "N/A = metric not applicable for this specification. ",
      "95% CI = 95% bootstrap percentile confidence interval (B = 500 resamples). ",
      "— indicates CI not available."
    )
  )
  doc <- body_add_par(doc, "", style = "Normal")
  message("[report] Table 4 (", if (has_vqifs_metrics) "six" else "four", "-model performance) added.")

  # ---- Figure 2: Combined ROC curve (Iannuzzi + mFI-5 + sVQI-FS) ------------
  dual_roc_file <- NULL
  if (!is.null(person_level_mfi5) && "outcome" %in% names(person_level_mfi5)) {
    # Use the raw mFI-5 integer score (normalised 0–1) for the ROC curve;
    # no published probability mapping exists so recalibration is excluded.
    mfi5_roc_p <- if ("total_score" %in% names(person_level_mfi5)) {
      mx <- max(person_level_mfi5$total_score, na.rm = TRUE)
      if (mx > 0) person_level_mfi5$total_score / mx else person_level_mfi5$total_score
    } else NULL
    mfi5_roc_y <- person_level_mfi5$outcome

    # Same normalisation approach for sVQI-FS raw integer score, when available.
    vqifs_roc_p <- NULL
    vqifs_roc_y <- NULL
    if (!is.null(person_level_vqifs) && "outcome" %in% names(person_level_vqifs) &&
        "total_score" %in% names(person_level_vqifs)) {
      mx_v <- max(person_level_vqifs$total_score, na.rm = TRUE)
      vqifs_roc_p <- if (mx_v > 0) person_level_vqifs$total_score / mx_v else person_level_vqifs$total_score
      vqifs_roc_y <- person_level_vqifs$outcome
    }

    if (!is.null(mfi5_roc_p)) {
      dual_roc_file <- tryCatch(
        .save_dual_roc_plot(
          y1 = roc_y, p1 = roc_p,
          y2 = mfi5_roc_y, p2 = mfi5_roc_p,
          y3 = vqifs_roc_y, p3 = vqifs_roc_p,
          label1 = "Iannuzzi 2020 (lookup)",
          label2 = "mFI-5 (raw score)",
          label3 = "sVQI-FS (raw score)",
          auc1   = metric_value("AUROC", "lookup"),
          auc2   = metric_value_mfi5("AUROC", "score_only"),
          auc3   = metric_value_vqifs("AUROC", "score_only"),
          output_folder = temp_figure_dir
        ),
        error = function(e) {
          message("[report] Dual ROC plot skipped: ", conditionMessage(e))
          NULL
        }
      )
    }
  }

  has_vqifs_roc <- !is.null(person_level_vqifs) && "outcome" %in% names(person_level_vqifs)

  fig2_file    <- if (!is.null(dual_roc_file) && file.exists(dual_roc_file)) dual_roc_file else roc_plot_file
  fig2_caption <- if (!is.null(dual_roc_file) && file.exists(dual_roc_file) && has_vqifs_roc) {
    paste0("ROC curves for the Iannuzzi 2020 NHD score (published lookup, blue solid), ",
           "the Subramaniam mFI-5 (raw integer score 0–5, red dashed), and the Kraiss 2022 ",
           "sVQI-FS (raw integer score, orange dot-dash) predicting non-home discharge. For mFI-5 ",
           "and sVQI-FS, the raw score is normalised to [0, 1] for curve plotting. ",
           "AUROC with 95% bootstrap percentile CI (B = 500 resamples).")
  } else if (!is.null(dual_roc_file) && file.exists(dual_roc_file)) {
    paste0("ROC curves for the Iannuzzi 2020 NHD score (published lookup, blue solid) ",
           "and the Subramaniam mFI-5 (raw integer score 0\u20135, red dashed) predicting non-home ",
           "discharge. For mFI-5, the raw score is normalised to [0, 1] for curve plotting. ",
           "AUROC with 95% bootstrap percentile CI (B\u2009=\u2009500 resamples).")
  } else {
    paste0("ROC curve for the Iannuzzi 2020 lookup model predicting non-home ",
           "discharge. AUROC with 95% bootstrap percentile CI (B = 500 resamples). ",
           "Dashed diagonal = no-discrimination reference line.")
  }

  if (!is.null(fig2_file) && file.exists(fig2_file)) {
    doc <- body_add_img(doc, src = fig2_file, width = 5.0, height = 4.5)
    doc <- add_doc_caption(doc, "Figure 2. Receiver operating characteristic (ROC) curves.", fig2_caption)
    doc <- body_add_par(doc, "", style = "Normal")
    message("[report] Figure 2 (ROC) added.")
  }

  # ---- Figure 3: Calibration — Iannuzzi lookup, Iannuzzi recal, mFI-5 recal ---
  # Attempt to build a triple-curve combined plot; fall back to dual (Iannuzzi only)
  # or single-curve when mFI-5 calibration data are unavailable.
  mfi5_calibration_table_path <- if (!is.null(mfi5_output_dir)) {
    file.path(mfi5_output_dir, "calibration_table_recalibrated.csv")
  } else NULL

  vqifs_calibration_table_path <- if (!is.null(vqifs_output_dir)) {
    file.path(vqifs_output_dir, "calibration_table_recalibrated.csv")
  } else NULL

  dual_cal_file <- tryCatch(
    .save_dual_calibration_plot(
      lookup_source = if (file.exists(calibration_table_lookup_path))
                        calibration_table_lookup_path
                      else NULL,
      recal_source  = if (file.exists(calibration_table_recalibrated_path))
                        calibration_table_recalibrated_path
                      else NULL,
      mfi5_source   = if (!is.null(mfi5_calibration_table_path) &&
                          file.exists(mfi5_calibration_table_path))
                        mfi5_calibration_table_path
                      else NULL,
      vqifs_source  = if (!is.null(vqifs_calibration_table_path) &&
                          file.exists(vqifs_calibration_table_path))
                        vqifs_calibration_table_path
                      else NULL,
      output_folder = temp_figure_dir,
      file_name     = "calibration_dual.png"
    ),
    error = function(e) {
      message("[report] Calibration plot skipped: ", conditionMessage(e))
      NULL
    }
  )

  if (!is.null(dual_cal_file) && file.exists(dual_cal_file)) {
    # Combined plot (up to four curves).
    has_mfi5_cal <- !is.null(mfi5_calibration_table_path) &&
                    file.exists(mfi5_calibration_table_path)
    has_vqifs_cal <- !is.null(vqifs_calibration_table_path) &&
                     file.exists(vqifs_calibration_table_path)
    fig3_title <- if (has_mfi5_cal && has_vqifs_cal)
      "Figure 3. NHD risk score calibration — four model specifications."
    else if (has_mfi5_cal)
      "Figure 3. NHD risk score calibration — three model specifications."
    else
      "Figure 3. Iannuzzi 2020: calibration — published lookup vs. temporal recalibration."
    fig3_caption <- if (has_mfi5_cal && has_vqifs_cal)
      paste0("Calibration curves for four NHD risk score specifications. ",
             "Blue solid line: Iannuzzi 2020 refitted score-to-risk lookup (monotone isotonic). ",
             "Red dashed line: Iannuzzi 2020 temporal recalibration — logistic regression of the ",
             "total integer score fitted on the chronologically earlier half of the cohort and ",
             "evaluated on the later half. ",
             "Green dot-dash line: mFI-5 temporal recalibration — same split applied to the mFI-5 score. ",
             "Orange dotted line: Kraiss 2022 sVQI-FS temporal recalibration — same split applied to ",
             "the sVQI-FS score. ",
             "Mean predicted NHD risk (x-axis) vs. observed NHD rate (y-axis) by quantile bin. ",
             "Dotted diagonal = perfect calibration.")
    else if (has_mfi5_cal)
      paste0("Calibration curves for three NHD risk score specifications. ",
             "Blue solid line: Iannuzzi 2020 refitted score-to-risk lookup (monotone isotonic). ",
             "Red dashed line: Iannuzzi 2020 temporal recalibration — logistic regression of the ",
             "total integer score fitted on the chronologically earlier half of the cohort and ",
             "evaluated on the later half. ",
             "Green dot-dash line: mFI-5 temporal recalibration — same split applied to the mFI-5 score. ",
             "Mean predicted NHD risk (x-axis) vs. observed NHD rate (y-axis) by quantile bin. ",
             "Dotted diagonal = perfect calibration.")
    else
      paste0("Calibration of the Iannuzzi 2020 score under two specifications. ",
             "Blue solid line: published score-to-risk lookup (no local refitting). ",
             "Red dashed line: temporal recalibration — logistic regression fitted on the ",
             "chronologically earlier half of the cohort and evaluated on the later half. ",
             "Mean predicted NHD risk (x-axis) vs. observed NHD rate (y-axis) by quantile bin. ",
             "Dotted diagonal = perfect calibration.")
    doc <- body_add_img(doc, src = dual_cal_file, width = 5.0, height = 5.0)
    doc <- add_doc_caption(doc, fig3_title, fig3_caption)
    doc <- body_add_par(doc, "", style = "Normal")
    message("[report] Figure 3 (calibration) added.")
  } else if (file.exists(lookup_calibration_plot_temp) ||
             file.exists(recalibrated_calibration_plot_temp)) {
    # Fallback: single-curve from whichever source is available.
    cal_plot_path <- if (file.exists(lookup_calibration_plot_temp))
      lookup_calibration_plot_temp else recalibrated_calibration_plot_temp
    cal_caption <- if (identical(cal_plot_path, lookup_calibration_plot_temp))
      paste0("Calibration plot for the Iannuzzi 2020 published lookup model. ",
             "Mean predicted non-home discharge risk (x-axis) vs. observed NHD rate (y-axis) ",
             "by quantile bin. Dashed diagonal = perfect calibration.")
    else
      paste0("Calibration plot for the Iannuzzi 2020 temporal recalibration model. ",
             "Mean predicted non-home discharge risk (x-axis) vs. observed NHD rate (y-axis) ",
             "by quantile bin. Dashed diagonal = perfect calibration.")
    doc <- body_add_img(doc, src = cal_plot_path, width = 4.5, height = 4.5)
    doc <- add_doc_caption(doc, "Figure 3. Iannuzzi 2020: calibration plot.", cal_caption)
    doc <- body_add_par(doc, "", style = "Normal")
    message("[report] Figure 3 (single calibration fallback) added.")
  }

    # ---- Risk tier table (after Figure 3) ------------------------------------
  # Multi-model risk tier analysis across every available score specification.
  # Tier thresholds: Low <30%, Intermediate 30-70%, High >70%.
  # Each model section contributes three rows (one per tier) with a header separator row.
  #
  # EVALUATION SET: every recalibrated specification is restricted to the test
  # partition (split_set == "test"), matching Table 4. Tiering the recalibrated
  # models over the full cohort re-included the rows their own recalibration was
  # fitted on. The published-lookup specification is exempt — nothing is fitted,
  # so it is tiered over the full cohort, consistent with Table 4.
  # (.test_rows() itself is defined earlier in this function, near the DCA plot,
  # since that is its first point of use.)
  build_tier_rows <- function(scores, outcome_vec, model_label) {
    tier_levels <- c("Low (<30%)", "Intermediate (30\u201370%)", "High (>70%)")
    keep        <- !is.na(scores)
    if (sum(keep) == 0) return(NULL)
    s  <- scores[keep]
    y  <- as.integer(outcome_vec[keep])
    tier_v <- ifelse(s < 0.30, "Low (<30%)",
               ifelse(s <= 0.70, "Intermediate (30\u201370%)", "High (>70%)"))
    tier_v <- factor(tier_v, levels = tier_levels)
    # Header row for this model
    header_row <- data.frame(
      "Risk Tier"             = model_label,
      "N"                     = "",
      "NHD Events"            = "",
      "Observed NHD Rate (%)" = "",
      is_model_header         = TRUE,
      check.names = FALSE, stringsAsFactors = FALSE
    )
    tier_rows <- do.call(rbind, lapply(tier_levels, function(tier) {
      sub      <- y[tier_v == tier]
      n_tier   <- length(sub)
      n_ev     <- sum(sub, na.rm = TRUE)
      obs_rate <- if (n_tier > 0) paste0(round(100 * n_ev / n_tier, 1), "%") else "N/A"
      data.frame(
        "Risk Tier"             = paste0("  ", tier),
        "N"                     = as.character(n_tier),
        "NHD Events"            = as.character(n_ev),
        "Observed NHD Rate (%)" = obs_rate,
        is_model_header         = FALSE,
        check.names = FALSE, stringsAsFactors = FALSE
      )
    }))
    rbind(header_row, tier_rows)
  }

  # Build rows for each model, stacking into one table.
  # Lookup = full cohort (nothing fitted); every recalibrated model = test set.
  pl_test       <- .test_rows(person_level)
  pl_mfi5_test  <- .test_rows(person_level_mfi5)
  pl_vqifs_test <- .test_rows(person_level_vqifs)

  tier_sections <- list()
  if ("predicted_risk_lookup" %in% names(person_level))
    tier_sections[["lookup"]] <- build_tier_rows(
      person_level$predicted_risk_lookup,
      person_level$outcome,
      "Iannuzzi 2020 (Lookup, full cohort)"
    )
  if ("predicted_risk_recalibrated" %in% names(person_level))
    tier_sections[["recal"]] <- build_tier_rows(
      person_level$predicted_risk_recalibrated[pl_test],
      person_level$outcome[pl_test],
      "Iannuzzi 2020 (Recalibrated, test set)"
    )
  if (!is.null(person_level_mfi5) &&
      "predicted_risk_recalibrated" %in% names(person_level_mfi5))
    tier_sections[["mfi5"]] <- build_tier_rows(
      person_level_mfi5$predicted_risk_recalibrated[pl_mfi5_test],
      person_level_mfi5$outcome[pl_mfi5_test],
      "mFI-5 (Recalibrated, test set)"
    )
  if (!is.null(person_level_vqifs) &&
      "predicted_risk_recalibrated" %in% names(person_level_vqifs))
    tier_sections[["vqifs"]] <- build_tier_rows(
      person_level_vqifs$predicted_risk_recalibrated[pl_vqifs_test],
      person_level_vqifs$outcome[pl_vqifs_test],
      "sVQI-FS (Recalibrated, test set)"
    )

  # ---- Iannuzzi published score-based strata (Table II / Fig 2 of the paper) --
  # The paper tiers on the RAW SCORE, not on predicted probability:
  #   Low 0-4 (published NHD 10.1%), Moderate 5-9 (36.7%), High >=10 (66.1%).
  # Reporting these alongside the probability tiers makes this table directly
  # comparable to the source publication. Full cohort: no model is fitted.
  iannuzzi_published_rates <- c("Low (score 0–4)"        = 10.1,
                                "Moderate (score 5–9)"   = 36.7,
                                "High (score ≥10)"       = 66.1)
  if ("total_score" %in% names(person_level)) {
    s_all <- person_level$total_score
    y_all <- as.integer(person_level$outcome)
    keep_s <- !is.na(s_all)
    s_all <- s_all[keep_s]; y_all <- y_all[keep_s]
    band <- ifelse(s_all <= 4, "Low (score 0–4)",
             ifelse(s_all <= 9, "Moderate (score 5–9)", "High (score ≥10)"))
    band <- factor(band, levels = names(iannuzzi_published_rates))
    score_tier_rows <- do.call(rbind, lapply(names(iannuzzi_published_rates), function(b) {
      sub <- y_all[band == b]
      data.frame(
        "Risk Tier"             = paste0("  ", b),
        "N"                     = as.character(length(sub)),
        "NHD Events"            = as.character(sum(sub, na.rm = TRUE)),
        "Observed NHD Rate (%)" = if (length(sub) > 0)
                                    paste0(round(100 * sum(sub, na.rm = TRUE) / length(sub), 1),
                                           "% (published ", iannuzzi_published_rates[[b]], "%)")
                                  else "N/A",
        is_model_header         = FALSE,
        check.names = FALSE, stringsAsFactors = FALSE
      )
    }))
    tier_sections[["iannuzzi_score_bands"]] <- rbind(
      data.frame(
        "Risk Tier"             = "Iannuzzi 2020 published score strata (full cohort)",
        "N" = "", "NHD Events" = "", "Observed NHD Rate (%)" = "",
        is_model_header = TRUE,
        check.names = FALSE, stringsAsFactors = FALSE
      ),
      score_tier_rows
    )
  }

  tier_sections <- Filter(Negate(is.null), tier_sections)

  if (length(tier_sections) > 0) {
    tier_all <- do.call(rbind, tier_sections)
    row.names(tier_all) <- NULL

    # Row indices for model-header rows (bold + shaded)
    header_idx  <- which(tier_all$is_model_header)
    tier_display <- tier_all[, c("Risk Tier", "N", "NHD Events", "Observed NHD Rate (%)"),
                              drop = FALSE]

    tier_ft <- flextable::flextable(tier_display) |>
      flextable::bold(part = "header") |>
      flextable::fontsize(size = 10, part = "all") |>
      flextable::font(fontname = "Calibri", part = "all") |>
      flextable::bg(part = "header", bg = "#1F3864") |>
      flextable::color(part = "header", color = "white") |>
      flextable::padding(padding = 4, part = "all") |>
      flextable::bold(i = header_idx, part = "body") |>
      flextable::bg(i = header_idx, bg = "#F2F2F2", part = "body") |>
      flextable::align(j = c("N", "NHD Events", "Observed NHD Rate (%)"),
                       align = "center", part = "all") |>
      flextable::width(j = "Risk Tier",              width = 2.2) |>
      flextable::width(j = "N",                      width = 0.7) |>
      flextable::width(j = "NHD Events",             width = 0.9) |>
      flextable::width(j = "Observed NHD Rate (%)",  width = 1.6) |>
      flextable::set_table_properties(layout = "fixed")

    n_tier_models <- length(tier_sections)
    model_names_used <- c(
      if ("lookup" %in% names(tier_sections)) "Iannuzzi 2020 (Lookup, full cohort)",
      if ("recal"  %in% names(tier_sections)) "Iannuzzi 2020 (Recalibrated, test set)",
      if ("mfi5"   %in% names(tier_sections)) "mFI-5 (Recalibrated, test set)",
      if ("vqifs"  %in% names(tier_sections)) "sVQI-FS (Recalibrated, test set)",
      if ("iannuzzi_score_bands" %in% names(tier_sections))
        "Iannuzzi 2020 published score strata (full cohort)"
    )
    n_word <- c("one", "two", "three", "four", "five")[min(n_tier_models, 5)]

    doc <- body_add_par(doc, section_num("Risk tier analysis"), style = "heading 3")
    doc <- body_add_flextable(doc, tier_ft)
    doc <- add_doc_caption(doc,
      paste0(
        "Table 5. Risk tier classification by model specification. ",
        "Patients are stratified into three tiers based on each model's predicted NHD risk: ",
        "Low (<30%), Intermediate (30\u201370%), and High (>70%); a final section instead uses ",
        "the Iannuzzi 2020 publication's own raw-score strata. ",
        "The observed NHD rate within each tier provides a direct assessment of clinical utility."
      ),
      paste0(
        n_word, " specification", if (n_tier_models > 1) "s are" else " is",
        " shown in separate sections: ",
        paste(model_names_used, collapse = "; "), ". ",
        "EVALUATION SET: recalibrated specifications are restricted to the temporal test ",
        "partition, matching Table 4, because the training half was used to fit their ",
        "recalibration. The published lookup and the published score strata involve no ",
        "fitting and are therefore reported over the full cohort. ",
        "N = number of patients assigned to that tier by the corresponding model. ",
        "NHD Events = number with non-home discharge. ",
        "Observed NHD Rate = NHD Events / N. ",
        "Predicted risk thresholds: Low <30%, Intermediate 30\u201370%, High >70%. ",
        "Iannuzzi published score strata: Low 0\u20134, Moderate 5\u20139, High \u226510, with the ",
        "publication's own observed rates (10.1% / 36.7% / 66.1%) shown in parentheses for ",
        "direct comparison. ",
        "Note that logistic recalibration of a weakly discriminating integer score compresses ",
        "the predicted probability range toward the cohort base rate; when this places every ",
        "patient in a single probability tier, that is a substantive finding about the score's ",
        "limited spread, not a computational artefact."
      )
    )
    doc <- body_add_par(doc, "", style = "Normal")
    message("[report] Risk tier table (Table 5, ", n_tier_models, " sections) added.")
  }

  # ---- Figure 4: Decision curve analysis ------------------------------------
  doc <- add_doc_page_break(doc)
  doc <- body_add_par(doc, section_num("Decision curve analysis"), style = "heading 3")
  if (!is.null(dca_plot_file) && file.exists(dca_plot_file)) {
    doc <- body_add_img(doc, src = dca_plot_file, width = 5.5, height = 3.8)
    doc <- add_doc_caption(doc, "Figure 4. Decision curve analysis.", paste0(
      "Decision curve analysis for every NHD risk prediction model specification available in ",
      "this run (Iannuzzi lookup and recalibrated; mFI-5 and sVQI-FS recalibrated, when their ",
      "pipelines were run), evaluated on the temporal test partition so that all curves compare ",
      "net benefit across the same patients. ",
      "Net benefit is plotted across threshold probabilities from 1% to 99%. ",
      "Model curves are compared with the 'treat-all' (dashed) and ",
      "'treat-none' (zero reference) strategies. Threshold probabilities correspond ",
      "to the minimum predicted NHD risk at which a clinician would recommend an intervention."
    ))
    doc <- body_add_par(doc, "", style = "Normal")
    message("[report] Figure 4 (DCA) added.")
  }

  # Helper: load, sort, and render a subgroup_bias.csv as a table + forest plot.
  # Used for both Iannuzzi recalibrated and mFI-5 recalibrated subgroup sections.
  add_subgroup_section <- function(doc, bias_path, overall_ece_val,
                                   section_heading, table_caption, figure_caption,
                                   forest_file_name) {
    df <- NULL
    if (file.exists(bias_path)) {
      df <- tryCatch(
        readr::read_csv(bias_path, show_col_types = FALSE),
        error = function(e) NULL
      )
    }
    if (is.null(df) || nrow(df) == 0) return(doc)

    subgroup_order <- c(age_group = 1, sex = 2, race = 3, ethnicity = 4,
                        indication = 5, proc_type = 6, year = 7)
    sort_key <- subgroup_order[match(df$subgroup_var, names(subgroup_order))]
    sort_key[is.na(sort_key)] <- 99L
    df <- df[order(sort_key, df$subgroup_level), ]

    bias_display <- data.frame(
      Subgroup  = tools::toTitleCase(gsub("_", " ", df$subgroup_var)),
      Level     = df$subgroup_level,
      N         = df$n,
      Events    = df$n_events,
      ECE       = round(df$ece, 3),
      "95% CI"  = paste0("(", round(df$ci_lower, 3),
                         "\u2013",
                         round(df$ci_upper, 3), ")"),
      check.names = FALSE, stringsAsFactors = FALSE
    )

    bias_ft <- flextable::flextable(bias_display) |>
      flextable::bold(part = "header") |>
      flextable::fontsize(size = 10, part = "all") |>
      flextable::font(fontname = "Calibri", part = "all") |>
      flextable::bg(part = "header", bg = "#1F3864") |>
      flextable::color(part = "header", color = "white") |>
      flextable::padding(padding = 4, part = "all") |>
      flextable::align(j = c("N", "Events", "ECE", "95% CI"),
                       align = "center", part = "all") |>
      flextable::width(j = "Subgroup", width = 1.2) |>
      flextable::width(j = "Level",    width = 1.6) |>
      flextable::width(j = "N",        width = 0.6) |>
      flextable::width(j = "Events",   width = 0.7) |>
      flextable::width(j = "ECE",      width = 0.7) |>
      flextable::width(j = "95% CI",   width = 1.2) |>
      flextable::set_table_properties(layout = "fixed")

    doc <- body_add_par(doc, section_heading, style = "heading 3")
    doc <- body_add_flextable(doc, bias_ft)
    doc <- add_doc_caption(doc,
      table_caption,
      paste0("Subgroups with fewer than 10 observed NHD events are suppressed. ",
             "Overall ECE = ", round(overall_ece_val, 3), ". ",
             "95% CI = bootstrap percentile interval (B\u2009=\u2009200 resamples).")
    )
    doc <- body_add_par(doc, "", style = "Normal")

    forest_png <- save_subgroup_forest_plot(df, overall_ece_val,
                                            temp_figure_dir,
                                            file_name = forest_file_name)
    if (!is.null(forest_png) && file.exists(forest_png)) {
      plot_height <- max(4.0, nrow(df) * 0.35 + 1.5)
      doc <- body_add_img(doc, src = forest_png,
                          width  = 5.5,
                          height = min(plot_height, 9.0))
      doc <- add_doc_caption(doc,
        figure_caption,
        paste0("Expected calibration error (ECE) with 95% bootstrap percentile CIs ",
               "(B\u2009=\u2009200) by subgroup. ",
               "Dashed vertical line = overall ECE. ",
               "Subgroups with < 10 NHD events are suppressed.")
      )
      doc <- body_add_par(doc, "", style = "Normal")
    }
    doc
  }

  # ---- Subgroup \u2014 Iannuzzi recalibrated -------------------------------------
  ece_iannuzzi_recal <- tryCatch(
    as.numeric(metric_value("ECE", "recalibrated")),
    error = function(e) NA_real_
  )
  if (is.na(ece_iannuzzi_recal)) ece_iannuzzi_recal <- 0.0

  doc <- add_subgroup_section(
    doc,
    bias_path        = file.path(score_output_dir, "subgroup_bias.csv"),
    overall_ece_val  = ece_iannuzzi_recal,
    section_heading  = section_num("Subgroup bias assessment \u2014 Iannuzzi 2020 (Recalibrated)"),
    table_caption    = paste0(supp_table("subgroup_tbl_iannuzzi"), ". ECE by subgroup \u2014 Iannuzzi 2020 (Recalibrated)."),
    figure_caption   = paste0(supp_figure("subgroup_fig_iannuzzi"), ". Subgroup calibration forest plot \u2014 Iannuzzi 2020 (Recalibrated)."),
    forest_file_name = "subgroup_forest_iannuzzi.png"
  )
  message("[report] Subgroup section (Iannuzzi recalibrated) processed.")

  # ---- Subgroup \u2014 mFI-5 recalibrated ----------------------------------------
  if (!is.null(mfi5_output_dir)) {
    ece_mfi5_recal <- tryCatch({
      mfi5_met <- readr::read_csv(file.path(mfi5_output_dir, "metrics.csv"),
                                  show_col_types = FALSE)
      names(mfi5_met) <- tolower(names(mfi5_met))
      as.numeric(mfi5_met$value[mfi5_met$metric == "ECE" &
                                  mfi5_met$model == "recalibrated"][1])
    }, error = function(e) NA_real_)
    if (is.na(ece_mfi5_recal)) ece_mfi5_recal <- 0.0

    doc <- add_subgroup_section(
      doc,
      bias_path        = file.path(mfi5_output_dir, "subgroup_bias.csv"),
      overall_ece_val  = ece_mfi5_recal,
      section_heading  = section_num("Subgroup bias assessment \u2014 mFI-5 (Recalibrated)"),
      table_caption    = paste0(supp_table("subgroup_tbl_mfi5"), ". ECE by subgroup \u2014 mFI-5 (Recalibrated)."),
      figure_caption   = paste0(supp_figure("subgroup_fig_mfi5"), ". Subgroup calibration forest plot \u2014 mFI-5 (Recalibrated)."),
      forest_file_name = "subgroup_forest_mfi5.png"
    )
    message("[report] Subgroup section (mFI-5 recalibrated) processed.")
  }

  # ---- Subgroup \u2014 sVQI-FS recalibrated --------------------------------------
  if (!is.null(vqifs_output_dir)) {
    ece_vqifs_recal <- tryCatch({
      vqifs_met <- readr::read_csv(file.path(vqifs_output_dir, "metrics.csv"),
                                   show_col_types = FALSE)
      names(vqifs_met) <- tolower(names(vqifs_met))
      as.numeric(vqifs_met$value[vqifs_met$metric == "ECE" &
                                   vqifs_met$model == "recalibrated"][1])
    }, error = function(e) NA_real_)
    if (is.na(ece_vqifs_recal)) ece_vqifs_recal <- 0.0

    doc <- add_subgroup_section(
      doc,
      bias_path        = file.path(vqifs_output_dir, "subgroup_bias.csv"),
      overall_ece_val  = ece_vqifs_recal,
      section_heading  = section_num("Subgroup bias assessment \u2014 sVQI-FS (Recalibrated)"),
      table_caption    = paste0(supp_table("subgroup_tbl_vqifs"), ". ECE by subgroup \u2014 sVQI-FS (Recalibrated)."),
      figure_caption   = paste0(supp_figure("subgroup_fig_vqifs"), ". Subgroup calibration forest plot \u2014 sVQI-FS (Recalibrated)."),
      forest_file_name = "subgroup_forest_vqifs.png"
    )
    message("[report] Subgroup section (sVQI-FS recalibrated) processed.")
  }

  # ---- Supplemental section ------------------------------------------------
  doc <- body_add_par(doc, section_major("Supplemental Material"), style = "heading 2")

  # ---- S1–S3: DB-sourced tables (own connection; order: CDM, CPT, ICD) ------
  if (!is.null(connection_details) && !is.null(config)) {
    tryCatch({
      conn_supp <- DatabaseConnector::connect(connection_details)
      on.exit(try(DatabaseConnector::disconnect(conn_supp), silent = TRUE), add = TRUE)

      # ---- Supplemental Table S1 — CDM Source --------------------------------
      tryCatch({
        sql_cdm_src <- SqlRender::render(
          "SELECT cdm_source_name, cdm_source_abbreviation, cdm_holder,
                  source_release_date, cdm_release_date, cdm_version,
                  vocabulary_version
           FROM @cdm_schema.cdm_source",
          cdm_schema = config$cdm_schema
        )
        cdm_src_raw <- DatabaseConnector::querySql(
          conn_supp,
          SqlRender::translate(sql_cdm_src, targetDialect = "sql server")
        )
        names(cdm_src_raw) <- tolower(names(cdm_src_raw))
        if (nrow(cdm_src_raw) > 0) {
          cdm_src_display <- data.frame(
            Field = c("CDM Source Name", "Source Abbreviation", "CDM Holder",
                      "Source Release Date", "CDM Release Date",
                      "CDM Version", "Vocabulary Version",
                      "Study Start Date", "Study End Date"),
            Value = c(as.character(cdm_src_raw$cdm_source_name[1]),
                      as.character(cdm_src_raw$cdm_source_abbreviation[1]),
                      as.character(cdm_src_raw$cdm_holder[1]),
                      as.character(cdm_src_raw$source_release_date[1]),
                      as.character(cdm_src_raw$cdm_release_date[1]),
                      as.character(cdm_src_raw$cdm_version[1]),
                      as.character(cdm_src_raw$vocabulary_version[1]),
                      if (!is.null(config$study_start_date)) as.character(config$study_start_date) else "N/A",
                      if (!is.null(config$study_end_date))   as.character(config$study_end_date)   else "N/A"),
            stringsAsFactors = FALSE
          )
          cdm_src_ft <- flextable::flextable(cdm_src_display) |>
            flextable::bold(part = "header") |>
            flextable::fontsize(size = 10, part = "all") |>
            flextable::font(fontname = "Calibri", part = "all") |>
            flextable::bg(part = "header", bg = "#1F3864") |>
            flextable::color(part = "header", color = "white") |>
            flextable::padding(padding = 4, part = "all") |>
            flextable::width(j = "Field", width = 2.0) |>
            flextable::width(j = "Value", width = 4.0) |>
            flextable::set_table_properties(layout = "fixed")
          doc <- body_add_par(doc, "CDM source metadata", style = "heading 3")
          doc <- body_add_flextable(doc, cdm_src_ft)
          doc <- add_doc_caption(doc,
            paste0(supp_table("cdm_metadata"), ". CDM source metadata."),
            paste0("Metadata from the cdm_source table of the OMOP CDM instance ",
                   "used for this analysis. CDM Version and Vocabulary Version confirm ",
                   "compliance with OMOP CDM v5.4 and the Athena vocabulary release used ",
                   "during ETL.")
          )
          doc <- body_add_par(doc, "", style = "Normal")
          message("[report] Supplemental Table S1 (CDM source) added.")
        }
      }, error = function(e) {
        message("[report] CDM source table skipped: ", conditionMessage(e))
      })

      # ---- Supplemental Table S2 — Model descriptions -------------------------
      #
      # Read from covariates/model_metadata.yaml — the single source of truth
      # for score derivation details — rather than a hardcoded data.frame().
      #
      # WHY: the previous hardcoded block described a DIFFERENT score entirely
      # (NSQIP-derived, amputation cohort, 9 components including BMI/ABI/
      # operative time, AUROC 0.73) than the one Table 3b actually implements
      # (VSGNE-derived, elective lower-extremity bypass, C statistic 0.77).
      # Table 3b was always correct because it is generated from
      # covariates/covariates.csv; S2 was wrong because nothing forced it to
      # agree with that same source. Reading both from data (Table 3b from the
      # covariates CSVs, S2 from model_metadata.yaml, both curated files this
      # report treats as ground truth) can't re-diverge the way two
      # independently hand-typed R blocks did.
      tryCatch({
        model_meta <- yaml::read_yaml("covariates/model_metadata.yaml")

        # Build the Field/Description rows for one score's section. Optional
        # fields (derivation_n, derivation_nhd_rate, derivation_exclusions,
        # prediction_window, c_statistic_*) are omitted when NULL/empty in the
        # YAML rather than rendered as blank rows — e.g. the mFI-5 and sVQI-FS
        # entries have no published NHD derivation_n because they were not
        # derived to predict NHD.
        .fmt_field <- function(label, value) {
          if (is.null(value) || (is.character(value) && !nzchar(trimws(value)))) return(NULL)
          if (is.list(value)) value <- paste(unlist(value), collapse = "; ")
          # YAML ">" folded block scalars join lines with spaces but keep a
          # trailing newline; collapse any remaining whitespace runs (including
          # that trailing \n) to single spaces so it doesn't render as a stray
          # blank line inside the flextable cell.
          value <- gsub("\\s+", " ", trimws(as.character(value)))
          data.frame(Field = label, Description = value, stringsAsFactors = FALSE)
        }

        .build_score_rows <- function(meta) {
          derivation_cohort <- paste0(
            meta$derivation_registry,
            if (!is.null(meta$derivation_years)) paste0(", ", meta$derivation_years) else "",
            if (!is.null(meta$derivation_procedure)) paste0("; ", meta$derivation_procedure) else "",
            if (!is.null(meta$derivation_n)) paste0(" (n = ", format(meta$derivation_n, big.mark = ","), ")") else ""
          )
          exclusions_str <- if (length(meta$derivation_exclusions) > 0)
            paste0("Derivation exclusions: ", paste(unlist(meta$derivation_exclusions), collapse = "; "), ".")
          else NULL
          c_stat <- c(
            if (!is.null(meta$c_statistic_derivation))
              paste0("Derivation: ", meta$c_statistic_derivation) else NULL,
            if (!is.null(meta$c_statistic_validation))
              paste0("Validation: ", meta$c_statistic_validation) else NULL
          )
          c_stat_str <- if (length(c_stat) > 0) paste(c_stat, collapse = "; ") else NULL

          do.call(rbind, Filter(Negate(is.null), list(
            .fmt_field("Score name",        meta$score_name),
            .fmt_field("Reference",         meta$reference),
            .fmt_field("Derivation cohort", derivation_cohort),
            .fmt_field("Derivation exclusions", exclusions_str),
            .fmt_field("Outcome definition", meta$outcome_definition),
            .fmt_field("Prediction window",  meta$prediction_window),
            .fmt_field("Score design",       meta$score_design),
            .fmt_field("Score range",        meta$score_range),
            .fmt_field("Original discrimination (C statistic / AUC)", c_stat_str)
          )))
        }

        # Only include scores whose pipeline actually ran this report (mirrors
        # the has_mfi5/has_vqifs gating used everywhere else in this function).
        score_sections <- Filter(Negate(is.null), list(
          .build_score_rows(model_meta$iannuzzi),
          if (!is.null(person_level_mfi5))  .build_score_rows(model_meta$mfi5),
          if (!is.null(person_level_vqifs)) .build_score_rows(model_meta$vqifs)
        ))

        # Interleave a blank spacer row between (not after) sections, then drop
        # it for the single-score-only case.
        model_desc_display <- score_sections[[1]]
        if (length(score_sections) > 1) {
          for (i in 2:length(score_sections)) {
            model_desc_display <- rbind(
              model_desc_display,
              data.frame(Field = "", Description = "", stringsAsFactors = FALSE),
              score_sections[[i]]
            )
          }
        }

        mdesc_ft <- flextable::flextable(model_desc_display) |>
          flextable::bold(part = "header") |>
          flextable::fontsize(size = 9, part = "all") |>
          flextable::font(fontname = "Calibri", part = "all") |>
          flextable::bg(part = "header", bg = "#1F3864") |>
          flextable::color(part = "header", color = "white") |>
          flextable::padding(padding = 3, part = "all") |>
          flextable::width(j = "Field",       width = 1.8) |>
          flextable::width(j = "Description", width = 4.7) |>
          flextable::bold(i = which(model_desc_display$Field %in%
                                      c("Score name")), part = "body") |>
          flextable::bg(i = which(model_desc_display$Field %in%
                                    c("Score name")), bg = "#F2F2F2", part = "body") |>
          flextable::set_table_properties(layout = "fixed")
        doc <- body_add_par(doc, "Model descriptions", style = "heading 3")
        doc <- body_add_flextable(doc, mdesc_ft)
        doc <- add_doc_caption(doc,
          paste0(supp_table("model_descriptions"), ". Description of integer risk score models evaluated."),
          paste0(
            "Derivation details for each score are read from covariates/model_metadata.yaml. ",
            "None of these instruments was derived to predict non-home discharge after major ",
            "lower-extremity amputation (see Methods 1.3 for the transportability framing this ",
            "study uses instead of describing itself as an external validation). ",
            "The Iannuzzi 2020 score's own outcome definition (rehabilitation or SNF discharge ",
            "only) is narrower than this study's NHD definition, which additionally counts ",
            "hospice and LTAC discharges as non-home — see the Limitations discussion. ",
            "The Iannuzzi 2020 NHD score uses a published lookup table to convert integer score ",
            "to predicted probability; a temporally recalibrated version is also evaluated. ",
            if (!is.null(person_level_mfi5)) paste0(
              "The Subramaniam mFI-5 — derived to predict mortality and postoperative ",
              "complications, not NHD — is evaluated via temporal logistic recalibration only ",
              "(no published NHD lookup table exists). "
            ) else "",
            if (!is.null(person_level_vqifs)) paste0(
              "The Kraiss 2022 sVQI-FS — derived to predict 9-month mortality, not NHD — is ",
              "likewise evaluated via temporal logistic recalibration only. "
            ) else ""
          )
        )
        doc <- body_add_par(doc, "", style = "Normal")
        message("[report] Supplemental Table S2 (model descriptions) added.")
      }, error = function(e) {
        message("[report] Model description table skipped: ", conditionMessage(e))
      })

      # ---- Supplemental Table S3 — CPT codes by procedure subgroup -----------
      # Uses concept_relationship ('Mapped from') to find CPT4 source codes
      # that map to SNOMED standard descendants — CPT4 codes are source codes
      # (standard_concept IS NULL), not in concept_ancestor as descendants.
      #
      # TWO BUGS FIXED HERE (2026-07-26), both of which independently produced
      # an all-zero table that contradicted the cohort's own existence:
      #
      # 1. NOT SCOPED TO THE COHORT: the original query counted ANY
      #    procedure_occurrence row anywhere in the CDM matching a CPT4/HCPCS
      #    concept, not procedures belonging to this study's target-cohort
      #    patients at their index date. Fixed by adding a target_population
      #    CTE (the same pattern used throughout R/risk_score_pipeline.R) and
      #    joining procedure_occurrence on person_id + procedure_date =
      #    cohort_start_date (verified against cohorts/target_amputation.sql:
      #    cohort_start_date IS the qualifying procedure's procedure_date).
      #
      # 2. procedure_source_concept_id MAY NEVER BE POPULATED WITH A CPT4
      #    CONCEPT: Synthea's procedures.csv (see synthea/modules/pad_amp_nhd.json)
      #    emits amputation procedures with their SNOMED display name directly —
      #    there is no evidence this ETL ever writes a CPT4 concept into
      #    procedure_source_concept_id for these rows; it may instead be 0,
      #    NULL, or the same SNOMED concept as procedure_concept_id. Added an
      #    OR'd fallback match against procedure_source_value = the CPT4
      #    concept_code text, in case the source system recorded the CPT4 code
      #    as a raw string without a successful concept mapping.
      #
      #    If BOTH bugs are fixed and every row is still 0 (i.e., this ETL
      #    genuinely never records CPT4 codes for these procedures — expected
      #    for Synthea-generated SNOMED-native procedure data), this function
      #    now says so explicitly in the report rather than rendering a table
      #    that silently implies the cohort itself is empty. See the
      #    total_cases == 0 branch below.
      tryCatch({
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
        cpt_raw <- DatabaseConnector::querySql(
          conn_supp,
          SqlRender::translate(sql_cpt, targetDialect = "sql server")
        )
        names(cpt_raw) <- tolower(names(cpt_raw))
        total_cases <- sum(cpt_raw$case_count, na.rm = TRUE)
        if (nrow(cpt_raw) > 0 && total_cases == 0) {
          # Both scoping bugs above are fixed, and the table is STILL all
          # zero: this ETL genuinely never records a CPT4/HCPCS concept or
          # source value for these procedures (expected when procedures are
          # sourced from Synthea's native SNOMED procedure codes). Say so
          # explicitly — loudly, per the plan's sanity-check requirement —
          # rather than rendering a table whose all-zero rows would otherwise
          # read as evidence the cohort itself is empty.
          message("[report] Supplemental Table S3 skipped: 0 procedure_occurrence rows matched ",
                  "any CPT4/HCPCS concept or source value for the target cohort at its index date ",
                  "after scoping the query correctly. This ETL likely records index amputation ",
                  "procedures using native SNOMED codes only (see synthea/modules/pad_amp_nhd.json) ",
                  "and never populates a CPT4 procedure_source_concept_id/procedure_source_value.")
          doc <- body_add_par(doc, "Index procedure CPT codes", style = "heading 3")
          doc <- body_add_par(doc, paste0(
            "No CPT-4/HCPCS-coded procedure records were found for the target cohort at their ",
            "index amputation date. Index amputations in this dataset are captured using native ",
            "standard (SNOMED) procedure concepts rather than CPT-4 source codes (see Table 1 and ",
            "the amputation-level concept IDs cited in its footnote); a CPT-4 crosswalk table is ",
            "therefore not applicable to this data source and is omitted rather than shown as an ",
            "uninformative all-zero table."
          ), style = "Normal")
          doc <- body_add_par(doc, "", style = "Normal")
        } else if (nrow(cpt_raw) > 0) {
          cpt_display <- data.frame(
            "Procedure Group" = cpt_raw$proc_group,
            "CPT Code"        = cpt_raw$cpt_code,
            "Description"     = cpt_raw$cpt_description,
            "Cases (n)"       = cpt_raw$case_count,
            check.names = FALSE, stringsAsFactors = FALSE
          )
          cpt_ft <- flextable::flextable(cpt_display) |>
            flextable::bold(part = "header") |>
            flextable::fontsize(size = 9, part = "all") |>
            flextable::font(fontname = "Calibri", part = "all") |>
            flextable::bg(part = "header", bg = "#1F3864") |>
            flextable::color(part = "header", color = "white") |>
            flextable::padding(padding = 3, part = "all") |>
            flextable::width(j = "Procedure Group", width = 1.7) |>
            flextable::width(j = "CPT Code",        width = 0.9) |>
            flextable::width(j = "Description",     width = 3.5) |>
            flextable::width(j = "Cases (n)",       width = 0.8) |>
            flextable::align(j = "Cases (n)", align = "right", part = "all") |>
            flextable::set_table_properties(layout = "fixed")
          doc <- body_add_par(doc, "Index procedure CPT codes", style = "heading 3")
          doc <- body_add_flextable(doc, cpt_ft)
          doc <- add_doc_caption(doc,
            paste0(supp_table("cpt_codes"), ". CPT codes for index amputation subgroups."),
            paste0("CPT-4 codes identified via concept_relationship ('Mapped from') ",
                   "from SNOMED concept-ancestor descendants of each amputation subgroup anchor ",
                   "(AKA/transfemoral: 4195136; BKA/transtibial: 4338257; knee disarticulation: 4143795; ",
                   "hip disarticulation: 4242396; ankle disarticulation: 4264289; hemipelvectomy: 36675618). ",
                   "Cases (n) = number of distinct patients in procedure_occurrence with that source concept. ",
                   "A code may appear in more than one group.")
          )
          doc <- body_add_par(doc, "", style = "Normal")
          message("[report] Supplemental Table S3 (CPT codes) added.")
        } else {
          # combined itself has zero rows — distinct from the total_cases == 0
          # branch above: here, the vocab query never found ANY CPT4/HCPCS
          # concept related to the amputation ancestors at all (no descendant
          # in concept_ancestor, no 'Maps to' relationship), independent of
          # whether the cohort has matching procedure_occurrence rows. Worth
          # its own log line since it points to a vocabulary build issue
          # rather than an ETL/source-value issue.
          message("[report] Supplemental Table S3 skipped: no CPT4/HCPCS concept was found ",
                  "related to any amputation-level ancestor in this vocabulary build (0 rows from ",
                  "the concept_ancestor/concept_relationship join, before any cohort join).")
        }
      }, error = function(e) {
        message("[report] CPT supplemental table skipped: ", conditionMessage(e))
      })

      # ---- Supplemental Table S4 — Discharge destination source codes ---------
      # For NHD, outcomes are determined from visit_occurrence.discharged_to_source_value
      # (UB-04 NUBC codes) and secondarily from discharged_to_concept_id.  This table
      # enumerates ALL distinct source codes observed in the target cohort's index
      # visits — including rows with NULL source values — so that the full cohort is
      # accounted for and site-specific coding patterns are transparent.
      # Modelled after the equivalent fetch in pad-oler-nhd-val/R/report_prognostic.R
      # (fetch_nhd_disposition_codes).
      tryCatch({
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
        dest_supp_raw <- DatabaseConnector::querySql(
          conn_supp,
          SqlRender::translate(sql_dest_supp, targetDialect = "sql server")
        )
        names(dest_supp_raw) <- tolower(names(dest_supp_raw))
        if (nrow(dest_supp_raw) > 0) {
          dest_supp_display <- data.frame(
            "NUBC Code"       = dest_supp_raw$nubc_code,
            # Concept ID is an IDENTIFIER, not a quantity — kept as character
            # so flextable's default colformat_num() (big.mark = ",") does not
            # render it as "581,476" etc. Peer review flagged this exact
            # artefact; format(..., big.mark = "", scientific = FALSE) also
            # guards against any accidental scientific notation on large IDs.
            "OMOP Concept ID" = format(dest_supp_raw$omop_concept_id, big.mark = "", scientific = FALSE, trim = TRUE),
            "Concept Name"    = dest_supp_raw$concept_name,
            "Classification"  = dest_supp_raw$classification,
            "Visits (n)"      = dest_supp_raw$visit_count,
            "Persons (n)"     = dest_supp_raw$person_count,
            check.names = FALSE, stringsAsFactors = FALSE
          )
          dest_supp_ft <- flextable::flextable(dest_supp_display) |>
            flextable::bold(part = "header") |>
            flextable::fontsize(size = 9, part = "all") |>
            flextable::font(fontname = "Calibri", part = "all") |>
            flextable::bg(part = "header", bg = "#1F3864") |>
            flextable::color(part = "header", color = "white") |>
            flextable::padding(padding = 3, part = "all") |>
            flextable::width(j = "NUBC Code",       width = 0.9) |>
            flextable::width(j = "OMOP Concept ID", width = 1.1) |>
            flextable::width(j = "Concept Name",    width = 2.2) |>
            flextable::width(j = "Classification",  width = 1.3) |>
            flextable::width(j = "Visits (n)",      width = 0.7) |>
            flextable::width(j = "Persons (n)",     width = 0.7) |>
            flextable::align(j = c("Visits (n)", "Persons (n)"), align = "right", part = "all") |>
            flextable::set_table_properties(layout = "fixed")
          doc <- body_add_par(doc, "Discharge destination source codes", style = "heading 3")
          doc <- body_add_flextable(doc, dest_supp_ft)
          doc <- add_doc_caption(doc,
            paste0(supp_table("discharge_codes"), ". UB-04 NUBC discharge disposition codes observed in the validation cohort."),
            paste0("All distinct values of visit_occurrence.discharged_to_source_value ",
                   "for the index inpatient visit (visit_concept_id 9201) among target cohort patients, ",
                   "including rows with no source code recorded ('(none / NULL)'). ",
                   "OMOP Concept ID and Concept Name are drawn from discharged_to_concept_id. ",
                   "Classification: Home = confirmed home discharge (not-NHD); SNF/IRF/Hospice/LTAC/Other NHD = ",
                   "confirmed non-home discharge; HO = classified Home (source value treated as authoritative) ",
                   "even when the ETL has assigned a conflicting non-home concept_id; ",
                   "AM = Against Medical Advice, classified as home discharge (not-NHD); ",
                   "Other / Unknown = destination not classifiable from source code or concept ID, also ",
                   "classified not-NHD (presumed home) — a conservative default, not a removal from the ",
                   "analytic denominator; see Methods 1.2. ",
                   "Visits (n) = total index hospitalizations; Persons (n) = distinct patients.")
          )
          doc <- body_add_par(doc, "", style = "Normal")
          message("[report] Supplemental Table S4 (discharge destination source codes) added.")
        }
      }, error = function(e) {
        message("[report] Discharge destination supplemental table skipped: ", conditionMessage(e))
      })

      DatabaseConnector::disconnect(conn_supp)
    }, error = function(e) {
      message("[report] Supplemental DB tables skipped: ", conditionMessage(e))
    })
  }

  # ---- S4: NHD rate by month of year ----------------------------------------
  if (!is.null(nhd_month_plot_file) && file.exists(nhd_month_plot_file)) {
    doc <- body_add_par(doc, "NHD rate by month", style = "heading 3")
    doc <- body_add_img(doc, src = nhd_month_plot_file, width = 5.5, height = 3.5)
    doc <- add_doc_caption(doc,
      paste0(supp_figure("nhd_rate_by_month"), ". Non-home discharge (NHD) rate by calendar month of amputation."),
      paste0("Observed non-home discharge rate (%) for each calendar month ",
             "(January through December), pooled across all study years. Bar height represents the ",
             "NHD rate; numbers above each bar show the total procedure count for that month. ",
             "Months with fewer than 5 procedures are suppressed.")
    )
    doc <- body_add_par(doc, "", style = "Normal")
    message("[report] Supplemental Figure S5 (NHD by month) added.")
  }

  # ---- Score distribution plots (one per model) ------------------------------
  # Each model gets its own supplemental figure number from the pre-allocated
  # plan. Previously the label "Supplemental Figure S6" was hardcoded inside
  # this loop, so Iannuzzi and mFI-5 both rendered as S6 and sVQI-FS — which was
  # not in the loop at all — had no figure.
  for (dist_info in list(
    list(file = score_dist_plot_file_iannuzzi, label = "Iannuzzi 2020", key = "score_dist_iannuzzi"),
    list(file = score_dist_plot_file_mfi5,     label = "mFI-5",         key = "score_dist_mfi5"),
    list(file = score_dist_plot_file_vqifs,    label = "sVQI-FS",       key = "score_dist_vqifs")
  )) {
    if (!is.null(dist_info$file) && file.exists(dist_info$file)) {
      fig_label <- supp_figure(dist_info$key)
      doc <- body_add_par(doc, paste0("Score distribution — ", dist_info$label),
                          style = "heading 3")
      doc <- body_add_img(doc, src = dist_info$file, width = 5.5, height = 3.5)
      doc <- add_doc_caption(doc,
        paste0(fig_label, ". Distribution of ",
               dist_info$label, " integer risk score by outcome group."),
        paste0("Stacked count histogram of ", dist_info$label, " integer scores. ",
               "Blue bars = patients without NHD; red bars = patients with NHD. ",
               "Bars are stacked so the total bar height equals the count of all patients at that score. ",
               "NHD = non-home discharge.")
      )
      doc <- body_add_par(doc, "", style = "Normal")
      message("[report] ", fig_label, " (", dist_info$label, " score distribution) added.")
    }
  }

  # ---- Edge cases — CSV export (not in Word report) -----------------------
  # Two groups of patients that the model handled worst (2017–2019 window):
  #   Group A — lowest predicted risk who nonetheless had NHD (false negatives)
  #   Group B — highest predicted risk who did not have NHD (false positives)
  # Written to a dated CSV file in the same output folder as the report.
  if (!is.null(connection_details) && !is.null(config) &&
      "predicted_risk_lookup" %in% names(person_level)) {

    tryCatch({
      conn_f <- DatabaseConnector::connect(connection_details)
      on.exit(try(DatabaseConnector::disconnect(conn_f), silent = TRUE), add = TRUE)

      # Restrict edge candidates to index procedures in 2017–2019.
      edge_year_min <- 2017L
      edge_year_max <- 2019L
      edge_year_vec <- suppressWarnings(
        as.integer(format(as.Date(person_level$index_date), "%Y"))
      )
      in_window <- !is.na(edge_year_vec) &
                   edge_year_vec >= edge_year_min &
                   edge_year_vec <= edge_year_max
      pl_edge <- person_level[in_window, ]

      # Identify the two edge case groups from person_level_scores (2017–2019 only)
      fn_mask <- pl_edge$outcome == 1 & !is.na(pl_edge$predicted_risk_lookup)
      fn_ids  <- pl_edge$subject_id[fn_mask]
      fn_risk <- pl_edge$predicted_risk_lookup[fn_mask]
      fn_top  <- fn_ids[order(fn_risk)][seq_len(min(10L, sum(fn_mask)))]

      fp_mask <- pl_edge$outcome == 0 & !is.na(pl_edge$predicted_risk_lookup)
      fp_ids  <- pl_edge$subject_id[fp_mask]
      fp_risk <- pl_edge$predicted_risk_lookup[fp_mask]
      fp_top  <- fp_ids[order(fp_risk, decreasing = TRUE)][seq_len(min(10L, sum(fp_mask)))]

      message(sprintf("[report] Edge case filter: %d patients in %d\u2013%d (of %d total).",
                      sum(in_window), edge_year_min, edge_year_max, nrow(person_level)))

      all_ids <- unique(c(fn_top, fp_top))
      id_str  <- paste(as.integer(all_ids), collapse = ",")

      sql_edge <- SqlRender::render(
        "SELECT
           p.person_id,
           p.person_source_value                                                AS mrn,
           CAST(po.procedure_date AS DATE)                                      AS procedure_date,
           COALESCE(c.concept_name, po.procedure_source_value, 'Unknown')       AS procedure_name,
           YEAR(CAST(po.procedure_date AS DATE)) - p.year_of_birth             AS age_at_procedure
         FROM @cdm_schema.person p
         JOIN @cdm_schema.procedure_occurrence po
           ON po.person_id = p.person_id
         JOIN @vocab_schema.concept_ancestor ca
           ON ca.descendant_concept_id = po.procedure_concept_id
          AND ca.ancestor_concept_id   IN (4195136, 4338257, 4143795, 4242396, 4264289, 36675618)
         LEFT JOIN @vocab_schema.concept c
           ON c.concept_id = po.procedure_concept_id
         WHERE p.person_id IN (@id_list)",
        cdm_schema   = config$cdm_schema,
        vocab_schema = config$vocab_schema,
        id_list      = id_str
      )

      raw <- DatabaseConnector::querySql(
        conn_f,
        SqlRender::translate(sql_edge, targetDialect = "sql server")
      )
      names(raw) <- tolower(names(raw))

      # One row per person — earliest qualifying procedure
      raw <- raw[order(raw$person_id, raw$procedure_date), ]
      raw <- raw[!duplicated(raw$person_id), ]

      # Merge outcome + predicted risk from person_level
      pl_sub <- person_level[, c("subject_id", "predicted_risk_lookup", "outcome")]
      raw    <- merge(raw, pl_sub, by.x = "person_id", by.y = "subject_id", all.x = TRUE)

      # Merge individual score component columns from person_level
      score_cols <- grep("^score_", names(person_level), value = TRUE)
      if (length(score_cols) > 0) {
        pl_scores <- person_level[, c("subject_id", score_cols), drop = FALSE]
        raw <- merge(raw, pl_scores, by.x = "person_id", by.y = "subject_id", all.x = TRUE)
      }

      make_group <- function(ids, label) {
        sub        <- raw[raw$person_id %in% ids, ]
        sub$group  <- label
        sub
      }

      fn_df <- make_group(fn_top, "Low risk, NHD occurred")
      fp_df <- make_group(fp_top, "High risk, no NHD")
      fn_df <- fn_df[order(fn_df$predicted_risk_lookup), ]
      fp_df <- fp_df[order(fp_df$predicted_risk_lookup, decreasing = TRUE), ]

      # Build display-friendly column names for score components
      score_display_names <- c(
        score_female                 = "Female Sex (pts)",
        score_overweight             = "Overweight BMI 25-<30 (pts)",
        score_obese                  = "Obese BMI >=30 (pts)",
        score_urgnt                  = "Urgent Case (pts)",
        score_abi_35                 = "ABI <=0.35 (pts)",
        score_prrevasc_any           = "Prior Revascularization (pts)",
        score_prolong_abx            = "Prolonged Antibiotics (pts)",
        score_optime4h               = "Op Time >=4h (pts)",
        score_mFI_high               = "High mFI (pts)",
        score_indicationClaudication = "Indication: Claudication (pts)"
      )
      present_score_cols <- score_cols[score_cols %in% names(score_display_names)]
      score_labels       <- unname(score_display_names[present_score_cols])

      combined <- rbind(fn_df, fp_df)
      edge_tbl <- combined[,
        c("group", "mrn", "age_at_procedure",
          "procedure_date", "procedure_name", "predicted_risk_lookup",
          present_score_cols),
        drop = FALSE
      ]

      names(edge_tbl) <- c(
        "Group", "MRN", "Age at Procedure",
        "Procedure Date", "Procedure", "Predicted Risk",
        score_labels
      )
      edge_tbl[["Predicted Risk"]] <- round(as.numeric(edge_tbl[["Predicted Risk"]]), 3)

      # Prepend model_name and report_date columns
      model_nm    <- if (!is.null(config$model_name)) config$model_name else NA_character_
      report_date <- format(Sys.Date(), "%Y-%m-%d")
      edge_tbl  <- cbind(
        "Model"       = model_nm,
        "Report Date" = report_date,
        edge_tbl,
        stringsAsFactors = FALSE
      )

      # Write to dated CSV file alongside the report
      export_date  <- format(Sys.Date(), "%Y%m%d")
      edge_file  <- file.path(output_dir,
                              paste0("pad_amp_nhd_edge_", export_date, ".csv"))
      readr::write_csv(edge_tbl, edge_file)
      message("[report] Edge case CSV written to: ",
              normalizePath(edge_file, winslash = "/", mustWork = FALSE))

    }, error = function(e) {
      message("[report] Edge case CSV skipped: ", conditionMessage(e))
    })
  }

  doc <- .append_references_section(doc, citations)

  print(doc, target = report_file)
  # Section numbers are generated explicitly by section_major()/section_num();
  # remove the Word template's competing automatic heading numbering so headings
  # are not double-numbered ("1.1.1. 1.1. Data source").
  .strip_heading_autonumbering(report_file)
  message("Manuscript report written to: ", normalizePath(report_file))
  invisible(report_file)
}
