# =============================================================================
# config.R
# Central configuration for the pad-amp-nhd-prog prognostic Strategus study.
#
# Reads study_params.yaml and returns a named list consumed by:
#   scripts/analysis/integer_score_validation.R (the custom scoring step) and
#   R/risk_score_pipeline.R.
#
# NOTE: this is deliberately a MINIMAL config layer, not the full
# synthea-omop-template config.R. Under Strategus, cohort instantiation,
# database connection, and JDBC/Java setup are all owned elsewhere:
#   - StrategusCodeToRun.R builds its own connectionDetails directly from
#     environment variables (not from this file).
#   - Cohorts are Strategus/CohortGenerator-owned, defined in inst/ and
#     referenced here only by their cohort_definition_id.
# This file exists to supply the schema names, cohort ids, and per-score
# covariate file paths the custom step still needs.
#
# The Word report (pad-amp-nhd-prog-report, a separate repo as of 2026-08-11)
# does NOT read this file. Its ~9 narrative/parameter fields — a subset of
# what get_validation_config() returns below — are written into
# report_inputs/_report_config.yaml by R/extract_report_inputs.R instead, so
# the report repo needs no study_params.yaml of its own. See that file's
# header for why duplicating this file into the report repo would be worse.
# =============================================================================

get_validation_config <- function() {

  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop("Package 'yaml' is required. Install with: renv::install('yaml')")
  }

  params_file <- file.path(getwd(), "study_params.yaml")
  if (!file.exists(params_file)) {
    stop("study_params.yaml not found in ", getwd())
  }

  p <- yaml::read_yaml(params_file)

  # Helper: coerce a YAML value (may be NULL / list / scalar) to integer vector.
  as_int_vec <- function(x) {
    if (is.null(x)) return(integer(0))
    v <- suppressWarnings(as.integer(unlist(x)))
    v[!is.na(v)]
  }

  # Helper: return y when x is NULL.
  `%||%` <- function(x, y) if (is.null(x)) y else x

  study_name <- p$study_name %||% "pad_amp_nhd_prog"

  # ---------------------------------------------------------------------------
  # Per-score definitions, keyed by score id.
  #
  # This replaces the fragile arrangement inherited from pad-amp-nhd-val, where
  # the sVQI-FS sex-specific anaemia threshold was selected by testing whether
  # the covariate FILE PATH contained the substring "vqifs". That test matched
  # any path containing the string — including an output folder — so it could
  # silently resolve the wrong threshold. Score identity is now explicit data.
  # ---------------------------------------------------------------------------
  scores <- list()
  for (s in (p$scores %||% list())) {
    scores[[s$id]] <- list(
      id                         = s$id,
      label                      = s$label,
      covariate_definitions_file = s$covariate_definitions_file,
      covariate_concepts_file    = s$covariate_concepts_file,
      lookup_file                = s$lookup_file   # NULL when no published table
    )
  }

  list(
    # -------------------------------------------------------------------------
    # Study identity / design
    # -------------------------------------------------------------------------
    study_name   = study_name,
    study_design = p$study_design %||% "prognostic_model",

    # Study window — narrative only (the Methods "Data source" paragraph). Cohort
    # date bounds live in the cohort definitions, not here.
    study_start_date = p$study_start_date,
    study_end_date   = p$study_end_date,

    # -------------------------------------------------------------------------
    # Database schemas
    # -------------------------------------------------------------------------
    vocab_schema   = p$vocab_schema   %||% "omop_vocab",
    # Physical clinical CDM produced by pad-amp-dispo-synth (registry id
    # pad_amp, dataset version v2 as of the 2026-08-09 consolidation).
    # Strategus itself reads a view-overlay built over this. Must match the
    # physical schema the overlay (pad_amp_nhd_prog_cdm_test by default) was
    # actually built from -- verify with the overlay's view definition before
    # changing this, since the two silently diverging is a person_id-join
    # corruption, not an error.
    cdm_schema     = p$cdm_schema     %||% "omop_synth_pad_amp_v2",
    results_schema = p$results_schema %||% paste0(study_name, "_results"),
    cohort_table   = p$cohort_table   %||% study_name,

    # -------------------------------------------------------------------------
    # Cohort ids in the Strategus-generated cohort table
    # -------------------------------------------------------------------------
    target_cohort_id  = as.integer(p$target$cohort_id  %||% 1797941L),
    outcome_cohort_id = as.integer(p$outcome$cohort_id %||% 9100001L),

    # Index-procedure concept ids — drives the report's cohort-restricted
    # procedure crosswalk, not cohort entry (inst/ owns that).
    target_index_concept_ids = as_int_vec(p$target$index_event$ancestor_concept_ids),

    # -------------------------------------------------------------------------
    # Analysis parameters
    # -------------------------------------------------------------------------
    prediction_window_days     = as.integer(p$prediction_window_days     %||% 90L),
    min_prior_observation_days = as.integer(p$min_prior_observation_days %||% 1L),
    covariate_lookback_days    = as.integer(p$covariate_lookback_days    %||% 365L),

    # Upper bound (in PERCENT) of the decision-curve threshold axis.
    # NULL/absent = choose it from the data (see save_dca_plot() in
    # pad-amp-nhd-prog-report's R/report_prognostic.R). Set this explicitly
    # when the clinically plausible threshold range is known and should be
    # shown regardless of what the models happen to predict — a data-driven
    # bound must never be allowed to crop the range a clinician would
    # actually act over. Ported from pad-amp-nhd-val (#42). Written into
    # report_inputs/_report_config.yaml by R/extract_report_inputs.R for the
    # report repo to read — see that file's header.
    dca_threshold_max_pct = if (is.null(p$dca_threshold_max_pct)) NULL
                            else as.numeric(p$dca_threshold_max_pct),

    # -------------------------------------------------------------------------
    # Scoring
    #
    # score_id / covariate_definitions_file / covariate_concepts_file are set
    # PER RUN by the custom step, which loops over `scores` and overrides them
    # on a shallow copy of this list. They are NULL here on purpose: a run that
    # forgets to set them should fail loudly rather than silently score the
    # wrong model.
    # -------------------------------------------------------------------------
    scores                     = scores,
    score_id                   = NULL,
    covariate_definitions_file = NULL,
    covariate_concepts_file    = NULL,

    # Read covariate presence from the Strategus cohort table (TRUE) or by
    # querying CDM domain tables directly (FALSE). FALSE is the comparison
    # oracle used by tests/regression/.
    use_cohort_covariates = isTRUE(p$use_cohort_covariates),
    cohort_map_file       = p$cohort_map_file %||% "covariates/cohort_map.csv",

    # -------------------------------------------------------------------------
    # Report
    # -------------------------------------------------------------------------
    score_type             = p$report$score_type             %||% "integer",
    outcome_label          = p$report$outcome_label          %||% "Non-home Discharge (NHD)",
    model_type_description = p$report$model_type_description %||% "integer risk score",

    # -------------------------------------------------------------------------
    # Output folder — runtime-only, excluded from git via .gitignore
    # -------------------------------------------------------------------------
    output_folder = file.path(getwd(), p$output_folder %||% "output"),

    # -------------------------------------------------------------------------
    # Database metadata — narrative text in the report's Methods/Supplemental
    # -------------------------------------------------------------------------
    cdm_database_id          = p$cdm_database_id          %||% "pad_amp_dispo_cdm_v5.4",
    cdm_database_name        = p$cdm_database_name        %||% "PAD Amputation Disposition — Synthetic Dataset",
    cdm_database_description = p$cdm_database_description %||% "Brief description of the patient population."
  )
}
