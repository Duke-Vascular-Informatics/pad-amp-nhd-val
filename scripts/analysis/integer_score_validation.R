# =============================================================================
# scripts/analysis/integer_score_validation.R
#
# The custom step: apply three published integer risk scores to the
# Strategus-generated cohorts and evaluate them against non-home discharge.
#
#   Iannuzzi 2020 NHD score   0-18 points, published score-to-risk lookup
#   Subramaniam 2018 mFI-5    0-5 points,  no published lookup
#   Kraiss 2022 sVQI-FS       0-10 points, no published lookup
#
# WHY THIS IS A CUSTOM STEP AND NOT A STRATEGUS MODULE
# ----------------------------------------------------
# Strategus' PatientLevelPredictionModule FITS models; it cannot apply a fixed
# set of published weights. Strategus 1.5.0's PatientLevelPredictionValidation
# Module can apply an existing model, but it produces none of what this study
# reports: the published isotonic score-to-risk mapping, the temporal 50/50
# split, expected calibration error, seeded bootstrap confidence intervals, or
# the subgroup bias table. So scoring runs here, after Strategus::execute(),
# reading the cohort table Strategus just populated.
#
# CALLED BY: StrategusCodeToRun.R (the hybrid tail), or standalone for a
# re-score without re-running Strategus.
#
# OUTPUTS: one subdirectory per score under config$output_folder, each holding
# person_level_scores.csv, covariate_summary.csv, metrics.csv, split_info.csv,
# calibration tables and plots, and (Iannuzzi only) subgroup_bias.csv.
# =============================================================================

run_integer_score_validation <- function(connectionDetails, config,
                                         use_cohort_covariates = NULL) {

  if (length(config$scores) == 0L) {
    stop("No scores defined. study_params.yaml must carry a `scores:` block.")
  }

  # Allow a caller (notably the regression test) to force the covariate
  # resolution path without editing study_params.yaml.
  if (!is.null(use_cohort_covariates)) {
    config$use_cohort_covariates <- isTRUE(use_cohort_covariates)
  }

  mode_label <- if (isTRUE(config$use_cohort_covariates)) {
    "cohort-based (Strategus cohort table via covariates/cohort_map.csv)"
  } else {
    "domain-query (direct CDM queries — the comparison oracle)"
  }
  message("\n[scores] Covariate resolution: ", mode_label)

  results <- list()

  for (score in config$scores) {

    message("\n[scores] ==== ", score$label, " (", score$id, ") ====")

    # Shallow copy per score. score_id is what makes the run unambiguous: it
    # keys cohort_map.csv lookups and selects the anemia threshold on the
    # domain path. It is NULL in the base config on purpose, so a run that
    # forgets to set it fails loudly instead of scoring the wrong model.
    score_config <- config
    score_config$score_id                   <- score$id
    score_config$covariate_definitions_file <- score$covariate_definitions_file
    score_config$covariate_concepts_file    <- score$covariate_concepts_file

    output_dir <- file.path(config$output_folder, score$id)

    # lookup_file is NULL for mFI-5 and sVQI-FS: neither publishes a
    # score-to-risk table, so those scores get discrimination plus a locally
    # recalibrated fit, but no external-validation calibration arm.
    lookup <- score$lookup_file
    if (!is.null(lookup) && !file.exists(lookup)) {
      stop("Score '", score$id, "' names a lookup file that does not exist: ", lookup)
    }

    results[[score$id]] <- run_integer_risk_score_pipeline(
      score_config,
      connectionDetails,
      output_folder = output_dir,
      lookup_file   = lookup
    )

    message("[scores] ", score$label, " complete -> ", output_dir)
  }

  invisible(results)
}
