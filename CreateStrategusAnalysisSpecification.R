################################################################################
# CreateStrategusAnalysisSpecification.R  —  PIPELINE STEP 1
#
# Builds the Strategus (v1.5.0) analysis specification for pad-amp-nhd-prog:
# external validation of three published integer risk scores against NON-HOME
# DISCHARGE (NHD) after major lower extremity amputation, plus a disabled
# scaffold for de-novo model development.
#
# Pipeline:
#   1  THIS SCRIPT                                   -> inst/padAmpNhdProgAnalysisSpecification.json
#   2  StrategusCodeToRun.R                          -> Strategus::execute() + the custom scoring step
#   9  workflow/09_build_portable_analysis_bundle.sh -> Duke GitLab deployment bundle
#
# Re-run step 1 whenever inst/cohorts/*.json, inst/Cohorts.csv, or the module
# settings below change.
#
# WHAT STRATEGUS DOES AND DOES NOT DO HERE
# ----------------------------------------
# Strategus generates the cohorts and produces the Table 1 characterization. It
# does NOT score the three published instruments. Applying a FIXED set of
# published integer weights is not something the PatientLevelPredictionModule
# does — that module FITS models. The scoring therefore runs as a custom step
# after Strategus::execute() (the hybrid tail in StrategusCodeToRun.R), which is
# the same shape pad-amp-ed-desc uses for its own custom analysis.
#
# (Strategus 1.5.0 does ship a PatientLevelPredictionValidationModule that can
# APPLY an existing model via validateExternal(). It is not used here: it would
# not produce the published isotonic score-to-risk mapping, the temporal 50/50
# split, ECE, the seeded bootstrap CIs, or subgroup_bias.csv, all of which the
# custom step does produce. Worth revisiting only as a confirmatory arm.)
#
# COHORTS — see inst/Cohorts.csv for the full per-cohort rationale.
#   target     1797941   [DVI] Major Lower Extremity Amputation   (reused verbatim)
#   outcome    9100001   [DVI] Non-Home Discharge                 (hand-authored SQL)
#   covariates 1797949-1797952 reused VA-FI; 9100002-9100010 authored here
################################################################################
library(dplyr)
library(Strategus)

# ==============================================================================
# 1. Cohort definitions
# ==============================================================================
cohortDefinitionSet <- CohortGenerator::getCohortDefinitionSet(
  settingsFileName = "inst/Cohorts.csv",
  jsonFolder       = "inst/cohorts",
  sqlFolder        = "inst/sql/sql_server",
  packageName      = NULL
)
if (any(duplicated(cohortDefinitionSet$cohortId))) stop("*** duplicate cohort IDs ***")

targetId      <- 1797941L
nhdOutcomeId  <- 9100001L
covariateIds  <- setdiff(cohortDefinitionSet$cohortId, c(targetId, nhdOutcomeId))

# ==============================================================================
# 2. SENTINEL GUARD — protect the hand-authored NHD SQL
# ==============================================================================
# inst/sql/sql_server/9100001.sql is hand-authored because circe cannot express
# a discharge-disposition cohort (see inst/cohorts/9100001.json). Its companion
# JSON is a placeholder that entered as ALL INPATIENT VISITS.
#
# The failure this guards against is silent, not loud: if anyone ever runs
# CirceR::buildCohortQuery() over that placeholder and writes the result to the
# .sql, the NHD outcome quietly becomes "every inpatient admission". The run
# still completes, the report still renders, and the metrics still look
# plausible — they are just measuring the wrong thing. Fail the build instead.
nhdSqlPath <- file.path("inst", "sql", "sql_server", paste0(nhdOutcomeId, ".sql"))
if (!file.exists(nhdSqlPath)) {
  stop("*** ", nhdSqlPath, " is missing. The non-home discharge cohort is ",
       "hand-authored SQL and cannot be regenerated from its placeholder JSON. ",
       "Restore it from git. ***")
}
nhdSql <- readLines(nhdSqlPath, warn = FALSE)
if (!any(grepl("UB04 Pt dis status", nhdSql, fixed = TRUE))) {
  stop("*** ", nhdSqlPath, " has lost its UB04 home-discharge CTE.\n",
       "    The hand-authored non-home discharge logic was almost certainly ",
       "overwritten by a CirceR re-render from the placeholder JSON, which ",
       "would silently redefine the outcome as ALL INPATIENT VISITS.\n",
       "    Restore the file from git; do not regenerate it. ***")
}
if (any(grepl("@outcome_cohort_id", nhdSql, fixed = TRUE))) {
  stop("*** ", nhdSqlPath, " still references @outcome_cohort_id. ",
       "CohortGenerator supplies @target_cohort_id and renders with ",
       "warnOnMissingParameters = FALSE, so this would reach SQL Server ",
       "un-substituted. ***")
}
message("[spec] Sentinel guard passed: NHD SQL retains its UB04 CTE.")

# ==============================================================================
# 3. Modules
# ==============================================================================

# ---- CohortGenerator ---------------------------------------------------------
cg <- CohortGeneratorModule$new()
cohortDefinitionShared <- cg$createCohortSharedResourceSpecifications(cohortDefinitionSet)
cohortGeneratorSpecs   <- cg$createModuleSpecifications(generateStats = TRUE)

# ---- CohortDiagnostics -------------------------------------------------------
# Scoped to the TARGET only.
#   - The covariate cohorts (9100002-9100010, 1797949-1797952) are covariate
#     DEFINITIONS, not phenotypes to diagnose. Running seven sub-analyses on
#     each was ~88% of total diagnostics runtime in pad-amp-ed-desc for no
#     manuscript value.
#   - The NHD outcome is EXCLUDED on purpose: its cohort JSON is a placeholder
#     whose concept sets do not describe the real (hand-authored) logic, so any
#     concept-level diagnostic on it would be actively misleading.
# runInclusionStatistics = FALSE avoids CohortGenerator::insertInclusionRuleNames
# crashing on a backslash results schema (dhe\netid) on Duke PRCC; attrition
# still comes from the CohortGenerator module.
cd <- CohortDiagnosticsModule$new()
cohortDiagnosticsSpecs <- cd$createModuleSpecifications(
  cohortIds                         = c(targetId),
  runInclusionStatistics            = FALSE,
  runIncludedSourceConcepts         = TRUE,
  runOrphanConcepts                 = TRUE,
  runBreakdownIndexEvents           = TRUE,
  runVisitContext                   = TRUE,
  runIncidenceRate                  = FALSE,
  runCohortRelationship             = TRUE,
  runTemporalCohortCharacterization = FALSE,
  minCharacterizationMean           = 0.01
)

# ---- Characterization (Table 1) ---------------------------------------------
# minPriorObservation = 0: amputation patients frequently lack 365d of prior
# observation, and study_params.yaml sets min_prior_observation_days = 1 for the
# same reason.
#
# includeRiskFactors / includeDechallengeRechallenge / includeCaseSeries MUST
# stay FALSE: Characterization 3.0.1 emits raw IFNULL(...) in those SQL paths,
# which is not a SQL Server built-in ("'IFNULL' is not a recognized built-in
# function name"). None are needed for this study.
timeAtRisks <- tibble(
  label           = c("1 to 90d"),
  riskWindowStart = c(1),
  startAnchor     = c("cohort start"),
  riskWindowEnd   = c(90),
  endAnchor       = c("cohort start")
)
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == nhdOutcomeId) %>%
  transmute(outcomeCohortId = cohortId, outcomeCohortName = cohortName, cleanWindow = 0)

ch <- CharacterizationModule$new()
characterizationSpecs <- ch$createModuleSpecifications(
  targetIds                    = targetId,
  outcomeIds                   = oList$outcomeCohortId,
  minPriorObservation          = 0,
  outcomeWashoutDays           = rep(0, nrow(oList)),
  riskWindowStart              = timeAtRisks$riskWindowStart,
  startAnchor                  = timeAtRisks$startAnchor,
  riskWindowEnd                = timeAtRisks$riskWindowEnd,
  endAnchor                    = timeAtRisks$endAnchor,
  minCharacterizationMean      = 0.01,
  includeTargetBaseline        = TRUE,
  includeTimeToEvent           = TRUE,
  includeRiskFactors           = FALSE,
  includeDechallengeRechallenge = FALSE,
  includeCaseSeries            = FALSE
)

# ==============================================================================
# 4. De-novo model development arm — SCAFFOLDED BUT DISABLED
# ==============================================================================
# Deliberately not fitted. The candidate predictor set and modelling approach
# are a study-team decision that has not been made, and a placeholder model
# fitted on synthetic data would produce numbers that look like results.
#
# Strategus has no per-module "enabled" flag — a module either is or is not in
# the specification — so the switch below gates addModuleSpecifications() while
# the spec object itself is still constructed unconditionally. That keeps the
# intended design reviewable, type-checked, and diffable rather than living in
# a comment.
#
# THE GATE IS THE ONLY THING KEEPING THIS FROM RUNNING. An earlier version of
# this block passed modelSettings = NULL on the theory that the design would
# then be inert even if the module were added; that is wrong.
# PatientLevelPrediction::createModelDesign() validates the class and rejects
# NULL outright ("modelSettings is wrong class"), so a real setting must be
# supplied for the object to construct at all. The LASSO below is therefore a
# PLACEHOLDER, not a decision — flipping ENABLE_PLP_DEVELOPMENT to TRUE without
# first agreeing the predictor set and approach WILL fit and report a model.
ENABLE_PLP_DEVELOPMENT <- FALSE

plpDevelopmentSpecs <- PatientLevelPredictionModule$new()$createModuleSpecifications(
  modelDesignList = list(
    PatientLevelPrediction::createModelDesign(
      targetId  = targetId,
      outcomeId = nhdOutcomeId,
      populationSettings = PatientLevelPrediction::createStudyPopulationSettings(
        binary                    = TRUE,
        riskWindowStart           = 1,
        startAnchor               = "cohort start",
        riskWindowEnd             = 90,
        endAnchor                 = "cohort start",
        requireTimeAtRisk         = FALSE,
        removeSubjectsWithPriorOutcome = FALSE
      ),
      # TODO [PLP-DESIGN]: candidate predictors. This placeholder reuses the
      # scored component cohorts so the design is concrete and runnable; the
      # intended set is those PLUS a large-scale FeatureExtraction covariate
      # set. Requires a study-team decision before enabling.
      #
      # NOTE on startDay/endDay: cohort-based covariates use FeatureExtraction's
      # interval-OVERLAP semantics, which coincide with the custom step's
      # start-in-window semantics only because every covariate cohort here exits
      # at StartDate+0. Changing a covariate cohort's EndStrategy would silently
      # desynchronise this arm from the scoring step.
      covariateSettings = FeatureExtraction::createCohortBasedCovariateSettings(
        analysisId       = 150,
        covariateCohorts = data.frame(
          cohortId   = covariateIds,
          cohortName = paste0("cov_", covariateIds)
        ),
        valueType = "binary",
        startDay  = -3650,
        endDay    = -1
      ),
      # TODO [PLP-DESIGN]: PLACEHOLDER ONLY — not a chosen model. LASSO logistic
      # regression is the conventional OHDSI starting point and is used here
      # solely because createModelDesign() will not construct without a valid
      # modelSettings object. Alternatives to weigh once scope is agreed:
      #   PatientLevelPrediction::setGradientBoostingMachine()
      #   PatientLevelPrediction::setRandomForest()
      modelSettings = PatientLevelPrediction::setLassoLogisticRegression(),
      splitSettings = PatientLevelPrediction::createDefaultSplitSetting(
        type = "time", testFraction = 0.25, nfold = 3
      )
    )
  ),
  skipDiagnostics = FALSE
)

# ==============================================================================
# 5. Assemble and serialise
# ==============================================================================
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorSpecs) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsSpecs) |>
  Strategus::addModuleSpecifications(characterizationSpecs)

if (ENABLE_PLP_DEVELOPMENT) {
  analysisSpecifications <- analysisSpecifications |>
    Strategus::addModuleSpecifications(plpDevelopmentSpecs)
  message("[spec] PatientLevelPredictionModule ENABLED — a model WILL be fitted.")
} else {
  message("[spec] PatientLevelPredictionModule scaffolded but DISABLED ",
          "(ENABLE_PLP_DEVELOPMENT = FALSE). No model is fitted; the de-novo ",
          "arm awaits a study-team decision on predictors and approach.")
}

specPath <- file.path("inst", "padAmpNhdProgAnalysisSpecification.json")
ParallelLogger::saveSettingsToJson(analysisSpecifications, specPath)

message("[spec] Wrote ", specPath)
message("[spec]   cohorts   : ", nrow(cohortDefinitionSet),
        " (target ", targetId, ", outcome ", nhdOutcomeId,
        ", ", length(covariateIds), " covariate)")
message("[spec]   modules   : CohortGenerator, CohortDiagnostics, Characterization",
        if (ENABLE_PLP_DEVELOPMENT) ", PatientLevelPrediction" else "")
message("[spec]   scoring   : the three published scores run in the custom step ",
        "(scripts/analysis/integer_score_validation.R), not in Strategus.")
