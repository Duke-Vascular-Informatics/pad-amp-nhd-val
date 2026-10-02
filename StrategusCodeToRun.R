################################################################################
# StrategusCodeToRun.R  —  PIPELINE STEP 2
#
# Runs the pad-amp-nhd-val analysis in the devcontainer against the
# pad_amp_dispo synthetic CDM: Strategus, the custom scoring step, and the
# report-input extract. This repo is Strategus-faithful and stops here —
# the Word manuscript is a SEPARATE repo, pad-amp-nhd-val-report, run as its
# own step against this run's output/ directory. See charon's "Multi-Repo Analysis
# Pipeline" README section (https://github.com/Duke-Vascular-Informatics/charon#multi-repo-analysis-pipeline; split made 2026-08-11) for why the report was moved out entirely rather than
# just having its database access removed.
#
#   1  CreateStrategusAnalysisSpecification.R  (run this first)
#   2  THIS SCRIPT
#   9  workflow/09_build_portable_analysis_bundle.sh
#   —  pad-amp-nhd-val-report/GenerateReport.R (separate repo, run manually
#      after this script, pointed at this script's output/ directory)
#
# WHAT RUNS, IN ORDER
#   Strategus::execute()  -> generates 15 cohorts, CohortDiagnostics on the
#                            target, Characterization for Table 1
#   custom step           -> applies the three published integer risk scores
#                            (scripts/analysis/integer_score_validation.R)
#   extract                -> writes report_inputs/*.csv for the report repo
#                            to consume (R/extract_report_inputs.R)
#
# DATA SOURCE
#   Physical CDM  : omop_synth_pad_amp_v2   (built by pad-amp-dispo-synth,
#                   registry id pad_amp, version v2 as of the 2026-08-09
#                   consolidation). MUST match the physical schema the
#                   overlay below was actually built from -- these drifting
#                   apart is a silent cross-schema person_id join, not an
#                   error. Found out of sync 2026-08-11: this default (and
#                   study_params.yaml's cdm_schema) had been left at v1
#                   (omop_synth_pad_amp_dispo, 1,474 persons) while the
#                   already-built overlay pointed at v2 (1,544 persons).
#   Strategus sees: pad_amp_nhd_val_cdm_test, a schema of read-only views over
#                   the physical CDM UNION omop_vocab. The overlay exists because
#                   Strategus::createCdmExecutionSettings has no separate
#                   vocabulary-schema parameter, so clinical and vocabulary
#                   tables must live in one schema. The pre-flight below builds
#                   it automatically if it is missing or empty.
#
#   The custom step and report query the PHYSICAL schema directly (via
#   config.R's cdm_schema), because they issue their own SQL and do not need
#   the union.
#
# RUN IN A FRESH R SESSION — rJava can only initialise the JVM once per process.
################################################################################

# ---- JVM guard ---------------------------------------------------------------
loadedJavaNs <- intersect(c("rJava", "DatabaseConnector", "PatientLevelPrediction"),
                          loadedNamespaces())
if (length(loadedJavaNs) > 0) {
  stop("Run this script in a FRESH R session. Already-loaded namespaces: ",
       paste(loadedJavaNs, collapse = ", "))
}

Sys.setenv("_JAVA_OPTIONS" = "-Xmx4g")  # FeatureExtraction/Andromeda need heap headroom
Sys.setenv("VROOM_THREADS" = 1)         # single-threaded vroom avoids FS deadlocks
Sys.setenv(DATABASECONNECTOR_JAR_FOLDER = "/workspace/synthea-omop-template/drivers/jdbc-runtime")

# ========== START OF SITE INPUTS =============================================

# Two-part "database.schema" form is REQUIRED — on this SQL Server instance
# CohortGenerator misreads a bare schema name as the database name.
omopDatabase       <- Sys.getenv("OMOP_DATABASE", unset = "omop_synth")
overlaySchemaBare  <- Sys.getenv("OMOP_CDM_SCHEMA_OVERRIDE",     unset = "pad_amp_nhd_val_cdm_test")
resultsSchemaBare  <- Sys.getenv("OMOP_RESULTS_SCHEMA_OVERRIDE", unset = "pad_amp_nhd_val_results")
physicalCdmSchema  <- Sys.getenv("OMOP_PHYSICAL_CDM_SCHEMA",     unset = "omop_synth_pad_amp_v2")
vocabSchemaBare    <- Sys.getenv("OMOP_VOCAB_SCHEMA",            unset = "omop_vocab")
overlayRegistryId  <- "pad_amp_dispo"   # synthetic_data/registry.yaml id (former_ids -> pad_amp)

cdmDatabaseSchema  <- paste0(omopDatabase, ".", overlaySchemaBare)
workDatabaseSchema <- paste0(omopDatabase, ".", resultsSchemaBare)

databaseName    <- Sys.getenv("OMOP_CDM_DATABASE_NAME", unset = "DevContainerSynthea")
outputLocation  <- file.path(getwd(), "results")   # gitignored
minCellCount    <- 5                                # OHDSI small-cell suppression default
cohortTableName <- "pad_amp_nhd_val"

connectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms     = "sql server",
  server   = Sys.getenv("OMOP_SERVER", unset = Sys.getenv("MSSQL_HOST", unset = "mssql_dev")),
  user     = Sys.getenv("MSSQL_USER", unset = "sa"),
  password = Sys.getenv("MSSQL_SA_PASSWORD"),
  extraSettings = paste0(
    "database=", omopDatabase,
    ";trustServerCertificate=true",
    ";portNumber=", Sys.getenv("MSSQL_PORT", unset = "1433"),
    ";socketTimeout=0",
    ";queryTimeout=0"
  )
)

# ========== END OF SITE INPUTS ===============================================

# ---- Connection smoke test ---------------------------------------------------
conn <- DatabaseConnector::connect(connectionDetails)
DatabaseConnector::querySql(conn, "SELECT 1 AS ok;")
DatabaseConnector::disconnect(conn)
message("Connection preflight passed: ", databaseName, " (cdm=", cdmDatabaseSchema, ")")

# ---- Overlay schema pre-flight -----------------------------------------------
# SQL Server being reachable does not mean the overlay schema has any tables.
# Without this check, a missing overlay surfaces much later as a raw
# "Invalid object name" deep inside CohortGenerator.
conn <- DatabaseConnector::connect(connectionDetails)
overlayTables <- DatabaseConnector::getTableNames(conn, databaseSchema = cdmDatabaseSchema)
DatabaseConnector::disconnect(conn)

if (length(overlayTables) == 0) {
  message("Overlay schema '", cdmDatabaseSchema, "' has no tables -- building it via ",
          "generate_overlay_schema.R (registry id: ", overlayRegistryId, ").")
  rebuildStatus <- system2(
    "Rscript",
    c("../synthetic_data/scripts/generate_overlay_schema.R",
      "--source",        paste0(omopDatabase, ".", physicalCdmSchema),
      "--target",        overlaySchemaBare,
      "--vocab-schema",  paste0(omopDatabase, ".", vocabSchemaBare),
      "--registry-id",   overlayRegistryId)
  )
  if (rebuildStatus != 0) {
    stop("Overlay schema auto-build failed (see output above). Build the physical ",
         "schema '", physicalCdmSchema, "' first — see synthetic_data/README.md.")
  }
  conn <- DatabaseConnector::connect(connectionDetails)
  overlayTables <- DatabaseConnector::getTableNames(conn, databaseSchema = cdmDatabaseSchema)
  DatabaseConnector::disconnect(conn)
  if (length(overlayTables) == 0) {
    stop("Overlay schema '", cdmDatabaseSchema, "' is still empty after the auto-build.")
  }
  message("Overlay auto-build succeeded: ", length(overlayTables), " tables.")
} else {
  message("Overlay schema preflight passed: ", length(overlayTables), " tables in ",
          cdmDatabaseSchema, ".")
}

# ---- Execute Strategus -------------------------------------------------------
specPath <- file.path("inst", "padAmpNhdValAnalysisSpecification.json")
if (!file.exists(specPath)) {
  stop("Analysis specification not found: ", specPath,
       "\n  Run step 1 first: Rscript CreateStrategusAnalysisSpecification.R")
}

# ---- Staleness guard: the spec is a BUILD ARTIFACT ---------------------------
# Strategus reads the cohort definitions out of this JSON, not off disk. Editing
# inst/cohorts/*.json or inst/Cohorts.csv without re-running step 1 means
# Strategus silently generates the PREVIOUS definitions while the repo shows the
# new ones — and the run completes normally with quietly wrong cohorts.
#
# This happened during development: a covariate cohort was widened, the
# regression test (which regenerates from disk) reported the new value, and the
# full pipeline reported the old one. Only the disagreement between the two
# surfaced it. Compare timestamps and refuse to run on a stale spec.
local({
  sources <- c(list.files(file.path("inst", "cohorts"), pattern = "[.]json$", full.names = TRUE),
               file.path("inst", "Cohorts.csv"),
               file.path("inst", "sql", "sql_server") |>
                 list.files(pattern = "[.]sql$", full.names = TRUE))
  sources <- sources[file.exists(sources)]
  specTime <- file.info(specPath)$mtime
  newer    <- sources[file.info(sources)$mtime > specTime]
  if (length(newer) > 0) {
    stop("*** The analysis specification is STALE.\n",
         "    ", specPath, "\n    was built ", format(specTime),
         " but these definition files have changed since:\n",
         paste0("      ", basename(newer), collapse = "\n"),
         "\n\n    Strategus reads cohort definitions from the specification, not from\n",
         "    disk, so running now would generate the PREVIOUS definitions while the\n",
         "    repository shows the new ones — silently, with a clean exit.\n\n",
         "    Rebuild first:  Rscript CreateStrategusAnalysisSpecification.R ***")
  }
  message("Specification freshness check passed (", length(sources),
          " definition files, none newer than the spec).")
})

analysisSpecifications <- ParallelLogger::loadSettingsFromJson(specPath)

executionSettings <- Strategus::createCdmExecutionSettings(
  workDatabaseSchema = workDatabaseSchema,
  cdmDatabaseSchema  = cdmDatabaseSchema,
  cohortTableNames   = CohortGenerator::getCohortTableNames(cohortTable = cohortTableName),
  workFolder         = file.path(outputLocation, databaseName, "strategusWork"),
  resultsFolder      = file.path(outputLocation, databaseName, "strategusOutput"),
  minCellCount       = minCellCount
)
dir.create(file.path(outputLocation, databaseName), recursive = TRUE, showWarnings = FALSE)
ParallelLogger::saveSettingsToJson(
  executionSettings,
  file.path(outputLocation, databaseName, "executionSettings.json")
)

Strategus::execute(
  analysisSpecifications = analysisSpecifications,
  executionSettings      = executionSettings,
  connectionDetails      = connectionDetails
)
message("Strategus execution complete -> ",
        file.path(outputLocation, databaseName, "strategusOutput"))

# ---- Load study config ---------------------------------------------------------
# Moved up from just before the scoring step below: restrict_to_study_period()
# (right after the NHD-cohort repair) needs config$study_start_date /
# config$study_end_date, so config must exist before that point now.
source("config.R")
config <- get_validation_config()

# ---- Repair the NHD cohort ----------------------------------------------------
# MUST run before scoring. Strategus does not execute
# inst/sql/sql_server/9100001.sql — its specification stores only each cohort's
# JSON, and the CohortGeneratorModule re-renders SQL from that JSON with CirceR.
# For 9100001 the JSON is a placeholder (circe cannot express a
# discharge-disposition criterion), so Strategus has just written "every
# inpatient visit" into the cohort table. Left alone, every target patient would
# have an outcome, the outcome would be constant, and every metric would be NA.
#
# See R/generate_nhd_cohort.R for the full explanation and the behavioural
# assertion that fails the run if the placeholder logic ever takes effect.
source("R/generate_nhd_cohort.R")
conn <- DatabaseConnector::connect(connectionDetails)
generate_nhd_cohort(
  connection           = conn,
  cdmDatabaseSchema    = cdmDatabaseSchema,     # overlay: has concept + concept_relationship
  cohortDatabaseSchema = workDatabaseSchema,
  cohortTable          = cohortTableName
)
DatabaseConnector::disconnect(conn)

# ---- Restrict the target cohort to the study period ---------------------------
# study_start_date/study_end_date (study_params.yaml) have been configured since
# this study's original synthea-omop-template incarnation, where R/cohorts.R
# applied them as a literal cohort_start_date BETWEEN filter right after
# CohortGenerator populated the cohort table. That SQL step was never carried
# forward into the Strategus port -- circe's cohort JSON has no primary-event
# date-range field (this cohort's CensorWindow is empty, and CensorWindow only
# bounds cohort END dates via end-strategy calculations in any case), so
# CohortGenerator applied no date bound at all. The two config fields were
# silently reduced to report-narrative text only (see extract_report_inputs.R's
# report_config$study_end_date) -- confirmed as a live problem, not theoretical,
# by a real PRCC run whose agg_nhd_by_year.csv had a genuine 2026 row (24
# patients) despite study_end_date being "2025-12-31": Duke's live CDM keeps
# accruing amputation encounters past any date fixed at protocol-writing time,
# and nothing was stopping them from entering this cohort. See
# R/restrict_to_study_period.R for the full rationale and the exact SQL.
source("R/restrict_to_study_period.R")
conn <- DatabaseConnector::connect(connectionDetails)
studyPeriodExclusion <- restrict_to_study_period(
  connection           = conn,
  cohortDatabaseSchema = workDatabaseSchema,
  cohortTable          = cohortTableName,
  studyStartDate       = config$study_start_date,
  studyEndDate         = config$study_end_date
)
DatabaseConnector::disconnect(conn)

# ---- Exclude facility-admitted patients from the target cohort ---------------
# Order relative to generate_nhd_cohort() above does not matter (that
# function computes NHD independently, with no dependency on 9100011
# membership) -- this just has to run before the scoring step below, which
# is the first thing that reads cohort 9100011's membership. Runs after the
# study-period restriction above so the CONSORT funnel's stage order matches
# the pipeline's actual execution order. See R/exclude_facility_admissions.R
# for the full rationale, the verified admission-source code mapping, and why
# this does not need the 9100001-style placeholder-JSON escape-hatch machinery.
source("R/exclude_facility_admissions.R")
conn <- DatabaseConnector::connect(connectionDetails)
facilityExclusion <- exclude_facility_admissions(
  connection           = conn,
  cdmDatabaseSchema    = cdmDatabaseSchema,
  cohortDatabaseSchema = workDatabaseSchema,
  cohortTable          = cohortTableName
)
DatabaseConnector::disconnect(conn)

# ---- Custom step: apply the three published integer risk scores --------------
# Strategus has populated the cohort table; the scores are computed from it.
# (config was loaded earlier, right after Strategus::execute() -- see above.)

# config.R carries the PHYSICAL cdm schema (the custom step issues its own SQL
# and does not need the vocab union), but results_schema/cohort_table must match
# what Strategus was just told to write to, or the scoring step reads an empty
# or stale cohort table.
config$results_schema <- resultsSchemaBare
config$cohort_table   <- cohortTableName

source("R/risk_score_pipeline.R")
source("scripts/analysis/integer_score_validation.R")
run_integer_score_validation(connectionDetails, config)

# ---- Extract report inputs -------------------------------------------------
# This is where this repo's responsibility ENDS. Per charon's "Multi-Repo Analysis
# Pipeline" README section (https://github.com/Duke-Vascular-Informatics/charon#multi-repo-analysis-pipeline; split made 2026-08-11) the Word report moved out entirely, into its own repo
# — pad-amp-nhd-val-report — so that this repo can stay Strategus-faithful:
# cohorts, spec, the retained scoring step, and this extract layer, which turns
# CDM queries into CSV artifacts. Nothing past this point imports ggplot2,
# officer, or flextable, and nothing here builds a Word document.
source("R/extract_report_inputs.R")
message("Extracting report inputs from the CDM ...")
extractResult <- extract_report_inputs(
  config                    = config,
  connection_details        = connectionDetails,
  strategus_output_dir      = file.path(outputLocation, databaseName, "strategusOutput"),
  facility_exclusion        = facilityExclusion,
  study_period_exclusion    = studyPeriodExclusion
)

# ---- Aggregate report inputs ------------------------------------------------
# Reduces this run's PERSON-LEVEL output to aggregate-only artifacts so the
# report repo can render every figure and table without any patient-level data
# (see R/aggregate_report_inputs.R's header for how ROC/DCA/tiers are exact
# under this reduction, not approximated).
#
# demog_age/discharge_types are extract_report_inputs()'s in-memory return
# values, passed straight through in the same R session — as of 2026-08-19,
# neither is ever written to disk as its own file; see both scripts'
# REVISION notes. Runs AFTER the scoring step, which is what produces the
# person_level_scores.csv files this step reads from disk.
source("R/aggregate_report_inputs.R")
aggregate_report_inputs(
  config,
  min_cell_count  = minCellCount,
  demog_age       = extractResult$demog_age,
  discharge_types = extractResult$discharge_types
)

# PHI: edge_case_export.R (writes output/pad_amp_nhd_edge_<date>.csv containing
# MRN, age, procedure date) moved to duke-prcc-deploy/studies/pad-amp-nhd-val/
# 2026-08-11 -- it does not belong in this shareable analysis-core repo. Run it
# from a clone of duke-prcc-deploy on Duke PRCC, pointed at this run's config
# and person_level_scores.csv, once that study's PRCC runtime layer exists
# (see that repo's studies/pad-amp-nhd-val/README.md).
message(
  "Edge-case export moved to duke-prcc-deploy/studies/pad-amp-nhd-val/ -- run ",
  "it from there on Duke PRCC, not from this repo."
)

message(
  "Analysis complete. To generate the Word report, run GenerateReport.R from ",
  "a clone of pad-amp-nhd-val-report with RESULTS_DIR=", config$output_folder,
  " — see that repo's README."
)
message("Done.")
