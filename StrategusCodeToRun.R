################################################################################
# StrategusCodeToRun.R  —  PIPELINE STEP 2
#
# Runs the pad-amp-nhd-prog analysis in the devcontainer against the
# pad_amp_dispo synthetic CDM, then the custom scoring step and the report.
#
#   1  CreateStrategusAnalysisSpecification.R  (run this first)
#   2  THIS SCRIPT
#   9  workflow/09_build_portable_analysis_bundle.sh
#
# WHAT RUNS, IN ORDER
#   Strategus::execute()  -> generates 15 cohorts, CohortDiagnostics on the
#                            target, Characterization for Table 1
#   custom step           -> applies the three published integer risk scores
#                            (scripts/analysis/integer_score_validation.R)
#   report                -> Word manuscript (R/report_extended.R)
#
# DATA SOURCE
#   Physical CDM  : omop_synth_pad_amp_dispo   (built by pad-amp-dispo-synth,
#                   registry id pad_amp_dispo)
#   Strategus sees: pad_amp_nhd_prog_cdm_test, a schema of read-only views over
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
overlaySchemaBare  <- Sys.getenv("OMOP_CDM_SCHEMA_OVERRIDE",     unset = "pad_amp_nhd_prog_cdm_test")
resultsSchemaBare  <- Sys.getenv("OMOP_RESULTS_SCHEMA_OVERRIDE", unset = "pad_amp_nhd_prog_results")
physicalCdmSchema  <- Sys.getenv("OMOP_PHYSICAL_CDM_SCHEMA",     unset = "omop_synth_pad_amp_dispo")
vocabSchemaBare    <- Sys.getenv("OMOP_VOCAB_SCHEMA",            unset = "omop_vocab")
overlayRegistryId  <- "pad_amp_dispo"   # synthetic_data/registry.yaml id

cdmDatabaseSchema  <- paste0(omopDatabase, ".", overlaySchemaBare)
workDatabaseSchema <- paste0(omopDatabase, ".", resultsSchemaBare)

databaseName    <- Sys.getenv("OMOP_CDM_DATABASE_NAME", unset = "DevContainerSynthea")
outputLocation  <- file.path(getwd(), "results")   # gitignored
minCellCount    <- 5                                # OHDSI small-cell suppression default
cohortTableName <- "pad_amp_nhd_prog"

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
specPath <- file.path("inst", "padAmpNhdProgAnalysisSpecification.json")
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

# ---- Custom step: apply the three published integer risk scores --------------
# Strategus has populated the cohort table; the scores are computed from it.
source("config.R")
config <- get_validation_config()

# config.R carries the PHYSICAL cdm schema (the custom step issues its own SQL
# and does not need the vocab union), but results_schema/cohort_table must match
# what Strategus was just told to write to, or the scoring step reads an empty
# or stale cohort table.
config$results_schema <- resultsSchemaBare
config$cohort_table   <- cohortTableName

source("R/risk_score_pipeline.R")
source("scripts/analysis/integer_score_validation.R")
run_integer_score_validation(connectionDetails, config)

# ---- Report ------------------------------------------------------------------
source("R/report_extended.R")
generate_manuscript_report(
  output_dir         = config$output_folder,
  score_output_dir   = file.path(config$output_folder, "iannuzzi"),
  mfi5_output_dir    = file.path(config$output_folder, "mfi5"),
  vqifs_output_dir   = file.path(config$output_folder, "vqifs"),
  connection_details = connectionDetails,
  config             = config
)
message("Done.")
