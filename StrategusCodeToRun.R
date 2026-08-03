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
