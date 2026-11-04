# =============================================================================
# R/synthetic_provenance.R
#
# PURPOSE
# -------
# Reads inst/synthetic_data_provenance.yaml and logs which synthetic dataset
# this version of the analysis code was developed and tested against. Called at
# the start of a run (StrategusCodeToRun.R, and the Duke bundle's run_analysis.R)
# so every run log, including one from the secure environment that analyses real
# data, carries the development-data tag.
#
# INPUTS
# ------
#   inst/synthetic_data_provenance.yaml  the provenance record (see its header)
#
# OUTPUTS
# -------
#   A short block of log lines. Returns the parsed record invisibly.
#   Never stops a run: a missing or unreadable record is a warning, because the
#   record documents development and must not block an analysis.
#
# FUNCTIONS
# ---------
#   read_synthetic_provenance()  parse and validate the record
#   log_synthetic_provenance()   print it as a log block
# =============================================================================

# Fields the record must carry for the log (and the manuscript's repository table)
# to be meaningful. Kept here so the regression test and the logger agree.
SYNTHETIC_PROVENANCE_REQUIRED <- c(
  "dataset$id", "dataset$version", "dataset$physical_schema", "dataset$persons",
  "generator$repo", "generator$commit", "release$tag", "release$asset",
  "release$sha256", "release$status"
)

# -----------------------------------------------------------------------------
# read_synthetic_provenance()
#
# Reads and validates the provenance record.
#
# @param path  Path to the YAML record. Default is the one shipped in inst/.
# @return      The record as a list. Stops (so callers can catch it) if the file
#              is missing, unparseable, or lacks a required field.
# -----------------------------------------------------------------------------
read_synthetic_provenance <- function(path = file.path("inst", "synthetic_data_provenance.yaml")) {
  if (!file.exists(path)) stop("provenance record not found: ", path)
  rec <- yaml::read_yaml(path)

  # Walk each "section$field" requirement through the parsed list.
  for (req in SYNTHETIC_PROVENANCE_REQUIRED) {
    parts <- strsplit(req, "$", fixed = TRUE)[[1]]
    val <- rec[[parts[1]]][[parts[2]]]
    if (is.null(val) || identical(val, "")) stop("provenance record is missing '", req, "'")
  }

  # A sha256 is 64 hex characters; anything else means a paste error that would
  # make the published asset unverifiable.
  if (!grepl("^[0-9a-f]{64}$", rec$release$sha256)) {
    stop("release$sha256 is not a 64-character lowercase hex digest")
  }
  rec
}

# -----------------------------------------------------------------------------
# log_synthetic_provenance()
#
# Prints the development-data tag as a log block.
#
# @param path  Path to the YAML record.
# @return      The record invisibly, or NULL if it could not be read.
# Side effects: writes lines to the console / run log via message().
# -----------------------------------------------------------------------------
log_synthetic_provenance <- function(path = file.path("inst", "synthetic_data_provenance.yaml")) {
  rec <- tryCatch(read_synthetic_provenance(path), error = function(e) {
    warning("Synthetic-data provenance not logged: ", conditionMessage(e), call. = FALSE)
    NULL
  })
  if (is.null(rec)) return(invisible(NULL))

  d <- rec$dataset; g <- rec$generator; r <- rec$release
  message("[provenance] ----------------------------------------------------------------")
  message("[provenance] This code version was developed against SYNTHETIC data (development only):")
  message("[provenance]   dataset   : ", d$id, " ", d$version, " (", d$persons, " synthetic persons, ",
          "schema ", d$physical_schema, ")")
  message("[provenance]   release   : ", r$repo, "@", r$tag, " [", r$status, "]",
          if (!is.null(r$doi)) paste0("  doi:", r$doi) else "")
  message("[provenance]   asset     : ", r$asset, "  sha256 ", substr(r$sha256, 1, 16), "...")
  message("[provenance]   generator : ", g$repo, " @ ", substr(g$commit, 1, 7),
          " (Synthea ", g$synthea_version, ")")
  message("[provenance]   verified  : ", rec$verified$date)
  message("[provenance] Results from this run come from whatever CDM the run is connected to,")
  message("[provenance] not from this synthetic dataset.")
  message("[provenance] ----------------------------------------------------------------")
  invisible(rec)
}
