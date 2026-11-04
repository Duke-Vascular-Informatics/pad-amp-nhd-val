#!/usr/bin/env Rscript
# =============================================================================
# tests/regression/test_synthetic_provenance.R
#
# PURPOSE
# -------
# Guards inst/synthetic_data_provenance.yaml, the record of which synthetic
# dataset this code version was developed against. No database needed.
#
# CHECKS
# ------
#   1. The record parses and carries every required field (R/synthetic_provenance.R).
#   2. The dataset version in the record matches the physical schema suffix, so a
#      repoint to v4 that forgets one of the two fails here.
#   3. If a local export zip is present (synthetic_data/exports/<id>_clinical_tables.zip
#      in the lab workspace), its sha256 equals the recorded one. Skipped, with a
#      note, when no zip is available (for example on a bare clone).
#   4. A record marked `published` names a published date; one marked `pending`
#      is reported so it is not forgotten.
#   5. log_synthetic_provenance() runs and mentions the dataset tag.
#
# USAGE
#   Rscript tests/regression/test_synthetic_provenance.R      # from the study folder
# =============================================================================

source("R/synthetic_provenance.R")

failures <- character()
fail <- function(msg) { failures <<- c(failures, msg); cat("  FAIL:", msg, "\n") }
ok   <- function(msg) cat("  ok  :", msg, "\n")

# ---- 1. parses and is complete ---------------------------------------------
rec <- tryCatch(read_synthetic_provenance(), error = function(e) { fail(conditionMessage(e)); NULL })
if (!is.null(rec)) ok("record parses and has every required field")

if (!is.null(rec)) {
  # ---- 2. version agrees with the schema name --------------------------------
  # omop_synth_pad_amp_v3 -> "v3": the suffix after the last underscore.
  suffix <- sub("^.*_", "", rec$dataset$physical_schema)
  if (identical(suffix, rec$dataset$version)) ok(paste("version", rec$dataset$version, "matches schema", rec$dataset$physical_schema))
  else fail(paste0("dataset version '", rec$dataset$version, "' does not match schema suffix '", suffix, "'"))

  # The release tag should name the same dataset and version.
  expected_tag <- paste0("synth-data-", rec$dataset$id, "-", rec$dataset$version)
  if (identical(rec$release$tag, expected_tag)) ok(paste("release tag", expected_tag))
  else fail(paste0("release tag '", rec$release$tag, "' should be '", expected_tag, "'"))

  # ---- 3. checksum against a local export, when there is one -----------------
  zip_candidates <- c(
    file.path("..", "synthetic_data", "exports", rec$release$asset),
    file.path("synthetic_data", "exports", rec$release$asset),
    file.path("/workspace", "synthetic_data", "exports", rec$release$asset)
  )
  zip_path <- zip_candidates[file.exists(zip_candidates)][1]
  if (is.na(zip_path)) {
    cat("  skip: no local export zip found to checksum (", rec$release$asset, ")\n", sep = "")
  } else {
    # sha256sum on Linux (the dev container), shasum on macOS; whichever exists.
    tool <- if (nzchar(Sys.which("sha256sum"))) c("sha256sum") else c("shasum", "-a", "256")
    sha <- system2(tool[1], c(tool[-1], shQuote(zip_path)), stdout = TRUE)
    sha <- strsplit(sha, " ")[[1]][1]
    if (identical(sha, rec$release$sha256)) ok("local export sha256 equals the recorded sha256")
    else fail(paste0("local export sha256 ", sha, " differs from the record"))
  }

  # ---- 4. status consistency -------------------------------------------------
  if (identical(rec$release$status, "published")) {
    if (is.null(rec$release$published_date)) fail("status is published but published_date is empty")
    else ok(paste("published", rec$release$published_date))
  } else {
    cat("  note: release status is '", rec$release$status, "' -- update after the GitHub release is created\n", sep = "")
  }

  # ---- 5. the logger runs and names the dataset ------------------------------
  out <- capture.output(log_synthetic_provenance(), type = "message")
  if (any(grepl(rec$release$tag, out, fixed = TRUE))) ok("logger output names the release tag")
  else fail("logger output does not mention the release tag")
}

if (length(failures)) {
  cat("\nSYNTHETIC PROVENANCE TEST FAILED (", length(failures), " problem(s))\n", sep = "")
  quit(status = 1)
}
cat("\nSYNTHETIC PROVENANCE TEST PASSED\n")
