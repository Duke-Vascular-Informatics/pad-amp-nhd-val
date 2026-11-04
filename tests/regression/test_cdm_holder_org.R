#!/usr/bin/env Rscript
# =============================================================================
# tests/regression/test_cdm_holder_org.R
#
# Guards .cdm_holder_org() (R/extract_report_inputs.R): the cdm_source.cdm_holder text is
# reduced to the organisation before it is written to the export, so a data steward's name
# and email never leave the secure environment. No database needed.
#
# USAGE   Rscript tests/regression/test_cdm_holder_org.R     (from the study folder)
# =============================================================================
suppressMessages(source("R/extract_report_inputs.R"))

failures <- character()
check <- function(label, got, want) {
  if (identical(got, want)) cat("  ok  :", label, "\n")
  else { failures <<- c(failures, label); cat("  FAIL:", label, "| got:", deparse(got), "| want:", deparse(want), "\n") }
}

# The realistic shape: organisation - person - email.
check("organisation - person - email -> organisation",
      .cdm_holder_org("Duke Health - Jane Doe - jane.doe@example.edu"), "Duke Health")
# En and em dash separators.
check("en dash separator", .cdm_holder_org("Duke Health – Jane Doe – jane.doe@example.edu"), "Duke Health")
check("em dash separator", .cdm_holder_org("Duke Health — Jane Doe"), "Duke Health")
# No separator: kept whole, but an email-like token is dropped.
check("no separator is kept", .cdm_holder_org("Duke Health"), "Duke Health")
check("bare email token is dropped", .cdm_holder_org("Duke Health jane.doe@example.edu"), "Duke Health")
# A hyphen inside a name is NOT a separator (no surrounding spaces).
check("hyphenated name is not split", .cdm_holder_org("Winston-Salem Health - Jane Doe"), "Winston-Salem Health")
# Vectorised, NA-safe.
check("vector with NA", .cdm_holder_org(c("A - x - y@z.org", NA)), c("A", NA_character_))
# Nothing resembling an email survives.
out <- .cdm_holder_org("Duke Health - Jane Doe - jane.doe@example.edu")
check("no '@' in output", grepl("@", out, fixed = TRUE), FALSE)

if (length(failures)) { cat("\nCDM HOLDER TEST FAILED (", length(failures), ")\n", sep = ""); quit(status = 1) }
cat("\nCDM HOLDER TEST PASSED\n")
