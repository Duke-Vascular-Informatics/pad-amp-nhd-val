# R/

Core R helpers for the pad-amp-ed-desc descriptive Strategus study. Not a
package — sourced directly by `StrategusCodeToRun.R` / the portable bundle's
`run_analysis.R`.

## Files

| File | Purpose | Sourced by |
|------|---------|------------|
| `connection.R` | Builds DatabaseConnector connection details for SQL Server (JDBC); retry helpers for transient DB errors | `report_descriptive.R` (live CDM metadata queries) |
| `drivers.R` | Downloads and stages the Microsoft JDBC 13.2.1 driver bundle into `drivers/` on first run | `connection.R` |
| `db_maintenance.R` | SQL Server maintenance utilities (transaction log / tempdb pre-grow) | Manual, if ever bulk-loading this repo's own schema |
| `report_extended.R` | Entry point for the Word manuscript report — sources `report_helpers.R` + `report_descriptive.R` and calls `.report_descriptive()`. This study is permanently `study_design = "descriptive"`; the generic synthea-omop-template's prognostic/causal dispatch and templates were removed in the Strategus conversion. | `StrategusCodeToRun.R`, bundle `run_analysis.R` |
| `report_helpers.R` | Shared Word-document primitives (headings, captions, flextable theme, small-cell suppression) used by `report_descriptive.R` | `report_extended.R` |
| `report_descriptive.R` | The study's Word report: reconfigures CohortIncidence (Table 2a) and Characterization (Table S1b) module outputs plus the custom step's CSVs (Table 1, 2b, VA-FI per-deficit ORs, crosswalks) into the manuscript | `report_extended.R` |

Cohort instantiation (Strategus CohortGenerator), phenotype QA
(CohortDiagnostics), and the standardized baseline characterization
(Characterization) are NOT in this directory — they run via
`CreateStrategusAnalysisSpecification.R` / `inst/`, orchestrated by
`Strategus::execute()` in `StrategusCodeToRun.R`.
