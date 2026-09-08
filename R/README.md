# R/

Core R helpers for the pad-amp-nhd-val prognostic Strategus study. Not a
package — sourced directly by `StrategusCodeToRun.R`.

This README previously described `pad-amp-ed-desc`'s descriptive-study report
(`report_descriptive.R`, CohortIncidence Table 2a, VA-FI per-deficit ORs) —
copied over unmodified when this repo was scaffolded and never corrected to
match this study's own design (prognostic_model, integer risk scores, no
`report_descriptive.R` anywhere in this repo). Rewritten 2026-08-11 to
describe what is actually here.

## Files

| File | Purpose | Sourced by |
|------|---------|------------|
| `connection.R` | Builds DatabaseConnector connection details for SQL Server (JDBC); retry helpers for transient DB errors | `StrategusCodeToRun.R`, `risk_score_pipeline.R` |
| `drivers.R` | Downloads and stages the Microsoft JDBC 13.2.1 driver bundle into `drivers/` on first run | `connection.R` |
| `db_maintenance.R` | SQL Server maintenance utilities (transaction log / tempdb pre-grow) | Manual, if ever bulk-loading this repo's own schema |
| `generate_nhd_cohort.R` | Repairs the non-home-discharge outcome cohort after `Strategus::execute()` — circe cannot express a discharge-disposition criterion, so Strategus writes a placeholder ("every inpatient visit") that this overwrites with the real hand-authored SQL | `StrategusCodeToRun.R`, before the scoring step |
| `risk_score_pipeline.R` | Applies the three published integer risk scores (Iannuzzi 2020, Subramaniam mFI-5, Kraiss sVQI-FS) to the Strategus-generated cohort; also produces a per-model diagnostic calibration plot via `omopReportToolkit` | `scripts/analysis/integer_score_validation.R` |
| `cohort_demographics.R` | `fetch_subgroup_labels()` / `fetch_proc_type_labels()` — DB-touching label lookups for the subgroup bias analysis | `risk_score_pipeline.R` (independently, with a skip-if-missing guard — NOT via the report, which never used this file despite an old comment elsewhere claiming otherwise) |
| `extract_report_inputs.R` | The Phase 0 extract layer (`docs/MIGRATION_PLAN_REPO_SPLIT.md`): every remaining live CDM query, writing CSV artifacts to `output/report_inputs/` for [`pad-amp-nhd-val-report`](https://github.com/Duke-Vascular-Informatics/pad-amp-nhd-val-report) to consume. Also writes the PHI edge-case export (`export_edge_cases()`) — reserved for `duke-prcc-deploy` long-term, embedded here only because that repo doesn't exist yet. | `StrategusCodeToRun.R` |

## What is deliberately NOT here

The Word manuscript report (`report_prognostic.R`, `report_extended.R`,
`report_helpers.R`) moved to `pad-amp-nhd-val-report` 2026-08-11 (Phase 1 of
the migration plan), so that this repo can be Strategus-faithful and never
import `ggplot2`, `officer`, or `flextable` for the purpose of building a Word
document. `risk_score_pipeline.R`'s own diagnostic calibration PNG is the one
remaining plotting output in this repo — a QC artifact of the scoring step,
produced even if the report is never generated, not part of the manuscript.

Cohort instantiation (Strategus CohortGenerator), phenotype QA
(CohortDiagnostics), and the standardized baseline characterization
(Characterization) are NOT in this directory either — they run via
`CreateStrategusAnalysisSpecification.R` / `inst/`, orchestrated by
`Strategus::execute()` in `StrategusCodeToRun.R`.
