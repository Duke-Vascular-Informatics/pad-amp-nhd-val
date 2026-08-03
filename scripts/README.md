# scripts/

Standalone utility scripts for the pad-amp-ed-desc descriptive Strategus
study. Not part of a numbered workflow — run manually as needed.

## Top-level files

| File | Description | Usage |
|------|-------------|-----------|
| `concept_lookup.R` | OMOP vocabulary lookup. Queries `omop_vocab` for standard concept IDs matching a clinical term, with synonym fallback and descendant expansion. Labels results `[vocab query]`. Equivalent to the `/concept-lookup` Claude skill. | `Rscript scripts/concept_lookup.R "<term>" [domain]` |
| `create_support_bundle.R` | Creates a redacted troubleshooting bundle in `output/support/` including a setup report, git diagnostics, and recent logs. | `Rscript scripts/create_support_bundle.R` |

## Subdirectories

| Directory | Purpose |
|-----------|---------|
| `analysis/` | `descriptive_ed_analysis.R` — the retained custom analysis step (curated Table 1, ED-diagnosis categorization, VA-FI per-deficit prevalence + odds ratios, CPT4/ICD-10-CM crosswalks). Called from `StrategusCodeToRun.R` / the bundle's `run_analysis.R` after `Strategus::execute()`. |
| `hooks/` | Local git-hook installer and hook documentation for analyst guardrails |

Cohort generation, ETL, and vocabulary loading live outside this repo now:
the synthetic CDM is built by the companion `pad-amp-dispo-synth` repo, and
cohort instantiation is owned by Strategus (`inst/`,
`CreateStrategusAnalysisSpecification.R`).
