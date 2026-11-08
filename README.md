# pad-amp-nhd-val

External validation of three published integer risk scores against **non-home discharge
(NHD)** after major lower extremity amputation. The analysis is a Strategus study package
(CohortGenerator, CohortDiagnostics, Characterization) plus a custom scoring step. It contains
validation only; no new model is fitted.

| Score | Range | Published lookup |
|---|---|---|
| Iannuzzi 2020 NHD score | 0–18 | yes (isotonic fit over the published table) |
| Subramaniam 2018 mFI-5 | 0–5 | no |
| Kraiss 2022 sVQI-FS | 0–10 (10 of 11 items) | no |

## Quick start

This runs the study against a SQL Server database that holds an OMOP CDM. It does not need
the real data; the synthetic CDM described under [Data source](#data-source) is enough to
check the pipeline end to end.

### Prerequisites

- **R 4.5.2** (the version in `renv.lock`) and a **JDK 17**. Charon's dev container provides both; outside it, install them yourself.
- **Docker**, if you use charon's dev container. Its setup is in charon's [`docs/SETUP.md`](https://github.com/Duke-Vascular-Informatics/charon/blob/main/docs/SETUP.md).
- **The OMOP vocabulary** loaded in the `omop_vocab` schema, per charon's `docs/SETUP.md`.
- **The synthetic CDM** `omop_synth_pad_amp_v3` on the same SQL Server instance. Download and checksum it from the release linked under [Data source](#data-source).

### Steps

1. **Restore the R packages** from the lockfile:

   ```bash
   Rscript -e 'renv::restore(prompt = FALSE)'
   ```

   Expected: the command finishes with no error.

2. **Set the connection settings** in your shell. Do not put the password in a file in this repo.

   | Variable | Required | Default if unset | Meaning |
   |---|---|---|---|
   | `MSSQL_SA_PASSWORD` | yes | none | SQL Server password |
   | `OMOP_SERVER` (or `MSSQL_HOST`) | no | `mssql_dev` | SQL Server host |
   | `MSSQL_PORT` | no | `1433` | SQL Server port |
   | `MSSQL_USER` | no | `sa` | SQL Server login |
   | `OMOP_DATABASE` | no | `omop_synth` | Database that holds the CDM |
   | `OMOP_PHYSICAL_CDM_SCHEMA` | no | `omop_synth_pad_amp_v3` | Schema with the synthetic CDM |
   | `OMOP_VOCAB_SCHEMA` | no | `omop_vocab` | Schema with the vocabulary |

   The defaults are the dev container's values. Set them to your own. They are environment variables so that one script can run against different sites without editing the code.

3. **Build the study specification:**

   ```bash
   Rscript CreateStrategusAnalysisSpecification.R
   ```

   Expected: `inst/padAmpNhdValAnalysisSpecification.json` is written. Run this again after any change under `inst/`. The next step checks that it is current.

4. **Run the study** in a new R session:

   ```bash
   Rscript StrategusCodeToRun.R
   ```

   Expected: a line `Specification freshness check passed`, then Strategus generates the 15 cohorts, the diagnostics and the Table 1 characterization, and then the scoring step runs. Strategus output goes to `results/`. The scoring results and report inputs go to `output/`. Both folders are gitignored.

   If you run through charon's dev container, use the container's name from `docker ps`:

   ```bash
   docker exec <your-dev-container-name> bash -lc 'cd /workspace/pad-amp-nhd-val && Rscript StrategusCodeToRun.R'
   ```

5. **Check the covariate regression test:**

   ```bash
   Rscript tests/regression/test_cohort_vs_domain_covariates.R
   ```

   Expected: covariates whose meaning is the same by both routes match exactly. Per-covariate differences are written to `output/regression/cohort_vs_domain_deltas.csv`.

### Troubleshooting

| Symptom | Likely cause | Fix |
|---|---|---|
| `Analysis specification not found` | Step 3 has not been run | Run step 3 |
| `Rebuild first: Rscript CreateStrategusAnalysisSpecification.R` | A cohort definition file changed after the spec was built. The run stops because Strategus reads the spec, not the files, and would otherwise run the old definitions silently | Run step 3 again |
| `Overlay schema auto-build failed` | The physical CDM schema is missing, or your login cannot create a view | Confirm the schema name in `OMOP_PHYSICAL_CDM_SCHEMA` and the login's permissions; see [CLAUDE.md, Data source](CLAUDE.md) |
| Cohort counts do not change after you edit a cohort or schema | Strategus reused cohorts from an earlier run | See [CLAUDE.md, Data source](CLAUDE.md) on clearing incremental state |
| A schema name is rejected or misread | Schemas must be two-part `database.schema` | Set the variable to the form `database.schema` |

## What runs where

Strategus generates the cohorts and the Table 1 characterization. It does **not** score
the three instruments — applying a fixed set of published weights is not what
`PatientLevelPredictionModule` does (it fits models). Scoring runs as a custom step after
`Strategus::execute()`, in `scripts/analysis/integer_score_validation.R`, reading the
cohort table Strategus just populated.

## Data source

Physical CDM `omop_synth_pad_amp_v3`, generated by
[`pad-amp-synth`](https://github.com/Duke-Vascular-Informatics/pad-amp-synth) (version v3).
Synthetic only — no real patients. This is the data the code was **developed and tested**
against; the manuscript's results come from the real-data run, not from it.

- Generator, archived: Zenodo DOI [10.5281/zenodo.23955868](https://doi.org/10.5281/zenodo.23955868)
- Data: GitHub release [`synth-data-pad_amp-v3`](https://github.com/Duke-Vascular-Informatics/pad-amp-synth/releases/tag/synth-data-pad_amp-v3)
  (`pad_amp_clinical_tables.zip`, SHA-256 `9c402386…254f`, CC BY 4.0)
- Machine-readable record: `inst/synthetic_data_provenance.yaml`; every run logs it at start.

Strategus reads a view-overlay unioning that CDM with `omop_vocab`; the runner builds it
automatically if absent.

## Phenotypes

Every cohort is registered on the public OHDSI ATLAS demonstration instance
(<https://atlas-demo.ohdsi.org>) with a `[DVI]` name prefix, except the outcome. The library
column is the closest entry in the [OHDSI Phenotype Library](https://github.com/OHDSI/PhenotypeLibrary):
*aligned* means our concept set follows the library's; *deviates* means we kept our own scope
because the score item differs from the library phenotype (reasons in `inst/Cohorts.csv`); *none*
means no matching library entry. All library entries were pending peer review when compared
(2026-10-30 and 2026-11-04), and the comparison covers concept sets only.

| Phenotype | Used for | ATLAS ID | OHDSI Phenotype Library ID |
|---|---|---|---|
| Major lower extremity amputation (dysvascular; trauma and cancer excluded) | Target population | 1798563 | 999 (deviates) |
| Non-home discharge | Outcome | not in ATLAS (hand-written SQL) | none |
| Diabetes mellitus (any type) | mFI-5, sVQI-FS | 1798598 | 40 (deviates) |
| Hypertension (any hypertensive disorder) | mFI-5, sVQI-FS | 1798599 | 770 (deviates) |
| Heart failure (any etiology) | mFI-5, sVQI-FS | 1798600 | 934 (deviates) |
| Coronary artery disease | sVQI-FS | 1798601 | 197 (aligned, minus the all-vessel root) |
| COPD | mFI-5, sVQI-FS | 1798591 | 1271 (aligned) |
| Pneumonia (current) | mFI-5 | 1798602 | 1261 (deviates) |
| Peripheral arterial occlusive disease (lower-extremity PAD; same set as the target indication) | sVQI-FS | 1798592 | 371 (deviates) |
| Ischemic tissue loss | Iannuzzi | 1798593 | none |
| Dependent functional status (ADL dependence) | mFI-5 | 1798603 | none |
| Ambulatory aid use (strict) | Iannuzzi | 1798604 | none |
| Impaired ambulation (broad) | sVQI-FS | 1798605 | none |
| Insulin-treated diabetes | Iannuzzi | 1798594 | none |
| Anemia, hemoglobin below 10 g/dL | Iannuzzi | 1798595 | none (901 is diagnosis-based) |
| Anemia, sex-specific hemoglobin threshold | sVQI-FS | 1798596 | none (901 is diagnosis-based) |
| Renal impairment (creatinine above 1.8 mg/dL or dialysis) | sVQI-FS | 1798597 | 946 (deviates) |

The earlier shared target cohort (ATLAS 1797941) is also generated, for lineage only; no analysis uses it.
Age, sex, race and body mass index components are computed directly from the CDM and have no
cohort. Per-cohort rationale, concept sets and lookback windows are in `inst/Cohorts.csv` and
`inst/cohorts/*.json`.

## Regression test

```bash
Rscript tests/regression/test_cohort_vs_domain_covariates.R
```

Scores each instrument twice — once resolving covariates from the Strategus cohort table,
once by querying the CDM directly — and compares person-level counts. Covariates whose
semantics are identical across both paths must match **exactly** — since 2026-10-05 that is
every covariate, including mFI-5's COPD-or-pneumonia item. Per-covariate deltas are written to
`output/regression/cohort_vs_domain_deltas.csv`.

## Known limitations

Recorded at v1.0.0 (2026-11-06), against synthetic dataset v3. Recheck each point when the synthetic dataset or the cohort definitions change.

- On the current synthetic dataset the module generates NHD **unconditionally** of
  frailty, so discrimination sits near chance (AUROC ≈ 0.47–0.54) by construction. The
  pipeline is being exercised, not the clinical hypothesis.
- `age_ge80` activates in 0% — the generator's age guard plus exposure delay cap index age
  near 81, and it carries Iannuzzi's largest single weight (6 points). Fixable upstream by
  widening the exposure-onset delay.
- The de-novo model arm is scaffolded but **disabled** (`ENABLE_PLP_DEVELOPMENT <- FALSE`);
  predictors and approach are a study-team decision.

See `CLAUDE.md` for the full set of gotchas.

## Decisions needed

These are open questions for the study team, not settled facts:

- **Licence.** This repo is GPL-2.0-only. It was forked from `pad-amp-nhd-prog`, which is GPL-3.0. Shared files were copied from that repo, and GPL-2.0-only code cannot be combined with GPL-3.0 code. Duke's office for technology transfer should decide which licence applies.
- **Output folders.** The runner writes Strategus output to `results/`, but the scoring step, the report inputs and the docs use `output/`. Both are gitignored. Choose one name and update the code and docs together.
- **JDBC driver path.** `StrategusCodeToRun.R` sets the driver folder to a dev-container path (`/workspace/...`). Make it an environment variable before running outside that container.

## Citing

If you use this code, please cite it: see [`CITATION.cff`](CITATION.cff) (GitHub's
**Cite this repository** button reads it). Each release is archived on Zenodo, which
provides the DOI; the synthetic data it was developed with is cited separately (see Data source).

Developed in an institution-specific configuration of the
[Charon](https://github.com/Duke-Vascular-Informatics/charon) container
(*Containerized HADES Analytics for Research in OHDSI Networks*, v1.2.1,
DOI [10.5281/zenodo.23874907](https://doi.org/10.5281/zenodo.23874907)); please cite it too:
Johnson AP. Charon [software]. Zenodo; 2026.

## Funding

Research reported in this publication was supported by the National Center For Advancing
Translational Sciences of the National Institutes of Health under Award Number K12TR005435.
The content is solely the responsibility of the authors and does not necessarily represent
the official views of the National Institutes of Health.

## License

Copyright 2026 Duke University. All Rights Reserved. The software is hereby licensed under the GNU GPL License v2 (see [LICENSE](LICENSE)). This is copyleft: redistributed or modified versions must remain GPL v2-licensed. OMOP vocabulary files are subject to the
[Athena license terms](https://www.ohdsi.org/analytic-tools/athena-standardized-vocabularies/).

The synthetic development data (see Data source) is licensed separately, under **CC BY 4.0**; the GPL v2 applies to the code only.

### Third-party vocabulary content

A small number of concept sets in `inst/cohorts/*.json` include CPT4/HCPCS
procedure concepts (proprietary terminologies — CPT4 is AMA-owned) alongside
SNOMED CT. No vocabulary data is distributed with this repo, only concept
IDs/names referenced in cohort definitions.
