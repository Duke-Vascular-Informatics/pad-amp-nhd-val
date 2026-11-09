# pad-amp-nhd-val — CLAUDE Instructions (Local Wrapper)

Shared baseline (applies first):

- [charon's `CLAUDE.md`](https://github.com/Duke-Vascular-Informatics/charon/blob/main/CLAUDE.md)

## Local Overrides

- **Study design: prognostic model, Strategus-based.** External validation of three
  published integer risk scores against **non-home discharge (NHD)** after major lower
  extremity amputation.
  - Iannuzzi 2020 NHD score — 0–19 points (the paper observed 0–18; the published weights allow 19), has a published score→risk lookup
  - Subramaniam 2018 mFI-5 — 0–5 points, no published lookup
  - Kraiss 2022 sVQI-FS — 0–10 points as implemented (10 of the paper's 11 items)
- **This repo is a frozen, validation-only fork of `pad-amp-nhd-prog`** (forked
  2026-09-07, full git history preserved), made to keep this analysis stable for a
  publication. `pad-amp-nhd-prog` continues toward a de-novo risk score — this repo
  still carries its disabled `ENABLE_PLP_DEVELOPMENT` scaffold only because it was
  forked before that work started; de-novo development happens in `pad-amp-nhd-prog`,
  not here.
- Layout follows `pad-amp-ed-desc` (Strategus), **not** the `synthea-omop-template`
  scaffold: root-level spec builder + runner, `inst/` for cohorts, `R/` is `source()`d
  rather than installed. There is no `workflow/01–08` and no `synthea/`.
- **This repo builds no Word document and never should.** The manuscript report
  moved to [`pad-amp-nhd-val-report`](https://github.com/Duke-Vascular-Informatics/pad-amp-nhd-val-report)
  entirely (2026-08-11) — a separate repo, not just a separate file — so this one
  can stay Strategus-faithful: no `ggplot2`/`officer`/`flextable` imports for
  reporting purposes. If a change here seems to need one of those, it belongs in
  the report repo instead. The one exception is `R/risk_score_pipeline.R`'s own
  diagnostic calibration plot (via `omopReportToolkit`) — a QC artifact of the
  scoring step, independent of whether a report is ever generated from a run.
- **Development-data tag.** `inst/synthetic_data_provenance.yaml` records which synthetic dataset this code
  version was developed against (dataset id and version, generator commit, published release tag and SHA-256).
  `R/synthetic_provenance.R :: log_synthetic_provenance()` writes it to every run log, including the Duke bundle's
  (`run_analysis.R`). Update it in the same commit that repoints the study at another synthetic version, and run
  `tests/regression/test_synthetic_provenance.R`. It is development provenance only; no result comes from that data.
- Repo is **public** (made public 2026-10-02 ahead of publication; GPL-2.0, see `LICENSE`). Nothing patient-level or site-credentialed may ever be committed here.

### Pipeline

| Step | File | What it does |
|------|------|--------------|
| 1 | `CreateStrategusAnalysisSpecification.R` | Builds `inst/padAmpNhdValAnalysisSpecification.json`. Re-run after any change to `inst/`. |
| 2 | `StrategusCodeToRun.R` | `Strategus::execute()`, then the custom scoring step, then `R/extract_report_inputs.R`. Stops there — see below. Fresh R session required. |
| 9 | (separate repo) `duke-prcc-deploy/studies/pad-amp-nhd-val/` | Duke GitLab deployment bundle config + PHI-producing `edge_case_export.R`, moved out of this repo 2026-08-11 (see `workflow/README.md`). **Not yet deployable** — this study's PRCC runtime layer was never built; the in-repo script it replaced was an unmodified `pad-amp-ed-desc` copy that would have failed immediately. |
| — | (separate repo) `pad-amp-nhd-val-report/GenerateReport.R` | Manual step, run against this repo's `output/` directory (or a copied-out results export) to produce the Word manuscript. |

Supporting: `scripts/render_cohort_sql.R` (JSON → SQL, skips the hand-authored NHD
cohort), `tests/regression/test_cohort_vs_domain_covariates.R`.

### Data source

Physical CDM `omop_synth_pad_amp_v3`, produced by **`pad-amp-synth`** and
registered in the workspace's `synthetic_data/registry.yaml` (format: [charon's synthetic_data README](https://github.com/Duke-Vascular-Informatics/charon/blob/main/synthetic_data/README.md)) as `pad_amp` (this study is pinned to
version v3 as of 2026-10-11; it was v2 before). Strategus itself reads a view-overlay
(`pad_amp_nhd_val_cdm_test`) that unions the CDM with `omop_vocab`, because
`Strategus::createCdmExecutionSettings` has no vocabulary-schema parameter. The runner
builds that overlay automatically if it is missing. The custom step and report query
the **physical** schema directly, via `config$cdm_schema` — **this must match the
physical schema the overlay was actually built from.** Found out of sync 2026-08-11:
`study_params.yaml` had drifted to v1 (`omop_synth_pad_amp_dispo`, 1,474 persons) while
the already-built overlay pointed at v2 (1,544 persons) — a silent cross-schema
person_id join, not an error. Verify with the overlay's view definition
(`SELECT VIEW_DEFINITION FROM INFORMATION_SCHEMA.VIEWS WHERE TABLE_SCHEMA = '<overlay>'
AND TABLE_NAME = 'person'`) before ever changing either value. If a re-run shows
`Skipping cohorts already generated: <ids>` for cohorts other than 9100001 right after
changing which physical schema is pinned, the cohort table itself is stale from a run
against the old schema — Strategus's incremental mode does not know the underlying data
changed. Drop the `<results_schema>_*` incremental-tracking tables (the study's own
results schema only — `pad_amp_nhd_val`, `_checksum`, `_inclusion*`, `_censor_stats`,
`_summary_stats`, `_subset_attrition`, plus the `attrition_*`/`characterization_cohorts_*`/
`target_settings_*` Characterization tables) and clear the local gitignored `results/`
folder before re-running.

### Things that will bite you

- **`inst/sql/sql_server/9100001.sql` is hand-authored. Never regenerate it.** Non-home
  discharge depends on `visit_occurrence.discharged_to_concept_id`, and circe 1.11.3 has
  no such criterion attribute (verified by class inspection). Its companion JSON is a
  placeholder that entered as *all inpatient visits*, so a stray `CirceR::buildCohortQuery()`
  would silently redefine the outcome and still produce plausible metrics. Step 1 carries
  a sentinel guard; `render_cohort_sql.R` refuses to touch the file.
- **Cohorts are named by clinical definition, not by score item name.** The mapping from
  published item → cohort lives in `covariates/cohort_map.csv`, keyed by the *pair*
  `(score_id, score_item_id)`. Never key on `covariate_id` alone: `anemia` means two
  different thresholds, `chf` two different windows, and `ambu_deficit`/`nonambulatory`
  were one concept set under two names until 2026-10-15; they are now separate cohorts
  (strict aid use `9100027`, broad impaired ambulation `9100028`).
- **Cohort regeneration is automatic; you do not reset the incremental state after a cohort
  edit.** `R/cohort_freshness.R` runs around `Strategus::execute()`: it forces the two cohorts
  modified after generation (target `9100011`, outcome `9100001`) to regenerate every run,
  clears every generation record if the CDM release/vocabulary or study window changed since
  the last run, and lets CohortGenerator regenerate any cohort whose definition changed. Right
  after execute and before any post-step it verifies each cohort's live subject count equals
  the count CohortGenerator generated (and that every cohort in the spec was generated), then
  logs which cohorts were regenerated vs reused. Why: the target is edited in place by the
  study-period/facility steps, so a reused copy made them run twice and the CONSORT funnel
  blamed 293 exclusions on the study period instead of 31 + 262 (PRCC, 2026-10-15). The
  CONSORT extractor also refuses to write a funnel whose stages do not chain. Tested by
  `tests/regression/test_cohort_freshness.R`. `reset_incremental_state` remains for a fully
  clean slate (e.g. repointing the physical CDM schema).
- **Never resolve score identity from a file path.** The inherited pipeline selected the
  anemia threshold with `grepl("vqifs", <path>)`, which also matched output folders. Use
  `config$score_id`, which is deliberately `NULL` by default so a forgotten assignment
  fails loudly.
- **Score-item windows are chosen per item (changed 2026-11-05).** Chronic conditions (diabetes, hypertension,
  COPD, coronary artery disease, peripheral artery disease, insulin-treated diabetes) look back **any time up to and including the index date**, encoded as
  `-36500` days (100 years, safely inside the SQL DATE range) `..0` (diagnoses recorded at the index admission count; changed 2026-11-09 so Table 1's PAD and diabetes counts equal Table 3's); hemoglobin and BMI (anemia, underweight)
  use **90 days** so they describe the patient at surgery; mFI-5 CHF and the mFI-5 pneumonia arm are 30 days;
  everything else (functional status, ambulation, sVQI-FS CHF, which can resolve or improve, and renal impairment) is 365 days
  (`-365..0`). **Every window ends on day 0, so the index date is always included (2026-11-09)**; the windows differ only in how far back they reach. This is set in three places that must agree:
  `covariates/cohort_map.csv` (cohort path), `covariates/covariates*.csv` (domain-query oracle) and, for
  the pneumonia arm, a per-row `lookback_start_day`/`lookback_end_day` override in
  `covariate_concepts_mfi5.csv`. mFI-5 `copd` is the one item with TWO `cohort_map.csv` rows (COPD any time
  OR pneumonia 30 d); a pair may have several rows and any hit is positive. The regression test requires
  exact cohort-vs-domain agreement.
- **The sVQI-FS `pvd` item and the target's PAD indication are one phenotype** (cohort 9100003 uses the same
  concept set as 9100011's "Lower Extremity Peripheral Artery Occlusive Disease": four roots less ten
  non-lower-extremity exclusions). Keep them identical; the oracle rows in `covariate_concepts_vqifs.csv`
  and the cohort JSON are checked by the regression test's concept-set drift check.
- **Cohort-freshness check compares PEOPLE (mode_id 1), not events (mode_id 0)** in `cg_cohort_attrition.csv`. They coincide for a
  Limit=First cohort (the target) but not for a Limit=All cohort with an inclusion rule (9100007: 979,789 events vs 97,677 people), where
  reading mode 0 stopped the 2026-11-05 PRCC run. The check only covers cohorts that have attrition rows (those with inclusion rules).
- **Insulin item (2026-11-05): "who with diabetes is currently on insulin".** Positive = an outpatient insulin exposure in the
  the 365 days up to and including the index date AND a diabetes diagnosis on or before the index date. Outpatient = no inpatient visit (9201, 262, with
  descendants) spans the exposure date (start and end inclusive); exposures with no visit link count; ED-only visits are not
  excluded. Cohort path: cohort 9100007 AND cohort 9100021 via `and_cohort_id` in `cohort_map.csv`. Domain path:
  `query_insulin_covariate_counts()` with the `diagnosis_required` rows of `covariate_concepts.csv`. **Circe gotcha:** an
  inclusion rule that checks a visit's END date needs `EndWindow.UseEventEnd = true`; without it Circe silently tests the
  visit START date twice. `tests/regression/test_insulin_item.R` (16 hand-built patients: window edges, inpatient overlap,
  ER-to-inpatient, ED-only, no visit link, diabetes before/after index) is the only thing that exercises this; the synthetic
  dataset has no inpatient insulin and cannot.
- **mFI-5 hypertension requires medication (2026-11-06; run at PRCC 2026-11-06).** Subramaniam 2018 (NSQIP HYPERMED) scores
  hypertension only when it is treated. Positive = an outpatient antihypertensive exposure (the OHDSI PL 1046/1047/1049 ingredient sets: thiazide-like diuretics, dihydropyridine CCBs, beta blockers; plus ATC C09A-C09D for
  ACE inhibitors and ARBs; loop/K-sparing diuretics, non-dihydropyridine CCBs and C02 excluded as in the PL) in the 365 days up to and including the index date AND a hypertension diagnosis (cohort 9100022) on or before the index date. Cohort path:
  9100032 AND 9100022 via `and_cohort_id`; domain path: `query_outpatient_drug_with_dx_counts()` (the insulin oracle, generalised;
  it is selected by the `diagnosis_required` row). The sVQI-FS htn item is unchanged. 9100032 = ATLAS 1798678 (created 2026-11-06);
  its SQL is rendered from the JSON by `scripts/render_cohort_sql.R`.
- **One diabetes definition everywhere (2026-11-05):** cohort 9100021 = 201820 + 442793 with descendants (all types incl. type 1
  and 2, and diabetic complications), also the target's diabetes indication, Table 1, and the insulin item's requirement.
- **Wound set = 29 roots in BOTH the target's wound entry criterion and Table 1 (9100029)**: the original 24 plus five gangrene roots
  (4263116 generic/dry/wet/trophic/symmetrical, 44782426, 35615085, 4141477, 4142822), added 2026-11-05. Keep the two identical.
  `scripts/render_cohort_sql.R` now honors the `generate_stats` column (the target needs inclusion statistics for the CONSORT figure).
- **Table 1 phenotype rows are read from ATLAS cohorts (2026-11-05).** PAD = 9100003, diabetes (all types) = 9100021,
  lower-extremity wound = 9100029, above-knee = 9100030, below-knee = 9100031 (Other = the rest of the target cohort),
  via `.table1_indication_sql()` / `.table1_amp_level_sql()` in `R/extract_report_inputs.R`. 9100029-31 exist only for
  Table 1 (ATLAS 1798663-65). Indication = an entry on or before the index date (PAD and diabetes any time; the wound row only in the 365 days through the index date, the window of the Iannuzzi tissue-loss item, 2026-11-09); level = an entry on the index visit.
  PASS 3 of `tests/regression/test_cohort_vs_domain_covariates.R` recomputes these rows straight from the cohort JSON
  concept sets and requires an exact match. Age, sex, race and ethnicity stay direct person-table queries.
- **Never reuse a `Limit: First` cohort for a score item.** The four VA-FI cohorts
  (1797949–1797952) were dropped on 2026-10-05 for exactly this reason: one row per person
  at their first-ever record means a lookback window cannot bind, and the published 30-day
  mFI-5 CHF window silently became "ever prior". Every cohort authored here is `Limit: All`
  with `EndStrategy StartDate+0`.
- **9100005 / 9100006 are NOT defined in this repo any more.** They are shared with
  `pad-amp-nhd-prog`, so redefining them here would give one id two meanings; the revised
  dependent-functional-status, ambulatory aid-use and impaired-ambulation cohorts are `9100026` / `9100027` / `9100028`.
  Cohort JSON for `9100021–9100028` is generated by `scripts/author_covariate_cohorts.R`;
  edit that script, not the JSON.
- **Don't borrow VA-FI cohorts for score items without reading their concept sets.** VA-FI
  deficits are deliberately broad organ-system categories: its "Chronic lung disease"
  spans asthma/ILD/bronchiectasis and its "Peripheral vascular disease" spans aortic
  aneurysm and venous thrombosis. Correct for VA-FI, wrong for a COPD or PAD score item —
  hence cohorts 9100002 and 9100003.
- **`EndStrategy StartDate+0` on covariate cohorts is load-bearing.** FeatureExtraction's
  cohort-based covariates use interval-*overlap* semantics; the scoring step uses
  start-in-window. They coincide only for point events.
- Carried forward from `pad-amp-ed-desc`: `Characterization 3.0.1` emits raw `IFNULL()`
  and fails on SQL Server, so `includeRiskFactors` / `includeDechallengeRechallenge` /
  `includeCaseSeries` must stay `FALSE`. `runInclusionStatistics = FALSE` avoids a
  backslash-schema crash on Duke PRCC. Schemas must be two-part `database.schema`.

### Cohort id allocation

This repo holds **`9100001`–`9100011`** (minus `9100005`/`9100006`, no longer defined here) and its own **`9100021`–`9100028`** out of the reserved local block, which sits
deliberately outside the ATLAS-demo id range so a future ATLAS assignment cannot
collide. **Shared with `pad-amp-nhd-prog`** — this repo is a frozen fork of it and the
cohort definitions were byte-identical at fork time (2026-09-07), so the ledger
records the range once against `pad-amp-nhd-prog` rather than claiming a duplicate
range here. The block's bounds and the allocation ledger live in
[`strategus-study-template/docs/STRATEGUS_CONVENTIONS.md`](https://github.com/Duke-Vascular-Informatics/strategus-study-template/blob/main/docs/STRATEGUS_CONVENTIONS.md) §6 — do not restate them
here. (They used to be restated here and in every `logic_description` below, and
all of it went stale at once when the block was widened on 2026-08-13.) All are named
`[DVI] …` from the start so pushing them upstream is not a rename exercise. They are
**not yet in ATLAS** (`alignment_status: LOCAL ONLY` in `inst/Cohorts.csv`); creating them
there is a separate, explicit, user-initiated action — the workspace `[DVI]` tooling is
read-only and its one guarded writer handles concept sets, not cohorts.

### Version Control Routing

Independent repository inside the workspace folder; **not** a submodule, and not added to
the workspace root's index.

| Remote | URL | What to push |
|--------|-----|-------------|
| `origin` | `git@github.com:Duke-Vascular-Informatics/pad-amp-nhd-val.git` | Full repository |

```bash
BRANCH=$(gh api user --jq .login)
git push origin "$BRANCH"   # then open a PR into main
```

Duke GitLab deployment (Step 9) is handled entirely by the separate
[`duke-prcc-deploy`](https://gitlab.dhe.duke.edu/apj20/duke-prcc-deploy) repo's (the "site-deploy" bucket of charon's [Multi-Repo Analysis Pipeline](https://github.com/Duke-Vascular-Informatics/charon#multi-repo-analysis-pipeline); Duke-internal, not public)
`studies/pad-amp-nhd-val/` config — not by anything in this repo. Never `git subtree
push` or a bare `git push gitlab` from here.
