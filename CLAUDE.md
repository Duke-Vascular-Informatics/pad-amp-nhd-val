# pad-amp-nhd-prog — CLAUDE Instructions (Local Wrapper)

Shared baseline (applies first):

- `../CLAUDE.md`

## Local Overrides

- **Study design: prognostic model, Strategus-based.** External validation of three
  published integer risk scores against **non-home discharge (NHD)** after major lower
  extremity amputation, plus a scaffold for de-novo model development.
  - Iannuzzi 2020 NHD score — 0–18 points, has a published score→risk lookup
  - Subramaniam 2018 mFI-5 — 0–5 points, no published lookup
  - Kraiss 2022 sVQI-FS — 0–10 points as implemented (10 of the paper's 11 items)
- Layout follows `pad-amp-ed-desc` (Strategus), **not** the `synthea-omop-template`
  scaffold: root-level spec builder + runner, `inst/` for cohorts, `R/` is `source()`d
  rather than installed. There is no `workflow/01–08` and no `synthea/`.
- Repo is **private**.

### Pipeline

| Step | File | What it does |
|------|------|--------------|
| 1 | `CreateStrategusAnalysisSpecification.R` | Builds `inst/padAmpNhdProgAnalysisSpecification.json`. Re-run after any change to `inst/`. |
| 2 | `StrategusCodeToRun.R` | `Strategus::execute()`, then the custom scoring step, then the Word report. Fresh R session required. |
| 9 | `workflow/09_build_portable_analysis_bundle.sh` | Duke GitLab deployment bundle. |

Supporting: `scripts/render_cohort_sql.R` (JSON → SQL, skips the hand-authored NHD
cohort), `tests/regression/test_cohort_vs_domain_covariates.R`.

### Data source

Physical CDM `omop_synth_pad_amp_dispo`, produced by **`pad-amp-dispo-synth`** and
registered in `../synthetic_data/registry.yaml` as `pad_amp_dispo`. Strategus itself
reads a view-overlay (`pad_amp_nhd_prog_cdm_test`) that unions the CDM with `omop_vocab`,
because `Strategus::createCdmExecutionSettings` has no vocabulary-schema parameter. The
runner builds that overlay automatically if it is missing. The custom step and report
query the **physical** schema directly.

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
  are one concept set under two names.
- **Never resolve score identity from a file path.** The inherited pipeline selected the
  anemia threshold with `grepl("vqifs", <path>)`, which also matched output folders. Use
  `config$score_id`, which is deliberately `NULL` by default so a forgotten assignment
  fails loudly.
- **The reused `[DVI]` VA-FI cohorts are `Limit: First`** — one row per person at their
  first-ever record — so lookback windows cannot bind and those four covariates are
  effectively *ever prior to index*. Its sharpest consequence: mFI-5's published 30-day
  CHF window collapses into the sVQI-FS's 10-year one, so the two scores no longer differ
  on that item. Quantified per covariate by the regression test. Every cohort authored
  here is `Limit: All` with `EndStrategy StartDate+0`.
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

`9100001–9100029` is a **local** block, deliberately outside the ATLAS-demo
`1796xxx–1797xxx` range so a future ATLAS assignment cannot collide. All are named
`[DVI] …` from the start so pushing them upstream is not a rename exercise. They are
**not yet in ATLAS** (`alignment_status: LOCAL ONLY` in `inst/Cohorts.csv`); creating them
there is a separate, explicit, user-initiated action — the workspace `[DVI]` tooling is
read-only and its one guarded writer handles concept sets, not cohorts.

### Version Control Routing

Independent repository inside the workspace folder; **not** a submodule, and not added to
the workspace root's index.

| Remote | URL | What to push |
|--------|-----|-------------|
| `origin` | `git@github.com:adam-mdmph/pad-amp-nhd-prog.git` | Full repository |

```bash
BRANCH=$(gh api user --jq .login)
git push origin "$BRANCH"   # then open a PR into main
```

Duke GitLab routing, if added, is handled by `workflow/09_build_portable_analysis_bundle.sh`
only. Never `git subtree push` or a bare `git push gitlab`.
