# Protocol: External Validation of Three Published Integer Risk Scores for Non-Home Discharge After Major Lower Extremity Amputation

**Study repository:** `pad-amp-nhd-val`
**Report repository:** `pad-amp-nhd-val-report`
**OSF project:** `hwgbe` (private)
**Status:** DRAFT v0.2 — assembled from the finalized, already-executed analysis; IRB confirmed, not yet otherwise reviewed or approved by the study team
**Draft date:** 2026-10-01
**Prepared by:** Adam Johnson, Duke Vascular Informatics
**Funding:** NIH National Center for Advancing Translational Sciences, Award K12TR005435

> This protocol documents the study design as it was actually implemented and
> executed, not a prospective plan awaiting data collection — the validation
> cohort has been assembled against Duke's real OMOP CDM instance (ACE_DATA)
> and the full analysis described below has been run to completion (n = 587,
> see Section 5). It is drafted retrospectively, alongside manuscript
> preparation, for methodological transparency and OSF registration. The
> one open item from the initial draft (Section 11's IRB Pro-number) was
> confirmed by the study team 2026-10-01 — nothing below should otherwise
> be treated as approved or complete pending full study-team review.

---

## 1. Background and Rationale

Non-home discharge (NHD) — discharge to a skilled nursing facility, inpatient
rehabilitation, long-term acute care, hospice, or other institutional
setting rather than home — is a common and consequential outcome after
major lower extremity amputation, with implications for recovery,
functional independence, and downstream healthcare utilization. Several
integer risk scores have been published for related outcomes in vascular
surgery populations, but none was derived specifically to predict NHD after
major amputation:

- **Iannuzzi et al. (2020)** — an NHD risk score derived in patients
  undergoing elective lower extremity bypass, not amputation.
- **Subramaniam et al. (2018)** — the modified Frailty Index-5 (mFI-5), a
  general frailty index derived to predict mortality, postoperative
  complications, and unplanned readmission, not discharge destination.
- **Kraiss et al. (2022)** — the simple VQI Frailty Score (sVQI-FS), derived
  to predict 9-month mortality in non-emergent vascular surgery cases.

This study evaluates whether any of these three existing instruments
transports to NHD prediction in a major-amputation population, as a
screening step and benchmark ahead of possible de novo model development
(pursued separately in the sibling repository `pad-amp-nhd-prog`, see
Section 11) — this is **not** an external validation of the scores against
the outcomes and populations for which they were originally derived.

---

## 2. Objectives

**Primary objective:** Externally evaluate the discrimination (AUROC,
AUPRC) and calibration (Brier score, expected calibration error,
calibration intercept/slope) of the Iannuzzi 2020, Subramaniam mFI-5, and
Kraiss 2022 sVQI-FS integer risk scores for predicting non-home discharge
after major lower extremity amputation, using a temporally recalibrated
logistic specification for each score alongside its published mapping
(Iannuzzi only — no published NHD probability mapping exists for the other
two).

**Secondary objective:** Assess clinical utility via decision curve
analysis, comparing each score's recalibrated specification against
treat-all/treat-none reference strategies across a range of threshold
probabilities.

**Tertiary objective:** Assess differential performance (calibration and
discrimination) of the best-performing score across prespecified
demographic and clinical subgroups, to identify populations where the
score may be unreliable.

---

## 3. Study Design

Retrospective, single-site external validation cohort study conducted on
the Duke OMOP CDM v5.4 instance (ACE_DATA), implemented as an OHDSI
Strategus (v1.5.0) study package. This repository is a frozen,
validation-only fork of `pad-amp-nhd-prog` (forked 2026-09-07, full git
history preserved), created specifically to keep this analysis stable for
publication while `pad-amp-nhd-prog` continues independently toward a
de novo risk score (see Section 11).

---

## 4. Data Source

- **CDM:** Duke OMOP CDM v5.4 (ACE_DATA), SQL Server. CDM version 5.4.0;
  vocabulary release v5.0 27-FEB-26 (Supplemental Table S1 of the
  manuscript report).
- **Study window:** 2017-01-01 to 2025-12-31 (`study_params.yaml`). This
  bound is enforced by `R/restrict_to_study_period.R`, a post-execute step
  that restricts the materialized target cohort to index dates within this
  range — added 2026-09-28 after a real PRCC run showed index events
  outside the intended window (Duke's live CDM continues to accrue
  encounters past any date fixed at protocol-writing time).
- **Development and pipeline verification:** conducted first against the
  synthetic dataset `omop_synth_pad_amp_v2` (registered in
  the workspace's `synthetic_data/registry.yaml` (format: [charon's synthetic_data README](https://github.com/Duke-Vascular-Informatics/charon/blob/main/synthetic_data/README.md)), id `pad_amp`) before any run against
  real patient data.

---

## 5. Study Population

**Target cohort** (OMOP cohort id `9100011`, `[DVI] Major Lower Extremity
Amputation (dysvascular, trauma/cancer excluded)`): adults aged 18 years or
older undergoing an inpatient major lower-extremity amputation, with
peripheral arterial disease, diabetes mellitus, or a lower-extremity wound
documented on or before the index date, and at least 1 day of prior
observation. Traumatic, burn, and oncologic amputations are excluded by the
cohort's circe inclusion rules. Patients admitted from another hospital or
a skilled nursing facility are additionally excluded by a post-execute step
(`R/exclude_facility_admissions.R`), restricting the cohort to patients
admitted from home (including elective admissions coordinated through an
outpatient clinic referral) — circe cannot express an admission-source
criterion directly (no `admitted_from_concept_id`/`admitted_from_source_value`
attribute on the `VisitOccurrence` criterion), so this is a row-level
narrowing of an already-correctly-generated cohort table, not a circe
definition change.

**Cumulative attrition** (final PRCC run, 2026-09-28):

| Stage | n remaining | Excluded at this stage |
|---|---|---|
| Major LE amputation, entry criteria met | 1,034 | — |
| Age ≥ 18 years | 1,013 | 21 (age < 18 years) |
| PAD, diabetes, or wound diagnosis | 960 | 53 (none of the above) |
| No limb-trauma mechanism during the index admission | 911 | 49 (traumatic mechanism) |
| No lower-limb malignancy during the index admission | 880 | 31 (lower-limb malignancy) |
| Index amputation within the study period (2017-01-01 to 2025-12-31) | 849 | 31 (outside the study period) |
| Admitted from home | **587** | 262 (admitted from hospital transfer or skilled nursing facility) |

**Final validation cohort: n = 587 patients.**

**Outcome cohort** (OMOP cohort id `9100001`, hand-authored SQL — circe
cannot express a discharge-disposition criterion): non-home discharge (NHD)
at the end of the index hospitalization, ascertained from
`visit_occurrence.discharged_to_concept_id`/`discharged_to_source_value`
and classified as non-home for any unambiguous institutional destination
(skilled nursing facility, inpatient rehabilitation, long-term acute care,
hospice, or other institutional care). NHD is a complete-case
classification: every target-cohort patient is retained, and a discharge is
classified as not-NHD by default whenever the destination cannot be
confirmed non-home (a conservative choice that may under-count true NHD
events; see Section 10). Hospice discharges are classified as a single
non-home category regardless of setting (home vs. facility). **Observed
NHD rate: 344/587 = 58.6%.**

---

## 6. Candidate Predictors — Three Published Integer Risk Scores

| Score | Range | Published probability mapping? |
|---|---|---|
| Iannuzzi 2020 NHD score | 0–18 points | Yes (score-to-risk lookup table) |
| Subramaniam 2018 mFI-5 | 0–5 points | No |
| Kraiss 2022 sVQI-FS | 0–10 points as implemented | No |

Every component is ascertained from clinical documentation on or before the
index date over a **365-day lookback window** (index date excluded), unless
the score's own publication defines the item differently: the mFI-5
congestive-heart-failure item and the pneumonia arm of its "COPD or current
pneumonia" item use 30 days, and the Iannuzzi tissue-loss item includes the
index date. The mFI-5 "COPD or current pneumonia" item is positive when
either COPD (365 days) or pneumonia (30 days) is recorded. A patient meeting a
component's evidence threshold receives its full published point value,
and an absent record is treated as zero evidence (two exceptions — Iannuzzi
anemia and sVQI-FS underweight/anemia — treat an absent measurement as
unknown rather than a true negative; see each table's own footnote in the
manuscript report). Component definitions, point values, and observed
activation rates are reported in the manuscript's Tables 3a–c; the full
ATLAS concept-set/cohort inventory is in Supplemental Table S2.

**Known deviations from each score's original publication** (fully
addressed in the manuscript's Limitations section, not re-derived here):

- Iannuzzi 2020's published four-level sex/race interaction is implemented
  as two additive binary components (female +1, non-White +2), which
  reproduces the published point totals exactly.
- sVQI-FS's eleventh published item (non-home residence) is omitted as
  near-circular with the outcome; the equally weighted form is evaluated
  rather than the authors' differentially weighted variant, which requires
  a procedure-specific risk term whose categories do not include major
  amputation.
- The source publications' own missing-data suppression rules (e.g.
  sVQI-FS's rule suppressing the score when fewer than five frailty
  domains have data) are not applied, since ascertainment here is
  presence/absence of coded records rather than an explicit missing state
  with its own flag.

---

## 7. Implementation and Code Deployment

Implemented as an OHDSI Strategus (v1.5.0) study package: cohort
construction, cohort diagnostics, and baseline characterization run through
standard HADES modules (`CohortGenerator`, `CohortDiagnostics`,
`Characterization`) from a single declarative specification
(`inst/padAmpNhdValAnalysisSpecification.json`), so the same code executes
unmodified at any OMOP CDM v5 site with no site-specific SQL editing,
credentials, or institution-specific identifiers embedded. Predictor
ascertainment is cohort-based: every score component resolves to a named
`[DVI]` cohort definition authored from the score's own concept set (all
`Limit: All`, so a lookback window binds); one cohort may back several
score items, with the window held in `covariates/cohort_map.csv`.

Three elements fall outside what Strategus/circe can express and are
implemented as documented extensions:

1. The non-home discharge outcome (circe has no discharge-disposition
   criterion) — `R/generate_nhd_cohort.R`.
2. Application of the three published scoring rules (Strategus's
   prediction module fits new models rather than applying fixed published
   weights) — `R/risk_score_pipeline.R`, `scripts/analysis/integer_score_validation.R`.
3. Arithmetic on measurement values (BMI, laboratory unit normalization) —
   `R/risk_score_pipeline.R`.

To verify that cohort-based ascertainment measures the same thing as a
direct CDM query, both routes are retained and an automated regression test
(`tests/regression/test_cohort_vs_domain_covariates.R`) compares every
predictor's per-patient values between them and requires exact agreement,
person for person, including the pneumonia-or-COPD item.

---

## 8. Statistical Analysis Plan

### 8.1 Model specifications

For each score, two specifications are considered: the published mapping,
where one exists (Iannuzzi only, via a monotone isotonic fit pooling the
derivation and validation columns of the source publication's Table III),
and a temporal recalibration — a logistic regression of the score's total
integer value on the observed NHD outcome. Patients are sorted
chronologically by index amputation date and divided at the midpoint
(training set n = 293, through 2021-05-02; test set n = 294, after
2021-05-02); each recalibration is fitted on the training set only and
evaluated on the test set. The Iannuzzi published-lookup specification is
the exception — a fixed external mapping with no parameters estimated from
these data — and is evaluated over the full cohort. Because recalibration
is a monotone transform of a single predictor, AUROC/AUPRC do not differ
between a score's published/raw and recalibrated specifications.

### 8.2 Discrimination and calibration

Discrimination: AUROC and AUPRC. Calibration: Brier score, expected
calibration error (ECE — the probability-weighted mean absolute difference
between predicted and observed NHD rates across quantile-based bins),
calibration intercept, and calibration slope. All metrics reported with
95% bootstrap percentile confidence intervals (B = 500 resamples).

### 8.3 Risk tiers

Patients are stratified into two tiers by each recalibrated model's
predicted NHD risk: Low (<60%), High (>60%), with the observed NHD rate
within each tier reported as a direct assessment of clinical utility.

### 8.4 Decision curve analysis

Net benefit of using each recalibrated model to guide a binary
treat/do-not-treat decision, across threshold probabilities from 1% to
99%, on the temporal test partition, compared against treat-all and
treat-none reference strategies.

### 8.5 Subgroup analysis and bias assessment

Model calibration (ECE) and discrimination (AUROC) are assessed across
prespecified subgroups — biological sex, race, ethnicity, age group
(<65, 65–74, ≥75 years), amputation level (above-knee, below-knee, other),
and calendar year of the index procedure — using the same recalibrated
predicted probabilities and temporal test partition as the overall
analysis, with 95% CIs from 200 bootstrap resamples. Subgroup levels with
fewer than 10 observed NHD events are suppressed (small-cell privacy
rule); AUROC is additionally suppressed where the outcome is constant
within a subgroup. Reported for the best-discriminating score
(mFI-5, recalibrated: overall AUROC 0.603, overall ECE 0.08 on the current
run).

### 8.6 Small-cell suppression

Every count-bearing output (demographic tables, discharge-destination
breakdowns, subgroup results, CPT-code and admission-source supplemental
tables) is suppressed when a cell's count is below 5 (10 for subgroup
event counts, per Section 8.5) — a hard structural rule enforced both at
extraction (`.suppress_counts()`, `R/risk_score_pipeline.R`) and again as a
packaging-time guardrail in `duke-prcc-deploy` that refuses to export any
results archive containing an unsuppressed small cell.

---

## 9. Concept Set and Vocabulary Transparency

Every OMOP concept ID used in this study is recorded in
`phenotype_library/catalog.yaml` with `status: verified` and a
`vocab_query_date`, per this workspace's concept-ID transparency rule — no
concept ID in this protocol is a `[pretraining]`/unverified guess. The full
ATLAS concept-set/cohort inventory, with standard-concept logic and source
OMOP table(s) for every cohort/outcome/covariate definition, is reported in
the manuscript's Supplemental Table S2. Cohort ids `9100001`–`9100011` are
claimed in this workspace's reserved local block
([`strategus-study-template/docs/STRATEGUS_CONVENTIONS.md`](https://github.com/Duke-Vascular-Informatics/strategus-study-template/blob/main/docs/STRATEGUS_CONVENTIONS.md) §6), outside the
ATLAS-demo id range, and are shared with (byte-identical to, at fork time)
`pad-amp-nhd-prog`'s own cohort definitions. Cohorts `9100021`–`9100027`
(DM, HTN, HF, CAD, pneumonia, ADL-dependent functional status, ambulatory
status) were added on 2026-10-05 in this repository's own sub-range; the
dependent-functional-status and ambulatory-status cohorts replace `9100005`
and `9100006`, which are left unchanged in `pad-amp-nhd-prog`.

---

## 10. Known Limitations

- **NHD ascertainment is conservative by construction**: an unconfirmable
  discharge destination defaults to not-NHD, which may under-count true
  NHD events.
- **Hospice granularity**: home hospice and facility-based hospice are not
  distinguished (both classified as non-home).
- **Item definitions are operationalisations, not the source instruments'
  own coding**: the mFI-5 "dependent functional status" item is mapped to
  ADL-dependence findings, bed-ridden, confined-to-chair and severe frailty;
  the Iannuzzi/sVQI-FS ambulatory items are mapped to walking-aid, wheelchair,
  walker/frame/crutch, bed-ridden and unable-to-walk findings (gait
  descriptors are excluded). The mFI-5 pneumonia arm includes two
  non-infectious interstitial-pneumonia concepts. Observation concepts are
  sparsely recorded in routine EHR data, so both items likely under-count.
- **365-day lookback**: a uniform window is applied to every score item
  rather than each source paper's own look-back convention, except where the
  paper states one (above); chronic conditions recorded earlier than a year
  before surgery are not counted.
- **sVQI-FS deviations**: eleventh item omitted, equally weighted form
  evaluated instead of the authors' differentially weighted variant (see
  Section 6).
- **Transportability framing, not external validation**: none of the three
  scores was derived to predict NHD after major amputation specifically —
  see Section 1.

---

## 11. Regulatory and Ethical Considerations

- **IRB:** Duke IRB protocol **Pro00119168** (confirmed by the study team,
  2026-10-01) — the same blanket PAD-outcomes IRB cited by sibling studies
  `pad-ler-ldl-desc` and `pad-oler-ssi-prog`, confirming the "Data Driven
  Optimization of Outcomes in Patient with Peripheral Artery Disease" /
  "Optimization in PAD" application on file
  (`irbs-protocols/Duke-irb-pad-optimizaztion.pdf`, PI Adam Johnson,
  DUHS-Vascular, Application for Exemption from IRB Review — the raw
  application packet, attached to this study's OSF project for reference)
  is this study's approval.
- No PHI/PII is written to disk by this repository or its report
  counterpart; all analysis outputs are aggregate statistics only (Section
  8.6). The PHI-producing edge-case export (`edge_case_export.R`, MRN, age,
  procedure date, for clinical chart review) stays exclusively within
  Duke's PRCC environment and is never copied out, per
  `duke-prcc-deploy`'s own design (bucket 4, no GitHub mirror).

---

## 12. Relationship to Other Repositories

| Repo | Relationship |
|---|---|
| `pad-amp-dispo-synth` | Produces the synthetic development/verification dataset (`omop_synth_pad_amp_v2`) |
| `pad-amp-nhd-prog` | Parent study; this repo is a frozen, validation-only fork (forked 2026-09-07) made to keep this analysis stable for publication while `pad-amp-nhd-prog` continues toward a de novo risk score |
| `pad-amp-nhd-val-report` | Manuscript/report repo, rendering the Word document from this study's `report_inputs/*.csv` artifacts only — no database, no VPN, no credentials |
| `duke-prcc-deploy` (`studies/pad-amp-nhd-val/`) | Duke GitLab-only PRCC deployment bundle; mirrors this repo's analysis code via a synced manifest plus a hand-maintained PRCC runner, checked for drift by an automated registry (`common/check_bundle_drift.sh`) |

---

## 13. Reproducibility and Data Management

- All cohort and analysis code is version-controlled (git) and
  Strategus-based; cohort definitions are not hand-edited except the
  single documented escape hatch for the NHD outcome cohort (Section 7,
  item 1), which carries a sentinel guard against accidental regeneration.
- Development and pipeline verification are conducted against a synthetic
  OMOP CDM before any run against real patient data (Section 4).
- This protocol is registered on OSF (private project `hwgbe`)
  retrospectively, alongside manuscript preparation — see the status note
  at the top of this document for why that differs from a prospective
  pre-registration.

---

## 14. Status and Next Steps

This is a first draft, assembled from the finalized manuscript report
(`pad-amp-nhd-val-report`'s rendered Methods/Results sections, confirmed
current as of the 2026-09-28 PRCC run) and this repository's own
documentation (`CLAUDE.md`, `study_params.yaml`, `inst/Cohorts.csv`,
`phenotype_library/catalog.yaml`). It is **not** ready for external
release. Section 11's IRB Pro-number (Pro00119168) was confirmed by the
study team 2026-10-01; everything in this protocol describes the study as
actually executed and should not require further revision absent a
study-team correction. Once the study team has reviewed this document in
full, it can be flagged as ready for review; the study team can then
decide on OSF visibility via the manual steps described in `osf/README.md`
(making the project public, minting a DOI) — none of which are automated
as part of drafting or uploading this document.

---

## Version History

| Version | Date | Change |
|---|---|---|
| v0.1 | 2026-10-01 | Initial draft, assembled from the finalized manuscript report and repository documentation |
| v0.2 | 2026-10-01 | IRB confirmed as Pro00119168 (blanket PAD-outcomes IRB shared with `pad-ler-ldl-desc`/`pad-oler-ssi-prog`); Section 11 and status header updated |
