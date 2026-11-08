# Changelog

Dated record of changes to the analysis, moved here from `CLAUDE.md` so the
rules there can state only what is true now and why. Each entry names the rule
it produced, which is stated in `CLAUDE.md`.

## v1.0.0 (2026-11-06)

- Frozen for the manuscript. Tag `v1.0.0` points at the release commit.
- mFI-5 hypertension requires an outpatient antihypertensive exposure as well as the diagnosis. Cohort 9100032 (ATLAS 1798678) AND cohort 9100022. Run at PRCC 2026-11-06. See `CLAUDE.md`, "mFI-5 hypertension requires medication".

## 2026-11-05

- Insulin item redefined as "who with diabetes is currently on insulin". See `CLAUDE.md`, "Insulin item".
- Score-item lookback windows set per item. See `CLAUDE.md`, "Score-item windows".
- One diabetes definition used everywhere (cohort 9100021).
- Wound set extended to 29 roots, shared by the target and Table 1 (9100029).
- Table 1 phenotype rows read from ATLAS cohorts (9100003, 9100021, 9100029 to 9100031).
- The 2026-11-05 PRCC run was stopped by reading attrition mode 0 (events) rather than mode 1 (people). The freshness check now compares people. See `CLAUDE.md`, "Cohort-freshness check".

## 2026-10-15

- `ambu_deficit` and `nonambulatory` were one concept set under two names. They are now separate cohorts: strict aid use (9100027) and broad impaired ambulation (9100028).
- The PRCC run blamed 293 exclusions on the study period instead of 31 + 262, because a target cohort was reused after an in-place edit. Cohort freshness is now automatic. See `CLAUDE.md`, "Cohort regeneration is automatic".

## 2026-10-11

- Pinned to the physical CDM `omop_synth_pad_amp_v3` (was v2).

## 2026-10-05

- The four VA-FI cohorts (1797949 to 1797952) were dropped, because a `Limit: First` cohort cannot bind a lookback window. See `CLAUDE.md`, "Never reuse a Limit: First cohort".

## 2026-10-02

- Repository made public ahead of the manuscript.

## 2026-09-28

- First real-data run at PRCC. The attrition figures in `docs/PROTOCOL.md` come from this run. An index-date range rule was added after it. See `docs/PROTOCOL.md`.

## 2026-09-07

- Forked from `pad-amp-nhd-prog` as a frozen, validation-only copy. Cohort definitions were byte-identical at the fork.

## 2026-08-13

- Reserved local cohort-id block widened. The allocation ledger is in `strategus-study-template/docs/STRATEGUS_CONVENTIONS.md` section 6.

## 2026-08-12

- Output folders were committed by mistake during debugging and later untracked (see the `.gitignore` comment on `output*/`). The git history still contains them.

## 2026-08-11

- Report generation moved to `pad-amp-nhd-val-report`. Step 9 (PRCC bundle) moved to `duke-prcc-deploy`. The out-of-date bundle script was removed. The synthetic-data pin was found out of sync with the built overlay (see `CLAUDE.md`, "Data source").

## 2026-08-03

- Development-run result files and an edge-case export were written into `output_pre_v2fix_backup/`. They were later untracked (see 2026-08-12), but remain in git history. Whether they came from the synthetic dataset is being confirmed.
