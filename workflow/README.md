# workflow/

Step 9 (Duke PRCC portable bundle) moved to
[`duke-prcc-deploy`](https://gitlab.dhe.duke.edu/apj20/duke-prcc-deploy)
(`studies/pad-amp-nhd-val/`) on 2026-08-11 — Duke GitLab only, VPN-gated, not
mirrored to GitHub. The file previously here
(`09_build_portable_analysis_bundle.sh`) was removed: it was an unmodified
copy of `pad-amp-ed-desc`'s bundle script (wrong `STUDY_NAME`, wrong paths,
referenced a pipeline this study doesn't have) and would have failed
immediately if run — this study has never actually had a working PRCC
bundle. See `duke-prcc-deploy/studies/pad-amp-nhd-val/README.md` for what
still needs to be built before one exists.

Steps 1-2 (`CreateStrategusAnalysisSpecification.R`, `StrategusCodeToRun.R`)
stay at this repo's root — see the repo's own `CLAUDE.md`.
