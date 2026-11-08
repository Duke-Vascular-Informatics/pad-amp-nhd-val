# workflow/

Step 9 (Duke PRCC portable bundle) moved to
[`duke-prcc-deploy`](https://gitlab.dhe.duke.edu/apj20/duke-prcc-deploy) (the "site-deploy" bucket of charon's [Multi-Repo Analysis Pipeline](https://github.com/Duke-Vascular-Informatics/charon#multi-repo-analysis-pipeline); Duke-internal, not public)
(`studies/pad-amp-nhd-val/`) on 2026-08-11 — Duke GitLab only, VPN-gated, not
mirrored to GitHub. Why it lives there: the bundle contains site-specific
configuration and an export that must not leave Duke, so it stays with the
Duke deployment.

Status: no PRCC bundle exists yet. The bundle script copied from
`pad-amp-ed-desc` was removed because it used the wrong study name and paths
and would have failed on its first run (see `CHANGELOG.md`, 2026-08-11). See
`duke-prcc-deploy/studies/pad-amp-nhd-val/README.md` for what still needs to be
built.

Steps 1-2 (`CreateStrategusAnalysisSpecification.R`, `StrategusCodeToRun.R`)
stay at this repo's root — see the repo's own `CLAUDE.md`.
