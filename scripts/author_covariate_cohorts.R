#!/usr/bin/env Rscript
# =============================================================================
# scripts/author_covariate_cohorts.R
#
# Purpose : Author the seven Limit=All covariate cohorts added on 2026-10-05
#           (ids 9100021-9100027) as circe cohort JSON under inst/cohorts/.
#           This repo is R-only, so the JSON is generated here from a single
#           declarative table rather than hand-edited.
# Inputs  : A live connection to the omop_vocab schema (concept metadata for the
#           circe `concept` objects), via the same connection settings
#           tests/regression/ uses. Run inside the devcontainer.
# Outputs : inst/cohorts/<id>.json for each cohort below. SQL is NOT written
#           here; run scripts/render_cohort_sql.R afterwards.
# Assumes : Concept ids below are [vocab query] verified (2026-10-05) against
#           this workspace's vocabulary build; this script re-reads each id's
#           name/domain/vocabulary from omop_vocab so the JSON cannot carry a
#           stale or mistyped label, and stops if an id is not a standard,
#           valid concept.
#
# WHY NEW IDS INSTEAD OF EDITING 9100005 / 9100006 IN PLACE
# ---------------------------------------------------------
# 9100001-9100011 are shared with pad-amp-nhd-prog (see CLAUDE.md "Cohort id
# allocation"). Redefining 9100005 (dependent functional status) and 9100006
# (ambulatory deficit) here would leave the same id meaning two different
# things in two repos. The revised definitions therefore get new ids in this
# repo's own block (9100021-9100049, ledger: STRATEGUS_CONVENTIONS.md 6.1), and
# 9100005 / 9100006 are dropped from this repo's manifest.
#
# WHY THE FOUR VA-FI COHORTS (1797949-1797952) ARE REPLACED
# ---------------------------------------------------------
# They are PrimaryCriteriaLimit = "First": one row per person at their first-ever
# record, so a lookback window cannot bind and the item is effectively "ever
# prior to index". The 2026-10-05 decision is a 365-day window on every score
# item (except where the published definition says otherwise), which requires
# Limit=All. DM / HTN / HF / CAD therefore get purpose-authored cohorts built
# from each score CSV's own concept set.
#
# EVERY COHORT BELOW
#   * PrimaryCriteriaLimit / QualifiedLimit / ExpressionLimit = "All"
#   * EndStrategy = DateOffset(StartDate, 0)  -- load-bearing: the scoring step
#     uses start-in-window while FeatureExtraction uses interval overlap; the two
#     coincide only when cohort_end_date == cohort_start_date.
#   * CollapseSettings ERA / 0
# =============================================================================

suppressMessages({
  library(jsonlite)
  library(DatabaseConnector)
})

Sys.setenv(DATABASECONNECTOR_JAR_FOLDER = "/workspace/synthea-omop-template/drivers/jdbc-runtime")

# -----------------------------------------------------------------------------
# Declarative cohort table.
#
# Each cohort has one or more concept sets; each concept set is bound to exactly
# one circe primary-criteria type (so a concept that lives in the Device or
# Observation domain is not silently missed by a Condition-only criterion --
# the failure that once made the ambulatory item activate in 0 of 225 patients).
#
# items: list(concept_id, include_descendants, is_excluded)
# -----------------------------------------------------------------------------
cohorts <- list(

  `9100021` = list(
    readme = c(
      "Diabetes mellitus (any type) - the mFI-5 dm item (1 point).",
      "Concept set is EXACTLY covariates/covariate_concepts_mfi5.csv :: dm (201820 + descendants).",
      "Replaces the reused Limit=First [DVI] VA-FI Diabetes cohort 1797952, which was also broader than the CSV (adds 4034964)."),
    sets = list(list(name = "[DVI] Diabetes mellitus (risk score item)", criterion = "ConditionOccurrence",
                     items = list(list(201820L, TRUE, FALSE))))  # [vocab query] SNOMED: Diabetes mellitus
  ),

  `9100022` = list(
    readme = c(
      "Hypertension - the mFI-5 and Kraiss sVQI-FS htn items (1 point each).",
      "Concept set is EXACTLY the score CSVs' htn concept (316866 + descendants).",
      "Replaces the reused Limit=First [DVI] VA-FI Hypertension cohort 1797951, which was broader than the CSV (adds hypertensive heart / renal disease)."),
    sets = list(list(name = "[DVI] Hypertension (risk score item)", criterion = "ConditionOccurrence",
                     items = list(list(316866L, TRUE, FALSE))))  # [vocab query] SNOMED: Hypertensive disorder
  ),

  `9100023` = list(
    readme = c(
      "Heart failure - the mFI-5 chf item (30-day window per the original publication) and the Kraiss sVQI-FS chf item (365-day window).",
      "One cohort backs both; the window lives in covariates/cohort_map.csv, which is the whole point of Limit=All.",
      "Concept set is EXACTLY the score CSVs' chf concept (316139 + descendants).",
      "Replaces the reused Limit=First [DVI] VA-FI Heart failure cohort 1797950, whose Limit=First made the published 30-day mFI-5 window collapse to 'ever prior'."),
    sets = list(list(name = "[DVI] Heart failure (risk score item)", criterion = "ConditionOccurrence",
                     items = list(list(316139L, TRUE, FALSE))))  # [vocab query] SNOMED: Heart failure
  ),

  `9100024` = list(
    readme = c(
      "Coronary artery disease - the Kraiss sVQI-FS cad item (1 point).",
      "Concept set is EXACTLY covariates/covariate_concepts_vqifs.csv :: cad (4185932 Ischemic heart disease + descendants).",
      "Replaces the reused Limit=First [DVI] VA-FI Coronary artery disease cohort 1797949."),
    sets = list(list(name = "[DVI] Coronary artery disease (risk score item)", criterion = "ConditionOccurrence",
                     items = list(list(4185932L, TRUE, FALSE))))  # [vocab query] SNOMED: Ischemic heart disease
  ),

  `9100025` = list(
    readme = c(
      "Pneumonia - the 'current pneumonia' arm of the Subramaniam mFI-5 'COPD or current pneumonia' item.",
      "Scored over a 30-day window (current pneumonia); the COPD arm (cohort 9100002) keeps 365 days. covariates/cohort_map.csv carries two rows for the pair (mfi5, copd) and the item is positive if EITHER arm hits.",
      "Concept set: 255848 Pneumonia + descendants (188 concepts, [vocab query] 2026-10-05).",
      "KNOWN, ACCEPTED LOOSENESS: the subtree includes two non-infectious interstitial concepts (4273378 Interstitial pneumonia, 1340380 Exacerbation of interstitial pneumonia) and congenital / neonatal pneumonia (irrelevant in an adult amputation cohort). Excluding the first two would put an isExcluded item in the cohort that the score CSV cannot express, breaking the cohort-vs-domain regression check; the effect on a 30-day window in this population is negligible."),
    sets = list(list(name = "[DVI] Pneumonia (mFI-5 current pneumonia)", criterion = "ConditionOccurrence",
                     items = list(list(255848L, TRUE, FALSE))))  # [vocab query] SNOMED: Pneumonia
  ),

  `9100026` = list(
    readme = c(
      "Dependent functional status - the Subramaniam mFI-5 fs_dep item (1 point): partial or total dependence for activities of daily living.",
      "REVISED 2026-10-05 (replaces 9100005, which is shared with pad-amp-nhd-prog). The old set (Frailty + Impaired mobility, both with descendants) measured MOBILITY and mild frailty, not ADL dependence, and overlapped the ambulatory item.",
      "New set = ADL-dependence findings (Dependent for bathing/dressing/feeding/grooming/hygiene/drinking/rising/sitting/standing/walking; Needs help with ...; Requires assistance with ...) + Bed-ridden + Confined to chair + Severe frailty (CFS 7: completely dependent for personal care). Mild / moderate frailty and the deprecated quality-measure concept are deliberately dropped. IADL concepts (housework, shopping, cooking, medications) are NOT included: the mFI-5 item is ADL dependence.",
      "TWO primary criteria because the set spans two OMOP domains: Severe frailty is Condition, the rest are Observation.",
      "Overlap with the ambulatory cohort 9100027 is deliberate and clinically meaningful: Dependent for walking, Needs help with walking, Bed-ridden and Confined to chair are both ADL dependence and an ambulatory deficit."),
    sets = list(
      list(name = "[DVI] Dependent Functional Status - Condition domain", criterion = "ConditionOccurrence",
           items = list(list(45770280L, FALSE, FALSE))),   # [vocab query] SNOMED: Severe frailty
      list(name = "[DVI] Dependent Functional Status - Observation domain", criterion = "Observation",
           items = list(
             list(4044722L,  FALSE, FALSE),  # [vocab query] SNOMED: Dependent for bathing
             list(4043532L,  FALSE, FALSE),  # [vocab query] SNOMED: Dependent for dressing
             list(4012662L,  FALSE, FALSE),  # [vocab query] SNOMED: Dependent for feeding
             list(36717538L, FALSE, FALSE),  # [vocab query] SNOMED: Dependent for drinking
             list(45767125L, FALSE, FALSE),  # [vocab query] SNOMED: Dependent for personal grooming
             list(36716238L, FALSE, FALSE),  # [vocab query] SNOMED: Dependent for personal hygiene activity
             list(36716241L, FALSE, FALSE),  # [vocab query] SNOMED: Dependent for rising to feet
             list(36716239L, FALSE, FALSE),  # [vocab query] SNOMED: Dependent for sitting
             list(37206158L, FALSE, FALSE),  # [vocab query] SNOMED: Dependent for sitting down
             list(36716240L, FALSE, FALSE),  # [vocab query] SNOMED: Dependent for standing
             list(4146424L,  FALSE, FALSE),  # [vocab query] SNOMED: Dependent for walking
             list(4043370L,  FALSE, FALSE),  # [vocab query] SNOMED: Needs help with dressing
             list(4012838L,  FALSE, FALSE),  # [vocab query] SNOMED: Needs help with feeding
             list(1450437L,  FALSE, FALSE),  # [vocab query] SNOMED: Needs help with personal grooming
             list(1450204L,  FALSE, FALSE),  # [vocab query] SNOMED: Needs help with walking
             list(1450205L,  FALSE, FALSE),  # [vocab query] SNOMED: Needs help with washing self
             list(1450206L,  FALSE, FALSE),  # [vocab query] SNOMED: Needs help with performing mouthcare activities
             list(44791682L, FALSE, FALSE),  # [vocab query] SNOMED: Needs assistance with shaving
             list(40481489L, FALSE, FALSE),  # [vocab query] SNOMED: Requires assistance with all daily activities
             list(35610195L, FALSE, FALSE),  # [vocab query] SNOMED: Requires assistance with personal care activities
             list(42537792L, FALSE, FALSE),  # [vocab query] SNOMED: Requires continuous supervision for activities of daily living
             list(4058155L,  FALSE, FALSE),  # [vocab query] SNOMED: Bed-ridden
             list(4058154L,  FALSE, FALSE)   # [vocab query] SNOMED: Confined to chair
           )))
  ),

  `9100027` = list(
    readme = c(
      "Ambulatory status - serves BOTH the Iannuzzi ambu_deficit item (3 points; use of an ambulatory aid) and the Kraiss sVQI-FS nonambulatory item (1 point; impaired preadmission ambulation). One cohort backs both; covariates/cohort_map.csv maps both items here.",
      "REVISED 2026-10-05 (replaces 9100006, which is shared with pad-amp-nhd-prog).",
      "DROPPED from the old set: 437643 Abnormal gait (94 descendants - 'Antalgic gait', 'Ataxic gait', 'Buttocks prominent when walking'... gait DESCRIPTORS, not an aid or a deficit in ambulation), 36714126 Difficulty walking (18 descendants of the same kind) and 4085915 Provision of long cane (a PROCEDURE - provision is not use).",
      "KEPT: 4012645 Walking aid use, 4044714 Using wheelchair, 439405 Walking disability, 4240470 Wheelchair (includeDescendants=true per the 2026-08-03 clinical decision; the subtree also contains parts/accessories, accepted because a parts record still implies wheelchair use).",
      "ADDED: walker / walking frame / crutch DEVICES (which the old set missed entirely), 'Does mobilize using walker / crutch', Bed-ridden, Confined to chair, Needs walking aid in home, Mobile outside with aid, Impaired wheelchair mobility, Unable to walk, Needs help with walking, Dependent for walking.",
      "Two primary criteria (Observation, Device) - the declarative replacement for the pipeline's domain='auto' union. HIGHEST-RISK translation in this repo: querying a single domain once returned 0 of 225 patients. The regression test compares this cohort against query_auto_domain_covariate_counts() person for person."),
    sets = list(
      list(name = "[DVI] Ambulatory Status - Observation domain", criterion = "Observation",
           items = list(
             list(4012645L, TRUE,  FALSE),  # [vocab query] SNOMED: Walking aid use
             list(4044714L, FALSE, FALSE),  # [vocab query] SNOMED: Using wheelchair
             list(439405L,  FALSE, FALSE),  # [vocab query] SNOMED: Walking disability
             list(4086548L, FALSE, FALSE),  # [vocab query] SNOMED: Unable to walk
             list(4058155L, FALSE, FALSE),  # [vocab query] SNOMED: Bed-ridden
             list(4058154L, FALSE, FALSE),  # [vocab query] SNOMED: Confined to chair
             list(4052477L, FALSE, FALSE),  # [vocab query] SNOMED: Needs walking aid in home
             list(4052045L, FALSE, FALSE),  # [vocab query] SNOMED: Mobile outside with aid
             list(4032531L, FALSE, FALSE),  # [vocab query] SNOMED: Impaired wheelchair mobility
             list(619446L,  FALSE, FALSE),  # [vocab query] Does mobilize using walker
             list(619444L,  FALSE, FALSE),  # [vocab query] Does mobilize using crutch
             list(1450204L, FALSE, FALSE),  # [vocab query] SNOMED: Needs help with walking
             list(4146424L, FALSE, FALSE)   # [vocab query] SNOMED: Dependent for walking
           )),
      list(name = "[DVI] Ambulatory Status - Device domain", criterion = "DeviceExposure",
           items = list(
             list(4240470L,  TRUE, FALSE),  # [vocab query] SNOMED: Wheelchair (+ descendants, 2026-08-03 decision)
             list(37165652L, TRUE, FALSE),  # [vocab query] Wheeled walker
             list(4141765L,  TRUE, FALSE),  # [vocab query] SNOMED: Walking frame
             list(4251933L,  TRUE, FALSE)   # [vocab query] SNOMED: Crutch
           )))
  )
)

# -----------------------------------------------------------------------------
# Connect and fetch circe concept metadata for every id above.
# -----------------------------------------------------------------------------
cd <- createConnectionDetails(
  dbms = "sql server",
  server   = Sys.getenv("MSSQL_HOST", unset = "mssql_dev"),
  user     = Sys.getenv("MSSQL_USER", unset = "sa"),
  password = Sys.getenv("MSSQL_SA_PASSWORD"),
  extraSettings = paste0("database=omop_synth;trustServerCertificate=true;portNumber=",
                         Sys.getenv("MSSQL_PORT", unset = "1433"))
)
con <- connect(cd)
on.exit(disconnect(con), add = TRUE)

all_ids <- unique(unlist(lapply(cohorts, function(co)
  unlist(lapply(co$sets, function(s) vapply(s$items, `[[`, integer(1), 1))))))
meta <- querySql(con, sprintf(
  "SELECT concept_id, concept_name, standard_concept, invalid_reason, concept_code,
          domain_id, vocabulary_id, concept_class_id
   FROM omop_vocab.concept WHERE concept_id IN (%s)", paste(all_ids, collapse = ",")))
names(meta) <- tolower(names(meta))

# Guard: every id must exist, be standard, and be valid. A pretraining-era id
# that was never verified would otherwise be written into a cohort silently.
missing <- setdiff(all_ids, meta$concept_id)
if (length(missing) > 0) stop("Concept id(s) not in omop_vocab: ", paste(missing, collapse = ", "))
bad <- meta[is.na(meta$standard_concept) | meta$standard_concept != "S" |
            !is.na(meta$invalid_reason), ]
if (nrow(bad) > 0) stop("Non-standard / invalid concept(s): ", paste(bad$concept_id, collapse = ", "))

# circe's expression JSON labels a valid concept INVALID_REASON "V".
concept_obj <- function(id) {
  m <- meta[meta$concept_id == id, ]
  list(CONCEPT_ID = as.integer(m$concept_id), CONCEPT_NAME = m$concept_name,
       STANDARD_CONCEPT = "S", STANDARD_CONCEPT_CAPTION = "Standard",
       INVALID_REASON = "V", INVALID_REASON_CAPTION = "Valid",
       CONCEPT_CODE = m$concept_code, DOMAIN_ID = m$domain_id,
       VOCABULARY_ID = m$vocabulary_id, CONCEPT_CLASS_ID = m$concept_class_id)
}

# -----------------------------------------------------------------------------
# Assemble and write each cohort's circe JSON.
# -----------------------------------------------------------------------------
empty_obj <- structure(list(), names = character(0))  # serialises as {} not []

for (cid in names(cohorts)) {
  co <- cohorts[[cid]]

  concept_sets <- lapply(seq_along(co$sets), function(i) {
    s <- co$sets[[i]]
    list(id = i - 1L, name = s$name,
         expression = list(items = lapply(s$items, function(it)
           list(concept = concept_obj(it[[1]]), isExcluded = it[[3]],
                includeDescendants = it[[2]], includeMapped = FALSE))))
  })
  criteria <- lapply(seq_along(co$sets), function(i) {
    out <- list(list(CodesetId = i - 1L)); names(out) <- co$sets[[i]]$criterion; out
  })

  expr <- list(
    cdmVersionRange = ">=5.0.0",
    `_README` = c(co$readme,
      "PrimaryCriteriaLimit / QualifiedLimit / ExpressionLimit are all \"All\" and EndStrategy is StartDate+0, unlike the [DVI] VA-FI cohorts (1797948-1797977) which are \"First\". A first-ever-only cohort cannot answer a lookback-window question."),
    ConceptSets = concept_sets,
    PrimaryCriteria = list(
      CriteriaList = criteria,
      ObservationWindow = list(PriorDays = 0L, PostDays = 0L),
      PrimaryCriteriaLimit = list(Type = "All")),
    QualifiedLimit  = list(Type = "All"),
    ExpressionLimit = list(Type = "All"),
    InclusionRules  = list(),
    CensoringCriteria = list(),
    CollapseSettings = list(CollapseType = "ERA", EraPad = 0L),
    CensorWindow = empty_obj,
    EndStrategy = list(DateOffset = list(DateField = "StartDate", Offset = 0L))
  )

  path <- file.path("inst", "cohorts", paste0(cid, ".json"))
  writeLines(toJSON(expr, pretty = TRUE, auto_unbox = TRUE, null = "null"), path)
  cat(sprintf("  wrote %s  (%d concept set(s), %d concept(s))\n", path, length(concept_sets),
              sum(vapply(co$sets, function(s) length(s$items), integer(1)))))
}
cat("\nNext: Rscript scripts/render_cohort_sql.R   (renders SQL for ids whose .sql is absent)\n")
