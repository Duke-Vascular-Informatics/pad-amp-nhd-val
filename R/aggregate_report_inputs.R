# =============================================================================
# R/aggregate_report_inputs.R
#
# Turns this study's PERSON-LEVEL scoring output into AGGREGATE-ONLY artifacts
# that pad-amp-nhd-prog-report can render every figure and table from, so no
# row-level patient data ever has to leave PRCC.
#
# WHY THIS EXISTS
# ---------------
# Phase 0/1 split the report out of this repo and made it render from CSV
# artifacts instead of a database. But three of those artifacts were still
# row-level -- one row per patient:
#   demographics_age.csv   every patient's exact age
#   discharge_types.csv    subject_id + that patient's discharge disposition
#   {iannuzzi,mfi5,vqifs}/person_level_scores.csv
#                          subject_id, index_date (a real calendar date),
#                          outcome, every score component, predicted risk
# The report genuinely needed them (ROC, DCA, risk tiers, score histograms,
# rate-by-year/month, Table 1's age row), so "the report has no database
# dependency" was true while "the report needs no patient-level data" was not.
# That distinction matters at the egress boundary, not at the code boundary:
# a reviewer approving a results export should never be asked to approve a
# per-subject table just so a figure can be drawn.
#
# THE KEY OBSERVATION that makes this possible: every one of those figures is
# a function of COUNTS, not of individual rows.
#   - These are INTEGER risk scores with a small number of distinct values,
#     and each predicted risk is a deterministic function of the score. So an
#     ROC curve is fully determined by, at each distinct score value, how many
#     patients had the outcome and how many did not -- cumulative sums of
#     those two counts give every (FPR, TPR) point exactly. Nothing is
#     approximated here; the curve is identical to the one computed from the
#     person-level frame.
#   - Net benefit at threshold t is (TP/n) - (FP/n) * (t/(1-t)); TP and FP at
#     every t come from the same cumulative counts.
#   - Risk tiers, score histograms, and rate-by-year/month are counts already.
# So this file computes each figure's inputs ONCE, here, where the
# person-level data legitimately lives, and writes them as small aggregate
# tables. The report becomes a pure plotter.
#
# SMALL-CELL SUPPRESSION
# ----------------------
# Count-bearing cells below `min_cell_count` are written as NA with a
# `suppressed = TRUE` flag rather than dropped, so the report can render an
# honest gap instead of silently omitting a category (a dropped row and a
# genuinely-zero row are not the same claim). Rate-valued outputs (ROC points,
# net benefit) carry no counts at all -- they are cohort-level summary
# statistics, and suppressing points along a curve would distort its shape
# while disclosing nothing. Year/month strata additionally keep the stricter
# minimums the report's own plots already applied (>= 11 and >= 5) so this
# change cannot loosen an existing threshold.
#
# INPUTS   config$output_folder/{iannuzzi,mfi5,vqifs}/person_level_scores.csv
#          config$output_folder/report_inputs/discharge_types.csv  (transient)
#          config$output_folder/report_inputs/demographics_age.csv (transient)
# OUTPUTS  config$output_folder/report_inputs/agg_*.csv
#
# Called by StrategusCodeToRun.R (and run_analysis.R in the PRCC bundle)
# immediately after extract_report_inputs(), which produces the two transient
# row-level files above. Those two are DELETED once aggregated -- see
# .drop_row_level_inputs() at the bottom.
# =============================================================================


# -----------------------------------------------------------------------------
# .suppress_counts()
#
# Applies small-cell suppression to the named count columns of a data frame.
# Rows whose count falls below `min_cell_count` (and is not exactly 0) get NA
# in every named column plus suppressed = TRUE.
#
# A genuine 0 is NOT suppressed: "no patients in this category" discloses no
# individual, and blanking it would make an empty stratum indistinguishable
# from a small one -- the opposite of what a reviewer needs to see.
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# .rbind_union()
#
# rbind() over data frames whose columns differ, filling absent columns with NA.
# Needed because only the Iannuzzi score has a published lookup table, so its
# score-value frame carries predicted_risk_lookup while mFI-5's and sVQI-FS's
# do not -- a plain rbind() fails with "numbers of columns do not match".
# -----------------------------------------------------------------------------
.rbind_union <- function(dfs) {
  dfs <- Filter(function(d) !is.null(d) && nrow(d) > 0, dfs)
  if (length(dfs) == 0) return(NULL)
  all_cols <- unique(unlist(lapply(dfs, names)))
  do.call(rbind, lapply(dfs, function(d) {
    for (cc in setdiff(all_cols, names(d))) d[[cc]] <- NA
    d[, all_cols, drop = FALSE]
  }))
}


.suppress_counts <- function(df, count_cols, min_cell_count = 5L,
                             derived_cols = character(0)) {
  if (is.null(df) || nrow(df) == 0) return(df)
  count_cols <- intersect(count_cols, names(df))
  if (length(count_cols) == 0) return(df)

  small <- Reduce(`|`, lapply(count_cols, function(cc) {
    v <- suppressWarnings(as.numeric(df[[cc]]))
    !is.na(v) & v > 0 & v < min_cell_count
  }))
  small[is.na(small)] <- FALSE

  for (cc in count_cols) df[[cc]][small] <- NA

  # DERIVED COLUMNS MUST GO TOO. Blanking a count while leaving a rate computed
  # from it is not suppression: with n = 11 shown and nhd_rate = 27.272727%,
  # the "suppressed" event count is recoverable by multiplication (= 3). Found
  # exactly that leak in agg_nhd_by_year on 2026-08-11. Any column derived from
  # a suppressed count must be blanked in the same rows.
  for (dc in intersect(derived_cols, names(df))) df[[dc]][small] <- NA

  df$suppressed <- small
  df
}


# -----------------------------------------------------------------------------
# .score_value_counts()
#
# The core reduction. Collapses a person_level frame to one row per
# (split_set, total_score), carrying the event/non-event counts and the
# predicted risks attached to that score value.
#
# Predicted risks are deterministic functions of the integer score, so they are
# constant within a score group -- taken as the first non-NA value and checked
# for that assumption (a warning, not a stop: a recalibration fitted on a
# covariate not in the score would break the invariant, and that is worth
# surfacing loudly without killing an otherwise-good run).
# -----------------------------------------------------------------------------
.score_value_counts <- function(pl) {
  if (is.null(pl) || nrow(pl) == 0) return(NULL)
  if (!all(c("total_score", "outcome") %in% names(pl))) return(NULL)

  pl$split_set <- if ("split_set" %in% names(pl)) as.character(pl$split_set) else "all"
  risk_cols <- intersect(c("predicted_risk_lookup", "predicted_risk_recalibrated"),
                         names(pl))

  keys <- unique(pl[, c("split_set", "total_score"), drop = FALSE])
  keys <- keys[order(keys$split_set, keys$total_score), , drop = FALSE]

  out <- do.call(rbind, lapply(seq_len(nrow(keys)), function(i) {
    sel <- pl$split_set == keys$split_set[i] &
           !is.na(pl$total_score) & pl$total_score == keys$total_score[i]
    sub <- pl[sel, , drop = FALSE]
    y   <- as.integer(sub$outcome)

    row <- data.frame(
      split_set   = keys$split_set[i],
      total_score = keys$total_score[i],
      n_total     = nrow(sub),
      n_events    = sum(y == 1L, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
    for (rc in risk_cols) {
      v <- sub[[rc]][!is.na(sub[[rc]])]
      if (length(v) == 0) {
        row[[rc]] <- NA_real_
      } else {
        if (length(unique(round(v, 10))) > 1L) {
          warning(sprintf(
            paste0("[aggregate] %s is not constant within total_score = %s ",
                   "(%d distinct values). The score-value reduction assumes ",
                   "predicted risk is a function of the score alone; the ",
                   "aggregated ROC/DCA for this model will not match a ",
                   "person-level computation. Investigate before trusting it."),
            rc, keys$total_score[i], length(unique(round(v, 10)))
          ), call. = FALSE)
        }
        row[[rc]] <- v[1]
      }
    }
    row
  }))
  out
}


# -----------------------------------------------------------------------------
# .roc_from_counts()
#
# Exact ROC curve from per-score-value counts. `risk` orders the score values
# (higher = more likely positive); ties at one risk value collapse to a single
# operating point, exactly as they do in a person-level ROC.
#
# Returns fpr/tpr only -- no counts -- so nothing here is a disclosive cell.
# -----------------------------------------------------------------------------
.roc_from_counts <- function(risk, n_events, n_total) {
  keep <- !is.na(risk) & !is.na(n_total) & n_total > 0
  if (sum(keep) == 0) return(NULL)
  risk <- risk[keep]; n_events <- n_events[keep]; n_total <- n_total[keep]

  # TIES MUST COLLAPSE TO ONE OPERATING POINT. Several distinct score values
  # can map to the same predicted risk (the Iannuzzi lookup table saturates at
  # both ends, so 15 distinct risks cover far more score values). Patients who
  # share a predicted risk cannot be separated by any threshold, so they
  # contribute a SINGLE point, and the curve crosses that block along its
  # diagonal chord. Cumulating the score-value rows individually instead draws
  # a staircase through the block, which strictly over-estimates the area --
  # measured here at +7.4e-05 AUC against pROC before this grouping was added.
  # Grouping by risk first makes the result exact, not merely close.
  agg <- stats::aggregate(cbind(n_events, n_total) ~ risk,
                          data = data.frame(risk = risk, n_events = n_events,
                                            n_total = n_total),
                          FUN = sum)

  n_neg_v <- agg$n_total - agg$n_events
  P <- sum(agg$n_events); N <- sum(n_neg_v)
  if (P == 0 || N == 0) return(NULL)

  ord <- order(agg$risk, decreasing = TRUE)
  tp <- cumsum(agg$n_events[ord])
  fp <- cumsum(n_neg_v[ord])

  data.frame(
    fpr = c(0, fp / N, 1),
    tpr = c(0, tp / P, 1),
    stringsAsFactors = FALSE
  )
}


# -----------------------------------------------------------------------------
# .net_benefit_from_counts()
#
# Net benefit across thresholds, from per-score-value counts. Mirrors the
# report's own formula exactly: NB(t) = TP/n - FP/n * (t/(1-t)), classifying
# positive when predicted risk >= t.
# -----------------------------------------------------------------------------
.net_benefit_from_counts <- function(risk, n_events, n_total, thresholds) {
  keep <- !is.na(risk) & !is.na(n_total) & n_total > 0
  if (sum(keep) == 0) return(NULL)
  risk <- risk[keep]; n_events <- n_events[keep]; n_total <- n_total[keep]

  n <- sum(n_total)
  if (n == 0) return(NULL)
  n_neg_v <- n_total - n_events

  vapply(thresholds, function(pt) {
    pos <- risk >= pt
    tp <- sum(n_events[pos]); fp <- sum(n_neg_v[pos])
    tp / n - fp / n * (pt / (1 - pt))
  }, numeric(1L))
}


# -----------------------------------------------------------------------------
# aggregate_report_inputs()
#
# Main entry point. Reads the person-level scoring outputs plus the two
# transient row-level extract artifacts, writes the agg_*.csv set, and (unless
# keep_row_level = TRUE) deletes the row-level inputs it consumed.
#
# @param config           Study config from get_validation_config()/get_prcc_config().
# @param min_cell_count   Suppression threshold for count-bearing cells.
# @param keep_row_level   TRUE leaves demographics_age.csv / discharge_types.csv
#                         and person_level_scores.csv in place. Only for local
#                         debugging -- never on PRCC.
# @return The report_inputs directory path, invisibly.
# -----------------------------------------------------------------------------
aggregate_report_inputs <- function(config,
                                    min_cell_count = 5L,
                                    keep_row_level = FALSE) {

  out_dir <- config$output_folder
  ri_dir  <- file.path(out_dir, "report_inputs")
  if (!dir.exists(ri_dir)) dir.create(ri_dir, recursive = TRUE, showWarnings = FALSE)

  message("[aggregate] Writing aggregate report inputs to ", ri_dir)

  .write <- function(df, name) {
    if (is.null(df) || nrow(df) == 0) {
      message("[aggregate] ", name, ": no data -- file not written")
      return(invisible(NULL))
    }
    utils::write.csv(df, file.path(ri_dir, paste0(name, ".csv")), row.names = FALSE)
    message("[aggregate] ", name, ": ", nrow(df), " row(s)")
  }

  # Model specifications, in the order the report presents them. label is what
  # appears in the figure legend / table section header, so it is defined once
  # here and carried through every artifact rather than reconstructed downstream.
  specs <- list(
    list(id = "iannuzzi", label = "Iannuzzi 2020"),
    list(id = "mfi5",     label = "mFI-5"),
    list(id = "vqifs",    label = "sVQI-FS")
  )

  pl_list <- list()
  for (sp in specs) {
    f <- file.path(out_dir, sp$id, "person_level_scores.csv")
    if (file.exists(f)) {
      pl_list[[sp$id]] <- utils::read.csv(f, stringsAsFactors = FALSE)
    }
  }
  if (length(pl_list) == 0) {
    warning("[aggregate] No person_level_scores.csv found for any score -- ",
            "nothing to aggregate. Did the scoring step run?", call. = FALSE)
    return(invisible(ri_dir))
  }

  # ---------------------------------------------------------------------------
  # 1. Cohort summary + score-value counts (the reduction everything else uses)
  # ---------------------------------------------------------------------------
  cohort_rows <- list(); sv_rows <- list()
  for (sp in specs) {
    pl <- pl_list[[sp$id]]
    if (is.null(pl)) next
    cohort_rows[[sp$id]] <- data.frame(
      score_id = sp$id, model_label = sp$label,
      n_total  = nrow(pl),
      n_events = sum(as.integer(pl$outcome) == 1L, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
    sv <- .score_value_counts(pl)
    if (!is.null(sv)) {
      sv$score_id <- sp$id; sv$model_label <- sp$label
      sv_rows[[sp$id]] <- sv
    }
  }
  .write(.rbind_union(cohort_rows), "agg_cohort_summary")

  sv_all <- .rbind_union(sv_rows)
  # Written suppressed for human/reviewer consumption. The UNsuppressed copy
  # stays in memory for the curve computations below: suppressing a cell then
  # computing a cumulative curve from the NA would silently corrupt the curve,
  # which is a far worse outcome than publishing a rate that was derived from
  # a small cell but discloses no individual.
  .write(.suppress_counts(sv_all, c("n_total", "n_events"), min_cell_count),
         "agg_score_value_counts")

  # ---------------------------------------------------------------------------
  # 2. Score distribution histograms (Supplemental Figures S6-S8)
  # ---------------------------------------------------------------------------
  dist_rows <- list()
  for (sp in specs) {
    pl <- pl_list[[sp$id]]
    if (is.null(pl) || !all(c("total_score", "outcome") %in% names(pl))) next
    tab <- as.data.frame(table(
      total_score = pl$total_score,
      outcome     = ifelse(as.integer(pl$outcome) == 1L, "NHD", "No NHD")
    ), stringsAsFactors = FALSE)
    names(tab)[names(tab) == "Freq"] <- "count"
    tab$total_score <- as.numeric(as.character(tab$total_score))
    tab$score_id <- sp$id; tab$model_label <- sp$label
    dist_rows[[sp$id]] <- tab[order(tab$total_score, tab$outcome), ]
  }
  .write(.suppress_counts(.rbind_union(dist_rows), "count", min_cell_count),
         "agg_score_distribution")

  # ---------------------------------------------------------------------------
  # 3. ROC points (Figure 2) -- one curve per model/specification
  # ---------------------------------------------------------------------------
  # RANKED BY RAW SCORE, NOT BY PREDICTED RISK -- deliberately, and this is the
  # one place the choice actually changes the answer.
  #
  # metrics.csv computes AUROC from the integer score with pROC's
  # direction = "<" (see R/risk_score_pipeline.R), and the report's own
  # multi-model ROC figure likewise plots total_score / max(total_score). A
  # recalibration is a monotone transform of the score ONLY when its fitted
  # slope is positive; for an anti-predictive score it is monotone DECREASING,
  # which flips the curve. Observed here: sVQI-FS's recalibrated risk ranks
  # inversely to its score, so ranking by risk gave AUC 0.4397 against the
  # 0.5603 reported in metrics.csv -- the figure would have contradicted the
  # table it sits next to. Ranking by score keeps curve and metric consistent
  # by construction, whatever the recalibration does.
  #
  # DCA below is unaffected and deliberately still uses predicted risk: net
  # benefit is a function of the probability itself, not of the ranking.
  roc_rows <- list()
  add_roc <- function(sv, rank_col, label, split_filter = NULL) {
    if (is.null(sv) || !rank_col %in% names(sv)) return(NULL)
    d <- sv
    if (!is.null(split_filter)) d <- d[d$split_set == split_filter, , drop = FALSE]
    else d <- .collapse_splits(d, rank_col)
    if (is.null(d) || nrow(d) == 0) return(NULL)
    pts <- .roc_from_counts(d[[rank_col]], d$n_events, d$n_total)
    if (is.null(pts)) return(NULL)
    pts$curve_label <- label
    # Trapezoidal AUC of the emitted points, carried alongside them so the
    # report can annotate the figure without recomputing, and so a mismatch
    # against metrics.csv is visible in the artifact itself.
    o <- order(pts$fpr, pts$tpr)
    pts$auc <- sum(diff(pts$fpr[o]) *
                   (utils::head(pts$tpr[o], -1) + utils::tail(pts$tpr[o], -1)) / 2)
    roc_rows[[label]] <<- pts
  }
  for (sp in specs) {
    sv <- sv_rows[[sp$id]]
    if (is.null(sv)) next
    # Score-ranked, TEST PARTITION -- confirmed empirically (2026-08-11) to be
    # exactly what metrics.csv's AUROC reports for all three scores: full
    # cohort and train partition both disagree, test agrees to 6+ decimals.
    add_roc(sv, "total_score", paste0(sp$label, " (Score)"), split_filter = "test")
    # The published Iannuzzi lookup additionally gets its own probability-ranked
    # curve over the FULL cohort, matching how the report presents it (nothing
    # is fitted, so the whole cohort is a valid evaluation set for it).
    if (sp$id == "iannuzzi") add_roc(sv, "predicted_risk_lookup", "Iannuzzi (Lookup)")
  }
  .write(.rbind_union(roc_rows), "agg_roc_points")

  # ---------------------------------------------------------------------------
  # 4. Decision curve analysis (Figure 4)
  #
  # Computed over the SAME threshold grid the report used (0.01-0.99 by 0.005)
  # so the rendered curve is unchanged. Reference strategies (treat-all /
  # treat-none) are emitted here too rather than recomputed downstream, since
  # treat-all depends on prevalence -- a cohort statistic the report should not
  # have to re-derive.
  #
  # EVALUATION SET: the test partition, matching the report's own behaviour.
  # The published Iannuzzi lookup is evaluated on that same partition here for
  # curve-to-curve comparability (it fits nothing, so a subset introduces no
  # leakage -- only fewer events), which is exactly what the report did.
  # ---------------------------------------------------------------------------
  thresholds <- seq(0.01, 0.99, by = 0.005)
  dca_rows <- list(); dca_meta <- list()
  for (sp in specs) {
    sv <- sv_rows[[sp$id]]
    if (is.null(sv)) next
    sv_test <- sv[sv$split_set == "test", , drop = FALSE]
    if (nrow(sv_test) == 0) sv_test <- sv
    cand <- list()
    if (sp$id == "iannuzzi" && "predicted_risk_lookup" %in% names(sv_test))
      cand[["Iannuzzi (Lookup)"]] <- "predicted_risk_lookup"
    if ("predicted_risk_recalibrated" %in% names(sv_test))
      cand[[paste0(sp$label, " (Recal.)")]] <- "predicted_risk_recalibrated"

    for (lbl in names(cand)) {
      rc <- cand[[lbl]]
      nb <- .net_benefit_from_counts(sv_test[[rc]], sv_test$n_events,
                                     sv_test$n_total, thresholds)
      if (is.null(nb)) next
      # pct_high = the share of the evaluated cohort this model would classify
      # positive at each threshold. It drives the DCA's clinical-impact strip,
      # which the report used to compute as mean(pv >= t) over per-patient
      # predictions. It is a plain weighted proportion of the same counts, so
      # it belongs here rather than being the one thing that keeps the report
      # needing patient-level vectors.
      .r <- sv_test[[rc]]; .n <- sv_test$n_total
      .ok <- !is.na(.r) & !is.na(.n)
      pct_high <- vapply(thresholds, function(pt) {
        if (!any(.ok) || sum(.n[.ok]) == 0) return(NA_real_)
        100 * sum(.n[.ok][.r[.ok] >= pt]) / sum(.n[.ok])
      }, numeric(1L))
      dca_rows[[lbl]] <- data.frame(threshold = thresholds, net_benefit = nb,
                                    pct_high = pct_high,
                                    strategy = lbl, stringsAsFactors = FALSE)
      dca_meta[[lbl]] <- data.frame(
        strategy = lbl,
        max_predicted_risk = max(sv_test[[rc]], na.rm = TRUE),
        stringsAsFactors = FALSE
      )
    }
  }
  if (length(dca_rows) > 0) {
    # Prevalence from the primary specification's test partition -- the same
    # denominator every model curve above was computed on.
    sv_ref <- sv_rows[["iannuzzi"]]
    if (is.null(sv_ref)) sv_ref <- sv_rows[[1]]
    ref_test <- sv_ref[sv_ref$split_set == "test", , drop = FALSE]
    if (nrow(ref_test) == 0) ref_test <- sv_ref
    n_ref    <- sum(ref_test$n_total)
    prev     <- if (n_ref > 0) sum(ref_test$n_events) / n_ref else NA_real_

    dca_rows[["Treat all"]] <- data.frame(
      threshold = thresholds,
      net_benefit = pmax(prev - (1 - prev) * (thresholds / (1 - thresholds)), 0),
      strategy = "Treat all", stringsAsFactors = FALSE
    )
    dca_rows[["Treat none"]] <- data.frame(
      threshold = thresholds, net_benefit = 0,
      strategy = "Treat none", stringsAsFactors = FALSE
    )
    .write(.rbind_union(dca_rows), "agg_dca_net_benefit")
    .write(data.frame(
      n_evaluated = n_ref, prevalence = prev,
      max_predicted_risk_any = max(vapply(dca_meta, function(d) d$max_predicted_risk,
                                          numeric(1)), na.rm = TRUE),
      stringsAsFactors = FALSE
    ), "agg_dca_meta")
  }

  # ---------------------------------------------------------------------------
  # 5. Risk tiers (Table 5)
  # ---------------------------------------------------------------------------
  tier_rows <- list()
  add_tiers <- function(sv, risk_col, label, split_filter) {
    if (is.null(sv) || !risk_col %in% names(sv)) return(NULL)
    d <- if (is.null(split_filter)) .collapse_splits(sv, risk_col)
         else sv[sv$split_set == split_filter, , drop = FALSE]
    if (is.null(d) || nrow(d) == 0) return(NULL)
    r <- d[[risk_col]]
    tier <- ifelse(is.na(r), NA_character_,
             ifelse(r < 0.30, "Low (<30%)",
              ifelse(r <= 0.70, "Intermediate (30–70%)", "High (>70%)")))
    lv <- c("Low (<30%)", "Intermediate (30–70%)", "High (>70%)")
    tier_rows[[label]] <<- do.call(rbind, lapply(lv, function(tl) {
      sel <- !is.na(tier) & tier == tl
      data.frame(model_label = label, tier = tl,
                 n = sum(d$n_total[sel]), events = sum(d$n_events[sel]),
                 stringsAsFactors = FALSE)
    }))
  }
  sv_i <- sv_rows[["iannuzzi"]]
  add_tiers(sv_i, "predicted_risk_lookup",
            "Iannuzzi 2020 (Lookup, full cohort)", NULL)
  add_tiers(sv_i, "predicted_risk_recalibrated",
            "Iannuzzi 2020 (Recalibrated, test set)", "test")
  add_tiers(sv_rows[["mfi5"]], "predicted_risk_recalibrated",
            "mFI-5 (Recalibrated, test set)", "test")
  add_tiers(sv_rows[["vqifs"]], "predicted_risk_recalibrated",
            "sVQI-FS (Recalibrated, test set)", "test")

  # Iannuzzi's PUBLISHED score bands (Table II / Fig 2 of the paper), which
  # tier on the raw score rather than predicted probability. Published rates
  # travel with the data so the report does not carry a second copy of them.
  if (!is.null(sv_i)) {
    d <- .collapse_splits(sv_i, "predicted_risk_lookup")
    pub <- c("Low (score 0–4)" = 10.1, "Moderate (score 5–9)" = 36.7,
             "High (score ≥10)" = 66.1)
    band <- ifelse(d$total_score <= 4, names(pub)[1],
             ifelse(d$total_score <= 9, names(pub)[2], names(pub)[3]))
    tier_rows[["iannuzzi_published"]] <- do.call(rbind, lapply(names(pub), function(b) {
      sel <- band == b
      data.frame(
        model_label = "Iannuzzi 2020 published score strata (full cohort)",
        tier = b, n = sum(d$n_total[sel]), events = sum(d$n_events[sel]),
        published_rate_pct = unname(pub[[b]]), stringsAsFactors = FALSE
      )
    }))
  }
  tiers <- .rbind_union(tier_rows)
  .write(.suppress_counts(tiers, c("n", "events"), min_cell_count),
         "agg_risk_tiers")

  # ---------------------------------------------------------------------------
  # 6. Age summary (Table 1's age row) -- replaces demographics_age.csv
  # ---------------------------------------------------------------------------
  age_f <- file.path(ri_dir, "demographics_age.csv")
  if (file.exists(age_f)) {
    a <- utils::read.csv(age_f, stringsAsFactors = FALSE)
    ac <- names(a)[toupper(names(a)) == "AGE_AT_INDEX"][1]
    ages <- if (!is.na(ac)) as.numeric(a[[ac]]) else numeric(0)
    ages <- ages[!is.na(ages)]
    if (length(ages) > 0) {
      q <- stats::quantile(ages, probs = c(0.25, 0.75), na.rm = TRUE)
      .write(data.frame(
        n = length(ages),
        median = as.numeric(stats::median(ages)),
        p25 = as.numeric(q[[1]]), p75 = as.numeric(q[[2]]),
        stringsAsFactors = FALSE
      ), "agg_age_summary")

      brk <- c(-Inf, 49, 59, 69, 79, Inf)
      lbl <- c("<50", "50–59", "60–69", "70–79", "≥80")
      grp <- as.data.frame(table(age_group = cut(ages, breaks = brk, labels = lbl)),
                           stringsAsFactors = FALSE)
      names(grp)[names(grp) == "Freq"] <- "n"
      .write(.suppress_counts(grp, "n", min_cell_count), "agg_age_groups")
    }
  }

  # ---------------------------------------------------------------------------
  # 7. NHD rate by year and by month (Figure 1, Supplemental Figure S5)
  #
  # Needs index_date x discharge_type, which lives across two row-level files:
  # person_level_scores.csv (index_date, outcome) and discharge_types.csv
  # (subject_id -> disposition). Joined here, once, then reduced to counts.
  #
  # The >= 11 (year) and >= 5 (month) minimums are the report's own existing
  # thresholds, kept as-is rather than replaced by min_cell_count so this
  # change can only tighten suppression, never loosen it.
  # ---------------------------------------------------------------------------
  pl_i  <- pl_list[["iannuzzi"]]
  dt_f  <- file.path(ri_dir, "discharge_types.csv")
  if (!is.null(pl_i) && "index_date" %in% names(pl_i)) {
    if (file.exists(dt_f)) {
      dt <- utils::read.csv(dt_f, stringsAsFactors = FALSE)
      if (nrow(dt) > 0 && "subject_id" %in% names(dt)) {
        dt$subject_id <- as.integer(dt$subject_id)
        pl_i <- merge(pl_i, dt, by = "subject_id", all.x = TRUE)
      }
    }
    yr <- suppressWarnings(as.integer(format(as.Date(pl_i$index_date), "%Y")))
    mo <- suppressWarnings(as.integer(format(as.Date(pl_i$index_date), "%m")))

    if ("discharge_type" %in% names(pl_i)) {
      nhd_levels <- c("SNF", "IRF", "Hospice", "LTAC", "Other NHD")
      dfy <- data.frame(year = yr, dtype = as.character(pl_i$discharge_type),
                        stringsAsFactors = FALSE)
      dfy <- dfy[!is.na(dfy$year), ]
      yrs <- sort(unique(dfy$year))
      year_tbl <- do.call(rbind, lapply(yrs, function(y) {
        sub <- dfy[dfy$year == y, ]
        n_y <- nrow(sub)
        if (n_y < 11L) return(NULL)   # report's own existing year minimum
        do.call(rbind, lapply(nhd_levels, function(tp) data.frame(
          year = y, n = n_y, discharge_type = tp,
          events = sum(sub$dtype == tp, na.rm = TRUE), stringsAsFactors = FALSE
        )))
      }))
      if (!is.null(year_tbl)) {
        year_tbl$nhd_rate <- 100 * year_tbl$events / year_tbl$n
        # nhd_rate is derived from events and must be suppressed with it --
        # otherwise rate x n recovers the blanked count exactly.
        .write(.suppress_counts(year_tbl, "events", min_cell_count,
                                derived_cols = "nhd_rate"),
               "agg_nhd_by_year")
      }
    }

    dfm <- data.frame(month = mo, outcome = as.integer(pl_i$outcome))
    dfm <- dfm[!is.na(dfm$month), ]
    month_tbl <- do.call(rbind, lapply(1:12, function(m) {
      sub <- dfm[dfm$month == m, ]
      data.frame(month = m, n = nrow(sub),
                 events = sum(sub$outcome, na.rm = TRUE), stringsAsFactors = FALSE)
    }))
    # Below the report's own month minimum, n and events are both blanked so a
    # rate cannot be back-computed from them.
    month_tbl$n[month_tbl$n < 5L]      <- NA
    month_tbl$events[is.na(month_tbl$n)] <- NA
    .write(.suppress_counts(month_tbl, "events", min_cell_count),
           "agg_nhd_by_month")
  }

  if (!keep_row_level) .drop_row_level_inputs(config, ri_dir)

  message("[aggregate] Done.")
  invisible(ri_dir)
}


# -----------------------------------------------------------------------------
# .collapse_splits()
#
# Sums a score-value table across train/test splits so a full-cohort curve can
# be computed. Predicted risk is carried through as the first non-NA value per
# score, which is safe for exactly the reason documented in
# .score_value_counts(): it is a function of the score, not of the split.
# -----------------------------------------------------------------------------
.collapse_splits <- function(sv, risk_col) {
  if (is.null(sv) || nrow(sv) == 0) return(NULL)
  scores <- sort(unique(sv$total_score))
  do.call(rbind, lapply(scores, function(s) {
    sub <- sv[sv$total_score == s, , drop = FALSE]
    r <- sub[[risk_col]][!is.na(sub[[risk_col]])]
    data.frame(
      total_score = s,
      n_total  = sum(sub$n_total),
      n_events = sum(sub$n_events),
      risk     = if (length(r)) r[1] else NA_real_,
      stringsAsFactors = FALSE
    ) |> (\(d) { names(d)[names(d) == "risk"] <- risk_col; d })()
  }))
}


# -----------------------------------------------------------------------------
# .drop_row_level_inputs()
#
# Deletes the row-level files the aggregation consumed, so what remains under
# output/ for a results export is aggregate-only.
#
# person_level_scores.csv is deliberately NOT deleted: it is this study's
# primary analytic result, it stays on PRCC as the source of truth for any
# re-analysis, and export_results_for_review.R already refuses to package it.
# The point of this function is narrower -- remove the row-level files that
# existed ONLY to feed the report, so a report-inputs export needs no
# per-subject data at all.
# -----------------------------------------------------------------------------
.drop_row_level_inputs <- function(config, ri_dir) {
  for (f in c("demographics_age.csv", "discharge_types.csv")) {
    p <- file.path(ri_dir, f)
    if (file.exists(p)) {
      unlink(p)
      message("[aggregate] Removed row-level input: report_inputs/", f,
              " (aggregated above)")
    }
  }

  # Rewrite _manifest.csv's files_written to match what is actually on disk.
  # extract_report_inputs() writes that field before this step runs, so it
  # still named demographics_age and discharge_types -- a manifest asserting
  # that per-patient files are part of the output, which is precisely the
  # wrong thing for a data-egress reviewer to read. Rebuilt from a directory
  # listing rather than patched, so it cannot drift again.
  mf <- file.path(ri_dir, "_manifest.csv")
  if (file.exists(mf)) {
    m <- tryCatch(utils::read.csv(mf, stringsAsFactors = FALSE, check.names = FALSE),
                  error = function(e) NULL)
    if (!is.null(m) && nrow(m) > 0 && "files_written" %in% names(m)) {
      present <- setdiff(list.files(ri_dir, pattern = "\\.csv$"), "_manifest.csv")
      present <- sub("\\.csv$", "", present)
      if (file.exists(file.path(ri_dir, "_report_config.yaml")))
        present <- c(present, "_report_config.yaml")
      m$files_written[1] <- paste(sort(present), collapse = ";")
      m$aggregate_only <- TRUE
      utils::write.csv(m, mf, row.names = FALSE)
      message("[aggregate] _manifest.csv files_written refreshed (",
              length(present), " file(s)); aggregate_only = TRUE")
    }
  }
}
