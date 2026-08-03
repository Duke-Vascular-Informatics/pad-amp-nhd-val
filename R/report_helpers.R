# =============================================================================
# R/report_helpers.R
#
# Shared helper functions for all study-design-specific Word report templates.
#
# Purpose:
#   Centralises utility functions that are used across report templates so that
#   they do not need to be duplicated in each template file.  All templates
#   source this file (via the dispatcher R/report_extended.R) before defining
#   their own template-specific functions.
#
# Exports (functions sourced into the caller's environment):
#   .append_references_section()        — bibliography formatter (Vancouver/NLM)
#   .compute_ece()                      — expected calibration error
#   .save_roc_plot()                    — ROC curve PNG generation
#   .save_calibration_plot_from_table() — calibration plot from CSV
#   .save_calibration_plot_from_vectors() — calibration plot from vectors
#   .build_table1()                     — flextable styling helper
#   .build_cohort_summary_table()       — cohort-level summary stats flextable
#
# Usage:
#   This file is sourced automatically by R/report_extended.R (the dispatcher).
#   Template files (report_prognostic.R, etc.) DO NOT source it directly —
#   rely on the dispatcher to have sourced it first.
#
# Dependencies: officer, flextable, ggplot2, pROC (all managed via renv)
#
# Note: R/cohort_demographics.R is also sourced here because its
#   fetch_demographics_from_omop() helper is shared by all templates that
#   include a Table 1 demographics section.
# =============================================================================

library(officer)
library(flextable)
library(ggplot2)
library(pROC)

# Load cohort demographics helper functions (shared across all templates)
source("R/cohort_demographics.R")

# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

# =============================================================================
# .append_references_section()
#
# Appends a numbered Vancouver/NLM reference list to an officer Word document.
#
# Arguments:
#   doc       — an officer rdocx object (modified in place via return value)
#   citations — named list of citation objects, or NULL (no-op).
#               Each element must have: authors, title, journal, year,
#               volume, issue, pages, doi.
#               Names are used only for human readability; order determines
#               the citation numbers printed in the document.
#
# Returns: the updated rdocx object.
# =============================================================================
.append_references_section <- function(doc, citations) {
  if (is.null(citations) || length(citations) == 0L) return(doc)

  doc <- body_add_par(doc, "", style = "Normal")
  doc <- body_add_par(doc, "References", style = "heading 1")

  for (i in seq_along(citations)) {
    ref  <- citations[[i]]
    # Vancouver format: Authors. Title. Journal. Year;Vol(Issue):Pages. doi:DOI
    line <- sprintf(
      "%d. %s. %s. %s. %s;%s(%s):%s. doi:%s",
      i,
      ref$authors,
      ref$title,
      ref$journal,
      ref$year,
      ref$volume,
      ref$issue,
      ref$pages,
      ref$doi
    )
    doc <- body_add_par(doc, line, style = "Normal")
  }
  doc
}

# -----------------------------------------------------------------------------
# .compute_ece()   [internal — report_helpers.R shared copy]
#
# Computes Expected Calibration Error (ECE) for display in the Word report.
# This is a self-contained copy that does not depend on risk_score_pipeline.R
# being loaded, so the report can be regenerated independently of the pipeline.
#
# Uses equal-frequency (quantile) bins so that each bin contains approximately
# the same number of patients.  Falls back to a single [0, 1] bin when the
# probability distribution is degenerate (< 3 unique quantile breakpoints).
#
# Returns a named list:
#   $ece      — scalar ECE value
#   $bin_data — data frame with columns bin, n_pred, mean_pred, n_obs, mean_obs
# -----------------------------------------------------------------------------
.compute_ece <- function(y, p, n_bins = 10) {
  p <- pmin(pmax(p, 0.0001), 0.9999)
  breaks <- quantile(p, probs = seq(0, 1, length.out = n_bins + 1), na.rm = TRUE)
  if (length(unique(breaks)) < 3) breaks <- c(0, 1)

  p_binned <- cut(p, breaks = breaks, include.lowest = TRUE)

  ece_data <- aggregate(
    cbind(predicted = p, observed = y) ~ p_binned,
    data = data.frame(p = p, y = y, p_binned = p_binned),
    FUN = function(x) c(n = length(x), mean = mean(x, na.rm = TRUE))
  )

  ece_data <- cbind(ece_data[, 1], do.call(rbind, ece_data[, 2]))
  colnames(ece_data) <- c("bin", "n_pred", "mean_pred", "n_obs", "mean_obs")

  ece_value <- sum(ece_data$n_pred * abs(ece_data$mean_pred - ece_data$mean_obs)) / length(y)

  list(ece = ece_value, bin_data = ece_data)
}

# -----------------------------------------------------------------------------
# .save_roc_plot()
#
# Generates a ROC curve plot and saves it as a PNG to output_folder/roc_curve.png.
#
# When auc_override is supplied, the function tries both pROC direction
# conventions ("<" and ">") and picks whichever gives an AUC closest to
# auc_override.  This handles the rare case where pROC auto-detects the wrong
# direction — the label shown on the plot will still use the published AUC
# value passed in auc_override.
#
# Returns the path to the saved PNG file (invisibly NULL if y is degenerate).
# -----------------------------------------------------------------------------
.save_roc_plot <- function(y, p, output_folder, auc_override = NA_real_) {
  if (length(unique(y)) < 2) return(NULL)

  p <- pmin(pmax(p, 0.0001), 0.9999)

  if (!is.na(auc_override)) {
    roc_lt <- pROC::roc(response = y, predictor = p, quiet = TRUE, direction = "<")
    roc_gt <- pROC::roc(response = y, predictor = p, quiet = TRUE, direction = ">")
    auc_lt <- as.numeric(pROC::auc(roc_lt))
    auc_gt <- as.numeric(pROC::auc(roc_gt))

    if (abs(auc_lt - as.numeric(auc_override)) <= abs(auc_gt - as.numeric(auc_override))) {
      roc_obj <- roc_lt
      auc_val <- auc_lt
    } else {
      roc_obj <- roc_gt
      auc_val <- auc_gt
    }
    auc_label <- as.numeric(auc_override)
  } else {
    roc_obj <- pROC::roc(response = y, predictor = p, quiet = TRUE)
    auc_val <- as.numeric(pROC::auc(roc_obj))
    auc_label <- auc_val
  }

  # Create ROC curve data
  roc_data <- data.frame(
    fpr = 1 - roc_obj$specificities,
    tpr = roc_obj$sensitivities
  )

  p <- ggplot2::ggplot(roc_data, ggplot2::aes(x = fpr, y = tpr)) +
    ggplot2::geom_path(linewidth = 1) +
    ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") +
    ggplot2::labs(
      title = "Receiver Operating Characteristic Curve",
      subtitle = paste0("AUROC = ", round(auc_label, 3)),
      x = "False Positive Rate",
      y = "True Positive Rate"
    ) +
    ggplot2::xlim(0, 1) +
    ggplot2::ylim(0, 1) +
    ggplot2::coord_equal() +
    ggplot2::theme_minimal()

  out_file <- file.path(output_folder, "roc_curve.png")
  ggplot2::ggsave(out_file, p, width = 7, height = 5, dpi = 150)
  out_file
}

# -----------------------------------------------------------------------------
# .save_dual_roc_plot()
#
# Overlays two or three ROC curves on one plot and saves as roc_curve_dual.png.
# Used for the two-model comparison (Iannuzzi 2020 vs. Subramaniam mFI-5), with
# an optional third curve (e.g. Kraiss 2022 sVQI-FS).
#
# Parameters:
#   y1, p1      — outcome and predicted probability/score for model 1
#   y2, p2      — outcome and predicted probability/score for model 2
#   y3, p3      — optional outcome/score for a third model (NULL = two-curve plot)
#   label1      — legend label for model 1 (e.g. "Iannuzzi 2020 (lookup)")
#   label2      — legend label for model 2 (e.g. "mFI-5 (logistic)")
#   label3      — legend label for model 3 (e.g. "sVQI-FS (raw score)")
#   auc1, auc2, auc3 — optional AUC overrides for the subtitle (NA = compute from data)
#   output_folder — directory to write the PNG
# Returns the file path, or NULL if all outcomes are degenerate.
# -----------------------------------------------------------------------------
.save_dual_roc_plot <- function(y1, p1, y2, p2, y3 = NULL, p3 = NULL,
                                label1 = "Iannuzzi 2020",
                                label2 = "mFI-5",
                                label3 = "sVQI-FS",
                                auc1   = NA_real_,
                                auc2   = NA_real_,
                                auc3   = NA_real_,
                                output_folder) {
  has_third <- !is.null(y3) && !is.null(p3)

  if (length(unique(y1)) < 2 && length(unique(y2)) < 2 &&
      (!has_third || length(unique(y3)) < 2)) return(NULL)

  build_roc_df <- function(y, p, label) {
    if (length(unique(y)) < 2) return(NULL)
    p <- pmin(pmax(p, 0.0001), 0.9999)
    roc_obj <- pROC::roc(response = y, predictor = p, quiet = TRUE)
    data.frame(
      fpr   = 1 - roc_obj$specificities,
      tpr   = roc_obj$sensitivities,
      model = label,
      stringsAsFactors = FALSE
    )
  }

  df1 <- build_roc_df(y1, p1, label1)
  df2 <- build_roc_df(y2, p2, label2)
  df3 <- if (has_third) build_roc_df(y3, p3, label3) else NULL
  roc_df <- do.call(rbind, Filter(Negate(is.null), list(df1, df2, df3)))
  if (is.null(roc_df) || nrow(roc_df) == 0) return(NULL)

  # Compute AUCs for subtitle
  fmt_auc <- function(y, p, override) {
    if (!is.na(override)) return(round(as.numeric(override), 3))
    if (length(unique(y)) < 2) return(NA_real_)
    p <- pmin(pmax(p, 0.0001), 0.9999)
    round(as.numeric(pROC::auc(pROC::roc(y, p, quiet = TRUE))), 3)
  }
  auc1_val <- fmt_auc(y1, p1, auc1)
  auc2_val <- fmt_auc(y2, p2, auc2)

  subtitle <- paste0(
    label1, " AUROC = ", ifelse(is.na(auc1_val), "N/A", auc1_val),
    "   |   ",
    label2, " AUROC = ", ifelse(is.na(auc2_val), "N/A", auc2_val)
  )

  curve_levels <- c(label1, label2)
  pal          <- c("#1F3864", "#C00000")
  linetypes    <- c("solid", "longdash")

  if (has_third && !is.null(df3)) {
    auc3_val <- fmt_auc(y3, p3, auc3)
    subtitle <- paste0(subtitle, "   |   ",
                       label3, " AUROC = ", ifelse(is.na(auc3_val), "N/A", auc3_val))
    curve_levels <- c(curve_levels, label3)
    pal          <- c(pal, "#E68A1F")
    linetypes    <- c(linetypes, "dotdash")
  }

  roc_df$model <- factor(roc_df$model, levels = curve_levels)

  plt <- ggplot2::ggplot(roc_df, ggplot2::aes(x = fpr, y = tpr,
                                               colour = model, linetype = model)) +
    ggplot2::geom_path(linewidth = 1) +
    ggplot2::geom_abline(intercept = 0, slope = 1,
                         linetype = "dashed", colour = "grey60", linewidth = 0.5) +
    ggplot2::scale_colour_manual(values = pal) +
    ggplot2::scale_linetype_manual(values = linetypes) +
    ggplot2::labs(
      title    = "Receiver Operating Characteristic Curves",
      subtitle = subtitle,
      x        = "False Positive Rate (1 − Specificity)",
      y        = "True Positive Rate (Sensitivity)",
      colour   = NULL, linetype = NULL
    ) +
    ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1) +
    ggplot2::coord_equal() +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom")

  out_file <- file.path(output_folder, "roc_curve_dual.png")
  ggplot2::ggsave(out_file, plt, width = 7, height = 5.5, dpi = 150)
  out_file
}

# -----------------------------------------------------------------------------
# .save_calibration_plot_from_table()
#
# Reads a calibration CSV (must contain "predicted" and "observed" columns) and
# saves a calibration plot PNG to output_folder.
#
# Called from generate_manuscript_report() for the lookup-model calibration plot
# when the calibration_table_lookup.csv file was produced by a prior pipeline run.
# Returns NULL silently if the input file does not exist or lacks the required
# columns.
# -----------------------------------------------------------------------------
.save_calibration_plot_from_table <- function(calibration_table_path,
                                              output_folder,
                                              file_name = "calibration_lookup.png",
                                              plot_title = "Calibration Plot") {
  if (!file.exists(calibration_table_path)) {
    return(NULL)
  }

  cal <- read.csv(calibration_table_path, stringsAsFactors = FALSE)
  if (!all(c("predicted", "observed") %in% names(cal))) {
    return(NULL)
  }

  p <- ggplot2::ggplot(cal, ggplot2::aes(x = predicted, y = observed)) +
    ggplot2::geom_point(size = 2) +
    ggplot2::geom_line() +
    ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") +
    ggplot2::labs(
      title = plot_title,
      x = "Mean predicted risk",
      y = "Observed event rate"
    ) +
    ggplot2::scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
    ggplot2::scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
    ggplot2::coord_equal() +
    ggplot2::theme_minimal()

  out_file <- file.path(output_folder, file_name)
  ggplot2::ggsave(out_file, p, width = 5, height = 5, dpi = 150)
  out_file
}

# -----------------------------------------------------------------------------
# .save_calibration_plot_from_vectors()
#
# Fallback calibration plot generator when pipeline PNG/CSV artifacts are
# missing but person-level predictions are available in memory.
# -----------------------------------------------------------------------------
.save_calibration_plot_from_vectors <- function(y,
                                                p,
                                                output_folder,
                                                file_name,
                                                plot_title,
                                                n_bins = 10) {
  ok <- !(is.na(y) | is.na(p))
  y <- as.numeric(y[ok])
  p <- as.numeric(p[ok])

  if (length(y) < 10 || length(unique(y)) < 2) {
    return(NULL)
  }

  p <- pmin(pmax(p, 0.0001), 0.9999)
  qbreaks <- unique(stats::quantile(p, probs = seq(0, 1, length.out = n_bins + 1), na.rm = TRUE))
  if (length(qbreaks) < 3) {
    qbreaks <- c(0, 1)
  }

  bins <- cut(p, breaks = qbreaks, include.lowest = TRUE)
  cal <- aggregate(
    cbind(predicted = p, observed = y) ~ bins,
    data = data.frame(p = p, y = y, bins = bins),
    FUN = mean
  )

  p_cal <- ggplot2::ggplot(cal, ggplot2::aes(x = predicted, y = observed)) +
    ggplot2::geom_point(size = 2) +
    ggplot2::geom_line() +
    ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") +
    ggplot2::labs(
      title = plot_title,
      x = "Mean predicted risk",
      y = "Observed event rate"
    ) +
    ggplot2::scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
    ggplot2::scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
    ggplot2::coord_equal() +
    ggplot2::theme_minimal()

  out_file <- file.path(output_folder, file_name)
  ggplot2::ggsave(out_file, p_cal, width = 5, height = 5, dpi = 150)
  out_file
}

# -----------------------------------------------------------------------------
# .save_dual_calibration_plot()
#
# Overlays up to four calibration curves on a single ggplot:
#   1. Iannuzzi (Lookup)        — dark blue solid
#   2. Iannuzzi (Recal.)  — dark red longdash
#   3. mFI-5 (Recal.)          — dark green dotdash (optional)
#   4. sVQI-FS (Recal.)        — dark orange dotted (optional)
#
# Each source may be supplied as:
#   - a file path (character) to a CSV with "predicted" and "observed" columns
#   - a data frame already in memory with those columns
#   - NULL / missing (that curve is omitted; at least one must be non-NULL)
#
# Arguments:
#   lookup_source  — path or data frame for the Iannuzzi published lookup
#   recal_source   — path or data frame for the Iannuzzi recalibrated model
#   mfi5_source    — path or data frame for the mFI-5 recalibrated model (optional)
#   vqifs_source   — path or data frame for the sVQI-FS recalibrated model (optional)
#   output_folder  — directory for the output PNG
#   file_name      — output filename (default "calibration_dual.png")
#
# Returns the output file path, or NULL if no source is usable.
# -----------------------------------------------------------------------------
.save_dual_calibration_plot <- function(lookup_source  = NULL,
                                        recal_source   = NULL,
                                        mfi5_source    = NULL,
                                        vqifs_source   = NULL,
                                        output_folder,
                                        file_name = "calibration_dual.png") {

  # Helper: coerce a source to a data frame or return NULL.
  .read_cal <- function(src) {
    if (is.null(src)) return(NULL)
    if (is.data.frame(src)) {
      df <- src
    } else if (is.character(src) && file.exists(src)) {
      df <- read.csv(src, stringsAsFactors = FALSE)
    } else {
      return(NULL)
    }
    if (!all(c("predicted", "observed") %in% names(df))) return(NULL)
    df[, c("predicted", "observed")]
  }

  lookup_df <- .read_cal(lookup_source)
  recal_df  <- .read_cal(recal_source)
  mfi5_df   <- .read_cal(mfi5_source)
  vqifs_df  <- .read_cal(vqifs_source)

  if (is.null(lookup_df) && is.null(recal_df) && is.null(mfi5_df) && is.null(vqifs_df)) return(NULL)

  # Build a long-format data frame so ggplot colour/linetype mapping is simple.
  curve_levels <- c()
  parts        <- list()
  if (!is.null(lookup_df)) {
    lookup_df$model  <- "Iannuzzi (Lookup)"
    parts[["lookup"]] <- lookup_df
    curve_levels      <- c(curve_levels, "Iannuzzi (Lookup)")
  }
  if (!is.null(recal_df)) {
    recal_df$model   <- "Iannuzzi (Recal.)"
    parts[["recal"]]  <- recal_df
    curve_levels      <- c(curve_levels, "Iannuzzi (Recal.)")
  }
  if (!is.null(mfi5_df)) {
    mfi5_df$model    <- "mFI-5 (Recal.)"
    parts[["mfi5"]]   <- mfi5_df
    curve_levels      <- c(curve_levels, "mFI-5 (Recal.)")
  }
  if (!is.null(vqifs_df)) {
    vqifs_df$model   <- "sVQI-FS (Recal.)"
    parts[["vqifs"]]  <- vqifs_df
    curve_levels      <- c(curve_levels, "sVQI-FS (Recal.)")
  }
  cal_long       <- do.call(rbind, parts)
  cal_long$model <- factor(cal_long$model, levels = curve_levels)

  # Colour palette: dark blue, dark red, dark green, dark orange.
  pal_all       <- c("Iannuzzi (Lookup)"       = "#1F3864",
                     "Iannuzzi (Recal.)" = "#C00000",
                     "mFI-5 (Recal.)"         = "#217346",
                     "sVQI-FS (Recal.)"       = "#E68A1F")
  linetype_all  <- c("Iannuzzi (Lookup)"       = "solid",
                     "Iannuzzi (Recal.)" = "longdash",
                     "mFI-5 (Recal.)"         = "dotdash",
                     "sVQI-FS (Recal.)"       = "dotted")
  pal       <- pal_all[curve_levels]
  linetypes <- linetype_all[curve_levels]

  plot_title <- if (length(curve_levels) >= 4)
    "NHD Risk Score Calibration: four model specifications"
  else if (length(curve_levels) >= 3)
    "NHD Risk Score Calibration: three model specifications"
  else
    "Iannuzzi 2020: Calibration (lookup vs. recalibration)"

  p <- ggplot2::ggplot(cal_long,
         ggplot2::aes(x = predicted, y = observed,
                      colour = model, linetype = model)) +
    ggplot2::geom_point(size = 2) +
    ggplot2::geom_line() +
    ggplot2::geom_abline(intercept = 0, slope = 1,
                         linetype = "dotted", colour = "grey50") +
    ggplot2::scale_colour_manual(values = pal) +
    ggplot2::scale_linetype_manual(values = linetypes) +
    ggplot2::labs(
      title    = plot_title,
      x        = "Mean predicted NHD risk",
      y        = "Observed NHD rate",
      colour   = NULL,
      linetype = NULL
    ) +
    ggplot2::annotate("text",
                      x = 0.82, y = 0.10,
                      label    = "Overestimates risk",
                      hjust    = 1,
                      size     = 3,
                      colour   = "grey40",
                      fontface = "italic") +
    ggplot2::annotate("text",
                      x = 0.10, y = 0.82,
                      label    = "Underestimates risk",
                      hjust    = 0,
                      size     = 3,
                      colour   = "grey40",
                      fontface = "italic") +
    ggplot2::scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
    ggplot2::scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
    ggplot2::coord_equal() +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom",
                   legend.text     = ggplot2::element_text(size = 8))

  out_file <- file.path(output_folder, file_name)
  ggplot2::ggsave(out_file, p, width = 5.5, height = 5.5, dpi = 150)
  out_file
}

# -----------------------------------------------------------------------------
# .build_table1()
#
# Formats a data frame as a styled flextable for the Word report.
# Used for Table 2 (score component definitions).
#
# Styling conventions:
#   - Dark blue (#1F3864) header background with white text
#   - Light gray (#BFBFBF) horizontal rules between body rows
#   - Calibri 10pt throughout
#   - Fixed column widths totalling ~7 inches (US letter body width)
#   - Points and Lookback columns center-aligned
# -----------------------------------------------------------------------------
.build_table1 <- function(df) {
  border_h  <- officer::fp_border(color = "#BFBFBF", width = 0.5)
  border_out <- officer::fp_border(color = "#1F3864", width = 1.5)

  ft <- flextable(df) |>
    set_header_labels(
      variable    = "Variable",
      points      = "Points",
      lookback    = "Lookback Window",
      omop_domain = "OMOP Domain",
      derivation  = "OMOP Derivation Method"
    ) |>
    bold(part = "header") |>
    fontsize(size = 10, part = "all") |>
    font(fontname = "Calibri", part = "all") |>
    width(j = "variable",    width = 1.5) |>
    width(j = "points",      width = 0.55) |>
    width(j = "lookback",    width = 0.85) |>
    width(j = "omop_domain", width = 1.1) |>
    width(j = "derivation",  width = 3.0) |>
    align(j = "points",   align = "center", part = "all") |>
    align(j = "lookback", align = "center", part = "all") |>
    bg(part = "header", bg = "#1F3864") |>
    color(part = "header", color = "white") |>
    hline(border = border_h, part = "body") |>
    border_outer(border = border_out, part = "all") |>
    set_table_properties(layout = "fixed") |>
    padding(padding = 4, part = "all")

  ft
}

# -----------------------------------------------------------------------------
# .build_cohort_summary_table()
#
# Builds a flextable summary of overall cohort statistics from the person-level
# scores data frame.  Presented as Table 1 (overall cohort summary) in the Word
# report.
#
# Statistics included:
#   - Total procedures (rows in person_level)
#   - Unique patients
#   - outcome events and incidence rate
#   - Mean total risk score (SD) and median total score (IQR)
# -----------------------------------------------------------------------------
.build_cohort_summary_table <- function(person_level_df) {
  # Build cohort characteristics summary table from person_level scores dataframe
  # Assumes columns: subject_id, outcome, total_score, age (if available)

  border_h  <- officer::fp_border(color = "#BFBFBF", width = 0.5)
  border_out <- officer::fp_border(color = "#1F3864", width = 1.5)

  n_procedures <- nrow(person_level_df)
  n_patients <- length(unique(person_level_df$subject_id))
  n_ssi <- sum(person_level_df$outcome, na.rm = TRUE)
  ssi_rate <- 100 * n_ssi / n_procedures

  # Create summary statistics
  summary_data <- data.frame(
    Characteristic = c(
      "Total number of procedures",
      "Number of unique patients",
      "Number of outcome events",
      "Outcome incidence rate (%)",
      "Mean total score (SD)",
      "Median total score (IQR)"
    ),
    Value = c(
      n_procedures,
      n_patients,
      n_ssi,
      paste0(round(ssi_rate, 1), "%"),
      paste0(
        round(mean(person_level_df$total_score, na.rm = TRUE), 2), " (",
        round(sd(person_level_df$total_score, na.rm = TRUE), 2), ")"
      ),
      paste0(
        round(median(person_level_df$total_score, na.rm = TRUE), 2), " (",
        round(quantile(person_level_df$total_score, 0.25, na.rm = TRUE), 2), " – ",
        round(quantile(person_level_df$total_score, 0.75, na.rm = TRUE), 2), ")"
      )
    ),
    stringsAsFactors = FALSE
  )

  ft <- flextable(summary_data) |>
    set_header_labels(
      Characteristic = "Characteristic",
      Value = "Value"
    ) |>
    bold(part = "header") |>
    fontsize(size = 10, part = "all") |>
    font(fontname = "Calibri", part = "all") |>
    width(j = "Characteristic", width = 3.0) |>
    width(j = "Value", width = 2.0) |>
    bg(part = "header", bg = "#1F3864") |>
    color(part = "header", color = "white") |>
    hline(border = border_h, part = "body") |>
    border_outer(border = border_out, part = "all") |>
    padding(padding = 4, part = "all")

  ft
}


# -----------------------------------------------------------------------------
# .strip_heading_autonumbering()
#
# Removes the Word template's automatic multilevel heading numbering from a
# generated .docx file.
#
# WHY: officer's default reference template attaches a numbering definition
# (numId = 3) to its heading styles — Titre1 -> ilvl 0, Titre2 -> ilvl 1,
# Titre3 -> ilvl 2 (the styles officer exposes as "heading 1/2/3"; the default
# template ships with French style IDs). This report only ever uses heading
# levels 2 and 3, so Word renders the unused level 1 as a leading counter and
# the section titles come out double-numbered:
#
#     "1.1.1.  1.1. Data source"
#      ^^^^^^   ^^^^ our own number, from section_num()
#      Word's automatic numbering
#
# Section numbers are generated explicitly by section_major()/section_num() in
# R/report_prognostic.R so that they stay in render order. This function removes
# the competing automatic numbering by stripping the <w:numPr> block from the
# heading style definitions in word/styles.xml.
#
# Operates on the .docx in place (a .docx is a zip archive). Any failure is
# non-fatal: the original file is left untouched and a message is emitted, since
# a double-numbered report is still a readable report.
#
# Parameters:
#   docx_path — path to the .docx file to rewrite in place
# Returns docx_path invisibly.
# -----------------------------------------------------------------------------
.strip_heading_autonumbering <- function(docx_path) {
  if (!file.exists(docx_path)) return(invisible(docx_path))

  work_dir <- tempfile("docx_renumber_")
  dir.create(work_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(work_dir, recursive = TRUE, force = TRUE), add = TRUE)

  ok <- tryCatch({
    utils::unzip(docx_path, exdir = work_dir)
    styles_path <- file.path(work_dir, "word", "styles.xml")
    if (!file.exists(styles_path)) stop("word/styles.xml not found in archive")

    xml <- paste(readLines(styles_path, warn = FALSE, encoding = "UTF-8"),
                 collapse = "\n")

    # Heading style IDs differ by template locale: Titre* (officer's default
    # French template) and Heading* / heading* (English templates).
    heading_ids <- c("Titre1", "Titre2", "Titre3",
                     "Heading1", "Heading2", "Heading3",
                     "heading1", "heading2", "heading3")

    n_stripped <- 0L
    for (sid in heading_ids) {
      # Match the full <w:style ...styleId="sid"> ... </w:style> block, then
      # drop any <w:numPr>...</w:numPr> inside it. Non-greedy so adjacent style
      # definitions are not swallowed.
      pattern <- paste0("(<w:style[^>]*w:styleId=\"", sid, "\".*?</w:style>)")
      m <- regmatches(xml, regexpr(pattern, xml, perl = TRUE))
      if (length(m) == 0 || !grepl("<w:numPr>", m[1], fixed = TRUE)) next
      replacement <- gsub("<w:numPr>.*?</w:numPr>", "", m[1], perl = TRUE)
      xml <- sub(pattern, replacement, xml, perl = TRUE)
      n_stripped <- n_stripped + 1L
    }

    if (n_stripped == 0L) {
      message("[report] No heading auto-numbering found to strip.")
      return(TRUE)
    }

    writeLines(xml, styles_path, useBytes = TRUE)

    # Rezip. utils::zip() needs to run from the archive root so the stored
    # paths stay relative (word/..., _rels/..., [Content_Types].xml).
    old_wd <- setwd(work_dir)
    on.exit(setwd(old_wd), add = TRUE)
    entries <- list.files(".", recursive = TRUE, all.files = TRUE, no.. = TRUE)
    rc <- utils::zip(zipfile = "rebuilt.docx", files = entries, flags = "-q -X")
    setwd(old_wd)
    if (rc != 0) stop("zip returned status ", rc)

    file.copy(file.path(work_dir, "rebuilt.docx"), docx_path, overwrite = TRUE)
    message("[report] Stripped template heading auto-numbering from ",
            n_stripped, " heading style(s).")
    TRUE
  }, error = function(e) {
    message("[report] Could not strip heading auto-numbering (non-fatal): ",
            conditionMessage(e))
    FALSE
  })

  invisible(docx_path)
}
