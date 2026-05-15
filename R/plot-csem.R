# plot method for the csem class, plus the package colour palette.
#
# plot.csem renders Brennan-style plots of the per-person conditional SEM
# against the observed score, in base graphics, aiming at visual parity
# with the gtcsem_plot Stata command (mini-spec v1.1 section 6.3).
#
# This file covers sub-phases 3.5, 3.6, 3.7a and 3.7b:
#   3.5  - the plot.csem dispatcher, the package palette (csem_palette(),
#          exported) and its theme resolver, the column/series resolver,
#          and the single-panel layout .plot_csem_single() for
#          plot_type = "csem".
#   3.6  - the confidence-band layers for plot_type = "ci" / "both":
#          .plot_csem_bands() (delta-method bands, sources "person" and
#          "model"), consumed by .plot_csem_single() as a ribbon drawn
#          behind the data.
#   3.7a - the side-by-side layout for two error types:
#          .plot_csem_sidebyside() draws the absolute and relative
#          panels on one shared y-axis.
#   3.7b - the compare layout: .plot_csem_compare() overlays the three
#          relative-error estimators on one panel with a legend.
#
# The per-person scatter is drawn from $estimates -- one point per person
# -- on purpose: persons with the same observed score can carry different
# CSEMs (for the relative_full and relative_large_a estimators, and for
# almost every estimator with non-binary items), and the plot is meant to
# show that within-score spread. For the absolute and uncorrelated
# estimators with binary items there is no within-score variability and
# the cloud collapses onto one point per score, which is correct. The
# smoother curve is drawn from $by_score, one fitted value per score.

# Package palette: one colour per estimator plus a neutral for structural
# elements (grid, guides). Okabe-Ito hues -- colourblind-safe, public
# domain, no dependency.
.csem_palette_values <- c(
  absolute              = "#E69F00",
  relative_full         = "#0072B2",
  relative_large_a      = "#009E73",
  relative_uncorrelated = "#CC79A7",
  structural            = "#BBBBBB"
)


#' The csemGT colour palette
#'
#' Returns the package colour palette: one colour per CSEM estimator
#' (`absolute`, `relative_full`, `relative_large_a`,
#' `relative_uncorrelated`) plus a neutral `structural` colour used for
#' grid lines and other non-data elements. The hues are taken from the
#' Okabe-Ito qualitative palette, which is colourblind-safe. [plot.csem()]
#' uses this palette by default; it is exported so the same colours can
#' be reused elsewhere, for example to keep a manuscript figure
#' consistent with the package plots.
#'
#' @param which Optional character vector of palette entry names to
#'   return. If `NULL` (the default), the full named vector is returned.
#'
#' @return A named character vector of hex colour strings.
#'
#' @seealso [plot.csem()], which uses this palette.
#'
#' @examples
#' csem_palette()
#' csem_palette("relative_full")
#' csem_palette(c("absolute", "relative_full"))
#'
#' @export
csem_palette <- function(which = NULL) {
  if (is.null(which)) {
    return(.csem_palette_values)
  }
  bad <- setdiff(which, names(.csem_palette_values))
  if (length(bad) > 0L) {
    stop("unknown palette entr", if (length(bad) > 1L) "ies" else "y", ": ",
         paste(bad, collapse = ", "), ". Available entries: ",
         paste(names(.csem_palette_values), collapse = ", "), ".",
         call. = FALSE)
  }
  .csem_palette_values[which]
}


#' Resolve a plot theme to its concrete settings
#'
#' Maps a `theme` name to the list of concrete settings consumed by the
#' plotting helpers. csemGT ships a single own theme, `"csem"`; the
#' argument is kept as a vector for forward extension.
#'
#' @param theme Theme name; currently only `"csem"`.
#'
#' @return A list with the palette and the structural (grid) colour.
#'
#' @keywords internal
.resolve_plot_theme <- function(theme = c("csem")) {
  theme <- match.arg(theme)
  pal <- csem_palette()
  list(
    palette = pal,
    grid    = unname(pal["structural"])
  )
}


#' Resolve which estimator series a plot call should draw
#'
#' Translates the `error_types` / `method` / `compare_methods` arguments
#' of [plot.csem()] into a list of series descriptors, one per estimator
#' to be drawn. Each descriptor carries the estimator key, the names of
#' the point-estimate, error-variance, sampling-SE and smoothed columns,
#' a display label, a short label for legends, and the palette colour.
#'
#' The sampling-SE and error-variance column names are constructed but
#' not validated here: a fit may legitimately lack `se.boot.*` (no
#' bootstrap was run), and whether a given band source needs a given
#' column is decided by [.plot_csem_bands()] at band-computation time.
#'
#' @param x A `csem` object.
#' @param error_types Character vector of error types to plot.
#' @param method Relative-error estimator to use for `error_types`
#'   containing `"relative"`.
#' @param compare_methods Logical; if `TRUE`, the three relative-error
#'   estimators are returned regardless of `error_types` and `method`.
#'
#' @return A list of series descriptors (each itself a list).
#'
#' @keywords internal
.resolve_plot_columns <- function(x, error_types, method, compare_methods) {
  pal <- csem_palette()
  meta <- list(
    absolute = list(label = "Absolute conditional SEM",
                    short = "absolute"),
    relative_full = list(label = "Relative conditional SEM",
                         short = "full"),
    relative_large_a = list(label = "Relative conditional SEM",
                            short = "large_a"),
    relative_uncorrelated = list(label = "Relative conditional SEM",
                                 short = "uncorrelated")
  )

  build_series <- function(key) {
    csem_col <- paste0("csem.", key)
    if (!csem_col %in% names(x$estimates)) {
      stop("the fitted object has no column '", csem_col, "'; csem_gt() ",
           "was not called with the matching error_type / method.",
           call. = FALSE)
    }
    list(
      key             = key,
      csem_col        = csem_col,
      csem_var_col    = paste0("csem_var.", key),
      se_analytic_col = paste0("se.analytic.", key),
      se_boot_col     = paste0("se.boot.", key),
      smooth_col      = paste0("smoothed_csem.", key),
      label           = meta[[key]]$label,
      short           = meta[[key]]$short,
      color           = unname(pal[key])
    )
  }

  keys <- if (isTRUE(compare_methods)) {
    c("relative_full", "relative_large_a", "relative_uncorrelated")
  } else {
    vapply(error_types, function(et) {
      if (identical(et, "absolute")) {
        "absolute"
      } else {
        paste0("relative_", method)
      }
    }, character(1))
  }

  lapply(unname(keys), build_series)
}


#' Compute confidence-band vertices for a CSEM plot
#'
#' Builds the ribbon vertices consumed by [.plot_csem_single()] when
#' `plot_type` is `"ci"` or `"both"`. Two band sources are supported,
#' following the `gtcsem_plot` Stata command:
#'
#' * `cibands = "person"` — a band around the by-score CSEM curve,
#'   \eqn{\widehat{csem} \pm z\, \widehat{se}}, where \eqn{\widehat{se}}
#'   is the per-person sampling SE of the CSEM collapsed to the score
#'   level (`se.analytic.*` or `se.boot.*`, selected by `asemethod`).
#'   The lower edge is truncated at zero. The ribbon is restricted to
#'   the non-extreme score range when the fit excluded extremes from
#'   the smoother (mirroring the `keep` filter of `gtcsem_plot`).
#' * `cibands = "model"` — a band around the quadratic smoother. The
#'   per-person error variance is refit on the observed score and its
#'   square by ordinary least squares, over the same non-extreme sample
#'   the smoother used. Because that is the same fit `.apply_smoother()`
#'   performs, the band centre coincides with the stored
#'   `smoothed_csem.*` curve; `predict(se.fit = TRUE)` supplies the SE
#'   of the mean fit, which the delta method converts to the CSEM scale
#'   as \eqn{se.fit / (2\, \widehat{csem})}. `asemethod` is ignored for
#'   this source.
#'
#' @param x A `csem` object.
#' @param series A single series descriptor from [.resolve_plot_columns()].
#' @param cibands Band source: `"person"` or `"model"`.
#' @param asemethod Sampling-SE source for `cibands = "person"`:
#'   `"analytical"` or `"bootstrap"`. Ignored when `cibands = "model"`.
#' @param ci_level Confidence level for the band.
#'
#' @return A list with numeric vectors `x` (the sorted score grid),
#'   `lo` and `hi` (the ribbon edges) and `center` (the curve the band
#'   is built around).
#'
#' @keywords internal
.plot_csem_bands <- function(x, series, cibands, asemethod, ci_level) {

  z <- stats::qnorm(1 - (1 - ci_level) / 2)

  if (identical(cibands, "person")) {
    # Per-person band, collapsed to the score level: csem +/- z * se on
    # the by-score table. asemethod selects the sampling-SE column.
    se_col <- if (identical(asemethod, "analytical")) {
      series$se_analytic_col
    } else {
      series$se_boot_col
    }
    bs <- x$by_score
    if (!se_col %in% names(bs) || all(is.na(bs[[se_col]]))) {
      stop("plot.csem(): cibands = \"person\" with asemethod = \"",
           asemethod, "\" needs the column '", se_col,
           "', but the fitted object does not carry it",
           if (identical(asemethod, "bootstrap"))
             "; re-run csem_gt() with bootstrap = TRUE." else ".",
           call. = FALSE)
    }

    bs <- bs[order(bs$observed_score), , drop = FALSE]

    # Match the ribbon sample to the smoother sample: drop floor/ceiling
    # rows when exclude_extremes was used (smoothed_csem is NA there).
    keep <- if (series$smooth_col %in% names(bs)) {
      !is.na(bs[[series$smooth_col]])
    } else {
      rep(TRUE, nrow(bs))
    }

    csem_bs <- bs[[series$csem_col]][keep]
    se_bs   <- bs[[se_col]][keep]

    return(list(
      x      = bs$observed_score[keep],
      lo     = pmax(0, csem_bs - z * se_bs),
      hi     = csem_bs + z * se_bs,
      center = csem_bs
    ))
  }

  # cibands == "model": quadratic refit of the per-person error variance.
  est <- x$estimates

  # Same sample .apply_smoother() fits on: every person, or the
  # non-extreme persons when the fit excluded extremes from the
  # smoother. lm()'s default na.action drops any person whose error
  # variance is NA, exactly as the smoother does. Fitting on this
  # sample makes the refit coefficients -- hence the band centre --
  # reproduce the stored smoothed_csem.* curve.
  keep <- if (isTRUE(x$arguments$exclude_extremes)) {
    !est$extreme
  } else {
    rep(TRUE, nrow(est))
  }

  fit_df <- data.frame(
    y = est[[series$csem_var_col]][keep],
    x = est$observed_score[keep]
  )

  # Quadratic OLS of the per-person error variance on the observed
  # score. The column space is identical to the one .apply_smoother()
  # fits, so the coefficients -- hence the band centre -- coincide with
  # the stored smoother; predict(se.fit = TRUE) supplies the SE of the
  # mean fit that the smoother does not store. Matches the cibands(model)
  # refit of gtcsem_plot.ado.
  fit  <- stats::lm(y ~ x + I(x^2), data = fit_df)
  score_grid <- sort(unique(fit_df$x))
  pred <- stats::predict(fit, newdata = data.frame(x = score_grid),
                         se.fit = TRUE)

  # predict.lm() names its output by row; strip the names so the band
  # vectors are clean unnamed numerics.
  yhat   <- unname(pred$fit)
  se_fit <- unname(pred$se.fit)

  csem_hat <- sqrt(pmax(yhat, 0))
  se_csem  <- ifelse(csem_hat > 0, se_fit / (2 * csem_hat), NA_real_)

  list(
    x      = score_grid,
    lo     = pmax(0, csem_hat - z * se_csem),
    hi     = csem_hat + z * se_csem,
    center = csem_hat
  )
}


#' Draw a single-panel CSEM plot
#'
#' Implements the single-panel layout of [plot.csem()]: a per-person
#' scatter of the conditional SEM against the observed score, an optional
#' quadratic-smoother curve, and an optional confidence-band ribbon. The
#' scatter is drawn from the per-person `$estimates` table; the smoother
#' curve from the score-level `$by_score` table; the ribbon from the
#' vertices computed by [.plot_csem_bands()].
#'
#' Layering, back to front: faint horizontal grid, the confidence ribbon,
#' the per-person scatter, the smoother curve.
#'
#' @param x A `csem` object.
#' @param series A single series descriptor from [.resolve_plot_columns()].
#' @param theme_settings A list from [.resolve_plot_theme()].
#' @param show_smooth Logical; draw the smoother curve when available.
#' @param plot_type One of `"csem"`, `"ci"`, `"both"`. The scatter is
#'   drawn for `"csem"` and `"both"`; the ribbon for `"ci"` and `"both"`.
#' @param bands A list of ribbon vertices from [.plot_csem_bands()], or
#'   `NULL` when no ribbon is drawn.
#' @param manage_par Logical; if `TRUE` (the default) the helper saves,
#'   sets and restores the graphical parameters (`mar`, `mgp`, `tcl`,
#'   `las`) itself. The side-by-side orchestrator passes `FALSE` so it
#'   can own the layout state (`mfrow`) without the per-panel
#'   save/restore resetting it.
#' @param col,pch,cex,lwd,lty,alpha Graphical overrides; `NULL` means
#'   "use the theme or calibrated default".
#' @param main,sub,xlab,ylab,ylim,xlim Annotation and axis overrides;
#'   `NULL` means "use a sensible default".
#' @param add Logical; if `TRUE`, draw onto the current plot without
#'   opening a new one.
#' @param ... Passed to the underlying `plot()` call.
#'
#' @return `NULL`, invisibly.
#'
#' @keywords internal
.plot_csem_single <- function(x, series, theme_settings, show_smooth,
                              plot_type, bands, manage_par = TRUE,
                              col, pch, cex, lwd, lty, alpha,
                              main, sub, xlab, ylab, ylim, xlim, add, ...) {

  est <- x$estimates
  bs  <- x$by_score[order(x$by_score$observed_score), ]

  series_col   <- col %||% series$color
  cex          <- if (is.null(cex)) 0.7 else cex
  draw_scatter <- plot_type %in% c("csem", "both")

  # Match the scatter sample to the smoother sample, so the cloud and the
  # fitted curve cover the same persons. With exclude_extremes the
  # smoothed column is NA at floor/ceiling; without it, nothing is
  # dropped. (Mirrors the `keep' filter of gtcsem_plot.)
  keep <- rep(TRUE, nrow(est))
  if (isTRUE(show_smooth) && series$smooth_col %in% names(est)) {
    keep <- !is.na(est[[series$smooth_col]])
  }

  xv <- est$observed_score[keep]
  yv <- est[[series$csem_col]][keep]

  # Point transparency, calibrated to the number of plotted persons:
  # near-opaque for small samples, about 0.3 around N = 20000, so the
  # per-person cloud stays legible without saturating. The form is
  # alpha = a * N^(-0.2), with `a' chosen so alpha is about 0.85 at
  # N = 100 and about 0.30 at N = 20000.
  n_pts <- length(xv)
  alpha <- if (is.null(alpha)) {
    max(0.15, min(0.95, 2.135 * n_pts^(-0.2)))
  } else {
    alpha
  }

  # Smoother curve: one fitted value per observed score.
  sm_x <- bs$observed_score
  sm_y <- if (series$smooth_col %in% names(bs)) {
    bs[[series$smooth_col]]
  } else {
    NULL
  }
  has_smooth <- isTRUE(show_smooth) && !is.null(sm_y) && any(!is.na(sm_y))

  if (!isTRUE(add)) {
    main <- main %||% series$label
    xlab <- xlab %||% "Observed score"
    ylab <- ylab %||% "Conditional SEM"

    y_all <- c(if (draw_scatter) yv,
               if (has_smooth) sm_y,
               if (!is.null(bands)) c(bands$lo, bands$hi))
    if (is.null(ylim)) ylim <- c(0, max(y_all, na.rm = TRUE) * 1.05)

    x_all <- c(if (draw_scatter) xv,
               if (has_smooth) sm_x,
               if (!is.null(bands)) bands$x)
    if (is.null(xlim)) xlim <- range(x_all, na.rm = TRUE)

    if (isTRUE(manage_par)) {
      op <- graphics::par(no.readonly = TRUE)
      on.exit(graphics::par(op))
      graphics::par(mar = c(5, 5, 4, 2), mgp = c(2.7, 0.7, 0),
                    tcl = -0.3, las = 1)
    }

    plot(NULL, xlim = xlim, ylim = ylim, xlab = xlab, ylab = ylab,
         main = main, sub = sub, bty = "l", ...)

    # Faint horizontal grid, drawn behind the data.
    graphics::abline(h = graphics::axTicks(2),
                     col = theme_settings$grid, lwd = 0.5)
  }

  # Confidence-band ribbon, drawn behind the scatter and the smoother.
  if (!is.null(bands)) {
    graphics::polygon(
      c(bands$x, rev(bands$x)),
      c(bands$lo, rev(bands$hi)),
      col    = grDevices::adjustcolor(series_col, alpha.f = 0.20),
      border = NA
    )
  }

  if (isTRUE(draw_scatter)) {
    graphics::points(xv, yv, pch = pch, cex = cex,
                     col = grDevices::adjustcolor(series_col, alpha.f = alpha))
  }

  if (has_smooth) {
    graphics::lines(sm_x, sm_y, col = series_col, lwd = lwd, lty = lty)
  }

  invisible(NULL)
}


#' Draw the side-by-side layout for two error types
#'
#' Implements the two-panel layout of [plot.csem()]: when both error
#' types are requested, the absolute and relative CSEMs are drawn as a
#' pair of panels on one shared y-axis, so they are read on a common
#' vertical scale. Each panel is rendered by [.plot_csem_single()] with
#' `manage_par = FALSE`, the orchestrator owning the `mfrow` layout
#' state. Each panel keeps its own series label as its title; a
#' user-supplied `main` is not applied in this layout.
#'
#' @param x A `csem` object.
#' @param series_list A list of two series descriptors from
#'   [.resolve_plot_columns()].
#' @param theme_settings A list from [.resolve_plot_theme()].
#' @param plot_type One of `"csem"`, `"ci"`, `"both"`.
#' @param cibands,asemethod,ci_level Confidence-band controls, passed to
#'   [.plot_csem_bands()] for each panel.
#' @param show_smooth Logical; overlay the smoother curve when available.
#' @param col,pch,cex,lwd,lty,alpha Graphical overrides passed through to
#'   each panel.
#' @param sub,xlab,ylab,xlim Annotation and axis overrides passed through
#'   to each panel. `sub` defaults, when bands are drawn, to the same
#'   confidence-level subtitle on both panels.
#' @param ... Passed to the underlying `plot()` calls.
#'
#' @return The shared y-axis limits, invisibly.
#'
#' @keywords internal
.plot_csem_sidebyside <- function(x, series_list, theme_settings,
                                  plot_type   = "csem",
                                  cibands     = "person",
                                  asemethod   = "analytical",
                                  ci_level    = 0.95,
                                  show_smooth = TRUE,
                                  col = NULL, pch = 16, cex = NULL,
                                  lwd = 2, lty = 1, alpha = NULL,
                                  sub = NULL, xlab = NULL, ylab = NULL,
                                  xlim = NULL, ...) {

  draw_bands <- plot_type %in% c("ci", "both")

  # Confidence-band vertices for each panel.
  bands_list <- lapply(series_list, function(s) {
    if (draw_bands) {
      .plot_csem_bands(x, s, cibands, asemethod, ci_level)
    } else {
      NULL
    }
  })

  # Shared y-axis. The two panels are read on one vertical scale, so the
  # range is the union of what each panel would need: the per-person
  # scatter (when drawn), the smoother curve, and the band upper edge.
  # The scatter maximum is taken over all persons -- the keep filter
  # only drops floor/ceiling cases, whose CSEMs sit at the low end and
  # never set the maximum -- so the keep logic need not be replicated.
  panel_ymax <- function(s, b) {
    vals <- numeric(0)
    if (plot_type %in% c("csem", "both")) {
      vals <- c(vals, x$estimates[[s$csem_col]])
    }
    if (isTRUE(show_smooth) && s$smooth_col %in% names(x$by_score)) {
      vals <- c(vals, x$by_score[[s$smooth_col]])
    }
    if (!is.null(b)) vals <- c(vals, b$hi)
    max(vals, na.rm = TRUE)
  }
  shared_ymax <- max(vapply(
    seq_along(series_list),
    function(i) panel_ymax(series_list[[i]], bands_list[[i]]),
    numeric(1)
  ))
  shared_ylim <- c(0, shared_ymax * 1.05)

  # Default per-panel subtitle when bands are drawn (same text on both).
  if (is.null(sub) && draw_bands) {
    pct <- formatC(ci_level * 100, format = "g")
    sub <- if (identical(cibands, "model")) {
      paste0(pct, "% CI bands around quadratic fit")
    } else {
      paste0(pct, "% CI bands using ", asemethod, " SE")
    }
  }

  # One row of two panels. .plot_csem_single() is called with
  # manage_par = FALSE so it does not run its own par() save/restore,
  # which would reset the mfrow/mfg state between panels.
  op <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(op))
  graphics::par(mfrow = c(1L, 2L), mar = c(5, 5, 4, 2),
                mgp = c(2.7, 0.7, 0), tcl = -0.3, las = 1)

  for (i in seq_along(series_list)) {
    .plot_csem_single(x, series_list[[i]], theme_settings,
                      show_smooth = show_smooth,
                      plot_type = plot_type, bands = bands_list[[i]],
                      manage_par = FALSE,
                      col = col, pch = pch, cex = cex,
                      lwd = lwd, lty = lty, alpha = alpha,
                      main = NULL, sub = sub,
                      xlab = xlab, ylab = ylab,
                      ylim = shared_ylim, xlim = xlim,
                      add = FALSE, ...)
  }

  invisible(shared_ylim)
}


#' Draw the compare layout overlaying the relative estimators
#'
#' Implements the compare layout of [plot.csem()]: the three
#' relative-error estimators (`relative_full`, `relative_large_a`,
#' `relative_uncorrelated`) are overlaid on one panel with a legend, so
#' their smoother curves can be read against each other. By default only
#' the curves are drawn; `compare_points = TRUE` adds the per-person
#' scatter for every estimator.
#'
#' Each estimator keeps its own palette colour, so a user-supplied `col`
#' is not applied here. No confidence ribbon is drawn -- three
#' overlapping ribbons would not be legible -- so `bands` is `NULL`
#' throughout and the inner `plot_type` only toggles the scatter:
#' `"csem"` draws it, `"ci"` (with `bands = NULL`) suppresses both the
#' scatter and the ribbon, leaving the curve alone.
#'
#' @param x A `csem` object.
#' @param series_list A list of three series descriptors from
#'   [.resolve_plot_columns()].
#' @param theme_settings A list from [.resolve_plot_theme()].
#' @param compare_points Logical; also draw the per-person scatter for
#'   each estimator.
#' @param show_smooth Logical; draw the smoother curves.
#' @param pch,cex,lwd,lty,alpha Graphical overrides passed through to
#'   each series.
#' @param main,sub,xlab,ylab,ylim,xlim Annotation and axis overrides.
#'   `main` defaults to the shared `"Relative conditional SEM"` label;
#'   `ylim`, when `NULL`, is shared across the three series.
#' @param ... Passed to the underlying `plot()` call.
#'
#' @return The y-axis limits used, invisibly.
#'
#' @keywords internal
.plot_csem_compare <- function(x, series_list, theme_settings,
                               compare_points = FALSE,
                               show_smooth    = TRUE,
                               pch = 16, cex = NULL, lwd = 2, lty = 1,
                               alpha = NULL,
                               main = NULL, sub = NULL,
                               xlab = NULL, ylab = NULL,
                               ylim = NULL, xlim = NULL, ...) {

  # No ribbon in the compare layout; the inner plot_type only decides
  # whether the per-person scatter is drawn. "csem" draws it; "ci" with
  # bands = NULL draws neither scatter nor ribbon, leaving the curve.
  inner_plot_type <- if (isTRUE(compare_points)) "csem" else "ci"

  # Shared y-axis over the three series: the smoother curves always
  # (when show_smooth), the per-person scatter only when compare_points.
  if (is.null(ylim)) {
    series_ymax <- function(s) {
      vals <- numeric(0)
      if (isTRUE(compare_points)) {
        vals <- c(vals, x$estimates[[s$csem_col]])
      }
      if (isTRUE(show_smooth) && s$smooth_col %in% names(x$by_score)) {
        vals <- c(vals, x$by_score[[s$smooth_col]])
      }
      max(vals, na.rm = TRUE)
    }
    ymax <- max(vapply(series_list, series_ymax, numeric(1)))
    ylim <- c(0, ymax * 1.05)
  }

  # All three relative estimators share the "Relative conditional SEM"
  # label; use it as the default title.
  main <- main %||% series_list[[1L]]$label

  # The first series opens the panel; the rest are overlaid with
  # add = TRUE. Each series is drawn in its own palette colour.
  for (i in seq_along(series_list)) {
    .plot_csem_single(x, series_list[[i]], theme_settings,
                      show_smooth = show_smooth,
                      plot_type = inner_plot_type, bands = NULL,
                      manage_par = TRUE,
                      col = series_list[[i]]$color,
                      pch = pch, cex = cex, lwd = lwd, lty = lty,
                      alpha = alpha,
                      main = main, sub = sub, xlab = xlab, ylab = ylab,
                      ylim = ylim, xlim = xlim,
                      add = (i > 1L), ...)
  }

  # Legend: one entry per estimator, in its palette colour. The point
  # marker is shown in the key only when the scatter is drawn.
  graphics::legend(
    "topright",
    legend = vapply(series_list, `[[`, character(1), "short"),
    col    = vapply(series_list, `[[`, character(1), "color"),
    lwd    = lwd,
    lty    = lty,
    pch    = if (isTRUE(compare_points)) pch else NA,
    bty    = "n"
  )

  invisible(ylim)
}


#' Plot a `csem` object
#'
#' Draws a Brennan-style plot of the per-person conditional standard
#' error of measurement against the observed score, in base graphics,
#' following the layout of the `gtcsem_plot` Stata command. The per-person
#' scatter is drawn from the `$estimates` table -- one point per person,
#' so that the within-score spread of the CSEM is visible -- and the
#' optional quadratic-smoother curve from the `$by_score` table.
#'
#' @details
#' When `plot_type` is `"ci"` or `"both"` a confidence-band ribbon is
#' added. Two band sources are available through `cibands`:
#'
#' * `"person"` (the default) draws \eqn{\widehat{csem} \pm z\,
#'   \widehat{se}} around the by-score CSEM curve, using the per-person
#'   sampling SE of the CSEM. `asemethod` selects whether that SE is the
#'   analytical or the bootstrap one; the bootstrap SE is only available
#'   when `csem_gt()` was run with `bootstrap = TRUE`.
#' * `"model"` draws a band around the quadratic smoother. The
#'   per-person error variance is refit on the observed score and its
#'   square, and the SE of the mean fit is converted to the CSEM scale
#'   by the delta method. `asemethod` is ignored for this source.
#'
#' The confidence level is `ci_level` (defaulting to the level stored in
#' `x`), so a level different from the one used at fitting time can be
#' requested at plot time. The lower edge of every band is truncated at
#' zero.
#'
#' When two error types are requested (`error_types = c("absolute",
#' "relative")`) the two are drawn as a pair of panels sharing one
#' y-axis, so the absolute and relative CSEMs are read on a common
#' scale. Each panel keeps its own title; a user-supplied `main` is not
#' applied in this layout, and `add = TRUE` is not supported.
#'
#' When `compare_methods = TRUE` the three relative-error estimators are
#' overlaid on one panel with a legend, to compare their smoother curves
#' directly. The per-person scatter is omitted by default -- three
#' clouds would not be legible -- and added for every estimator by
#' `compare_points = TRUE`. Confidence bands are not available in this
#' layout, `add = TRUE` is not supported, and `col` is not applied (each
#' estimator keeps its palette colour).
#'
#' @param x A `csem` object.
#' @param plot_type One of `"csem"` (the per-person scatter, the
#'   default), `"ci"` (confidence bands only), or `"both"`. Not used by
#'   the compare layout.
#' @param error_types Character vector selecting the error type(s) to
#'   plot: `"absolute"`, `"relative"`, or both. Two error types are
#'   drawn as a side-by-side pair of panels. Defaults to the error types
#'   carried by `x`.
#' @param method Relative-error estimator to plot when `error_types`
#'   includes `"relative"`: `"full"`, `"large_a"`, or `"uncorrelated"`.
#'   Defaults to the first method carried by `x`.
#' @param show_smooth Logical; overlay the quadratic-smoother curve when
#'   it is available. Defaults to `TRUE`.
#' @param compare_methods Logical; overlay the three relative-error
#'   estimators on one panel with a legend. Defaults to `FALSE`.
#' @param compare_points Logical; in the compare layout
#'   (`compare_methods = TRUE`), also draw the per-person scatter for
#'   each estimator. Defaults to `FALSE`, which overlays the smoother
#'   curves alone.
#' @param cibands Source of the confidence bands when `plot_type` is
#'   `"ci"` or `"both"`: `"person"` (per-person intervals collapsed to
#'   the score level) or `"model"` (a band around the quadratic fit).
#' @param asemethod Sampling-variance source for the `"person"` bands:
#'   `"analytical"` or `"bootstrap"`. Ignored when `cibands = "model"`.
#' @param ci_level Confidence level for the bands. Defaults to the level
#'   stored in `x`.
#' @param theme Plot theme. csemGT ships a single own theme, `"csem"`.
#' @param col Override colour for the plotted series. `NULL` uses the
#'   theme palette. Not applied in the compare layout.
#' @param pch,cex,lwd,lty Graphical parameters for the scatter points
#'   (`pch`, `cex`) and the smoother curve (`lwd`, `lty`).
#' @param alpha Point transparency in `[0, 1]`. `NULL` calibrates it to
#'   the number of plotted persons.
#' @param main,sub,xlab,ylab Title, subtitle and axis labels. `NULL`
#'   selects a sensible default; for `plot_type` `"ci"` / `"both"` the
#'   default subtitle reports the confidence level and band source. In
#'   the side-by-side layout `main` is not applied (each panel keeps its
#'   own title).
#' @param ylim,xlim Axis limits. `NULL` selects a sensible default; the
#'   side-by-side and compare layouts share one `ylim` across series.
#' @param add Logical; if `TRUE`, draw onto the current plot instead of
#'   opening a new one. Not supported with the side-by-side or compare
#'   layouts.
#' @param ... Additional graphical parameters passed to the underlying
#'   `plot()` call.
#'
#' @return `x`, invisibly.
#'
#' @seealso [csem_palette()] for the colours used; [by_score()] and
#'   [as.data.frame.csem()] for the tables the plot is built from.
#'
#' @examples
#' set.seed(1)
#' d <- matrix(rbinom(100 * 14, 1, 0.5), nrow = 100)
#' fit <- csem_gt(d, error_type = "absolute")
#' plot(fit)
#' plot(fit, plot_type = "both")
#'
#' @export
plot.csem <- function(x,
                      plot_type       = c("csem", "ci", "both"),
                      error_types     = NULL,
                      method          = NULL,
                      show_smooth     = TRUE,
                      compare_methods = FALSE,
                      compare_points  = FALSE,
                      cibands         = c("person", "model"),
                      asemethod       = c("analytical", "bootstrap"),
                      ci_level        = NULL,
                      theme           = c("csem"),
                      col             = NULL,
                      pch             = 16,
                      cex             = NULL,
                      lwd             = 2,
                      lty             = 1,
                      alpha           = NULL,
                      main            = NULL,
                      sub             = NULL,
                      xlab            = NULL,
                      ylab            = NULL,
                      ylim            = NULL,
                      xlim            = NULL,
                      add             = FALSE,
                      ...) {

  plot_type <- match.arg(plot_type)
  cibands   <- match.arg(cibands)
  asemethod <- match.arg(asemethod)
  theme     <- match.arg(theme)

  if (is.null(ci_level))    ci_level    <- x$arguments$ci_level
  if (is.null(error_types)) error_types <- x$error_types
  if (is.null(method) && !isTRUE(compare_methods)) {
    method <- x$methods[1L]
  }

  theme_settings <- .resolve_plot_theme(theme)

  series <- .resolve_plot_columns(x, error_types, method, compare_methods)

  # Compare layout: the three relative estimators overlaid on one panel
  # with a legend. Confidence bands are not available here (three
  # overlapping ribbons would not be legible) and add = TRUE is rejected,
  # as the layout opens its own panel.
  if (isTRUE(compare_methods)) {
    if (isTRUE(add)) {
      stop("add = TRUE is not supported with the compare layout; ",
           "plot a single estimator to use add.", call. = FALSE)
    }
    if (plot_type %in% c("ci", "both")) {
      stop("confidence bands are not available with the compare layout; ",
           "three overlapping ribbons would not be legible. Use ",
           "plot_type = \"csem\" (the default).", call. = FALSE)
    }
    if (!isTRUE(show_smooth) && !isTRUE(compare_points)) {
      stop("nothing to draw with compare_methods: set show_smooth = TRUE ",
           "or compare_points = TRUE.", call. = FALSE)
    }
    if (isTRUE(show_smooth) && !isTRUE(compare_points) &&
        is.null(x$smooth_fits)) {
      stop("nothing to draw with compare_methods: this fit has no ",
           "smoother (csem_gt() ran with smoother = \"none\"), so there ",
           "are no curves to overlay. Set compare_points = TRUE to ",
           "overlay the per-person scatter instead.", call. = FALSE)
    }
    .plot_csem_compare(x, series, theme_settings,
                       compare_points = compare_points,
                       show_smooth = show_smooth,
                       pch = pch, cex = cex, lwd = lwd, lty = lty,
                       alpha = alpha, main = main, sub = sub,
                       xlab = xlab, ylab = ylab, ylim = ylim, xlim = xlim,
                       ...)
    return(invisible(x))
  }

  # Side-by-side layout: two error types are drawn as a pair of panels
  # on a shared y-axis. add = TRUE is incompatible with opening a fresh
  # two-panel layout, so it is rejected here.
  if (length(series) == 2L) {
    if (isTRUE(add)) {
      stop("add = TRUE is not supported with the side-by-side layout ",
           "(two error types); plot a single error type to use add.",
           call. = FALSE)
    }
    .plot_csem_sidebyside(x, series, theme_settings,
                          plot_type = plot_type, cibands = cibands,
                          asemethod = asemethod, ci_level = ci_level,
                          show_smooth = show_smooth,
                          col = col, pch = pch, cex = cex,
                          lwd = lwd, lty = lty, alpha = alpha,
                          sub = sub, xlab = xlab, ylab = ylab,
                          xlim = xlim, ...)
    return(invisible(x))
  }

  # Single-panel layout. .plot_csem_bands() returns the ribbon vertices
  # for plot_type "ci" / "both"; for "csem" no band is computed and the
  # scatter is drawn alone. When the user did not set an explicit
  # subtitle, a default one reporting the level and band source is used.
  bands <- NULL
  if (plot_type %in% c("ci", "both")) {
    bands <- .plot_csem_bands(x, series[[1L]], cibands, asemethod, ci_level)
    if (is.null(sub)) {
      pct <- formatC(ci_level * 100, format = "g")
      sub <- if (identical(cibands, "model")) {
        paste0(pct, "% CI bands around quadratic fit")
      } else {
        paste0(pct, "% CI bands using ", asemethod, " SE")
      }
    }
  }

  .plot_csem_single(x, series[[1L]], theme_settings,
                    show_smooth = show_smooth,
                    plot_type = plot_type, bands = bands,
                    col = col, pch = pch, cex = cex, lwd = lwd, lty = lty,
                    alpha = alpha, main = main, sub = sub,
                    xlab = xlab, ylab = ylab, ylim = ylim, xlim = xlim,
                    add = add, ...)

  invisible(x)
}
