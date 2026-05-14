# plot method for the csem class, plus the package colour palette.
#
# plot.csem renders Brennan-style plots of the per-person conditional SEM
# against the observed score, in base graphics, aiming at visual parity
# with the gtcsem_plot Stata command (mini-spec v1.1 section 6.3).
#
# This file covers sub-phase 3.5: the plot.csem dispatcher, the package
# palette (csem_palette(), exported) and its theme resolver, the
# column/series resolver, and the single-panel layout .plot_csem_single()
# for plot_type = "csem". The confidence-band layers (plot_type "ci" /
# "both") and the side-by-side and compare layouts are added by later
# sub-phases; until then the dispatcher stops with an explicit message on
# those branches.
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
#' the point-estimate and smoothed columns, a display label, a short
#' label for legends, and the palette colour.
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
      key        = key,
      csem_col   = csem_col,
      smooth_col = paste0("smoothed_csem.", key),
      label      = meta[[key]]$label,
      short      = meta[[key]]$short,
      color      = unname(pal[key])
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


#' Draw a single-panel CSEM plot
#'
#' Implements the single-panel layout of [plot.csem()] for
#' `plot_type = "csem"`: a per-person scatter of the conditional SEM
#' against the observed score, with an optional quadratic-smoother curve.
#' The scatter is drawn from the per-person `$estimates` table; the
#' smoother curve from the score-level `$by_score` table.
#'
#' @param x A `csem` object.
#' @param series A single series descriptor from [.resolve_plot_columns()].
#' @param theme_settings A list from [.resolve_plot_theme()].
#' @param show_smooth Logical; draw the smoother curve when available.
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
                              col, pch, cex, lwd, lty, alpha,
                              main, sub, xlab, ylab, ylim, xlim, add, ...) {

  est <- x$estimates
  bs  <- x$by_score[order(x$by_score$observed_score), ]

  series_col <- col %||% series$color
  cex        <- if (is.null(cex)) 0.7 else cex

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

    y_all <- c(yv, if (has_smooth) sm_y else NULL)
    if (is.null(ylim)) ylim <- c(0, max(y_all, na.rm = TRUE) * 1.05)
    if (is.null(xlim)) xlim <- range(xv, na.rm = TRUE)

    op <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(op))
    graphics::par(mar = c(5, 5, 4, 2), mgp = c(2.7, 0.7, 0),
                  tcl = -0.3, las = 1)

    plot(NULL, xlim = xlim, ylim = ylim, xlab = xlab, ylab = ylab,
         main = main, sub = sub, bty = "l", ...)

    # Faint horizontal grid, drawn behind the data.
    graphics::abline(h = graphics::axTicks(2),
                     col = theme_settings$grid, lwd = 0.5)
  }

  graphics::points(xv, yv, pch = pch, cex = cex,
                   col = grDevices::adjustcolor(series_col, alpha.f = alpha))

  if (has_smooth) {
    graphics::lines(sm_x, sm_y, col = series_col, lwd = lwd, lty = lty)
  }

  invisible(NULL)
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
#' @param x A `csem` object.
#' @param plot_type One of `"csem"` (the per-person scatter, the
#'   default), `"ci"` (confidence bands only), or `"both"`.
#' @param error_types Character vector selecting the error type(s) to
#'   plot: `"absolute"`, `"relative"`, or both. Defaults to the error
#'   types carried by `x`.
#' @param method Relative-error estimator to plot when `error_types`
#'   includes `"relative"`: `"full"`, `"large_a"`, or `"uncorrelated"`.
#'   Defaults to the first method carried by `x`.
#' @param show_smooth Logical; overlay the quadratic-smoother curve when
#'   it is available. Defaults to `TRUE`.
#' @param compare_methods Logical; overlay the three relative-error
#'   estimators on one panel. Defaults to `FALSE`.
#' @param cibands Source of the confidence bands when `plot_type` is
#'   `"ci"` or `"both"`: `"person"` (per-person intervals) or `"model"`
#'   (a band around the quadratic fit).
#' @param asemethod Sampling-variance source for the confidence bands:
#'   `"analytical"` or `"bootstrap"`.
#' @param ci_level Confidence level for the bands. Defaults to the level
#'   stored in `x`.
#' @param theme Plot theme. csemGT ships a single own theme, `"csem"`.
#' @param col Override colour for the plotted series. `NULL` uses the
#'   theme palette.
#' @param pch,cex,lwd,lty Graphical parameters for the scatter points
#'   (`pch`, `cex`) and the smoother curve (`lwd`, `lty`).
#' @param alpha Point transparency in `[0, 1]`. `NULL` calibrates it to
#'   the number of plotted persons.
#' @param main,sub,xlab,ylab Title, subtitle and axis labels. `NULL`
#'   selects a sensible default.
#' @param ylim,xlim Axis limits. `NULL` selects a sensible default.
#' @param add Logical; if `TRUE`, draw onto the current plot instead of
#'   opening a new one.
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
#'
#' @export
plot.csem <- function(x,
                      plot_type       = c("csem", "ci", "both"),
                      error_types     = NULL,
                      method          = NULL,
                      show_smooth     = TRUE,
                      compare_methods = FALSE,
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

  # Deferred-branch guards. The confidence-band layers and the
  # side-by-side and compare layouts are added by later sub-phases of
  # Sprint 3; until then those branches stop with an explicit message
  # rather than silently doing something else.
  if (isTRUE(compare_methods)) {
    stop("compare_methods is not yet available in plot.csem; ",
         "plot a single estimator with error_types and method.",
         call. = FALSE)
  }
  if (length(error_types) == 2L) {
    stop("the side-by-side layout (two error types) is not yet available ",
         "in plot.csem; pass error_types = \"absolute\" or \"relative\".",
         call. = FALSE)
  }
  if (plot_type %in% c("ci", "both")) {
    stop("confidence-band layers (plot_type \"ci\" / \"both\") are not yet ",
         "available in plot.csem; use plot_type = \"csem\".",
         call. = FALSE)
  }

  series <- .resolve_plot_columns(x, error_types, method, compare_methods)

  .plot_csem_single(x, series[[1L]], theme_settings,
                    show_smooth = show_smooth,
                    col = col, pch = pch, cex = cex, lwd = lwd, lty = lty,
                    alpha = alpha, main = main, sub = sub,
                    xlab = xlab, ylab = ylab, ylim = ylim, xlim = xlim,
                    add = add, ...)

  invisible(x)
}
