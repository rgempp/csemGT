# Quadratic smoothing of conditional error variances on observed
# score (Brennan, 2001, p. 162). Polynomial degree is configurable but
# the default (degree = 2) gives the cross-package canonical behavior.

#' Smooth `csem_var.*` columns of a per-person wide table
#'
#' For each `csem_var.<suffix>` column in `per_person_wide`, fits an ordinary
#' least squares polynomial regression of the variance on the observed
#' score and takes the square root of the fitted values, leaving the
#' smoothed CSEM undefined (`NA`) where the fitted variance is negative,
#' and writes the result back as `smoothed_csem.<suffix>`. Smoother
#' diagnostics (intercept, slope, quadratic coefficient, R^2, RMSE,
#' sample size used for the fit) are returned as an `attr(<>, "smooth_fits")`.
#'
#' The RMSE reported here is the population-style residual standard
#' deviation `sqrt(SSE / N)`, matching the convention used by
#' `gtcsem.ado` (see `_gtcsem_qfit`) rather than the small-sample
#' adjusted `sqrt(SSE / (N - k - 1))` that `summary(lm(.))$sigma`
#' returns. This choice preserves cross-package parity.
#'
#' @param per_person_wide Data frame with at least `observed_score` and one or
#'   more `csem_var.<suffix>` columns.
#' @param smoother Character; `"polynomial"` (default) or `"none"`. The
#'   `"none"` value returns the input unchanged.
#' @param smoother_args Named list. The element `degree` controls the
#'   polynomial degree (default 2).
#' @param exclude_extremes Logical; if `TRUE`, rows of `per_person_wide` whose
#'   `observed_score` is in `score_extremes` are excluded from the OLS
#'   fit and their smoothed values are set to `NA`.
#' @param score_extremes Numeric vector of scores to exclude when
#'   `exclude_extremes = TRUE`. Typically `c(0, J)`.
#'
#' @return The input `per_person_wide` with new `smoothed_csem.<suffix>`
#'   columns and two attributes: `"smooth_fits"` (a named list, one
#'   element per smoothed suffix) and `"smoothing_diagnostics"` (a list
#'   with `n_floor`, `n_ceiling`, `n_fit` — the floor, ceiling, and
#'   fit-sample counts when `exclude_extremes = TRUE`, all `NA` when
#'   extremes are retained). The `smoother = "none"` passthrough sets
#'   neither attribute.
#' @keywords internal
.apply_smoother <- function(per_person_wide,
                            smoother         = "polynomial",
                            smoother_args    = list(degree = 2),
                            exclude_extremes = FALSE,
                            score_extremes   = NULL) {

  if (identical(smoother, "none")) {
    return(per_person_wide)
  }
  if (!identical(smoother, "polynomial")) {
    stop("Smoother '", smoother,
         "' not implemented in csemGT v1.0.", call. = FALSE)
  }

  degree <- smoother_args$degree %||% 2L
  if (!is.numeric(degree) || length(degree) != 1L ||
      degree < 1L || degree != round(degree)) {
    stop("`smoother_args$degree` must be a positive integer.", call. = FALSE)
  }
  degree <- as.integer(degree)

  if (exclude_extremes && !is.null(score_extremes)) {
    fit_idx <- !(per_person_wide$observed_score %in% score_extremes)
  } else {
    fit_idx <- rep(TRUE, nrow(per_person_wide))
  }

  # Floor / ceiling / fit-sample counts for the smoother, mirroring the
  # r(n_floor) / r(n_ceiling) / r(n_fit) scalars of gtcsem.ado. These are
  # surfaced in the csem object under
  # variance_components$reliability_coefficients$smoothing_diagnostics. They
  # are only meaningful when extremes are actually excluded; with
  # exclude_extremes = FALSE every row feeds the fit and the counts are NA.
  if (exclude_extremes && !is.null(score_extremes)) {
    smoothing_diagnostics <- list(
      n_floor   = sum(per_person_wide$observed_score == score_extremes[1L],
                      na.rm = TRUE),
      n_ceiling = sum(per_person_wide$observed_score == score_extremes[2L],
                      na.rm = TRUE),
      n_fit     = sum(fit_idx)
    )
  } else {
    smoothing_diagnostics <- list(
      n_floor   = NA_integer_,
      n_ceiling = NA_integer_,
      n_fit     = NA_integer_
    )
  }

  # Only the four error-variance POINT ESTIMATES are smoothed:
  # csem_var.<estimator> where <estimator> is a single token (no dot).
  # The qualified columns csem_var.analytic.* and csem_var.boot.* are
  # sampling variances of the estimators, not error variances, and may
  # carry NA (e.g. csem_var.analytic.* is NA where the point estimate is
  # <= 0). The [^.]+$ anchor excludes them: estimator names contain no
  # dot, the qualifiers introduce one.
  ev_cols <- grep("^csem_var\\.[^.]+$", names(per_person_wide), value = TRUE)
  if (length(ev_cols) == 0L) {
    # Nothing to smooth; pass through with empty smoother diagnostics but
    # still report the floor/ceiling/fit counts computed above.
    attr(per_person_wide, "smooth_fits")           <- list()
    attr(per_person_wide, "smoothing_diagnostics") <- smoothing_diagnostics
    return(per_person_wide)
  }

  smooth_fits <- list()

  for (col in ev_cols) {
    suffix <- sub("^csem_var\\.", "", col)

    y <- per_person_wide[[col]]
    x <- per_person_wide$observed_score

    fit_data <- data.frame(y = y[fit_idx], x = x[fit_idx])

    # Build y ~ I(x^1) + I(x^2) + ... + I(x^degree)
    rhs <- paste0("I(x^", seq_len(degree), ")", collapse = " + ")
    fit_formula <- stats::as.formula(paste("y ~", rhs))

    fit  <- stats::lm(fit_formula, data = fit_data)
    pred <- stats::predict(fit, newdata = data.frame(x = x))

    # A negative fitted error variance has no real square root: the
    # smoothed CSEM is left undefined (NA) at that score rather than
    # truncated to zero, which would assert perfect precision where the
    # quadratic model yields no usable estimate. This matches the
    # gtcsem.ado convention, cond(ev_sm >= 0, sqrt(ev_sm), .). The
    # explicit index avoids feeding negatives to sqrt() (which would
    # emit a "NaNs produced" warning before the NA mask is applied).
    csem_smooth <- rep(NA_real_, length(pred))
    nonneg <- !is.na(pred) & pred >= 0
    csem_smooth[nonneg] <- sqrt(pred[nonneg])

    if (exclude_extremes && !is.null(score_extremes)) {
      excluded_idx <- per_person_wide$observed_score %in% score_extremes
      csem_smooth[excluded_idx] <- NA_real_
    }

    per_person_wide[[paste0("smoothed_csem.", suffix)]] <- csem_smooth

    # RMSE under the gtcsem.ado convention: sqrt(SSE / N)
    sse  <- sum(stats::residuals(fit)^2)
    n_fit <- sum(fit_idx)
    rmse  <- sqrt(sse / n_fit)

    # R^2 computed directly as 1 - SSE/SST rather than via summary.lm():
    # for a binary item set, csem_var.absolute is an exact quadratic in
    # observed_score, so the OLS fit is numerically perfect and
    # summary.lm() emits an "essentially perfect fit" warning. The direct
    # formula is identical to summary(fit)$r.squared for an
    # intercept-bearing model and is warning-free.
    sst <- sum((fit_data$y - mean(fit_data$y))^2)
    r2  <- if (sst > 0) 1 - sse / sst else NA_real_

    coefs <- stats::coef(fit)
    # Coefficient layout: (Intercept), I(x^1), I(x^2), ...
    smooth_fits[[suffix]] <- list(
      b0   = unname(coefs[1]),
      b1   = if (degree >= 1L) unname(coefs[2]) else NA_real_,
      b2   = if (degree >= 2L) unname(coefs[3]) else NA_real_,
      R2   = r2,
      RMSE = rmse,
      N    = n_fit
    )
  }

  attr(per_person_wide, "smooth_fits")           <- smooth_fits
  attr(per_person_wide, "smoothing_diagnostics") <- smoothing_diagnostics
  per_person_wide
}
