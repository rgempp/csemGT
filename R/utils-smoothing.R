# Quadratic smoothing of conditional error variances on observed
# score (Brennan, 2001, p. 162). Polynomial degree is configurable but
# the default (degree = 2) gives the cross-package canonical behavior.

#' Smooth `csem_var.*` columns of a by-score table
#'
#' For each `csem_var.<suffix>` column in `by_score`, fits an ordinary
#' least squares polynomial regression of the variance on the observed
#' score, truncates fitted values at zero, takes the square root, and
#' writes the result back as `smoothed_csem.<suffix>`. Smoother
#' diagnostics (intercept, slope, quadratic coefficient, R^2, RMSE,
#' sample size used for the fit) are returned as an `attr(<>, "smooth_fits")`.
#'
#' The RMSE reported here is the population-style residual standard
#' deviation `sqrt(SSE / N)`, matching the convention used by
#' `gtcsem.ado` (see `_gtcsem_qfit`) rather than the small-sample
#' adjusted `sqrt(SSE / (N - k - 1))` that `summary(lm(.))$sigma`
#' returns. This choice preserves cross-package parity.
#'
#' @param by_score Data frame with at least `observed_score` and one or
#'   more `csem_var.<suffix>` columns.
#' @param smoother Character; `"polynomial"` (default) or `"none"`. The
#'   `"none"` value returns the input unchanged.
#' @param smoother_args Named list. The element `degree` controls the
#'   polynomial degree (default 2).
#' @param exclude_extremes Logical; if `TRUE`, rows of `by_score` whose
#'   `observed_score` is in `score_extremes` are excluded from the OLS
#'   fit and their smoothed values are set to `NA`.
#' @param score_extremes Numeric vector of scores to exclude when
#'   `exclude_extremes = TRUE`. Typically `c(0, J)`.
#'
#' @return The input `by_score` with new `smoothed_csem.<suffix>`
#'   columns and a `"smooth_fits"` attribute (a named list, one element
#'   per smoothed suffix).
#' @keywords internal
.apply_smoother <- function(by_score,
                            smoother         = "polynomial",
                            smoother_args    = list(degree = 2),
                            exclude_extremes = FALSE,
                            score_extremes   = NULL) {

  if (identical(smoother, "none")) {
    return(by_score)
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
    fit_idx <- !(by_score$observed_score %in% score_extremes)
  } else {
    fit_idx <- rep(TRUE, nrow(by_score))
  }

  ev_cols <- grep("^csem_var\\.", names(by_score), value = TRUE)
  if (length(ev_cols) == 0L) {
    # Nothing to smooth; pass through with empty diagnostics
    attr(by_score, "smooth_fits") <- list()
    return(by_score)
  }

  smooth_fits <- list()

  for (col in ev_cols) {
    suffix <- sub("^csem_var\\.", "", col)

    y <- by_score[[col]]
    x <- by_score$observed_score

    fit_data <- data.frame(y = y[fit_idx], x = x[fit_idx])

    # Build y ~ I(x^1) + I(x^2) + ... + I(x^degree)
    rhs <- paste0("I(x^", seq_len(degree), ")", collapse = " + ")
    fit_formula <- stats::as.formula(paste("y ~", rhs))

    fit  <- stats::lm(fit_formula, data = fit_data)
    pred <- stats::predict(fit, newdata = data.frame(x = x))
    pred <- pmax(pred, 0)                       # variances >= 0
    csem_smooth <- sqrt(pred)

    if (exclude_extremes && !is.null(score_extremes)) {
      excluded_idx <- by_score$observed_score %in% score_extremes
      csem_smooth[excluded_idx] <- NA_real_
    }

    by_score[[paste0("smoothed_csem.", suffix)]] <- csem_smooth

    # RMSE under the gtcsem.ado convention: sqrt(SSE / N)
    sse  <- sum(stats::residuals(fit)^2)
    n_fit <- sum(fit_idx)
    rmse  <- sqrt(sse / n_fit)

    coefs <- stats::coef(fit)
    # Coefficient layout: (Intercept), I(x^1), I(x^2), ...
    smooth_fits[[suffix]] <- list(
      b0   = unname(coefs[1]),
      b1   = if (degree >= 1L) unname(coefs[2]) else NA_real_,
      b2   = if (degree >= 2L) unname(coefs[3]) else NA_real_,
      R2   = summary(fit)$r.squared,
      RMSE = rmse,
      N    = n_fit
    )
  }

  attr(by_score, "smooth_fits") <- smooth_fits
  by_score
}
