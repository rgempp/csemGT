#' csemGT: Conditional Standard Errors of Measurement in Generalizability Theory
#'
#' Estimates the conditional standard error of measurement (CSEM) within the
#' Generalizability Theory framework for the univariate, single-facet,
#' persons-by-items (p x i) crossed design (Brennan, 1998). Returns
#' person-level CSEM estimates for absolute and relative error variances,
#' three estimators of the relative error variance (`full`, `large_a`,
#' `uncorrelated`), closed-form analytical and item-resampling bootstrap
#' sampling variances, quadratic smoothing of CSEMs on observed score, and
#' D-study extrapolation.
#'
#' @section Main function:
#' The user-facing function is `csem_gt()` (implemented in Sprint 2 of the
#' csemGT pilot).
#'
#' @references
#' Brennan, R. L. (1998). Raw-score conditional standard errors of
#'   measurement in generalizability theory. \emph{Applied Psychological
#'   Measurement}, 22(4), 307-331.
#'
#' Brennan, R. L. (2001). \emph{Generalizability theory}. Springer.
#'
#' @keywords internal
"_PACKAGE"
