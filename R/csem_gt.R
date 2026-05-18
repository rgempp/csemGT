# R/csem_gt.R — public orchestrator for the GT paradigm.
#
# csem_gt() is the single user-facing entry point of csemGT. It is pure
# glue: every numerical operation is delegated to an internal helper whose
# contract is verified by its own test file. The 13-step structure mirrors
# the mini-spec v1.1 §5.3 pseudocode:
#
#    1. validate inputs, resolve categorical arguments
#    2. resolve n_items_D (default = number of observed items)
#    3. variance components                       .gt_variance_components()
#    4. conditioning variable X                   rowMeans() or user vector
#    5. population quantities + reliability        .gt_population_quantities(),
#                                                  .gt_reliability_coefficients()
#    6. per-person point estimates -> long-tidy    .gt_compute_per_person(),
#                                                  .gt_long_format()
#    7. analytical SE                             .gt_add_analytical_se()
#    8. bootstrap                                 .item_bootstrap() /
#                                                  .person_bootstrap() ->
#                                                  .gt_add_bootstrap_se()
#    9. confidence intervals                      .gt_add_ci()
#   10. pivot to wide                             .pivot_to_wide()
#   11. smooth (on the per-person table), collapse .apply_smoother(),
#                                                  .collapse_to_score()
#   12. assemble the S3 object                    new_csem()
#   13. validate + return                        validate_csem()


#' Conditional standard errors of measurement under Generalizability Theory
#'
#' Estimates conditional standard errors of measurement (CSEMs) for the
#' univariate, single-facet, persons-by-items (p x i) crossed design
#' following Brennan (1998). Returns absolute and relative error variances,
#' three estimators of the relative error variance, closed-form analytical
#' sampling variances, optional item-resampling bootstrap, quadratic
#' smoothing, and D-study extrapolation.
#'
#' @param data Matrix or data.frame, N persons by J items, all numeric and
#'   complete (the single-facet crossed design requires balanced data).
#' @param method Character vector; one or more of `"full"`, `"large_a"`,
#'   `"uncorrelated"`. Default: all three. Only applies when `error_type`
#'   includes `"relative"`.
#' @param error_type Character vector; one or both of `"absolute"`,
#'   `"relative"`. Default: both.
#' @param conditioning Either `"total"` (the per-person mean over items,
#'   i.e. `rowMeans(data)`) or a numeric vector of length N specifying an
#'   alternative conditioning variable. Default: `"total"`. In csemGT v1.0
#'   `observed_score` and `conditioning_value` coincide.
#' @param n_items_D Integer; number of items for D-study extrapolation.
#'   Default: the number of observed items in `data`.
#' @param bootstrap Logical; if `TRUE`, a resampling bootstrap is run.
#'   Default: `FALSE`.
#' @param R Integer; number of bootstrap replications. Default: 1000.
#' @param bootstrap_type One of `"item"` (default) or `"person"`. The item
#'   bootstrap is the default following Brennan (1998) and matches the
#'   `gtcsem` Stata package.
#' @param ci_method One of `"percentile"`, `"basic"`, `"normal"`, `"bca"`.
#'   Default: `"percentile"`. In csemGT v1.0 the confidence intervals are
#'   Wald (normal-approximation) intervals; replicate-based methods are
#'   deferred to a later release, and a non-`"normal"` request is satisfied
#'   with the normal interval (the requested and effective methods are both
#'   recorded).
#' @param ci_level Confidence level. Default: 0.95.
#' @param seed Integer or `NULL`; seed for the bootstrap. The bootstrap
#'   engines restore the prior RNG state on exit, so a non-`NULL` seed does
#'   not leak into the caller's session.
#' @param return_analytical Logical; if `TRUE` (default), analytical SEs are
#'   computed even when `bootstrap = TRUE`.
#' @param smoother One of `"polynomial"` (default) or `"none"`.
#' @param smoother_args List of smoother arguments. Default:
#'   `list(degree = 2)`.
#' @param exclude_extremes Logical; if `TRUE`, floor and ceiling cases are
#'   excluded from the smoother fit (their unsmoothed CSEMs remain, but
#'   their `smoothed_csem.*` values are `NA`). Default: `FALSE`.
#' @param cutpoint Numeric or `NULL`; cutpoint lambda for the mastery
#'   dependability coefficient Phi(lambda), on the same mean-per-item scale
#'   as `observed_score`. Default: `NULL` (not computed).
#' @param truncate_vc Logical; truncate negative ANOVA variance components
#'   to zero. Default: `FALSE`.
#' @param truncate_negative_error_var Logical; truncate negative per-person
#'   error variances to zero before taking the CSEM square root.
#'   Default: `FALSE`.
#' @param boot_keep_replicates Logical; retain the bootstrap replicate
#'   summary in the output. Default: `FALSE`.
#' @param na_action One of `"listwise"` (default) or `"fail"`. `"pairwise"`
#'   is not supported for the GT paradigm.
#' @param verbose Logical; emit progress messages during the bootstrap.
#' @param ... Reserved for forward compatibility; currently ignored.
#'
#' @return An object of class `csem`. Key components:
#'   \describe{
#'     \item{`estimates`}{Data frame, one row per person, wide format.
#'       Identifier columns `person_id`, `observed_score`,
#'       `conditioning_value`, `group_size`, `extreme`, followed by
#'       `cov_xim` and the estimation columns `csem.<estimator>`,
#'       `csem_var.<estimator>`, `se.analytic.<estimator>` /
#'       `se.boot.<estimator>`, `ci_low.*` / `ci_up.*`, and
#'       `smoothed_csem.<estimator>`.}
#'     \item{`by_score`}{Data frame, one row per distinct observed score,
#'       the score-level summary of the estimation columns.}
#'     \item{`variance_components`}{ANOVA table, the three variance
#'       components, population-level error variances/SEMs, and the
#'       reliability-like coefficients (`erho2`, `phi`, `phi_lambda`).}
#'     \item{`smooth_fits`}{Per-estimator smoother diagnostics, or `NULL`
#'       when `smoother = "none"`.}
#'     \item{`bootstrap`}{Bootstrap metadata, or `NULL` when
#'       `bootstrap = FALSE`.}
#'   }
#'   See [`new_csem`] for the full structure.
#'
#' @references
#' Brennan, R. L. (1998). Raw-score conditional standard errors of
#'   measurement in generalizability theory. Applied Psychological
#'   Measurement, 22(4), 307-331.
#'
#' @details
#' The analytical standard errors returned in the `se.analytic.*` columns
#' are **original contributions of the csemGT package**; they are not
#' derived in Brennan (1998), which focuses on the point estimators. Users
#' requiring fully verified inferential coverage should prefer the
#' bootstrap standard errors (`bootstrap = TRUE`).
#'
#' @examples
#' set.seed(1)
#' d <- matrix(rbinom(60 * 12, 1, 0.5), nrow = 60)
#' fit <- csem_gt(d, error_type = "absolute")
#' head(fit$estimates)
#' fit$variance_components$reliability_coefficients
#'
#' @export
csem_gt <- function(data,
                    method       = c("full", "large_a", "uncorrelated"),
                    error_type   = c("absolute", "relative"),
                    conditioning = "total",
                    n_items_D    = NULL,
                    bootstrap    = FALSE,
                    R            = 1000L,
                    bootstrap_type = c("item", "person"),
                    ci_method    = c("percentile", "basic", "normal", "bca"),
                    ci_level     = 0.95,
                    seed         = NULL,
                    return_analytical = TRUE,
                    smoother     = c("polynomial", "none"),
                    smoother_args = list(degree = 2),
                    exclude_extremes = FALSE,
                    cutpoint     = NULL,
                    truncate_vc  = FALSE,
                    truncate_negative_error_var = FALSE,
                    boot_keep_replicates = FALSE,
                    na_action    = c("listwise", "fail", "pairwise"),
                    verbose      = FALSE,
                    ...) {

  cl <- match.call()

  # ---------------------------------------------------------------------
  # Step 1 — validate inputs, resolve categorical arguments
  # ---------------------------------------------------------------------
  bootstrap_type <- match.arg(bootstrap_type)
  ci_method      <- match.arg(ci_method)
  smoother       <- match.arg(smoother)
  na_action      <- match.arg(na_action)

  if (!is.list(smoother_args)) {
    stop("`smoother_args` must be a list (e.g. list(degree = 2)).",
         call. = FALSE)
  }

  data <- .validate_data(data, na_action = na_action)

  method <- .validate_method(
    method, valid = c("full", "large_a", "uncorrelated"), paradigm = "gt")
  error_type <- .validate_error_type(
    method, error_type, paradigm = "gt")

  .validate_args(
    ci_level = ci_level, R = R, n_items_D = n_items_D,
    cutpoint = cutpoint, seed = seed, bootstrap = bootstrap,
    return_analytical = return_analytical,
    exclude_extremes = exclude_extremes, truncate_vc = truncate_vc,
    truncate_negative_error_var = truncate_negative_error_var,
    boot_keep_replicates = boot_keep_replicates, verbose = verbose)

  N <- nrow(data)
  J <- ncol(data)

  # ---------------------------------------------------------------------
  # Step 2 — resolve n_items_D
  # ---------------------------------------------------------------------
  if (is.null(n_items_D)) n_items_D <- J
  n_items_D <- as.numeric(n_items_D)

  # ---------------------------------------------------------------------
  # Step 3 — variance components
  # ---------------------------------------------------------------------
  vc <- .gt_variance_components(data, truncate_vc = truncate_vc)

  # ---------------------------------------------------------------------
  # Step 4 — conditioning variable X
  #   conditioning = "total" -> per-person mean over items (mean-per-item
  #   scale), identical to vc$person_mean and to the legacy / .ado
  #   convention. A user-supplied numeric vector is taken verbatim.
  # ---------------------------------------------------------------------
  if (identical(conditioning, "total")) {
    X <- vc$person_mean
  } else {
    if (!is.numeric(conditioning) || length(conditioning) != N) {
      stop("`conditioning` must be \"total\" or a numeric vector of ",
           "length nrow(data) = ", N, ".", call. = FALSE)
    }
    X <- as.numeric(conditioning)
  }

  # ---------------------------------------------------------------------
  # Step 5 — population quantities + reliability coefficients
  # ---------------------------------------------------------------------
  pop_q    <- .gt_population_quantities(vc, n_items_D)
  rel_coef <- .gt_reliability_coefficients(vc, n_items_D, cutpoint = cutpoint)

  # ---------------------------------------------------------------------
  # Step 6 — per-person point estimates -> long-tidy table
  #   estimators_keep is derived from error_type x method:
  #     "absolute" in error_type            -> "absolute"
  #     "relative" in error_type            -> "relative_<method>" for each
  #   Default (both error types, all three methods) -> all four estimators.
  # ---------------------------------------------------------------------
  estimators_keep <- character(0)
  if ("absolute" %in% error_type) {
    estimators_keep <- c(estimators_keep, "absolute")
  }
  if ("relative" %in% error_type) {
    estimators_keep <- c(estimators_keep, paste0("relative_", method))
  }

  pp_matrix <- .gt_compute_per_person(vc, n_items_D)
  per_person_long <- .gt_long_format(
    pp_matrix,
    observed_score              = X,
    cov_x_itemmean              = vc$cov_x_itemmean,
    estimators_keep             = estimators_keep,
    truncate_negative_error_var = truncate_negative_error_var)

  # ---------------------------------------------------------------------
  # Step 7 — analytical SE
  #   Computed whenever return_analytical is TRUE, or whenever no bootstrap
  #   is run (so that the object always carries at least one SE source).
  # ---------------------------------------------------------------------
  if (return_analytical || !bootstrap) {
    per_person_long <- .gt_add_analytical_se(per_person_long, vc, n_items_D)
  }

  # ---------------------------------------------------------------------
  # Step 8 — bootstrap
  # ---------------------------------------------------------------------
  boot_results <- NULL
  if (bootstrap) {
    boot_results <- if (bootstrap_type == "item") {
      .item_bootstrap(
        data, vc, n_items_D = n_items_D, R = R, seed = seed,
        return_replicates = boot_keep_replicates, verbose = verbose)
    } else {
      .person_bootstrap(
        data, vc, n_items_D = n_items_D, X = X, paradigm = "gt",
        method = method, error_type = error_type, R = R, seed = seed,
        return_replicates = boot_keep_replicates, verbose = verbose)
    }
    per_person_long <- .gt_add_bootstrap_se(per_person_long, boot_results)
  }

  # ---------------------------------------------------------------------
  # Step 9 — confidence intervals
  # ---------------------------------------------------------------------
  per_person_long <- .gt_add_ci(
    per_person_long, ci_method = ci_method, ci_level = ci_level,
    bootstrap = bootstrap)

  # ---------------------------------------------------------------------
  # Step 10 — pivot to wide (one row per person)
  # ---------------------------------------------------------------------
  per_person_wide <- .pivot_to_wide(per_person_long, paradigm = "gt")

  # ---------------------------------------------------------------------
  # Step 11 — smooth the per-person error variances, then collapse.
  #   The quadratic smoother is fit on the PER-PERSON table (one row per
  #   person, observed scores repeated across persons), matching the
  #   legacy csem_g1f() and the gtcsem .ado, both of which regress the
  #   per-person estimator on the observed score over all N persons
  #   (Brennan, 2001, p. 162). Fitting on the score-collapsed table
  #   instead would silently reweight every score level equally and
  #   break parity for the full / large_a estimators, whose per-person
  #   values vary within a fixed observed score. Smoothing first and
  #   collapsing afterwards also makes .merge_smoothed_to_person()
  #   unnecessary: the smoothed columns are already person-level.
  # ---------------------------------------------------------------------
  score_extremes <- if (exclude_extremes) {
    # A floor/ceiling case is a constant response pattern: every item at
    # the global minimum or maximum, which on the mean-per-item scale
    # maps to an observed score equal to min(data) or max(data). This is
    # gtcsem.ado's definition and the spec v4 {0, J} convention.
    # Conditioning on range(observed_score) instead would wrongly drop
    # the lowest/highest *observed* score even when that person's
    # responses are not degenerate (e.g. 1 of 30 items endorsed).
    range(data, na.rm = TRUE)
  } else {
    NULL
  }
  per_person_wide <- .apply_smoother(
    per_person_wide,
    smoother         = smoother,
    smoother_args    = smoother_args,
    exclude_extremes = exclude_extremes,
    score_extremes   = score_extremes)

  smooth_fits <- attr(per_person_wide, "smooth_fits")
  attr(per_person_wide, "smooth_fits") <- NULL

  # Smoother sample counts (n_floor / n_ceiling / n_fit). .apply_smoother()
  # attaches these whenever it smooths; under smoother = "none" it is a bare
  # passthrough and the attribute is absent, so we substitute the all-NA
  # placeholder to keep reliability_coefficients structurally invariant
  # across smoother settings.
  smoothing_diagnostics <- attr(per_person_wide, "smoothing_diagnostics")
  attr(per_person_wide, "smoothing_diagnostics") <- NULL
  if (is.null(smoothing_diagnostics)) {
    smoothing_diagnostics <- list(
      n_floor   = NA_integer_,
      n_ceiling = NA_integer_,
      n_fit     = NA_integer_
    )
  }

  by_score_wide <- .collapse_to_score(per_person_wide)

  # ---------------------------------------------------------------------
  # Identifier columns required by validate_csem() and the spec v4 schema.
  #   conditioning_value == observed_score in csemGT v1.0.
  #   group_size = number of persons sharing the focal observed score.
  #   extreme = constant response pattern (observed score at the global
  #     floor or ceiling, i.e. min(data) or max(data)). Same definition
  #     as score_extremes above, so the persons flagged here are exactly
  #     the persons dropped from the smoother fit.
  # ---------------------------------------------------------------------
  os <- per_person_wide$observed_score
  per_person_wide$conditioning_value <- os
  per_person_wide$group_size <- as.integer(stats::ave(os, os, FUN = length))
  data_range <- range(data, na.rm = TRUE)
  per_person_wide$extreme <- !is.na(os) & os %in% data_range

  id_cols    <- c("person_id", "observed_score", "conditioning_value",
                  "group_size", "extreme")
  other_cols <- setdiff(names(per_person_wide), id_cols)
  per_person_wide <- per_person_wide[, c(id_cols, other_cols), drop = FALSE]
  rownames(per_person_wide) <- NULL

  # ---------------------------------------------------------------------
  # Step 12 — assemble the S3 object
  # ---------------------------------------------------------------------
  arguments <- list(
    n_items_D                   = n_items_D,
    bootstrap                   = bootstrap,
    bootstrap_type              = bootstrap_type,
    return_analytical           = return_analytical,
    R                           = as.integer(R),
    seed                        = seed,
    ci_method                   = ci_method,
    ci_level                    = ci_level,
    smoother                    = smoother,
    smoother_args               = smoother_args,
    truncate_vc                 = truncate_vc,
    truncate_negative_error_var = truncate_negative_error_var,
    exclude_extremes            = exclude_extremes,
    cutpoint                    = cutpoint,
    boot_keep_replicates        = boot_keep_replicates,
    na_action                   = na_action,
    conditioning                = if (identical(conditioning, "total")) {
      "total"
    } else {
      "custom"
    })

  variance_components <- list(
    anova_table              = vc$anova_table,
    person                   = vc$sigma2_p,
    item                     = vc$sigma2_i,
    residual                 = vc$sigma2_pi,
    population_quantities    = pop_q,
    reliability_coefficients = rel_coef)
    bootstrap_meta <- if (bootstrap) {
    list(
      type       = boot_results$type,
      R          = boot_results$R,
      seed       = boot_results$seed,
      replicates = if (boot_keep_replicates) boot_results$replicates else NULL)
  } else {
    NULL
  }

  obj <- new_csem(
    estimates           = per_person_wide,
    by_score            = by_score_wide,
    call                = cl,
    paradigm            = "gt",
    methods             = method,
    error_types         = error_type,
    arguments           = arguments,
    variance_components = variance_components,
    smooth_fits         = smooth_fits,
    diagnostics         = smoothing_diagnostics,
    bootstrap           = bootstrap_meta,
    scale_transform     = NULL,
    n_persons           = N,
    n_items             = J)
  # ---------------------------------------------------------------------
  # Step 13 — validate and return
  # ---------------------------------------------------------------------
  validate_csem(obj)
}
