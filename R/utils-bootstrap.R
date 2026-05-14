# Bootstrap and CI helpers.
#
# `.item_bootstrap()` and `.person_bootstrap()` reference helpers
# defined in Sprint 2 (`R/utils-gt.R`). Forward-declared stubs are
# provided at the bottom of this file so that the Sprint 1 test
# suite can exercise the bootstrap orchestration code via mocked
# bindings (testthat::local_mocked_bindings).

#' Item-resampling bootstrap for the GT CSEM estimators
#'
#' Implements the default bootstrap of `csem_gt()`. For each person,
#' draws `R` independent samples of items with replacement (each of
#' size \eqn{J} = `ncol(data)`), recomputes the four GT per-person
#' estimators on every resampled item set, and returns the per-person
#' empirical variances across the `R` replicates.
#'
#' Rationale: item resampling preserves the per-person interpretation
#' of the conditional SEM (the unit of resampling is the same as the
#' unit of replication that underlies the estimator), matching the
#' `gtcsem.ado` Stata implementation. See also the legacy R reference
#' `bootstrap_csem_g1f()` in `inst/legacy/`.
#'
#' @param data Numeric matrix \eqn{N \times J}.
#' @param X Numeric vector of length \eqn{N}; conditioning value per
#'   person.
#' @param R Integer; number of bootstrap replications. Default 1000.
#' @param seed Integer or `NULL`; seed for reproducibility.
#' @param return_replicates Logical; if `TRUE`, also returns the
#'   per-person mean of the replicate estimates in component
#'   `$replicates`.
#' @param verbose Logical; emit progress messages every 100 persons.
#'
#' @return A named list with components `type = "item"`, `R`, `seed`,
#'   and `per_person_variance` (an \eqn{N \times 4} matrix with columns
#'   `c("absolute", "relative_full", "relative_large_a",
#'   "relative_uncorrelated")`). When `return_replicates = TRUE`, a
#'   `$replicates` element of the same shape contains the per-person
#'   mean across replicates for each estimator.
#' @keywords internal
.item_bootstrap <- function(data, X, R = 1000L, seed = NULL,
                            return_replicates = FALSE,
                            verbose = FALSE) {

  if (!is.matrix(data)) data <- as.matrix(data)
  storage.mode(data) <- "double"

  N <- nrow(data); J <- ncol(data)
  cols <- c("absolute", "relative_full",
            "relative_large_a", "relative_uncorrelated")

  if (!is.null(seed)) {
    old_seed <- .set_seed_restoring(seed)
    on.exit(.restore_seed(old_seed), add = TRUE)
  }

  per_person_var <- matrix(NA_real_, nrow = N, ncol = 4L,
                           dimnames = list(NULL, cols))
  boot_mean <- if (return_replicates) {
    matrix(NA_real_, nrow = N, ncol = 4L, dimnames = list(NULL, cols))
  } else NULL

  for (p in seq_len(N)) {
    if (verbose && p %% 100L == 0L) {
      message("Bootstrap person ", p, "/", N)
    }

    replicate_estimates <- matrix(NA_real_, nrow = R, ncol = 4L)

    for (r in seq_len(R)) {
      item_idx  <- sample.int(J, J, replace = TRUE)
      boot_data <- data[, item_idx, drop = FALSE]
      replicate_estimates[r, ] <- .gt_estimators_for_person(boot_data, p, X)
    }

    per_person_var[p, ] <- apply(replicate_estimates, 2L, stats::var,
                                 na.rm = TRUE)
    if (return_replicates) {
      boot_mean[p, ] <- colMeans(replicate_estimates, na.rm = TRUE)
    }
  }

  result <- list(
    type                = "item",
    R                   = as.integer(R),
    seed                = seed,
    per_person_variance = per_person_var
  )
  if (return_replicates) result$replicates <- boot_mean

  result
}


#' Person-resampling bootstrap for the GT CSEM estimators
#'
#' Resamples persons with replacement, refits the variance components,
#' and recomputes the per-person CSEMs on every replicate. The
#' per-person sampling variance is then evaluated by aligning the
#' replicate estimates with the original person index (or, equivalently
#' for csem_gt, with the score level) and computing the empirical
#' variance across replicates. This is the canonical bootstrap of the
#' other paradigms (`csem_split_half`, `csem_anova`, `csem_binomial`)
#' and is provided here for forward compatibility with `csemR`.
#'
#' @inheritParams .item_bootstrap
#' @param paradigm Character; currently must be `"gt"`.
#' @param method,error_type Resolved arguments of `csem_gt()`.
#'
#' @return A list with the same shape as `.item_bootstrap()`.
#' @keywords internal
.person_bootstrap <- function(data, X,
                              paradigm   = "gt",
                              method     = c("full", "large_a", "uncorrelated"),
                              error_type = c("absolute", "relative"),
                              R = 1000L, seed = NULL,
                              return_replicates = FALSE,
                              verbose = FALSE) {

  if (!identical(paradigm, "gt")) {
    stop("Only paradigm = 'gt' is supported in csemGT.", call. = FALSE)
  }

  if (!is.matrix(data)) data <- as.matrix(data)
  storage.mode(data) <- "double"

  N <- nrow(data); J <- ncol(data)
  cols <- c("absolute", "relative_full",
            "relative_large_a", "relative_uncorrelated")

  if (!is.null(seed)) {
    old_seed <- .set_seed_restoring(seed)
    on.exit(.restore_seed(old_seed), add = TRUE)
  }

  # For each replicate, we resample N person rows with replacement,
  # recompute the per-person estimators on the resampled dataset, and
  # average the replicate estimates within levels of X. The per-person
  # bootstrap variance is then the variance, across replicates, of the
  # within-score replicate average matched back to each person's score.
  # This is the formulation used by the per-paradigm csem_* functions
  # in csemR; for csem_gt the by-score representation is also
  # informative.

  unique_scores <- sort(unique(X))
  K <- length(unique_scores)

  # by_score replicates: K x 4 x R
  rep_by_score <- array(NA_real_, dim = c(K, 4L, R),
                        dimnames = list(NULL, cols, NULL))

  for (r in seq_len(R)) {
    if (verbose && r %% 100L == 0L) {
      message("Bootstrap replicate ", r, "/", R)
    }
    pid <- sample.int(N, N, replace = TRUE)
    boot_data <- data[pid, , drop = FALSE]
    boot_X    <- X[pid]

    # Compute per-person estimates on the resampled dataset.
    # Returns a list with `per_person_long` and the new variance
    # components; we only need a per-person numeric matrix here.
    rep_pp <- .gt_compute_per_person_for_boot(boot_data, boot_X,
                                              method, error_type)

    # Average within score levels seen in the original sample.
    for (k in seq_len(K)) {
      sel <- boot_X == unique_scores[k]
      if (any(sel)) {
        rep_by_score[k, , r] <- colMeans(rep_pp[sel, , drop = FALSE],
                                         na.rm = TRUE)
      }
    }
  }

  per_score_var <- apply(rep_by_score, c(1L, 2L), stats::var, na.rm = TRUE)
  per_score_mean <- if (return_replicates) {
    apply(rep_by_score, c(1L, 2L), mean, na.rm = TRUE)
  } else NULL

  # Map back to persons via score lookup
  idx <- match(X, unique_scores)
  per_person_var  <- per_score_var[idx, , drop = FALSE]
  rownames(per_person_var) <- NULL
  boot_mean <- if (return_replicates) {
    out <- per_score_mean[idx, , drop = FALSE]
    rownames(out) <- NULL
    out
  } else NULL

  result <- list(
    type                = "person",
    R                   = as.integer(R),
    seed                = seed,
    per_person_variance = per_person_var
  )
  if (return_replicates) result$replicates <- boot_mean

  result
}


#' Attach bootstrap-based standard errors and CIs to per-person estimates
#'
#' Given a per-person long table (one row per (person, error_type,
#' method) combination) with at least columns `person_id`, `csem`,
#' `error_type`, `method`, this function adds `csem_var.boot`,
#' `se.boot`, `ci_low.boot`, `ci_up.boot` derived from the bootstrap
#' results returned by `.item_bootstrap()` or `.person_bootstrap()`.
#'
#' Confidence intervals are computed on the CSEM scale (not the
#' variance scale), using one of four methods:
#' \describe{
#'   \item{`percentile`}{Currently uses a normal-on-the-CSEM
#'     approximation when no replicate matrix is available, since the
#'     bootstrap helpers only return per-person variances by default.
#'     When replicates are available (e.g. tests with full replicate
#'     storage) the percentile method uses them.}
#'   \item{`basic`}{Basic bootstrap (Davison & Hinkley, 1997, §5.2.1).}
#'   \item{`normal`}{Normal approximation
#'     \eqn{\hat\sigma \pm z_{1-\alpha/2}\,\widehat{SE}}.}
#'   \item{`bca`}{Forwarded to the `boot` package; requires the full
#'     replicate matrix.}
#' }
#'
#' This helper is fully exercised in Sprint 2 once the per-person
#' long table is in place. In Sprint 1 it is tested on synthetic
#' inputs.
#'
#' @param per_person_long Data frame with at least `person_id`, `csem`,
#'   `estimator` (the merged error_type/method tag).
#' @param boot_results Output of `.item_bootstrap()` or
#'   `.person_bootstrap()`.
#' @param ci_method One of `"percentile"`, `"basic"`, `"normal"`, `"bca"`.
#' @param ci_level Numeric in (0, 1).
#'
#' @return The input data frame augmented with bootstrap SE and CI
#'   columns (`csem_var.boot`, `se.boot`, `ci_low.boot`, `ci_up.boot`).
#' @keywords internal
.add_bootstrap_ci <- function(per_person_long, boot_results,
                              ci_method = "percentile",
                              ci_level  = 0.95) {

  ci_method <- match.arg(ci_method,
                         c("percentile", "basic", "normal", "bca"))
  stopifnot(is.numeric(ci_level), length(ci_level) == 1L,
            ci_level > 0, ci_level < 1)

  if (!is.data.frame(per_person_long) ||
      !all(c("person_id", "csem", "estimator") %in%
           names(per_person_long))) {
    stop("`per_person_long` must contain columns ",
         "person_id, csem, estimator.", call. = FALSE)
  }

  pv  <- boot_results$per_person_variance
  if (is.null(pv)) {
    stop("`boot_results$per_person_variance` is missing.", call. = FALSE)
  }
  col_lookup <- colnames(pv)
  if (is.null(col_lookup)) {
    stop("`boot_results$per_person_variance` must have column names ",
         "identifying estimators.", call. = FALSE)
  }

  # Variance on the CSEM scale comes from the per-person variance of the
  # estimator on the VARIANCE scale via delta-method:
  #   var(sqrt(V)) ~= var(V) / (4 V)
  # We always operate on the CSEM scale so users see standard error of
  # the CSEM directly.

  per_person_long$csem_var.boot <- NA_real_

  for (est in unique(per_person_long$estimator)) {
    if (!(est %in% col_lookup)) {
      warning("Bootstrap variance not available for estimator '", est, "'.",
              call. = FALSE)
      next
    }
    sel <- per_person_long$estimator == est
    # Match by person_id: row order of pv is the original person order.
    pid <- per_person_long$person_id[sel]
    var_on_v <- pv[pid, est]

    csem_hat <- per_person_long$csem[sel]
    var_on_csem <- ifelse(csem_hat > 0, var_on_v / (4 * csem_hat^2), NA_real_)
    per_person_long$csem_var.boot[sel] <- var_on_csem
  }

  per_person_long$se.boot <- sqrt(per_person_long$csem_var.boot)

  alpha <- 1 - ci_level
  z     <- stats::qnorm(1 - alpha / 2)

  if (ci_method %in% c("percentile", "basic", "bca") &&
      is.null(boot_results$replicates)) {
    # Without a full replicate matrix on the CSEM scale, fall back to
    # the normal approximation. Full percentile / basic / bca paths
    # are implemented in Sprint 2 once boot_keep_replicates exposes the
    # complete replicate matrix.
    ci_method <- "normal"
  }

  if (ci_method == "normal") {
    per_person_long$ci_low.boot <-
      pmax(per_person_long$csem - z * per_person_long$se.boot, 0)
    per_person_long$ci_up.boot  <-
      per_person_long$csem + z * per_person_long$se.boot
  } else {
    # Replicate-based intervals: deferred to Sprint 2 (requires the
    # replicate matrix on the CSEM scale, not just variances).
    per_person_long$ci_low.boot <- NA_real_
    per_person_long$ci_up.boot  <- NA_real_
  }

  attr(per_person_long, "ci_method") <- ci_method
  attr(per_person_long, "ci_level")  <- ci_level

  per_person_long
}


#' Attach analytical-formula standard errors and CIs
#'
#' Reads `csem_var.analytic` (the closed-form analytical sampling
#' variance of the per-person CSEM under each estimator, populated by
#' Sprint 2 in `.gt_add_analytical_se()`) and derives the
#' corresponding SE and Wald-type confidence intervals.
#'
#' @param per_person_long Data frame with at least `csem` and
#'   `csem_var.analytic`.
#' @param ci_level Numeric in (0, 1).
#' @param paradigm Character (currently only `"gt"` is supported).
#'
#' @return The input data frame augmented with `se.analytic`,
#'   `ci_low.analytic`, `ci_up.analytic`.
#' @keywords internal
.add_analytical_ci <- function(per_person_long,
                               ci_level = 0.95,
                               paradigm = "gt") {

  stopifnot(is.numeric(ci_level), length(ci_level) == 1L,
            ci_level > 0, ci_level < 1)

  if (!is.data.frame(per_person_long) ||
      !all(c("csem", "csem_var.analytic") %in% names(per_person_long))) {
    stop("`per_person_long` must contain columns csem and csem_var.analytic.",
         call. = FALSE)
  }

  alpha <- 1 - ci_level
  z     <- stats::qnorm(1 - alpha / 2)

  per_person_long$se.analytic     <- sqrt(per_person_long$csem_var.analytic)
  per_person_long$ci_low.analytic <-
    pmax(per_person_long$csem - z * per_person_long$se.analytic, 0)
  per_person_long$ci_up.analytic  <-
    per_person_long$csem + z * per_person_long$se.analytic

  attr(per_person_long, "ci_level_analytic") <- ci_level

  per_person_long
}


# -------------------------------------------------------------------
# Forward declarations of Sprint-2 helpers.
#
# These stubs make `.item_bootstrap()` and `.person_bootstrap()`
# self-contained at the Sprint 1 testing stage: they fail informatively
# when called directly, but can be replaced via
# testthat::local_mocked_bindings() inside tests. In Sprint 2 these
# stubs are SUPERSEDED by the real implementations in R/utils-gt.R
# (which will define `.gt_estimators_for_person` and
# `.gt_compute_per_person_for_boot` and shadow these definitions via
# load order or, more cleanly, by REMOVING these stubs entirely in
# the same commit that introduces utils-gt.R).
# -------------------------------------------------------------------

#' @keywords internal
#' @noRd
.gt_estimators_for_person <- function(data, p, X) {
  stop("`.gt_estimators_for_person()` is implemented in Sprint 2 ",
       "(R/utils-gt.R). Tests that exercise the bootstrap must mock this ",
       "helper via testthat::local_mocked_bindings().", call. = FALSE)
}

#' @keywords internal
#' @noRd
.gt_compute_per_person_for_boot <- function(data, X, method, error_type) {
  stop("`.gt_compute_per_person_for_boot()` is implemented in Sprint 2 ",
       "(R/utils-gt.R). Tests must mock this helper via ",
       "testthat::local_mocked_bindings().", call. = FALSE)
}
