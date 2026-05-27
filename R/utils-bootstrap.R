# Bootstrap and bootstrap-SE helpers.
#
# Sprint 2 sub-fase 4(b) refactor:
# -------------------------------
# - `.add_bootstrap_ci()` and `.add_analytical_ci()` (Sprint 1) are REMOVED.
#   Their responsibilities are now split, per the spec v4 §5.3 step
#   sequence, into single-purpose helpers:
#     * SE attachment  -> `.gt_add_analytical_se()` / `.gt_add_bootstrap_se()`
#     * CI attachment  -> `.gt_add_ci()`
#   `.gt_add_analytical_se()` and `.gt_add_ci()` live in `R/utils-gt.R`;
#   `.gt_add_bootstrap_se()` lives here, next to the bootstrap engines whose
#   output it consumes.
#
# - `.item_bootstrap()` and `.person_bootstrap()` are UNCHANGED from the
#   Sprint 2 sub-fase 3 refactor: they receive the pre-computed
#   variance-component list `vc` and `n_items_D`, and delegate per-person
#   point estimation to `R/utils-gt.R`.
#
# Sprint 2 sub-fase 3 refactor (unchanged, retained for context):
# - `.item_bootstrap()` receives `vc` and `n_items_D` instead of the
#   conditioning vector `X`. Its per-person loop delegates to
#   `.gt_compute_per_person_for_boot()` in `R/utils-gt.R`, vectorized over
#   the B replicates per person — matching the legacy `bootstrap_csem_g1f()`
#   and the Mata implementation in `gtcsem.ado` bit-for-bit under a common
#   seed.
# - `.person_bootstrap()` receives `vc`, `n_items_D` and the original `X`
#   (used for the by-score aggregation). Inside each replicate it recomputes
#   vc on the resampled persons and delegates to `.gt_compute_per_person()`.
# - The two Sprint 1 forward stubs were removed; the real implementations
#   live in `R/utils-gt.R`, loaded ahead of this file by R's alphabetical
#   source order.


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
#' @param vc Output of `.gt_variance_components(data)` — the variance
#'   components and per-person ingredients from the ORIGINAL sample
#'   (NOT from any bootstrap resample). Item bootstrap holds
#'   `sigma2_i` and `b_vec` fixed across replicates; this is the
#'   parity anchor against the legacy and `.ado` implementations.
#' @param n_items_D Positive scalar; D-study number of items.
#'   Defaults to `ncol(data)`.
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
.item_bootstrap <- function(data, vc, n_items_D = NULL,
                            R = 1000L, seed = NULL,
                            return_replicates = FALSE,
                            verbose = FALSE) {

  if (!is.matrix(data)) data <- as.matrix(data)
  storage.mode(data) <- "double"

  N <- vc$N
  J <- vc$J
  if (is.null(n_items_D)) n_items_D <- J

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

    # B x 4 matrix of point estimates across B item-resamples for
    # this focal person. PRNG consumption pattern matches legacy.
    replicate_estimates <- .gt_compute_per_person_for_boot(
      xrow      = data[p, ],
      b_full    = vc$b_vec,
      sigma2_i  = vc$sigma2_i,
      n_items_D = n_items_D,
      N         = N,
      B         = R
    )

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
#' variance across replicates. 
#'
#' @param data Numeric matrix \eqn{N \times J}.
#' @param vc Output of `.gt_variance_components(data)` from the ORIGINAL
#'   sample. Used only for dimensions (N, J) and for fall-back; each
#'   replicate recomputes its own vc internally.
#' @param n_items_D Positive scalar; D-study number of items.
#'   Defaults to `ncol(data)`.
#' @param X Numeric vector of length \eqn{N}; conditioning value per
#'   person, used to aggregate replicate estimates by score level.
#' @param paradigm Character; currently must be `"gt"`.
#' @param method,error_type Resolved arguments of `csem_gt()`.
#'   Accepted for API compatibility with Sprint 1 tests but not used
#'   internally — the helper always returns all four estimators and
#'   downstream filtering is done by the orchestrator.
#' @param R Integer; number of bootstrap replications. Default 1000.
#' @param seed Integer or `NULL`; seed for reproducibility.
#' @param return_replicates Logical; if `TRUE`, also returns the
#'   per-person mean of the replicate estimates in component
#'   `$replicates`.
#' @param verbose Logical.
#'
#' @return A list with the same shape as `.item_bootstrap()` but
#'   `type = "person"`.
#' @keywords internal
.person_bootstrap <- function(data, vc, n_items_D = NULL, X,
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

  N <- vc$N
  J <- vc$J
  if (is.null(n_items_D)) n_items_D <- J

  cols <- c("absolute", "relative_full",
            "relative_large_a", "relative_uncorrelated")

  if (!is.null(seed)) {
    old_seed <- .set_seed_restoring(seed)
    on.exit(.restore_seed(old_seed), add = TRUE)
  }

  # By-score aggregation: each replicate's per-person estimates are
  # averaged within levels of the original conditioning variable X,
  # then variances are taken across replicates per score level, then
  # mapped back to each person via their own X.
  unique_scores <- sort(unique(X))
  K <- length(unique_scores)

  rep_by_score <- array(NA_real_, dim = c(K, 4L, R),
                        dimnames = list(NULL, cols, NULL))

  for (r in seq_len(R)) {
    if (verbose && r %% 100L == 0L) {
      message("Bootstrap replicate ", r, "/", R)
    }
    pid       <- sample.int(N, N, replace = TRUE)
    boot_data <- data[pid, , drop = FALSE]
    boot_X    <- X[pid]

    # Recompute variance components on the resampled sample and obtain
    # the four point estimators per person.
    boot_vc <- .gt_variance_components(boot_data, truncate_vc = FALSE)
    rep_pp  <- .gt_compute_per_person(boot_vc, n_items_D)

    for (k in seq_len(K)) {
      sel <- boot_X == unique_scores[k]
      if (any(sel)) {
        rep_by_score[k, , r] <- colMeans(rep_pp[sel, , drop = FALSE],
                                         na.rm = TRUE)
      }
    }
  }

  per_score_var  <- apply(rep_by_score, c(1L, 2L), stats::var, na.rm = TRUE)
  per_score_mean <- if (return_replicates) {
    apply(rep_by_score, c(1L, 2L), mean, na.rm = TRUE)
  } else NULL

  idx <- match(X, unique_scores)
  per_person_var <- per_score_var[idx, , drop = FALSE]
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


#' Attach bootstrap-based standard errors to the per-person long table
#'
#' Given a per-person long table (long-tidy format, one row per
#' (person, estimator) pair) with at least columns `person_id`,
#' `estimator`, and `csem`, this helper reads the per-person bootstrap
#' variances produced by `.item_bootstrap()` or `.person_bootstrap()`
#' and attaches two columns: `csem_var.boot` and `se.boot`.
#'
#' Scale. The bootstrap engines return `per_person_variance` on the
#' ERROR-VARIANCE scale (the sampling variance of \eqn{\hat V_p}). This
#' helper converts to the CSEM scale via the delta method
#'   \eqn{\mathrm{Var}(\sqrt{V}) \approx \mathrm{Var}(V) / (4 V)},
#' using the per-person point estimate `csem` (which is \eqn{\sqrt{V_p}})
#' as the evaluation point. The result `csem_var.boot` is therefore on
#' the CSEM scale, dimensionally consistent with `se.analytic` from
#' `.gt_add_analytical_se()` and with the Wald intervals built by
#' `.gt_add_ci()`. Rows with `csem <= 0` (or NA) receive NA, since the
#' delta-method denominator is not defined there.
#'
#' This helper performs SE attachment only. Confidence intervals are
#' attached separately by `.gt_add_ci()` (spec v4 §5.3, steps 8 and 9).
#'
#' @param per_person_long Data frame in long-tidy format, with at least
#'   `person_id`, `estimator`, `csem`.
#' @param boot_results Output of `.item_bootstrap()` or
#'   `.person_bootstrap()`; its `per_person_variance` matrix must have
#'   column names identifying the estimators.
#'
#' @return The input data frame augmented with `csem_var.boot` and
#'   `se.boot`. Estimators present in `per_person_long$estimator` but
#'   absent from the bootstrap result trigger a warning and receive NA.
#' @keywords internal
.gt_add_bootstrap_se <- function(per_person_long, boot_results) {

  if (!is.data.frame(per_person_long) ||
      !all(c("person_id", "estimator", "csem") %in%
           names(per_person_long))) {
    stop("`per_person_long` must contain columns ",
         "person_id, estimator, csem.", call. = FALSE)
  }

  pv <- boot_results$per_person_variance
  if (is.null(pv)) {
    stop("`boot_results$per_person_variance` is missing.", call. = FALSE)
  }
  col_lookup <- colnames(pv)
  if (is.null(col_lookup)) {
    stop("`boot_results$per_person_variance` must have column names ",
         "identifying estimators.", call. = FALSE)
  }

  per_person_long$csem_var.boot <- NA_real_

  for (est in unique(per_person_long$estimator)) {
    if (!(est %in% col_lookup)) {
      warning("Bootstrap variance not available for estimator '", est, "'.",
              call. = FALSE)
      next
    }
    sel <- per_person_long$estimator == est
    pid <- per_person_long$person_id[sel]
    var_on_v <- pv[pid, est]

    csem_hat <- per_person_long$csem[sel]
    # Delta method to the CSEM scale.
    var_on_csem <- ifelse(!is.na(csem_hat) & csem_hat > 0,
                          var_on_v / (4 * csem_hat^2),
                          NA_real_)
    per_person_long$csem_var.boot[sel] <- var_on_csem
  }

  per_person_long$se.boot <- sqrt(per_person_long$csem_var.boot)

  per_person_long
}
