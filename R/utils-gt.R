# =============================================================================
# R/utils-gt.R — internal helpers for Generalizability-Theory CSEMs
#
# This file implements the numerical core of `csem_gt()` for the
# univariate single-facet (persons x items, crossed) design of
# Brennan (1998) and Brennan (2001, sec. 5.4.1).
#
# Parity contract
# ---------------
# Every helper here is held to two parity targets:
#
#   1. Against `csem_g1f()` in `inst/legacy/csem_gt_estimation_v3.R` —
#      tolerance 1e-10, on identical inputs (same matrix, same n_items_D,
#      same method).
#   2. Against `gtcsem.ado` reference output (`inst/extdata/gtcsem_reference_results.dta`)
#      — tolerance 1e-6, on the same simulated dataset.
#
# Both targets are enforced by tests in `tests/testthat/test-csem_gt_parity*.R`.
# No helper here should silently round, truncate, or transform its inputs in
# a way that breaks either tolerance.
#
# Style
# -----
# All helpers in this file are internal (`.gt_*`); they are not exported. They
# assume their inputs have already passed `.validate_data()` upstream in
# `csem_gt()`, so they do minimal re-validation. The single exception is
# `.gt_variance_components()`, which is the entry point and re-derives every
# building block from `data`.
# =============================================================================


# -----------------------------------------------------------------------------
# .gt_anova(): ANOVA table for the p x i crossed single-facet design.
#
# Returns a data.frame with columns (source, df, SS, MS) and 3 rows
# (person, item, person:item), matching the legacy `comp$anova` exactly
# and the `r(anova)` matrix of `gtcsem.ado` up to column re-ordering
# (the .ado matrix also carries a sigma2 column; here we return df/SS/MS
# only and let `.gt_variance_components()` glue sigma2 in).
#
# Inputs
#   data : N x J numeric matrix, complete, balanced (already validated).
#
# Algorithm
#   SS_person   = J * sum((person_mean - grand_mean)^2)
#   SS_item     = N * sum((item_mean   - grand_mean)^2)
#   SS_residual = sum((X - person_mean - item_mean + grand_mean)^2)
#   df_person   = N - 1
#   df_item     = J - 1
#   df_residual = (N - 1) * (J - 1)
#   MS_*        = SS_* / df_*
#
# This is the closed-form decomposition Brennan (2001, eq. 3.2 and Table 3.1)
# states for the balanced single-facet crossed design. The legacy R uses the
# same expressions verbatim; gtcsem.ado uses the algebraically equivalent
# accumulation loop. Floating-point differences between the two have been
# observed at ~5e-13 on N = 300, J = 30.
#
# Returns
#   data.frame(source, df, SS, MS), with attributes carrying the per-person /
#   per-item ingredients needed downstream:
#     attr(, "person_mean") : length-N numeric
#     attr(, "item_mean")   : length-J numeric
#     attr(, "grand_mean")  : scalar
#     attr(, "residual_matrix") : N x J numeric (person- and item-centered)
#
# The attributes are an internal optimization to avoid recomputing these
# sweeps twice; they are stripped before the table reaches the user.
# -----------------------------------------------------------------------------
.gt_anova <- function(data) {
  X <- as.matrix(data)
  storage.mode(X) <- "double"
  N <- nrow(X)
  J <- ncol(X)

  person_mean <- rowMeans(X)
  item_mean   <- colMeans(X)
  grand_mean  <- mean(X)

  SS_person <- J * sum((person_mean - grand_mean)^2)
  SS_item   <- N * sum((item_mean   - grand_mean)^2)

  residual_matrix <- X
  residual_matrix <- sweep(residual_matrix, 1L, person_mean, "-")
  residual_matrix <- sweep(residual_matrix, 2L, item_mean,   "-")
  residual_matrix <- residual_matrix + grand_mean
  SS_residual <- sum(residual_matrix * residual_matrix)

  df_person   <- N - 1L
  df_item     <- J - 1L
  df_residual <- (N - 1L) * (J - 1L)

  MS_person   <- SS_person   / df_person
  MS_item     <- SS_item     / df_item
  MS_residual <- SS_residual / df_residual

  out <- data.frame(
    source = c("person", "item", "person:item"),
    df     = c(df_person, df_item, df_residual),
    SS     = c(SS_person, SS_item, SS_residual),
    MS     = c(MS_person, MS_item, MS_residual),
    row.names        = NULL,
    stringsAsFactors = FALSE
  )

  attr(out, "person_mean")     <- person_mean
  attr(out, "item_mean")       <- item_mean
  attr(out, "grand_mean")      <- grand_mean
  attr(out, "residual_matrix") <- residual_matrix

  out
}


# -----------------------------------------------------------------------------
# .gt_variance_components(): full variance-component decomposition
#
# Wraps `.gt_anova()` and adds the three ANOVA variance components plus the
# per-person ingredients that the downstream estimators consume. This is the
# single entry point for the numerical core; the rest of `utils-gt.R` reads
# its output and never re-touches the raw data.
#
# Inputs
#   data        : N x J numeric matrix (already validated).
#   truncate_vc : logical; if TRUE, negative ANOVA components are set to 0
#                 (Brennan 2001 advises truncating, but the legacy default
#                 is FALSE for diagnostic transparency).
#
# Returns a list with:
#   anova_table     : data.frame from .gt_anova(), no attributes.
#   sigma2_p        : scalar; ANOVA estimate of sigma^2(person).
#   sigma2_i        : scalar; ANOVA estimate of sigma^2(item).
#   sigma2_pi       : scalar; ANOVA estimate of sigma^2(person:item).
#                     Equal to MS_residual.
#   person_mean     : length-N numeric vector of row means.
#   item_mean       : length-J numeric vector of column means.
#   grand_mean      : scalar.
#   b_vec           : length-J numeric vector, item_mean - grand_mean.
#                     This is the "b_i" in Brennan eq. 36 and in the analytic
#                     SE formulas of `.gt_analytical_se()`.
#   person_centered : N x J numeric matrix, X - person_mean (row-wise).
#                     Reused by per-person estimators.
#   row_var         : length-N numeric, sample variance per person on (J-1) df.
#                     Equal to `rowSums(person_centered^2) / (J - 1)`.
#   cov_x_itemmean  : length-N numeric, per-person covariance of item scores
#                     with centered item means. Brennan eq. 35 component.
#   N, J            : integers; sample sizes.
#
# Parity targets
#   Against legacy `g1f_components(data, truncate_vc)`: identical attributes
#   `variance_components`, `anova`, plus `person_mean`/`item_mean`/
#   `grand_mean`/`residual_matrix` at tolerance 1e-10.
#   Against `.ado` r(vc), r(anova), r() scalars sigma2_*: 1e-6.
# -----------------------------------------------------------------------------
.gt_variance_components <- function(data, truncate_vc = FALSE) {
  X <- as.matrix(data)
  storage.mode(X) <- "double"
  N <- nrow(X)
  J <- ncol(X)

  if (N < 2L || J < 2L) {
    stop(".gt_variance_components(): need at least 2 persons and 2 items.")
  }

  anova_tbl <- .gt_anova(X)

  MS_person   <- anova_tbl$MS[anova_tbl$source == "person"]
  MS_item     <- anova_tbl$MS[anova_tbl$source == "item"]
  MS_residual <- anova_tbl$MS[anova_tbl$source == "person:item"]

  sigma2_p  <- (MS_person - MS_residual) / J
  sigma2_i  <- (MS_item   - MS_residual) / N
  sigma2_pi <- MS_residual

  if (isTRUE(truncate_vc)) {
    sigma2_p  <- max(sigma2_p,  0)
    sigma2_i  <- max(sigma2_i,  0)
    sigma2_pi <- max(sigma2_pi, 0)
  }

  person_mean <- attr(anova_tbl, "person_mean")
  item_mean   <- attr(anova_tbl, "item_mean")
  grand_mean  <- attr(anova_tbl, "grand_mean")

  # Strip the optimization attributes from the public table.
  attr(anova_tbl, "person_mean")     <- NULL
  attr(anova_tbl, "item_mean")       <- NULL
  attr(anova_tbl, "grand_mean")      <- NULL
  attr(anova_tbl, "residual_matrix") <- NULL

  # Per-person building blocks. These are exactly the quantities that the
  # legacy script computes inside csem_g1f() right after calling
  # g1f_components(); centralizing them here lets the per-person estimators
  # be O(N) and stateless.
  person_centered <- sweep(X, 1L, person_mean, "-")
  b_vec           <- item_mean - grand_mean
  row_var         <- rowSums(person_centered * person_centered) / (J - 1L)
  cov_x_itemmean  <- as.vector(person_centered %*% b_vec) / (J - 1L)

  list(
    anova_table     = anova_tbl,
    sigma2_p        = sigma2_p,
    sigma2_i        = sigma2_i,
    sigma2_pi       = sigma2_pi,
    person_mean     = person_mean,
    item_mean       = item_mean,
    grand_mean      = grand_mean,
    b_vec           = b_vec,
    person_centered = person_centered,
    row_var         = row_var,
    cov_x_itemmean  = cov_x_itemmean,
    N               = N,
    J               = J
  )
}


# =============================================================================
# Part 2 — per-person point estimators (Brennan 1998, eqs. 20, 36, 40, 41)
#
# The four point estimators are pure-function transformations of the per-
# person ingredients computed by `.gt_variance_components()`. They are
# vectorized over their first arguments (so they admit either length-N
# vectors for the main pipeline or length-B vectors during item-resampling
# bootstrap). They never read or write state; they never set seeds; they
# never validate inputs beyond what stops() require.
#
# Naming convention
#   - `row_var`        : per-person sample variance on (J-1) df. The bare
#                        building block before D-study scaling. Equivalent
#                        to `rowSums((X - rowMeans(X))^2) / (J-1)` in the
#                        legacy script.
#   - `abs_ev`         : per-person absolute error variance estimate, i.e.
#                        the output of `.gt_csem_absolute()`. Already
#                        scaled by 1/n_items_D.
#   - `cov_x_itemmean` : per-person covariance between item scores and the
#                        centered item-means vector `b_vec`. This is the
#                        `c_p` term in Brennan eq. 35.
#   - `sigma2_i`       : the ANOVA item variance component (scalar; the
#                        same for every person within a single fit).
#   - `n_items_D`      : the D-study number of items (scalar; defaults to
#                        the observed J upstream in `csem_gt()`).
#   - `N`              : the number of persons (scalar; only needed for
#                        the `full` estimator).
# =============================================================================


# -----------------------------------------------------------------------------
# .gt_csem_absolute(): per-person ABSOLUTE error-variance estimator
#                      (Brennan 1998, eq. 20)
#
# sigmâ^2(Delta_p) = (1 / (D * (J-1))) * sum_i (X_{p,i} - mean_p)^2
#                  = row_var / D
#
# Inputs
#   row_var   : numeric, per-person sample variance on (J-1) df.
#   n_items_D : positive scalar, number of items in the D-study.
#
# Returns a numeric vector of the same length as `row_var`.
#
# Parity
#   Legacy line 754: abs_error_var <- row_var / n_items_D
#   .ado  line 235: `abs_ev' = `rowss' / ((`Iobs' - 1) * `D')   (algebraically
#                   identical: rowss/(Iobs-1) is row_var)
# -----------------------------------------------------------------------------
.gt_csem_absolute <- function(row_var, n_items_D) {
  row_var / n_items_D
}


# -----------------------------------------------------------------------------
# .gt_csem_relative_full(): per-person RELATIVE error-variance estimator
#                           with finite-A correction (Brennan 1998, eq. 36)
#
# sigmâ^2(delta_p)_full = ((N+1)/(N-1)) * abs_ev
#                        + sigma2_i / D
#                        - (N/(N-1)) * (2 * cov_x_itemmean / D)
#
# This is the "full" estimator, which retains the (N+1)/(N-1) correction
# coming from estimating sigma2_i from the same A persons used to evaluate
# the focal person. As N grows, both correction factors -> 1 and the
# estimator converges to `large_a` (eq. 40).
#
# Inputs
#   abs_ev         : numeric, per-person absolute error variance.
#   cov_x_itemmean : numeric (same length), Brennan c_p term.
#   sigma2_i       : scalar, ANOVA item variance component.
#   n_items_D      : positive scalar.
#   N              : integer, number of persons.
#
# Returns a numeric vector of the same length as `abs_ev`.
#
# Parity
#   Legacy lines 186-188 (compute_relative_estimators$full); identical
#   algebra.
#   .ado  lines 239-241; identical algebra modulo `(A+1)/(A-1)` vs `(N+1)/(N-1)`
#                       notation.
# -----------------------------------------------------------------------------
.gt_csem_relative_full <- function(abs_ev, cov_x_itemmean,
                                   sigma2_i, n_items_D, N) {
  ((N + 1L) / (N - 1L)) * abs_ev +
    sigma2_i / n_items_D -
    (N / (N - 1L)) * (2 * cov_x_itemmean / n_items_D)
}


# -----------------------------------------------------------------------------
# .gt_csem_relative_large_a(): per-person RELATIVE error-variance estimator
#                              under the large-A limit (Brennan 1998, eq. 40)
#
# sigmâ^2(delta_p)_LA = abs_ev + sigma2_i / D - 2 * cov_x_itemmean / D
#
# Equivalent to the `full` estimator with both (N+1)/(N-1) and N/(N-1)
# factors set to 1. Numerically very close to `full` whenever N > 200.
#
# Parity
#   Legacy lines 190-192. .ado lines 243-244.
# -----------------------------------------------------------------------------
.gt_csem_relative_large_a <- function(abs_ev, cov_x_itemmean,
                                      sigma2_i, n_items_D) {
  abs_ev + sigma2_i / n_items_D - 2 * cov_x_itemmean / n_items_D
}


# -----------------------------------------------------------------------------
# .gt_csem_relative_uncorrelated(): per-person RELATIVE error-variance
#                                   estimator under sigma(i, r_p) = 0
#                                   (Brennan 1998, eq. 41)
#
# sigmâ^2(delta_p)_unc = abs_ev - sigma2_i / D
#
# Equivalent to the `large_a` estimator with cov_x_itemmean set to zero,
# i.e. assuming the within-person item-by-residual covariance is null
# for the focal person. This estimator is degenerate-to-absolute on the
# binary scale: when items are dichotomous and the global mean equals
# the per-person mean, cov_x_itemmean vanishes and `uncorrelated` reduces
# to a function of observed score only (the Keats-Lord conditional
# variance up to scaling; see test-csem_gt_identity.R for the formal check).
#
# Parity
#   Legacy line 194. .ado lines 246-247.
# -----------------------------------------------------------------------------
.gt_csem_relative_uncorrelated <- function(abs_ev, sigma2_i, n_items_D) {
  abs_ev - sigma2_i / n_items_D
}


# -----------------------------------------------------------------------------
# .gt_estimators_for_person(): all four per-person estimators in one call
#
# This is the canonical entry point for "give me the four per-person point
# estimates given the ingredients". It is vectorized: `row_var` and
# `cov_x_itemmean` can be vectors of any common length L. Returns a named
# list with four length-L numeric components.
#
# The function lives at the bottom of the per-person stack: the bootstrap
# helper `.gt_compute_per_person_for_boot()` calls it L = B times within a
# single person; the main-pipeline helper `.gt_compute_per_person()` calls
# it once with L = N.
#
# Inputs
#   row_var, cov_x_itemmean : numeric, common length L.
#   sigma2_i                : scalar.
#   n_items_D               : positive scalar.
#   N                       : integer, total number of persons in the fit
#                             (NB: this is the N of the data, not L; required
#                             for the `full` estimator's (N+1)/(N-1) factor).
#
# Returns
#   list(absolute, full, large_a, uncorrelated), each a length-L numeric.
# -----------------------------------------------------------------------------
.gt_estimators_for_person <- function(row_var, cov_x_itemmean,
                                      sigma2_i, n_items_D, N) {
  abs_ev <- .gt_csem_absolute(row_var, n_items_D)
  list(
    absolute     = abs_ev,
    full         = .gt_csem_relative_full(abs_ev, cov_x_itemmean,
                                          sigma2_i, n_items_D, N),
    large_a      = .gt_csem_relative_large_a(abs_ev, cov_x_itemmean,
                                             sigma2_i, n_items_D),
    uncorrelated = .gt_csem_relative_uncorrelated(abs_ev,
                                                  sigma2_i, n_items_D)
  )
}


# =============================================================================
# Part 3 — wrappers used by the main pipeline and by the bootstrap
#
# Two helpers, complementary roles:
#
#   .gt_compute_per_person(vc, n_items_D)
#       Returns an N x 4 matrix of per-person POINT estimates of the four
#       error-variance quantities (absolute, full, large_a, uncorrelated)
#       given pre-computed variance components vc. Used by the main pipeline
#       (csem_gt() step 6) and by .person_bootstrap() inside each replicate
#       (after recomputing vc on the resampled persons).
#
#   .gt_compute_per_person_for_boot(xrow, b_full, sigma2_i, n_items_D, N, B)
#       For ONE focal person: draws B independent item resamples and returns
#       a B x 4 matrix of point estimates across replicates. Used by
#       .item_bootstrap() inside its outer loop over persons. The vectorized
#       inner-replicate structure mirrors bootstrap_csem_g1f() in the legacy
#       and the Mata implementation in gtcsem.ado, so PRNG consumption is
#       bit-identical to the legacy under the same set.seed().
#
# The first helper was not in the original spec §5.2 list of utils-gt.R
# helpers, but the analysis of utils-bootstrap.R after Sprint 1 showed that
# .person_bootstrap() needs it as a separate symbol to delegate cleanly to
# csemGT's GT algebra (otherwise the function would duplicate the entire
# estimator chain inside its replicate loop). The expansion is documented
# here and reflected in the package CHANGELOG (Sprint 2 entry).
# =============================================================================


# -----------------------------------------------------------------------------
# .gt_compute_per_person(): point estimates per person, given vc
#
# Inputs
#   vc        : list, output of .gt_variance_components().
#   n_items_D : positive scalar, D-study number of items.
#
# Returns
#   numeric matrix, N rows by 4 columns. Column names:
#     absolute, relative_full, relative_large_a, relative_uncorrelated
#   matching the wide-format spec v4 §6.4 naming (minus the `csem_var.`
#   prefix, which is added by .pivot_to_wide() downstream).
#
# The function is a thin wrapper around .gt_estimators_for_person() applied
# vectorially to the length-N ingredients vc$row_var and vc$cov_x_itemmean.
# It exists so that the main pipeline and the person bootstrap share a
# single, testable code path for "given vc, give me the four per-person
# point estimates".
# -----------------------------------------------------------------------------
.gt_compute_per_person <- function(vc, n_items_D) {
  est <- .gt_estimators_for_person(vc$row_var, vc$cov_x_itemmean,
                                   vc$sigma2_i, n_items_D, vc$N)
  cbind(
    absolute              = est$absolute,
    relative_full         = est$full,
    relative_large_a      = est$large_a,
    relative_uncorrelated = est$uncorrelated
  )
}


# -----------------------------------------------------------------------------
# .gt_compute_per_person_for_boot(): per-person item-resampling bootstrap
#
# For a single focal person, draws B independent item resamples (with
# replacement) and computes the four per-person estimators on every
# resample. Returns a B x 4 matrix; .item_bootstrap() takes the column-wise
# variance to obtain per-person bootstrap variances.
#
# Inputs
#   xrow      : length-J numeric, item scores for the focal person.
#   b_full    : length-J numeric, item_mean - grand_mean from the ORIGINAL
#               sample (NOT the resampled items). Fixed across replicates.
#   sigma2_i  : scalar, ANOVA item variance component from the ORIGINAL
#               sample. Fixed across replicates. (This is the key parity
#               anchor against legacy/.ado: both treat sigma2_i as
#               estimated once from the observed sample, not re-estimated
#               per item resample.)
#   n_items_D : positive scalar.
#   N         : integer, the original-sample N (parameter of the `full`
#               estimator's (N+1)/(N-1) correction; NOT B and not L=J).
#   B         : positive integer, number of replicates for this person.
#
# Returns
#   numeric matrix, B rows by 4 columns. Column names as in
#   .gt_compute_per_person().
#
# Parity
#   Against bootstrap_csem_g1f() in the legacy (legacy lines 402-432):
#   identical PRNG consumption pattern (single sample.int(J, B*J, ...)
#   call per person; identical matrix assembly; identical algebra in
#   .gt_estimators_for_person()). Under the same set.seed() the returned
#   matrices are bit-identical.
# -----------------------------------------------------------------------------
.gt_compute_per_person_for_boot <- function(xrow, b_full,
                                            sigma2_i, n_items_D,
                                            N, B) {
  J <- length(xrow)

  # Single PRNG draw of B*J integer indices in [1, J]. Same pattern as
  # the legacy line 404, so consumption order matches bit-for-bit.
  idx_mat <- matrix(sample.int(J, B * J, replace = TRUE), B, J)

  Xb <- matrix(xrow[idx_mat],   B, J)
  bb <- matrix(b_full[idx_mat], B, J)

  pm_b   <- rowMeans(Xb)
  a_b    <- Xb - pm_b
  row_var_b <- rowSums(a_b * a_b) / (J - 1L)
  cov_b     <- rowSums(a_b * bb)  / (J - 1L)

  est <- .gt_estimators_for_person(row_var_b, cov_b,
                                   sigma2_i, n_items_D, N)
  cbind(
    absolute              = est$absolute,
    relative_full         = est$full,
    relative_large_a      = est$large_a,
    relative_uncorrelated = est$uncorrelated
  )
}

# =============================================================================
# Part 4 — analytical sampling variances and population-level quantities
#
# Three pure-numeric helpers, all consuming the variance-component list `vc`
# from `.gt_variance_components()`:
#
#   .gt_analytical_se(vc, n_items_D)
#       Closed-form sampling variance of each per-person estimator under the
#       Gaussian-residuals-conditional-on-items model. Constant across
#       persons by construction. Returns the variance on the ERROR-VARIANCE
#       scale (NOT the CSEM scale); the delta-method conversion to the CSEM
#       scale is applied later by `.gt_add_analytical_se()` in Part 5.
#
#   .gt_population_quantities(vc, n_items_D)
#       D-study population-level absolute/relative error variances and SEMs
#       (Brennan 2001, eqs. 2.26, 2.27, 2.32, 2.34).
#
#   .gt_reliability_coefficients(vc, n_items_D, cutpoint)
#       Generalizability coefficient E rho^2 (Brennan 2001, eq. 2.40),
#       dependability coefficient Phi (eq. 2.41), and — when a cutpoint is
#       supplied — the mastery-decision dependability coefficient Phi(lambda)
#       (Brennan & Kane 1977; Brennan 2001, eq. 2.55).
#
# Parity note on the analytical SE
# --------------------------------
# As documented in the csem_gt() roxygen and in the companion methodological
# paper (Gempp 2026), the analytical sampling-variance formulas are an
# original contribution of csemGT/gtcsem; they are not derived in Brennan
# (1998), which addresses only the point estimators. Both the legacy R
# (analytic_var_estimators(), exact branch) and gtcsem.ado implement the
# alpha-gamma decomposition reproduced here. The reference is the
# closed-form moment expressions for s_p^2 and c_p under the Gaussian
# single-facet model.
# =============================================================================
 
 
# -----------------------------------------------------------------------------
# .gt_analytical_se(): closed-form sampling variances of the per-person
#                      estimators (error-variance scale, constant across
#                      persons)
#
# Under the model X_{p,i} = mu + alpha_p + beta_i + r_{p,i} with
# r_{p,i} ~ N(0, sigma^2_pi) i.i.d. and CONDITIONAL on the observed item
# difficulties b_i = item_mean_i - grand_mean, the two per-person building
# blocks
#   s_p^2 = (1/(J-1)) sum_i (X_{p,i} - mean_p)^2     [sample variance]
#   c_p   = (1/(J-1)) sum_i (X_{p,i} - mean_p) b_i   [Brennan eq. 35 term]
# have closed-form second moments that do NOT depend on the focal person:
#   var_sp2  = 4 sigma2_pi S_b / (J-1)^2 + 2 sigma2_pi^2 / (J-1)
#   var_cp   =   sigma2_pi S_b / (J-1)^2
#   cov_spcp = 2 sigma2_pi S_b / (J-1)^2
# with S_b = sum_i b_i^2.
#
# Each estimator is a linear form alpha * s_p^2 + gamma * c_p + C, so its
# sampling variance is
#   alpha^2 var_sp2 + gamma^2 var_cp + 2 alpha gamma cov_spcp.
# The (alpha, gamma) pairs:
#   absolute     : alpha = 1/D,                 gamma = 0
#   uncorrelated : alpha = 1/D,                 gamma = 0          (= absolute)
#   large_a      : alpha = 1/D,                 gamma = -2/D
#   full         : alpha = (N+1)/((N-1) D),     gamma = -2N/((N-1) D)
#
# Inputs
#   vc        : list, output of .gt_variance_components().
#   n_items_D : positive scalar, D-study number of items.
#
# Returns
#   list(absolute, full, large_a, uncorrelated), each a single non-negative
#   scalar (the sampling variance of the corresponding estimator on the
#   error-variance scale). Negative values are truncated at 0, matching the
#   legacy and the .ado.
#
# Parity
#   Legacy analytic_var_estimators() exact branch (legacy lines 276-317):
#   identical algebra. .ado lines 268-316: identical, with S_b = SSitem/A.
#   Tolerance 1e-10 vs legacy, 1e-6 vs .ado.
# -----------------------------------------------------------------------------
.gt_analytical_se <- function(vc, n_items_D) {
  N         <- vc$N
  J         <- vc$J
  sigma2_pi <- vc$sigma2_pi
  S_b       <- sum(vc$b_vec^2)
  D         <- n_items_D
  Jm1       <- J - 1L
 
  var_sp2  <- 4 * sigma2_pi * S_b / Jm1^2 + 2 * sigma2_pi^2 / Jm1
  var_cp   <-     sigma2_pi * S_b / Jm1^2
  cov_spcp <- 2 * sigma2_pi * S_b / Jm1^2
 
  # absolute / uncorrelated: alpha = 1/D, gamma = 0
  v_abs <- var_sp2 / D^2
  v_unc <- v_abs
 
  # large_a: alpha = 1/D, gamma = -2/D
  alpha_la <- 1 / D
  gamma_la <- -2 / D
  v_la <- alpha_la^2 * var_sp2 +
          gamma_la^2 * var_cp +
          2 * alpha_la * gamma_la * cov_spcp
 
  # full: alpha = (N+1)/((N-1) D), gamma = -2N/((N-1) D)
  alpha_full <- (N + 1L) / ((N - 1L) * D)
  gamma_full <- -2 * N / ((N - 1L) * D)
  v_full <- alpha_full^2 * var_sp2 +
            gamma_full^2 * var_cp +
            2 * alpha_full * gamma_full * cov_spcp
 
  list(
    absolute     = max(v_abs,  0),
    full         = max(v_full, 0),
    large_a      = max(v_la,   0),
    uncorrelated = max(v_unc,  0)
  )
}
 
 
# -----------------------------------------------------------------------------
# .gt_population_quantities(): D-study population-level error variances
#                              and SEMs
#
# sigma^2(I)     = sigma^2(i) / D                       (Brennan 2001, eq. 2.26)
# sigma^2(pI)    = sigma^2(pi) / D                      (eq. 2.27)
# sigma^2(delta) = sigma^2(pI)                          (eq. 2.34, relative)
# sigma^2(Delta) = sigma^2(I) + sigma^2(pI)             (eq. 2.32, absolute)
#
# Inputs
#   vc        : list, output of .gt_variance_components().
#   n_items_D : positive scalar.
#
# Returns
#   list(absolute_error_var, absolute_sem, relative_error_var, relative_sem),
#   all scalars. SEMs are the square roots of the corresponding error
#   variances.
#
# Parity
#   Legacy lines 899-913 (`overall` data.frame). .ado lines 580-588.
#   Note (s2i + s2pi)/D == s2i/D + s2pi/D, so the legacy's single-division
#   form and the .ado's two-term form are algebraically identical; parity
#   holds at ~1e-13 from FP reordering.
# -----------------------------------------------------------------------------
.gt_population_quantities <- function(vc, n_items_D) {
  abs_ev <- (vc$sigma2_i + vc$sigma2_pi) / n_items_D
  rel_ev <- vc$sigma2_pi / n_items_D
  list(
    absolute_error_var = abs_ev,
    absolute_sem       = sqrt(abs_ev),
    relative_error_var = rel_ev,
    relative_sem       = sqrt(rel_ev)
  )
}
 
 
# -----------------------------------------------------------------------------
# .gt_reliability_coefficients(): generalizability and dependability
#                                 coefficients
#
# E rho^2 = sigma^2(p) / (sigma^2(p) + sigma^2(delta))   (Brennan 2001, eq. 2.40)
# Phi     = sigma^2(p) / (sigma^2(p) + sigma^2(Delta))   (eq. 2.41)
#
# Phi(lambda), the dependability coefficient for mastery decisions at
# cutpoint lambda (Brennan & Kane 1977; Brennan 2001, eq. 2.55):
#   Phi(lambda) = [s2p + (Xbar - lambda)^2 - s2(Xbar)] /
#                 [s2p + (Xbar - lambda)^2 - s2(Xbar) + sigma^2(Delta)]
# where s2(Xbar) = [s2p + sigma^2(I) + sigma^2(pI)] / N    (eq. 2.38)
# and Xbar is the grand mean of the data matrix (mean-per-item scale).
#
# Inputs
#   vc        : list, output of .gt_variance_components().
#   n_items_D : positive scalar.
#   cutpoint  : numeric scalar (the lambda) or NULL. When NULL, phi_lambda
#               is returned as NA_real_.
#
# Returns
#   list(erho2, phi, phi_lambda), all scalars. erho2/phi are NA_real_ when
#   their denominators are not strictly positive; phi_lambda is NA_real_
#   when cutpoint is NULL or its denominator is not strictly positive.
#
# Parity
#   Legacy lines 915-954. .ado lines 590-619. The cutpoint lambda is on the
#   mean-per-item scale (same scale as vc$grand_mean), matching the .ado's
#   use of r(mean) of the per-person row means.
# -----------------------------------------------------------------------------
.gt_reliability_coefficients <- function(vc, n_items_D, cutpoint = NULL) {
  pop      <- .gt_population_quantities(vc, n_items_D)
  sigma2_p <- vc$sigma2_p
 
  erho2 <- if (sigma2_p + pop$relative_error_var > 0) {
    sigma2_p / (sigma2_p + pop$relative_error_var)
  } else {
    NA_real_
  }
 
  phi <- if (sigma2_p + pop$absolute_error_var > 0) {
    sigma2_p / (sigma2_p + pop$absolute_error_var)
  } else {
    NA_real_
  }
 
  phi_lambda <- NA_real_
  if (!is.null(cutpoint)) {
    s2_Xbar <- (sigma2_p +
                vc$sigma2_i  / n_items_D +
                vc$sigma2_pi / n_items_D) / vc$N
    dev2 <- (vc$grand_mean - cutpoint)^2
    num  <- sigma2_p + dev2 - s2_Xbar
    den  <- num + pop$absolute_error_var
    phi_lambda <- if (is.finite(den) && den > 0) num / den else NA_real_
  }
 
  list(erho2 = erho2, phi = phi, phi_lambda = phi_lambda)
}

# =============================================================================
# Part 5 — long-format assembly, analytical SE attachment, CIs, wide pivot
#
# These helpers turn the N x 4 point-estimate matrix from
# .gt_compute_per_person() into the long-format intermediate table that the
# orchestrator threads through steps 6-9, then pivots to the wide table that
# becomes the `$estimates` component of the csem object.
#
# Data-flow contract
# ------------------
#   .gt_compute_per_person(vc, n_items_D)                     -> N x 4 matrix
#   .gt_long_format(matrix, observed_score, cov_x_itemmean,   -> long tidy df
#                   estimators_keep, truncate_negative_error_var)
#   .gt_add_analytical_se(long, vc, n_items_D)                -> long + se cols
#   .gt_add_bootstrap_se(long, boot_results)   [utils-bootstrap.R]
#                                                            -> long + se cols
#   .gt_add_ci(long, ci_method, ci_level, bootstrap)          -> long + ci cols
#   .pivot_to_wide(long, paradigm)                            -> wide df
#
# Long-format intermediate schema
# -------------------------------
#   person_id      : integer 1..N
#   observed_score : numeric, per-person mean (mean-per-item scale)
#   cov_xim        : numeric, per-person cov(item scores, b_vec) — auxiliary,
#                    constant within person; present iff supplied
#   estimator      : character, one of absolute / relative_full /
#                    relative_large_a / relative_uncorrelated
#   csem_var       : numeric, per-person error-variance point estimate
#   csem           : numeric, sqrt(csem_var) with negative handling
#   csem_var.analytic, se.analytic   : added by .gt_add_analytical_se()
#   csem_var.boot,     se.boot       : added by .gt_add_bootstrap_se()
#   ci_low.analytic, ci_up.analytic  : added by .gt_add_ci()
#   ci_low.boot,     ci_up.boot      : added by .gt_add_ci()
#
# All the *.analytic / *.boot variances live on the CSEM scale (the
# delta-method conversion var(sqrt(V)) ~= var(V)/(4 V) is applied inside the
# add_*_se helpers), so the Wald CIs csem +/- z * se are dimensionally
# consistent.
# =============================================================================


# -----------------------------------------------------------------------------
# .gt_sem_sqrt(): square-root with the legacy's negative-variance policy
#
# Mirrors sem_sqrt() inside csem_g1f() (legacy lines 734-744):
#   - truncate_negative = TRUE  : sqrt(pmax(v, 0))   -> 0 for negatives
#   - truncate_negative = FALSE : NaN for negatives, NA preserved for NA
#
# Inputs
#   v                 : numeric vector.
#   truncate_negative : logical.
#
# Returns a numeric vector of the same length as v.
# -----------------------------------------------------------------------------
.gt_sem_sqrt <- function(v, truncate_negative = FALSE) {
  if (isTRUE(truncate_negative)) {
    sqrt(pmax(v, 0))
  } else {
    out <- rep(NaN, length(v))
    ok  <- !is.na(v) & v >= 0
    out[ok] <- sqrt(v[ok])
    out[is.na(v)] <- NA_real_
    out
  }
}


# -----------------------------------------------------------------------------
# .gt_long_format(): melt the N x 4 point-estimate matrix to long-tidy
#
# Inputs
#   pp_matrix                   : N x 4 numeric matrix, columns named
#                                 absolute / relative_full / relative_large_a /
#                                 relative_uncorrelated (output of
#                                 .gt_compute_per_person()).
#   observed_score              : length-N numeric, per-person score.
#   cov_x_itemmean              : length-N numeric or NULL; per-person
#                                 cov(item scores, b_vec), carried into the
#                                 long table as the auxiliary column `cov_xim`.
#   estimators_keep             : character vector, subset of the matrix
#                                 columns to retain (NULL = all). Order
#                                 follows the matrix column order, not the
#                                 order of estimators_keep.
#   truncate_negative_error_var : logical, passed to .gt_sem_sqrt().
#
# Returns
#   data.frame in long-tidy format. Rows are grouped by estimator (all N rows
#   of the first kept estimator, then all N of the second, ...). This row
#   order is irrelevant downstream because .pivot_to_wide() keys on
#   (person_id, estimator); grouping by estimator is simply the natural
#   column-major unrolling of the matrix.
# -----------------------------------------------------------------------------
.gt_long_format <- function(pp_matrix, observed_score,
                            cov_x_itemmean = NULL,
                            estimators_keep = NULL,
                            truncate_negative_error_var = FALSE) {
  N       <- nrow(pp_matrix)
  all_est <- colnames(pp_matrix)
  if (is.null(all_est)) {
    stop(".gt_long_format(): pp_matrix must have column names.")
  }
  if (is.null(estimators_keep)) {
    estimators_keep <- all_est
  }
  # Preserve matrix column order, drop anything not present.
  estimators_keep <- all_est[all_est %in% estimators_keep]
  if (length(estimators_keep) == 0L) {
    stop(".gt_long_format(): no requested estimators found in pp_matrix.")
  }
  k <- length(estimators_keep)

  long <- data.frame(
    person_id      = rep(seq_len(N), times = k),
    observed_score = rep(observed_score, times = k),
    stringsAsFactors = FALSE
  )
  if (!is.null(cov_x_itemmean)) {
    long$cov_xim <- rep(cov_x_itemmean, times = k)
  }
  long$estimator <- rep(estimators_keep, each = N)
  long$csem_var  <- as.vector(pp_matrix[, estimators_keep, drop = FALSE])
  long$csem      <- .gt_sem_sqrt(long$csem_var, truncate_negative_error_var)

  long
}


# -----------------------------------------------------------------------------
# .gt_add_analytical_se(): attach analytical SE columns to the long table
#
# Computes the closed-form sampling variance of each estimator on the
# error-variance scale via .gt_analytical_se() (constant across persons),
# then converts to the CSEM scale per person via the delta method
#   var(csem_p) = var(V_p) / (4 V_p)
# where V_p is the per-person error-variance point estimate (csem_var).
#
# Inputs
#   per_person_long : data.frame in long-tidy format, with at least
#                     `estimator` and `csem_var`.
#   vc              : list, output of .gt_variance_components().
#   n_items_D       : positive scalar.
#
# Returns
#   The input data.frame with two added columns:
#     csem_var.analytic : numeric, per-person sampling variance of the CSEM.
#     se.analytic       : numeric, sqrt(csem_var.analytic).
#   Rows with csem_var <= 0 (or NA) get NA for both, because the delta-method
#   denominator is not defined there.
# -----------------------------------------------------------------------------
.gt_add_analytical_se <- function(per_person_long, vc, n_items_D) {
  if (!all(c("estimator", "csem_var") %in% names(per_person_long))) {
    stop(".gt_add_analytical_se(): `per_person_long` must contain ",
         "columns estimator and csem_var.", call. = FALSE)
  }

  # Error-variance-scale sampling variances (one scalar per estimator).
  se_var <- .gt_analytical_se(vc, n_items_D)

  # The long table labels estimators as relative_full / relative_large_a /
  # relative_uncorrelated; .gt_analytical_se() keys them as full / large_a /
  # uncorrelated. Map between the two naming schemes.
  name_map <- c(absolute              = "absolute",
                relative_full         = "full",
                relative_large_a      = "large_a",
                relative_uncorrelated = "uncorrelated")

  mapped <- name_map[per_person_long$estimator]
  if (anyNA(mapped)) {
    stop(".gt_add_analytical_se(): unrecognized estimator label(s): ",
         paste(unique(per_person_long$estimator[is.na(mapped)]),
               collapse = ", "),
         call. = FALSE)
  }
  var_on_v <- unname(unlist(se_var[mapped], use.names = FALSE))

  csem_var_point <- per_person_long$csem_var
  ok <- !is.na(csem_var_point) & csem_var_point > 0

  csem_var_analytic <- rep(NA_real_, length(csem_var_point))
  csem_var_analytic[ok] <- var_on_v[ok] / (4 * csem_var_point[ok])

  per_person_long$csem_var.analytic <- csem_var_analytic
  per_person_long$se.analytic       <- sqrt(csem_var_analytic)

  per_person_long
}


# -----------------------------------------------------------------------------
# .gt_add_ci(): attach Wald confidence-interval columns to the long table
#
# For csemGT v1.0 the confidence intervals are Wald (normal-approximation)
# intervals on the CSEM scale:
#   ci = csem +/- z_{1-alpha/2} * se
# applied independently to the analytical SE (if `se.analytic` is present)
# and the bootstrap SE (if `se.boot` is present). The lower bound is
# truncated at 0 because the CSEM is non-negative.
#
# The `ci_method` argument (percentile / basic / normal / bca) is accepted
# for forward compatibility with the spec v4 signature, but replicate-based
# intervals (percentile / basic / bca) require the full B x N replicate
# matrix, which the bootstrap helpers do not retain by default. For v1.0 any
# non-"normal" request is satisfied with the normal interval; the requested
# and the effective method are both recorded as attributes. Replicate-based
# intervals are DEFERRED to v1.0.1.
#
# Inputs
#   per_person_long : data.frame in long-tidy format, with `csem` and at
#                     least one of `se.analytic` / `se.boot`.
#   ci_method       : one of "percentile", "basic", "normal", "bca".
#   ci_level        : numeric in (0, 1).
#   bootstrap       : logical; informational only (the presence of
#                     `se.boot` is what actually drives the bootstrap CI).
#
# Returns
#   The input data.frame with ci_low.analytic / ci_up.analytic added when
#   se.analytic is present, and ci_low.boot / ci_up.boot added when se.boot
#   is present. Attributes `ci_method` (effective) and `ci_method_requested`
#   and `ci_level` are set.
# -----------------------------------------------------------------------------
.gt_add_ci <- function(per_person_long,
                       ci_method = "percentile",
                       ci_level  = 0.95,
                       bootstrap = FALSE) {
  ci_method_requested <- match.arg(
    ci_method, c("percentile", "basic", "normal", "bca"))
  stopifnot(is.numeric(ci_level), length(ci_level) == 1L,
            ci_level > 0, ci_level < 1)
  if (!("csem" %in% names(per_person_long))) {
    stop(".gt_add_ci(): `per_person_long` must contain column csem.",
         call. = FALSE)
  }

  # v1.0: effective method is always "normal" (Wald). Replicate-based
  # intervals are deferred; see the function's documentation block.
  ci_method_effective <- "normal"

  alpha <- 1 - ci_level
  z     <- stats::qnorm(1 - alpha / 2)

  if ("se.analytic" %in% names(per_person_long)) {
    per_person_long$ci_low.analytic <-
      pmax(per_person_long$csem - z * per_person_long$se.analytic, 0)
    per_person_long$ci_up.analytic  <-
      per_person_long$csem + z * per_person_long$se.analytic
  }

  if ("se.boot" %in% names(per_person_long)) {
    per_person_long$ci_low.boot <-
      pmax(per_person_long$csem - z * per_person_long$se.boot, 0)
    per_person_long$ci_up.boot  <-
      per_person_long$csem + z * per_person_long$se.boot
  }

  attr(per_person_long, "ci_method")           <- ci_method_effective
  attr(per_person_long, "ci_method_requested") <- ci_method_requested
  attr(per_person_long, "ci_level")            <- ci_level

  per_person_long
}


# -----------------------------------------------------------------------------
# .pivot_to_wide(): long-tidy -> wide, one row per person
#
# Spreads every value column of the long table across the `estimator`
# factor, producing columns named <value>.<estimator>. Person-constant
# columns (person_id, observed_score, cov_xim) are carried through once.
#
# Column naming follows spec v4 §6.4 and the "explicit" SE convention
# confirmed for csemGT: csem.absolute, csem_var.absolute,
# se.analytic.absolute, se.boot.absolute, ci_low.analytic.absolute, etc.
#
# Inputs
#   per_person_long : data.frame in long-tidy format.
#   paradigm        : character; currently only "gt" is supported (the
#                     argument exists for forward compatibility with the
#                     csemR multi-paradigm design).
#
# Returns
#   A wide data.frame with N rows. Column order: the person-constant columns
#   first, then, for each estimator (in first-appearance order), all of its
#   value columns grouped together.
# -----------------------------------------------------------------------------
.pivot_to_wide <- function(per_person_long, paradigm = "gt") {
  if (!identical(paradigm, "gt")) {
    stop(".pivot_to_wide(): only paradigm = 'gt' is supported in csemGT.",
         call. = FALSE)
  }
  if (!all(c("person_id", "estimator") %in% names(per_person_long))) {
    stop(".pivot_to_wide(): `per_person_long` must contain columns ",
         "person_id and estimator.", call. = FALSE)
  }

  # Person-constant columns carried through once.
  id_cols <- intersect(c("person_id", "observed_score", "cov_xim"),
                       names(per_person_long))
  # Value columns spread across estimator.
  value_cols <- setdiff(names(per_person_long), c(id_cols, "estimator"))

  estimators <- unique(per_person_long$estimator)

  # Build the id portion: first row per person, ordered by person_id.
  first_rows <- !duplicated(per_person_long$person_id)
  wide <- per_person_long[first_rows, id_cols, drop = FALSE]
  ord  <- order(wide$person_id)
  wide <- wide[ord, , drop = FALSE]
  rownames(wide) <- NULL

  # For each estimator, append its value columns (grouped by estimator).
  for (est in estimators) {
    sel <- per_person_long$estimator == est
    sub <- per_person_long[sel, c("person_id", value_cols), drop = FALSE]
    sub <- sub[order(sub$person_id), , drop = FALSE]
    for (vname in value_cols) {
      wide[[paste0(vname, ".", est)]] <- sub[[vname]]
    }
  }

  wide
}

# =============================================================================
# Part 6 — merge smoothed CSEMs back onto the per-person wide table
#
# Final data-plumbing step of the csem_gt() pipeline (spec v4 §5.3 step 11).
# After:
#   .pivot_to_wide()      -> per_person_wide (one row per person)
#   .apply_smoother()     -> per_person_wide + smoothed_csem.<estimator> columns
#   .collapse_to_score()  -> by_score_wide   (one row per unique score)
# this helper carries the smoothed values back to every person by matching on
# observed_score.
# =============================================================================

# -----------------------------------------------------------------------------
# .merge_smoothed_to_person(): attach smoothed_csem.* columns to per_person_wide
#
# .apply_smoother() writes one `smoothed_csem.<estimator>` column per smoothed
# error-variance column onto the per-person wide table (already on the CSEM
# scale: it applies sqrt(pmax(fitted, 0)) internally). This helper looks each
# person's observed_score up in the by-score table and copies the
# corresponding smoothed values across.
#
# Inputs
#   per_person_wide : wide data.frame from .pivot_to_wide(); one row per
#                     person; must carry an `observed_score` column.
#   by_score_wide   : by-score data.frame from .collapse_to_score(); one row
#                     per unique observed score; carries `observed_score` and
#                     zero or more `smoothed_csem.<estimator>` columns.
#
# Returns
#   per_person_wide with every `smoothed_csem.<estimator>` column of
#   by_score_wide appended, each person taking the value from the by-score row
#   whose observed_score matches. Persons whose observed_score was excluded
#   from the smoother fit (exclude_extremes = TRUE) inherit the NA that
#   .apply_smoother() wrote for that score level. When by_score_wide carries
#   no smoothed_* columns (e.g. smoother = "none"), per_person_wide is
#   returned unchanged.
#
# Note
#   The match is on `observed_score` values. In the normal pipeline every
#   person's score is present in the by-score table by construction (the
#   by-score table is built from the same persons), so `match()` never
#   produces NA from a missing level; an NA in a smoothed column therefore
#   always means "excluded extreme", never "score not found".
# -----------------------------------------------------------------------------
.merge_smoothed_to_person <- function(per_person_wide, by_score_wide) {
  smoothed_cols <- grep("^smoothed_", names(by_score_wide), value = TRUE)
  if (length(smoothed_cols) == 0L) {
    return(per_person_wide)
  }
  if (!("observed_score" %in% names(per_person_wide))) {
    stop(".merge_smoothed_to_person(): `per_person_wide` must contain ",
         "an observed_score column.", call. = FALSE)
  }
  if (!("observed_score" %in% names(by_score_wide))) {
    stop(".merge_smoothed_to_person(): `by_score_wide` must contain ",
         "an observed_score column.", call. = FALSE)
  }
 
  idx <- match(per_person_wide$observed_score,
               by_score_wide$observed_score)
  for (sc in smoothed_cols) {
    per_person_wide[[sc]] <- by_score_wide[[sc]][idx]
  }
 
  per_person_wide
}
