# tests/testthat/test-csem_gt_parity.R
#
# Numerical parity of the csem_gt() orchestrator against the legacy
# reference implementation csem_g1f(), shipped in inst/legacy/. Both
# implementations share the same estimating equations (Brennan, 1998),
# so the only admissible discrepancy is floating-point reordering;
# tolerance is therefore 1e-10.
#
# Scope: per-person point estimates, the cov(X_pi, item means) term,
# the ANOVA variance components, the population-level error variances
# and SEMs, and the reliability-like coefficients. The analytical
# sampling variances are NOT re-checked here: the helper that produces
# them, .gt_analytical_se(), already carries its own legacy-parity
# suite in test-utils-gt-population.R, and csem_gt() only calls it.
#
# The legacy script is sourced once at file scope, matching the pattern
# used by the other parity test files in this suite.

source(system.file("legacy", "csem_gt_estimation_v3.R",
                    package = "csemGT", mustWork = TRUE))

.make_parity_data <- function(seed = 42L, N = 500L, J = 40L, p = 0.5) {
  set.seed(seed)
  matrix(rbinom(N * J, 1, p), nrow = N, ncol = J)
}


test_that("csem_gt() reproduces csem_g1f() per-person point estimates (all methods)", {
  data <- .make_parity_data()

  old <- csem_g1f(data, method = "all", se_method = "analytical")
  new <- suppressMessages(csem_gt(
    data, method = c("full", "large_a", "uncorrelated"),
    error_type = c("absolute", "relative"), smoother = "none"))

  # Row-alignment guard: both implementations index persons 1..N in
  # input order, so the estimation columns must be directly comparable.
  expect_equal(new$estimates$person_id, old$person$id)
  expect_equal(new$estimates$observed_score, old$person$observed_score,
               tolerance = 1e-10)

  # Absolute estimator (Brennan, 1998, eq. 20).
  expect_equal(new$estimates$csem.absolute, old$person$abs_csem,
               tolerance = 1e-10)
  expect_equal(new$estimates$csem_var.absolute, old$person$abs_error_var,
               tolerance = 1e-10)

  # Per-person cov(X_pi, item means) — Brennan (1998), eq. 33.
  expect_equal(new$estimates$cov_xim, old$person$cov_x_itemmean,
               tolerance = 1e-10)

  # The three relative-error estimators (Brennan, 1998, eqs. 36, 40, 41).
  expect_equal(new$estimates$csem.relative_full,
               old$person$rel_csem_full, tolerance = 1e-10)
  expect_equal(new$estimates$csem.relative_large_a,
               old$person$rel_csem_large_a, tolerance = 1e-10)
  expect_equal(new$estimates$csem.relative_uncorrelated,
               old$person$rel_csem_uncorrelated, tolerance = 1e-10)

  expect_equal(new$estimates$csem_var.relative_full,
               old$person$rel_error_var_full, tolerance = 1e-10)
  expect_equal(new$estimates$csem_var.relative_large_a,
               old$person$rel_error_var_large_a, tolerance = 1e-10)
  expect_equal(new$estimates$csem_var.relative_uncorrelated,
               old$person$rel_error_var_uncorrelated, tolerance = 1e-10)
})


test_that("csem_gt() reproduces csem_g1f() for a single relative method", {
  data <- .make_parity_data()

  old <- csem_g1f(data, method = "full", se_method = "analytical")
  new <- suppressMessages(csem_gt(
    data, method = "full", error_type = "relative", smoother = "none"))

  # With a single method csem_g1f() exposes it through the unsuffixed
  # rel_* columns.
  expect_equal(new$estimates$csem.relative_full, old$person$rel_csem,
               tolerance = 1e-10)
  expect_equal(new$estimates$csem_var.relative_full,
               old$person$rel_error_var, tolerance = 1e-10)
})


test_that("csem_gt() reproduces csem_g1f() D-study extrapolation", {
  data <- .make_parity_data(N = 400L, J = 30L)

  old <- csem_g1f(data, n_items_D = 60L, method = "all",
                  se_method = "analytical")
  new <- suppressMessages(csem_gt(
    data, n_items_D = 60L, method = c("full", "large_a", "uncorrelated"),
    error_type = c("absolute", "relative"), smoother = "none"))

  expect_equal(new$estimates$csem.absolute, old$person$abs_csem,
               tolerance = 1e-10)
  expect_equal(new$estimates$csem.relative_full,
               old$person$rel_csem_full, tolerance = 1e-10)
  expect_equal(new$estimates$csem.relative_large_a,
               old$person$rel_csem_large_a, tolerance = 1e-10)
  expect_equal(new$estimates$csem.relative_uncorrelated,
               old$person$rel_csem_uncorrelated, tolerance = 1e-10)
})


test_that("csem_gt() reproduces csem_g1f() variance components and coefficients", {
  data <- .make_parity_data()

  old <- csem_g1f(data, method = "all", se_method = "analytical",
                  cutpoint = 0.5)
  new <- suppressMessages(csem_gt(
    data, method = c("full", "large_a", "uncorrelated"),
    error_type = c("absolute", "relative"), cutpoint = 0.5,
    smoother = "none"))

  vc <- new$variance_components

  # ANOVA variance components (legacy: named numeric vector).
  expect_equal(vc$person,   unname(old$variance_components["person"]),
               tolerance = 1e-10)
  expect_equal(vc$item,     unname(old$variance_components["item"]),
               tolerance = 1e-10)
  expect_equal(vc$residual, unname(old$variance_components["resid"]),
               tolerance = 1e-10)

  # Population-level error variances and SEMs. The legacy stores these
  # in overall$estimate with rows in the fixed order:
  #   1 absolute_error_var, 2 absolute_sem,
  #   3 relative_error_var, 4 relative_sem.
  expect_equal(vc$population_quantities$absolute_error_var,
               old$overall$estimate[1], tolerance = 1e-10)
  expect_equal(vc$population_quantities$absolute_sem,
               old$overall$estimate[2], tolerance = 1e-10)
  expect_equal(vc$population_quantities$relative_error_var,
               old$overall$estimate[3], tolerance = 1e-10)
  expect_equal(vc$population_quantities$relative_sem,
               old$overall$estimate[4], tolerance = 1e-10)

  # Reliability-like coefficients (Brennan, 2001, eqs. 2.40, 2.41, 2.55).
  expect_equal(vc$reliability_coefficients$erho2, old$erho2,
               tolerance = 1e-10)
  expect_equal(vc$reliability_coefficients$phi, old$phi,
               tolerance = 1e-10)
  expect_equal(vc$reliability_coefficients$phi_lambda, old$phi_lambda,
               tolerance = 1e-10)

  # ANOVA table: degrees of freedom exactly, sums of squares and mean
  # squares to floating-point tolerance.
  expect_equal(vc$anova_table$df, old$anova$df)
  expect_equal(vc$anova_table$SS, old$anova$SS, tolerance = 1e-10)
  expect_equal(vc$anova_table$MS, old$anova$MS, tolerance = 1e-10)
})


test_that("csem_gt() smoothed CSEMs reproduce csem_g1f(smooth = TRUE)", {
  data <- .make_parity_data()

  old <- csem_g1f(data, method = "all", se_method = "analytical",
                  smooth = TRUE)
  new <- suppressMessages(csem_gt(
    data, method = c("full", "large_a", "uncorrelated"),
    error_type = c("absolute", "relative"), smoother = "polynomial"))

  # The legacy smooths the absolute error variance and reports
  # abs_csem_sm; csemGT smooths on the same quadratic basis and
  # exposes smoothed_csem.absolute on the person table.
  expect_equal(new$estimates$smoothed_csem.absolute,
               old$person$abs_csem_sm, tolerance = 1e-10)
  expect_equal(new$estimates$smoothed_csem.relative_full,
               old$person$rel_csem_sm_full, tolerance = 1e-10)
})
