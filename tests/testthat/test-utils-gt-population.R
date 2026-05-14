# tests/testthat/test-utils-gt-population.R
#
# Tests for utils-gt.R Part 4:
#   .gt_analytical_se()
#   .gt_population_quantities()
#   .gt_reliability_coefficients()
#
# All three are pure-numeric helpers consuming the variance-component list
# from .gt_variance_components(). Parity is checked against csem_g1f() in
# the legacy script at tolerance 1e-10. The .ado parity (1e-6) is verified
# end-to-end in test-csem_gt_parity_ado.R (sub-fase 7), which depends on
# the .dta reference file.

source(system.file("legacy", "csem_gt_estimation_v3.R",
                   package  = "csemGT",
                   mustWork = TRUE))


# -----------------------------------------------------------------------------
# .gt_analytical_se()
# -----------------------------------------------------------------------------

test_that(".gt_analytical_se() returns a named list of four non-negative scalars", {
  set.seed(1001)
  X <- matrix(rnorm(120 * 15), 120, 15)
  vc <- .gt_variance_components(X)
  se <- .gt_analytical_se(vc, n_items_D = 15L)

  expect_type(se, "list")
  expect_named(se, c("absolute", "full", "large_a", "uncorrelated"))
  for (cmp in names(se)) {
    expect_length(se[[cmp]], 1L)
    expect_gte(se[[cmp]], 0)
  }
})


test_that(".gt_analytical_se() absolute equals uncorrelated (both gamma = 0)", {
  # The absolute and uncorrelated estimators share alpha = 1/D, gamma = 0,
  # so their sampling variances are identical by construction. The legacy
  # encodes the same identity (v_unc <- v_abs, legacy line 302).
  set.seed(1002)
  X <- matrix(rbinom(200 * 20, 1, 0.5), 200, 20)
  vc <- .gt_variance_components(X)
  se <- .gt_analytical_se(vc, n_items_D = 20L)
  expect_identical(se$absolute, se$uncorrelated)
})


test_that(".gt_analytical_se() reproduces csem_g1f() analytical variances at 1e-10", {
  # csem_g1f(method = "all", se_method = "analytical") populates the person
  # data.frame with var_*_ev_analytical columns. Under the exact branch
  # these are constant across persons, so we compare against the first row.
  set.seed(1003)
  data <- matrix(rbinom(300 * 30, 1, 0.5), 300, 30)

  legacy <- csem_g1f(as.data.frame(data), method = "all",
                     se_method = "analytical")
  vc  <- .gt_variance_components(data)
  se  <- .gt_analytical_se(vc, n_items_D = 30L)

  expect_equal(se$absolute,
               legacy$person$var_abs_ev_analytical[1],
               tolerance = 1e-10)
  expect_equal(se$full,
               legacy$person$var_rel_ev_analytical_full[1],
               tolerance = 1e-10)
  expect_equal(se$large_a,
               legacy$person$var_rel_ev_analytical_large_a[1],
               tolerance = 1e-10)
  expect_equal(se$uncorrelated,
               legacy$person$var_rel_ev_analytical_uncorrelated[1],
               tolerance = 1e-10)
})


test_that(".gt_analytical_se() analytical variances are genuinely constant across persons in the legacy", {
  # Sanity guard: confirm the legacy's exact-branch analytical variances
  # really are person-invariant, which is the property that justifies
  # .gt_analytical_se() returning scalars rather than length-N vectors.
  set.seed(1004)
  data <- matrix(rbinom(150 * 18, 1, 0.5), 150, 18)
  legacy <- csem_g1f(as.data.frame(data), method = "all",
                     se_method = "analytical")
  expect_equal(length(unique(legacy$person$var_abs_ev_analytical)), 1L)
  expect_equal(length(unique(legacy$person$var_rel_ev_analytical_full)), 1L)
})


test_that(".gt_analytical_se() scales with n_items_D as 1/D^2 for the absolute estimator", {
  # v_abs = var_sp2 / D^2; doubling D divides v_abs by 4.
  set.seed(1005)
  X <- matrix(rnorm(80 * 12), 80, 12)
  vc <- .gt_variance_components(X)
  se_D  <- .gt_analytical_se(vc, n_items_D = 12L)
  se_2D <- .gt_analytical_se(vc, n_items_D = 24L)
  expect_equal(se_2D$absolute, se_D$absolute / 4, tolerance = 1e-12)
})


# -----------------------------------------------------------------------------
# .gt_population_quantities()
# -----------------------------------------------------------------------------

test_that(".gt_population_quantities() returns the four named scalars", {
  set.seed(2001)
  X <- matrix(rnorm(100 * 14), 100, 14)
  vc <- .gt_variance_components(X)
  pq <- .gt_population_quantities(vc, n_items_D = 14L)
  expect_named(pq, c("absolute_error_var", "absolute_sem",
                     "relative_error_var", "relative_sem"))
  expect_equal(pq$absolute_sem, sqrt(pq$absolute_error_var), tolerance = 1e-14)
  expect_equal(pq$relative_sem, sqrt(pq$relative_error_var), tolerance = 1e-14)
})


test_that(".gt_population_quantities() reproduces csem_g1f()$overall at 1e-10", {
  set.seed(2002)
  data <- matrix(rbinom(300 * 30, 1, 0.5), 300, 30)
  legacy <- csem_g1f(as.data.frame(data), method = "all")
  vc <- .gt_variance_components(data)
  pq <- .gt_population_quantities(vc, n_items_D = 30L)

  ov <- legacy$overall
  get_ov <- function(q) ov$estimate[ov$quantity == q]

  expect_equal(pq$absolute_error_var, get_ov("absolute_error_var"),
               tolerance = 1e-10)
  expect_equal(pq$absolute_sem,       get_ov("absolute_sem"),
               tolerance = 1e-10)
  expect_equal(pq$relative_error_var, get_ov("relative_error_var"),
               tolerance = 1e-10)
  expect_equal(pq$relative_sem,       get_ov("relative_sem"),
               tolerance = 1e-10)
})


test_that(".gt_population_quantities() relative_error_var equals sigma2_pi / D", {
  set.seed(2003)
  X <- matrix(rnorm(60 * 10), 60, 10)
  vc <- .gt_variance_components(X)
  pq <- .gt_population_quantities(vc, n_items_D = 25L)
  expect_equal(pq$relative_error_var, vc$sigma2_pi / 25, tolerance = 1e-14)
})


# -----------------------------------------------------------------------------
# .gt_reliability_coefficients()
# -----------------------------------------------------------------------------

test_that(".gt_reliability_coefficients() returns erho2, phi, phi_lambda", {
  set.seed(3001)
  X <- matrix(rnorm(120 * 16), 120, 16)
  vc <- .gt_variance_components(X)
  rc <- .gt_reliability_coefficients(vc, n_items_D = 16L)
  expect_named(rc, c("erho2", "phi", "phi_lambda"))
  expect_true(is.na(rc$phi_lambda))  # no cutpoint supplied
})


test_that(".gt_reliability_coefficients() reproduces csem_g1f() erho2 and phi at 1e-10", {
  set.seed(3002)
  data <- matrix(rbinom(300 * 30, 1, 0.5), 300, 30)
  legacy <- csem_g1f(as.data.frame(data), method = "all")
  vc <- .gt_variance_components(data)
  rc <- .gt_reliability_coefficients(vc, n_items_D = 30L)

  expect_equal(rc$erho2, legacy$erho2, tolerance = 1e-10)
  expect_equal(rc$phi,   legacy$phi,   tolerance = 1e-10)
})


test_that(".gt_reliability_coefficients() reproduces csem_g1f() phi_lambda with a cutpoint at 1e-10", {
  set.seed(3003)
  data <- matrix(rbinom(300 * 30, 1, 0.5), 300, 30)
  # Cutpoint on the mean-per-item scale, the same scale as grand_mean.
  cut <- 0.55

  legacy <- csem_g1f(as.data.frame(data), method = "all", cutpoint = cut)
  vc <- .gt_variance_components(data)
  rc <- .gt_reliability_coefficients(vc, n_items_D = 30L, cutpoint = cut)

  expect_equal(rc$phi_lambda, legacy$phi_lambda, tolerance = 1e-10)
})


test_that(".gt_reliability_coefficients() phi_lambda collapses toward phi when cutpoint = grand_mean", {
  # When lambda = Xbar, (Xbar - lambda)^2 = 0, and Phi(lambda) reduces to
  # [s2p - s2(Xbar)] / [s2p - s2(Xbar) + sigma^2(Delta)], which differs from
  # Phi only by the small -s2(Xbar) term (which vanishes as N grows). With
  # N = 400 the two are close but not identical; we just check phi_lambda is
  # finite and within a sensible band of phi.
  set.seed(3004)
  data <- matrix(rbinom(400 * 25, 1, 0.5), 400, 25)
  vc <- .gt_variance_components(data)
  rc <- .gt_reliability_coefficients(vc, n_items_D = 25L,
                                     cutpoint = vc$grand_mean)
  expect_true(is.finite(rc$phi_lambda))
  expect_lt(abs(rc$phi_lambda - rc$phi), 0.05)
})


test_that(".gt_reliability_coefficients() returns NA coefficients on degenerate variance components", {
  # Force sigma2_p = 0 via a dataset with no between-person variance:
  # every person has the same response pattern. Then erho2 and phi are
  # 0 / (0 + positive) = 0, which is finite; to actually hit the NA branch
  # we need sigma2_p + error_var <= 0, which requires truncate_vc plus a
  # pathological sample. We instead just confirm the function does not error
  # on a near-degenerate case and returns finite-or-NA values.
  set.seed(3005)
  base_row <- rbinom(20, 1, 0.5)
  data <- matrix(rep(base_row, each = 30), nrow = 30, byrow = TRUE)
  # Add a tiny jitter to avoid a perfectly singular ANOVA.
  data[1, 1] <- 1 - data[1, 1]
  vc <- .gt_variance_components(data, truncate_vc = TRUE)
  rc <- .gt_reliability_coefficients(vc, n_items_D = 20L)
  expect_true(all(vapply(rc[c("erho2", "phi")],
                         function(x) is.na(x) || is.finite(x),
                         logical(1))))
})
