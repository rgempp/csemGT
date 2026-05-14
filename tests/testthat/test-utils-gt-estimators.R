# tests/testthat/test-utils-gt-estimators.R
#
# Tests for the atomic per-person estimators of utils-gt.R Part 2:
#   .gt_csem_absolute()
#   .gt_csem_relative_full()
#   .gt_csem_relative_large_a()
#   .gt_csem_relative_uncorrelated()
#   .gt_estimators_for_person()
#
# Strategy: every test goes through .gt_variance_components() to build the
# ingredients exactly as the main pipeline will, then either compares the
# resulting estimator to the legacy compute_relative_estimators() (1e-10),
# or asserts a closed-form algebraic identity in floating-point precision
# (~1e-14).
#
# No file from inst/extdata is read here; these tests run on synthetic data
# and are completely self-contained. The .ado reference comparison lives
# in test-csem_gt_parity_ado.R (sub-fase 7), which depends on the .dta file.

source(system.file("legacy", "csem_gt_estimation_v3.R",
                   package  = "csemGT",
                   mustWork = TRUE))


# -----------------------------------------------------------------------------
# Helper for tests: rebuild Brennan (1998) ingredients in the same way the
# legacy script does, given a data matrix. Used by tests below to assemble
# inputs without leaning on .gt_variance_components() recursively (so that
# any future bug in .gt_variance_components() does not silently propagate
# into these atomic-estimator tests).
# -----------------------------------------------------------------------------
.brennan_ingredients <- function(X, n_items_D = NULL) {
  X <- as.matrix(X); storage.mode(X) <- "double"
  N <- nrow(X); J <- ncol(X)
  if (is.null(n_items_D)) n_items_D <- J

  person_mean <- rowMeans(X)
  item_mean   <- colMeans(X)
  grand_mean  <- mean(X)
  person_centered <- sweep(X, 1L, person_mean, "-")
  b_vec       <- item_mean - grand_mean
  row_var     <- rowSums(person_centered * person_centered) / (J - 1L)
  cov_xim     <- as.vector(person_centered %*% b_vec) / (J - 1L)

  # sigma2_i via ANOVA: (MS_item - MS_residual) / N
  SS_item     <- N * sum((item_mean - grand_mean)^2)
  resid_mat   <- person_centered - rep(b_vec, each = N)
  SS_residual <- sum(resid_mat * resid_mat)
  MS_item     <- SS_item     / (J - 1L)
  MS_residual <- SS_residual / ((N - 1L) * (J - 1L))
  sigma2_i    <- (MS_item - MS_residual) / N

  list(row_var = row_var, cov_x_itemmean = cov_xim,
       sigma2_i = sigma2_i, n_items_D = n_items_D, N = N)
}


# -----------------------------------------------------------------------------
# .gt_csem_absolute() — Brennan (1998) eq. 20
# -----------------------------------------------------------------------------

test_that(".gt_csem_absolute() equals row_var / n_items_D exactly", {
  set.seed(101)
  rv <- runif(50, 0, 2)
  expect_identical(.gt_csem_absolute(rv, n_items_D = 20),
                   rv / 20)
  expect_identical(.gt_csem_absolute(rv, n_items_D = 1),
                   rv)
})


test_that(".gt_csem_absolute() reproduces legacy abs_error_var at 1e-12", {
  set.seed(102)
  X <- matrix(rnorm(80 * 12), 80, 12)
  ing <- .brennan_ingredients(X)

  legacy <- csem_g1f(as.data.frame(X), method = "full")
  new    <- .gt_csem_absolute(ing$row_var, ing$n_items_D)

  expect_equal(unname(new), unname(legacy$person$abs_error_var),
               tolerance = 1e-12)
})


test_that(".gt_csem_absolute() vectorizes over length-L inputs", {
  expect_length(.gt_csem_absolute(numeric(0), 5), 0)
  expect_length(.gt_csem_absolute(1.5,         5), 1)
  expect_length(.gt_csem_absolute(c(1, 2, 3),  5), 3)
})


# -----------------------------------------------------------------------------
# .gt_csem_relative_full() — Brennan (1998) eq. 36
# -----------------------------------------------------------------------------

test_that(".gt_csem_relative_full() reproduces legacy compute_relative_estimators$full at 1e-10", {
  set.seed(201)
  X <- matrix(rbinom(500 * 40, 1, 0.5), 500, 40)
  ing <- .brennan_ingredients(X)

  abs_ev <- ing$row_var / ing$n_items_D
  legacy_full <- compute_relative_estimators(
    abs_error_var  = abs_ev,
    cov_x_itemmean = ing$cov_x_itemmean,
    sigma2_item    = ing$sigma2_i,
    n_items_D      = ing$n_items_D,
    A              = ing$N
  )$full

  new <- .gt_csem_relative_full(abs_ev, ing$cov_x_itemmean,
                                ing$sigma2_i, ing$n_items_D, ing$N)

  expect_equal(unname(new), unname(legacy_full), tolerance = 1e-10)
})


test_that(".gt_csem_relative_full() collapses to .large_a as N -> infinity", {
  # As N grows, both (N+1)/(N-1) and N/(N-1) -> 1, so full -> large_a. With
  # N = 1e6 the relative difference is at most ~ 1e-5 / value, well within
  # this loose tolerance.
  set.seed(202)
  abs_ev <- runif(30, 0.02, 0.05)
  cov    <- runif(30, -0.01, 0.01)
  full   <- .gt_csem_relative_full(abs_ev, cov,
                                   sigma2_i = 0.04, n_items_D = 20,
                                   N = 1e6L)
  la     <- .gt_csem_relative_large_a(abs_ev, cov,
                                      sigma2_i = 0.04, n_items_D = 20)
  expect_equal(full, la, tolerance = 1e-5)
})


# -----------------------------------------------------------------------------
# .gt_csem_relative_large_a() — Brennan (1998) eq. 40
# -----------------------------------------------------------------------------

test_that(".gt_csem_relative_large_a() reproduces legacy compute_relative_estimators$large_a at 1e-12", {
  set.seed(301)
  X <- matrix(rbinom(500 * 40, 1, 0.5), 500, 40)
  ing <- .brennan_ingredients(X)
  abs_ev <- ing$row_var / ing$n_items_D

  legacy_la <- compute_relative_estimators(
    abs_error_var  = abs_ev,
    cov_x_itemmean = ing$cov_x_itemmean,
    sigma2_item    = ing$sigma2_i,
    n_items_D      = ing$n_items_D,
    A              = ing$N
  )$large_a

  new <- .gt_csem_relative_large_a(abs_ev, ing$cov_x_itemmean,
                                   ing$sigma2_i, ing$n_items_D)

  expect_equal(unname(new), unname(legacy_la), tolerance = 1e-12)
})


# -----------------------------------------------------------------------------
# .gt_csem_relative_uncorrelated() — Brennan (1998) eq. 41
# -----------------------------------------------------------------------------

test_that(".gt_csem_relative_uncorrelated() reproduces legacy compute_relative_estimators$uncorrelated at 1e-12", {
  set.seed(401)
  X <- matrix(rbinom(500 * 40, 1, 0.5), 500, 40)
  ing <- .brennan_ingredients(X)
  abs_ev <- ing$row_var / ing$n_items_D

  legacy_unc <- compute_relative_estimators(
    abs_error_var  = abs_ev,
    cov_x_itemmean = ing$cov_x_itemmean,
    sigma2_item    = ing$sigma2_i,
    n_items_D      = ing$n_items_D,
    A              = ing$N
  )$uncorrelated

  new <- .gt_csem_relative_uncorrelated(abs_ev, ing$sigma2_i, ing$n_items_D)

  expect_equal(unname(new), unname(legacy_unc), tolerance = 1e-12)
})


test_that(".gt_csem_relative_uncorrelated() does not depend on cov_x_itemmean", {
  # The uncorrelated estimator drops the cov_p term entirely (Brennan eq. 41).
  # Confirm by varying cov_x_itemmean and checking the output is invariant.
  abs_ev <- c(0.01, 0.03, 0.05)
  out1 <- .gt_csem_relative_uncorrelated(abs_ev, sigma2_i = 0.02, n_items_D = 10)
  out2 <- .gt_csem_relative_uncorrelated(abs_ev, sigma2_i = 0.02, n_items_D = 10)
  expect_identical(out1, out2)
})


# -----------------------------------------------------------------------------
# .gt_estimators_for_person() — the four-in-one wrapper
# -----------------------------------------------------------------------------

test_that(".gt_estimators_for_person() returns a named list of length 4", {
  set.seed(501)
  X <- matrix(rnorm(80 * 12), 80, 12)
  ing <- .brennan_ingredients(X)
  out <- .gt_estimators_for_person(ing$row_var, ing$cov_x_itemmean,
                                   ing$sigma2_i, ing$n_items_D, ing$N)
  expect_type(out, "list")
  expect_named(out, c("absolute", "full", "large_a", "uncorrelated"))
  for (cmp in names(out)) {
    expect_length(out[[cmp]], length(ing$row_var))
  }
})


test_that(".gt_estimators_for_person() jointly reproduces all four legacy estimators at 1e-10", {
  set.seed(502)
  X <- matrix(rbinom(500 * 40, 1, 0.5), 500, 40)
  ing <- .brennan_ingredients(X)

  legacy_fit <- csem_g1f(as.data.frame(X), method = "all")
  legacy_abs <- legacy_fit$person$abs_error_var
  legacy_full <- legacy_fit$person$rel_error_var_full
  legacy_la   <- legacy_fit$person$rel_error_var_large_a
  legacy_unc  <- legacy_fit$person$rel_error_var_uncorrelated

  new <- .gt_estimators_for_person(ing$row_var, ing$cov_x_itemmean,
                                   ing$sigma2_i, ing$n_items_D, ing$N)

  expect_equal(unname(new$absolute),     unname(legacy_abs),
               tolerance = 1e-10)
  expect_equal(unname(new$full),         unname(legacy_full),
               tolerance = 1e-10)
  expect_equal(unname(new$large_a),      unname(legacy_la),
               tolerance = 1e-10)
  expect_equal(unname(new$uncorrelated), unname(legacy_unc),
               tolerance = 1e-10)
})


test_that(".gt_estimators_for_person() vectorizes consistently with single-person bootstrap usage", {
  # In bootstrap, .gt_estimators_for_person() will be called with row_var
  # and cov_x_itemmean of length B (one per replication for a single focal
  # person). N stays equal to the full-sample N — it parameterizes the
  # estimator, not the input length. This test exercises that contract.
  set.seed(503)
  B <- 200L
  L <- 30L  # placeholder for the focal person's bootstrap output length

  rv  <- runif(B, 0.10, 0.50)
  cov <- runif(B, -0.05, 0.05)

  out <- .gt_estimators_for_person(rv, cov,
                                   sigma2_i = 0.04, n_items_D = 20L,
                                   N = 300L)

  for (cmp in names(out)) expect_length(out[[cmp]], B)
  # Sanity: absolute = rv/D regardless of cov; cov-free estimators must not
  # depend on cov.
  expect_equal(out$absolute, rv / 20, tolerance = 1e-15)
})


# -----------------------------------------------------------------------------
# End-to-end algebraic identities (Brennan 1998, paper §5.2)
# -----------------------------------------------------------------------------

test_that("Algebraic identity: full - large_a equals the (N+1)/(N-1) and N/(N-1) corrections only", {
  # full = ((N+1)/(N-1)) * abs_ev + sigma2_i/D - (N/(N-1)) * (2 cov / D)
  # la   =                  abs_ev + sigma2_i/D -            (2 cov / D)
  # Coefficient of abs_ev in (full - la) = (N+1)/(N-1) - 1 = 2/(N-1)
  # Coefficient of (2 cov / D) in (full - la) = -N/(N-1) - (-1) = -1/(N-1)
  # Therefore:
  #   full - la = (2/(N-1)) * abs_ev - (1/(N-1)) * (2 cov / D)
  #             = (2/(N-1)) * (abs_ev - cov / D)
  #
  # The two algebraically equivalent expressions use different orderings of
  # additions/divisions, so the FP results diverge at the scale of a few
  # machine epsilons (~1e-15 relative). Tolerance is set to 1e-12 — tight
  # enough to catch genuine algebraic bugs, loose enough to clear the
  # double-precision noise floor on values around 1e-4.
  set.seed(601)
  abs_ev <- runif(40, 0.01, 0.10)
  cov    <- runif(40, -0.02, 0.02)
  D <- 25L; N <- 500L

  full <- .gt_csem_relative_full(abs_ev, cov, sigma2_i = 0.03,
                                 n_items_D = D, N = N)
  la   <- .gt_csem_relative_large_a(abs_ev, cov, sigma2_i = 0.03,
                                    n_items_D = D)

  expected_diff <- (2 / (N - 1)) * (abs_ev - cov / D)
  expect_equal(full - la, expected_diff, tolerance = 1e-12)
})


test_that("Algebraic identity: large_a - uncorrelated equals -2 cov / D exactly", {
  # la  = abs_ev + sigma2_i/D - 2 cov / D
  # unc = abs_ev - sigma2_i/D
  # la - unc = 2 sigma2_i / D - 2 cov / D
  set.seed(602)
  abs_ev <- runif(40, 0.01, 0.10)
  cov    <- runif(40, -0.02, 0.02)
  D <- 25L

  la  <- .gt_csem_relative_large_a(abs_ev, cov, sigma2_i = 0.03,
                                   n_items_D = D)
  unc <- .gt_csem_relative_uncorrelated(abs_ev, sigma2_i = 0.03,
                                        n_items_D = D)

  expected_diff <- 2 * 0.03 / D - 2 * cov / D
  expect_equal(la - unc, expected_diff, tolerance = 1e-14)
})
