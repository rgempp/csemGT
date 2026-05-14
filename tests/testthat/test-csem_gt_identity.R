# tests/testthat/test-csem_gt_identity.R
#
# Algebraic identities that the GT conditional SEMs must satisfy by
# construction, verified directly on the csem_gt() output and
# independent of the legacy reference. These are exact mathematical
# facts, so the tolerance is machine precision (1e-15); Gempp's paper
# reports a maximum discrepancy near 4e-17 for the Lord identity in
# the binary case.

.make_identity_data <- function(seed, N, J, p = 0.5) {
  set.seed(seed)
  matrix(rbinom(N * J, 1, p), nrow = N, ncol = J)
}


test_that("Brennan (1998) eq. 24: absolute CSEM equals Lord's (1955) binomial CSEM", {
  # For dichotomous items the absolute conditional SEM reduces to
  #   sigma(Delta)_p = sqrt( (J / (J - 1)) * Xbar_p * (1 - Xbar_p) / J )
  #                  = sqrt( Xbar_p * (1 - Xbar_p) / (J - 1) ),
  # i.e. Lord's (1955) binomial-error SEM on the proportion-correct
  # scale (Brennan, 1998, eq. 24; Brennan, 2001).
  J    <- 25L
  data <- .make_identity_data(seed = 101L, N = 800L, J = J, p = 0.55)

  fit <- suppressMessages(
    csem_gt(data, error_type = "absolute", smoother = "none"))

  xbar      <- rowMeans(data)
  lord_csem <- sqrt((J / (J - 1)) * xbar * (1 - xbar) / J)

  expect_equal(fit$estimates$csem.absolute, lord_csem, tolerance = 1e-15)
})


test_that("the absolute CSEM identity holds under D-study extrapolation", {
  # The absolute error variance is the per-person item sample variance
  # divided by the D-study item count: sigma^2(Delta)_p = sp2_p / D
  # (Brennan, 1998, eq. 20). With D != J only the divisor changes.
  J    <- 20L
  D    <- 50L
  data <- .make_identity_data(seed = 102L, N = 600L, J = J)

  fit <- suppressMessages(
    csem_gt(data, error_type = "absolute", n_items_D = D,
            smoother = "none"))

  sp2      <- apply(data, 1L, stats::var)   # per-person item variance
  expected <- sqrt(sp2 / D)

  expect_equal(fit$estimates$csem.absolute, expected, tolerance = 1e-15)
})


test_that("the uncorrelated relative estimator is a function of the observed score alone", {
  # The uncorrelated estimator drops the per-person item-residual
  # covariance term, leaving a quantity that depends on the person only
  # through the observed score; for binary data it coincides with the
  # Keats-Lord conditional error variance (Gempp, paper section 5.3).
  # Persons sharing an observed score must therefore share an identical
  # uncorrelated CSEM.
  data <- .make_identity_data(seed = 103L, N = 700L, J = 30L)

  fit <- suppressMessages(
    csem_gt(data, method = "uncorrelated", error_type = "relative",
            smoother = "none"))

  est    <- fit$estimates
  spread <- tapply(est$csem.relative_uncorrelated, est$observed_score,
                   function(v) diff(range(v)))
  expect_true(all(spread < 1e-12))
})


test_that("the absolute estimator is also constant within observed score for binary data", {
  # For dichotomous items sigma(Delta)_p depends on the person only
  # through Xbar_p (it is exactly Lord's binomial SEM), so it too is
  # constant within score. This is the property that the paper section
  # 5.3 contrasts with the full and large_a estimators, which DO admit
  # within-score heterogeneity.
  data <- .make_identity_data(seed = 104L, N = 700L, J = 30L)

  fit <- suppressMessages(
    csem_gt(data, error_type = "absolute", smoother = "none"))

  est    <- fit$estimates
  spread <- tapply(est$csem.absolute, est$observed_score,
                   function(v) diff(range(v)))
  expect_true(all(spread < 1e-12))
})


test_that("the full and large_a estimators do admit within-score heterogeneity", {
  # Complement to the previous test: the full and large_a relative
  # estimators carry the per-person cov(X_pi, item means) term, so for a
  # dataset with item heterogeneity at least one observed-score level
  # must show non-zero spread. This is the substantive finding the paper
  # builds its section 5.3 example around.
  set.seed(105L)
  # Items with widely varying difficulty -> non-degenerate cov term.
  probs <- seq(0.15, 0.85, length.out = 32L)
  data  <- sapply(probs, function(pr) rbinom(600L, 1, pr))

  fit <- suppressMessages(
    csem_gt(data, method = c("full", "large_a"), error_type = "relative",
            smoother = "none"))

  est <- fit$estimates
  spread_full <- tapply(est$csem.relative_full, est$observed_score,
                        function(v) diff(range(v)))
  spread_la   <- tapply(est$csem.relative_large_a, est$observed_score,
                        function(v) diff(range(v)))
  # At least one score level with genuine spread (ignore singleton
  # groups, whose spread is structurally zero).
  expect_true(any(spread_full > 1e-8, na.rm = TRUE))
  expect_true(any(spread_la   > 1e-8, na.rm = TRUE))
})
