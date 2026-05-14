# Tests for the csem S3 accessors in R/methods-csem.R:
# by_score(), as.data.frame.csem(), coef.csem().

# Shared fixture: a small balanced binary dataset.
.make_acc_data <- function(seed = 909L, N = 70L, J = 12L, p = 0.5) {
  set.seed(seed)
  matrix(rbinom(N * J, 1, p), nrow = N, ncol = J)
}

fit_acc <- function(data = .make_acc_data(), ...) {
  suppressMessages(csem_gt(data, ...))
}


# -----------------------------------------------------------------------------
# by_score()
# -----------------------------------------------------------------------------

test_that("by_score() returns the $by_score component unchanged", {
  fit <- fit_acc()
  expect_identical(by_score(fit), fit$by_score)
})

test_that("by_score() returns a data frame with one row per observed score", {
  fit <- fit_acc()
  bs  <- by_score(fit)
  expect_s3_class(bs, "data.frame")
  expect_equal(nrow(bs), length(unique(fit$estimates$observed_score)))
})

test_that("by_score() is generic and dispatches on class", {
  expect_true(isGeneric("by_score") ||
                is.function(by_score))           # UseMethod-style generic
  fit <- fit_acc()
  expect_identical(by_score(fit), by_score.csem(fit))
})

test_that("by_score() errors on a non-csem object", {
  expect_error(by_score(1:10), "no applicable method|not.*csem")
})


# -----------------------------------------------------------------------------
# as.data.frame.csem()
# -----------------------------------------------------------------------------

test_that("as.data.frame.csem() default returns the person-level table", {
  fit <- fit_acc()
  expect_identical(as.data.frame(fit), fit$estimates)
  expect_identical(as.data.frame(fit, by = "person"), fit$estimates)
})

test_that("as.data.frame.csem() with by = 'score' returns the score table", {
  fit <- fit_acc()
  expect_identical(as.data.frame(fit, by = "score"), fit$by_score)
})

test_that("as.data.frame.csem() rejects an invalid `by`", {
  fit <- fit_acc()
  expect_error(as.data.frame(fit, by = "bogus"), "should be one of")
})

test_that("as.data.frame.csem() ignores row.names and optional", {
  fit <- fit_acc()
  expect_identical(
    as.data.frame(fit, row.names = c("a", "b"), optional = TRUE),
    fit$estimates)
})

test_that("as.data.frame.csem() person-level table has one row per person", {
  fit <- fit_acc()
  expect_equal(nrow(as.data.frame(fit)), fit$n_persons)
})


# -----------------------------------------------------------------------------
# coef.csem()
# -----------------------------------------------------------------------------

test_that("coef.csem() returns the variance_components list", {
  fit <- fit_acc()
  expect_identical(coef(fit), fit$variance_components)
})

test_that("coef.csem() carries the expected top-level structure", {
  fit <- fit_acc()
  cf  <- coef(fit)
  expect_true(all(c("anova_table", "person", "item", "residual",
                    "population_quantities",
                    "reliability_coefficients") %in% names(cf)))
})

test_that("coef.csem() reliability_coefficients includes smoothing_diagnostics", {
  # Cross-check with the 3.1 contract: the smoothing diagnostics node is
  # part of variance_components and therefore reachable through coef().
  fit <- fit_acc()
  expect_true("smoothing_diagnostics" %in%
                names(coef(fit)$reliability_coefficients))
})
