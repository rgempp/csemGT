# Tests for print.csem in R/print-csem.R.
#
# The acceptance criterion (mini-spec v1.1 §6.5) is a console display
# indistinguishable from the gtcsem Stata command; that is checked by the
# snapshot tests below. The remaining tests pin the block-presence logic:
# the smoothing-fits block appears only when smoothing was applied, the
# cutpoint line only when a cutpoint was supplied, and the mean-variance
# block reports an Analytical and/or a Bootstrap column according to which
# sampling-variance columns the object carries.

# Fixture: a balanced binary dataset with genuine between-person
# structure. Each person carries a latent ability and each item a
# difficulty (both on the logit scale); responses are Bernoulli draws
# from plogis(theta_p - beta_i). Unlike a flat rbinom(., 0.5) matrix,
# this yields a positive person variance component and reliability
# coefficients in (0, 1), so the print.csem snapshots read as a typical
# run rather than a degenerate one.
.make_print_data <- function(seed = 2024L, N = 90L, J = 16L) {
  set.seed(seed)
  theta <- rnorm(N, mean = 0, sd = 1.2)   # person ability, logit scale
  beta  <- rnorm(J, mean = 0, sd = 0.6)   # item difficulty, logit scale
  p_pi  <- plogis(outer(theta, beta, "-"))
  matrix(rbinom(N * J, 1, as.vector(p_pi)), nrow = N, ncol = J)
}

# -----------------------------------------------------------------------------
# return value
# -----------------------------------------------------------------------------

test_that("print.csem returns the object invisibly", {
  fit <- suppressMessages(csem_gt(.make_print_data(), error_type = "absolute"))
  result <- expect_invisible(print(fit))
  expect_identical(result, fit)
})


# -----------------------------------------------------------------------------
# block-presence logic
# -----------------------------------------------------------------------------

test_that("print.csem always shows the core blocks", {
  fit <- suppressMessages(csem_gt(.make_print_data(), error_type = "absolute"))
  expect_output(print(fit), "Conditional SEMs in Generalizability Theory")
  expect_output(print(fit), "ANOVA table")
  expect_output(print(fit), "D-study error variances and SEMs")
  expect_output(print(fit), "Reliability-like coefficients")
})

test_that("print.csem shows the smoothing block only when smoothing was applied", {
  fit_sm <- suppressMessages(csem_gt(.make_print_data(), error_type = "absolute"))
  fit_no <- suppressMessages(
    csem_gt(.make_print_data(), error_type = "absolute", smoother = "none"))
  expect_output(print(fit_sm), "Quadratic smoothing fits")
  out_no <- capture.output(print(fit_no))
  expect_false(any(grepl("Quadratic smoothing fits", out_no)))
})

test_that("print.csem shows the cutpoint line only when a cutpoint was supplied", {
  fit_cut <- suppressMessages(csem_gt(.make_print_data(), cutpoint = 0.5))
  fit_no  <- suppressMessages(csem_gt(.make_print_data()))
  expect_output(print(fit_cut), "Phi\\(lambda\\)")
  expect_output(print(fit_cut), "Cutpoint")
  out_no <- capture.output(print(fit_no))
  expect_false(any(grepl("Phi\\(lambda\\)", out_no)))
  expect_false(any(grepl("^Cutpoint", out_no)))
})

test_that("print.csem mean-variance block reports Analytical without bootstrap", {
  fit <- suppressMessages(csem_gt(.make_print_data(), error_type = "absolute"))
  expect_output(print(fit), "Mean variance of estimator across persons")
  expect_output(print(fit), "Analytical")
  out <- capture.output(print(fit))
  expect_false(any(grepl("Bootstrap", out)))
})

test_that("print.csem mean-variance block reports Bootstrap when bootstrap was run", {
  fit <- suppressMessages(
    csem_gt(.make_print_data(N = 50L, J = 10L), error_type = "absolute",
            bootstrap = TRUE, R = 150L, seed = 123L))
  expect_output(print(fit), "Bootstrap")
})

test_that("print.csem reports D-study extrapolation when n_items_D differs", {
  fit <- suppressMessages(
    csem_gt(.make_print_data(J = 16L), error_type = "absolute",
            n_items_D = 32L))
  expect_output(print(fit), "extrapolated")
})


# -----------------------------------------------------------------------------
# snapshots (acceptance: display indistinguishable from gtcsem)
# -----------------------------------------------------------------------------

test_that("print.csem display is stable (analytical, smoothed, cutpoint)", {
  fit <- suppressMessages(
    csem_gt(.make_print_data(), method = c("full", "large_a", "uncorrelated"),
            error_type = c("absolute", "relative"), cutpoint = 0.5))
  expect_snapshot(print(fit))
})

test_that("print.csem display is stable (no smoother)", {
  fit <- suppressMessages(
    csem_gt(.make_print_data(), error_type = "absolute", smoother = "none"))
  expect_snapshot(print(fit))
})

test_that("print.csem display is stable (bootstrap, both SE sources)", {
  fit <- suppressMessages(
    csem_gt(.make_print_data(N = 60L, J = 12L),
            error_type = c("absolute", "relative"),
            bootstrap = TRUE, R = 200L, seed = 123L,
            return_analytical = TRUE))
  expect_snapshot(print(fit))
})
