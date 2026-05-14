# Tests for summary.csem and print.summary.csem in R/summary-csem.R.
#
# summary.csem and print.summary.csem are package-original design (there
# is no gtcsem Stata equivalent), so the snapshot tests below are
# reviewed against our own legibility criterion rather than a reference.

# Fixture: a balanced binary dataset with genuine between-person
# structure (latent ability + item difficulty on the logit scale), so
# the variance components and reliability coefficients land in a typical,
# non-degenerate range.
.make_summary_data <- function(seed = 7L, N = 100L, J = 15L) {
  set.seed(seed)
  theta <- rnorm(N, mean = 0, sd = 1.2)
  beta  <- rnorm(J, mean = 0, sd = 0.6)
  p_pi  <- plogis(outer(theta, beta, "-"))
  matrix(rbinom(N * J, 1, as.vector(p_pi)), nrow = N, ncol = J)
}


# -----------------------------------------------------------------------------
# summary.csem() structure
# -----------------------------------------------------------------------------

test_that("summary.csem() returns an object of class summary.csem", {
  fit <- suppressMessages(csem_gt(.make_summary_data()))
  expect_s3_class(summary(fit), "summary.csem")
})

test_that("summary.csem() carries the expected components", {
  fit <- suppressMessages(csem_gt(.make_summary_data()))
  s <- summary(fit)
  expect_true(all(c("call", "paradigm", "methods", "error_types",
                    "n_persons", "n_items", "global_stats",
                    "by_score_summary") %in% names(s)))
})

test_that("summary.csem() global_stats has the four global figures", {
  fit <- suppressMessages(csem_gt(.make_summary_data()))
  expect_named(summary(fit)$global_stats,
               c("relative_sem", "erho2", "absolute_sem", "phi"))
})

test_that("summary.csem() global_stats match the variance_components values", {
  fit <- suppressMessages(csem_gt(.make_summary_data()))
  gs <- summary(fit)$global_stats
  vc <- fit$variance_components
  expect_equal(gs$relative_sem, vc$population_quantities$relative_sem)
  expect_equal(gs$absolute_sem, vc$population_quantities$absolute_sem)
  expect_equal(gs$erho2, vc$reliability_coefficients$erho2)
  expect_equal(gs$phi, vc$reliability_coefficients$phi)
})


# -----------------------------------------------------------------------------
# by_score_summary
# -----------------------------------------------------------------------------

test_that("summary.csem() by_score_summary retains every original by_score column", {
  fit <- suppressMessages(csem_gt(.make_summary_data()))
  s <- summary(fit)
  expect_true(all(names(fit$by_score) %in% names(s$by_score_summary)))
})

test_that("summary.csem() by_score_summary adds cum_freq and percentile", {
  fit <- suppressMessages(csem_gt(.make_summary_data()))
  expect_true(all(c("cum_freq", "percentile") %in%
                    names(summary(fit)$by_score_summary)))
})

test_that("summary.csem() by_score_summary has one row per distinct observed score", {
  fit <- suppressMessages(csem_gt(.make_summary_data()))
  s <- summary(fit)
  expect_equal(nrow(s$by_score_summary),
               length(unique(fit$estimates$observed_score)))
})

test_that("summary.csem() cum_freq is monotone increasing and ends at 1", {
  fit <- suppressMessages(csem_gt(.make_summary_data()))
  cf <- summary(fit)$by_score_summary$cum_freq
  expect_false(is.unsorted(cf))
  expect_equal(cf[length(cf)], 1)
})

test_that("summary.csem() percentile equals 100 * cum_freq", {
  fit <- suppressMessages(csem_gt(.make_summary_data()))
  bss <- summary(fit)$by_score_summary
  expect_equal(bss$percentile, 100 * bss$cum_freq)
})

test_that("summary.csem() by_score_summary is ordered by observed_score", {
  fit <- suppressMessages(csem_gt(.make_summary_data()))
  expect_false(is.unsorted(summary(fit)$by_score_summary$observed_score))
})


# -----------------------------------------------------------------------------
# print.summary.csem()
# -----------------------------------------------------------------------------

test_that("print.summary.csem() returns the object invisibly", {
  fit <- suppressMessages(csem_gt(.make_summary_data()))
  s <- summary(fit)
  result <- expect_invisible(print(s))
  expect_identical(result, s)
})

test_that("print.summary.csem() shows the header and global statistics", {
  fit <- suppressMessages(csem_gt(.make_summary_data()))
  s <- summary(fit)
  expect_output(print(s),
                "Summary of Conditional SEMs in Generalizability Theory")
  expect_output(print(s), "Global statistics")
  expect_output(print(s), "E rho\\^2")
  expect_output(print(s), "Phi")
  expect_output(print(s), "CSEM by observed score")
})

test_that("print.summary.csem() shows csem columns but not the qualified columns", {
  fit <- suppressMessages(csem_gt(.make_summary_data()))
  s <- summary(fit)
  out <- paste(capture.output(print(s)), collapse = "\n")
  # the curated table shows the abbreviated csem point-estimate headers
  expect_true(grepl("rel_full", out))
  expect_true(grepl("rel_unc", out))
  # but not the qualified sampling-variance / SE / CI / smoothed columns
  expect_false(grepl("csem_var\\.", out))
  expect_false(grepl("se\\.analytic", out))
  expect_false(grepl("ci_low", out))
  expect_false(grepl("smoothed_csem", out))
})


# -----------------------------------------------------------------------------
# snapshots (acceptance: legible, package-original display)
# -----------------------------------------------------------------------------

test_that("summary.csem display is stable (all estimators)", {
  fit <- suppressMessages(
    csem_gt(.make_summary_data(),
            method = c("full", "large_a", "uncorrelated"),
            error_type = c("absolute", "relative")))
  expect_snapshot(print(summary(fit)))
})

test_that("summary.csem display is stable (absolute only)", {
  fit <- suppressMessages(
    csem_gt(.make_summary_data(), error_type = "absolute"))
  expect_snapshot(print(summary(fit)))
})
