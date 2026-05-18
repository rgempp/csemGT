# tests/testthat/test-csem_gt.R
#
# Structure and behaviour tests for the csem_gt() orchestrator.
# Numerical parity against the legacy R script, the .ado, and the
# algebraic identities is covered separately in test-csem_gt_parity_*.R
# and test-csem_gt_identity.R.
#
# .collapse_to_score() emits an informational message about within-score
# heterogeneity whenever full / large_a estimates are collapsed; all calls
# below are wrapped in suppressMessages() to keep the test log clean.


# Shared fixture: a small balanced binary dataset.
.make_gt_data <- function(seed = 4321L, N = 80L, J = 14L, p = 0.5) {
  set.seed(seed)
  matrix(rbinom(N * J, 1, p), nrow = N, ncol = J)
}

fit_default <- function(data = .make_gt_data(), ...) {
  suppressMessages(csem_gt(data, ...))
}

# Degenerate fixture: the base fixture with row 1 forced to an all-0
# response pattern (a floor case) and row 2 to an all-1 pattern (a ceiling
# case). This guarantees exclude_extremes has something to exclude, so the
# real branch is exercised deterministically instead of falling through to
# succeed().
.make_gt_data_degenerate <- function(seed = 4321L, N = 80L, J = 14L,
                                     p = 0.5) {
  d <- .make_gt_data(seed = seed, N = N, J = J, p = p)
  d[1, ] <- 0L
  d[2, ] <- 1L
  d
}


# -----------------------------------------------------------------------------
# S3 object structure
# -----------------------------------------------------------------------------

test_that("csem_gt() returns a well-formed csem object", {
  fit <- fit_default()
  expect_true(is.csem(fit))
  expect_s3_class(fit, "csem")
  expect_true(all(c("estimates", "by_score", "call", "paradigm", "methods",
                    "error_types", "arguments", "variance_components",
                    "smooth_fits", "bootstrap", "n_persons", "n_items",
                    "version") %in% names(fit)))
  expect_equal(fit$paradigm, "gt")
})

test_that("csem_gt() estimates has the required identifier columns and N rows", {
  data <- .make_gt_data(N = 75L, J = 12L)
  fit  <- fit_default(data)
  expect_true(all(c("person_id", "observed_score", "conditioning_value",
                    "group_size", "extreme") %in% names(fit$estimates)))
  expect_equal(nrow(fit$estimates), 75L)
  expect_equal(fit$n_persons, 75L)
  expect_equal(fit$n_items, 12L)
})

test_that("csem_gt() observed_score equals conditioning_value under conditioning = 'total'", {
  fit <- fit_default()
  expect_equal(fit$estimates$observed_score, fit$estimates$conditioning_value)
})

test_that("csem_gt() captures the call", {
  data <- .make_gt_data()
  fit  <- suppressMessages(csem_gt(data, error_type = "absolute"))
  expect_true(is.call(fit$call))
  expect_equal(fit$call[[1]], as.name("csem_gt"))
})

test_that("csem_gt() passes validate_csem()", {
  fit <- fit_default()
  expect_silent(validate_csem(fit))
})


# -----------------------------------------------------------------------------
# method / error_type filtering of the estimation columns
# -----------------------------------------------------------------------------

test_that("csem_gt() default produces all four estimators", {
  fit <- fit_default()
  for (est in c("absolute", "relative_full",
                "relative_large_a", "relative_uncorrelated")) {
    expect_true(paste0("csem.", est) %in% names(fit$estimates),
                info = est)
  }
})

test_that("csem_gt() error_type = 'absolute' yields only the absolute estimator", {
  fit <- suppressMessages(csem_gt(.make_gt_data(), error_type = "absolute"))
  expect_true("csem.absolute" %in% names(fit$estimates))
  rel_cols <- grep("^csem\\.relative", names(fit$estimates), value = TRUE)
  expect_length(rel_cols, 0L)
  expect_equal(fit$error_types, "absolute")
})

test_that("csem_gt() method = 'full' drops large_a and uncorrelated columns", {
  fit <- suppressMessages(
    csem_gt(.make_gt_data(), method = "full", error_type = "relative"))
  expect_true("csem.relative_full" %in% names(fit$estimates))
  expect_false("csem.relative_large_a" %in% names(fit$estimates))
  expect_false("csem.relative_uncorrelated" %in% names(fit$estimates))
  expect_equal(fit$methods, "full")
})

test_that("csem_gt() error_type = 'relative' with two methods yields exactly those two", {
  fit <- suppressMessages(
    csem_gt(.make_gt_data(), method = c("full", "uncorrelated"),
            error_type = "relative"))
  csem_cols <- grep("^csem\\.", names(fit$estimates), value = TRUE)
  expect_setequal(csem_cols,
                  c("csem.relative_full", "csem.relative_uncorrelated"))
})


# -----------------------------------------------------------------------------
# n_items_D — D-study extrapolation
# -----------------------------------------------------------------------------

test_that("csem_gt() n_items_D defaults to the observed number of items", {
  data <- .make_gt_data(J = 14L)
  fit  <- fit_default(data)
  expect_equal(fit$arguments$n_items_D, 14)
})

test_that("csem_gt() n_items_D rescales the absolute error variance", {
  data <- .make_gt_data(J = 14L)
  fit_J  <- suppressMessages(csem_gt(data, error_type = "absolute"))
  fit_2J <- suppressMessages(
    csem_gt(data, error_type = "absolute", n_items_D = 28L))
  # absolute error variance scales as 1/D, so doubling D halves it.
  expect_equal(fit_2J$estimates$csem_var.absolute,
               fit_J$estimates$csem_var.absolute / 2,
               tolerance = 1e-10)
})


# -----------------------------------------------------------------------------
# analytical SE / bootstrap / CI columns
# -----------------------------------------------------------------------------

test_that("csem_gt() with return_analytical = TRUE adds se.analytic columns", {
  fit <- fit_default()
  expect_true("se.analytic.absolute" %in% names(fit$estimates))
  expect_true("ci_low.analytic.absolute" %in% names(fit$estimates))
  expect_true("ci_up.analytic.absolute" %in% names(fit$estimates))
})

test_that("csem_gt() bootstrap = FALSE leaves bootstrap component NULL and no se.boot columns", {
  fit <- fit_default()
  expect_null(fit$bootstrap)
  boot_cols <- grep("\\.boot", names(fit$estimates), value = TRUE)
  expect_length(boot_cols, 0L)
})

test_that("csem_gt() bootstrap = TRUE populates bootstrap metadata and se.boot columns", {
  fit <- suppressMessages(
    csem_gt(.make_gt_data(N = 50L, J = 10L), error_type = "absolute",
            bootstrap = TRUE, R = 150L, seed = 11L))
  expect_false(is.null(fit$bootstrap))
  expect_equal(fit$bootstrap$type, "item")
  expect_equal(fit$bootstrap$R, 150L)
  expect_true("se.boot.absolute" %in% names(fit$estimates))
  expect_true("csem_var.boot.absolute" %in% names(fit$estimates))
})

test_that("csem_gt() bootstrap is reproducible with a fixed seed", {
  data <- .make_gt_data(N = 50L, J = 10L)
  f1 <- suppressMessages(
    csem_gt(data, error_type = "absolute", bootstrap = TRUE,
            R = 120L, seed = 99L))
  f2 <- suppressMessages(
    csem_gt(data, error_type = "absolute", bootstrap = TRUE,
            R = 120L, seed = 99L))
  expect_equal(f1$estimates$se.boot.absolute,
               f2$estimates$se.boot.absolute)
})

test_that("csem_gt() bootstrap_type = 'person' runs and is tagged", {
  fit <- suppressMessages(
    csem_gt(.make_gt_data(N = 50L, J = 10L), error_type = "absolute",
            bootstrap = TRUE, bootstrap_type = "person",
            R = 120L, seed = 7L))
  expect_equal(fit$bootstrap$type, "person")
  expect_true("se.boot.absolute" %in% names(fit$estimates))
})

test_that("csem_gt() boot_keep_replicates = TRUE retains the replicate summary", {
  fit <- suppressMessages(
    csem_gt(.make_gt_data(N = 40L, J = 10L), error_type = "absolute",
            bootstrap = TRUE, R = 120L, seed = 3L,
            boot_keep_replicates = TRUE))
  expect_false(is.null(fit$bootstrap$replicates))
})

test_that("csem_gt() does not leak the RNG state into the caller's session", {
  data <- .make_gt_data(N = 40L, J = 8L)
  set.seed(2024L)
  before <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  invisible(suppressMessages(
    csem_gt(data, error_type = "absolute", bootstrap = TRUE,
            R = 120L, seed = 555L)))
  after <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  expect_identical(before, after)
})


# -----------------------------------------------------------------------------
# variance components / reliability coefficients / cutpoint
# -----------------------------------------------------------------------------

test_that("csem_gt() variance_components has the expected structure", {
  fit <- fit_default()
  vc <- fit$variance_components
  expect_true(all(c("anova_table", "person", "item", "residual",
                    "population_quantities",
                    "reliability_coefficients") %in% names(vc)))
  expect_s3_class(vc$anova_table, "data.frame")
expect_named(vc$population_quantities,
               c("absolute_error_var", "absolute_sem",
                 "relative_error_var", "relative_sem"))
expect_named(vc$reliability_coefficients,
               c("erho2", "phi", "phi_lambda"))
})

test_that("csem_gt() phi_lambda is NA without a cutpoint and finite with one", {
  data <- .make_gt_data()
  fit_no  <- fit_default(data)
  fit_cut <- suppressMessages(csem_gt(data, cutpoint = 0.5))
  expect_true(is.na(fit_no$variance_components$reliability_coefficients$phi_lambda))
  expect_true(is.finite(
    fit_cut$variance_components$reliability_coefficients$phi_lambda))
})


# -----------------------------------------------------------------------------
# smoothing
# -----------------------------------------------------------------------------

test_that("csem_gt() smoother = 'polynomial' adds smoothed columns and diagnostics", {
  fit <- fit_default()
  expect_true("smoothed_csem.absolute" %in% names(fit$estimates))
  expect_false(is.null(fit$smooth_fits))
  expect_true("absolute" %in% names(fit$smooth_fits))
  expect_named(fit$smooth_fits$absolute,
               c("b0", "b1", "b2", "R2", "RMSE", "N"))
})

test_that("csem_gt() smoother = 'none' yields no smoothed columns and NULL smooth_fits", {
  fit <- suppressMessages(csem_gt(.make_gt_data(), smoother = "none"))
  smoothed <- grep("^smoothed_", names(fit$estimates), value = TRUE)
  expect_length(smoothed, 0L)
  expect_null(fit$smooth_fits)
})

test_that("csem_gt() exclude_extremes leaves smoothed values NA at floor and ceiling", {
  # The degenerate fixture forces a floor and a ceiling case, so the
  # exclude_extremes branch is exercised deterministically (no succeed()).
  fit <- suppressMessages(
    csem_gt(.make_gt_data_degenerate(), error_type = "absolute",
            exclude_extremes = TRUE))
  at_extreme <- fit$estimates$extreme
  expect_true(any(at_extreme))
  expect_true(all(is.na(fit$estimates$smoothed_csem.absolute[at_extreme])))
  expect_true(all(!is.na(
    fit$estimates$smoothed_csem.absolute[!at_extreme])))
})


# -----------------------------------------------------------------------------
# by_score component
# -----------------------------------------------------------------------------

test_that("csem_gt() by_score has one row per distinct observed score", {
  data <- .make_gt_data()
  fit  <- fit_default(data)
  expect_s3_class(fit$by_score, "data.frame")
  expect_true(all(c("observed_score", "group_size") %in%
                  names(fit$by_score)))
  expect_equal(nrow(fit$by_score),
               length(unique(fit$estimates$observed_score)))
  # group_size sums back to N.
  expect_equal(sum(fit$by_score$group_size), fit$n_persons)
})

test_that("csem_gt() exclude_extremes populates smoothing_diagnostics", {
  fit <- suppressMessages(
    csem_gt(.make_gt_data_degenerate(), error_type = "absolute",
            exclude_extremes = TRUE))
  sd <- fit$diagnostics
  expect_named(sd, c("n_floor", "n_ceiling", "n_fit"))
  expect_true(sd$n_floor   >= 1L)
  expect_true(sd$n_ceiling >= 1L)
  expect_equal(sd$n_fit, fit$n_persons - sd$n_floor - sd$n_ceiling)
  # n_fit must agree with the per-estimator smoother diagnostics.
  expect_equal(sd$n_fit, fit$smooth_fits$absolute$N)
})

test_that("csem_gt() smoothing_diagnostics are NA without exclude_extremes", {
  fit <- fit_default()
  sd  <- fit$diagnostics
  expect_named(sd, c("n_floor", "n_ceiling", "n_fit"))
  expect_true(all(is.na(unlist(sd))))
})

test_that("csem_gt() smoothing_diagnostics present and NA when smoother = 'none'", {
  fit <- suppressMessages(csem_gt(.make_gt_data(), smoother = "none"))
  sd  <- fit$diagnostics
  expect_named(sd, c("n_floor", "n_ceiling", "n_fit"))
  expect_true(all(is.na(unlist(sd))))
})



# -----------------------------------------------------------------------------
# conditioning argument
# -----------------------------------------------------------------------------

test_that("csem_gt() accepts a custom numeric conditioning vector", {
  data <- .make_gt_data(N = 60L, J = 10L)
  cond <- rowMeans(data)  # equivalent to "total" here, but passed explicitly
  fit  <- suppressMessages(
    csem_gt(data, error_type = "absolute", conditioning = cond))
  expect_equal(fit$estimates$conditioning_value, unname(cond))
  expect_equal(fit$arguments$conditioning, "custom")
})

test_that("csem_gt() rejects a conditioning vector of the wrong length", {
  data <- .make_gt_data(N = 60L, J = 10L)
  expect_error(
    csem_gt(data, conditioning = rep(0.5, 59L)),
    "length nrow\\(data\\)"
  )
})


# -----------------------------------------------------------------------------
# input validation
# -----------------------------------------------------------------------------

test_that("csem_gt() rejects non-numeric data", {
  bad <- data.frame(a = letters[1:5], b = letters[1:5])
  expect_error(csem_gt(bad), "numeric")
})

test_that("csem_gt() rejects an invalid method", {
  expect_error(
    csem_gt(.make_gt_data(), method = "bogus"),
    "Invalid method"
  )
})

test_that("csem_gt() rejects an invalid error_type", {
  expect_error(
    csem_gt(.make_gt_data(), error_type = "bogus"),
    "Invalid error_type"
  )
})

test_that("csem_gt() rejects ci_level outside (0, 1)", {
  expect_error(
    csem_gt(.make_gt_data(), ci_level = 1.5),
    "ci_level"
  )
})

test_that("csem_gt() rejects R below 100", {
  expect_error(
    csem_gt(.make_gt_data(), R = 50),
    "`R` must be"
  )
})

test_that("csem_gt() with na_action = 'fail' errors on missing data", {
  data <- .make_gt_data(N = 40L, J = 8L)
  data[1, 1] <- NA
  expect_error(
    csem_gt(data, na_action = "fail"),
    "Missing values"
  )
})

test_that("csem_gt() with na_action = 'listwise' drops incomplete rows", {
  data <- .make_gt_data(N = 41L, J = 8L)
  data[1, 1] <- NA
  fit <- suppressMessages(
    csem_gt(data, error_type = "absolute", na_action = "listwise"))
  expect_equal(fit$n_persons, 40L)
  expect_equal(nrow(fit$estimates), 40L)
})

test_that("csem_gt() rejects a non-list smoother_args", {
  expect_error(
    csem_gt(.make_gt_data(), smoother_args = 2),
    "smoother_args"
  )
})


# -----------------------------------------------------------------------------
# arguments record
# -----------------------------------------------------------------------------

test_that("csem_gt() records the resolved arguments", {
  data <- .make_gt_data()
  fit  <- suppressMessages(
    csem_gt(data, error_type = "absolute", bootstrap = TRUE, R = 200L,
            seed = 42L, ci_level = 0.90, cutpoint = 0.5))
  args <- fit$arguments
  expect_equal(args$bootstrap, TRUE)
  expect_equal(args$R, 200L)
  expect_equal(args$seed, 42L)
  expect_equal(args$ci_level, 0.90)
  expect_equal(args$cutpoint, 0.5)
  expect_equal(args$bootstrap_type, "item")
  expect_equal(args$na_action, "listwise")
})
