# Tests for R/utils-bootstrap.R
# Covers .item_bootstrap(), .person_bootstrap(), .add_bootstrap_ci(),
# .add_analytical_ci().
#
# The bootstrap functions depend on .gt_estimators_for_person() and
# .gt_compute_per_person_for_boot() which are implemented in Sprint 2.
# We mock those dependencies here so Sprint 1 tests exercise the
# orchestration code without depending on the GT estimator algebra.

# -------------------------------------------------------------------
# .item_bootstrap (with mocked .gt_estimators_for_person)
# -------------------------------------------------------------------

test_that(".item_bootstrap returns the expected list structure", {
  fake_estimator <- function(data, p, X) {
    # Deterministic, easy to verify: depends only on the person's
    # row sum so resampled item sets produce different values.
    s <- sum(data[p, ])
    c(s, s * 0.9, s * 0.8, s * 0.7)
  }
  testthat::local_mocked_bindings(
    .gt_estimators_for_person = fake_estimator
  )

  set.seed(123L)
  data <- matrix(rbinom(20 * 6, 1, 0.5), nrow = 20)
  X    <- rowSums(data)

  out <- .item_bootstrap(data, X, R = 200L, seed = 42L)
  expect_named(out, c("type", "R", "seed", "per_person_variance"))
  expect_equal(out$type, "item")
  expect_equal(out$R, 200L)
  expect_equal(out$seed, 42L)
  expect_equal(dim(out$per_person_variance), c(20L, 4L))
  expect_equal(colnames(out$per_person_variance),
               c("absolute", "relative_full",
                 "relative_large_a", "relative_uncorrelated"))
})

test_that(".item_bootstrap respects seed: identical seeds -> identical output", {
  fake_estimator <- function(data, p, X) {
    s <- sum(data[p, ])
    c(s, s + 1, s + 2, s + 3)
  }
  testthat::local_mocked_bindings(
    .gt_estimators_for_person = fake_estimator
  )

  set.seed(1L)
  data <- matrix(rbinom(15 * 5, 1, 0.5), nrow = 15)
  X    <- rowSums(data)

  o1 <- .item_bootstrap(data, X, R = 150L, seed = 7L)
  o2 <- .item_bootstrap(data, X, R = 150L, seed = 7L)
  expect_equal(o1$per_person_variance, o2$per_person_variance)
})

test_that(".item_bootstrap restores .Random.seed exactly after run", {
  fake_estimator <- function(data, p, X) c(0.1, 0.1, 0.1, 0.1)
  testthat::local_mocked_bindings(
    .gt_estimators_for_person = fake_estimator
  )

  set.seed(99L)
  data <- matrix(rbinom(20, 1, 0.5), nrow = 5)
  X    <- rowSums(data)
  # Snapshot AFTER all data setup so the comparison isolates the
  # bootstrap call's effect on the RNG state.
  before <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  invisible(.item_bootstrap(data, X, R = 120L, seed = 11L))
  after <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  expect_identical(before, after)
})

test_that(".item_bootstrap returns replicates when requested", {
  fake_estimator <- function(data, p, X) {
    s <- sum(data[p, ])
    c(s, s, s, s)
  }
  testthat::local_mocked_bindings(
    .gt_estimators_for_person = fake_estimator
  )

  set.seed(5L)
  data <- matrix(rbinom(40, 1, 0.5), nrow = 10)
  X    <- rowSums(data)
  out  <- .item_bootstrap(data, X, R = 120L, seed = 2L,
                          return_replicates = TRUE)
  expect_true(!is.null(out$replicates))
  expect_equal(dim(out$replicates), c(10L, 4L))
})

# -------------------------------------------------------------------
# .person_bootstrap (with mocked .gt_compute_per_person_for_boot)
# -------------------------------------------------------------------

test_that(".person_bootstrap returns the expected list structure", {
  fake_pp_boot <- function(data, X, method, error_type) {
    matrix(rep(1, 4 * nrow(data)), nrow = nrow(data), ncol = 4L,
           dimnames = list(NULL,
             c("absolute", "relative_full",
               "relative_large_a", "relative_uncorrelated")))
  }
  testthat::local_mocked_bindings(
    .gt_compute_per_person_for_boot = fake_pp_boot
  )

  set.seed(3L)
  data <- matrix(rbinom(10 * 4, 1, 0.5), nrow = 10)
  X    <- rowSums(data)
  out  <- .person_bootstrap(data, X, paradigm = "gt",
                            method     = "full",
                            error_type = "relative",
                            R = 120L, seed = 4L)
  expect_named(out, c("type", "R", "seed", "per_person_variance"))
  expect_equal(out$type, "person")
  expect_equal(dim(out$per_person_variance), c(10L, 4L))
})

test_that(".person_bootstrap variance is zero when estimator is constant across replicates", {
  fake_pp_boot <- function(data, X, method, error_type) {
    # All persons get identical estimates regardless of resample
    matrix(rep(0.5, 4 * nrow(data)), nrow = nrow(data), ncol = 4L,
           dimnames = list(NULL,
             c("absolute", "relative_full",
               "relative_large_a", "relative_uncorrelated")))
  }
  testthat::local_mocked_bindings(
    .gt_compute_per_person_for_boot = fake_pp_boot
  )

  set.seed(7L)
  data <- matrix(rbinom(10 * 5, 1, 0.5), nrow = 10)
  X    <- rowSums(data)
  out  <- .person_bootstrap(data, X, R = 120L, seed = 1L)
  expect_true(all(out$per_person_variance == 0 |
                  is.na(out$per_person_variance)))
})

test_that(".person_bootstrap rejects non-gt paradigm in csemGT", {
  expect_error(
    .person_bootstrap(matrix(0, 5, 5), rep(0, 5), paradigm = "split_half"),
    "paradigm = 'gt'"
  )
})

# -------------------------------------------------------------------
# .add_bootstrap_ci
# -------------------------------------------------------------------

test_that(".add_bootstrap_ci adds csem_var.boot, se.boot, ci_low.boot, ci_up.boot", {
  pp <- data.frame(
    person_id = 1:5,
    estimator = rep("absolute", 5),
    csem      = c(0.20, 0.30, 0.40, 0.50, 0.60),
    stringsAsFactors = FALSE
  )
  boot <- list(
    type = "item", R = 100L, seed = 1L,
    per_person_variance = matrix(
      c(0.001, 0.002, 0.003, 0.004, 0.005,
        rep(NA_real_, 15)),
      nrow = 5, ncol = 4,
      dimnames = list(NULL,
        c("absolute", "relative_full",
          "relative_large_a", "relative_uncorrelated"))
    )
  )
  res <- .add_bootstrap_ci(pp, boot, ci_method = "normal",
                           ci_level = 0.95)
  expect_true(all(c("csem_var.boot", "se.boot",
                    "ci_low.boot", "ci_up.boot") %in% names(res)))
  # Delta method: var(csem) = var(V) / (4 V); check first row.
  expect_equal(res$csem_var.boot[1], 0.001 / (4 * 0.20^2),
               tolerance = 1e-12)
})

test_that(".add_bootstrap_ci normal CI uses z * SE correctly", {
  pp <- data.frame(
    person_id = 1L,
    estimator = "absolute",
    csem      = 0.5,
    stringsAsFactors = FALSE
  )
  boot <- list(
    type = "item", R = 100L,
    per_person_variance = matrix(
      c(0.04, NA, NA, NA), nrow = 1, ncol = 4,
      dimnames = list(NULL,
        c("absolute", "relative_full",
          "relative_large_a", "relative_uncorrelated"))
    )
  )
  res <- .add_bootstrap_ci(pp, boot, ci_method = "normal",
                           ci_level = 0.95)
  z  <- qnorm(0.975)
  se <- sqrt(0.04 / (4 * 0.5^2))
  expect_equal(res$ci_up.boot,  0.5 + z * se, tolerance = 1e-12)
  expect_equal(res$ci_low.boot, max(0.5 - z * se, 0), tolerance = 1e-12)
})

test_that(".add_bootstrap_ci falls back to normal when no replicates available", {
  pp <- data.frame(
    person_id = 1L, estimator = "absolute", csem = 0.4,
    stringsAsFactors = FALSE
  )
  boot <- list(
    per_person_variance = matrix(
      c(0.01, NA, NA, NA), nrow = 1, ncol = 4,
      dimnames = list(NULL,
        c("absolute", "relative_full",
          "relative_large_a", "relative_uncorrelated"))
    )
  )
  res <- .add_bootstrap_ci(pp, boot, ci_method = "percentile",
                           ci_level = 0.95)
  expect_equal(attr(res, "ci_method"), "normal")
  expect_false(any(is.na(res$ci_low.boot)))
})

test_that(".add_bootstrap_ci warns when estimator missing from boot", {
  pp <- data.frame(
    person_id = 1L, estimator = "relative_full", csem = 0.5,
    stringsAsFactors = FALSE
  )
  boot <- list(
    per_person_variance = matrix(
      0.02, nrow = 1, ncol = 4,
      dimnames = list(NULL,
        c("absolute", "wrong_name", "x", "y"))
    )
  )
  expect_warning(
    .add_bootstrap_ci(pp, boot, ci_method = "normal"),
    "not available"
  )
})

test_that(".add_bootstrap_ci rejects malformed inputs", {
  expect_error(
    .add_bootstrap_ci(data.frame(x = 1), list()),
    "person_id"
  )
  expect_error(
    .add_bootstrap_ci(
      data.frame(person_id = 1, csem = 0.1, estimator = "absolute"),
      list()),
    "per_person_variance"
  )
})

# -------------------------------------------------------------------
# .add_analytical_ci
# -------------------------------------------------------------------

test_that(".add_analytical_ci adds se.analytic, ci_low.analytic, ci_up.analytic", {
  pp <- data.frame(
    csem               = c(0.20, 0.30, 0.40),
    csem_var.analytic  = c(0.001, 0.002, 0.003),
    stringsAsFactors   = FALSE
  )
  res <- .add_analytical_ci(pp, ci_level = 0.95, paradigm = "gt")
  expect_true(all(c("se.analytic", "ci_low.analytic", "ci_up.analytic") %in%
                  names(res)))
  expect_equal(res$se.analytic, sqrt(pp$csem_var.analytic),
               tolerance = 1e-12)
})

test_that(".add_analytical_ci ci_up = csem + z*SE", {
  pp <- data.frame(csem = 0.5, csem_var.analytic = 0.01,
                   stringsAsFactors = FALSE)
  res <- .add_analytical_ci(pp, ci_level = 0.95)
  expect_equal(res$ci_up.analytic, 0.5 + qnorm(0.975) * 0.1,
               tolerance = 1e-12)
})

test_that(".add_analytical_ci ci_low never goes below zero", {
  pp <- data.frame(csem = 0.05, csem_var.analytic = 0.01,
                   stringsAsFactors = FALSE)
  res <- .add_analytical_ci(pp, ci_level = 0.95)
  expect_gte(res$ci_low.analytic, 0)
})

test_that(".add_analytical_ci rejects malformed inputs", {
  expect_error(.add_analytical_ci(data.frame(x = 1)), "csem")
  expect_error(.add_analytical_ci(data.frame(csem = 0.1),
                                  ci_level = 0), "ci_level")
})
