# Tests for R/utils-bootstrap.R
# Covers .item_bootstrap(), .person_bootstrap(), .gt_add_bootstrap_se().
#
# Sprint 2 sub-fase 4(b) refactor:
# -------------------------------
# - The Sprint 1 helpers .add_bootstrap_ci() and .add_analytical_ci() were
#   removed; their tests are gone with them. SE attachment is now tested
#   here (.gt_add_bootstrap_se()) and in test-utils-gt-longwide.R
#   (.gt_add_analytical_se()); CI attachment is tested in
#   test-utils-gt-longwide.R (.gt_add_ci()).
# - .item_bootstrap() and .person_bootstrap() are unchanged from sub-fase 3;
#   their tests are carried over verbatim.
#
# Mock targets:
# - .item_bootstrap()   mocks .gt_compute_per_person_for_boot()
#                       (signature (xrow, b_full, sigma2_i, n_items_D, N, B),
#                       returns a B x 4 matrix).
# - .person_bootstrap() mocks .gt_compute_per_person()
#                       (signature (vc, n_items_D), returns an N x 4 matrix).


# -------------------------------------------------------------------
# .item_bootstrap (mocking .gt_compute_per_person_for_boot)
# -------------------------------------------------------------------

test_that(".item_bootstrap returns the expected list structure", {
  # Deterministic mock: depends on the resampled item indices via the
  # data row passed in, so different replicates produce different
  # values, matching the qualitative behaviour of the real helper.
  fake_boot_helper <- function(xrow, b_full, sigma2_i, n_items_D, N, B) {
    J <- length(xrow)
    idx <- matrix(sample.int(J, B * J, replace = TRUE), B, J)
    sums <- rowSums(matrix(xrow[idx], B, J))
    cbind(absolute              = sums,
          relative_full         = sums * 0.9,
          relative_large_a      = sums * 0.8,
          relative_uncorrelated = sums * 0.7)
  }
  testthat::local_mocked_bindings(
    .gt_compute_per_person_for_boot = fake_boot_helper
  )

  set.seed(123L)
  data <- matrix(rbinom(20 * 6, 1, 0.5), nrow = 20)
  vc   <- .gt_variance_components(data)

  out <- .item_bootstrap(data, vc, n_items_D = 6L,
                         R = 200L, seed = 42L)
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
  fake_boot_helper <- function(xrow, b_full, sigma2_i, n_items_D, N, B) {
    J <- length(xrow)
    idx <- matrix(sample.int(J, B * J, replace = TRUE), B, J)
    sums <- rowSums(matrix(xrow[idx], B, J))
    cbind(absolute = sums, relative_full = sums + 1,
          relative_large_a = sums + 2, relative_uncorrelated = sums + 3)
  }
  testthat::local_mocked_bindings(
    .gt_compute_per_person_for_boot = fake_boot_helper
  )

  set.seed(1L)
  data <- matrix(rbinom(15 * 5, 1, 0.5), nrow = 15)
  vc   <- .gt_variance_components(data)

  o1 <- .item_bootstrap(data, vc, n_items_D = 5L, R = 150L, seed = 7L)
  o2 <- .item_bootstrap(data, vc, n_items_D = 5L, R = 150L, seed = 7L)
  expect_equal(o1$per_person_variance, o2$per_person_variance)
})

test_that(".item_bootstrap restores .Random.seed exactly after run", {
  fake_boot_helper <- function(xrow, b_full, sigma2_i, n_items_D, N, B) {
    # Even the mock must consume the PRNG identically across calls so
    # the seed restore test isolates the bootstrap orchestration code.
    J <- length(xrow)
    idx <- matrix(sample.int(J, B * J, replace = TRUE), B, J)
    matrix(0.1, nrow = B, ncol = 4L,
           dimnames = list(NULL,
             c("absolute", "relative_full",
               "relative_large_a", "relative_uncorrelated")))
  }
  testthat::local_mocked_bindings(
    .gt_compute_per_person_for_boot = fake_boot_helper
  )

  set.seed(99L)
  data <- matrix(rbinom(20, 1, 0.5), nrow = 5)
  vc   <- .gt_variance_components(data)
  before <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  invisible(.item_bootstrap(data, vc, n_items_D = 4L,
                            R = 120L, seed = 11L))
  after <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  expect_identical(before, after)
})

test_that(".item_bootstrap returns replicates when requested", {
  fake_boot_helper <- function(xrow, b_full, sigma2_i, n_items_D, N, B) {
    s <- sum(xrow)
    matrix(s, nrow = B, ncol = 4L,
           dimnames = list(NULL,
             c("absolute", "relative_full",
               "relative_large_a", "relative_uncorrelated")))
  }
  testthat::local_mocked_bindings(
    .gt_compute_per_person_for_boot = fake_boot_helper
  )

  set.seed(5L)
  data <- matrix(rbinom(40, 1, 0.5), nrow = 10)
  vc   <- .gt_variance_components(data)
  out  <- .item_bootstrap(data, vc, n_items_D = 4L,
                          R = 120L, seed = 2L,
                          return_replicates = TRUE)
  expect_true(!is.null(out$replicates))
  expect_equal(dim(out$replicates), c(10L, 4L))
})


# -------------------------------------------------------------------
# .person_bootstrap (mocking .gt_compute_per_person)
# -------------------------------------------------------------------

test_that(".person_bootstrap returns the expected list structure", {
  fake_pp <- function(vc, n_items_D) {
    matrix(rep(1, 4 * vc$N), nrow = vc$N, ncol = 4L,
           dimnames = list(NULL,
             c("absolute", "relative_full",
               "relative_large_a", "relative_uncorrelated")))
  }
  testthat::local_mocked_bindings(
    .gt_compute_per_person = fake_pp
  )

  set.seed(3L)
  data <- matrix(rbinom(10 * 4, 1, 0.5), nrow = 10)
  vc   <- .gt_variance_components(data)
  X    <- rowSums(data)
  out  <- .person_bootstrap(data, vc, n_items_D = 4L, X = X,
                            paradigm = "gt",
                            method   = "full",
                            error_type = "relative",
                            R = 120L, seed = 4L)
  expect_named(out, c("type", "R", "seed", "per_person_variance"))
  expect_equal(out$type, "person")
  expect_equal(dim(out$per_person_variance), c(10L, 4L))
})

test_that(".person_bootstrap variance is zero when estimator is constant across replicates", {
  fake_pp <- function(vc, n_items_D) {
    matrix(rep(0.5, 4 * vc$N), nrow = vc$N, ncol = 4L,
           dimnames = list(NULL,
             c("absolute", "relative_full",
               "relative_large_a", "relative_uncorrelated")))
  }
  testthat::local_mocked_bindings(
    .gt_compute_per_person = fake_pp
  )

  set.seed(7L)
  data <- matrix(rbinom(10 * 5, 1, 0.5), nrow = 10)
  vc   <- .gt_variance_components(data)
  X    <- rowSums(data)
  out  <- .person_bootstrap(data, vc, n_items_D = 5L, X = X,
                            R = 120L, seed = 1L)
  expect_true(all(out$per_person_variance == 0 |
                  is.na(out$per_person_variance)))
})

test_that(".person_bootstrap rejects non-gt paradigm in csemGT", {
  data <- matrix(0, 5, 5)
  vc   <- list(N = 5L, J = 5L)
  expect_error(
    .person_bootstrap(data, vc, n_items_D = 5L,
                      X = rep(0, 5), paradigm = "split_half"),
    "paradigm = 'gt'"
  )
})


# -------------------------------------------------------------------
# .gt_add_bootstrap_se
# -------------------------------------------------------------------

test_that(".gt_add_bootstrap_se adds csem_var.boot and se.boot", {
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
  res <- .gt_add_bootstrap_se(pp, boot)
  expect_true(all(c("csem_var.boot", "se.boot") %in% names(res)))
  # Delta-method conversion: var(V) / (4 * csem^2).
  expect_equal(res$csem_var.boot[1], 0.001 / (4 * 0.20^2),
               tolerance = 1e-12)
  expect_equal(res$se.boot, sqrt(res$csem_var.boot), tolerance = 1e-12)
})

test_that(".gt_add_bootstrap_se delta method matches the closed form", {
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
  res <- .gt_add_bootstrap_se(pp, boot)
  # var(V) = 0.04, csem = 0.5 -> csem_var.boot = 0.04 / (4 * 0.25) = 0.04
  expect_equal(res$csem_var.boot, 0.04 / (4 * 0.5^2), tolerance = 1e-12)
  expect_equal(res$se.boot, sqrt(0.04 / (4 * 0.5^2)), tolerance = 1e-12)
})

test_that(".gt_add_bootstrap_se gives NA where csem <= 0", {
  pp <- data.frame(
    person_id = 1:3,
    estimator = rep("absolute", 3),
    csem      = c(0.0, -0.1, 0.4),
    stringsAsFactors = FALSE
  )
  boot <- list(
    per_person_variance = matrix(
      c(0.01, 0.01, 0.01, rep(NA_real_, 9)),
      nrow = 3, ncol = 4,
      dimnames = list(NULL,
        c("absolute", "relative_full",
          "relative_large_a", "relative_uncorrelated"))
    )
  )
  res <- .gt_add_bootstrap_se(pp, boot)
  expect_true(is.na(res$csem_var.boot[1]))
  expect_true(is.na(res$csem_var.boot[2]))
  expect_false(is.na(res$csem_var.boot[3]))
})

test_that(".gt_add_bootstrap_se handles multiple estimators independently", {
  pp <- data.frame(
    person_id = rep(1:2, times = 2),
    estimator = rep(c("absolute", "relative_full"), each = 2),
    csem      = c(0.30, 0.40, 0.30, 0.40),
    stringsAsFactors = FALSE
  )
  boot <- list(
    per_person_variance = matrix(
      c(0.001, 0.002,            # absolute
        0.003, 0.004,            # relative_full
        NA, NA, NA, NA),         # large_a, uncorrelated unused
      nrow = 2, ncol = 4,
      dimnames = list(NULL,
        c("absolute", "relative_full",
          "relative_large_a", "relative_uncorrelated"))
    )
  )
  res <- .gt_add_bootstrap_se(pp, boot)
  sel_abs  <- res$estimator == "absolute"
  sel_full <- res$estimator == "relative_full"
  expect_equal(res$csem_var.boot[sel_abs][1],
               0.001 / (4 * 0.30^2), tolerance = 1e-12)
  expect_equal(res$csem_var.boot[sel_full][1],
               0.003 / (4 * 0.30^2), tolerance = 1e-12)
})

test_that(".gt_add_bootstrap_se warns when estimator missing from boot", {
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
    .gt_add_bootstrap_se(pp, boot),
    "not available"
  )
})

test_that(".gt_add_bootstrap_se rejects malformed inputs", {
  expect_error(
    .gt_add_bootstrap_se(data.frame(x = 1), list()),
    "person_id"
  )
  expect_error(
    .gt_add_bootstrap_se(
      data.frame(person_id = 1, csem = 0.1, estimator = "absolute"),
      list()),
    "per_person_variance"
  )
})
