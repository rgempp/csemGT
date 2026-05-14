# tests/testthat/test-utils-gt-boot.R
#
# Tests for `.gt_compute_per_person()` and `.gt_compute_per_person_for_boot()`
# in R/utils-gt.R, and bit-exact bootstrap parity against the legacy
# `bootstrap_csem_g1f()` in `inst/legacy/csem_gt_estimation_v3.R`.
#
# Parity strategy
# ---------------
# Both implementations consume the PRNG in the same pattern: ONE call to
# sample.int(J, B*J, replace = TRUE) per person, in person-major order.
# Under set.seed() they produce bit-identical idx_mat, hence bit-identical
# Xb, bb, row_var_b, cov_b, and final estimators. We assert bit-exactness
# at 1e-12 absolute, which is essentially machine-precision for these
# magnitudes.

source(system.file("legacy", "csem_gt_estimation_v3.R",
                   package  = "csemGT",
                   mustWork = TRUE))


# -----------------------------------------------------------------------------
# .gt_compute_per_person()
# -----------------------------------------------------------------------------

test_that(".gt_compute_per_person() returns N x 4 matrix with canonical column names", {
  set.seed(101)
  X <- matrix(rnorm(60 * 12), 60, 12)
  vc <- .gt_variance_components(X)
  out <- .gt_compute_per_person(vc, n_items_D = 12L)

  expect_true(is.matrix(out))
  expect_equal(dim(out), c(60L, 4L))
  expect_equal(colnames(out),
               c("absolute", "relative_full",
                 "relative_large_a", "relative_uncorrelated"))
})


test_that(".gt_compute_per_person() reproduces csem_g1f() per-person estimates at 1e-10", {
  set.seed(102)
  data <- matrix(rbinom(300 * 30, 1, 0.5), 300, 30)

  legacy <- csem_g1f(as.data.frame(data), method = "all")
  vc     <- .gt_variance_components(data)
  out    <- .gt_compute_per_person(vc, n_items_D = 30L)

  expect_equal(unname(out[, "absolute"]),
               unname(legacy$person$abs_error_var), tolerance = 1e-10)
  expect_equal(unname(out[, "relative_full"]),
               unname(legacy$person$rel_error_var_full), tolerance = 1e-10)
  expect_equal(unname(out[, "relative_large_a"]),
               unname(legacy$person$rel_error_var_large_a), tolerance = 1e-10)
  expect_equal(unname(out[, "relative_uncorrelated"]),
               unname(legacy$person$rel_error_var_uncorrelated),
               tolerance = 1e-10)
})


test_that(".gt_compute_per_person() respects n_items_D for D-study extrapolation", {
  set.seed(103)
  X <- matrix(rnorm(40 * 10), 40, 10)
  vc <- .gt_variance_components(X)

  # D = J: identity
  out_id <- .gt_compute_per_person(vc, n_items_D = 10L)
  # D = 2J: absolute and relative quantities scale by 1/2 (eq. 20 and
  # downstream linear forms in 1/D).
  out_2J <- .gt_compute_per_person(vc, n_items_D = 20L)

  expect_equal(out_2J[, "absolute"], out_id[, "absolute"] / 2,
               tolerance = 1e-12)
})


# -----------------------------------------------------------------------------
# .gt_compute_per_person_for_boot()
# -----------------------------------------------------------------------------

test_that(".gt_compute_per_person_for_boot() returns B x 4 matrix with canonical column names", {
  set.seed(201)
  X <- matrix(rbinom(50 * 8, 1, 0.5), 50, 8)
  vc <- .gt_variance_components(X)

  set.seed(99)
  out <- .gt_compute_per_person_for_boot(
    xrow      = X[1, ],
    b_full    = vc$b_vec,
    sigma2_i  = vc$sigma2_i,
    n_items_D = 8L,
    N         = 50L,
    B         = 100L
  )

  expect_true(is.matrix(out))
  expect_equal(dim(out), c(100L, 4L))
  expect_equal(colnames(out),
               c("absolute", "relative_full",
                 "relative_large_a", "relative_uncorrelated"))
})


test_that(".gt_compute_per_person_for_boot() is deterministic under set.seed()", {
  set.seed(202)
  X <- matrix(rbinom(40 * 6, 1, 0.5), 40, 6)
  vc <- .gt_variance_components(X)

  set.seed(7)
  o1 <- .gt_compute_per_person_for_boot(X[1, ], vc$b_vec, vc$sigma2_i,
                                        6L, 40L, 200L)
  set.seed(7)
  o2 <- .gt_compute_per_person_for_boot(X[1, ], vc$b_vec, vc$sigma2_i,
                                        6L, 40L, 200L)
  expect_identical(o1, o2)
})


test_that(".gt_compute_per_person_for_boot() PRNG consumption matches legacy bootstrap_csem_g1f bit-for-bit", {
  # The strongest parity assertion: under the same set.seed() and the
  # same input data, the per-person B x 4 matrices produced by csemGT
  # and by the legacy script should be bit-identical, because both
  # consume the PRNG with a single call sample.int(J, B*J, replace=TRUE)
  # per person, in the same person-major order, and apply the same
  # algebra.
  set.seed(301)
  data <- matrix(rbinom(80 * 12, 1, 0.5), 80, 12)
  vc   <- .gt_variance_components(data)

  N <- 80L; J <- 12L; B <- 500L
  D <- J

  # Drive the legacy and the new bootstrap from the same seed, both
  # outputting per-person bootstrap variances. The seed is consumed
  # in the same order because we replicate the legacy's loop structure.
  set.seed(2026L)
  legacy_boot <- bootstrap_csem_g1f(
    X            = data,
    item_means   = colMeans(data),
    sigma2_item  = vc$sigma2_i,
    n_items_D    = D,
    B            = B
  )

  set.seed(2026L)
  v_abs  <- numeric(N); v_full <- numeric(N)
  v_la   <- numeric(N); v_unc  <- numeric(N)
  for (p in seq_len(N)) {
    reps <- .gt_compute_per_person_for_boot(
      xrow      = data[p, ],
      b_full    = vc$b_vec,
      sigma2_i  = vc$sigma2_i,
      n_items_D = D,
      N         = N,
      B         = B
    )
    v_abs[p]  <- stats::var(reps[, "absolute"])
    v_full[p] <- stats::var(reps[, "relative_full"])
    v_la[p]   <- stats::var(reps[, "relative_large_a"])
    v_unc[p]  <- stats::var(reps[, "relative_uncorrelated"])
  }

  expect_equal(v_abs,  unname(legacy_boot$abs),               tolerance = 1e-12)
  expect_equal(v_full, unname(legacy_boot$rel$full),          tolerance = 1e-12)
  expect_equal(v_la,   unname(legacy_boot$rel$large_a),       tolerance = 1e-12)
  expect_equal(v_unc,  unname(legacy_boot$rel$uncorrelated),  tolerance = 1e-12)
})


test_that(".item_bootstrap() produces identical per-person variances to the manual loop above", {
  # End-to-end check: the high-level .item_bootstrap() with vc and
  # n_items_D arguments collapses to the same per-person variances as
  # the manual loop in the test above, with the same seed.
  set.seed(401)
  data <- matrix(rbinom(60 * 10, 1, 0.5), 60, 10)
  vc   <- .gt_variance_components(data)

  out <- .item_bootstrap(data, vc, n_items_D = 10L,
                         R = 300L, seed = 2026L)

  # Reconstruct the per-person variances via the manual loop with the
  # same seed.
  set.seed(2026L)
  N <- 60L
  expected <- matrix(NA_real_, N, 4L,
                     dimnames = list(NULL,
                       c("absolute", "relative_full",
                         "relative_large_a", "relative_uncorrelated")))
  for (p in seq_len(N)) {
    reps <- .gt_compute_per_person_for_boot(
      xrow      = data[p, ],
      b_full    = vc$b_vec,
      sigma2_i  = vc$sigma2_i,
      n_items_D = 10L,
      N         = N,
      B         = 300L
    )
    expected[p, ] <- apply(reps, 2L, stats::var)
  }

  expect_equal(out$per_person_variance, expected, tolerance = 1e-14)
})
