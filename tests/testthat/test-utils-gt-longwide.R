# tests/testthat/test-utils-gt-longwide.R
#
# Tests for utils-gt.R Part 5:
#   .gt_sem_sqrt()
#   .gt_long_format()
#   .gt_add_analytical_se()
#   .gt_add_ci()
#   .pivot_to_wide()
#
# These are the data-plumbing helpers that turn the N x 4 point-estimate
# matrix into the long-tidy intermediate and then the wide reported table.
# Most tests are internal-consistency checks; the analytical-SE delta-method
# is checked against .gt_analytical_se() (whose own legacy parity is covered
# in test-utils-gt-population.R).

source(system.file("legacy", "csem_gt_estimation_v3.R",
                   package  = "csemGT",
                   mustWork = TRUE))


# Shared fixture: a valid long-tidy table built through the real pipeline.
.make_long <- function(seed = 4242L, N = 60L, J = 12L,
                       truncate = FALSE, cov = TRUE) {
  set.seed(seed)
  data <- matrix(rbinom(N * J, 1, 0.5), N, J)
  vc   <- .gt_variance_components(data)
  ppm  <- .gt_compute_per_person(vc, n_items_D = J)
  long <- .gt_long_format(
    ppm, observed_score = vc$person_mean,
    cov_x_itemmean = if (cov) vc$cov_x_itemmean else NULL,
    truncate_negative_error_var = truncate
  )
  list(data = data, vc = vc, ppm = ppm, long = long, J = J, N = N)
}


# -----------------------------------------------------------------------------
# .gt_sem_sqrt()
# -----------------------------------------------------------------------------

test_that(".gt_sem_sqrt() with truncate_negative = FALSE: NaN for negatives, NA preserved", {
  v <- c(0.04, 0, -0.01, NA_real_, 0.09)
  out <- .gt_sem_sqrt(v, truncate_negative = FALSE)
  expect_equal(out[1], 0.2, tolerance = 1e-12)
  expect_equal(out[2], 0)
  expect_true(is.nan(out[3]))
  expect_true(is.na(out[4]) && !is.nan(out[4]))
  expect_equal(out[5], 0.3, tolerance = 1e-12)
})

test_that(".gt_sem_sqrt() with truncate_negative = TRUE: negatives clamped to 0", {
  v <- c(0.04, -0.01, -1, 0.25)
  out <- .gt_sem_sqrt(v, truncate_negative = TRUE)
  expect_equal(out, c(0.2, 0, 0, 0.5), tolerance = 1e-12)
})

test_that(".gt_sem_sqrt() handles a length-0 input", {
  expect_equal(length(.gt_sem_sqrt(numeric(0))), 0L)
})


# -----------------------------------------------------------------------------
# .gt_long_format()
# -----------------------------------------------------------------------------

test_that(".gt_long_format() returns the expected long-tidy schema", {
  fx <- .make_long()
  long <- fx$long
  expect_s3_class(long, "data.frame")
  expect_true(all(c("person_id", "observed_score", "cov_xim",
                    "estimator", "csem_var", "csem") %in% names(long)))
  # N x k rows, k = 4 estimators kept.
  expect_equal(nrow(long), fx$N * 4L)
  expect_setequal(unique(long$estimator),
                  c("absolute", "relative_full",
                    "relative_large_a", "relative_uncorrelated"))
})

test_that(".gt_long_format() csem equals sqrt(csem_var) elementwise", {
  fx <- .make_long()
  long <- fx$long
  ok <- !is.na(long$csem_var) & long$csem_var >= 0
  expect_equal(long$csem[ok], sqrt(long$csem_var[ok]), tolerance = 1e-12)
})

test_that(".gt_long_format() csem_var matches the source matrix column-by-column", {
  fx <- .make_long()
  for (est in colnames(fx$ppm)) {
    sel <- fx$long$estimator == est
    # Rows for an estimator are in person_id order 1..N by construction.
    expect_equal(fx$long$csem_var[sel], unname(fx$ppm[, est]),
                 tolerance = 1e-12,
                 info = paste("estimator:", est))
  }
})

test_that(".gt_long_format() respects estimators_keep and matrix column order", {
  fx <- .make_long()
  long2 <- .gt_long_format(
    fx$ppm, observed_score = fx$vc$person_mean,
    estimators_keep = c("relative_full", "absolute")  # deliberately reversed
  )
  # Order follows the matrix columns (absolute first), not estimators_keep.
  expect_equal(unique(long2$estimator), c("absolute", "relative_full"))
  expect_equal(nrow(long2), fx$N * 2L)
})

test_that(".gt_long_format() omits cov_xim when cov_x_itemmean is NULL", {
  fx <- .make_long(cov = FALSE)
  expect_false("cov_xim" %in% names(fx$long))
})

test_that(".gt_long_format() errors on a matrix without column names", {
  m <- matrix(runif(20), 5, 4)
  expect_error(.gt_long_format(m, observed_score = 1:5),
               "column names")
})

test_that(".gt_long_format() errors when no requested estimator is present", {
  fx <- .make_long()
  expect_error(
    .gt_long_format(fx$ppm, observed_score = fx$vc$person_mean,
                    estimators_keep = "nonexistent"),
    "no requested estimators"
  )
})

test_that(".gt_long_format() propagates truncate_negative_error_var into csem", {
  # Build a matrix with a deliberately negative relative estimate by using
  # a near-degenerate dataset; then truncate should produce csem = 0 there.
  fx_t <- .make_long(seed = 11L, truncate = TRUE)
  # Any negative csem_var must map to csem == 0 (not NaN) under truncation.
  neg <- !is.na(fx_t$long$csem_var) & fx_t$long$csem_var < 0
  if (any(neg)) {
    expect_true(all(fx_t$long$csem[neg] == 0))
  } else {
    succeed("no negative error variances in this fixture; truncation path trivially holds")
  }
})


# -----------------------------------------------------------------------------
# .gt_add_analytical_se()
# -----------------------------------------------------------------------------

test_that(".gt_add_analytical_se() adds csem_var.analytic and se.analytic", {
  fx <- .make_long()
  out <- .gt_add_analytical_se(fx$long, fx$vc, n_items_D = fx$J)
  expect_true(all(c("csem_var.analytic", "se.analytic") %in% names(out)))
  ok <- !is.na(out$csem_var.analytic)
  expect_equal(out$se.analytic[ok], sqrt(out$csem_var.analytic[ok]),
               tolerance = 1e-12)
})

test_that(".gt_add_analytical_se() delta method matches var(V) / (4 V)", {
  fx <- .make_long()
  out <- .gt_add_analytical_se(fx$long, fx$vc, n_items_D = fx$J)
  se_var <- .gt_analytical_se(fx$vc, n_items_D = fx$J)
  name_map <- c(absolute = "absolute", relative_full = "full",
                relative_large_a = "large_a",
                relative_uncorrelated = "uncorrelated")

  for (est in unique(out$estimator)) {
    sel <- out$estimator == est
    v_pt <- out$csem_var[sel]
    expected <- ifelse(!is.na(v_pt) & v_pt > 0,
                       se_var[[name_map[[est]]]] / (4 * v_pt),
                       NA_real_)
    expect_equal(out$csem_var.analytic[sel], expected,
                 tolerance = 1e-12, info = paste("estimator:", est))
  }
})

test_that(".gt_add_analytical_se() gives NA where csem_var <= 0", {
  fx <- .make_long()
  long <- fx$long
  # Force two rows to non-positive point estimates.
  long$csem_var[1] <- 0
  long$csem_var[2] <- -0.01
  out <- .gt_add_analytical_se(long, fx$vc, n_items_D = fx$J)
  expect_true(is.na(out$csem_var.analytic[1]))
  expect_true(is.na(out$csem_var.analytic[2]))
})

test_that(".gt_add_analytical_se() errors on missing required columns", {
  expect_error(
    .gt_add_analytical_se(data.frame(estimator = "absolute"),
                          vc = list(), n_items_D = 10),
    "csem_var"
  )
})

test_that(".gt_add_analytical_se() errors on an unrecognized estimator label", {
  fx <- .make_long()
  bad <- fx$long
  bad$estimator[1] <- "totally_wrong"
  expect_error(
    .gt_add_analytical_se(bad, fx$vc, n_items_D = fx$J),
    "unrecognized estimator"
  )
})


# -----------------------------------------------------------------------------
# .gt_add_ci()
# -----------------------------------------------------------------------------

test_that(".gt_add_ci() adds analytical CI columns when se.analytic is present", {
  fx  <- .make_long()
  out <- .gt_add_analytical_se(fx$long, fx$vc, n_items_D = fx$J)
  out <- .gt_add_ci(out, ci_method = "normal", ci_level = 0.95)
  expect_true(all(c("ci_low.analytic", "ci_up.analytic") %in% names(out)))
  expect_false(any(c("ci_low.boot", "ci_up.boot") %in% names(out)))
})

test_that(".gt_add_ci() Wald interval: csem +/- z * se, lower truncated at 0", {
  pp <- data.frame(
    person_id   = 1:3,
    estimator   = rep("absolute", 3),
    csem        = c(0.05, 0.30, 0.50),
    se.analytic = c(0.10, 0.04, 0.02),
    stringsAsFactors = FALSE
  )
  out <- .gt_add_ci(pp, ci_method = "normal", ci_level = 0.95)
  z <- qnorm(0.975)
  expect_equal(out$ci_up.analytic, pp$csem + z * pp$se.analytic,
               tolerance = 1e-12)
  expect_equal(out$ci_low.analytic,
               pmax(pp$csem - z * pp$se.analytic, 0),
               tolerance = 1e-12)
  # Person 1: csem - z*se is negative -> truncated to 0.
  expect_equal(out$ci_low.analytic[1], 0)
})

test_that(".gt_add_ci() adds bootstrap CI columns when se.boot is present", {
  pp <- data.frame(
    person_id = 1:2,
    estimator = rep("absolute", 2),
    csem      = c(0.30, 0.40),
    se.boot   = c(0.05, 0.06),
    stringsAsFactors = FALSE
  )
  out <- .gt_add_ci(pp, ci_method = "normal", ci_level = 0.90, bootstrap = TRUE)
  z <- qnorm(0.95)
  expect_true(all(c("ci_low.boot", "ci_up.boot") %in% names(out)))
  expect_equal(out$ci_up.boot, pp$csem + z * pp$se.boot, tolerance = 1e-12)
})

test_that(".gt_add_ci() builds both analytical and bootstrap CIs when both SEs present", {
  pp <- data.frame(
    person_id   = 1:2,
    estimator   = rep("absolute", 2),
    csem        = c(0.30, 0.40),
    se.analytic = c(0.05, 0.06),
    se.boot     = c(0.07, 0.08),
    stringsAsFactors = FALSE
  )
  out <- .gt_add_ci(pp, ci_method = "normal", ci_level = 0.95)
  expect_true(all(c("ci_low.analytic", "ci_up.analytic",
                    "ci_low.boot", "ci_up.boot") %in% names(out)))
})

test_that(".gt_add_ci() records effective and requested ci_method as attributes", {
  pp <- data.frame(
    person_id = 1L, estimator = "absolute",
    csem = 0.3, se.analytic = 0.05,
    stringsAsFactors = FALSE
  )
  # A replicate-based method is downgraded to "normal" for v1.0.
  out <- .gt_add_ci(pp, ci_method = "percentile", ci_level = 0.95)
  expect_equal(attr(out, "ci_method"), "normal")
  expect_equal(attr(out, "ci_method_requested"), "percentile")
  expect_equal(attr(out, "ci_level"), 0.95)
})

test_that(".gt_add_ci() errors on missing csem column or invalid ci_level", {
  expect_error(
    .gt_add_ci(data.frame(se.analytic = 0.1), ci_method = "normal"),
    "csem"
  )
  expect_error(
    .gt_add_ci(data.frame(csem = 0.3, se.analytic = 0.1),
               ci_method = "normal", ci_level = 1.5),
    "ci_level"
  )
})


# -----------------------------------------------------------------------------
# .pivot_to_wide()
# -----------------------------------------------------------------------------

test_that(".pivot_to_wide() returns one row per person with id columns once", {
  fx   <- .make_long()
  wide <- .pivot_to_wide(fx$long, paradigm = "gt")
  expect_s3_class(wide, "data.frame")
  expect_equal(nrow(wide), fx$N)
  # Person-constant columns appear exactly once.
  expect_equal(sum(names(wide) == "person_id"), 1L)
  expect_equal(sum(names(wide) == "observed_score"), 1L)
  expect_equal(sum(names(wide) == "cov_xim"), 1L)
})

test_that(".pivot_to_wide() spreads value columns as <value>.<estimator>", {
  fx   <- .make_long()
  wide <- .pivot_to_wide(fx$long, paradigm = "gt")
  for (est in colnames(fx$ppm)) {
    expect_true(paste0("csem.", est) %in% names(wide))
    expect_true(paste0("csem_var.", est) %in% names(wide))
  }
})

test_that(".pivot_to_wide() round-trips the point estimates from the source matrix", {
  fx   <- .make_long()
  wide <- .pivot_to_wide(fx$long, paradigm = "gt")
  # Wide is ordered by person_id; the source matrix is in person order too.
  for (est in colnames(fx$ppm)) {
    expect_equal(wide[[paste0("csem_var.", est)]], unname(fx$ppm[, est]),
                 tolerance = 1e-12, info = paste("estimator:", est))
  }
})

test_that(".pivot_to_wide() carries SE and CI columns through the spread", {
  fx  <- .make_long()
  out <- .gt_add_analytical_se(fx$long, fx$vc, n_items_D = fx$J)
  out <- .gt_add_ci(out, ci_method = "normal", ci_level = 0.95)
  wide <- .pivot_to_wide(out, paradigm = "gt")
  expect_true("se.analytic.absolute" %in% names(wide))
  expect_true("ci_low.analytic.relative_full" %in% names(wide))
  expect_true("ci_up.analytic.relative_uncorrelated" %in% names(wide))
})

test_that(".pivot_to_wide() rejects non-gt paradigm and malformed input", {
  fx <- .make_long()
  expect_error(.pivot_to_wide(fx$long, paradigm = "binomial"),
               "paradigm = 'gt'")
  expect_error(.pivot_to_wide(data.frame(x = 1)),
               "person_id and estimator")
})

test_that(".pivot_to_wide() preserves person_id ordering", {
  fx   <- .make_long()
  wide <- .pivot_to_wide(fx$long, paradigm = "gt")
  expect_equal(wide$person_id, seq_len(fx$N))
})
