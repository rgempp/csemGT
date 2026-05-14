# tests/testthat/test-utils-gt-merge.R
#
# Tests for utils-gt.R Part 6:
#   .merge_smoothed_to_person()
#
# Two layers: isolated unit tests with synthetic per_person_wide /
# by_score_wide tables, and an integration test that runs the real
# .pivot_to_wide() -> (manual collapse) -> .apply_smoother() ->
# .merge_smoothed_to_person() chain to confirm the column contract holds.


# -----------------------------------------------------------------------------
# Isolated unit tests
# -----------------------------------------------------------------------------

test_that(".merge_smoothed_to_person() appends smoothed_csem.* columns", {
  per_person_wide <- data.frame(
    person_id      = 1:6,
    observed_score = c(0.1, 0.3, 0.5, 0.2, 0.4, 0.1),
    csem.absolute  = runif(6),
    stringsAsFactors = FALSE
  )
  by_score_wide <- data.frame(
    observed_score          = c(0.1, 0.2, 0.3, 0.4, 0.5),
    smoothed_csem.absolute  = c(0.11, 0.12, 0.13, 0.14, 0.15),
    smoothed_csem.relative_full = c(0.21, 0.22, 0.23, 0.24, 0.25),
    stringsAsFactors = FALSE
  )
  out <- .merge_smoothed_to_person(per_person_wide, by_score_wide)
  expect_true(all(c("smoothed_csem.absolute",
                    "smoothed_csem.relative_full") %in% names(out)))
  expect_equal(nrow(out), 6L)
})

test_that(".merge_smoothed_to_person() maps values by observed_score correctly", {
  per_person_wide <- data.frame(
    person_id      = 1:5,
    observed_score = c(0.5, 0.1, 0.3, 0.1, 0.5),
    stringsAsFactors = FALSE
  )
  by_score_wide <- data.frame(
    observed_score         = c(0.1, 0.3, 0.5),
    smoothed_csem.absolute = c(10, 30, 50),
    stringsAsFactors = FALSE
  )
  out <- .merge_smoothed_to_person(per_person_wide, by_score_wide)
  # Person scores 0.5, 0.1, 0.3, 0.1, 0.5 -> 50, 10, 30, 10, 50
  expect_equal(out$smoothed_csem.absolute, c(50, 10, 30, 10, 50))
})

test_that(".merge_smoothed_to_person() propagates NA for excluded extreme scores", {
  per_person_wide <- data.frame(
    person_id      = 1:4,
    observed_score = c(0, 0.3, 0.5, 1),
    stringsAsFactors = FALSE
  )
  # by_score with NA at the extremes (as .apply_smoother writes when
  # exclude_extremes = TRUE).
  by_score_wide <- data.frame(
    observed_score         = c(0, 0.3, 0.5, 1),
    smoothed_csem.absolute = c(NA_real_, 0.13, 0.15, NA_real_),
    stringsAsFactors = FALSE
  )
  out <- .merge_smoothed_to_person(per_person_wide, by_score_wide)
  expect_true(is.na(out$smoothed_csem.absolute[1]))
  expect_true(is.na(out$smoothed_csem.absolute[4]))
  expect_equal(out$smoothed_csem.absolute[2:3], c(0.13, 0.15))
})

test_that(".merge_smoothed_to_person() is a passthrough when no smoothed_* columns", {
  per_person_wide <- data.frame(
    person_id      = 1:3,
    observed_score = c(0.1, 0.2, 0.3),
    csem.absolute  = c(0.1, 0.2, 0.3),
    stringsAsFactors = FALSE
  )
  by_score_wide <- data.frame(
    observed_score    = c(0.1, 0.2, 0.3),
    csem_var.absolute = c(0.01, 0.04, 0.09),
    stringsAsFactors = FALSE
  )
  out <- .merge_smoothed_to_person(per_person_wide, by_score_wide)
  expect_identical(out, per_person_wide)
})

test_that(".merge_smoothed_to_person() errors when observed_score is missing", {
  ok_by_score <- data.frame(
    observed_score         = c(0.1, 0.2),
    smoothed_csem.absolute = c(0.1, 0.2),
    stringsAsFactors = FALSE
  )
  expect_error(
    .merge_smoothed_to_person(data.frame(person_id = 1:2), ok_by_score),
    "`per_person_wide` must contain"
  )
  expect_error(
    .merge_smoothed_to_person(
      data.frame(person_id = 1:2, observed_score = c(0.1, 0.2)),
      data.frame(smoothed_csem.absolute = c(0.1, 0.2))),
    "`by_score_wide` must contain"
  )
})

test_that(".merge_smoothed_to_person() carries every smoothed_* column", {
  per_person_wide <- data.frame(
    person_id      = 1:3,
    observed_score = c(0.2, 0.4, 0.6),
    stringsAsFactors = FALSE
  )
  by_score_wide <- data.frame(
    observed_score                      = c(0.2, 0.4, 0.6),
    smoothed_csem.absolute              = c(1, 2, 3),
    smoothed_csem.relative_full         = c(4, 5, 6),
    smoothed_csem.relative_large_a      = c(7, 8, 9),
    smoothed_csem.relative_uncorrelated = c(10, 11, 12),
    stringsAsFactors = FALSE
  )
  out <- .merge_smoothed_to_person(per_person_wide, by_score_wide)
  expect_equal(out$smoothed_csem.absolute,              c(1, 2, 3))
  expect_equal(out$smoothed_csem.relative_full,         c(4, 5, 6))
  expect_equal(out$smoothed_csem.relative_large_a,      c(7, 8, 9))
  expect_equal(out$smoothed_csem.relative_uncorrelated, c(10, 11, 12))
})


# -----------------------------------------------------------------------------
# Integration test: real .pivot_to_wide() -> collapse -> .apply_smoother()
#                    -> .merge_smoothed_to_person()
# -----------------------------------------------------------------------------

test_that(".merge_smoothed_to_person() integrates with the real pipeline helpers", {
  set.seed(909L)
  N <- 80L
  J <- 15L
  data <- matrix(rbinom(N * J, 1, 0.5), N, J)
  vc   <- .gt_variance_components(data)
  ppm  <- .gt_compute_per_person(vc, n_items_D = J)
  long <- .gt_long_format(ppm, observed_score = vc$person_mean,
                          cov_x_itemmean = vc$cov_x_itemmean)
  wide <- .pivot_to_wide(long, paradigm = "gt")

  # Collapse to one row per unique observed score by averaging the
  # csem_var.* columns (stands in for .collapse_to_score(); the smoother
  # only needs observed_score + csem_var.* columns).
  ev_cols <- grep("^csem_var\\.", names(wide), value = TRUE)
  by_score <- aggregate(
    wide[, ev_cols, drop = FALSE],
    by  = list(observed_score = wide$observed_score),
    FUN = mean
  )
  by_score_sm <- .apply_smoother(by_score, "polynomial",
                                 list(degree = 2))

  merged <- .merge_smoothed_to_person(wide, by_score_sm)

  # Every smoothed column from the by-score table must now be on the
  # per-person table.
  smoothed_cols <- grep("^smoothed_csem\\.", names(by_score_sm),
                        value = TRUE)
  expect_true(length(smoothed_cols) == 4L)
  expect_true(all(smoothed_cols %in% names(merged)))
  expect_equal(nrow(merged), N)

  # Spot-check the mapping for one estimator: each person's smoothed
  # value equals the by-score smoothed value at the person's score.
  exp_abs <- by_score_sm$smoothed_csem.absolute[
    match(merged$observed_score, by_score_sm$observed_score)]
  expect_equal(merged$smoothed_csem.absolute, exp_abs, tolerance = 1e-12)

  # Smoothed CSEMs are non-negative wherever defined.
  for (sc in smoothed_cols) {
    vals <- merged[[sc]]
    expect_true(all(vals[!is.na(vals)] >= 0), info = sc)
  }
})

test_that(".merge_smoothed_to_person() integration respects exclude_extremes NA pattern", {
  set.seed(910L)
  N <- 70L
  J <- 12L
  data <- matrix(rbinom(N * J, 1, 0.5), N, J)
  vc   <- .gt_variance_components(data)
  ppm  <- .gt_compute_per_person(vc, n_items_D = J)
  long <- .gt_long_format(ppm, observed_score = vc$person_mean,
                          cov_x_itemmean = vc$cov_x_itemmean)
  wide <- .pivot_to_wide(long, paradigm = "gt")

  ev_cols <- grep("^csem_var\\.", names(wide), value = TRUE)
  by_score <- aggregate(
    wide[, ev_cols, drop = FALSE],
    by  = list(observed_score = wide$observed_score),
    FUN = mean
  )
  # Exclude the lowest and highest observed scores.
  extremes <- range(by_score$observed_score)
  by_score_sm <- .apply_smoother(by_score, "polynomial",
                                 list(degree = 2),
                                 exclude_extremes = TRUE,
                                 score_extremes   = extremes)
  merged <- .merge_smoothed_to_person(wide, by_score_sm)

  # Persons at an excluded extreme score inherit NA; everyone else is
  # finite.
  at_extreme <- merged$observed_score %in% extremes
  expect_true(all(is.na(merged$smoothed_csem.absolute[at_extreme])))
  expect_true(all(!is.na(merged$smoothed_csem.absolute[!at_extreme])))
})
