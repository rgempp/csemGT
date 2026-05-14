# Tests for R/utils-smoothing.R
# Covers .apply_smoother() with smoother in c("polynomial", "none").

test_that(".apply_smoother adds smoothed_csem.* columns", {
  by_score <- .make_test_by_score()
  result <- .apply_smoother(by_score, "polynomial", list(degree = 2))
  expect_true("smoothed_csem.absolute" %in% names(result))
  expect_true("smoothed_csem.relative_full" %in% names(result))
  expect_true("smoothed_csem.relative_large_a" %in% names(result))
  expect_true("smoothed_csem.relative_uncorrelated" %in% names(result))
})

test_that(".apply_smoother smoothed_csem.* values are non-negative", {
  by_score <- .make_test_by_score()
  result <- .apply_smoother(by_score, "polynomial", list(degree = 2))
  smooth_cols <- grep("^smoothed_csem\\.", names(result), value = TRUE)
  for (col in smooth_cols) {
    expect_true(all(result[[col]][!is.na(result[[col]])] >= 0), info = col)
  }
})

test_that(".apply_smoother with exclude_extremes leaves extreme rows as NA", {
  by_score <- .make_test_by_score()  # scores 0:20
  result <- .apply_smoother(by_score, "polynomial", list(degree = 2),
                            exclude_extremes = TRUE,
                            score_extremes   = c(0, 20))
  excluded <- result$observed_score %in% c(0, 20)
  expect_true(all(is.na(result$smoothed_csem.absolute[excluded])))
  expect_true(all(!is.na(result$smoothed_csem.absolute[!excluded])))
})

test_that(".apply_smoother diagnostics match r(smooth_fits) format", {
  by_score <- .make_test_by_score()
  result <- .apply_smoother(by_score, "polynomial", list(degree = 2))
  diag <- attr(result, "smooth_fits")
  expect_true("absolute" %in% names(diag))
  expect_named(diag$absolute, c("b0", "b1", "b2", "R2", "RMSE", "N"))
})

test_that(".apply_smoother RMSE matches sqrt(SSE/N) (gtcsem.ado convention)", {
  # Build a simple by_score and re-fit by hand to verify the convention.
  by_score <- data.frame(
    observed_score    = 0:20,
    group_size        = rep(1L, 21L),
    csem_var.absolute = (0:20) * 0.001 + rnorm(21, 0, 0.0005)
  )
  by_score$csem_var.absolute <- abs(by_score$csem_var.absolute)
  by_score$csem.absolute <- sqrt(by_score$csem_var.absolute)

  smoothed <- .apply_smoother(by_score, "polynomial", list(degree = 2))
  diag <- attr(smoothed, "smooth_fits")$absolute

  # Reproduce: y ~ I(x^1) + I(x^2), then RMSE = sqrt(SSE / N).
  fit_man <- stats::lm(csem_var.absolute ~ I(observed_score) +
                                            I(observed_score^2),
                       data = by_score)
  sse_man  <- sum(stats::residuals(fit_man)^2)
  rmse_man <- sqrt(sse_man / nrow(by_score))

  expect_equal(diag$RMSE, rmse_man, tolerance = 1e-12)
  # And it should differ from summary()$sigma (which divides by N-k-1)
  expect_false(isTRUE(all.equal(diag$RMSE, summary(fit_man)$sigma)))
})

test_that(".apply_smoother with smoother='none' is a passthrough", {
  by_score <- .make_test_by_score()
  result <- .apply_smoother(by_score, smoother = "none")
  expect_identical(result, by_score)
  expect_null(attr(result, "smooth_fits"))
})

test_that(".apply_smoother rejects unsupported smoothers", {
  by_score <- .make_test_by_score()
  expect_error(.apply_smoother(by_score, smoother = "spline"),
               "not implemented")
  expect_error(.apply_smoother(by_score, smoother = "loess"),
               "not implemented")
})

test_that(".apply_smoother rejects invalid degree", {
  by_score <- .make_test_by_score()
  expect_error(.apply_smoother(by_score, smoother_args = list(degree = 0)),
               "positive integer")
  expect_error(.apply_smoother(by_score, smoother_args = list(degree = 2.5)),
               "positive integer")
})

test_that(".apply_smoother degree = 1 yields b2 = NA in diagnostics", {
  by_score <- .make_test_by_score()
  result <- .apply_smoother(by_score, smoother_args = list(degree = 1L))
  diag <- attr(result, "smooth_fits")$absolute
  expect_false(is.na(diag$b1))
  expect_true(is.na(diag$b2))
})

test_that(".apply_smoother fits over only the included rows when exclude_extremes", {
  by_score <- .make_test_by_score()
  excl <- c(0, 20)
  result <- .apply_smoother(by_score, "polynomial", list(degree = 2),
                            exclude_extremes = TRUE,
                            score_extremes   = excl)
  diag <- attr(result, "smooth_fits")$absolute
  expect_equal(diag$N, nrow(by_score) - length(excl))
})

test_that(".apply_smoother emits smoothing_diagnostics with exclude_extremes counts", {
  by_score <- .make_test_by_score()  # scores 0:20, one row per score
  result <- .apply_smoother(by_score, "polynomial", list(degree = 2),
                            exclude_extremes = TRUE,
                            score_extremes   = c(0, 20))
  sd <- attr(result, "smoothing_diagnostics")
  expect_named(sd, c("n_floor", "n_ceiling", "n_fit"))
  expect_equal(sd$n_floor,   1L)
  expect_equal(sd$n_ceiling, 1L)
  expect_equal(sd$n_fit,     nrow(by_score) - 2L)
  # n_fit must coincide with the per-estimator diagnostic N.
  expect_equal(sd$n_fit, attr(result, "smooth_fits")$absolute$N)
})

test_that(".apply_smoother smoothing_diagnostics are NA without exclude_extremes", {
  by_score <- .make_test_by_score()
  result <- .apply_smoother(by_score, "polynomial", list(degree = 2))
  sd <- attr(result, "smoothing_diagnostics")
  expect_named(sd, c("n_floor", "n_ceiling", "n_fit"))
  expect_true(all(is.na(unlist(sd))))
})

test_that(".apply_smoother passthrough ('none') sets no smoothing_diagnostics", {
  by_score <- .make_test_by_score()
  result <- .apply_smoother(by_score, smoother = "none")
  expect_null(attr(result, "smoothing_diagnostics"))
})

test_that(".apply_smoother on a by_score with no csem_var.* columns returns empty diagnostics", {
  by_score <- data.frame(observed_score = 0:10, group_size = rep(1L, 11))
  result <- .apply_smoother(by_score)
  expect_identical(names(result), names(by_score))
  expect_equal(length(attr(result, "smooth_fits")), 0L)
})
