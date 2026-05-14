# Tests for plot.csem and the plotting helpers in R/plot-csem.R:
# csem_palette(), .resolve_plot_theme(), .resolve_plot_columns(),
# .plot_csem_bands(), .plot_csem_single(), and the plot.csem dispatcher.
#
# Sub-phase 3.5 covers the single-panel plot_type = "csem" layout;
# sub-phase 3.6 adds the confidence-band layers (plot_type "ci" / "both",
# cibands "person" / "model"). The visual-parity checks (vdiffr against
# the four paper figures) belong to a later sub-phase; here the plotting
# paths are exercised as smoke tests -- they must run, return invisibly,
# and stop cleanly on the branches that are not yet wired -- while the
# pure helpers (palette, theme, column and band resolvers) are
# unit-tested directly.

.make_plot_data <- function(seed = 11L, N = 120L, J = 16L) {
  set.seed(seed)
  theta <- rnorm(N, 0, 1.2)
  beta  <- rnorm(J, 0, 0.6)
  p <- plogis(outer(theta, beta, "-"))
  matrix(rbinom(N * J, 1, p), nrow = N, ncol = J)
}

fit_abs <- function() {
  suppressMessages(csem_gt(.make_plot_data(), error_type = "absolute"))
}
fit_rel <- function() {
  suppressMessages(
    csem_gt(.make_plot_data(), error_type = "relative",
            method = c("full", "large_a", "uncorrelated")))
}
fit_both <- function() {
  suppressMessages(
    csem_gt(.make_plot_data(), error_type = c("absolute", "relative")))
}
fit_boot <- function() {
  suppressMessages(
    csem_gt(.make_plot_data(), error_type = "absolute",
            bootstrap = TRUE, R = 100L, seed = 1L))
}

# Run plotting code against a throwaway device so the test run does not
# open windows or leave a stray Rplots.pdf behind.
with_null_device <- function(code) {
  f <- tempfile(fileext = ".pdf")
  grDevices::pdf(f)
  on.exit({
    grDevices::dev.off()
    unlink(f)
  })
  force(code)
}


# -----------------------------------------------------------------------------
# csem_palette()
# -----------------------------------------------------------------------------

test_that("csem_palette() returns the full named palette by default", {
  pal <- csem_palette()
  expect_type(pal, "character")
  expect_named(pal, c("absolute", "relative_full", "relative_large_a",
                      "relative_uncorrelated", "structural"))
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", pal)))
})

test_that("csem_palette() subsets by name", {
  expect_identical(csem_palette("relative_full"),
                   csem_palette()["relative_full"])
  two <- csem_palette(c("absolute", "structural"))
  expect_named(two, c("absolute", "structural"))
})

test_that("csem_palette() errors on an unknown entry", {
  expect_error(csem_palette("burnt_sienna"), "unknown palette")
})


# -----------------------------------------------------------------------------
# .resolve_plot_theme()
# -----------------------------------------------------------------------------

test_that(".resolve_plot_theme() returns the palette and a grid colour", {
  th <- .resolve_plot_theme("csem")
  expect_named(th, c("palette", "grid"))
  expect_identical(th$palette, csem_palette())
  expect_identical(th$grid, unname(csem_palette("structural")))
})

test_that(".resolve_plot_theme() rejects an unknown theme", {
  expect_error(.resolve_plot_theme("ggplot2"), "should be")
})


# -----------------------------------------------------------------------------
# .resolve_plot_columns()
# -----------------------------------------------------------------------------

test_that(".resolve_plot_columns() resolves a single absolute series", {
  fit <- fit_abs()
  series <- .resolve_plot_columns(fit, "absolute", NULL, FALSE)
  expect_length(series, 1L)
  expect_identical(series[[1L]]$key, "absolute")
  expect_identical(series[[1L]]$csem_col, "csem.absolute")
  expect_identical(series[[1L]]$smooth_col, "smoothed_csem.absolute")
})

test_that(".resolve_plot_columns() resolves a single relative series by method", {
  fit <- fit_rel()
  series <- .resolve_plot_columns(fit, "relative", "large_a", FALSE)
  expect_length(series, 1L)
  expect_identical(series[[1L]]$key, "relative_large_a")
  expect_identical(series[[1L]]$csem_col, "csem.relative_large_a")
})

test_that(".resolve_plot_columns() expands compare_methods to three series", {
  fit <- fit_rel()
  series <- .resolve_plot_columns(fit, "relative", NULL, TRUE)
  expect_length(series, 3L)
  expect_identical(vapply(series, `[[`, character(1), "key"),
                   c("relative_full", "relative_large_a",
                     "relative_uncorrelated"))
})

test_that(".resolve_plot_columns() errors when the column is absent", {
  fit <- fit_abs()
  expect_error(.resolve_plot_columns(fit, "relative", "full", FALSE),
               "csem\\.relative_full")
})

test_that(".resolve_plot_columns() carries the band column names", {
  fit <- fit_rel()
  series <- .resolve_plot_columns(fit, "relative", "full", FALSE)[[1L]]
  expect_identical(series$csem_var_col,    "csem_var.relative_full")
  expect_identical(series$se_analytic_col, "se.analytic.relative_full")
  expect_identical(series$se_boot_col,     "se.boot.relative_full")
})


# -----------------------------------------------------------------------------
# plot.csem -- single-panel smoke tests
# -----------------------------------------------------------------------------

test_that("plot.csem returns the object invisibly", {
  fit <- fit_abs()
  result <- with_null_device(expect_invisible(plot(fit)))
  expect_identical(result, fit)
})

test_that("plot.csem draws a single absolute panel without error", {
  fit <- fit_abs()
  expect_no_error(with_null_device(plot(fit)))
})

test_that("plot.csem draws a single relative panel without error", {
  fit <- fit_rel()
  expect_no_error(with_null_device(plot(fit, error_types = "relative",
                                        method = "full")))
})

test_that("plot.csem runs with show_smooth = FALSE", {
  fit <- fit_abs()
  expect_no_error(with_null_device(plot(fit, show_smooth = FALSE)))
})

test_that("plot.csem runs with add = TRUE onto an existing plot", {
  fit <- fit_abs()
  expect_no_error(with_null_device({
    plot(fit)
    plot(fit, add = TRUE, col = "grey40")
  }))
})

test_that("plot.csem honours an explicit colour and alpha", {
  fit <- fit_abs()
  expect_no_error(
    with_null_device(plot(fit, col = "steelblue", alpha = 0.5)))
})


# -----------------------------------------------------------------------------
# .plot_csem_bands() -- delta-method confidence bands
# -----------------------------------------------------------------------------

test_that(".plot_csem_bands person band is csem +/- z*se on the by-score table", {
  fit    <- fit_abs()
  series <- .resolve_plot_columns(fit, "absolute", NULL, FALSE)[[1L]]
  b      <- .plot_csem_bands(fit, series, "person", "analytical", 0.95)

  bs <- fit$by_score[order(fit$by_score$observed_score), ]
  z  <- stats::qnorm(0.975)

  expect_named(b, c("x", "lo", "hi", "center"))
  expect_equal(b$x,      bs$observed_score,  tolerance = 1e-12)
  expect_equal(b$center, bs$csem.absolute,   tolerance = 1e-10)
  expect_equal(b$hi,
               bs$csem.absolute + z * bs$se.analytic.absolute,
               tolerance = 1e-10)
  expect_equal(b$lo,
               pmax(0, bs$csem.absolute - z * bs$se.analytic.absolute),
               tolerance = 1e-10)
})

test_that(".plot_csem_bands edges respect lo >= 0 and lo <= hi", {
  fit <- fit_rel()
  for (cb in c("person", "model")) {
    series <- .resolve_plot_columns(fit, "relative", "full", FALSE)[[1L]]
    b <- .plot_csem_bands(fit, series, cb, "analytical", 0.95)
    expect_true(all(b$lo >= 0, na.rm = TRUE),      info = cb)
    expect_true(all(b$lo <= b$hi, na.rm = TRUE),   info = cb)
  }
})

test_that(".plot_csem_bands model band centre matches the stored smoother", {
  # The model-band refit fits the same quadratic .apply_smoother() fits,
  # over the same sample, so its centre must reproduce smoothed_csem.*.
  fit    <- fit_rel()
  series <- .resolve_plot_columns(fit, "relative", "full", FALSE)[[1L]]
  b      <- .plot_csem_bands(fit, series, "model", "analytical", 0.95)

  bs       <- fit$by_score[order(fit$by_score$observed_score), ]
  idx      <- match(b$x, bs$observed_score)
  smoothed <- bs$smoothed_csem.relative_full[idx]
  ok       <- !is.na(smoothed)

  expect_equal(b$center[ok], smoothed[ok], tolerance = 1e-8)
})

test_that(".plot_csem_bands model band ignores asemethod", {
  # cibands = "model" must not consult asemethod -- not even to check
  # whether a bootstrap is available. fit_abs() has no bootstrap, so an
  # asemethod-sensitive path would error or differ.
  fit    <- fit_abs()
  series <- .resolve_plot_columns(fit, "absolute", NULL, FALSE)[[1L]]
  b_an <- .plot_csem_bands(fit, series, "model", "analytical", 0.95)
  b_bs <- .plot_csem_bands(fit, series, "model", "bootstrap",  0.95)
  expect_equal(b_an, b_bs)
})

test_that(".plot_csem_bands honours a non-default ci_level", {
  fit    <- fit_abs()
  series <- .resolve_plot_columns(fit, "absolute", NULL, FALSE)[[1L]]
  b95 <- .plot_csem_bands(fit, series, "person", "analytical", 0.95)
  b90 <- .plot_csem_bands(fit, series, "person", "analytical", 0.90)
  # A narrower confidence level gives a strictly narrower band wherever
  # both edges are defined and the lower edge is not truncated at zero.
  # se.* is NA at the floor/ceiling scores (csem == 0 there), so lo/hi
  # are NA at those scores -- they carry no band and are excluded.
  ok <- !is.na(b95$lo) & !is.na(b90$lo) & b95$lo > 0 & b90$lo > 0
  expect_true(any(ok))
  expect_true(all((b90$hi - b90$lo)[ok] < (b95$hi - b95$lo)[ok]))
})

test_that(".plot_csem_bands person band errors without the bootstrap SE", {
  fit    <- fit_abs()  # bootstrap = FALSE -> no se.boot.* column
  series <- .resolve_plot_columns(fit, "absolute", NULL, FALSE)[[1L]]
  expect_error(
    .plot_csem_bands(fit, series, "person", "bootstrap", 0.95),
    "bootstrap")
})

test_that(".plot_csem_bands person band works from the bootstrap SE", {
  fit    <- fit_boot()
  series <- .resolve_plot_columns(fit, "absolute", NULL, FALSE)[[1L]]
  b      <- .plot_csem_bands(fit, series, "person", "bootstrap", 0.95)
  bs     <- fit$by_score[order(fit$by_score$observed_score), ]
  z      <- stats::qnorm(0.975)
  expect_equal(b$hi,
               bs$csem.absolute + z * bs$se.boot.absolute,
               tolerance = 1e-10)
})


# -----------------------------------------------------------------------------
# plot.csem -- confidence-band smoke tests
# -----------------------------------------------------------------------------

test_that("plot.csem draws plot_type 'ci' and 'both' without error", {
  fit <- fit_abs()
  expect_no_error(with_null_device(plot(fit, plot_type = "ci")))
  expect_no_error(with_null_device(plot(fit, plot_type = "both")))
})

test_that("plot.csem returns the object invisibly under plot_type = 'both'", {
  fit <- fit_abs()
  result <- with_null_device(expect_invisible(plot(fit, plot_type = "both")))
  expect_identical(result, fit)
})

test_that("plot.csem draws model bands without error", {
  fit <- fit_rel()
  expect_no_error(with_null_device(
    plot(fit, plot_type = "both", error_types = "relative",
         method = "full", cibands = "model")))
})

test_that("plot.csem draws relative person bands without error", {
  fit <- fit_rel()
  expect_no_error(with_null_device(
    plot(fit, plot_type = "ci", error_types = "relative", method = "full")))
})

test_that("plot.csem draws person bands from the bootstrap SE", {
  fit <- fit_boot()
  expect_no_error(with_null_device(
    plot(fit, plot_type = "both", asemethod = "bootstrap")))
})

test_that("plot.csem honours a non-default ci_level in the band", {
  fit <- fit_abs()
  expect_no_error(
    with_null_device(plot(fit, plot_type = "ci", ci_level = 0.90)))
})

test_that("plot.csem errors on asemethod = 'bootstrap' without a bootstrap", {
  fit <- fit_abs()
  expect_error(
    with_null_device(plot(fit, plot_type = "ci", asemethod = "bootstrap")),
    "bootstrap")
})


# -----------------------------------------------------------------------------
# plot.csem -- deferred-branch guards
# -----------------------------------------------------------------------------

test_that("plot.csem stops on compare_methods (deferred branch)", {
  fit <- fit_rel()
  expect_error(with_null_device(plot(fit, compare_methods = TRUE)),
               "compare_methods")
})

test_that("plot.csem stops on the two-error-type side-by-side layout", {
  fit <- fit_both()
  expect_error(with_null_device(plot(fit)), "side-by-side")
})

test_that("plot.csem errors when asked for an estimator the fit lacks", {
  fit <- fit_abs()
  expect_error(with_null_device(plot(fit, error_types = "relative")),
               "csem\\.relative_full")
})
