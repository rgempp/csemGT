# Visual-regression tests for plot.csem, locking the layout of the four
# figures of the gtcsem Stata paper (Gempp, "Conditional standard errors
# of measurement in Generalizability Theory"). For figures 1, 3 and 4 we
# use small seeded synthetic fixtures chosen to exhibit the
# characteristic feature of the corresponding figure: vdiffr locks the
# visual arrangement of the plot, not the identity of the underlying
# data. Figure 2 contrasts the three relative-error estimators, and the
# pattern visible in the paper depends on properties of the SAT12 data
# (variable item discriminations) that a 1PL synthetic generator does
# not reproduce well, so figure 2 uses the actual SAT12 dataset
# distributed with mirt.
#
# Quantitative replications of the paper examples, on calibrated or
# real datasets, belong to the `examples` vignette and not here.
#
# Snapshots live in tests/testthat/_snaps/plot-vdiffr/. On the first
# run, vdiffr creates the baseline SVGs and the tests pass with a
# notice; on subsequent runs it compares against those baselines.
#
# Skips. The file is skipped wholesale on machines without vdiffr (a
# Suggested dependency) and on CRAN, where base-R SVG renderings drift
# across the platforms in the CRAN check farm. Figure 2 additionally
# requires mirt (also a Suggested dependency) and is skipped where mirt
# is unavailable. If CI ever becomes unreliable a skip_on_ci() can be
# added per test or once at the top.

skip_if_not_installed("vdiffr")
skip_on_cran()


# -----------------------------------------------------------------------------
# Synthetic fixtures
# -----------------------------------------------------------------------------

# Binary items with a narrow item-difficulty range. The absolute error
# variance becomes a near-perfect quadratic in the observed score
# (Brennan 2001, p. 163), mirroring the CES-D binary example of the
# paper (Figure 1). Item difficulties centred around 0 give roughly 50%
# correct, spreading the observed-score distribution well.
.fixture_binary_homog <- function(seed = 1L, N = 200L, J = 20L) {
  set.seed(seed)
  theta <- stats::rnorm(N, 0, 1.0)
  beta  <- stats::rnorm(J, 0, 0.3)
  prob  <- stats::plogis(outer(theta, beta, "-"))
  matrix(stats::rbinom(N * J, 1, prob), nrow = N, ncol = J)
}

# Polytomous items, four ordered categories. The per-person absolute
# error variance is no longer a deterministic function of the observed
# score, so the scatter shows real within-score dispersion -- the
# visual signature of the polytomous case (Gempp, paper Figure 3).
.fixture_polytomous <- function(seed = 3L, N = 200L, J = 20L,
                                cats = 4L) {
  set.seed(seed)
  theta      <- stats::rnorm(N, 0, 1.0)
  beta       <- stats::rnorm(J, 0, 0.6)
  thresholds <- seq(-1.2, 1.2, length.out = cats - 1L)
  out <- matrix(0L, nrow = N, ncol = J)
  for (j in seq_len(J)) {
    eta <- theta - beta[j]
    cum <- vapply(thresholds, function(tau) stats::plogis(eta - tau),
                  numeric(N))
    p_cat <- cbind(1 - cum[, 1L],
                   cum[, -ncol(cum)] - cum[, -1L],
                   cum[, ncol(cum)])
    for (i in seq_len(N)) {
      out[i, j] <- sample.int(cats, 1L, prob = p_cat[i, ]) - 1L
    }
  }
  out
}

# Likert items, five ordered categories. Same generative scheme as the
# polytomous fixture with one more category, mirroring the IPIP-50
# Conscientiousness example of the paper (Figure 4) on a much smaller
# sample so the snapshot rendering is fast.
.fixture_likert <- function(seed = 4L, N = 300L, J = 10L) {
  .fixture_polytomous(seed = seed, N = N, J = J, cats = 5L)
}


# -----------------------------------------------------------------------------
# Paper figures
# -----------------------------------------------------------------------------

test_that("paper figure 1 -- absolute CSEM with model CI bands, binary items", {
  fit <- suppressMessages(
    csem_gt(.fixture_binary_homog(), error_type = "absolute"))
  vdiffr::expect_doppelganger(
    "fig1-absolute-csem-binary-model-bands",
    function() {
      plot(fit, plot_type = "both", cibands = "model")
    }
  )
})

test_that("paper figure 2 -- compare overlay of the three relative estimators", {
  # Figure 2 is produced by `gtcsem plot, compare` on a fit obtained
  # WITHOUT the smooth option, so the .ado overlays the three scatter
  # clouds and no smoother lines. The R equivalent is a compare fit on
  # a smoother-less csem_gt() with compare_points = TRUE and
  # show_smooth = FALSE.
  #
  # We use the actual SAT12 dataset distributed with mirt (the same
  # dataset the paper uses for Example 5.3), scored against the key
  # supplied with the test documentation with the correction noted in
  # mirt's documentation that the key for item 32 should be 3 rather
  # than 5. The full and large_a estimators sit visibly above the
  # smooth uncorrelated curve, the signature pattern of SAT12; a 1PL
  # synthetic generator does not reproduce that pattern.
  skip_if_not_installed("mirt")
  key <- c(1L, 4L, 5L, 2L, 3L, 1L, 2L, 1L, 3L, 1L, 2L, 4L, 2L, 1L, 5L, 3L,
           4L, 4L, 1L, 4L, 3L, 3L, 4L, 1L, 3L, 5L, 1L, 3L, 1L, 5L, 4L, 3L)
  scored <- mirt::key2binary(mirt::SAT12, key)
  scored <- scored[stats::complete.cases(scored), , drop = FALSE]
  fit <- suppressMessages(
    csem_gt(scored,
            error_type = "relative",
            method     = c("full", "large_a", "uncorrelated"),
            smoother   = "none"))
  vdiffr::expect_doppelganger(
    "fig2-compare-relative-estimators",
    function() {
      plot(fit, compare_methods = TRUE,
           compare_points = TRUE, show_smooth = FALSE)
    }
  )
})

test_that("paper figure 3 -- absolute CSEM with model CI bands, polytomous items", {
  fit <- suppressMessages(
    csem_gt(.fixture_polytomous(), error_type = "absolute"))
  vdiffr::expect_doppelganger(
    "fig3-absolute-csem-polytomous-model-bands",
    function() {
      plot(fit, plot_type = "both", cibands = "model")
    }
  )
})

test_that("paper figure 4 -- relative CSEM with model CI bands, Likert items", {
  fit <- suppressMessages(
    csem_gt(.fixture_likert(),
            error_type = "relative", method = "full"))
  vdiffr::expect_doppelganger(
    "fig4-relative-csem-likert-model-bands",
    function() {
      plot(fit, plot_type = "both", cibands = "model")
    }
  )
})
