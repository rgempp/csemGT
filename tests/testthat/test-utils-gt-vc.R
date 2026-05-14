# tests/testthat/test-utils-gt-vc.R
#
# Tests for .gt_anova() and .gt_variance_components(). These are the
# foundations of the entire csem_gt() pipeline; if they drift even at 1e-12
# the downstream parity tests fail in cascade. Tolerance is 1e-10 against
# the legacy R; against the .ado the tolerance is loosened to 1e-6 per the
# global Sprint 2 contract (defended in test-csem_gt_parity_ado.R; here we
# stay legacy-only because the .ado writes a 1x3 vc matrix that .ado-side
# parity is verified once at the top-level fit).
#
# These tests run unconditionally — they do not depend on the .dta reference,
# only on the legacy script which ships inside the package at
# inst/legacy/csem_gt_estimation_v3.R.

source(system.file("legacy", "csem_gt_estimation_v3.R",
                   package  = "csemGT",
                   mustWork = TRUE))


# -----------------------------------------------------------------------------
# .gt_anova
# -----------------------------------------------------------------------------

test_that(".gt_anova() returns 3 rows with the canonical source/df/SS/MS layout", {
  set.seed(1)
  X <- matrix(rnorm(50 * 8), nrow = 50)
  tbl <- .gt_anova(X)

  expect_s3_class(tbl, "data.frame")
  expect_equal(nrow(tbl), 3L)
  expect_identical(colnames(tbl), c("source", "df", "SS", "MS"))
  expect_identical(tbl$source, c("person", "item", "person:item"))
  expect_identical(tbl$df, c(49L, 7L, 49L * 7L))
})


test_that(".gt_anova() df values are exactly N-1, J-1, (N-1)(J-1)", {
  set.seed(2)
  X <- matrix(rnorm(120 * 15), nrow = 120)
  tbl <- .gt_anova(X)
  expect_identical(tbl$df, c(119L, 14L, 119L * 14L))
})


test_that(".gt_anova() SS components sum to total SS (Brennan eq. 3.2 identity)", {
  # SS_total = sum((X - grand_mean)^2). For a balanced single-facet crossed
  # ANOVA, SS_total = SS_person + SS_item + SS_residual (Brennan 2001, p. 30).
  set.seed(3)
  X <- matrix(rnorm(80 * 12), nrow = 80)
  grand <- mean(X)
  SS_total <- sum((X - grand)^2)

  tbl <- .gt_anova(X)
  expect_equal(sum(tbl$SS), SS_total, tolerance = 1e-12)
})


test_that(".gt_anova() reproduces g1f_components()$anova at 1e-12", {
  set.seed(4)
  X <- matrix(rnorm(60 * 10), nrow = 60)

  legacy <- g1f_components(as.data.frame(X))
  new    <- .gt_anova(X)

  # Note: legacy$anova has columns in the same order; values must match.
  expect_equal(new$df, legacy$anova$df)
  expect_equal(new$SS, legacy$anova$SS, tolerance = 1e-12)
  expect_equal(new$MS, legacy$anova$MS, tolerance = 1e-12)
})


test_that(".gt_anova() residual_matrix attribute equals legacy residual_matrix", {
  set.seed(5)
  X <- matrix(rnorm(40 * 6), nrow = 40)

  legacy <- g1f_components(as.data.frame(X))
  new    <- .gt_anova(X)
  rm_new <- attr(new, "residual_matrix")

  expect_equal(dim(rm_new), dim(legacy$residual_matrix))
  expect_equal(as.vector(rm_new), as.vector(legacy$residual_matrix),
               tolerance = 1e-12)
})


# -----------------------------------------------------------------------------
# .gt_variance_components
# -----------------------------------------------------------------------------

test_that(".gt_variance_components() reproduces g1f_components() at 1e-10", {
  set.seed(42)
  X <- matrix(rbinom(500 * 40, 1, 0.5), nrow = 500)
  storage.mode(X) <- "double"

  legacy <- g1f_components(as.data.frame(X))
  new    <- .gt_variance_components(X)

  expect_equal(new$sigma2_p,  unname(legacy$variance_components["person"]),
               tolerance = 1e-10)
  expect_equal(new$sigma2_i,  unname(legacy$variance_components["item"]),
               tolerance = 1e-10)
  expect_equal(new$sigma2_pi, unname(legacy$variance_components["resid"]),
               tolerance = 1e-10)

  # The legacy installs names ("V1",...) and rowname-derived names on its
  # mean vectors because it accepts a data.frame; the new code accepts a
  # matrix and produces unnamed vectors. Parity here is numerical, not
  # attribute-based, so we strip names before comparing.
  expect_equal(unname(new$person_mean), unname(legacy$person_mean),
               tolerance = 1e-12)
  expect_equal(unname(new$item_mean),   unname(legacy$item_mean),
               tolerance = 1e-12)
  expect_equal(new$grand_mean,          legacy$grand_mean,
               tolerance = 1e-12)
})


test_that(".gt_variance_components() row_var and cov_x_itemmean match legacy", {
  # These two vectors are the per-person ingredients of every downstream
  # estimator. Legacy computes them inside csem_g1f() (lines 749-751); we
  # mirror that computation here.
  set.seed(7)
  X <- matrix(rnorm(100 * 12), nrow = 100)
  vc <- .gt_variance_components(X)

  person_mean <- rowMeans(X)
  item_mean   <- colMeans(X)
  grand_mean  <- mean(X)
  person_centered <- sweep(X, 1L, person_mean, "-")
  b_vec_ref <- item_mean - grand_mean

  expected_row_var <-
    rowSums(person_centered * person_centered) / (ncol(X) - 1L)
  expected_cov     <-
    as.vector(person_centered %*% b_vec_ref) / (ncol(X) - 1L)

  expect_equal(vc$row_var, expected_row_var, tolerance = 1e-12)
  expect_equal(vc$cov_x_itemmean, expected_cov, tolerance = 1e-12)
  expect_equal(vc$b_vec, b_vec_ref, tolerance = 1e-12)
})


test_that(".gt_variance_components() truncate_vc = TRUE truncates negatives at 0", {
  # Construct a case with at least one negative variance component. The
  # zero-column-variance trick guarantees that sigma2_i < 0 (because
  # (MS_item - MS_residual)/A = (0 - MSr)/N < 0 whenever MSr > 0). Whether
  # sigma2_p is also negative depends on the noise realization with
  # small N; with set.seed(9), N = 10, J = 4 it happens to be negative
  # too. The test is therefore written agnostically: it asserts the
  # universal contract (truncate_vc = TRUE yields non-negatives, and
  # already-non-negative components pass through unchanged) without
  # presuming which components fall on which side.
  set.seed(9)
  N <- 10
  J <- 4
  X <- matrix(rnorm(N * J), N, J)
  X <- sweep(X, 2L, colMeans(X), "-") + mean(X)  # kills item variance

  vc_nt <- .gt_variance_components(X, truncate_vc = FALSE)
  vc_tr <- .gt_variance_components(X, truncate_vc = TRUE)

  # Construction guarantees sigma2_i < 0.
  expect_lt(vc_nt$sigma2_i, 0)

  for (cmp in c("sigma2_p", "sigma2_i", "sigma2_pi")) {
    expect_gte(vc_tr[[cmp]], 0)
    if (vc_nt[[cmp]] >= 0) {
      expect_identical(vc_tr[[cmp]], vc_nt[[cmp]])
    } else {
      expect_identical(vc_tr[[cmp]], 0)
    }
  }
})


test_that(".gt_variance_components() errors out on N < 2 or J < 2", {
  expect_error(.gt_variance_components(matrix(0, 1, 5)),
               "at least 2 persons and 2 items")
  expect_error(.gt_variance_components(matrix(0, 5, 1)),
               "at least 2 persons and 2 items")
})


test_that(".gt_variance_components() N and J reflect input dimensions", {
  X <- matrix(rnorm(7 * 13), 7, 13)
  vc <- .gt_variance_components(X)
  expect_identical(vc$N, 7L)
  expect_identical(vc$J, 13L)
})
