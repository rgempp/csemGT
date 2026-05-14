# tests/testthat/test-csem_gt_parity_ado.R
#
# Numerical parity of the csem_gt() orchestrator against the Stata
# package gtcsem.ado v1.0.0. The reference file
# inst/extdata/gtcsem_reference_results.dta is produced by
# data-raw/make_reference_results.do, which runs
#
#   gtcsem i01-i30, method(all) semethod(analytical) generate(csem) ///
#       smooth excludeextremes cutpoint(<lambda>)
#
# on the reference dataset and persists every per-person variable plus
# every r() scalar as dataset columns (scalars as constant columns).
#
# Tolerance is 1e-6: Stata and R differ in floating-point accumulation
# order and intermediate rounding, so bit-exact parity is not expected;
# 1e-6 is far tighter than any substantively meaningful difference.
#
# NAMING: gtcsem.ado writes the relative-error columns with ABBREVIATED
# method suffixes (_full, _la, _unc), whereas csemGT uses the full
# estimator names (relative_full, relative_large_a,
# relative_uncorrelated). The mapping below is therefore explicit.

skip_if_not_installed("haven")

.ado_ref_file <- system.file("extdata", "gtcsem_reference_results.dta",
                             package = "csemGT")
.ado_dat_file <- system.file("extdata", "gtcsem_reference_data.rds",
                             package = "csemGT")
skip_if_not(nzchar(.ado_ref_file) && file.exists(.ado_ref_file),
            "gtcsem_reference_results.dta not installed")
skip_if_not(nzchar(.ado_dat_file) && file.exists(.ado_dat_file),
            "gtcsem_reference_data.rds not installed")

ado    <- haven::read_dta(.ado_ref_file)
# haven attaches Stata metadata (format.stata, variable labels, and the
# haven_labelled class) to every column. Strip it so expect_equal()
# compares values, not attributes; every column in this reference file
# is a plain numeric.
ado[]  <- lapply(ado, function(x) { attributes(x) <- NULL; x })
refdat <- readRDS(.ado_dat_file)

# Mirror the .do invocation: method(all), analytical SEs, polynomial
# smoother, exclude extremes. The cutpoint is read back from the
# reference file itself, so the test tracks whatever value the .do
# actually used rather than hard-coding it.
ado_fit <- suppressMessages(csem_gt(
  refdat,
  method           = c("full", "large_a", "uncorrelated"),
  error_type       = c("absolute", "relative"),
  smoother         = "polynomial",
  exclude_extremes = TRUE,
  cutpoint         = ado$cutpoint[1]))

est <- ado_fit$estimates


test_that("csem_gt() matches gtcsem.ado per-person point estimates", {
  # Row-alignment guard: the .rds and the .dta derive from the same
  # make_reference_data.R run, so persons appear in the same order.
  # If this fails first, the remaining comparisons are meaningless.
  expect_equal(est$observed_score, ado$csem_score, tolerance = 1e-6)

  # Absolute estimator (Brennan, 1998, eq. 20 / eq. 24).
  expect_equal(est$csem.absolute,     ado$csem_abs_csem, tolerance = 1e-6)
  expect_equal(est$csem_var.absolute, ado$csem_abs_ev,   tolerance = 1e-6)

  # Per-person cov(X_pi, item means) — Brennan (1998), eq. 33.
  expect_equal(est$cov_xim, ado$csem_cov_xim, tolerance = 1e-6)

  # The three relative-error estimators. Note the abbreviated .ado
  # suffixes: _la = large_a, _unc = uncorrelated.
  expect_equal(est$csem.relative_full,
               ado$csem_rel_csem_full, tolerance = 1e-6)
  expect_equal(est$csem.relative_large_a,
               ado$csem_rel_csem_la,   tolerance = 1e-6)
  expect_equal(est$csem.relative_uncorrelated,
               ado$csem_rel_csem_unc,  tolerance = 1e-6)

  expect_equal(est$csem_var.relative_full,
               ado$csem_rel_ev_full, tolerance = 1e-6)
  expect_equal(est$csem_var.relative_large_a,
               ado$csem_rel_ev_la,   tolerance = 1e-6)
  expect_equal(est$csem_var.relative_uncorrelated,
               ado$csem_rel_ev_unc,  tolerance = 1e-6)
})


test_that("csem_gt() matches gtcsem.ado smoothed CSEMs (extremes excluded)", {
  # With exclude_extremes the floor/ceiling persons are dropped from the
  # smoother fit: gtcsem leaves their *_sm variables missing (-> NA via
  # haven), csem_gt leaves smoothed_csem.* as NA. The NA pattern must
  # coincide, and the fitted values must agree everywhere else.
  pairs <- list(
    c("smoothed_csem.absolute",              "csem_abs_csem_sm"),
    c("smoothed_csem.relative_full",         "csem_rel_csem_sm_full"),
    c("smoothed_csem.relative_large_a",      "csem_rel_csem_sm_la"),
    c("smoothed_csem.relative_uncorrelated", "csem_rel_csem_sm_unc"))

  for (p in pairs) {
    r_col <- est[[p[[1]]]]
    s_col <- ado[[p[[2]]]]
    expect_equal(is.na(r_col), is.na(s_col), info = p[[1]])
    ok <- !is.na(s_col)
    expect_equal(r_col[ok], s_col[ok], tolerance = 1e-6, info = p[[1]])
  }
})


test_that("csem_gt() matches gtcsem.ado variance components and coefficients", {
  vc <- ado_fit$variance_components

  # ANOVA variance components (r() scalars, persisted as constant columns).
  expect_equal(vc$person,   ado$sigma2_p[1],  tolerance = 1e-6)
  expect_equal(vc$item,     ado$sigma2_i[1],  tolerance = 1e-6)
  expect_equal(vc$residual, ado$sigma2_pi[1], tolerance = 1e-6)

  # Population-level error variances and SEMs.
  expect_equal(vc$population_quantities$absolute_error_var,
               ado$absolute_error_var[1], tolerance = 1e-6)
  expect_equal(vc$population_quantities$absolute_sem,
               ado$absolute_sem[1], tolerance = 1e-6)
  expect_equal(vc$population_quantities$relative_error_var,
               ado$relative_error_var[1], tolerance = 1e-6)
  expect_equal(vc$population_quantities$relative_sem,
               ado$relative_sem[1], tolerance = 1e-6)

  # Reliability-like coefficients (Brennan, 2001, eqs. 2.40, 2.41, 2.55).
  expect_equal(vc$reliability_coefficients$erho2, ado$erho2[1],
               tolerance = 1e-6)
  expect_equal(vc$reliability_coefficients$phi, ado$phi[1],
               tolerance = 1e-6)
  expect_equal(vc$reliability_coefficients$phi_lambda, ado$phi_lambda[1],
               tolerance = 1e-6)

  # ANOVA table: rows in the conventional order person, item, residual.
  # The .do persists each cell as anova_<row>_<col>.
  expect_equal(vc$anova_table$df,
               c(ado$anova_p_df[1], ado$anova_i_df[1], ado$anova_pi_df[1]))
  expect_equal(vc$anova_table$SS,
               c(ado$anova_p_SS[1], ado$anova_i_SS[1], ado$anova_pi_SS[1]),
               tolerance = 1e-6)
  expect_equal(vc$anova_table$MS,
               c(ado$anova_p_MS[1], ado$anova_i_MS[1], ado$anova_pi_MS[1]),
               tolerance = 1e-6)
})


test_that("csem_gt() matches gtcsem.ado quadratic smoother diagnostics", {
  sf <- ado_fit$smooth_fits
  skip_if(is.null(sf), "smooth_fits is NULL")

  # gtcsem persists the smoother fit of each error-variance quantity as
  # smfit_<quantity>_<b0|b1|b2|R2|RMSE|n>; csemGT exposes the same fit
  # per estimator in smooth_fits. Both regress the per-person error
  # variance on the observed score over the non-extreme persons.
  map <- list(
    absolute              = "smfit_abs_ev",
    relative_full         = "smfit_rel_ev_full",
    relative_large_a      = "smfit_rel_ev_la",
    relative_uncorrelated = "smfit_rel_ev_unc")

  for (est_name in names(map)) {
    pfx <- map[[est_name]]
    f   <- sf[[est_name]]
    expect_equal(f$b0,   ado[[paste0(pfx, "_b0")]][1],   tolerance = 1e-6,
                 info = est_name)
    expect_equal(f$b1,   ado[[paste0(pfx, "_b1")]][1],   tolerance = 1e-6,
                 info = est_name)
    expect_equal(f$b2,   ado[[paste0(pfx, "_b2")]][1],   tolerance = 1e-6,
                 info = est_name)
    expect_equal(f$R2,   ado[[paste0(pfx, "_R2")]][1],   tolerance = 1e-6,
                 info = est_name)
    expect_equal(f$RMSE, ado[[paste0(pfx, "_RMSE")]][1], tolerance = 1e-6,
                 info = est_name)
    # n is the count of non-extreme persons used in the fit (= n_fit).
    expect_equal(f$N, ado[[paste0(pfx, "_n")]][1], info = est_name)
  }
})


test_that("csem_gt() matches gtcsem.ado analytical sampling variances", {
  # gtcsem persists the analytical sampling variance of each
  # error-variance estimator (csem_vabs_an, csem_vrev_an_{full,la,unc})
  # on the ERROR-VARIANCE scale, constant across persons. csemGT
  # computes that exact quantity in .gt_analytical_se().
  #
  # The csem_var.analytic.* columns of $estimates are a DIFFERENT
  # object: the delta-method conversion of that variance to the CSEM
  # scale, var(V) / (4 V), which varies per person. The cross-package
  # parity check therefore targets .gt_analytical_se() directly. The
  # two implementations share the alpha-gamma decomposition with
  # S_b = sum(b_vec^2) (csemGT) == SSitem / A (.ado), so parity holds
  # by construction.
  vc  <- .gt_variance_components(refdat)
  ase <- .gt_analytical_se(vc, n_items_D = ado$nitems_D[1])

  expect_equal(ase$absolute,     ado$csem_vabs_an[1],      tolerance = 1e-6)
  expect_equal(ase$full,         ado$csem_vrev_an_full[1], tolerance = 1e-6)
  expect_equal(ase$large_a,      ado$csem_vrev_an_la[1],   tolerance = 1e-6)
  expect_equal(ase$uncorrelated, ado$csem_vrev_an_unc[1],  tolerance = 1e-6)
})


test_that("csem_gt() matches gtcsem.ado sample-size scalars", {
  expect_equal(ado_fit$n_persons,           ado$A[1])
  expect_equal(ado_fit$n_items,             ado$I_observed[1])
  expect_equal(ado_fit$arguments$n_items_D, ado$nitems_D[1])
})
