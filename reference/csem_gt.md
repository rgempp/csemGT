# Conditional standard errors of measurement under Generalizability Theory

Estimates conditional standard errors of measurement (CSEMs) for the
univariate, single-facet, persons-by-items (p x i) crossed design
following Brennan (1998). Returns absolute and relative error variances,
three estimators of the relative error variance, closed-form analytical
sampling variances, optional item-resampling bootstrap, quadratic
smoothing, and D-study extrapolation.

## Usage

``` r
csem_gt(
  data,
  method = c("full", "large_a", "uncorrelated"),
  error_type = c("absolute", "relative"),
  conditioning = "total",
  n_items_D = NULL,
  bootstrap = FALSE,
  R = 1000L,
  bootstrap_type = c("item", "person"),
  ci_method = c("percentile", "basic", "normal", "bca"),
  ci_level = 0.95,
  seed = NULL,
  return_analytical = TRUE,
  smoother = c("polynomial", "none"),
  smoother_args = list(degree = 2),
  exclude_extremes = FALSE,
  cutpoint = NULL,
  truncate_vc = FALSE,
  truncate_negative_error_var = FALSE,
  boot_keep_replicates = FALSE,
  na_action = c("listwise", "fail", "pairwise"),
  verbose = FALSE,
  ...
)
```

## Arguments

- data:

  Matrix or data.frame, N persons by J items, all numeric and complete
  (the single-facet crossed design requires balanced data).

- method:

  Character vector; one or more of \`"full"\`, \`"large_a"\`,
  \`"uncorrelated"\`. Default: all three. Only applies when
  \`error_type\` includes \`"relative"\`.

- error_type:

  Character vector; one or both of \`"absolute"\`, \`"relative"\`.
  Default: both.

- conditioning:

  Either \`"total"\` (the per-person mean over items, i.e.
  \`rowMeans(data)\`) or a numeric vector of length N specifying an
  alternative conditioning variable. Default: \`"total"\`. In csemGT
  v1.0 \`observed_score\` and \`conditioning_value\` coincide.

- n_items_D:

  Integer; number of items for D-study extrapolation. Default: the
  number of observed items in \`data\`.

- bootstrap:

  Logical; if \`TRUE\`, a resampling bootstrap is run. Default:
  \`FALSE\`.

- R:

  Integer; number of bootstrap replications. Default: 1000.

- bootstrap_type:

  One of \`"item"\` (default) or \`"person"\`. The item bootstrap is the
  default following Brennan (1998) and matches the \`gtcsem\` Stata
  package.

- ci_method:

  One of \`"percentile"\`, \`"basic"\`, \`"normal"\`, \`"bca"\`.
  Default: \`"percentile"\`. In csemGT v1.0 the confidence intervals are
  Wald (normal-approximation) intervals; replicate-based methods are
  deferred to a later release, and a non-\`"normal"\` request is
  satisfied with the normal interval (the requested and effective
  methods are both recorded).

- ci_level:

  Confidence level. Default: 0.95.

- seed:

  Integer or \`NULL\`; seed for the bootstrap. The bootstrap engines
  restore the prior RNG state on exit, so a non-\`NULL\` seed does not
  leak into the caller's session.

- return_analytical:

  Logical; if \`TRUE\` (default), analytical SEs are computed even when
  \`bootstrap = TRUE\`.

- smoother:

  One of \`"polynomial"\` (default) or \`"none"\`.

- smoother_args:

  List of smoother arguments. Default: \`list(degree = 2)\`.

- exclude_extremes:

  Logical; if \`TRUE\`, floor and ceiling cases are excluded from the
  smoother fit (their unsmoothed CSEMs remain, but their
  \`smoothed_csem.\*\` values are \`NA\`). Default: \`FALSE\`.

- cutpoint:

  Numeric or \`NULL\`; cutpoint lambda for the mastery dependability
  coefficient Phi(lambda), on the same mean-per-item scale as
  \`observed_score\`. Default: \`NULL\` (not computed).

- truncate_vc:

  Logical; truncate negative ANOVA variance components to zero. Default:
  \`FALSE\`.

- truncate_negative_error_var:

  Logical; truncate negative per-person error variances to zero before
  taking the CSEM square root. Default: \`FALSE\`.

- boot_keep_replicates:

  Logical; retain the bootstrap replicate summary in the output.
  Default: \`FALSE\`.

- na_action:

  One of \`"listwise"\` (default) or \`"fail"\`. \`"pairwise"\` is not
  supported for the GT paradigm.

- verbose:

  Logical; emit progress messages during the bootstrap.

- ...:

  Reserved for forward compatibility; currently ignored.

## Value

An object of class \`csem\`. Key components:

- \`estimates\`:

  Data frame, one row per person, wide format. Identifier columns
  \`person_id\`, \`observed_score\`, \`conditioning_value\`,
  \`group_size\`, \`extreme\`, followed by \`cov_xim\` and the
  estimation columns \`csem.\<estimator\>\`, \`csem_var.\<estimator\>\`,
  \`se.analytic.\<estimator\>\` / \`se.boot.\<estimator\>\`,
  \`ci_low.\*\` / \`ci_up.\*\`, and \`smoothed_csem.\<estimator\>\`.

- \`by_score\`:

  Data frame, one row per distinct observed score, the score-level
  summary of the estimation columns.

- \`variance_components\`:

  ANOVA table, the three variance components, population-level error
  variances/SEMs, and the reliability-like coefficients (\`erho2\`,
  \`phi\`, \`phi_lambda\`).

- \`smooth_fits\`:

  Per-estimator smoother diagnostics, or \`NULL\` when \`smoother =
  "none"\`.

- \`bootstrap\`:

  Bootstrap metadata, or \`NULL\` when \`bootstrap = FALSE\`.

See \[\`new_csem\`\] for the full structure.

## Details

The analytical standard errors returned in the \`se.analytic.\*\`
columns are \*\*original contributions of the csemGT package\*\*; they
are not derived in Brennan (1998), which focuses on the point
estimators. Users requiring fully verified inferential coverage should
prefer the bootstrap standard errors (\`bootstrap = TRUE\`).

## References

Brennan, R. L. (1998). Raw-score conditional standard errors of
measurement in generalizability theory. Applied Psychological
Measurement, 22(4), 307-331.

## Examples

``` r
set.seed(1)
d <- matrix(rbinom(60 * 12, 1, 0.5), nrow = 60)
fit <- csem_gt(d, error_type = "absolute")
#> Within-score heterogeneity detected in person-level estimates during collapse_to_score(); row-wise mean used. Verify that the per-person estimator depends on the score alone.
head(fit$estimates)
#>   person_id observed_score conditioning_value group_size extreme      cov_xim
#> 1         1      0.4166667          0.4166667         15   FALSE  0.005681818
#> 2         2      0.4166667          0.4166667         15   FALSE -0.006439394
#> 3         3      0.5833333          0.5833333         10   FALSE -0.007196970
#> 4         4      0.4166667          0.4166667         15   FALSE  0.017803030
#> 5         5      0.5000000          0.5000000         14   FALSE  0.014393939
#> 6         6      0.5000000          0.5000000         14   FALSE  0.011363636
#>   csem_var.absolute csem.absolute csem_var.analytic.absolute
#> 1        0.02209596     0.1486471               0.0009477385
#> 2        0.02209596     0.1486471               0.0009477385
#> 3        0.02209596     0.1486471               0.0009477385
#> 4        0.02209596     0.1486471               0.0009477385
#> 5        0.02272727     0.1507557               0.0009214124
#> 6        0.02272727     0.1507557               0.0009214124
#>   se.analytic.absolute ci_low.analytic.absolute ci_up.analytic.absolute
#> 1           0.03078536                0.0883089               0.2089853
#> 2           0.03078536                0.0883089               0.2089853
#> 3           0.03078536                0.0883089               0.2089853
#> 4           0.03078536                0.0883089               0.2089853
#> 5           0.03035478                0.0912614               0.2102499
#> 6           0.03035478                0.0912614               0.2102499
#>   smoothed_csem.absolute
#> 1              0.1486471
#> 2              0.1486471
#> 3              0.1486471
#> 4              0.1486471
#> 5              0.1507557
#> 6              0.1507557
fit$variance_components$reliability_coefficients
#> $erho2
#> [1] -0.1791206
#> 
#> $phi
#> [1] -0.1784147
#> 
#> $phi_lambda
#> [1] NA
#> 
#> $smoothing_diagnostics
#> $smoothing_diagnostics$n_floor
#> [1] NA
#> 
#> $smoothing_diagnostics$n_ceiling
#> [1] NA
#> 
#> $smoothing_diagnostics$n_fit
#> [1] NA
#> 
#> 
```
