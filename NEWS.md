# csemGT 1.0.0

## Initial release

* `csem_gt()` implements the three relative-error estimators of Brennan (1998) -- `full`, `large_a`, `uncorrelated` -- together with the closed-form absolute error variance (Brennan 1998, eq. 20).
* Closed-form analytical sampling variances and item-resampling bootstrap variances for all four estimators.
* Quadratic smoothing of conditional error variances on observed score, with optional exclusion of floor and ceiling cases.
* D-study extrapolation via `n_items_D`.
* S3 class `csem` with full method set: `print`, `summary`, `plot`, `as.data.frame`, `by_score`, `coef`.
* `iowa_like` synthetic dataset calibrated to the ITED Vocabulary Test.
* Two vignettes: tutorial introduction and worked examples.
