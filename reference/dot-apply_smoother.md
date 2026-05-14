# Smooth \`csem_var.\*\` columns of a by-score table

For each \`csem_var.\<suffix\>\` column in \`by_score\`, fits an
ordinary least squares polynomial regression of the variance on the
observed score, truncates fitted values at zero, takes the square root,
and writes the result back as \`smoothed_csem.\<suffix\>\`. Smoother
diagnostics (intercept, slope, quadratic coefficient, R^2, RMSE, sample
size used for the fit) are returned as an \`attr(\<\>, "smooth_fits")\`.

## Usage

``` r
.apply_smoother(
  by_score,
  smoother = "polynomial",
  smoother_args = list(degree = 2),
  exclude_extremes = FALSE,
  score_extremes = NULL
)
```

## Arguments

- by_score:

  Data frame with at least \`observed_score\` and one or more
  \`csem_var.\<suffix\>\` columns.

- smoother:

  Character; \`"polynomial"\` (default) or \`"none"\`. The \`"none"\`
  value returns the input unchanged.

- smoother_args:

  Named list. The element \`degree\` controls the polynomial degree
  (default 2).

- exclude_extremes:

  Logical; if \`TRUE\`, rows of \`by_score\` whose \`observed_score\` is
  in \`score_extremes\` are excluded from the OLS fit and their smoothed
  values are set to \`NA\`.

- score_extremes:

  Numeric vector of scores to exclude when \`exclude_extremes = TRUE\`.
  Typically \`c(0, J)\`.

## Value

The input \`by_score\` with new \`smoothed_csem.\<suffix\>\` columns and
a \`"smooth_fits"\` attribute (a named list, one element per smoothed
suffix).

## Details

The RMSE reported here is the population-style residual standard
deviation \`sqrt(SSE / N)\`, matching the convention used by
\`gtcsem.ado\` (see \`\_gtcsem_qfit\`) rather than the small-sample
adjusted \`sqrt(SSE / (N - k - 1))\` that \`summary(lm(.))\$sigma\`
returns. This choice preserves cross-package parity.
