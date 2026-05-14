# Resolve which estimator series a plot call should draw

Translates the \`error_types\` / \`method\` / \`compare_methods\`
arguments of \[plot.csem()\] into a list of series descriptors, one per
estimator to be drawn. Each descriptor carries the estimator key, the
names of the point-estimate, error-variance, sampling-SE and smoothed
columns, a display label, a short label for legends, and the palette
colour.

## Usage

``` r
.resolve_plot_columns(x, error_types, method, compare_methods)
```

## Arguments

- x:

  A \`csem\` object.

- error_types:

  Character vector of error types to plot.

- method:

  Relative-error estimator to use for \`error_types\` containing
  \`"relative"\`.

- compare_methods:

  Logical; if \`TRUE\`, the three relative-error estimators are returned
  regardless of \`error_types\` and \`method\`.

## Value

A list of series descriptors (each itself a list).

## Details

The sampling-SE and error-variance column names are constructed but not
validated here: a fit may legitimately lack \`se.boot.\*\` (no bootstrap
was run), and whether a given band source needs a given column is
decided by \[.plot_csem_bands()\] at band-computation time.
