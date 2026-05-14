# Resolve which estimator series a plot call should draw

Translates the \`error_types\` / \`method\` / \`compare_methods\`
arguments of \[plot.csem()\] into a list of series descriptors, one per
estimator to be drawn. Each descriptor carries the estimator key, the
names of the point-estimate and smoothed columns, a display label, a
short label for legends, and the palette colour.

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
