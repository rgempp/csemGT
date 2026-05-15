# Draw the compare layout overlaying the relative estimators

Implements the compare layout of \[plot.csem()\]: the three
relative-error estimators (\`relative_full\`, \`relative_large_a\`,
\`relative_uncorrelated\`) are overlaid on one panel with a legend, so
their smoother curves can be read against each other. By default only
the curves are drawn; \`compare_points = TRUE\` adds the per-person
scatter for every estimator.

## Usage

``` r
.plot_csem_compare(
  x,
  series_list,
  theme_settings,
  compare_points = FALSE,
  show_smooth = TRUE,
  pch = 16,
  cex = NULL,
  lwd = 2,
  lty = 1,
  alpha = NULL,
  main = NULL,
  sub = NULL,
  xlab = NULL,
  ylab = NULL,
  ylim = NULL,
  xlim = NULL,
  ...
)
```

## Arguments

- x:

  A \`csem\` object.

- series_list:

  A list of three series descriptors from \[.resolve_plot_columns()\].

- theme_settings:

  A list from \[.resolve_plot_theme()\].

- compare_points:

  Logical; also draw the per-person scatter for each estimator.

- show_smooth:

  Logical; draw the smoother curves.

- pch, cex, lwd, lty, alpha:

  Graphical overrides passed through to each series.

- main, sub, xlab, ylab, ylim, xlim:

  Annotation and axis overrides. \`main\` defaults to the shared
  \`"Relative conditional SEM"\` label; \`ylim\`, when \`NULL\`, is
  shared across the three series.

- ...:

  Passed to the underlying \`plot()\` call.

## Value

The y-axis limits used, invisibly.

## Details

Each estimator keeps its own palette colour, so a user-supplied \`col\`
is not applied here. No confidence ribbon is drawn – three overlapping
ribbons would not be legible – so \`bands\` is \`NULL\` throughout and
the inner \`plot_type\` only toggles the scatter: \`"csem"\` draws it,
\`"ci"\` (with \`bands = NULL\`) suppresses both the scatter and the
ribbon, leaving the curve alone.
