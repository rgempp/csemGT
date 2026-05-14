# Draw a single-panel CSEM plot

Implements the single-panel layout of \[plot.csem()\] for \`plot_type =
"csem"\`: a per-person scatter of the conditional SEM against the
observed score, with an optional quadratic-smoother curve. The scatter
is drawn from the per-person \`\$estimates\` table; the smoother curve
from the score-level \`\$by_score\` table.

## Usage

``` r
.plot_csem_single(
  x,
  series,
  theme_settings,
  show_smooth,
  col,
  pch,
  cex,
  lwd,
  lty,
  alpha,
  main,
  sub,
  xlab,
  ylab,
  ylim,
  xlim,
  add,
  ...
)
```

## Arguments

- x:

  A \`csem\` object.

- series:

  A single series descriptor from \[.resolve_plot_columns()\].

- theme_settings:

  A list from \[.resolve_plot_theme()\].

- show_smooth:

  Logical; draw the smoother curve when available.

- col, pch, cex, lwd, lty, alpha:

  Graphical overrides; \`NULL\` means "use the theme or calibrated
  default".

- main, sub, xlab, ylab, ylim, xlim:

  Annotation and axis overrides; \`NULL\` means "use a sensible
  default".

- add:

  Logical; if \`TRUE\`, draw onto the current plot without opening a new
  one.

- ...:

  Passed to the underlying \`plot()\` call.

## Value

\`NULL\`, invisibly.
