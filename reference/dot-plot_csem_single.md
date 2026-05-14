# Draw a single-panel CSEM plot

Implements the single-panel layout of \[plot.csem()\]: a per-person
scatter of the conditional SEM against the observed score, an optional
quadratic-smoother curve, and an optional confidence-band ribbon. The
scatter is drawn from the per-person \`\$estimates\` table; the smoother
curve from the score-level \`\$by_score\` table; the ribbon from the
vertices computed by \[.plot_csem_bands()\].

## Usage

``` r
.plot_csem_single(
  x,
  series,
  theme_settings,
  show_smooth,
  plot_type,
  bands,
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

- plot_type:

  One of \`"csem"\`, \`"ci"\`, \`"both"\`. The scatter is drawn for
  \`"csem"\` and \`"both"\`; the ribbon for \`"ci"\` and \`"both"\`.

- bands:

  A list of ribbon vertices from \[.plot_csem_bands()\], or \`NULL\`
  when \`plot_type = "csem"\`.

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

## Details

Layering, back to front: faint horizontal grid, the confidence ribbon,
the per-person scatter, the smoother curve.
