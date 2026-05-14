# Extract the population-level global statistics of a \`csem\` object

Pulls the relative and absolute standard errors of measurement and their
companion reliability-like coefficients out of the
\`variance_components\` component. Internal helper for
\[summary.csem()\].

## Usage

``` r
.compute_global_stats(object)
```

## Arguments

- object:

  A \`csem\` object.

## Value

A named list with \`relative_sem\`, \`erho2\`, \`absolute_sem\` and
\`phi\`.
