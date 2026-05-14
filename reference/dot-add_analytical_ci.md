# Attach analytical-formula standard errors and CIs

Reads \`csem_var.analytic\` (the closed-form analytical sampling
variance of the per-person CSEM under each estimator, populated by
Sprint 2 in \`.gt_add_analytical_se()\`) and derives the corresponding
SE and Wald-type confidence intervals.

## Usage

``` r
.add_analytical_ci(per_person_long, ci_level = 0.95, paradigm = "gt")
```

## Arguments

- per_person_long:

  Data frame with at least \`csem\` and \`csem_var.analytic\`.

- ci_level:

  Numeric in (0, 1).

- paradigm:

  Character (currently only \`"gt"\` is supported).

## Value

The input data frame augmented with \`se.analytic\`,
\`ci_low.analytic\`, \`ci_up.analytic\`.
