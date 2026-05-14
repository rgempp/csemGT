# Summarise a \`csem\` object

Produces a score-level summary of a \`csem\` object. The returned object
carries the full \`by_score\` table augmented with two columns,
\`cum_freq\` (the cumulative proportion of persons at or below each
observed score) and \`percentile\` (\`100 \* cum_freq\`), together with
the population-level global statistics: the relative and absolute
standard errors of measurement and their companion reliability-like
coefficients (the generalizability coefficient \`E rho^2\` and the
dependability coefficient \`Phi\`).

## Usage

``` r
# S3 method for class 'csem'
summary(object, ...)
```

## Arguments

- object:

  A \`csem\` object.

- ...:

  Currently ignored; present for S3 consistency.

## Value

An object of class \`summary.csem\`: a list with components \`call\`,
\`paradigm\`, \`methods\`, \`error_types\`, \`n_persons\`, \`n_items\`,
\`global_stats\` (a list with \`relative_sem\`, \`erho2\`,
\`absolute_sem\`, \`phi\`) and \`by_score_summary\` (the augmented
\`by_score\` table).

## Details

The returned object retains every column of the original \`by_score\`
table; \[print.summary.csem()\] displays a curated subset.

## See also

\[print.csem()\] for the full-object display; \[by_score()\] and
\[coef.csem()\] for programmatic access to the underlying components.

## Examples

``` r
set.seed(1)
d <- matrix(rbinom(80 * 15, 1, 0.5), nrow = 80)
fit <- csem_gt(d, error_type = "absolute")
#> Within-score heterogeneity detected in person-level estimates during collapse_to_score(); row-wise mean used. Verify that the per-person estimator depends on the score alone.
s <- summary(fit)
s$global_stats
#> $relative_sem
#> [1] 0.1295275
#> 
#> $erho2
#> [1] -0.1549878
#> 
#> $absolute_sem
#> [1] 0.1295903
#> 
#> $phi
#> [1] -0.1548144
#> 
head(s$by_score_summary)
#>   observed_score group_size     cov_xim csem_var.absolute csem.absolute
#> 1      0.2000000          1 0.011250000        0.01142857     0.1069045
#> 2      0.2666667          4 0.007113095        0.01396825     0.1181874
#> 3      0.3333333          9 0.001587302        0.01587302     0.1259882
#> 4      0.4000000         16 0.002578125        0.01714286     0.1309307
#> 5      0.4666667         19 0.003536967        0.01777778     0.1333333
#> 6      0.5333333         12 0.005000000        0.01777778     0.1333333
#>   csem_var.analytic.absolute se.analytic.absolute ci_low.analytic.absolute
#> 1               0.0009033233           0.03005534               0.04799712
#> 2               0.0007390827           0.02718608               0.06490364
#> 3               0.0006503928           0.02550280               0.07600359
#> 4               0.0006022155           0.02454008               0.08283306
#> 5               0.0005807078           0.02409788               0.08610236
#> 6               0.0005807078           0.02409788               0.08610236
#>   ci_up.analytic.absolute smoothed_csem.absolute cum_freq percentile
#> 1               0.1658119              0.1069045   0.0125       1.25
#> 2               0.1714711              0.1181874   0.0625       6.25
#> 3               0.1759727              0.1259882   0.1750      17.50
#> 4               0.1790284              0.1309307   0.3750      37.50
#> 5               0.1805643              0.1333333   0.6125      61.25
#> 6               0.1805643              0.1333333   0.7625      76.25
```
