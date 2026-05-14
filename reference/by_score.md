# Extract the score-level table from a \`csem\` object

\`by_score()\` returns the score-level summary table of a \`csem\`
object: one row per distinct observed score, with the estimation columns
collapsed to that score. It is the accessor-style equivalent of the
\`\$by_score\` component and of \`as.data.frame(x, by = "score")\`.

## Usage

``` r
by_score(x, ...)

# S3 method for class 'csem'
by_score(x, ...)
```

## Arguments

- x:

  A \`csem\` object.

- ...:

  Currently ignored; present for S3 extensibility.

## Value

A data frame with one row per distinct observed score.

## See also

\[as.data.frame.csem()\] for the person-level and score-level tables
through the \`as.data.frame\` interface; \[coef.csem()\] for the
variance components.

## Examples

``` r
set.seed(1)
d <- matrix(rbinom(60 * 12, 1, 0.5), nrow = 60)
fit <- csem_gt(d, error_type = "absolute")
#> Within-score heterogeneity detected in person-level estimates during collapse_to_score(); row-wise mean used. Verify that the per-person estimator depends on the score alone.
head(by_score(fit))
#>   observed_score group_size       cov_xim csem_var.absolute csem.absolute
#> 1      0.1666667          1 -0.0022727273        0.01262626     0.1123666
#> 2      0.2500000          3  0.0008838384        0.01704545     0.1305582
#> 3      0.3333333          7  0.0034632035        0.02020202     0.1421338
#> 4      0.4166667         15  0.0076010101        0.02209596     0.1486471
#> 5      0.5000000         14  0.0042207792        0.02272727     0.1507557
#> 6      0.5833333         10  0.0052272727        0.02209596     0.1486471
#>   csem_var.analytic.absolute se.analytic.absolute ci_low.analytic.absolute
#> 1               0.0016585424           0.04072521               0.03254671
#> 2               0.0012285499           0.03505068               0.06186018
#> 3               0.0010365890           0.03219610               0.07903061
#> 4               0.0009477385           0.03078536               0.08830890
#> 5               0.0009214124           0.03035478               0.09126140
#> 6               0.0009477385           0.03078536               0.08830890
#>   ci_up.analytic.absolute smoothed_csem.absolute
#> 1               0.1921866              0.1123666
#> 2               0.1992563              0.1305582
#> 3               0.2052370              0.1421338
#> 4               0.2089853              0.1486471
#> 5               0.2102499              0.1507557
#> 6               0.2089853              0.1486471
```
