# Coerce a \`csem\` object to a data frame

Returns one of the two wide-format tables carried by a \`csem\` object:
the person-level table (\`by = "person"\`, the default) or the
score-level table (\`by = "score"\`).

## Usage

``` r
# S3 method for class 'csem'
as.data.frame(
  x,
  row.names = NULL,
  optional = FALSE,
  by = c("person", "score"),
  ...
)
```

## Arguments

- x:

  A \`csem\` object.

- row.names, optional:

  Accepted for consistency with the \[base::as.data.frame()\] generic;
  both are ignored.

- by:

  One of \`"person"\` (the default) or \`"score"\`. \`"person"\` returns
  the per-person \`\$estimates\` table; \`"score"\` returns the
  \`\$by_score\` table.

- ...:

  Currently ignored.

## Value

A data frame: \`\$estimates\` when \`by = "person"\`, \`\$by_score\`
when \`by = "score"\`.

## See also

\[by_score()\] for the score-level table through a dedicated accessor;
\[coef.csem()\] for the variance components.

## Examples

``` r
set.seed(1)
d <- matrix(rbinom(60 * 12, 1, 0.5), nrow = 60)
fit <- csem_gt(d, error_type = "absolute")
#> Within-score heterogeneity detected in person-level estimates during collapse_to_score(); row-wise mean used. Verify that the per-person estimator depends on the score alone.
head(as.data.frame(fit))                 # by = "person"
#>   person_id observed_score conditioning_value group_size extreme      cov_xim
#> 1         1      0.4166667          0.4166667         15   FALSE  0.005681818
#> 2         2      0.4166667          0.4166667         15   FALSE -0.006439394
#> 3         3      0.5833333          0.5833333         10   FALSE -0.007196970
#> 4         4      0.4166667          0.4166667         15   FALSE  0.017803030
#> 5         5      0.5000000          0.5000000         14   FALSE  0.014393939
#> 6         6      0.5000000          0.5000000         14   FALSE  0.011363636
#>   csem_var.absolute csem.absolute csem_var.analytic.absolute
#> 1        0.02209596     0.1486471               0.0009477385
#> 2        0.02209596     0.1486471               0.0009477385
#> 3        0.02209596     0.1486471               0.0009477385
#> 4        0.02209596     0.1486471               0.0009477385
#> 5        0.02272727     0.1507557               0.0009214124
#> 6        0.02272727     0.1507557               0.0009214124
#>   se.analytic.absolute ci_low.analytic.absolute ci_up.analytic.absolute
#> 1           0.03078536                0.0883089               0.2089853
#> 2           0.03078536                0.0883089               0.2089853
#> 3           0.03078536                0.0883089               0.2089853
#> 4           0.03078536                0.0883089               0.2089853
#> 5           0.03035478                0.0912614               0.2102499
#> 6           0.03035478                0.0912614               0.2102499
#>   smoothed_csem.absolute
#> 1              0.1486471
#> 2              0.1486471
#> 3              0.1486471
#> 4              0.1486471
#> 5              0.1507557
#> 6              0.1507557
head(as.data.frame(fit, by = "score"))
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
