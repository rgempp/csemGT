# Print a \`csem\` object

Displays a \`csem\` object as a sequence of console blocks mirroring the
output of the \`gtcsem\` Stata command: a header summarising the design,
the ANOVA table, the D-study population error variances and standard
errors of measurement, the reliability-like coefficients, the quadratic
smoothing fits (when smoothing was applied), and the mean sampling
variance of each estimator across persons.

## Usage

``` r
# S3 method for class 'csem'
print(x, ...)
```

## Arguments

- x:

  A \`csem\` object.

- ...:

  Currently ignored; present for S3 consistency.

## Value

\`x\`, invisibly.

## See also

\[summary.csem()\] for the score-level table and global statistics;
\[coef.csem()\] and \[by_score()\] for programmatic access to the
underlying components.

## Examples

``` r
set.seed(1)
d <- matrix(rbinom(80 * 15, 1, 0.5), nrow = 80)
fit <- csem_gt(d, cutpoint = 0.5)
#> Within-score heterogeneity detected in person-level estimates during collapse_to_score(); row-wise mean used. Verify that the per-person estimator depends on the score alone.
print(fit)
#> ----------------------------------------------------------------
#> Conditional SEMs in Generalizability Theory
#> ----------------------------------------------------------------
#> Design          :  univariate single-facet (p x i, crossed)
#> Persons (n_p)   :  80
#> G-study items   :  15
#> D-study items   :  15
#> Method          :  all
#> SE method       :  analytical
#> Smoothing       :  quadratic on observed score
#> Cutpoint        :      0.500000
#> ANOVA table
#> ----------------------------------------------------------------
#>   Effect    df              SS              MS         sigma^2
#> ----------------------------------------------------------------
#>   p           79       17.213333        0.217890     -0.002251
#>   i           14        3.796667        0.271190      0.000244
#>   pi        1106      278.336667        0.251661      0.251661
#> D-study error variances and SEMs (n_i' = 15)
#> ----------------------------------------------------------------
#>   sigma^2(Delta) =   0.016794      sigma(Delta) = 0.129590  (absolute)
#>   sigma^2(delta) =   0.016777      sigma(delta) = 0.129528  (relative)
#> Reliability-like coefficients
#> ----------------------------------------------------------------
#>   Generalizability coef.    E rho^2     =  -0.1550
#>   Dependability coef.       Phi         =  -0.1548
#>   Dep. coef. for cutpoint   Phi(lambda) =  -0.1267   (lambda =  0.500)
#> Quadratic smoothing fits  (y = b0 + b1*score + b2*score^2)
#> --------------------------------------------------------------------------
#>   Quantity              b0         b1         b2        R^2       RMSE
#> --------------------------------------------------------------------------
#>   abs_ev               -0.00000    0.07143   -0.07143     1.0000    0.00000
#>   rel_ev_full          -0.00191    0.07952   -0.07954     0.6770    0.00103
#>   rel_ev_la            -0.00189    0.07763   -0.07765     0.6719    0.00102
#>   rel_ev_unc           -0.00002    0.07143   -0.07143     1.0000    0.00000
#> Mean variance of estimator across persons
#> ----------------------------------------------------------------
#>   Quantity              Analytical
#> ----------------------------------
#>   abs_ev              6.195986e-04
#>   rel_ev_full         6.387278e-04
#>   rel_ev_la           6.232301e-04
#>   rel_ev_unc          6.202104e-04
```
