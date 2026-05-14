# Extract the variance components of a \`csem\` object

Returns the \`variance_components\` list of a \`csem\` object: the ANOVA
table, the person / item / residual variance components, the
population-level error variances and SEMs, and the reliability-like
coefficients. For the GT paradigm this is the natural set of fitted
"coefficients", so it is what \`coef()\` returns.

## Usage

``` r
# S3 method for class 'csem'
coef(object, ...)
```

## Arguments

- object:

  A \`csem\` object.

- ...:

  Currently ignored.

## Value

The \`variance_components\` list of \`object\`. See \[csem_gt()\] for
its structure.

## See also

\[by_score()\] and \[as.data.frame.csem()\] for the estimation tables.

## Examples

``` r
set.seed(1)
d <- matrix(rbinom(60 * 12, 1, 0.5), nrow = 60)
fit <- csem_gt(d, error_type = "absolute")
coef(fit)$reliability_coefficients
#> $erho2
#> [1] -0.1791206
#> 
#> $phi
#> [1] -0.1784147
#> 
#> $phi_lambda
#> [1] NA
#> 
#> $smoothing_diagnostics
#> $smoothing_diagnostics$n_floor
#> [1] NA
#> 
#> $smoothing_diagnostics$n_ceiling
#> [1] NA
#> 
#> $smoothing_diagnostics$n_fit
#> [1] NA
#> 
#> 
```
