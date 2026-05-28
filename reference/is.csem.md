# Test whether an object is a \`csem\` object

Test whether an object is a \`csem\` object

## Usage

``` r
is.csem(x)
```

## Arguments

- x:

  Any R object.

## Value

\`TRUE\` if \`x\` inherits from \`"csem"\`, \`FALSE\` otherwise.

## Examples

``` r
set.seed(1)
d <- matrix(rbinom(80 * 15, 1, 0.5), nrow = 80)
fit <- suppressMessages(csem_gt(d, error_type = "absolute"))
is.csem(fit)
#> [1] TRUE
is.csem(list())
#> [1] FALSE
```
