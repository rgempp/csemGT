# csemGT

The goal of `csemGT` is to estimate the conditional standard error of
measurement (CSEM) within the Generalizability Theory framework for the
univariate, single-facet, persons-by-items (p × i) crossed design,
following Brennan (1998). It was created and is maintained by [René
Gempp](https://gempp.cl/), paralleling the Stata module
[`gtcsem`](https://github.com/rgempp/gtcsem).

Unlike most other psychometric frameworks, Generalizability Theory
distinguishes two types of conditional measurement error: the **absolute
CSEM**, appropriate when decisions concern the absolute magnitude of a
person’s score (for example, mastery classification against a fixed
cutpoint), and the **relative CSEM**, appropriate when decisions concern
comparisons among persons (for example, ranking or selection). `csemGT`
estimates both.

## Installation

You can install the development version of `csemGT` from
[GitHub](https://github.com/rgempp/csemGT) with:

``` r

# Development installation
pak::pak("rgempp/csemGT")
# or
remotes::install_github("rgempp/csemGT", build_vignettes = TRUE)
```

Once on CRAN:

``` r

install.packages("csemGT")
```

## Example

``` r

library(csemGT)
data(iowa_like)

fit <- csem_gt(iowa_like)
print(fit)
plot(fit, plot_type = "both", error_types = "absolute")
```

## Citation

If you use `csemGT` in published work, please cite it as:

> Gempp, R. (2026). *csemGT: Conditional Standard Error of Measurement
> in Generalizability Theory*. R package version 1.0.0.
> <https://github.com/rgempp/csemGT>

## References

Brennan, R. L. (1998). Raw-score conditional standard errors of
measurement in generalizability theory. *Applied Psychological
Measurement*, *22*(4), 307–331.
<https://doi.org/10.1177/014662169802200401>
