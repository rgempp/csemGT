# Simulated ITED-like dichotomous responses (Brennan, 1998)

A simulated person-by-item matrix of dichotomously scored responses
built to mimic the summary characteristics of the ITED Vocabulary Test
example analysed in Brennan (1998, p. 314; Figure 1, p. 315). It is
intended as a self-contained illustration dataset for the single-facet,
person-by-item crossed Generalizability Theory design implemented in
[`csem_gt`](https://gempp.cl/csemGT/reference/csem_gt.md).

## Usage

``` r
iowa_like
```

## Format

An integer matrix with 3000 rows (persons) and 40 columns (items). Each
entry is 0 or 1 (incorrect / correct). Columns are named `item01`,
`item02`, ..., `item40`.

## Source

Simulated to match the summary statistics of the ITED Vocabulary Test
example in Brennan, R. L. (1998). Raw-score conditional standard errors
of measurement in generalizability theory. *Applied Psychological
Measurement, 22*(4), 307-331. The underlying instrument is described in
Feldt, L. S., Forsyth, R. A., Ansley, T. N., & Alnot, S. D. (1993,
1994). *Iowa Tests of Educational Development*. Riverside.

## Details

**These are simulated data, not the original ITED data.** The real Iowa
Tests of Educational Development Vocabulary data described by Feldt,
Forsyth, Ansley, and Alnot (1993, 1994) are not publicly available.
`iowa_like` was generated from a Rasch (1PL) model whose parameters were
calibrated so that the ANOVA-based mean absolute and relative error
variances reproduce the values Brennan reports: \\\bar{\sigma}^2(\Delta)
\approx .00514\\ and \\\bar{\sigma}^2(\delta) \approx .00475\\. The
matrix uses \\A = 3000\\ simulated persons, matching Brennan's own use
of 3,000 generated examinees for the absolute-error illustration. The
full, seeded generation script is in `data-raw/make_iowa_like.R`.

## References

Brennan, R. L. (1998). Raw-score conditional standard errors of
measurement in generalizability theory. *Applied Psychological
Measurement, 22*(4), 307-331.

## Examples

``` r
data(iowa_like)
dim(iowa_like)
#> [1] 3000   40
iowa_like[1:5, 1:6]
#>      item01 item02 item03 item04 item05 item06
#> [1,]      1      0      0      0      0      0
#> [2,]      1      1      1      1      1      1
#> [3,]      1      1      1      1      1      1
#> [4,]      0      0      1      0      0      0
#> [5,]      1      1      1      1      1      1

## Relative conditional SEM, 'full' estimator (the default),
## reproducing the kind of dispersion seen in Brennan (1998),
## Figure 1b/1d.
if (FALSE) { # \dontrun{
fit <- csem_gt(iowa_like, error_type = "relative", method = "full")
fit
plot(fit)
} # }
```
