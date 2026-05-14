# The csemGT colour palette

Returns the package colour palette: one colour per CSEM estimator
(\`absolute\`, \`relative_full\`, \`relative_large_a\`,
\`relative_uncorrelated\`) plus a neutral \`structural\` colour used for
grid lines and other non-data elements. The hues are taken from the
Okabe-Ito qualitative palette, which is colourblind-safe.
\[plot.csem()\] uses this palette by default; it is exported so the same
colours can be reused elsewhere, for example to keep a manuscript figure
consistent with the package plots.

## Usage

``` r
csem_palette(which = NULL)
```

## Arguments

- which:

  Optional character vector of palette entry names to return. If
  \`NULL\` (the default), the full named vector is returned.

## Value

A named character vector of hex colour strings.

## See also

\[plot.csem()\], which uses this palette.

## Examples

``` r
csem_palette()
#>              absolute         relative_full      relative_large_a 
#>             "#E69F00"             "#0072B2"             "#009E73" 
#> relative_uncorrelated            structural 
#>             "#CC79A7"             "#BBBBBB" 
csem_palette("relative_full")
#> relative_full 
#>     "#0072B2" 
csem_palette(c("absolute", "relative_full"))
#>      absolute relative_full 
#>     "#E69F00"     "#0072B2" 
```
