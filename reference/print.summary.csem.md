# Print a \`summary.csem\` object

Displays a curated console view of a \[summary.csem()\] object: a short
header, the population-level global statistics, and the score-level
table reduced to the identifier columns, the cumulative frequency and
percentile, and the conditional-SEM point estimates (the \`csem.\*\`
columns). The sampling-variance, standard-error, confidence-interval and
smoothed columns carried by the underlying object are not shown.

## Usage

``` r
# S3 method for class 'summary.csem'
print(x, ...)
```

## Arguments

- x:

  A \`summary.csem\` object.

- ...:

  Currently ignored; present for S3 consistency.

## Value

\`x\`, invisibly.
