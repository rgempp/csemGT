# Person-resampling bootstrap for the GT CSEM estimators

Resamples persons with replacement, refits the variance components, and
recomputes the per-person CSEMs on every replicate. The per-person
sampling variance is then evaluated by aligning the replicate estimates
with the original person index (or, equivalently for csem_gt, with the
score level) and computing the empirical variance across replicates.
This is the canonical bootstrap of the other paradigms
(\`csem_split_half\`, \`csem_anova\`, \`csem_binomial\`) and is provided
here for forward compatibility with \`csemR\`.

## Usage

``` r
.person_bootstrap(
  data,
  X,
  paradigm = "gt",
  method = c("full", "large_a", "uncorrelated"),
  error_type = c("absolute", "relative"),
  R = 1000L,
  seed = NULL,
  return_replicates = FALSE,
  verbose = FALSE
)
```

## Arguments

- data:

  Numeric matrix \\N \times J\\.

- X:

  Numeric vector of length \\N\\; conditioning value per person.

- paradigm:

  Character; currently must be \`"gt"\`.

- method, error_type:

  Resolved arguments of \`csem_gt()\`.

- R:

  Integer; number of bootstrap replications. Default 1000.

- seed:

  Integer or \`NULL\`; seed for reproducibility.

- return_replicates:

  Logical; if \`TRUE\`, also returns the per-person mean of the
  replicate estimates in component \`\$replicates\`.

- verbose:

  Logical; emit progress messages every 100 persons.

## Value

A list with the same shape as \`.item_bootstrap()\`.
