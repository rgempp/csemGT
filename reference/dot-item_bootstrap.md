# Item-resampling bootstrap for the GT CSEM estimators

Implements the default bootstrap of \`csem_gt()\`. For each person,
draws \`R\` independent samples of items with replacement (each of size
\\J\\ = \`ncol(data)\`), recomputes the four GT per-person estimators on
every resampled item set, and returns the per-person empirical variances
across the \`R\` replicates.

## Usage

``` r
.item_bootstrap(
  data,
  vc,
  n_items_D = NULL,
  R = 1000L,
  seed = NULL,
  return_replicates = FALSE,
  verbose = FALSE
)
```

## Arguments

- data:

  Numeric matrix \\N \times J\\.

- vc:

  Output of \`.gt_variance_components(data)\` — the variance components
  and per-person ingredients from the ORIGINAL sample (NOT from any
  bootstrap resample). Item bootstrap holds \`sigma2_i\` and \`b_vec\`
  fixed across replicates; this is the parity anchor against the legacy
  and \`.ado\` implementations.

- n_items_D:

  Positive scalar; D-study number of items. Defaults to \`ncol(data)\`.

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

A named list with components \`type = "item"\`, \`R\`, \`seed\`, and
\`per_person_variance\` (an \\N \times 4\\ matrix with columns
\`c("absolute", "relative_full", "relative_large_a",
"relative_uncorrelated")\`). When \`return_replicates = TRUE\`, a
\`\$replicates\` element of the same shape contains the per-person mean
across replicates for each estimator.

## Details

Rationale: item resampling preserves the per-person interpretation of
the conditional SEM (the unit of resampling is the same as the unit of
replication that underlies the estimator), matching the \`gtcsem.ado\`
Stata implementation. See also the legacy R reference
\`bootstrap_csem_g1f()\` in \`inst/legacy/\`.
