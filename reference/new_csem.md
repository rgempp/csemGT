# Construct a \`csem\` object

Low-level constructor for the \`csem\` S3 class. This is an internal
building block used by \`csem_gt()\` and (eventually) the other
\`csem\_\*()\` paradigms in the \`csemR\` family. End users should not
call this directly.

## Usage

``` r
new_csem(
  estimates,
  by_score,
  call,
  paradigm,
  methods,
  error_types,
  arguments,
  variance_components = NULL,
  smooth_fits = NULL,
  bootstrap = NULL,
  scale_transform = NULL,
  n_persons,
  n_items,
  version = .csemGT_version()
)
```

## Arguments

- estimates:

  A data frame, one row per person, in wide format. Must contain at
  minimum the identifier columns \`person_id\`, \`observed_score\`,
  \`group_size\`.

- by_score:

  A data frame, one row per distinct value of the conditioning variable,
  also in wide format.

- call:

  A captured \`match.call()\` of the user-facing function.

- paradigm:

  Character scalar; one of \`"gt"\`, \`"split_half"\`, \`"anova"\`,
  \`"binomial"\`.

- methods:

  Character vector of methods applied.

- error_types:

  Character vector of error types reported.

- arguments:

  Named list with the resolved arguments of the user-facing call.

- variance_components:

  Named list with ANOVA-based variance components (used by the GT
  paradigm). May be \`NULL\`.

- smooth_fits:

  Named list of smoother diagnostics (one element per smoothed column).
  May be \`NULL\`.

- bootstrap:

  Named list with bootstrap metadata and replicates, or \`NULL\` when
  bootstrap is not performed.

- scale_transform:

  Scale transformation specification, or \`NULL\`.

- n_persons, n_items:

  Integer scalars.

- version:

  Package version. Defaults to the installed version of \`csemGT\`;
  falls back to \`"0.0.0"\` when the package is being built.

## Value

An object of class \`c("csem", "list")\`.
