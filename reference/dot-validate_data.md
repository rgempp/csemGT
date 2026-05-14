# Validate the input data matrix

Coerces a data frame or matrix to a numeric matrix and checks the
preconditions of the persons-by-items single-facet crossed design: all
columns numeric, no missing values (the legacy \`csem_g1f()\` and
\`gtcsem.ado\` both require complete balanced data), and at least 2
persons by 2 items.

## Usage

``` r
.validate_data(
  data,
  na_action = "listwise",
  require_balanced = TRUE,
  require_complete = TRUE,
  require_dichotomous = FALSE
)
```

## Arguments

- data:

  Matrix or data frame, persons in rows, items in columns.

- na_action:

  Character; one of \`"listwise"\`, \`"fail"\`, \`"pairwise"\`. In
  \`csemGT\` v1.0 only \`"listwise"\` and \`"fail"\` are supported.

- require_balanced:

  Logical; reserved for future designs. Currently the single-facet
  crossed design is always balanced by construction.

- require_complete:

  Logical; if \`TRUE\` (default), an error is raised when missing values
  remain after \`na_action\` is applied.

- require_dichotomous:

  Logical; reserved for \`csemR\`'s binomial paradigm. Ignored in
  \`csemGT\`.

## Value

A numeric matrix, persons in rows, items in columns.
