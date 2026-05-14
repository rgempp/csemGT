# Expand a by-score wide table to one row per person

Given a wide-format \`by_score\` table (one row per distinct value of
the conditioning variable) and a vector of per-person observed scores,
returns the corresponding person-level table by matching each person's
score to the row of \`by_score\`. Persons whose score is not represented
in \`by_score\` receive \`NA\` values in all expanded columns and
trigger a warning.

## Usage

``` r
.expand_to_person(by_score_wide, X, person_id)
```

## Arguments

- by_score_wide:

  A data frame in wide format with identifier columns \`observed_score\`
  and \`group_size\`, plus any number of estimation columns (e.g.
  \`csem.absolute\`, \`csem_var.relative_full\`,
  \`smoothed_csem.absolute\`, etc.).

- X:

  Numeric vector of length \\N\\ (the conditioning value per person;
  typically the row sum of \`data\` when \`conditioning = "total"\`).

- person_id:

  Vector of length \\N\\ with person identifiers.

## Value

A data frame with \\N\\ rows containing the identifier columns
\`person_id\`, \`observed_score\`, \`conditioning_value\`,
\`group_size\`, \`extreme\`, followed by the expanded estimation columns
from \`by_score_wide\`.
