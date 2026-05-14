# Validate a \`csem\` object

Performs structural checks beyond the type assertions enforced by
\`new_csem()\`. Specifically, verifies that:

- all required top-level components are present;

- \`nrow(estimates)\` equals \`n_persons\`;

- the identifier columns \`person_id\`, \`observed_score\`,
  \`group_size\` are present in \`estimates\`.

## Usage

``` r
validate_csem(x)
```

## Arguments

- x:

  A candidate \`csem\` object.

## Value

Invisibly \`x\` if all checks pass; otherwise raises an error.

## Details

Called automatically at the end of \`csem_gt()\`; can also be invoked
programmatically.
