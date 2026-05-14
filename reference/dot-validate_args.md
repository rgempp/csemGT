# Lightweight check of scalar arguments supplied to csem_gt()

Validates types and ranges of the scalar arguments that \`csem_gt()\`
accepts. Numeric requirements are enforced (e.g. \`R \>= 100\`,
\`ci_level in (0, 1)\`); logical flags are checked for length 1.

## Usage

``` r
.validate_args(
  ci_level = 0.95,
  R = 1000L,
  n_items_D = NULL,
  cutpoint = NULL,
  seed = NULL,
  bootstrap = FALSE,
  return_analytical = TRUE,
  exclude_extremes = FALSE,
  truncate_vc = FALSE,
  truncate_negative_error_var = FALSE,
  boot_keep_replicates = FALSE,
  verbose = FALSE
)
```

## Arguments

- ci_level, R, n_items_D, cutpoint, seed:

  As in \`csem_gt()\`.

- bootstrap, return_analytical, exclude_extremes, truncate_vc,
  truncate_negative_error_var, boot_keep_replicates, verbose:

  As in \`csem_gt()\`.

## Value

Invisibly \`TRUE\`.
