# Attach bootstrap-based standard errors and CIs to per-person estimates

Given a per-person long table (one row per (person, error_type, method)
combination) with at least columns \`person_id\`, \`csem\`,
\`error_type\`, \`method\`, this function adds \`csem_var.boot\`,
\`se.boot\`, \`ci_low.boot\`, \`ci_up.boot\` derived from the bootstrap
results returned by \`.item_bootstrap()\` or \`.person_bootstrap()\`.

## Usage

``` r
.add_bootstrap_ci(
  per_person_long,
  boot_results,
  ci_method = "percentile",
  ci_level = 0.95
)
```

## Arguments

- per_person_long:

  Data frame with at least \`person_id\`, \`csem\`, \`estimator\` (the
  merged error_type/method tag).

- boot_results:

  Output of \`.item_bootstrap()\` or \`.person_bootstrap()\`.

- ci_method:

  One of \`"percentile"\`, \`"basic"\`, \`"normal"\`, \`"bca"\`.

- ci_level:

  Numeric in (0, 1).

## Value

The input data frame augmented with bootstrap SE and CI columns
(\`csem_var.boot\`, \`se.boot\`, \`ci_low.boot\`, \`ci_up.boot\`).

## Details

Confidence intervals are computed on the CSEM scale (not the variance
scale), using one of four methods:

- \`percentile\`:

  Currently uses a normal-on-the-CSEM approximation when no replicate
  matrix is available, since the bootstrap helpers only return
  per-person variances by default. When replicates are available (e.g.
  tests with full replicate storage) the percentile method uses them.

- \`basic\`:

  Basic bootstrap (Davison & Hinkley, 1997, §5.2.1).

- \`normal\`:

  Normal approximation \\\hat\sigma \pm z\_{1-\alpha/2}\\\widehat{SE}\\.

- \`bca\`:

  Forwarded to the \`boot\` package; requires the full replicate matrix.

This helper is fully exercised in Sprint 2 once the per-person long
table is in place. In Sprint 1 it is tested on synthetic inputs.
