# Attach bootstrap-based standard errors to the per-person long table

Given a per-person long table (long-tidy format, one row per (person,
estimator) pair) with at least columns \`person_id\`, \`estimator\`, and
\`csem\`, this helper reads the per-person bootstrap variances produced
by \`.item_bootstrap()\` or \`.person_bootstrap()\` and attaches two
columns: \`csem_var.boot\` and \`se.boot\`.

## Usage

``` r
.gt_add_bootstrap_se(per_person_long, boot_results)
```

## Arguments

- per_person_long:

  Data frame in long-tidy format, with at least \`person_id\`,
  \`estimator\`, \`csem\`.

- boot_results:

  Output of \`.item_bootstrap()\` or \`.person_bootstrap()\`; its
  \`per_person_variance\` matrix must have column names identifying the
  estimators.

## Value

The input data frame augmented with \`csem_var.boot\` and \`se.boot\`.
Estimators present in \`per_person_long\$estimator\` but absent from the
bootstrap result trigger a warning and receive NA.

## Details

Scale. The bootstrap engines return \`per_person_variance\` on the
ERROR-VARIANCE scale (the sampling variance of \\\hat V_p\\). This
helper converts to the CSEM scale via the delta method
\\\mathrm{Var}(\sqrt{V}) \approx \mathrm{Var}(V) / (4 V)\\, using the
per-person point estimate \`csem\` (which is \\\sqrt{V_p}\\) as the
evaluation point. The result \`csem_var.boot\` is therefore on the CSEM
scale, dimensionally consistent with \`se.analytic\` from
\`.gt_add_analytical_se()\` and with the Wald intervals built by
\`.gt_add_ci()\`. Rows with \`csem \<= 0\` (or NA) receive NA, since the
delta-method denominator is not defined there.

This helper performs SE attachment only. Confidence intervals are
attached separately by \`.gt_add_ci()\` (spec v4 §5.3, steps 8 and 9).
