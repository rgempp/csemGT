# =============================================================================
# data-raw/make_reference_data.R
#
# Generates the parity reference dataset for csemGT Sprint 2 tests.
#
# Produces two files:
#   - inst/extdata/gtcsem_reference_data.rds  (R matrix, double)
#   - data-raw/gtcsem_reference_data.dta      (Stata input for gtcsem.ado)
#
# Design rationale
# ----------------
# 300 persons x 30 binary items. A Rasch-like generator with normal abilities
# (sd = 1) and uniform difficulties in (-1.5, 1.5) keeps the observed-score
# distribution well in the interior, with at most a handful of floor / ceiling
# cases that exercise `exclude_extremes = TRUE` without making the smoothing
# fit degenerate. Binary scoring is intentional: it also lets the same dataset
# be reused (with a different test, see test-csem_gt_identity.R) to verify
# Brennan (1998) eq. 8 against the Lord (1955) binomial CSEM on the
# proportion-correct scale.
#
# Seed is fixed and public (20260513) so that anyone re-running this script
# obtains the same reference results from gtcsem.ado.
#
# Reproducibility
# ---------------
# Run from the package root with:
#   source("data-raw/make_reference_data.R")
# Outputs are written relative to the package root.
# =============================================================================

set.seed(20260513L)

N <- 300L
J <-  30L

theta <- rnorm(N, mean = 0, sd = 1)
beta  <- runif(J, min = -1.5, max = 1.5)

# Logistic IRF (2PL with a = 1, i.e. Rasch on the logit scale)
p_mat <- plogis(outer(theta, beta, "-"))
ref_data <- matrix(rbinom(N * J, size = 1L, prob = as.vector(p_mat)),
                   nrow = N, ncol = J)
storage.mode(ref_data) <- "double"
colnames(ref_data) <- sprintf("i%02d", seq_len(J))

# --- Sanity diagnostics (not stored; useful when re-running interactively) ---
cat("N =", N, " J =", J, "\n")
cat("Person totals: min =", min(rowSums(ref_data)),
    " max =", max(rowSums(ref_data)),
    " mean =", round(mean(rowSums(ref_data)), 2), "\n")
cat("Item p-values: min =", round(min(colMeans(ref_data)), 3),
    " max =", round(max(colMeans(ref_data)), 3), "\n")
cat("Floor cases (all 0) :", sum(rowSums(ref_data) == 0), "\n")
cat("Ceiling cases (all J):", sum(rowSums(ref_data) == J), "\n")

# --- 1. RDS for R parity tests --------------------------------------------
saveRDS(ref_data, file = "inst/extdata/gtcsem_reference_data.rds",
        version = 2L, compress = "xz")

# --- 1b. cutpoint for the phi(lambda) reference run -----------------------
# Fixed at 0.6 on the mean-per-item scale (NOT the total score scale).
#
# Why mean-per-item: gtcsem.ado evaluates Brennan (2001, eq. 2.55) using
# scalar(`grand') = r(mean) of `pm' = egen rowmean(varlist) — i.e., the
# grand mean is the mean of person means on the per-item scale (~0.5 for
# this binary dataset). The cutpoint lambda must live on the same scale,
# or the term (grand - lambda)^2 dominates the formula and Phi(lambda)
# collapses numerically to 1. The legacy R helper csem_g1f() uses the
# identical convention (mean(X), not sum(X)).
#
# Why the literal 0.6: matches the example call in Gempp (2026, draft
# v04, Listing 1: "gtcsem p1-p20, method(full) semethod(analytical)
# cutpoint(0.60)"). With grand_mean ~ 0.5 in this dataset, lambda = 0.6
# yields (grand - lambda)^2 = 0.01, comparable in order of magnitude to
# sigma^2_p and to sigma^2(Delta), so the resulting Phi(lambda) is
# informative and clearly distinct from both Phi and E rho^2.
ref_cutpoint <- 0.6
cat("cutpoint (fixed, mean-per-item scale) =", ref_cutpoint, "\n")
saveRDS(ref_cutpoint,
        file = "inst/extdata/gtcsem_reference_cutpoint.rds",
        version = 2L, compress = "xz")
writeLines(as.character(ref_cutpoint),
           con = "data-raw/gtcsem_reference_cutpoint.txt")

# --- 2. DTA for Stata input -----------------------------------------------
# haven preserves doubles round-trip without precision loss at 1e-12 or finer.
if (!requireNamespace("haven", quietly = TRUE)) {
  stop("Package 'haven' is required to write the Stata input file. ",
       "Run install.packages('haven') and re-source this script.")
}
haven::write_dta(as.data.frame(ref_data),
                 path = "data-raw/gtcsem_reference_data.dta",
                 version = 14L)

cat("\nWrote:\n",
    "  inst/extdata/gtcsem_reference_data.rds\n",
    "  data-raw/gtcsem_reference_data.dta\n",
    "Next step: run data-raw/make_reference_results.do in Stata.\n",
    sep = "")
