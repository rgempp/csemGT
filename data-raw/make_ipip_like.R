# data-raw/make_ipip_like.R
#
# Generator for the `ipip_like` dataset: a simulated set of responses
# to a hypothetical 10-item, 5-point Likert subscale, intended as the
# illustration dataset for the polytomous Likert use case in the
# csemGT 'Worked examples' vignette and in the package examples in
# general. The simulator implements the single-facet p x i random-
# effects model of Generalizability Theory directly: each score is
# constructed as the sum of independent normal person, item and
# residual effects, then rounded and clipped to the 1-5 Likert metric.
# To absorb the variance contraction induced by the clip-and-round
# step, the pre-truncation variance components are set above their
# post-truncation targets; the recovered ANOVA estimates of the
# components are checked at the end of the script and should match the
# targets within roughly one percent.
#
# The post-truncation targets are chosen to be broadly comparable to
# those obtained for the Conscientiousness subscale of the IPIP-50
# inventory, as administered in the public dataset of the Open-Source
# Psychometrics Project (https://openpsychometrics.org/rawdata/):
#
#   sigma^2(p)  ~ 0.434
#   sigma^2(i)  ~ 0.136
#   sigma^2(pi) ~ 1.000
#   E rho^2     ~ 0.81
#
# Item effects are drawn once and then rescaled to have *exact* sample
# SD = sqrt(sigma^2_i) before person and residual effects are drawn,
# so that sigma^2(i) does not depend on Monte-Carlo fluctuations of
# the 10 item draws. This mirrors the approach taken in `iowa_like`,
# where item difficulties are also treated as fixed quantities.
#
# To regenerate the dataset:
#
#   source("data-raw/make_ipip_like.R")
#
# This script is run by the maintainer; it is not part of the package
# itself. The resulting integer matrix is written to data/ipip_like.rda
# via usethis::use_data(). See `?ipip_like` for the user-facing
# documentation.

set.seed(1998)

# ---- Design constants and pre-truncation inputs ----------------------------
A     <- 2000L          # number of simulated persons
I     <- 10L            # number of items
mu    <- 3.30           # grand mean on the 1-5 scale
s2_p  <- 0.620          # inflated input; post-trunc target ~ 0.434
s2_i  <- 0.190          # inflated input; post-trunc target ~ 0.136
s2_pi <- 1.220          # inflated input; post-trunc target ~ 1.000

# ---- Draw and rescale item effects to exact pre-trunc SD --------------------
delta <- stats::rnorm(I)
delta <- delta - mean(delta)
delta <- delta * (sqrt(s2_i) / stats::sd(delta))

# ---- Draw person effects and residuals --------------------------------------
theta <- stats::rnorm(A, mean = 0, sd = sqrt(s2_p))
eps   <- matrix(stats::rnorm(A * I, mean = 0, sd = sqrt(s2_pi)),
                nrow = A, ncol = I)

# ---- Linear predictor, round, and clip to the 1-5 Likert metric ------------
lin       <- mu + outer(theta, delta, "+") + eps
ipip_like <- pmin(5L, pmax(1L, as.integer(round(lin))))
ipip_like <- matrix(ipip_like, nrow = A, ncol = I)
colnames(ipip_like) <- sprintf("item%02d", seq_len(I))
storage.mode(ipip_like) <- "integer"

# ---- Sanity check: recover variance components via direct ANOVA ------------
# Single-facet p x i design with one observation per cell: SS_pi is the
# residual after removing person and item effects. ANOVA estimators from
# Brennan (2001, eq. 3.2).
grand     <- mean(ipip_like)
row_means <- rowMeans(ipip_like)
col_means <- colMeans(ipip_like)
SS_p      <- I * sum((row_means - grand)^2)
SS_i      <- A * sum((col_means - grand)^2)
SS_tot    <- sum((ipip_like - grand)^2)
SS_pi     <- SS_tot - SS_p - SS_i
MS_p      <- SS_p  / (A - 1)
MS_i      <- SS_i  / (I - 1)
MS_pi     <- SS_pi / ((A - 1) * (I - 1))
s2_p_hat  <- (MS_p  - MS_pi) / I
s2_i_hat  <- (MS_i  - MS_pi) / A
s2_pi_hat <- MS_pi
Erho2     <- s2_p_hat / (s2_p_hat + s2_pi_hat / I)

message("Recovered ANOVA components (targets in parentheses):")
message(sprintf("  sigma^2(p)  = %.4f  (target 0.434)", s2_p_hat))
message(sprintf("  sigma^2(i)  = %.4f  (target 0.136)", s2_i_hat))
message(sprintf("  sigma^2(pi) = %.4f  (target 1.000)", s2_pi_hat))
message(sprintf("  E rho^2     = %.4f  (target ~0.81)", Erho2))
message(sprintf("  Grand mean  = %.3f  (target ~3.30)", grand))

# ---- Write ------------------------------------------------------------------
usethis::use_data(ipip_like, overwrite = TRUE)
