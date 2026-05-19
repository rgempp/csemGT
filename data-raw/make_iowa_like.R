# ---------------------------------------------------------------------------
# data-raw/make_iowa_like.R
#
# Reproducible generation of the `iowa_like` dataset shipped with csemGT.
#
# `iowa_like` is a SIMULATED dichotomous person-by-item matrix built to
# mimic the summary characteristics of the ITED Vocabulary Test example
# in Brennan (1998, p. 314; Figure 1, p. 315):
#
#     I = 40 items, dichotomously scored
#     A = 3000 persons  (Brennan's own large-sample generation; he notes
#                         that sigma^2(Delta) was generated using 3,000
#                         rather than 493 examinees "without any
#                         noticeable change in the quadratic fit")
#     sigma^2(Delta)_bar = .00514   (mean absolute error variance)
#     sigma^2(delta)_bar = .00475   (mean relative error variance)
#
# It is NOT the real Feldt, Forsyth, Ansley & Alnot (1993, 1994) ITED
# data, to which we have no access. It is a Rasch/1PL simulation whose
# generating parameters were calibrated so that the ANOVA-based absolute
# and relative mean error variances match the values Brennan reports.
#
# Generating model (Rasch / 1PL):
#     theta_p ~ Normal(0, sigma_theta)         person ability
#     b_i      = 40 equally spaced difficulties
#     P(X_pi = 1) = plogis(theta_p - b_i)
#     X_pi    ~ Bernoulli
#
# Calibrated parameters (grid search reproducing the eq.20 / eq.35-36
# estimators of csem_gt_estimation_v3.R; see commit history / data-raw
# notes). With set.seed(1998) these yield, on the 3000 x 40 matrix,
# mean absolute error variance ~= .00515 and mean relative ('full')
# error variance ~= .00474, both within 5e-4 of Brennan's values.
#
# Reference:
#   Brennan, R. L. (1998). Raw-score conditional standard errors of
#     measurement in generalizability theory. Applied Psychological
#     Measurement, 22(4), 307-331.
#
# To regenerate the dataset, run this entire script from the package
# root with usethis/devtools installed:
#     source("data-raw/make_iowa_like.R")
# ---------------------------------------------------------------------------

set.seed(1998)

## --- calibrated generating parameters ------------------------------------
I_items     <- 40L
A_persons   <- 3000L
sigma_theta <- 1.00
b_centre    <- -0.50
b_halfrange <- 1.10
b_i         <- seq(b_centre - b_halfrange,
                    b_centre + b_halfrange,
                    length.out = I_items)

## --- simulate dichotomous responses (Rasch / 1PL) ------------------------
theta <- rnorm(A_persons, mean = 0, sd = sigma_theta)
eta   <- outer(theta, b_i, "-")          # A x I linear predictor
prob  <- plogis(eta)                     # P(correct)
iowa_like <- matrix(
  as.integer(matrix(runif(A_persons * I_items),
                     nrow = A_persons) < prob),
  nrow = A_persons, ncol = I_items
)
colnames(iowa_like) <- sprintf("item%02d", seq_len(I_items))
storage.mode(iowa_like) <- "integer"

## --- internal validation against Brennan (1998) --------------------------
## Replicates EXACTLY the estimators in csem_gt_estimation_v3.R:
##   ANOVA p x i  (lines 107-130)
##   per-person absolute error variance, Brennan eq.20  (lines 412-413)
##   per-person relative 'full', Brennan eq.35/36       (lines 186-188)
.validate_iowa_like <- function(X,
                                 target_abs = 0.00514,
                                 target_rel = 0.00475,
                                 tol = 5e-4) {
  A <- nrow(X); I <- ncol(X)
  pm <- rowMeans(X); im <- colMeans(X); gm <- mean(X)

  MS_item  <- A * sum((im - gm)^2) / (I - 1L)
  resid    <- sweep(sweep(X, 1L, pm, "-"), 2L, im, "-") + gm
  MS_resid <- sum(resid^2) / ((A - 1L) * (I - 1L))
  sigma2_item <- (MS_item - MS_resid) / A

  Im1   <- I - 1L
  s2_p  <- rowSums((X - pm)^2) / Im1
  abs_p <- s2_p / I                                  # I' = I
  cov_p <- rowSums(sweep(X - pm, 2L, im - gm, "*")) / Im1
  full_p <- ((A + 1L) / (A - 1L)) * abs_p +
            sigma2_item / I -
            (A / (A - 1L)) * (2 * cov_p / I)

  abs_mean <- mean(abs_p)
  rel_mean <- mean(full_p)
  message(sprintf(
    "iowa_like calibration check:\n  mean sigma^2(Delta) = %.6f  (Brennan .00514, |d|=%.2e)\n  mean sigma^2(delta) = %.6f  (Brennan .00475, |d|=%.2e)\n  mean proportion-correct = %.4f  range [%.4f, %.4f]",
    abs_mean, abs(abs_mean - target_abs),
    rel_mean, abs(rel_mean - target_rel),
    gm, min(pm), max(pm)))
  if (abs(abs_mean - target_abs) > tol)
    stop("mean absolute error variance off target beyond tolerance")
  if (abs(rel_mean - target_rel) > tol)
    stop("mean relative error variance off target beyond tolerance")
  invisible(TRUE)
}

.validate_iowa_like(iowa_like)

## --- write data/iowa_like.rda --------------------------------------------
usethis::use_data(iowa_like, overwrite = TRUE, compress = "xz")

message("data/iowa_like.rda written (", A_persons, " x ", I_items,
        ", integer 0/1).")
