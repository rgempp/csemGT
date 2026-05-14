###############################################################
# Conditional Standard Errors of Measurement
# in Generalizability Theory
#
# Univariate single-facet crossed design: persons x items.
# Implements Brennan (1998) and Brennan (2001) section 5.4.1.
#
# Three estimators of the per-person relative error variance:
#
#   "full"          - Brennan eq.35/36, with finite-A correction
#                     and per-person item-residual covariance.
#   "large_a"       - Brennan eq.40, drops finite-A correction
#                     (assumes A -> infinity).
#   "uncorrelated"  - Brennan eq.41, additionally assumes
#                     sigma(i, r_p) = 0 for the focal person.
#
# The absolute error variance has a single estimator
# (Brennan eq.20).
#
# Two methods to obtain standard errors of these per-person
# estimators:
#
#   "analytical"  - Closed-form approximation:
#                   * abs / uncorrelated: sample-variance moment-4
#                     formula (Casella & Berger 2002, Thm 5.2.4).
#                   * full / large_a    : chi-square approximation
#                     2 * estimate^2 / (I-1) under normality.
#                   Treats sigma^2(i) as fixed (large A).
#
#   "bootstrap"   - Item-resampling bootstrap (B replicates).
#                   Resamples item indices with replacement and
#                   recomputes the focal estimator each time.
###############################################################


###############################################################
# Variance components for p x i crossed G-study
###############################################################

#' Variance components for a univariate single-facet G-study
#'
#' Estimates the variance components for the persons-by-items
#' (p x i) crossed Generalizability Theory design via the standard
#' ANOVA decomposition.
#'
#' @param dat A numeric data frame or matrix with persons in rows
#'   and items in columns. Must be balanced and complete (no
#'   missing values); all columns must be numeric.
#' @param truncate_vc Logical. If \code{TRUE}, negative ANOVA
#'   variance-component estimates are set to zero. Default
#'   \code{FALSE}.
#'
#' @return An object of class \code{"g1f_components"}: a list with
#'   \describe{
#'     \item{\code{A}}{Integer; number of persons.}
#'     \item{\code{I_observed}}{Integer; number of items.}
#'     \item{\code{X}}{Numeric A x I matrix of item scores.}
#'     \item{\code{person_mean}}{Numeric vector of length A; mean
#'       score per person.}
#'     \item{\code{item_mean}}{Numeric vector of length I; mean
#'       score per item.}
#'     \item{\code{grand_mean}}{Scalar; mean of all scores.}
#'     \item{\code{residual_matrix}}{A x I matrix of person-by-item
#'       residuals after removing the person and item main effects.}
#'     \item{\code{anova}}{Data frame with the ANOVA table
#'       (\code{source}, \code{df}, \code{SS}, \code{MS}).}
#'     \item{\code{variance_components}}{Named numeric vector with
#'       elements \code{person}, \code{item}, and \code{resid}
#'       (the p x i interaction component).}
#'   }
#'
#' @references
#' Brennan, R. L. (2001). \emph{Generalizability Theory}.
#' Springer-Verlag.
#'
#' @seealso \code{\link{csem_g1f}}, the main user-facing function
#'   that builds on these components.
#'
#' @examples
#' set.seed(1)
#' dat <- as.data.frame(matrix(rnorm(50 * 8), nrow = 50))
#' g1f_components(dat)$variance_components
#'
#' @export
g1f_components <- function(dat, truncate_vc = FALSE) {

  if (is.data.frame(dat)) {
    if (!all(vapply(dat, is.numeric, logical(1)))) {
      stop("All dataframe columns must be numeric item scores.")
    }
  }

  X <- as.matrix(dat)
  storage.mode(X) <- "double"

  if (!all(is.finite(X))) {
    stop("Complete, finite, balanced data required.")
  }

  A <- nrow(X)
  I <- ncol(X)

  if (A < 2L || I < 2L) {
    stop("At least 2 persons and 2 items are required.")
  }

  person_mean <- rowMeans(X)
  item_mean   <- colMeans(X)
  grand_mean  <- mean(X)

  SS_person <- I * sum((person_mean - grand_mean)^2)
  SS_item   <- A * sum((item_mean   - grand_mean)^2)

  residual_matrix <- X
  residual_matrix <- sweep(residual_matrix, 1L, person_mean, "-")
  residual_matrix <- sweep(residual_matrix, 2L, item_mean,   "-")
  residual_matrix <- residual_matrix + grand_mean
  SS_residual <- sum(residual_matrix^2)

  df_person   <- A - 1L
  df_item     <- I - 1L
  df_residual <- (A - 1L) * (I - 1L)

  MS_person   <- SS_person   / df_person
  MS_item     <- SS_item     / df_item
  MS_residual <- SS_residual / df_residual

  sigma2_person <- (MS_person   - MS_residual) / I
  sigma2_item   <- (MS_item     - MS_residual) / A
  sigma2_resid  <- MS_residual

  variance_components <- c(
    person = sigma2_person,
    item   = sigma2_item,
    resid  = sigma2_resid
  )

  if (truncate_vc) variance_components <- pmax(variance_components, 0)

  anova_table <- data.frame(
    source = c("person", "item", "person:item"),
    df     = c(df_person, df_item, df_residual),
    SS     = c(SS_person, SS_item, SS_residual),
    MS     = c(MS_person, MS_item, MS_residual),
    row.names = NULL
  )

  out <- list(
    A = A, I_observed = I, X = X,
    person_mean = person_mean, item_mean = item_mean,
    grand_mean = grand_mean, residual_matrix = residual_matrix,
    anova = anova_table, variance_components = variance_components
  )

  class(out) <- "g1f_components"
  out
}


###############################################################
# Compute the three relative error-variance estimators
# from the building blocks. Each is a vector of length A.
###############################################################

#' Compute the three per-person relative-error estimators
#'
#' Internal helper that constructs the \code{full}, \code{large_a},
#' and \code{uncorrelated} per-person estimators from the building
#' blocks.
#'
#' @param abs_error_var Numeric vector of length A.
#' @param cov_x_itemmean Numeric vector of length A.
#' @param sigma2_item Scalar; ANOVA item variance component.
#' @param n_items_D Positive scalar; number of items in the D-study.
#' @param A Integer; number of persons.
#'
#' @return A list with elements \code{full}, \code{large_a}, and
#'   \code{uncorrelated}, each a numeric vector of length A.
#'
#' @keywords internal
#' @noRd
compute_relative_estimators <- function(abs_error_var,
                                        cov_x_itemmean,
                                        sigma2_item, n_items_D, A) {
  list(
    full = ((A + 1L) / (A - 1L)) * abs_error_var +
           sigma2_item / n_items_D -
           (A / (A - 1L)) * (2 * cov_x_itemmean / n_items_D),

    large_a = abs_error_var +
              sigma2_item / n_items_D -
              2 * cov_x_itemmean / n_items_D,

    uncorrelated = abs_error_var - sigma2_item / n_items_D
  )
}


###############################################################
# Analytical variance of per-person estimators.
#
# Returns a list with $abs and $rel (named full / large_a /
# uncorrelated). Each element is a vector of length A.
#
# Derivation:
#   Var[abs_p]          = [m4_p - (I-3)/(I-1)(s_p^2)^2] / (I n_i'^2)
#                         (sample-variance moment-4 formula;
#                          Casella & Berger 2002, Thm 5.2.4)
#   Var[uncorrelated_p] ~= Var[abs_p]
#                         (sigma^2(i) treated as fixed, large A)
#   Var[full_p]         ~= 2 (full_p)^2     / (I-1)
#   Var[large_a_p]      ~= 2 (large_a_p)^2  / (I-1)
#                         (chi-square approximation; relative
#                          error variance ~ per-person residual
#                          variance / n_i')
###############################################################

#' Analytical variance of per-person error-variance estimators
#'
#' Internal helper. When called with the optional arguments
#' \code{b_vec}, \code{sigma2_pi}, and \code{A}, returns the exact
#' closed-form variances (constant across persons under Gaussian
#' residuals); otherwise falls back to the legacy chi-square /
#' moment-4 approximation.
#'
#' @param person_centered Numeric A x I matrix of person-centered
#'   item scores.
#' @param n_items_D Positive scalar; number of items in the D-study.
#' @param abs_ev Numeric vector of length A; per-person absolute
#'   error variance.
#' @param rel_ev List with \code{full}, \code{large_a},
#'   \code{uncorrelated}; per-person relative-error estimators.
#' @param b_vec Optional numeric vector of length I; centered item
#'   means (\code{item_mean - grand_mean}).
#' @param sigma2_pi Optional scalar; ANOVA residual variance
#'   component.
#' @param A Optional integer; number of persons.
#'
#' @return A list with elements \code{abs} and \code{rel} (the
#'   latter a list with \code{full}, \code{large_a},
#'   \code{uncorrelated}); each numeric vector of length A.
#'
#' @keywords internal
#' @noRd
analytic_var_estimators <- function(person_centered, n_items_D,
                                    abs_ev, rel_ev,
                                    b_vec = NULL, sigma2_pi = NULL,
                                    A = NULL) {
  I <- ncol(person_centered)

  if (is.null(b_vec) || is.null(sigma2_pi) || is.null(A)) {

    # ---- Backward-compatible chi-square approximation ----
    sp2 <- rowSums(person_centered^2) / (I - 1L)
    m4  <- rowSums(person_centered^4) / I

    var_sp2_vec <- (m4 - ((I - 3L) / (I - 1L)) * sp2^2) / I
    var_sp2_vec <- pmax(var_sp2_vec, 0)

    v_abs <- var_sp2_vec / n_items_D^2

    rel_pos_full         <- pmax(rel_ev$full,         0)
    rel_pos_large_a      <- pmax(rel_ev$large_a,      0)
    rel_pos_uncorrelated <- pmax(rel_ev$uncorrelated, 0)

    return(list(
      abs = v_abs,
      rel = list(
        full         = 2 * rel_pos_full^2         / (I - 1L),
        large_a      = 2 * rel_pos_large_a^2      / (I - 1L),
        uncorrelated = v_abs
      )
    ))
  }

  # ---- Exact analytical formulas (conditional on items,
  #      Gaussian residuals). Constant across persons. ----
  S_b <- sum(b_vec^2)
  D   <- n_items_D
  Im1 <- I - 1L

  var_sp2  <- 4 * sigma2_pi * S_b / Im1^2 + 2 * sigma2_pi^2 / Im1
  var_cp   <-     sigma2_pi * S_b / Im1^2
  cov_spcp <- 2 * sigma2_pi * S_b / Im1^2

  # Each estimator is alpha * sp2 + gamma * cp + C.
  # Var = alpha^2 var_sp2 + gamma^2 var_cp + 2 alpha gamma cov_spcp.
  n <- nrow(person_centered)

  v_abs <- var_sp2 / D^2

  alpha_la <- 1 / D
  gamma_la <- -2 / D
  v_la <- alpha_la^2 * var_sp2 + gamma_la^2 * var_cp +
          2 * alpha_la * gamma_la * cov_spcp

  alpha_full <- (A + 1L) / ((A - 1L) * D)
  gamma_full <- -2 * A / ((A - 1L) * D)
  v_full <- alpha_full^2 * var_sp2 + gamma_full^2 * var_cp +
            2 * alpha_full * gamma_full * cov_spcp

  v_unc <- v_abs

  v_abs  <- max(v_abs,  0)
  v_la   <- max(v_la,   0)
  v_full <- max(v_full, 0)
  v_unc  <- max(v_unc,  0)

  list(
    abs = rep(v_abs, n),
    rel = list(
      full         = rep(v_full, n),
      large_a      = rep(v_la,   n),
      uncorrelated = rep(v_unc,  n)
    )
  )
}


###############################################################
# Item-resampling bootstrap. Returns a list with $abs and $rel
# (named full / large_a / uncorrelated), each vector of length A.
###############################################################

#' Item-resampling bootstrap for per-person error variances
#'
#' Computes nonparametric bootstrap estimates of the sampling
#' variance of the absolute and three relative per-person
#' error-variance estimators by resampling item indices with
#' replacement.
#'
#' @param X Numeric matrix of item scores, persons in rows and
#'   items in columns.
#' @param item_means Numeric vector of column (item) means of
#'   \code{X}.
#' @param sigma2_item Scalar; ANOVA item variance component.
#' @param n_items_D Positive scalar; number of items in the
#'   D-study (use \code{ncol(X)} for no extrapolation).
#' @param B Integer; number of bootstrap replications per person.
#'   Default \code{1000L}.
#' @param seed Optional integer seed for reproducibility. If
#'   \code{NULL}, the current RNG state is used.
#' @param return_replicates Logical. If \code{TRUE}, the full
#'   A x B x 4 array of bootstrap replicates is also returned.
#'   Default \code{FALSE}.
#'
#' @return A list with
#'   \describe{
#'     \item{\code{abs}}{Numeric vector of length A; bootstrap
#'       variance of the per-person absolute error variance.}
#'     \item{\code{rel}}{Named list with \code{full},
#'       \code{large_a}, and \code{uncorrelated}, each a numeric
#'       vector of length A.}
#'     \item{\code{replicates}}{A x B x 4 array of replicates if
#'       \code{return_replicates = TRUE}; \code{NULL} otherwise.
#'       The third dimension is named \code{abs}, \code{full},
#'       \code{large_a}, \code{uncorrelated}.}
#'     \item{\code{B}}{Number of replications used.}
#'   }
#'
#' @references
#' Brennan, R. L. (1998). Raw-score conditional standard errors of
#' measurement in Generalizability Theory. \emph{Applied
#' Psychological Measurement}, \emph{22}(4), 307-331.
#' \doi{10.1177/014662169802200402}
#'
#' @seealso \code{\link{csem_g1f}}, which calls this function when
#'   \code{se_method} is \code{"bootstrap"} or \code{"both"}.
#'
#' @examples
#' set.seed(1)
#' X <- matrix(rnorm(40 * 6), nrow = 40)
#' bs <- bootstrap_csem_g1f(
#'   X = X, item_means = colMeans(X),
#'   sigma2_item = 0, n_items_D = 6,
#'   B = 200, seed = 42
#' )
#' head(bs$abs)
#'
#' @export
bootstrap_csem_g1f <- function(X, item_means, sigma2_item,
                               n_items_D, B = 1000L, seed = NULL,
                               return_replicates = FALSE) {
  if (!is.null(seed)) set.seed(seed)

  A <- nrow(X); I <- ncol(X)
  grand <- mean(X)
  b_full <- item_means - grand

  v_abs           <- numeric(A)
  v_full          <- numeric(A)
  v_large_a       <- numeric(A)
  v_uncorrelated  <- numeric(A)

  reps_array <- if (return_replicates) {
    array(NA_real_, dim = c(A, B, 4L),
          dimnames = list(NULL, NULL,
                          c("abs", "full",
                            "large_a", "uncorrelated")))
  } else NULL

  for (a in seq_len(A)) {
    xrow <- X[a, ]
    idx_mat <- matrix(sample.int(I, B * I, replace = TRUE), B, I)

    Xb <- matrix(xrow[idx_mat],   B, I)
    bb <- matrix(b_full[idx_mat], B, I)

    pm_b   <- rowMeans(Xb)
    a_b    <- Xb - pm_b
    rowss  <- rowSums(a_b^2)
    rowvar <- rowss / (I - 1L)
    abs_ev <- rowvar / n_items_D
    cov_b  <- rowSums(a_b * bb) / (I - 1L)

    full_b <- ((A + 1L) / (A - 1L)) * abs_ev +
              sigma2_item / n_items_D -
              (A / (A - 1L)) * (2 * cov_b / n_items_D)
    larg_b <- abs_ev + sigma2_item / n_items_D - 2 * cov_b / n_items_D
    unco_b <- abs_ev - sigma2_item / n_items_D

    v_abs[a]          <- var(abs_ev)
    v_full[a]         <- var(full_b)
    v_large_a[a]      <- var(larg_b)
    v_uncorrelated[a] <- var(unco_b)

    if (return_replicates) {
      reps_array[a, , "abs"]          <- abs_ev
      reps_array[a, , "full"]         <- full_b
      reps_array[a, , "large_a"]      <- larg_b
      reps_array[a, , "uncorrelated"] <- unco_b
    }
  }

  list(abs = v_abs,
       rel = list(full         = v_full,
                  large_a      = v_large_a,
                  uncorrelated = v_uncorrelated),
       replicates = reps_array, B = B)
}


###############################################################
# Smoothing: quadratic regression of per-person estimator on
# observed score (Brennan 2001, p. 162). OLS guarantees that
# mean(fitted) = mean(observed).
#
# Returns a list with:
#   $fitted : numeric vector of fitted values
#   $coefs  : named numeric of c(b0, b1, b2)
#   $R2, $RMSE, $n : scalar diagnostics
###############################################################

#' Quadratic smoother of a per-person quantity on the observed score
#'
#' Fits a quadratic regression of \code{y} on \code{x} by ordinary
#' least squares (so that the mean of the fitted values equals the
#' mean of \code{y}) and returns the fitted values together with
#' standard fit diagnostics.
#'
#' @param y Numeric vector of values to be smoothed.
#' @param x Numeric vector of the same length as \code{y};
#'   typically the observed score.
#' @param fit_subset Optional logical vector of \code{length(y)}
#'   indicating which rows to use in the fit. Rows where
#'   \code{fit_subset} is \code{FALSE} contribute neither to the
#'   regression nor to the fitted values (which are returned as
#'   \code{NA}). Default \code{NULL} (use all rows).
#'
#' @return A list with
#'   \describe{
#'     \item{\code{fitted}}{Numeric vector of length \code{length(y)};
#'       fitted values, with \code{NA} for rows excluded from the
#'       fit.}
#'     \item{\code{coefs}}{Named numeric vector of regression
#'       coefficients (\code{b0}, \code{b1}, \code{b2}).}
#'     \item{\code{R2}}{Coefficient of determination on the fit
#'       sample.}
#'     \item{\code{RMSE}}{Root mean squared residual on the fit
#'       sample, computed as \code{sqrt(SSE / n)} (population-style,
#'       not the residual-df version returned by \code{lm}).}
#'     \item{\code{n}}{Sample size used in the fit.}
#'   }
#'
#' @references
#' Brennan, R. L. (2001). \emph{Generalizability Theory}.
#' Springer-Verlag.
#'
#' @seealso \code{\link{csem_g1f}} called with \code{smooth = TRUE}
#'   applies this smoother to each per-person error variance.
#'
#' @examples
#' set.seed(1)
#' x <- runif(60, 0, 10)
#' y <- 2 + 0.3 * x - 0.05 * x^2 + rnorm(60, sd = 0.4)
#' fit <- quadratic_smooth(y, x)
#' fit$coefs
#'
#' @export
quadratic_smooth <- function(y, x, fit_subset = NULL) {
  # fit_subset: logical vector of length(y). If non-NULL, the
  # regression is fit on the rows where TRUE; fitted values for
  # the remaining rows are returned as NA. Used by csem_g1f when
  # `exclude_extremes = TRUE` to drop floor/ceiling cases from
  # the fit while keeping the original row alignment.
  if (is.null(fit_subset)) fit_subset <- rep(TRUE, length(y))

  yf <- y[fit_subset]
  xf <- x[fit_subset]

  fit <- lm(yf ~ xf + I(xf^2))
  cf  <- unname(coef(fit))
  res <- residuals(fit)
  ss_res <- sum(res^2)
  ss_tot <- sum((yf - mean(yf))^2)
  r2     <- if (ss_tot > 0) 1 - ss_res / ss_tot else NA_real_
  rmse   <- sqrt(mean(res^2))

  fitted_full <- rep(NA_real_, length(y))
  fitted_full[fit_subset] <- cf[1] + cf[2] * xf + cf[3] * xf^2

  list(fitted = fitted_full,
       coefs  = c(b0 = cf[1], b1 = cf[2], b2 = cf[3]),
       R2     = r2,
       RMSE   = rmse,
       n      = length(yf))
}


###############################################################
# Main user-facing function
###############################################################

#' Conditional standard errors of measurement in Generalizability Theory
#'
#' Estimates per-person conditional standard errors of measurement
#' (CSEMs) for a univariate, single-facet, persons-by-items
#' (p x i) crossed Generalizability Theory design, together with
#' generalizability and dependability coefficients.
#'
#' Whereas the overall (population) SEM summarizes measurement
#' precision averaged across the population, the CSEM characterizes
#' the precision of the measurement for an individual at a given
#' score level (Brennan, 1998). Three estimators of the per-person
#' relative error variance are available, corresponding to
#' alternative simplifying assumptions about the population:
#' \code{"full"} (Brennan, 1998, eqs. 35-36; no simplifying
#' assumption), \code{"large_a"} (eq. 40; assumes the number of
#' persons grows large), and \code{"uncorrelated"} (eq. 41;
#' additionally assumes that the within-person item-by-residual
#' covariance is zero). The absolute error variance has a single
#' estimator (eq. 20). Standard errors of the per-person estimators
#' are returned analytically (closed-form, conditional on items,
#' under Gaussianity of the residuals) or via an item-resampling
#' bootstrap.
#'
#' @param dat A numeric data frame or matrix of item scores, with
#'   persons in rows and items in columns. Must be balanced and
#'   complete (no missing values).
#' @param n_items_D Positive scalar; number of items in the
#'   D-study. Defaults to \code{ncol(dat)} (no extrapolation).
#' @param method Character string selecting the relative-error
#'   estimator: one of \code{"full"} (default), \code{"large_a"},
#'   \code{"uncorrelated"}, or \code{"all"} (computes the three
#'   jointly).
#' @param se_method Character string selecting the SE method for
#'   the per-person estimators: one of \code{"analytical"}
#'   (default), \code{"bootstrap"}, or \code{"both"}.
#' @param truncate_vc Logical. If \code{TRUE}, negative ANOVA
#'   variance components are truncated at zero. Default
#'   \code{FALSE}.
#' @param truncate_negative_error_var Logical. If \code{TRUE},
#'   negative per-person error variances are truncated at zero
#'   before taking the square root, so that the corresponding CSEM
#'   is 0 rather than \code{NaN}. Default \code{FALSE}.
#' @param smooth Logical. If \code{TRUE}, fits a quadratic
#'   regression of each per-person error variance on the observed
#'   score (Brennan, 2001, p. 162) and returns the smoothed values.
#'   Default \code{FALSE}.
#' @param exclude_extremes Logical. If \code{TRUE} (and
#'   \code{smooth = TRUE}), excludes from the quadratic fit those
#'   persons whose responses are all at the empirical minimum
#'   (floor) or maximum (ceiling) across \code{dat}. Smoothed
#'   values for the excluded cases are returned as \code{NA}.
#'   Default \code{FALSE}.
#' @param cutpoint Optional finite numeric scalar. If supplied,
#'   computes the dependability coefficient for mastery
#'   classifications, Phi(lambda), at the given cutpoint
#'   (Brennan & Kane, 1977; Brennan, 2001, eq. 2.55).
#' @param boot_B Integer; number of bootstrap replications per
#'   person when \code{se_method} requests the bootstrap. Default
#'   \code{1000L}.
#' @param boot_seed Optional integer seed for the bootstrap.
#' @param boot_keep_replicates Logical. If \code{TRUE}, the full
#'   A x B x 4 array of bootstrap replicates is retained in the
#'   \code{bootstrap} component of the return value. Default
#'   \code{FALSE}.
#'
#' @return An object of class \code{"g1f_csem"}: a list with the
#'   following components.
#'   \describe{
#'     \item{\code{call}}{The matched call.}
#'     \item{\code{A}, \code{I_observed}, \code{n_items_D}}{Sample
#'       sizes: persons, observed items, D-study items.}
#'     \item{\code{method}, \code{se_method}, \code{smooth},
#'       \code{exclude_extremes}, \code{cutpoint}}{Echo of the
#'       choices made.}
#'     \item{\code{n_floor}, \code{n_ceiling}, \code{n_fit}}{Counts
#'       of floor and ceiling cases excluded from the smoothing
#'       fit and the resulting fit sample size; \code{NA} when
#'       \code{exclude_extremes = FALSE}.}
#'     \item{\code{anova}}{ANOVA table (\code{source}, \code{df},
#'       \code{SS}, \code{MS}).}
#'     \item{\code{variance_components}}{Named numeric vector with
#'       the \code{person}, \code{item}, and \code{resid} (p x i)
#'       components.}
#'     \item{\code{overall}}{Data frame with the population-level
#'       absolute and relative error variances and SEMs
#'       (\code{absolute_error_var}, \code{absolute_sem},
#'       \code{relative_error_var}, \code{relative_sem}).}
#'     \item{\code{coefficients}}{Data frame with rows
#'       \code{erho2} (Brennan, 2001, eq. 2.40), \code{phi}
#'       (eq. 2.41), and, when \code{cutpoint} is supplied,
#'       \code{phi_lambda} (eq. 2.55).}
#'     \item{\code{erho2}, \code{phi}, \code{phi_lambda}}{Scalar
#'       copies of the same coefficients.}
#'     \item{\code{checks}}{List of sanity checks comparing the
#'       mean per-person error variances against the
#'       population-level quantities.}
#'     \item{\code{person}}{Data frame with one row per person
#'       containing \code{id}, \code{observed_score},
#'       \code{abs_error_var}, \code{abs_csem},
#'       \code{cov_x_itemmean}, \code{var_abs_ev_analytical},
#'       \code{var_abs_ev_boot}, the matching relative-error
#'       columns (\code{rel_error_var}, \code{rel_csem},
#'       \code{var_rel_ev_analytical}, \code{var_rel_ev_boot})
#'       and, when \code{smooth = TRUE}, the corresponding
#'       \code{*_sm} columns. When \code{method = "all"} the
#'       relative-error columns are duplicated with suffixes
#'       \code{_full}, \code{_large_a}, and \code{_uncorrelated}
#'       so that the three estimators can be compared.}
#'     \item{\code{bootstrap}}{Output of
#'       \code{\link{bootstrap_csem_g1f}} when \code{se_method}
#'       requests it; \code{NULL} otherwise.}
#'     \item{\code{smooth_fits}}{Data frame of smoothing-fit
#'       diagnostics (\code{quantity}, \code{b0}, \code{b1},
#'       \code{b2}, \code{R2}, \code{RMSE}, \code{n}); \code{NULL}
#'       when \code{smooth = FALSE}.}
#'   }
#'
#' @references
#' Brennan, R. L. (1998). Raw-score conditional standard errors of
#' measurement in Generalizability Theory. \emph{Applied
#' Psychological Measurement}, \emph{22}(4), 307-331.
#' \doi{10.1177/014662169802200402}
#'
#' Brennan, R. L. (2001). \emph{Generalizability Theory}.
#' Springer-Verlag.
#'
#' Brennan, R. L., & Kane, M. T. (1977). An index of dependability
#' for mastery tests. \emph{Journal of Educational Measurement},
#' \emph{14}(3), 277-289.
#'
#' Cronbach, L. J., Gleser, G. C., Nanda, H., & Rajaratnam, N.
#' (1972). \emph{The dependability of behavioral measurements: Theory of generalizability for scores and profiles}.
#' Wiley.
#'
#' @seealso \code{\link{g1f_components}} for the underlying
#'   variance-component estimation; \code{\link{bootstrap_csem_g1f}}
#'   for the item-resampling bootstrap;
#'   \code{\link{quadratic_smooth}} for the smoother used by
#'   \code{smooth = TRUE}.
#'
#' @examples
#' set.seed(123)
#' n_persons <- 80
#' n_items   <- 10
#' tau   <- rnorm(n_persons, mean = 5, sd = 1.5)
#' items <- as.data.frame(
#'   sapply(seq_len(n_items),
#'          function(j) tau + rnorm(n_persons, sd = 1))
#' )
#' fit <- csem_g1f(items, method = "full", smooth = TRUE)
#' fit$overall
#' fit$coefficients
#' head(fit$person[, c("observed_score", "rel_csem", "rel_csem_sm")])
#'
#' @export
csem_g1f <- function(dat,
                     n_items_D = NULL,
                     method    = c("full", "large_a",
                                   "uncorrelated", "all"),
                     se_method = c("analytical", "bootstrap",
                                   "both"),
                     truncate_vc = FALSE,
                     truncate_negative_error_var = FALSE,
                     smooth = FALSE,
                     exclude_extremes = FALSE,
                     cutpoint = NULL,
                     boot_B = 1000L,
                     boot_seed = NULL,
                     boot_keep_replicates = FALSE) {

  method    <- match.arg(method)
  se_method <- match.arg(se_method)

  if (!is.null(cutpoint)) {
    if (!is.numeric(cutpoint) || length(cutpoint) != 1L ||
        !is.finite(cutpoint))
      stop("cutpoint must be a single finite numeric value or NULL.")
  }

  if (exclude_extremes && !smooth) {
    stop("exclude_extremes requires smooth = TRUE.")
  }

  comp <- g1f_components(dat, truncate_vc = truncate_vc)

  X          <- comp$X
  A          <- comp$A
  I_observed <- comp$I_observed

  if (is.null(n_items_D)) n_items_D <- I_observed
  if (!is.numeric(n_items_D) || length(n_items_D) != 1L ||
      !is.finite(n_items_D) || n_items_D <= 0)
    stop("n_items_D must be a positive scalar.")

  person_mean  <- comp$person_mean
  item_mean    <- comp$item_mean
  grand_mean   <- comp$grand_mean
  sigma2_item  <- unname(comp$variance_components["item"])
  sigma2_resid <- unname(comp$variance_components["resid"])

  sem_sqrt <- function(v) {
    if (truncate_negative_error_var) {
      sqrt(pmax(v, 0))
    } else {
      out <- rep(NaN, length(v))
      ok  <- !is.na(v) & v >= 0
      out[ok] <- sqrt(v[ok])
      out[is.na(v)] <- NA_real_
      out
    }
  }

  # ---- ingredients ----
  person_centered <- sweep(X, 1L, person_mean, "-")
  b_vec           <- item_mean - grand_mean
  row_var         <- rowSums(person_centered^2) / (I_observed - 1L)
  cov_x_itemmean  <- as.vector(person_centered %*% b_vec) /
                     (I_observed - 1L)

  # ---- absolute and three relative estimators ----
  abs_error_var <- row_var / n_items_D
  abs_csem      <- sem_sqrt(abs_error_var)

  rel_ev <- compute_relative_estimators(abs_error_var,
                                        cov_x_itemmean,
                                        sigma2_item,
                                        n_items_D, A)

  # ---- analytical and/or bootstrap variance of estimators ----
  if (se_method %in% c("analytical", "both")) {
    av <- analytic_var_estimators(person_centered, n_items_D,
                                  abs_error_var, rel_ev,
                                  b_vec = b_vec,
                                  sigma2_pi = sigma2_resid,
                                  A = A)
  } else {
    av <- list(abs = rep(NA_real_, A),
               rel = list(full         = rep(NA_real_, A),
                          large_a      = rep(NA_real_, A),
                          uncorrelated = rep(NA_real_, A)))
  }

  boot_result <- NULL
  if (se_method %in% c("bootstrap", "both")) {
    boot_result <- bootstrap_csem_g1f(
      X = X, item_means = item_mean,
      sigma2_item = sigma2_item, n_items_D = n_items_D,
      B = boot_B, seed = boot_seed,
      return_replicates = boot_keep_replicates
    )
  } else {
    boot_result <- list(abs = rep(NA_real_, A),
                        rel = list(full         = rep(NA_real_, A),
                                   large_a      = rep(NA_real_, A),
                                   uncorrelated = rep(NA_real_, A)),
                        replicates = NULL, B = NA_integer_)
  }

  # ---- person-level results ----
  person_results <- data.frame(
    id = if (is.null(rownames(X))) seq_len(A) else rownames(X),
    observed_score = person_mean,
    abs_error_var  = abs_error_var,
    abs_csem       = abs_csem,
    cov_x_itemmean = cov_x_itemmean,
    var_abs_ev_analytical = av$abs,
    var_abs_ev_boot       = boot_result$abs,
    row.names = NULL
  )

  # Floor/ceiling detection for the smoothing fit sample.
  # Floor   = all items at the empirical minimum across `dat`.
  # Ceiling = all items at the empirical maximum across `dat`.
  # When `exclude_extremes = TRUE` these cases are dropped from
  # the quadratic regression and their smoothed values are set to
  # NA. Without `exclude_extremes`, fit_subset = NULL keeps the
  # original behaviour (fit on all rows).
  fit_subset <- NULL
  n_floor    <- 0L
  n_ceiling  <- 0L
  n_fit      <- A
  if (smooth && exclude_extremes) {
    gmin <- min(X)
    gmax <- max(X)
    rmin <- apply(X, 1L, min)
    rmax <- apply(X, 1L, max)
    is_floor   <- (rmin == gmin) & (rmax == gmin)
    is_ceiling <- (rmin == gmax) & (rmax == gmax)
    fit_subset <- !(is_floor | is_ceiling)
    n_floor    <- as.integer(sum(is_floor))
    n_ceiling  <- as.integer(sum(is_ceiling))
    n_fit      <- as.integer(sum(fit_subset))
    if (n_fit < 5L) {
      stop(sprintf(
        "exclude_extremes leaves n_fit = %d (< 5). Too few non-extreme cases to fit a quadratic.",
        n_fit))
    }
  }

  # Container for per-quantity smoothing diagnostics
  smooth_fits_list <- list()

  smooth_one <- function(name, y) {
    fit <- quadratic_smooth(y, person_mean, fit_subset = fit_subset)
    smooth_fits_list[[name]] <<- data.frame(
      quantity = name,
      b0 = unname(fit$coefs["b0"]),
      b1 = unname(fit$coefs["b1"]),
      b2 = unname(fit$coefs["b2"]),
      R2 = fit$R2,
      RMSE = fit$RMSE,
      n = fit$n,
      stringsAsFactors = FALSE,
      row.names = NULL
    )
    fit$fitted
  }

  # Smoothed absolute (always available if smooth = TRUE,
  # regardless of method)
  if (smooth) {
    person_results$abs_error_var_sm <- smooth_one("abs_ev",
                                                   abs_error_var)
    person_results$abs_csem_sm <-
      sem_sqrt(person_results$abs_error_var_sm)
  }

  add_relative_columns <- function(df, suffix, m) {
    rev <- rel_ev[[m]]
    df[[paste0("rel_error_var", suffix)]] <- rev
    df[[paste0("rel_csem",      suffix)]] <- sem_sqrt(rev)
    df[[paste0("var_rel_ev_analytical", suffix)]] <- av$rel[[m]]
    df[[paste0("var_rel_ev_boot",       suffix)]] <- boot_result$rel[[m]]
    if (smooth) {
      qname <- if (suffix == "") "rel_ev"
               else paste0("rel_ev", suffix)
      rev_sm <- smooth_one(qname, rev)
      df[[paste0("rel_error_var_sm", suffix)]] <- rev_sm
      df[[paste0("rel_csem_sm",      suffix)]] <- sem_sqrt(rev_sm)
    }
    df
  }

  if (method == "all") {
    person_results <- add_relative_columns(person_results,
                                           "_full",         "full")
    person_results <- add_relative_columns(person_results,
                                           "_large_a",      "large_a")
    person_results <- add_relative_columns(person_results,
                                           "_uncorrelated", "uncorrelated")
    # Default visible 'rel_*' columns point to the principal method (full)
    person_results$rel_error_var <- rel_ev$full
    person_results$rel_csem      <- sem_sqrt(rel_ev$full)
    person_results$var_rel_ev_analytical <- av$rel$full
    person_results$var_rel_ev_boot       <- boot_result$rel$full
    if (smooth) {
      person_results$rel_error_var_sm <-
        person_results$rel_error_var_sm_full
      person_results$rel_csem_sm <-
        person_results$rel_csem_sm_full
    }
  } else {
    person_results <- add_relative_columns(person_results, "", method)
  }

  # ---- overall (population-level) error variances ----
  abs_ev_overall <- (sigma2_item + sigma2_resid) / n_items_D
  rel_ev_overall <- sigma2_resid / n_items_D
  
  overall <- data.frame(
    quantity = c("absolute_error_var", "absolute_sem",
                 "relative_error_var", "relative_sem"),
    estimate = c(
      abs_ev_overall,
      sqrt(abs_ev_overall),
      rel_ev_overall,
      sqrt(rel_ev_overall)
    ),
    row.names = NULL
  )

  # ---- reliability-like coefficients ----
  # Brennan (2001) eqs. 2.40 (G), 2.41 (Phi), 2.55 (Phi(lambda))
  sigma2_p <- unname(comp$variance_components["person"])
  
  erho2 <- if (sigma2_p + rel_ev_overall > 0)
              sigma2_p / (sigma2_p + rel_ev_overall) else NA_real_
  phi   <- if (sigma2_p + abs_ev_overall > 0)
              sigma2_p / (sigma2_p + abs_ev_overall) else NA_real_

  coef_rows <- data.frame(
    coefficient = c("erho2", "phi"),
    estimate    = c(erho2, phi),
    stringsAsFactors = FALSE,
    row.names   = NULL
  )

  # Phi(lambda) (Brennan & Kane 1977; Brennan 2001 eq. 2.55):
  #   Phi(lambda) = [s2(p) + (Xbar - lambda)^2 - s2(Xbar)] /
  #                 [s2(p) + (Xbar - lambda)^2 - s2(Xbar) + s2(Delta)]
  # where s2(Xbar) = [s2(p) + s2(I) + s2(pI)] / n_p   (eq. 2.38)
  # and s2(I) = s2(i)/D, s2(pI) = s2(pi)/D, s2(Delta) = s2(I) + s2(pI).

  phi_lambda <- NA_real_
  if (!is.null(cutpoint)) {
    s2_Xbar <- (sigma2_p + sigma2_item / n_items_D +
                sigma2_resid / n_items_D) / A
    dev2    <- (grand_mean - cutpoint)^2
    num     <- sigma2_p + dev2 - s2_Xbar
    den     <- num + abs_ev_overall
    phi_lambda <- if (is.finite(den) && den > 0) num / den else NA_real_

    coef_rows <- rbind(
      coef_rows,
      data.frame(coefficient = "phi_lambda",
                 estimate    = phi_lambda,
                 stringsAsFactors = FALSE,
                 row.names = NULL)
    )
  }
  coefficients <- coef_rows

  # ---- assemble smooth_fits ----
  smooth_fits <- if (smooth && length(smooth_fits_list))
                   do.call(rbind, smooth_fits_list) else NULL
  if (!is.null(smooth_fits)) row.names(smooth_fits) <- NULL

  checks <- list(
    mean_abs_error_var       = mean(person_results$abs_error_var),
    overall_abs_error_var    = overall$estimate[1],
    mean_rel_error_var_full  = mean(rel_ev$full),
    mean_rel_error_var_large_a      = mean(rel_ev$large_a),
    mean_rel_error_var_uncorrelated = mean(rel_ev$uncorrelated),
    overall_rel_error_var    = overall$estimate[3]
  )

  out <- list(
    call                = match.call(),
    A                   = A,
    I_observed          = I_observed,
    n_items_D           = n_items_D,
    method              = method,
    se_method           = se_method,
    smooth              = smooth,
    exclude_extremes    = exclude_extremes,
    n_floor             = if (smooth && exclude_extremes) n_floor   else NA_integer_,
    n_ceiling           = if (smooth && exclude_extremes) n_ceiling else NA_integer_,
    n_fit               = if (smooth && exclude_extremes) n_fit     else NA_integer_,
    cutpoint            = cutpoint,
    anova               = comp$anova,
    variance_components = comp$variance_components,
    overall             = overall,
    coefficients        = coefficients,
    erho2               = erho2,
    phi                 = phi,
    phi_lambda          = phi_lambda,
    checks              = checks,
    person              = person_results,
    bootstrap           = if (se_method %in% c("bootstrap", "both"))
                            boot_result else NULL,
    smooth_fits         = smooth_fits
  )

  class(out) <- "g1f_csem"
  out
}
