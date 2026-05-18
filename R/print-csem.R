# print method for the csem class. The console display replicates the
# layout of the gtcsem Stata command block for block: a header with the
# design summary, the ANOVA table, the D-study population error variances
# and SEMs, the reliability-like coefficients, the quadratic smoothing
# fits, and the mean sampling variance of each estimator across persons.
#
# print.csem only selects and formats. The two display-time computations
# are (i) the sigma^2 column of the ANOVA table, glued from
# variance_components$person/item/residual -- the anova_table data frame
# itself carries only source/df/SS/MS, to stay byte-identical to the
# legacy comp$anova used by the parity tests -- and (ii) the per-estimator
# mean sampling variances, averaged over persons from the estimates table.

# Canonical estimator order and the short labels used by the gtcsem
# console display. Shared by the smoothing-fits and mean-variance blocks
# so both tables list estimators in the same order with the same names.
.csem_estimator_order <- c("absolute", "relative_full",
                           "relative_large_a", "relative_uncorrelated")

.csem_estimator_label <- c(absolute              = "abs_ev",
                           relative_full         = "rel_ev_full",
                           relative_large_a      = "rel_ev_la",
                           relative_uncorrelated = "rel_ev_unc")


# Header label for the "Method" line shared by print.csem() and
# print.summary.csem(). `method` governs only the relative-error
# estimators, so when the fit carries no relative error type that
# argument is irrelevant and reporting "all" (or a method list) there is
# misleading -- the label is "n/a (absolute error only)" instead.
# Otherwise: "all" when every relative method is present, else the
# comma-separated list of methods.
.csem_methods_label <- function(methods, error_types) {
  if (!("relative" %in% error_types)) {
    return("n/a (absolute error only)")
  }
  if (setequal(methods, c("full", "large_a", "uncorrelated"))) {
    return("all")
  }
  paste(methods, collapse = ", ")
}


#' Print a `csem` object
#'
#' Displays a `csem` object as a sequence of console blocks mirroring the
#' output of the `gtcsem` Stata command: a header summarising the design,
#' the ANOVA table, the D-study population error variances and standard
#' errors of measurement, the reliability-like coefficients, the quadratic
#' smoothing fits (when smoothing was applied), and the mean sampling
#' variance of each estimator across persons.
#'
#' @param x A `csem` object.
#' @param ... Currently ignored; present for S3 consistency.
#'
#' @return `x`, invisibly.
#'
#' @seealso [summary.csem()] for the score-level table and global
#'   statistics; [coef.csem()] and [by_score()] for programmatic access to
#'   the underlying components.
#'
#' @examples
#' set.seed(1)
#' d <- matrix(rbinom(80 * 15, 1, 0.5), nrow = 80)
#' fit <- csem_gt(d, cutpoint = 0.5)
#' print(fit)
#'
#' @export
print.csem <- function(x, ...) {

  rule <- function(n = 64L) cat(strrep("-", n), "\n", sep = "")

  vc   <- x$variance_components
  args <- x$arguments

  # -- header -----------------------------------------------------------
  rule()
  cat("Conditional SEMs in Generalizability Theory\n")
  rule()
  cat("Design          :  univariate single-facet (p x i, crossed)\n")
  cat(sprintf("Persons (n_p)   :  %d\n", x$n_persons))
  cat(sprintf("G-study items   :  %d\n", x$n_items))

  n_items_D <- as.integer(args$n_items_D)
  if (n_items_D != x$n_items) {
    cat(sprintf("D-study items   :  %d  (extrapolated; n_i' != n_i)\n",
                n_items_D))
  } else {
    cat(sprintf("D-study items   :  %d\n", n_items_D))
  }

  methods_str <- .csem_methods_label(x$methods, x$error_types)
  cat(sprintf("Method          :  %s\n", methods_str))

  se_method <- if (isTRUE(args$bootstrap) && isTRUE(args$return_analytical)) {
    "both"
  } else if (isTRUE(args$bootstrap)) {
    "bootstrap"
  } else {
    "analytical"
  }
  cat(sprintf("SE method       :  %s\n", se_method))

  if (!identical(args$smoother, "none")) {
    sm_degree <- as.integer(args$smoother_args$degree %||% 2L)
    sm_desc <- if (sm_degree == 2L) {
      "quadratic on observed score"
    } else {
      sprintf("polynomial (degree %d) on observed score", sm_degree)
    }
    cat(sprintf("Smoothing       :  %s\n", sm_desc))
    if (isTRUE(args$exclude_extremes)) {
      sd <- x$diagnostics
      cat(sprintf(
        "Smoothing fit   :  n_fit = %d (excluded %d floor + %d ceiling case(s))\n",
        sd$n_fit, sd$n_floor, sd$n_ceiling))
    }
  }

  if (!is.null(args$cutpoint)) {
    cat(sprintf("Cutpoint        :  %12.6f\n", args$cutpoint))
  }

  # -- ANOVA table ------------------------------------------------------
  cat("ANOVA table\n")
  rule()
  cat("  Effect    df              SS              MS         sigma^2\n")
  rule()
  anova_tbl        <- vc$anova_table
  src_label        <- c(person = "p", item = "i", "person:item" = "pi")
  sigma2_by_source <- c("person"      = vc$person,
                        "item"        = vc$item,
                        "person:item" = vc$residual)
  for (i in seq_len(nrow(anova_tbl))) {
    src <- anova_tbl$source[i]
    cat(sprintf("  %-6s%8.0f  %14.6f  %14.6f  %12.6f\n",
                src_label[[src]],
                anova_tbl$df[i],
                anova_tbl$SS[i],
                anova_tbl$MS[i],
                sigma2_by_source[[src]]))
  }

  # -- D-study population error variances and SEMs ----------------------
  cat(sprintf("D-study error variances and SEMs (n_i' = %d)\n", n_items_D))
  rule()
  pq <- vc$population_quantities
  cat(sprintf(
    "  sigma^2(Delta) = %10.6f      sigma(Delta) = %8.6f  (absolute)\n",
    pq$absolute_error_var, pq$absolute_sem))
  cat(sprintf(
    "  sigma^2(delta) = %10.6f      sigma(delta) = %8.6f  (relative)\n",
    pq$relative_error_var, pq$relative_sem))

  # -- reliability-like coefficients ------------------------------------
  cat("Reliability-like coefficients\n")
  rule()
  rc <- vc$reliability_coefficients
  cat(sprintf("  Generalizability coef.    E rho^2     = %8.4f\n", rc$erho2))
  cat(sprintf("  Dependability coef.       Phi         = %8.4f\n", rc$phi))
  if (!is.na(rc$phi_lambda)) {
    cat(sprintf(
      "  Dep. coef. for cutpoint   Phi(lambda) = %8.4f   (lambda = %6.3f)\n",
      rc$phi_lambda, args$cutpoint))
  }

  # -- quadratic smoothing fits -----------------------------------------
  if (!is.null(x$smooth_fits) && length(x$smooth_fits) > 0L) {
    sf_order <- intersect(.csem_estimator_order, names(x$smooth_fits))
    cat("Quadratic smoothing fits  (y = b0 + b1*score + b2*score^2)\n")
    rule(74L)
    cat("  Quantity              b0         b1         b2        R^2       RMSE\n")
    rule(74L)
    for (est in sf_order) {
      f <- x$smooth_fits[[est]]
      cat(sprintf("  %-19s%10.5f %10.5f %10.5f %10.4f %10.5f\n",
                  .csem_estimator_label[[est]],
                  f$b0, f$b1, f$b2, f$R2, f$RMSE))
    }
  }

  # -- mean sampling variance of each estimator across persons ----------
  scales <- character(0)
  if (any(grepl("^csem_var\\.analytic\\.", names(x$estimates)))) {
    scales <- c(scales, analytic = "Analytical")
  }
  if (any(grepl("^csem_var\\.boot\\.", names(x$estimates)))) {
    scales <- c(scales, boot = "Bootstrap")
  }
  if (length(scales) > 0L) {
    est_present <- intersect(
      .csem_estimator_order,
      sub("^csem\\.", "",
          grep("^csem\\.", names(x$estimates), value = TRUE)))
    cat("Mean variance of estimator across persons\n")
    rule()
    cat(sprintf("  %-14s%s\n",
                "Quantity",
                paste(sprintf("%18s", scales), collapse = "")))
    rule(2L + 14L + 18L * length(scales))
    for (est in est_present) {
      vals <- vapply(names(scales), function(sc) {
        col <- paste0("csem_var.", sc, ".", est)
        if (col %in% names(x$estimates)) {
          mean(x$estimates[[col]], na.rm = TRUE)
        } else {
          NA_real_
        }
      }, numeric(1))
      cat(sprintf("  %-14s%s\n",
                  .csem_estimator_label[[est]],
                  paste(sprintf("%18.6e", vals), collapse = "")))
    }
  }

  invisible(x)
}
