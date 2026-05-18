# summary method for the csem class. summary.csem() collapses a csem
# object to a score-level view: it returns the by_score table augmented
# with cumulative-frequency and percentile columns, together with the
# population-level "global" statistics (the relative and absolute SEMs
# and their companion reliability-like coefficients). print.summary.csem()
# renders a curated console display of that object.
#
# summary.csem() only selects, reshapes and accumulates; every quantity
# it reports is already present in the csem object. .compute_global_stats()
# is the thin extractor that pulls the four global figures out of
# variance_components.


# Short column labels for the csem.* point-estimate columns in the
# curated print.summary.csem() table; keeps the table within roughly
# 80 columns. The full csem.<estimator> names are retained in the
# summary.csem object itself.
.csem_short_labels <- c(
  csem.absolute              = "abs",
  csem.relative_full         = "rel_full",
  csem.relative_large_a      = "rel_la",
  csem.relative_uncorrelated = "rel_unc"
)


#' Extract the population-level global statistics of a `csem` object
#'
#' Pulls the relative and absolute standard errors of measurement and
#' their companion reliability-like coefficients out of the
#' `variance_components` component. Internal helper for [summary.csem()].
#'
#' @param object A `csem` object.
#' @return A named list with `relative_sem`, `erho2`, `absolute_sem` and
#'   `phi`.
#' @keywords internal
.compute_global_stats <- function(object) {
  pq <- object$variance_components$population_quantities
  rc <- object$variance_components$reliability_coefficients
  list(
    relative_sem = pq$relative_sem,
    erho2        = rc$erho2,
    absolute_sem = pq$absolute_sem,
    phi          = rc$phi
  )
}


#' Summarise a `csem` object
#'
#' Produces a score-level summary of a `csem` object. The returned object
#' carries the full `by_score` table augmented with two columns,
#' `cum_freq` (the cumulative proportion of persons at or below each
#' observed score) and `percentile` (`100 * cum_freq`), together with the
#' population-level global statistics: the relative and absolute standard
#' errors of measurement and their companion reliability-like
#' coefficients (the generalizability coefficient `E rho^2` and the
#' dependability coefficient `Phi`).
#'
#' The returned object retains every column of the original `by_score`
#' table; [print.summary.csem()] displays a curated subset.
#'
#' @param object A `csem` object.
#' @param ... Currently ignored; present for S3 consistency.
#'
#' @return An object of class `summary.csem`: a list with components
#'   `call`, `paradigm`, `methods`, `error_types`, `n_persons`,
#'   `n_items`, `global_stats` (a list with `relative_sem`, `erho2`,
#'   `absolute_sem`, `phi`) and `by_score_summary` (the augmented
#'   `by_score` table).
#'
#' @seealso [print.csem()] for the full-object display; [by_score()] and
#'   [coef.csem()] for programmatic access to the underlying components.
#'
#' @examples
#' set.seed(1)
#' d <- matrix(rbinom(80 * 15, 1, 0.5), nrow = 80)
#' fit <- csem_gt(d, error_type = "absolute")
#' s <- summary(fit)
#' s$global_stats
#' head(s$by_score_summary)
#'
#' @export
summary.csem <- function(object, ...) {

  bs <- object$by_score
  # Cumulative frequencies assume the table is ordered by ascending
  # observed score; .collapse_to_score() already returns it sorted, but
  # we reorder explicitly so summary.csem() does not depend on that
  # invariant of another helper.
  bs <- bs[order(bs$observed_score), , drop = FALSE]
  rownames(bs) <- NULL

  total <- sum(bs$group_size)
  bs$cum_freq   <- cumsum(bs$group_size) / total
  bs$percentile <- 100 * bs$cum_freq

  structure(
    list(
      call             = object$call,
      paradigm         = object$paradigm,
      methods          = object$methods,
      error_types      = object$error_types,
      n_persons        = object$n_persons,
      n_items          = object$n_items,
      global_stats     = .compute_global_stats(object),
      by_score_summary = bs
    ),
    class = "summary.csem"
  )
}


#' Print a `summary.csem` object
#'
#' Displays a curated console view of a [summary.csem()] object: a short
#' header, the population-level global statistics, and the score-level
#' table reduced to the identifier columns, the cumulative frequency and
#' percentile, and the conditional-SEM point estimates (the `csem.*`
#' columns). The sampling-variance, standard-error, confidence-interval
#' and smoothed columns carried by the underlying object are not shown.
#'
#' @param x A `summary.csem` object.
#' @param ... Currently ignored; present for S3 consistency.
#'
#' @return `x`, invisibly.
#'
#' @export
print.summary.csem <- function(x, ...) {

  rule <- function(n = 64L) cat(strrep("-", n), "\n", sep = "")

  rule()
  cat("Summary of Conditional SEMs in Generalizability Theory\n")
  rule()

methods_str <- .csem_methods_label(x$methods, x$error_types)
  cat(sprintf("Paradigm     :  %s\n", x$paradigm))
  cat(sprintf("Methods      :  %s\n", methods_str))
  cat(sprintf("Error types  :  %s\n", paste(x$error_types, collapse = ", ")))
  cat(sprintf("Persons      :  %d\n", x$n_persons))
  cat(sprintf("Items        :  %d\n", x$n_items))

  # -- global statistics ------------------------------------------------
  gs <- x$global_stats
  cat("Global statistics\n")
  rule()
  cat(sprintf(
    "  Relative SEM   sigma(delta) = %8.6f     E rho^2 = %8.4f\n",
    gs$relative_sem, gs$erho2))
  cat(sprintf(
    "  Absolute SEM   sigma(Delta) = %8.6f     Phi     = %8.4f\n",
    gs$absolute_sem, gs$phi))

  # -- curated score-level table ----------------------------------------
  # .csem_estimator_order is a package-internal constant defined in
  # R/print-csem.R; it fixes the canonical estimator ordering shared by
  # the print and summary methods.
  cat("CSEM by observed score\n")
  rule()
  bs <- x$by_score_summary
  csem_cols <- intersect(paste0("csem.", .csem_estimator_order), names(bs))
  show_cols <- c("observed_score", "group_size", "cum_freq",
                 "percentile", csem_cols)
  tbl <- bs[, show_cols, drop = FALSE]
  tbl$observed_score <- round(tbl$observed_score, 4)
  tbl$cum_freq       <- round(tbl$cum_freq, 4)
  tbl$percentile     <- round(tbl$percentile, 1)
  for (cc in csem_cols) tbl[[cc]] <- round(tbl[[cc]], 5)

  # Abbreviate the csem.* column headers for display only; the block
  # title already establishes these are conditional-SEM columns.
  names(tbl)[match(csem_cols, names(tbl))] <- unname(.csem_short_labels[csem_cols])

  # Widen the print width so the (potentially > 80-char) table is not
  # wrapped by print.data.frame; this keeps the display deterministic
  # regardless of the caller's console width.
  old_opts <- options(width = 10000L)
  on.exit(options(old_opts), add = TRUE)
  print(tbl, row.names = FALSE)

  invisible(x)
}