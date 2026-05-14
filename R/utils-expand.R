# Helpers that move between the person-level wide table (`estimates`)
# and the score-level wide table (`by_score`).

#' Expand a by-score wide table to one row per person
#'
#' Given a wide-format `by_score` table (one row per distinct value of
#' the conditioning variable) and a vector of per-person observed
#' scores, returns the corresponding person-level table by matching
#' each person's score to the row of `by_score`. Persons whose score is
#' not represented in `by_score` receive `NA` values in all expanded
#' columns and trigger a warning.
#'
#' @param by_score_wide A data frame in wide format with identifier
#'   columns `observed_score` and `group_size`, plus any number of
#'   estimation columns (e.g. `csem.absolute`, `csem_var.relative_full`,
#'   `smoothed_csem.absolute`, etc.).
#' @param X Numeric vector of length \eqn{N} (the conditioning value
#'   per person; typically the row sum of `data` when
#'   `conditioning = "total"`).
#' @param person_id Vector of length \eqn{N} with person identifiers.
#'
#' @return A data frame with \eqn{N} rows containing the identifier
#'   columns `person_id`, `observed_score`, `conditioning_value`,
#'   `group_size`, `extreme`, followed by the expanded estimation
#'   columns from `by_score_wide`.
#' @keywords internal
.expand_to_person <- function(by_score_wide, X, person_id) {

  stopifnot(
    is.data.frame(by_score_wide),
    "observed_score" %in% names(by_score_wide),
    "group_size"     %in% names(by_score_wide),
    length(X) == length(person_id)
  )

  N   <- length(X)
  idx <- match(X, by_score_wide$observed_score)

  n_orphans <- sum(is.na(idx))
  if (n_orphans > 0L) {
    warning(n_orphans, " person(s) have an observed score not found in ",
            "by_score; their CSEM values will be NA.", call. = FALSE)
  }

  cols_to_expand <- setdiff(names(by_score_wide),
                            c("observed_score", "group_size"))

  min_score <- min(by_score_wide$observed_score, na.rm = TRUE)
  max_score <- max(by_score_wide$observed_score, na.rm = TRUE)

  estimates <- data.frame(
    person_id          = person_id,
    observed_score     = X,
    conditioning_value = X,
    group_size         = by_score_wide$group_size[idx],
    extreme            = !is.na(X) & X %in% c(min_score, max_score),
    stringsAsFactors   = FALSE
  )

  for (col in cols_to_expand) {
    estimates[[col]] <- by_score_wide[[col]][idx]
  }

  estimates
}


#' Collapse a person-level wide table to a by-score wide table
#'
#' Inverse of `.expand_to_person()`. For each distinct value of
#' `observed_score`, returns a single row containing the value of every
#' estimation column at that score. Under the GT model the conditional
#' error variance is a function of the score (or, for `full` and
#' `large_a`, additionally of within-score variability — see Brennan,
#' 1998), so per-person values within a score are not necessarily
#' identical. Non-identical values are collapsed by the within-score
#' mean. Heterogeneity is by design for `cov_xim` and the `full` and
#' `large_a` estimators, so it is collapsed silently; for the
#' score-conditioned estimators (`absolute`, `relative_uncorrelated`) a
#' single descriptive message names the affected columns.
#'
#' @param per_person_wide A data frame in person-level wide format with
#'   columns `observed_score` plus any number of estimation columns.
#'
#' @return A data frame with one row per distinct observed score and
#'   identifier columns `observed_score` and `group_size`.
#' @keywords internal
.collapse_to_score <- function(per_person_wide) {

  stopifnot(
    is.data.frame(per_person_wide),
    "observed_score" %in% names(per_person_wide)
  )

  cols_to_collapse <- setdiff(
    names(per_person_wide),
    c("person_id", "observed_score", "conditioning_value",
      "group_size", "extreme")
  )

  sorted_scores <- sort(unique(per_person_wide$observed_score),
                        na.last = NA)

  group_size <- vapply(
    sorted_scores,
    function(s) sum(per_person_wide$observed_score == s, na.rm = TRUE),
    integer(1)
  )

  out <- data.frame(
    observed_score = sorted_scores,
    group_size     = group_size,
    stringsAsFactors = FALSE
  )

  # Track which columns show within-score heterogeneity (no <<-).
  state <- new.env(parent = emptyenv())
  state$het_cols <- character(0)

  for (col in cols_to_collapse) {
    vals <- vapply(
      sorted_scores,
      function(s) {
        v <- per_person_wide[[col]][per_person_wide$observed_score == s]
        v <- v[!is.na(v)]
        if (length(v) == 0L) return(NA_real_)
        # Numeric average if multiple values; for character columns the
        # first non-NA value (the by-score table is only meaningful for
        # numerics so this branch is a defensive fallback).
        if (is.numeric(v)) {
          if (length(v) > 1L && diff(range(v)) > 1e-12) {
            assign("het_cols", union(state$het_cols, col), envir = state)
          }
          mean(v)
        } else {
          v[[1L]]
        }
      },
      numeric(1)
    )
    out[[col]] <- vals
  }

  # Within-score heterogeneity is by design for any quantity built from
  # the person-specific covariance term cov_xim (Brennan, 1998, eq. 33):
  # cov_xim itself and the relative_full / relative_large_a estimators
  # that use it. Those columns are collapsed by the within-score mean
  # silently. For the score-conditioned estimators (absolute,
  # relative_uncorrelated) within-score heterogeneity is expected with
  # polytomous items but not with strictly dichotomous items, so it is
  # reported there -- descriptively, naming the columns, not as an error.
  if (length(state$het_cols) > 0L) {
    estimator_of <- function(nm) {
      parts <- strsplit(nm, ".", fixed = TRUE)[[1L]]
      parts[[length(parts)]]
    }
    by_design <- function(nm) {
      identical(nm, "cov_xim") ||
        estimator_of(nm) %in% c("relative_full", "relative_large_a")
    }
    report_cols <- state$het_cols[
      !vapply(state$het_cols, by_design, logical(1))
    ]
    if (length(report_cols) > 0L) {
      message(
        "Within-score heterogeneity when collapsing to the by-score ",
        "table, in: ", paste(sort(report_cols), collapse = ", "),
        ". The by-score value is the within-score mean. This is ",
        "expected with polytomous items; with strictly dichotomous ",
        "items these estimators are functions of the observed score ",
        "alone, so heterogeneity there would be worth checking."
      )
    }
  }

  out
}
