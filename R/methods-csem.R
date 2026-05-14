# S3 accessors for the `csem` class: by_score(), as.data.frame.csem(),
# coef.csem(). These expose components of the csem object through the
# conventional R accessor interface; they perform no computation, only
# selection. The display methods (print, summary, plot) live elsewhere.

#' Extract the score-level table from a `csem` object
#'
#' `by_score()` returns the score-level summary table of a `csem` object:
#' one row per distinct observed score, with the estimation columns
#' collapsed to that score. It is the accessor-style equivalent of the
#' `$by_score` component and of `as.data.frame(x, by = "score")`.
#'
#' @param x A `csem` object.
#' @param ... Currently ignored; present for S3 extensibility.
#'
#' @return A data frame with one row per distinct observed score.
#'
#' @seealso [as.data.frame.csem()] for the person-level and score-level
#'   tables through the `as.data.frame` interface; [coef.csem()] for the
#'   variance components.
#'
#' @examples
#' set.seed(1)
#' d <- matrix(rbinom(60 * 12, 1, 0.5), nrow = 60)
#' fit <- csem_gt(d, error_type = "absolute")
#' head(by_score(fit))
#'
#' @export
by_score <- function(x, ...) {
  UseMethod("by_score")
}

#' @rdname by_score
#' @export
by_score.csem <- function(x, ...) {
  x$by_score
}


#' Coerce a `csem` object to a data frame
#'
#' Returns one of the two wide-format tables carried by a `csem` object:
#' the person-level table (`by = "person"`, the default) or the
#' score-level table (`by = "score"`).
#'
#' @param x A `csem` object.
#' @param row.names,optional Accepted for consistency with the
#'   [base::as.data.frame()] generic; both are ignored.
#' @param by One of `"person"` (the default) or `"score"`. `"person"`
#'   returns the per-person `$estimates` table; `"score"` returns the
#'   `$by_score` table.
#' @param ... Currently ignored.
#'
#' @return A data frame: `$estimates` when `by = "person"`, `$by_score`
#'   when `by = "score"`.
#'
#' @seealso [by_score()] for the score-level table through a dedicated
#'   accessor; [coef.csem()] for the variance components.
#'
#' @examples
#' set.seed(1)
#' d <- matrix(rbinom(60 * 12, 1, 0.5), nrow = 60)
#' fit <- csem_gt(d, error_type = "absolute")
#' head(as.data.frame(fit))                 # by = "person"
#' head(as.data.frame(fit, by = "score"))
#'
#' @export
as.data.frame.csem <- function(x, row.names = NULL, optional = FALSE,
                               by = c("person", "score"), ...) {
  by <- match.arg(by)
  switch(by,
         person = x$estimates,
         score  = x$by_score)
}


#' Extract the variance components of a `csem` object
#'
#' Returns the `variance_components` list of a `csem` object: the ANOVA
#' table, the person / item / residual variance components, the
#' population-level error variances and SEMs, and the reliability-like
#' coefficients. For the GT paradigm this is the natural set of fitted
#' "coefficients", so it is what `coef()` returns.
#'
#' @param object A `csem` object.
#' @param ... Currently ignored.
#'
#' @return The `variance_components` list of `object`. See [csem_gt()]
#'   for its structure.
#'
#' @seealso [by_score()] and [as.data.frame.csem()] for the estimation
#'   tables.
#'
#' @examples
#' set.seed(1)
#' d <- matrix(rbinom(60 * 12, 1, 0.5), nrow = 60)
#' fit <- csem_gt(d, error_type = "absolute")
#' coef(fit)$reliability_coefficients
#'
#' @export
coef.csem <- function(object, ...) {
  object$variance_components
}
