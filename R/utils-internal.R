# Internal helpers shared across csemGT.
# Not exported. Documented with @keywords internal for completeness.

#' Null-coalescing operator
#'
#' Returns `b` when `a` is `NULL`, otherwise returns `a`. Provided here for
#' compatibility with R versions earlier than 4.4 (where `%||%` became a
#' base operator). Internal use only.
#'
#' @param a,b R objects.
#' @return `a` if not `NULL`, else `b`.
#' @keywords internal
#' @name grapes-or-or-grapes
`%||%` <- function(a, b) if (is.null(a)) b else a


#' Restore a previously captured RNG seed
#'
#' Restores the global `.Random.seed` from a value captured earlier by
#' `.set_seed_restoring()`. If `old_seed` is `NULL` the seed is removed
#' (returning the RNG to a "no seed set yet" state), reproducing the
#' state the session was in before the seed was modified.
#'
#' @param old_seed Integer vector previously captured, or `NULL`.
#' @return Invisibly `NULL`.
#' @keywords internal
.restore_seed <- function(old_seed) {
  if (is.null(old_seed)) {
    if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  } else {
    assign(".Random.seed", old_seed, envir = .GlobalEnv)
  }
  invisible(NULL)
}
