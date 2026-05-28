# S3 class `csem`: constructor, validator, predicate.

#' Construct a `csem` object
#'
#' Low-level constructor for the `csem` S3 class. This is an internal
#' building block used by `csem_gt()`. End users should not call this
#' directly.
#'
#' @param estimates A data frame, one row per person, in wide format.
#'   Must contain at minimum the identifier columns `person_id`,
#'   `observed_score`, `group_size`.
#' @param by_score A data frame, one row per distinct value of the
#'   conditioning variable, also in wide format.
#' @param call A captured `match.call()` of the user-facing function.
#' @param paradigm Character scalar; one of `"gt"`, `"split_half"`,
#'   `"anova"`, `"binomial"`.
#' @param methods Character vector of methods applied.
#' @param error_types Character vector of error types reported.
#' @param arguments Named list with the resolved arguments of the
#'   user-facing call.
#' @param variance_components Named list with ANOVA-based variance
#'   components (used by the GT paradigm). May be `NULL`.
#' @param smooth_fits Named list of smoother diagnostics (one element
#'   per smoothed column). May be `NULL`.
#' @param diagnostics Named list of smoother sample-size diagnostics
#'   (`n_floor`, `n_ceiling`, `n_fit`). May be `NULL`.
#' @param bootstrap Named list with bootstrap metadata and replicates,
#'   or `NULL` when bootstrap is not performed.
#' @param scale_transform Scale transformation specification, or `NULL`.
#' @param n_persons,n_items Integer scalars.
#' @param version Package version. Defaults to the installed version of
#'   `csemGT`; falls back to `"0.0.0"` when the package is being built.
#'
#' @return An object of class `c("csem", "list")`.
#' @keywords internal
new_csem <- function(estimates,
                     by_score,
                     call,
                     paradigm,
                     methods,
                     error_types,
                     arguments,
                     variance_components = NULL,
                     smooth_fits         = NULL,
                     diagnostics         = NULL,
                     bootstrap           = NULL,
                     scale_transform     = NULL,
                     n_persons,
                     n_items,
                     version             = .csemGT_version()) {

  stopifnot(
    is.data.frame(estimates),
    is.data.frame(by_score),
    is.character(paradigm), length(paradigm) == 1L,
    paradigm %in% c("gt", "split_half", "anova", "binomial"),
    is.character(methods), length(methods) >= 1L,
    is.character(error_types), length(error_types) >= 1L,
    is.list(arguments),
    is.numeric(n_persons), length(n_persons) == 1L,
    is.numeric(n_items),   length(n_items)   == 1L
  )

  obj <- list(
    estimates           = estimates,
    by_score            = by_score,
    call                = call,
    paradigm            = paradigm,
    methods             = methods,
    error_types         = error_types,
    arguments           = arguments,
    variance_components = variance_components,
    smooth_fits         = smooth_fits,
    diagnostics         = diagnostics,
    bootstrap           = bootstrap,
    scale_transform     = scale_transform,
    n_persons           = as.integer(n_persons),
    n_items             = as.integer(n_items),
    version             = version
  )
  class(obj) <- c("csem", "list")
  obj
}


#' Test whether an object is a `csem` object
#'
#' @param x Any R object.
#' @return `TRUE` if `x` inherits from `"csem"`, `FALSE` otherwise.
#'
#' @examples
#' set.seed(1)
#' d <- matrix(rbinom(80 * 15, 1, 0.5), nrow = 80)
#' fit <- suppressMessages(csem_gt(d, error_type = "absolute"))
#' is.csem(fit)
#' is.csem(list())
#'
#' @export
is.csem <- function(x) inherits(x, "csem")

#' Validate a `csem` object
#'
#' Performs structural checks beyond the type assertions enforced by
#' `new_csem()`. Specifically, verifies that:
#' \itemize{
#'   \item all required top-level components are present;
#'   \item `nrow(estimates)` equals `n_persons`;
#'   \item the identifier columns `person_id`, `observed_score`,
#'         `group_size` are present in `estimates`.
#' }
#'
#' Called automatically at the end of `csem_gt()`; can also be invoked
#' programmatically.
#'
#' @param x A candidate `csem` object.
#' @return Invisibly `x` if all checks pass; otherwise raises an error.
#' @keywords internal
validate_csem <- function(x) {

  if (!is.csem(x)) {
    stop("`x` must inherit from class 'csem'.", call. = FALSE)
  }

  required_top <- c("estimates", "by_score", "paradigm", "methods",
                    "n_persons", "n_items")
  missing_top <- setdiff(required_top, names(x))
  if (length(missing_top)) {
    stop("csem object missing required components: ",
         paste(missing_top, collapse = ", "), call. = FALSE)
  }

  if (nrow(x$estimates) != x$n_persons) {
    stop("estimates has ", nrow(x$estimates),
         " rows but n_persons = ", x$n_persons, call. = FALSE)
  }

  id_required <- c("person_id", "observed_score", "group_size")
  missing_id  <- setdiff(id_required, names(x$estimates))
  if (length(missing_id)) {
    stop("estimates missing identifier columns: ",
         paste(missing_id, collapse = ", "), call. = FALSE)
  }

  invisible(x)
}


#' Resolve installed package version, with a build-time fallback
#'
#' During `devtools::load_all()` and `R CMD check` the package may not
#' yet be registered, so `utils::packageVersion("csemGT")` can fail. We
#' fall back to `"0.0.0"` in that case.
#'
#' @return A `package_version` object.
#' @keywords internal
#' @noRd
.csemGT_version <- function() {
  tryCatch(
    utils::packageVersion("csemGT"),
    error = function(e) as.package_version("0.0.0")
  )
}
