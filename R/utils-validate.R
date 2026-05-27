# Validation helpers for csemGT.
# All helpers are internal and consume the user-facing arguments of
# csem_gt() prior to any numerical work.

#' Validate the input data matrix
#'
#' Coerces a data frame or matrix to a numeric matrix and checks the
#' preconditions of the persons-by-items single-facet crossed design:
#' all columns numeric, no missing values (the legacy `csem_g1f()` and
#' `gtcsem.ado` both require complete balanced data), and at least
#' 2 persons by 2 items.
#'
#' @param data Matrix or data frame, persons in rows, items in columns.
#' @param na_action Character; one of `"listwise"`, `"fail"`, `"pairwise"`.
#'   In `csemGT` v1.0 only `"listwise"` and `"fail"` are supported.
#' @param require_balanced Logical; reserved for future designs. Currently
#'   the single-facet crossed design is always balanced by construction.
#' @param require_complete Logical; if `TRUE` (default), an error is raised
#'   when missing values remain after `na_action` is applied.
#' @param require_dichotomous Logical; reserved for future use. Ignored
#'   in `csemGT`.
#'
#' @return A numeric matrix, persons in rows, items in columns.
#' @keywords internal
.validate_data <- function(data,
                           na_action          = "listwise",
                           require_balanced   = TRUE,
                           require_complete   = TRUE,
                           require_dichotomous = FALSE) {

  if (missing(data) || is.null(data)) {
    stop("`data` must be supplied (matrix or data.frame).", call. = FALSE)
  }

  # Coerce data.frame -> matrix, checking numeric columns explicitly
  if (is.data.frame(data)) {
    nn <- vapply(data, is.numeric, logical(1))
    if (!all(nn)) {
      bad <- names(data)[!nn]
      stop("All columns of `data` must be numeric; non-numeric: ",
           paste(bad, collapse = ", "), call. = FALSE)
    }
    data <- as.matrix(data)
  } else if (!is.matrix(data)) {
    stop("`data` must be a matrix or data.frame.", call. = FALSE)
  }

  storage.mode(data) <- "double"

  if (nrow(data) < 2L) {
    stop("At least 2 persons are required.", call. = FALSE)
  }
  if (ncol(data) < 2L) {
    stop("At least 2 items are required.", call. = FALSE)
  }

  # Handle missing values
  na_action <- match.arg(na_action, c("listwise", "fail", "pairwise"))
  if (anyNA(data)) {
    if (na_action == "fail") {
      stop("Missing values present in `data` and na_action = 'fail'.",
           call. = FALSE)
    } else if (na_action == "listwise") {
      keep <- stats::complete.cases(data)
      if (sum(keep) < 2L) {
        stop("Fewer than 2 persons remain after listwise deletion.",
             call. = FALSE)
      }
      data <- data[keep, , drop = FALSE]
    } else {
      # "pairwise" reserved for future paradigms; GT requires complete data
      stop("na_action = 'pairwise' is not supported for the GT paradigm; ",
           "use 'listwise' or 'fail'.", call. = FALSE)
    }
  }

  if (require_complete && anyNA(data)) {
    stop("Complete balanced data required; missing values remain.",
         call. = FALSE)
  }

  if (!all(is.finite(data))) {
    stop("`data` must contain only finite numeric values.", call. = FALSE)
  }

  if (require_dichotomous) {
    vals <- unique(as.vector(data))
    if (!all(vals %in% c(0, 1))) {
      stop("`data` must be dichotomous (0/1) for this paradigm.",
           call. = FALSE)
    }
  }

  data
}


#' Validate the method argument against the paradigm's valid set
#'
#' @param method Character vector supplied by the user.
#' @param valid Character vector of allowed methods.
#' @param paradigm Character scalar (for error messages).
#'
#' @return The validated character vector (subset of `valid`).
#' @keywords internal
.validate_method <- function(method, valid, paradigm = "gt") {
  if (!is.character(method) || length(method) == 0L) {
    stop("`method` must be a non-empty character vector.", call. = FALSE)
  }
  bad <- setdiff(method, valid)
  if (length(bad)) {
    stop("Invalid method(s) for paradigm '", paradigm, "': ",
         paste(shQuote(bad), collapse = ", "),
         ". Valid options: ", paste(shQuote(valid), collapse = ", "), ".",
         call. = FALSE)
  }
  # Drop duplicates, preserve user order
  method[!duplicated(method)]
}


#' Validate the error_type argument
#'
#' Enforces the paradigm-specific set of allowed values for `error_type`.
#' For `csem_gt()`, both `"absolute"` and `"relative"` are valid.
#'
#' @param method Character vector of resolved methods.
#' @param error_type Character vector supplied by the user.
#' @param paradigm Character scalar.
#'
#' @return The validated character vector (subset of valid values).
#' @keywords internal
.validate_error_type <- function(method, error_type, paradigm = "gt") {
  valid <- switch(
    paradigm,
    gt          = c("absolute", "relative"),
    split_half  = c("absolute", "relative"),
    anova       = "absolute",
    binomial    = "absolute",
    stop("Unknown paradigm '", paradigm, "'.", call. = FALSE)
  )
  if (!is.character(error_type) || length(error_type) == 0L) {
    stop("`error_type` must be a non-empty character vector.", call. = FALSE)
  }
  bad <- setdiff(error_type, valid)
  if (length(bad)) {
    stop("Invalid error_type for paradigm '", paradigm, "': ",
         paste(shQuote(bad), collapse = ", "),
         ". Valid options: ", paste(shQuote(valid), collapse = ", "), ".",
         call. = FALSE)
  }
  error_type[!duplicated(error_type)]
}


#' Lightweight check of scalar arguments supplied to csem_gt()
#'
#' Validates types and ranges of the scalar arguments that `csem_gt()`
#' accepts. Numeric requirements are enforced (e.g. `R >= 100`, `ci_level
#' in (0, 1)`); logical flags are checked for length 1.
#'
#' @param ci_level,R,n_items_D,cutpoint,seed As in `csem_gt()`.
#' @param bootstrap,return_analytical,exclude_extremes,truncate_vc,truncate_negative_error_var,boot_keep_replicates,verbose As in `csem_gt()`.
#'
#' @return Invisibly `TRUE`.
#' @keywords internal
.validate_args <- function(ci_level = 0.95,
                           R = 1000L,
                           n_items_D = NULL,
                           cutpoint = NULL,
                           seed = NULL,
                           bootstrap = FALSE,
                           return_analytical = TRUE,
                           exclude_extremes = FALSE,
                           truncate_vc = FALSE,
                           truncate_negative_error_var = FALSE,
                           boot_keep_replicates = FALSE,
                           verbose = FALSE) {

  .is_scalar_logical <- function(x) {
    is.logical(x) && length(x) == 1L && !is.na(x)
  }

  if (!is.numeric(ci_level) || length(ci_level) != 1L ||
      !is.finite(ci_level) || ci_level <= 0 || ci_level >= 1) {
    stop("`ci_level` must be a numeric scalar in (0, 1).", call. = FALSE)
  }
  if (!is.numeric(R) || length(R) != 1L || !is.finite(R) || R < 100) {
    stop("`R` must be a numeric scalar >= 100.", call. = FALSE)
  }
  if (!is.null(n_items_D) &&
      (!is.numeric(n_items_D) || length(n_items_D) != 1L ||
       !is.finite(n_items_D) || n_items_D <= 0)) {
    stop("`n_items_D` must be NULL or a positive numeric scalar.",
         call. = FALSE)
  }
  if (!is.null(cutpoint) &&
      (!is.numeric(cutpoint) || length(cutpoint) != 1L ||
       !is.finite(cutpoint))) {
    stop("`cutpoint` must be NULL or a finite numeric scalar.",
         call. = FALSE)
  }
  if (!is.null(seed) &&
      (!is.numeric(seed) || length(seed) != 1L || !is.finite(seed))) {
    stop("`seed` must be NULL or a finite numeric scalar.", call. = FALSE)
  }

  for (nm in c("bootstrap", "return_analytical", "exclude_extremes",
               "truncate_vc", "truncate_negative_error_var",
               "boot_keep_replicates", "verbose")) {
    val <- get(nm, inherits = FALSE)
    if (!.is_scalar_logical(val)) {
      stop("`", nm, "` must be a single logical (TRUE/FALSE).",
           call. = FALSE)
    }
  }

  invisible(TRUE)
}


#' Set the RNG seed while remembering the prior state
#'
#' Captures the current value of `.Random.seed` (or `NULL` if no seed
#' has been set in the session), calls `base::set.seed()` with the
#' user-supplied seed, and returns the captured state. The caller is
#' responsible for restoring the prior state using `.restore_seed()` via
#' an `on.exit()` registration, e.g.:
#'
#' \preformatted{
#' if (!is.null(seed)) {
#'   old_seed <- .set_seed_restoring(seed)
#'   on.exit(.restore_seed(old_seed), add = TRUE)
#' }
#' }
#'
#' This pattern keeps the public function reproducible without leaking
#' a deterministic state into the user's session.
#'
#' @param seed Integer or numeric scalar passed to `base::set.seed()`.
#' @return Invisibly the previous value of `.Random.seed`, or `NULL` if
#'   none was set.
#' @keywords internal
.set_seed_restoring <- function(seed) {
  if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
    old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  } else {
    old_seed <- NULL
  }
  set.seed(seed)
  invisible(old_seed)
}
