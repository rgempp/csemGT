# Tests for R/utils-validate.R
# Covers .validate_data, .validate_method, .validate_error_type,
# .validate_args, .set_seed_restoring, .restore_seed.

# -------------------------------------------------------------------
# .validate_data
# -------------------------------------------------------------------

test_that(".validate_data accepts matrices and returns numeric matrix", {
  M <- matrix(rbinom(40, 1, 0.5), nrow = 10, ncol = 4)
  out <- .validate_data(M)
  expect_true(is.matrix(out))
  expect_type(out, "double")
  expect_equal(dim(out), c(10L, 4L))
})

test_that(".validate_data coerces data.frame with numeric columns", {
  df <- as.data.frame(matrix(rnorm(20), 5, 4))
  out <- .validate_data(df)
  expect_true(is.matrix(out))
  expect_equal(dim(out), c(5L, 4L))
})

test_that(".validate_data rejects non-numeric data.frame columns", {
  df <- data.frame(x = 1:5, y = letters[1:5])
  expect_error(.validate_data(df), "numeric")
})

test_that(".validate_data rejects non-matrix non-data.frame input", {
  expect_error(.validate_data(list(a = 1)), "matrix or data.frame")
})

test_that(".validate_data enforces minimum 2 persons and 2 items", {
  expect_error(.validate_data(matrix(1, 1, 4)), "2 persons")
  expect_error(.validate_data(matrix(1, 4, 1)), "2 items")
})

test_that(".validate_data with na_action = 'fail' errors on NA", {
  M <- matrix(1, 5, 3); M[2, 2] <- NA
  expect_error(.validate_data(M, na_action = "fail"), "Missing")
})

test_that(".validate_data with na_action = 'listwise' drops NA rows", {
  M <- matrix(1, 5, 3); M[2, 2] <- NA
  out <- .validate_data(M, na_action = "listwise")
  expect_equal(nrow(out), 4L)
})

test_that(".validate_data rejects pairwise for GT", {
  M <- matrix(1, 5, 3); M[2, 2] <- NA
  expect_error(.validate_data(M, na_action = "pairwise"),
               "not supported for the GT paradigm")
})

test_that(".validate_data rejects non-finite values", {
  M <- matrix(1, 5, 3); M[2, 2] <- Inf
  expect_error(.validate_data(M), "finite")
})

test_that(".validate_data rejects non-binary input when require_dichotomous", {
  M <- matrix(c(0, 1, 2), 6, 3)
  expect_error(.validate_data(M, require_dichotomous = TRUE), "dichotomous")
})

# -------------------------------------------------------------------
# .validate_method
# -------------------------------------------------------------------

test_that(".validate_method accepts valid methods", {
  out <- .validate_method(c("full", "large_a"),
                          valid = c("full", "large_a", "uncorrelated"))
  expect_equal(out, c("full", "large_a"))
})

test_that(".validate_method preserves user order and deduplicates", {
  out <- .validate_method(c("large_a", "full", "large_a"),
                          valid = c("full", "large_a", "uncorrelated"))
  expect_equal(out, c("large_a", "full"))
})

test_that(".validate_method rejects unknown methods with clear message", {
  expect_error(.validate_method("ridiculous",
                                valid = c("full", "large_a")),
               "Invalid method")
})

test_that(".validate_method rejects empty input", {
  expect_error(.validate_method(character(0),
                                valid = c("full", "large_a")),
               "non-empty")
})

# -------------------------------------------------------------------
# .validate_error_type
# -------------------------------------------------------------------

test_that(".validate_error_type accepts both 'absolute' and 'relative' for gt", {
  expect_equal(
    .validate_error_type(method = "full",
                         error_type = c("absolute", "relative"),
                         paradigm   = "gt"),
    c("absolute", "relative")
  )
})

test_that(".validate_error_type rejects unknown values", {
  expect_error(
    .validate_error_type(method = "full",
                         error_type = "weird",
                         paradigm   = "gt"),
    "Invalid error_type"
  )
})

test_that(".validate_error_type rejects unknown paradigm", {
  expect_error(
    .validate_error_type(method = "x", error_type = "absolute",
                         paradigm = "unknown"),
    "Unknown paradigm"
  )
})

# -------------------------------------------------------------------
# .validate_args
# -------------------------------------------------------------------

test_that(".validate_args accepts defaults", {
  expect_true(.validate_args())
})

test_that(".validate_args rejects ci_level outside (0, 1)", {
  expect_error(.validate_args(ci_level = 0),  "ci_level")
  expect_error(.validate_args(ci_level = 1),  "ci_level")
  expect_error(.validate_args(ci_level = -1), "ci_level")
})

test_that(".validate_args rejects R < 100", {
  expect_error(.validate_args(R = 50), "R")
})

test_that(".validate_args rejects non-numeric n_items_D", {
  expect_error(.validate_args(n_items_D = "ten"), "n_items_D")
})

test_that(".validate_args rejects non-logical flags", {
  expect_error(.validate_args(bootstrap = "yes"),       "bootstrap")
  expect_error(.validate_args(exclude_extremes = NA),   "exclude_extremes")
  expect_error(.validate_args(verbose = c(TRUE, TRUE)), "verbose")
})

# -------------------------------------------------------------------
# .set_seed_restoring + .restore_seed
# -------------------------------------------------------------------

test_that(".set_seed_restoring captures the prior seed and sets a new one", {
  # Establish a known seed first
  set.seed(11L)
  before <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)

  old <- .set_seed_restoring(42L)
  # Old should equal `before`
  expect_identical(old, before)
  # And the new seed must have taken effect: a fresh draw is deterministic
  draw <- runif(1)

  set.seed(42L)
  expect_equal(draw, runif(1), tolerance = 1e-12)
})

test_that(".restore_seed brings .Random.seed back exactly", {
  set.seed(7L)
  before <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)

  old <- .set_seed_restoring(99L)
  runif(5)  # consume some random numbers
  .restore_seed(old)

  after <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  expect_identical(after, before)
})

test_that(".restore_seed handles NULL (no prior seed) by removing .Random.seed", {
  # Simulate "no seed yet" state
  if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
    rm(".Random.seed", envir = .GlobalEnv)
  }
  old <- .set_seed_restoring(3L)
  expect_null(old)

  .restore_seed(old)
  expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))

  # Restore session state for downstream tests
  set.seed(1L)
})
