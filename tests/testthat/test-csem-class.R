# Tests for R/csem-class.R
# Covers new_csem(), is.csem(), validate_csem().

test_that("new_csem constructs object of class 'csem'", {
  obj <- .make_minimal_csem()
  expect_s3_class(obj, "csem")
  expect_true(is.csem(obj))
})

test_that("new_csem stores n_persons / n_items as integers", {
  obj <- .make_minimal_csem()
  expect_type(obj$n_persons, "integer")
  expect_type(obj$n_items,   "integer")
})

test_that("new_csem rejects unknown paradigm", {
  expect_error(
    new_csem(
      estimates = data.frame(person_id = 1, observed_score = 0, group_size = 1L),
      by_score  = data.frame(observed_score = 0, group_size = 1L),
      call      = quote(f()),
      paradigm  = "frobnicator",
      methods   = "full",
      error_types = "absolute",
      arguments = list(),
      n_persons = 1L,
      n_items   = 1L
    )
  )
})

test_that("is.csem distinguishes csem objects from plain lists", {
  obj <- .make_minimal_csem()
  expect_true(is.csem(obj))
  expect_false(is.csem(list()))
  expect_false(is.csem(NULL))
  expect_false(is.csem(data.frame(x = 1)))
})

test_that("validate_csem returns the object invisibly on success", {
  obj <- .make_minimal_csem()
  out <- withVisible(validate_csem(obj))
  expect_false(out$visible)
  expect_identical(out$value, obj)
})

test_that("validate_csem catches wrong nrow(estimates)", {
  bad <- .make_minimal_csem()
  bad$n_persons <- bad$n_persons + 1L
  expect_error(validate_csem(bad), "n_persons")
})

test_that("validate_csem catches missing identifier columns", {
  bad <- .make_minimal_csem()
  bad$estimates$person_id <- NULL
  expect_error(validate_csem(bad), "identifier")
})

test_that("validate_csem catches missing top-level components", {
  bad <- .make_minimal_csem()
  bad$paradigm <- NULL
  expect_error(validate_csem(bad), "missing required")
})

test_that("validate_csem rejects non-csem input", {
  expect_error(validate_csem(list(a = 1)), "class 'csem'")
})
