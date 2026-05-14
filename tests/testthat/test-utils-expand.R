# Tests for R/utils-expand.R
# Covers .expand_to_person() and .collapse_to_score().

# -------------------------------------------------------------------
# .expand_to_person
# -------------------------------------------------------------------

test_that(".expand_to_person produces N rows", {
  by_score <- .make_test_by_score()
  X <- c(5, 5, 10, 12, 12, 12)
  result <- .expand_to_person(by_score, X, seq_along(X))
  expect_equal(nrow(result), 6L)
})

test_that(".expand_to_person assigns same CSEM to same score", {
  by_score <- .make_test_by_score()
  X <- c(5, 5, 10)
  result <- .expand_to_person(by_score, X, 1:3)
  expect_equal(result$csem.absolute[1], result$csem.absolute[2])
})

test_that(".expand_to_person warns about orphan scores", {
  by_score <- .make_test_by_score()
  X <- c(5, 999)  # 999 is not in by_score (scores 0:20)
  expect_warning(
    res <- .expand_to_person(by_score, X, 1:2),
    "not found"
  )
  expect_true(is.na(res$csem.absolute[2]))
  expect_false(is.na(res$csem.absolute[1]))
})

test_that(".expand_to_person carries every estimation column from by_score", {
  by_score <- .make_test_by_score()
  X <- c(0, 5, 10, 15, 20)
  result <- .expand_to_person(by_score, X, 1:5)
  cols_expected <- setdiff(names(by_score),
                           c("observed_score", "group_size"))
  expect_true(all(cols_expected %in% names(result)))
})

test_that(".expand_to_person flags extreme scores correctly", {
  by_score <- .make_test_by_score()  # scores 0:20
  X <- c(0, 10, 20)
  result <- .expand_to_person(by_score, X, 1:3)
  expect_equal(result$extreme, c(TRUE, FALSE, TRUE))
})

test_that(".expand_to_person identifier columns are correctly set", {
  by_score <- .make_test_by_score()
  X <- c(3, 7, 12)
  pid <- c("p1", "p2", "p3")
  result <- .expand_to_person(by_score, X, pid)
  expect_equal(result$person_id,          pid)
  expect_equal(result$observed_score,     X)
  expect_equal(result$conditioning_value, X)
})

test_that(".expand_to_person preserves group_size from by_score", {
  by_score <- .make_test_by_score()
  by_score$group_size <- 1:21        # encode score so we can verify lookup
  X <- c(0, 5, 20)
  result <- .expand_to_person(by_score, X, 1:3)
  expect_equal(result$group_size, c(1L, 6L, 21L))
})

# -------------------------------------------------------------------
# .collapse_to_score
# -------------------------------------------------------------------

test_that(".collapse_to_score returns one row per distinct score", {
  # Build a person-level frame from the by_score template
  by_score <- .make_test_by_score()
  X <- c(0, 5, 5, 10, 10, 10, 20)
  pp <- .expand_to_person(by_score, X, seq_along(X))

  collapsed <- .collapse_to_score(pp)
  expect_equal(sort(unique(X)), collapsed$observed_score)
  expect_equal(nrow(collapsed), length(unique(X)))
})

test_that(".collapse_to_score group_size counts persons per score", {
  by_score <- .make_test_by_score()
  X  <- c(0, 5, 5, 10, 10, 10, 20)
  pp <- .expand_to_person(by_score, X, seq_along(X))
  collapsed <- .collapse_to_score(pp)
  expect_equal(collapsed$group_size, c(1L, 2L, 3L, 1L))
})

test_that(".collapse_to_score round-trips through .expand_to_person", {
  # Start from by_score, expand to persons (1 person per score), collapse back.
  by_score <- .make_test_by_score()
  X  <- by_score$observed_score
  pp <- .expand_to_person(by_score, X, seq_along(X))
  back <- .collapse_to_score(pp)

  # observed_score and estimation columns must coincide (group_size was 1
  # in the input and 1 in the output by construction).
  est_cols <- intersect(names(by_score), names(back))
  for (col in setdiff(est_cols, "group_size")) {
    expect_equal(back[[col]], by_score[[col]],
                 tolerance = 1e-12, info = col)
  }
})

test_that(".collapse_to_score drops person-level identifier columns", {
  by_score <- .make_test_by_score()
  X  <- by_score$observed_score
  pp <- .expand_to_person(by_score, X, seq_along(X))
  back <- .collapse_to_score(pp)
  expect_false("person_id"          %in% names(back))
  expect_false("conditioning_value" %in% names(back))
  expect_false("extreme"            %in% names(back))
})

test_that(".collapse_to_score messages when within-score heterogeneity is detected", {
  # Two persons at score 5 with different csem.absolute values
  pp <- data.frame(
    person_id       = 1:3,
    observed_score  = c(5, 5, 10),
    group_size      = c(1L, 1L, 1L),
    csem.absolute   = c(0.30, 0.40, 0.50),
    csem_var.absolute = c(0.09, 0.16, 0.25),
    stringsAsFactors = FALSE
  )
  expect_message(.collapse_to_score(pp), "heterogeneity")
})

test_that(".collapse_to_score names the heterogeneous score-conditioned columns", {
  # absolute and relative_uncorrelated are score-conditioned estimators:
  # within-score heterogeneity is reported, and the offending columns are
  # named in the message (descriptively, not as an error).
  pp <- data.frame(
    person_id                  = 1:3,
    observed_score             = c(5, 5, 10),
    group_size                 = c(1L, 1L, 1L),
    csem.absolute              = c(0.30, 0.40, 0.50),
    csem.relative_uncorrelated = c(0.25, 0.35, 0.45),
    stringsAsFactors = FALSE
  )
  msg <- paste(capture_messages(.collapse_to_score(pp)), collapse = "")
  expect_match(msg, "heterogeneity")
  expect_match(msg, "within-score mean")
  expect_match(msg, "csem\\.absolute")
  expect_match(msg, "csem\\.relative_uncorrelated")
})

test_that(".collapse_to_score stays silent for by-design heterogeneity", {
  # cov_xim and the relative_full / relative_large_a estimators built on
  # it carry the person-specific covariance term (Brennan, 1998, eq. 33),
  # so within-score heterogeneity is expected by construction and must
  # not be reported.
  pp <- data.frame(
    person_id             = 1:3,
    observed_score        = c(5, 5, 10),
    group_size            = c(1L, 1L, 1L),
    cov_xim               = c(0.10, 0.20, 0.30),
    csem.relative_full    = c(0.30, 0.40, 0.50),
    csem.relative_large_a = c(0.28, 0.38, 0.48),
    stringsAsFactors = FALSE
  )
  expect_no_message(.collapse_to_score(pp))
})

test_that(".collapse_to_score reports only score-conditioned columns when heterogeneity is mixed", {
  # csem.absolute (score-conditioned -> reported) and csem.relative_full
  # (by design -> silent) are both heterogeneous within score 5. The
  # message must fire, name csem.absolute, and say nothing about
  # csem.relative_full.
  pp <- data.frame(
    person_id          = 1:3,
    observed_score     = c(5, 5, 10),
    group_size         = c(1L, 1L, 1L),
    csem.absolute      = c(0.30, 0.40, 0.50),
    csem.relative_full = c(0.30, 0.40, 0.50),
    stringsAsFactors = FALSE
  )
  msg <- paste(capture_messages(.collapse_to_score(pp)), collapse = "")
  expect_match(msg, "csem\\.absolute")
  expect_false(grepl("relative_full", msg))
})
