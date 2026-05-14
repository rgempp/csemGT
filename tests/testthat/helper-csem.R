# Test helpers shared across test files. testthat sources every file
# named helper-*.R in tests/testthat/ before running tests.

# A small but realistic by_score data frame for GT, in wide format,
# including absolute and the three relative estimators (full, large_a,
# uncorrelated). Scores 0:20 so the spec's extreme-handling tests work.
#
# A small amount of deterministic noise is added to each variance
# column so the quadratic OLS fit does not become a perfect fit
# (otherwise `lm` emits the "essentially perfect fit" warning).
.make_test_by_score <- function(score_max = 20L, noise_sd = 5e-5) {

  scores <- 0:score_max

  # Inverted-U variance shape (Brennan 2001, Fig. 5.1): max near midpoint.
  p <- scores / max(scores)
  var_abs <- 0.005 * 4 * p * (1 - p)         # in [0, 0.005]

  # Deterministic noise: seed locally and restore. Using a fixed seed
  # makes the helper reproducible across calls.
  if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
    .old <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    on.exit(assign(".Random.seed", .old, envir = .GlobalEnv), add = TRUE)
  } else {
    on.exit({
      if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
        rm(".Random.seed", envir = .GlobalEnv)
      }
    }, add = TRUE)
  }
  set.seed(20251114L)
  n_rows <- length(scores)
  noise <- function() stats::rnorm(n_rows, 0, noise_sd)

  var_abs       <- pmax(var_abs + noise(), 0)
  var_rel_full  <- pmax(var_abs * 1.05 + 1e-4 + noise(), 0)
  var_rel_la    <- pmax(var_abs * 1.02 + 1e-4 + noise(), 0)
  var_rel_un    <- pmax(var_abs * 0.95 + 1e-4 + noise(), 0)

  data.frame(
    observed_score              = scores,
    group_size                  = rep(1L, length(scores)),
    csem.absolute               = sqrt(var_abs),
    csem_var.absolute           = var_abs,
    csem.relative_full          = sqrt(var_rel_full),
    csem_var.relative_full      = var_rel_full,
    csem.relative_large_a       = sqrt(var_rel_la),
    csem_var.relative_large_a   = var_rel_la,
    csem.relative_uncorrelated  = sqrt(var_rel_un),
    csem_var.relative_uncorrelated = var_rel_un,
    stringsAsFactors            = FALSE
  )
}


# A minimal but structurally valid `csem` object for GT.
# 6 persons, scores 0:5 (one each), 5 items.
# IMPORTANT: build by_score with score_max = 5 so its row count
# matches the 6 rows of `estimates`.
.make_minimal_csem <- function() {

  by_score <- .make_test_by_score(score_max = 5L)

  estimates <- data.frame(
    person_id          = 1:6,
    observed_score     = 0:5,
    conditioning_value = 0:5,
    group_size         = rep(1L, 6L),
    extreme            = c(TRUE, FALSE, FALSE, FALSE, FALSE, TRUE),
    csem.absolute      = by_score$csem.absolute,
    csem_var.absolute  = by_score$csem_var.absolute,
    stringsAsFactors   = FALSE
  )

  new_csem(
    estimates           = estimates,
    by_score            = by_score,
    call                = quote(csem_gt(dummy)),
    paradigm            = "gt",
    methods             = "full",
    error_types         = c("absolute", "relative"),
    arguments           = list(R = 1000L, bootstrap = FALSE),
    variance_components = NULL,
    smooth_fits         = NULL,
    bootstrap           = NULL,
    scale_transform     = NULL,
    n_persons           = 6L,
    n_items             = 5L
  )
}
