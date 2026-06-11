library(testthat)

# Tests for the kNN / tree-list imputation helpers in R/knn_tools.R.
#
# The reference frame is make_knn_ref(n=20, seed=7) (ids r1..r20). Targets are
# built from make_knn_ref(n=5, seed=99) but relabeled t1..t5 so their row names
# do not collide with the reference ids -- yaImpute keys references vs targets
# by row name, and identical ids would make newtargets() ignore the targets as
# "original references".

# ---- yai_id (WORKS) --------------------------------------------------------

test_that("yai_id returns a yai object tagged with the id column", {
  skip_if_not_installed("yaImpute")

  m <- yai_id(
    xNms = c("x1", "x2"), yNms = c("y1", "y2"), idNm = "id",
    data = make_knn_ref(), k = 3, method = "euclidean"
  )

  expect_s3_class(m, "yai")
  expect_true(inherits(m, "yai"))
  expect_identical(m$updateID, "id")
})

test_that("yai_id errors when a required argument is NULL (4 guards)", {
  skip_if_not_installed("yaImpute")

  d <- make_knn_ref()
  expect_error(
    yai_id(xNms = NULL, yNms = c("y1", "y2"), idNm = "id", data = d, k = 3),
    "xNms"
  )
  expect_error(
    yai_id(xNms = c("x1", "x2"), yNms = NULL, idNm = "id", data = d, k = 3),
    "yNms"
  )
  expect_error(
    yai_id(xNms = c("x1", "x2"), yNms = c("y1", "y2"), idNm = NULL, data = d, k = 3),
    "idNm"
  )
  expect_error(
    yai_id(xNms = c("x1", "x2"), yNms = c("y1", "y2"), idNm = "id", data = NULL, k = 3),
    "data"
  )
})

# ---- newtargets_id (WORKS) -------------------------------------------------

test_that("newtargets_id returns a yai object tagged with update_id", {
  skip_if_not_installed("yaImpute")

  m <- yai_id(
    xNms = c("x1", "x2"), yNms = c("y1", "y2"), idNm = "id",
    data = make_knn_ref(), k = 3, method = "euclidean"
  )
  tg <- make_knn_ref(5, 99)
  tg$id <- paste0("t", seq_len(nrow(tg)))

  nt <- newtargets_id(m, idNm = "id", data = tg, k = 3, ann = FALSE)

  expect_s3_class(nt, "yai")
  expect_true(inherits(nt, "yai"))
  expect_identical(nt$update_id, "id")
})

# ---- yai_weights (WORKS) ---------------------------------------------------

test_that("yai_weights returns named structure with normalized inverse-dist weights", {
  skip_if_not_installed("yaImpute")

  m <- yai_id(
    xNms = c("x1", "x2"), yNms = c("y1", "y2"), idNm = "id",
    data = make_knn_ref(), k = 3, method = "euclidean"
  )
  tg <- make_knn_ref(5, 99)
  tg$id <- paste0("t", seq_len(nrow(tg)))
  nt <- newtargets_id(m, idNm = "id", data = tg, k = 3, ann = FALSE)

  w <- yai_weights(nt, dtype = "invdist", zero_dist = "small")

  expect_identical(names(w), c("k", "col_dist", "col_wt", "col_id", "wts"))
  expect_equal(w$k, 3)
  expect_identical(w$col_wt, c("wt.k1", "wt.k2", "wt.k3"))
  # inverse-distance weights are normalized to sum to 1 per target row
  expect_true(all(abs(rowSums(w$wts[, w$col_wt]) - 1) < 1e-8))
})

# ---- tl_impute (WORKS) -----------------------------------------------------

test_that("tl_impute returns a long tree list keyed by source/target", {
  skip_if_not_installed("yaImpute")
  skip_if_not_installed("reshape2")

  m <- yai_id(
    xNms = c("x1", "x2"), yNms = c("y1", "y2"), idNm = "id",
    data = make_knn_ref(), k = 3, method = "euclidean"
  )
  tg <- make_knn_ref(5, 99)
  tg$id <- paste0("t", seq_len(nrow(tg)))
  nt <- newtargets_id(m, idNm = "id", data = tg, k = 3, ann = FALSE)
  w  <- yai_weights(nt, dtype = "invdist", zero_dist = "small")

  trees <- make_knn_ref() # source tree records keyed by id

  out <- tl_impute(wts = w, idNm = "id", trees = trees)

  expect_true(is.data.frame(out))
  # core columns plus the joined tree (source-record) columns
  expect_true(all(c("source_id", "target_id", "weight", "distance") %in% names(out)))
  expect_true(all(c("x1", "x2", "y1", "y2") %in% names(out)))
  # k neighbors per target: 3 * 5 targets = 15 rows
  expect_identical(nrow(out), 15L)
})

# ---- tl_impute_2 (BROKEN at baseline) --------------------------------------

test_that("tl_impute_2 produces the same shaped data.frame as tl_impute", {
  skip_if_not_installed("yaImpute")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("reshape2")

  m <- yai_id(
    xNms = c("x1", "x2"), yNms = c("y1", "y2"), idNm = "id",
    data = make_knn_ref(), k = 3, method = "euclidean"
  )
  tg <- make_knn_ref(5, 99)
  tg$id <- paste0("t", seq_len(nrow(tg)))
  nt <- newtargets_id(m, idNm = "id", data = tg, k = 3, ann = FALSE)
  w  <- yai_weights(nt, dtype = "invdist", zero_dist = "small")
  trees <- make_knn_ref()

  # FAILS at baseline: L330 calls non-existent `data.frameQ()` (typo for
  # data.frame), and L333 passes a stray bare `y` argument to dplyr::left_join.
  # Intended behavior is the same long tree list that tl_impute returns.
  out <- tl_impute_2(wts = w, idNm = "id", trees = trees)

  expect_true(is.data.frame(out))
  expect_true(all(c("source_id", "target_id", "weight", "distance") %in% names(out)))
  expect_true(all(c("x1", "x2", "y1", "y2") %in% names(out)))
  expect_identical(nrow(out), 15L)
})

# ---- impute_id (BROKEN at baseline) ----------------------------------------

test_that("impute_id imputes responses to targets and labels the id column", {
  skip_if_not_installed("yaImpute")

  m <- yai_id(
    xNms = c("x1", "x2"), yNms = c("y1", "y2"), idNm = "id",
    data = make_knn_ref(), k = 3, method = "euclidean"
  )
  tg <- make_knn_ref(5, 99)
  tg$id <- paste0("t", seq_len(nrow(tg)))
  nt <- newtargets_id(m, idNm = "id", data = tg, k = 3, ann = FALSE)

  # FAILS at baseline: an unconditional browser() at L197 abandons the caller's
  # assignment under the test runner, so `out` is never a usable data.frame.
  # Intended behavior: a data.frame whose first column is named "id" with one
  # row per target.
  out <- impute_id(nt)

  expect_true(is.data.frame(out))
  expect_identical(names(out)[1], "id")
  expect_identical(nrow(out), length(nt$trgRows))
})

# ---- yai_cv (WORKS) --------------------------------------------------------

test_that("yai_cv returns a cross-validation summary with rmse/rsq columns", {
  skip_if_not_installed("yaImpute")
  skip_if_not_installed("plyr")

  set.seed(1)
  dat <- make_knn_ref(40, seed = 3)
  cv <- suppressWarnings(yai_cv(
    omit = 5, idNm = "id", xNm = c("x1", "x2"), yNm = c("y1", "y2"),
    pdNm = NA, data = dat, iter_max = 10, k = 3,
    method = "msn", method_impute = "closest"
  ))

  expect_true(is.data.frame(cv))
  expect_true("yy0.rmse" %in% names(cv))
  expect_true("yy0.rsq" %in% names(cv))
})

test_that("yai_cv returns an all-NA placeholder for small data", {
  skip_if_not_installed("yaImpute")
  skip_if_not_installed("plyr")

  cv <- suppressWarnings(yai_cv(
    omit = 5, idNm = "id", xNm = c("x1", "x2"), yNm = c("y1", "y2"),
    pdNm = NA, data = make_knn_ref(10), min_rows = 15, iter_max = 10, k = 3,
    method = "msn", method_impute = "closest"
  ))

  expect_true(is.data.frame(cv))
  expect_identical(cv$n[1], nrow(make_knn_ref(10)))
  expect_true(all(is.na(cv$bias)))
  expect_true(all(is.na(cv$rmse)))
  expect_true(all(is.na(cv$rsq)))
})
