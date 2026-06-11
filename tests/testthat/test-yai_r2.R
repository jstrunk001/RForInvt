library(testthat)

# Tests for yai_r2() in R/yai_r2.R. The roxygen @examples there are
# non-runnable (reference undefined dat6/vEnvironment1/etc.), so we build our
# own fixture from make_knn_ref().

# Build a yai object whose row names are the record ids, then impute observed
# values. yai_r2() accepts either the impute() data.frame or the yai object
# directly.
.make_yai_fixture <- function() {
  d <- make_knn_ref()
  rownames(d) <- d$id
  yaImpute::yai(
    x = d[, c("x1", "x2")],
    y = d[, c("y1", "y2")],
    method = "euclidean", k = 3
  )
}

test_that("yai_r2 returns an R2_in data.frame keyed by response variable", {
  skip_if_not_installed("yaImpute")

  x <- .make_yai_fixture()
  imp <- yaImpute::impute(x, observed = TRUE)

  r <- yai_r2(imp)

  expect_true(is.data.frame(r))
  expect_identical(names(r), "R2_in")
  expect_identical(rownames(r), c("y1", "y2"))
  expect_true(is.numeric(r$R2_in))
  expect_true(all(r$R2_in <= 1))
})

test_that("yai_r2 errors when x is missing", {
  skip_if_not_installed("yaImpute")

  expect_error(yai_r2(), "x required")
})

test_that("yai_r2 gives the same result for a yai object and its impute df", {
  skip_if_not_installed("yaImpute")

  x <- .make_yai_fixture()
  imp <- yaImpute::impute(x, observed = TRUE)

  r_df  <- yai_r2(imp)
  r_yai <- yai_r2(x)

  expect_equal(r_yai, r_df)
})
