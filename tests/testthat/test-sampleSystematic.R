library(testthat)

# Tests for R/sampleSystematic.R.

testthat::test_that("sampleSystematic returns n sorted, unique, in-range indices", {
  set.seed(42)
  s <- sampleSystematic(2000, 25)
  testthat::expect_equal(length(s), 25)
  testthat::expect_true(all(s >= 1 & s <= 2000))
  testthat::expect_false(as.logical(anyDuplicated(s)))
  testthat::expect_false(is.unsorted(s))
})

testthat::test_that("sampleSystematic as_idx=FALSE returns a logical mask", {
  set.seed(1)
  s3 <- sampleSystematic(100, 10, as_idx = FALSE)
  testthat::expect_true(is.logical(s3))
  testthat::expect_equal(length(s3), 100)
  testthat::expect_equal(sum(s3), 10)
})

testthat::test_that("sampleSystematic multi-group assigns group ids", {
  # FAILS at baseline: L64 calls the non-existent function sample.systematic
  # ('could not find function "sample.systematic"'), so the multi-group branch
  # errors instead of returning a grouped assignment vector.
  s <- sampleSystematic(205, n = c(50, 50))
  testthat::expect_equal(length(s), 205)
  testthat::expect_true(all(s %in% 0:2))
  testthat::expect_equal(sum(s == 1), 50)
  testthat::expect_equal(sum(s == 2), 50)
})

testthat::test_that("sampleSystematic never trips the browser() safety net", {
  testthat::expect_silent(
    for (sd in 1:50) {
      set.seed(sd)
      sampleSystematic(50, 10)
    }
  )
})
