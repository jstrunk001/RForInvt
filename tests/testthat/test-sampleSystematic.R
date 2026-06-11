library(testthat)

# Tests for R/sampleSystematic.R (function renamed sampleSystematic -> sample_systematic).

testthat::test_that("sample_systematic returns n sorted, unique, in-range indices", {
  set.seed(42)
  s <- sample_systematic(2000, 25)
  testthat::expect_equal(length(s), 25)
  testthat::expect_true(all(s >= 1 & s <= 2000))
  testthat::expect_false(as.logical(anyDuplicated(s)))
  testthat::expect_false(is.unsorted(s))
})

testthat::test_that("sample_systematic as_idx=FALSE returns a logical mask", {
  set.seed(1)
  s3 <- sample_systematic(100, 10, as_idx = FALSE)
  testthat::expect_true(is.logical(s3))
  testthat::expect_equal(length(s3), 100)
  testthat::expect_equal(sum(s3), 10)
})

testthat::test_that("sample_systematic multi-group assigns group ids", {
  # Now FIXED: the multi-group branch returns a grouped assignment vector of
  # length 205 with values in 0:2 and exactly 50 each in groups 1 and 2.
  s <- sample_systematic(205, n = c(50, 50))
  testthat::expect_equal(length(s), 205)
  testthat::expect_true(all(s %in% 0:2))
  testthat::expect_equal(sum(s == 1), 50)
  testthat::expect_equal(sum(s == 2), 50)
})

testthat::test_that("sample_systematic never trips the browser() safety net", {
  testthat::expect_silent(
    for (sd in 1:50) {
      set.seed(sd)
      sample_systematic(50, 10)
    }
  )
})
