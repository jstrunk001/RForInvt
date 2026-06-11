library(testthat)

# Tests for R/pairs2.R. The internal helpers .plot_ij / .plot2_ij are
# exercised indirectly through pairs2(). Graphics are routed to a temp pdf.

testthat::test_that("pairs2 draws non-combination pair plots without error", {
  pf <- tempfile(fileext = ".pdf")
  pdf(pf)
  testthat::expect_silent(pairs2(mtcars[, 1:4], mtcars[, 5:8], combination = FALSE))
  dev.off()
  testthat::expect_gt(file.info(pf)$size, 0)
})

testthat::test_that("pairs2 draws combination pair plots without error", {
  pf <- tempfile(fileext = ".pdf")
  pdf(pf)
  testthat::expect_silent(pairs2(mtcars[, 1:3], mtcars[, 1:3], combination = TRUE))
  dev.off()
  testthat::expect_gt(file.info(pf)$size, 0)
})
