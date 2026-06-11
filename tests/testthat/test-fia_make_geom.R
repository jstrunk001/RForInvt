library(testthat)

# Tests for R/fia_make_geom.R. WORKS at the pre-refactor baseline. Requires sf.

test_that("fia_make_geom builds subplot/plot geometry from xy centers", {
  testthat::skip_if_not_installed("sf")

  dat <- data.frame(PLOT = 1:5, x = (101:105) * 10000, y = (101:105) * 10000)
  res <- fia_make_geom(dat)

  testthat::expect_type(res, "list")

  # 4 subplots per plot -> 4 * 5 = 20 rows
  sub_df <- res[["SUBPLOT data.frame"]]
  testthat::expect_s3_class(sub_df, "data.frame")
  testthat::expect_equal(nrow(sub_df), 20L)

  # subplot points are an sf object
  testthat::expect_true(inherits(res[["SUBPLOT POINT"]], "sf"))

  # one combined feature per plot
  testthat::expect_equal(nrow(res[["PLOT POINT"]]), 5L)
})

test_that("fia_make_geom honors the output argument for selective generation", {
  testthat::skip_if_not_installed("sf")

  dat <- data.frame(PLOT = 1:5, x = (101:105) * 10000, y = (101:105) * 10000)

  # only the data.frame requested
  res1 <- fia_make_geom(dat, output = "SUBPLOT data.frame")
  testthat::expect_named(res1, "SUBPLOT data.frame")
  testthat::expect_length(res1, 1L)

  # subplot data.frame + points only
  res2 <- fia_make_geom(dat, output = c("SUBPLOT data.frame", "SUBPLOT POINT"))
  testthat::expect_setequal(names(res2), c("SUBPLOT data.frame", "SUBPLOT POINT"))
  testthat::expect_true(inherits(res2[["SUBPLOT POINT"]], "sf"))
})
