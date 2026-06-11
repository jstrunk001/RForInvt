library(testthat)

# Tests for the tree-level compilation helpers in R/compile_trees.R.
# All behaviors below WORK at the pre-refactor baseline.

test_that("ba_ft adds ba_ft = 0.005454 * dbh^2", {
  x <- ba_ft(data.frame(dbh = c(10, 20)), tree_nms = c(dbh = "dbh"))
  testthat::expect_true("ba_ft" %in% names(x))
  testthat::expect_equal(x$ba_ft, c(0.5454, 2.1816))

  x2 <- ba_ft(data.frame(dbh = c(30, 5, 15)), tree_nms = c(dbh = "dbh"))
  testthat::expect_equal(x2$ba_ft, c(4.9086, 0.13635, 1.22715))
})

test_that("tpa computes TPA = (nstems or 1) / acres", {
  x <- tpa(data.frame(acres = 0.1), tree_nms = c(acres = "acres"))
  testthat::expect_true("TPA" %in% names(x))
  testthat::expect_equal(x$TPA, 10)

  x2 <- tpa(
    data.frame(acres = 0.1, nstems = 2),
    tree_nms = c(acres = "acres", nstems = "nstems")
  )
  testthat::expect_equal(x2$TPA, 20)
})

test_that("tph computes the metric analogue (value 25) and names the column TPH", {
  x <- tph(data.frame(hectares = 0.04), tree_nms = c(hectares = "hectares"))
  # tph divides by hectares: 1 / 0.04 == 25
  testthat::expect_equal(x$TPH, 25)
  # Fixed behavior: output column is named "TPH", not "TPA".
  testthat::expect_true("TPH" %in% names(x))
  testthat::expect_false("TPA" %in% names(x))
})

test_that("dbcl assigns the diameter-class midpoint of the break interval", {
  x <- dbcl(
    data.frame(dbh = c(2, 18, 40, 60)),
    tree_nms = c(dbh = "dbh", dbcl = "dbcl")
  )
  testthat::expect_equal(x$dbcl, c(2, 18, 41, 525))

  x2 <- dbcl(
    data.frame(dbh = c(6, 10)),
    tree_nms = c(dbh = "dbh", dbcl = "dbcl")
  )
  testthat::expect_equal(x2$dbcl, c(6, 10))
})

test_that("dbcl_y spreads a grouped variable across diameter classes", {
  testthat::skip_if_not_installed("reshape2")

  df <- data.frame(id = 1:4, dbh = c(5, 5, 15, 15), spp = c("a", "b", "a", "b"))
  df <- ba_ft(df, tree_nms = c(dbh = "dbh"))
  df <- dbcl(df, tree_nms = c(dbh = "dbh", dbcl = "dbcl"))
  # confirm fixture diameter classes
  testthat::expect_equal(df$dbcl, c(6, 6, 14, 14))

  res <- dbcl_y(
    df,
    tree_nms = c(tree_ids = "id", dbh = "dbh", dbcl = "dbcl"),
    vars_group = "ba_ft"
  )
  testthat::expect_true(all(c("ba_ft_dbcl6", "ba_ft_dbcl14") %in% names(res)))

  # value lands in own class, NA elsewhere
  testthat::expect_equal(res$ba_ft_dbcl6, c(0.13635, 0.13635, NA, NA))
  testthat::expect_equal(res$ba_ft_dbcl14, c(NA, NA, 1.22715, 1.22715))
})

test_that("spp_y spreads a grouped variable across species", {
  testthat::skip_if_not_installed("reshape2")

  df <- data.frame(id = 1:4, dbh = c(5, 5, 15, 15), spp = c("a", "b", "a", "b"))
  df <- ba_ft(df, tree_nms = c(dbh = "dbh"))
  df <- dbcl(df, tree_nms = c(dbh = "dbh", dbcl = "dbcl"))

  res <- spp_y(
    df,
    tree_nms = c(tree_ids = "id", dbh = "dbh", dbcl = "dbcl", spp = "spp"),
    vars_group = "ba_ft"
  )
  testthat::expect_true(all(c("ba_ft_spp_a", "ba_ft_spp_b") %in% names(res)))

  # rows for spp "a" (ids 1,3) carry ba_ft in ba_ft_spp_a; "b" rows NA there.
  testthat::expect_equal(res$ba_ft_spp_a, c(0.13635, NA, 1.22715, NA))
  testthat::expect_equal(res$ba_ft_spp_b, c(NA, 0.13635, NA, 1.22715))
})

test_that("dbcl_spp_y spreads a grouped variable across species x dbcl crosses", {
  testthat::skip_if_not_installed("reshape2")

  df <- data.frame(id = 1:4, dbh = c(5, 5, 15, 15), spp = c("a", "b", "a", "b"))
  df <- ba_ft(df, tree_nms = c(dbh = "dbh"))
  df <- dbcl(df, tree_nms = c(dbh = "dbh", dbcl = "dbcl"))

  res <- dbcl_spp_y(
    df,
    tree_nms = c(tree_ids = "id", dbh = "dbh", dbcl = "dbcl", spp = "spp"),
    vars_group = "ba_ft"
  )
  testthat::expect_true("ba_ft_spp_a_dbcl_6" %in% names(res))

  # id 1 is spp a, dbcl 6 -> value in ba_ft_spp_a_dbcl_6; others NA there.
  testthat::expect_equal(res$ba_ft_spp_a_dbcl_6, c(0.13635, NA, NA, NA))
})

test_that("compile_trees applies a list of functions, preserving row count", {
  df <- data.frame(
    plot = c(1, 1, 2, 2, 3, 3),
    id = 1:6,
    dbh = c(5, 10, 15, 20, 30, 15),
    spp = c("a", "b", "a", "b", "a", "b"),
    acres = 0.1
  )
  res <- compile_trees(
    df,
    tree_nms = c(tree_ids = "id", dbh = "dbh", dbcl = "dbcl", spp = "spp", acres = "acres"),
    fns_compute = list(tpa, ba_ft, dbcl),
    vars_group = "ba_ft"
  )
  testthat::expect_s3_class(res, "data.frame")
  testthat::expect_equal(nrow(res), nrow(df))
  testthat::expect_true(all(c("TPA", "ba_ft", "dbcl") %in% names(res)))
  testthat::expect_equal(res$ba_ft, 0.005454 * res$dbh^2)
})
