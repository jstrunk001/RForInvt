library(testthat)
library(plyr)

# Mock DLL paths for the test environment if needed
# dll_path <- "path/to/your/vollib.dll"

test_that("NVEL_buck returns correct column prefixes and structure", {
  # 1. Setup minimal valid input
  df_test <- data.frame(
    trid = 1,
    region = 6,
    forest = "01",
    district = "01",
    spcd = 202,
    dbh = 20,
    ht = 100
  )

  res <- NVEL_buck(dfTL = df_test, vol2biomass = TRUE)

  # Check that it returns a data.frame
  expect_s3_class(res, "data.frame")

  # Check for Log-level prefixes
  expect_true(any(grepl("^LG_", names(res))))

  # Check for Tree-level prefixes
  expect_true(any(grepl("^TR_", names(res))))

  # Check that original columns (trid) are preserved
  expect_true("trid" %in% names(res))
})

test_that("NVEL_buck handles non-merchantable trees (Zero Logs)", {
  # A 4-inch tree is usually below merchantable limits for most equations
  df_small <- data.frame(
    region = 6,
    forest = "01",
    district = "01",
    spcd = 202,
    dbh = 4.0,
    ht = 20
  )

  res <- NVEL_buck(dfTL = df_small)

  # Should return exactly 1 row with LG_NUM = 0
  expect_equal(nrow(res), 1)
  expect_equal(res$LG_NUM[1], 0)
  expect_equal(res$TR_NLOGS_ALL[1], 0)
})

test_that("NVEL_buck correctly expands a large tree into multiple logs", {
  # A 30-inch DBH, 150ft tree should definitely produce multiple logs
  df_large <- data.frame(
    region = 6,
    forest = "01",
    district = "01",
    spcd = 202,
    dbh = 30.0,
    ht = 150
  )

  res <- NVEL_buck(dfTL = df_large)

  # Should have more than 1 row
  expect_gt(nrow(res), 1)

  # LG_NUM should be sequential
  expect_equal(res$LG_NUM, 1:nrow(res))

  # Total height of logs should be less than or equal to total tree height
  expect_true(max(res$LG_HT) <= 150)
})

test_that("NVEL_buck biomass columns are added when vol2biomass is TRUE", {
  df_test <- data.frame(
    region = 6, forest = "01", district = "01",
    spcd = 202, dbh = 10, ht = 60
  )

  res_with <- NVEL_buck(dfTL = df_test, vol2biomass = TRUE)
  res_without <- NVEL_buck(dfTL = df_test, vol2biomass = FALSE)

  expect_true("TR_TBIOGRN_LBS" %in% names(res_with))
  expect_false("TR_TBIOGRN_LBS" %in% names(res_without))
})

test_that("NVEL_buck handles custom column names correctly", {
  # Input data with non-standard names
  df_custom <- data.frame(
    MY_SPECIES = 202,
    MY_DBH = 15.5,
    MY_HT = 90,
    region = 6,
    forest = "01",
    district = "01"
  )

  res <- NVEL_buck(
    dfTL = df_custom,
    spcdNm = "MY_SPECIES",
    dbhNm = "MY_DBH",
    htNm = "MY_HT"
  )

  expect_true(all(res$LG_NUM > 0))
  expect_true("MY_DBH" %in% names(res))
})

test_that("NVEL_buck voleq parameter overrides default equations", {
  df_test <- data.frame(
    region = 1, forest = "01", district = "01",
    spcd = 202, dbh = 12, ht = 80
  )

  # Force a specific Region 6 equation on a Region 1 tree
  forced_eq <- "F0162521"
  res <- NVEL_buck(dfTL = df_test, voleq = forced_eq)

  # In our revised formatTL2NVEL2, the voleqNm column (default "voleq")
  # should contain the forced value
  expect_equal(unique(res$voleq), forced_eq)
})
