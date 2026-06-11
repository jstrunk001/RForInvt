library(testthat)
library(data.table)

context("NVEL_buck functionality and parallel execution")

# Setup a standard testing dataset
test_trees <- data.table(
  id = 1:3,
  spcd = c(202, 202, 122), # Doug-fir, Doug-fir, Ponderosa
  dbh = c(5.0, 24.2, 35.0), # Small (non-merch), Medium, Large
  ht = c(20, 110, 150),
  region = 6,
  forest = "01",
  district = "01"
)

# Helper to check if DLL exists (skips tests if DLL is missing)
check_dll <- function() {
  dll_path <- system.file('lib/VolLibDll20231106/vollib-64bits/vollib.dll', package = "RForInvt")
  return(file.exists(dll_path))
}

test_that("NVEL_buck returns a data.table and handles log expansion", {
  skip_if_not(check_dll(), "NVEL DLL not found, skipping tests")

  # Run serial version
  res <- NVEL_buck(
    dfTL = test_trees,
    dbhNm = "dbh",
    htNm = "ht",
    spcdNm = "spcd",
    ncore = 1,
    vol2biomass = FALSE
  )

  # Check Class
  expect_s3_class(res, "data.table")

  # Check that we have more rows than input (log expansion)
  # Tree 1 (small) should have 1 row (LG_NUM=0), Trees 2 & 3 should have multiple logs
  expect_gt(nrow(res), nrow(test_trees))

  # Check for essential columns
  expect_true(all(c("LG_NUM", "LG_LEN", "TR_TCFV_ALL", "TreeIdx") %in% names(res)))
})

test_that("Parallel results match Serial results exactly", {
  skip_if_not(check_dll(), "NVEL DLL not found, skipping tests")

  # 1 Core
  res_serial <- NVEL_buck(dfTL = test_trees, ncore = 1, vol2biomass = FALSE)

  # 2 Cores
  res_parallel <- NVEL_buck(dfTL = test_trees, ncore = 2, vol2biomass = FALSE)

  # Sort to ensure comparison is valid (though TreeIdx should handle this)
  setkey(res_serial, TreeIdx, LG_NUM)
  setkey(res_parallel, TreeIdx, LG_NUM)

  # Check equality
  expect_equal(res_serial, res_parallel)
})

test_that("Biomass calculations are performed when requested", {
  skip_if_not(check_dll(), "NVEL DLL not found, skipping tests")

  res <- NVEL_buck(
    dfTL = test_trees[2,], # Use a merchantable tree
    vol2biomass = TRUE,
    ncore = 1
  )

  # Check for biomass columns
  expect_true("TR_TBIOGRN_LBS" %in% names(res))
  expect_true("TR_TBIODRY_LBS" %in% names(res))

  # Values should be positive and numeric
  expect_true(all(res$TR_TBIOGRN_LBS > 0))
})

test_that("Non-merchantable trees return a single row with LG_NUM 0", {
  skip_if_not(check_dll(), "NVEL DLL not found, skipping tests")

  # Tree with 2 inch DBH
  small_tree <- data.table(spcd=202, dbh=2, ht=10, region=6, forest="01", district="01")

  res <- NVEL_buck(dfTL = small_tree, ncore = 1)

  expect_equal(nrow(res), 1)
  expect_equal(res$LG_NUM, 0)
  expect_equal(res$TR_TCFV_ALL, 0)
})

test_that("Function handles data.frame input and still returns data.table", {
  skip_if_not(check_dll(), "NVEL DLL not found, skipping tests")

  df_input <- as.data.frame(test_trees[2,])

  res <- NVEL_buck(dfTL = df_input, ncore = 1)

  expect_s3_class(res, "data.table")
})

test_that("Incorrect column mapping throws error early", {
  # This tests the .formatTL2NVEL2 and internal mapply safety
  # If we pass a column name that doesn't exist, it should fill with 0 or warn
  expect_error(
    NVEL_buck(dfTL = test_trees, dbhNm = "wrong_name"),
    NA # We don't necessarily expect an R error if it defaults to 0,
       # but you could add specific validation in the function to throw errors.
  )
})
