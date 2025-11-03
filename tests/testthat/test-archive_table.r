
# test-archive_table.R
library(testthat)
library(sf)
library(openxlsx)

test_that("archive_table creates all specified files", {
  # Temporary directory for testing
  test_dir <- tempdir()
  test_path <- file.path(test_dir, "test_data")

  # Example data frame
  df <- data.frame(
    id = 1:5,
    name = c("A", "B", "C", "D", "E"),
    value = runif(5)
  )

  # Example sf object for SQLite test
  sf_data <- st_as_sf(df, coords = c("id", "value"), crs = 4326)

  # ---- TEST 1: All formats ----
  files_all <- archive_table(df, test_path, table_nm = "Sheet1")
  expect_true(all(file.exists(unlist(files_all))))
  expect_named(files_all, c("csv", "rds", "sqlite", "xlsx"))

  # ---- TEST 2: Only CSV and XLSX ----
  files_csv_xlsx <- archive_table(df, test_path,
                                  do_rds = FALSE,
                                  do_sqlite = FALSE,
                                  table_nm = "MySheet")
  expect_true(file.exists(files_csv_xlsx$csv))
  expect_true(file.exists(files_csv_xlsx$xlsx))
  expect_false("rds" %in% names(files_csv_xlsx))
  expect_false("sqlite" %in% names(files_csv_xlsx))

  # ---- TEST 3: SQLite only with sf object ----
  files_sqlite <- archive_table(sf_data, test_path,
                                do_csv = FALSE,
                                do_rds = FALSE,
                                do_xlsx = FALSE,
                                table_nm = "SpatialTable")
  expect_true(file.exists(files_sqlite$sqlite))
  expect_equal(names(files_sqlite), "sqlite")

  # ---- TEST 4: File paths are unique and versioned ----
  all_paths <- unlist(c(files_all, files_csv_xlsx, files_sqlite))
  expect_true(length(unique(all_paths)) == length(all_paths))
})
