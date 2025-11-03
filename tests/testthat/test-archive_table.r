# tests/testthat/test-archive_table.R
library(testthat)
library(sf)
library(openxlsx)

test_that("archive_table creates files in correct locations", {
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

  # ---- TEST 1: All formats without subdirs ----
  files_all <- archive_table(df, test_path, table_nm = "Sheet1", use_subdirs = FALSE)
  expect_true(all(file.exists(unlist(files_all))))
  expect_named(files_all, c("csv", "rds", "sqlite", "xlsx"))

  # ---- TEST 2: All formats with subdirs ----
  files_subdirs <- archive_table(df, test_path, table_nm = "Sheet1", use_subdirs = TRUE)
  expect_true(all(file.exists(unlist(files_subdirs))))
  # Check that each file is in its respective subdirectory
  expect_true(all(grepl("/csv/", files_subdirs$csv)))
  expect_true(all(grepl("/rds/", files_subdirs$rds)))
  expect_true(all(grepl("/sqlite/", files_subdirs$sqlite)))
  expect_true(all(grepl("/xlsx/", files_subdirs$xlsx)))

  # ---- TEST 3: Only CSV and XLSX ----
  files_csv_xlsx <- archive_table(df, test_path,
                                  do_rds = FALSE,
                                  do_sqlite = FALSE,
                                  use_subdirs = TRUE,
                                  table_nm = "MySheet")
  expect_true(file.exists(files_csv_xlsx$csv))
  expect_true(file.exists(files_csv_xlsx$xlsx))
  expect_false("rds" %in% names(files_csv_xlsx))
  expect_false("sqlite" %in% names(files_csv_xlsx))

  # ---- TEST 4: SQLite only with sf object ----
  files_sqlite <- archive_table(sf_data, test_path,
                                do_csv = FALSE,
                                do_rds = FALSE,
                                do_xlsx = FALSE,
                                use_subdirs = TRUE,
                                table_nm = "SpatialTable")
  expect_true(file.exists(files_sqlite$sqlite))
  expect_equal(names(files_sqlite), "sqlite")

  # ---- TEST 5: Argument type checks ----
  expect_error(archive_table("not_a_df", test_path), "is.data.frame")
  expect_error(archive_table(df, 123), "is.character")
  expect_error(archive_table(df, test_path, do_csv = "yes"), "is.logical")
  expect_error(archive_table(df, test_path, use_subdirs = "no"), "is.logical")
})

test_that("versioned filenames are unique", {
  test_dir <- tempdir()
  test_path <- file.path(test_dir, "test_data_unique")

  files1 <- archive_table(mtcars, test_path, increment = TRUE)
  files2 <- archive_table(mtcars, test_path, increment = TRUE)

  all_paths <- unlist(c(files1, files2))
  expect_true(length(unique(all_paths)) == length(all_paths))
})
