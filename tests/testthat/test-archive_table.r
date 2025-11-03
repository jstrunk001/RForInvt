# test-archive_table.R
# Run with: devtools::test() or testthat::test_dir("tests/testthat")

testthat::test_that("archive_table: basic CSV & RDS writes succeed and report status", {
  testthat::skip_on_cran()
  testthat::skip_on_ci()  # Optional, if file I/O is flaky on CI
  # --- Arrange ---
  td = withr::local_tempdir()
  path_out = file.path(td, "mydata")
  data = data.frame(a = 1:3, b = c("x", "y", "z"))

  # Mock file_version to be identity (no suffixing). Keeps path assertions simple.
  testthat::with_mocked_bindings(
    file_version = function(p, increment = TRUE) p,
  {
    # --- Act ---
    res = archive_table(
      data = data,
      path_out = path_out,
      do_csv = TRUE,
      do_rds = TRUE,
      do_sqlite = FALSE,
      do_xlsx = FALSE,
      use_subdirs = FALSE,
      increment = FALSE,
      stop_on_error = FALSE
    )

    # --- Assert ---
    testthat::expect_true(is.list(res))
    testthat::expect_true(res$csv$success)
    testthat::expect_true(res$rds$success)
    testthat::expect_null(res$sqlite)
    testthat::expect_null(res$xlsx)
    testthat::expect_true(file.exists(res$csv$path))
    testthat::expect_true(file.exists(res$rds$path))
  })
})

testthat::test_that("archive_table: subdirectories are created when use_subdirs = TRUE", {
  testthat::skip_on_cran()

  td = withr::local_tempdir()
  path_out = file.path(td, "base/mydata")  # nested base dir
  dir.create(dirname(path_out), recursive = TRUE, showWarnings = FALSE)
  data = data.frame(a = 1:2)

  testthat::with_mocked_bindings(
    file_version = function(p, increment = TRUE) p,
  {
    res = archive_table(
      data = data,
      path_out = path_out,
      do_csv = TRUE,
      do_rds = TRUE,
      do_sqlite = FALSE,
      do_xlsx = FALSE,
      use_subdirs = TRUE,
      increment = FALSE
    )

    csv_dir = file.path(dirname(path_out), "csv")
    rds_dir = file.path(dirname(path_out), "rds")
    testthat::expect_true(dir.exists(csv_dir))
    testthat::expect_true(dir.exists(rds_dir))
    testthat::expect_true(grepl(file.path(csv_dir, "mydata[.]csv$"), res$csv$path))
    testthat::expect_true(grepl(file.path(rds_dir, "mydata[.]RDS$"), res$rds$path))
  })
})

testthat::test_that("archive_table: duplicate column names are normalized (case-insensitive)", {
  testthat::skip_on_cran()

  td = withr::local_tempdir()
  path_out = file.path(td, "mydata")

  data = data.frame(A = 1:3, a = 4:6, B = 7:9, b = 10:12, check.names = FALSE)

  testthat::with_mocked_bindings(
    file_version = function(p, increment = TRUE) p,
  {
    res = archive_table(
      data = data,
      path_out = path_out,
      do_csv = TRUE,
      do_rds = FALSE,
      do_sqlite = FALSE,
      do_xlsx = FALSE,
      increment = FALSE
    )

    # Read CSV back and inspect column names
    got = utils::read.csv(res$csv$path, stringsAsFactors = FALSE, check.names = FALSE)
    # Expected behavior: first instance keeps original, subsequent duplicates get .2, .3, etc.
    # Original order was A, a, B, b
    testthat::expect_identical(colnames(got), c("A", "a.2", "B", "b.2"))
  })
})

testthat::test_that("archive_table: XLSX write and sheet name clipping to 30 chars", {
  testthat::skip_on_cran()
  if (!requireNamespace("openxlsx", quietly = TRUE)) testthat::skip("openxlsx not available")

  td = withr::local_tempdir()
  path_out = file.path(td, "mydata")
  data = data.frame(a = 1:3)
  long_tbl = paste(rep("VeryLongTableNameSegment", 3), collapse = "_")  # >> 30 chars
  testthat::with_mocked_bindings(
    file_version = function(p, increment = TRUE) p,
  {
    res = archive_table(
      data = data,
      path_out = path_out,
      do_csv = FALSE,
      do_rds = FALSE,
      do_sqlite = FALSE,
      do_xlsx = TRUE,
      table_nm = long_tbl,
      increment = FALSE
    )

    testthat::expect_true(res$xlsx$success)
    testthat::expect_true(file.exists(res$xlsx$path))

    # Check sheet name length = 30
    wb = openxlsx::loadWorkbook(res$xlsx$path)
    sheets = openxlsx::sheets(wb)
    testthat::expect_true(length(sheets) == 1)
    testthat::expect_true(nchar(sheets[1]) <= 30)
  })
})

testthat::test_that("archive_table: SQLite write skipped with clear error when data is not sf", {
  testthat::skip_on_cran()

  td = withr::local_tempdir()
  path_out = file.path(td, "mydata")
  data = data.frame(a = 1:3)

  testthat::with_mocked_bindings(
    file_version = function(p, increment = TRUE) p,
  {
    res = archive_table(
      data = data,
      path_out = path_out,
      do_csv = FALSE,
      do_rds = FALSE,
      do_sqlite = TRUE,
      do_xlsx = FALSE,
      increment = FALSE,
      stop_on_error = FALSE
    )

    testthat::expect_false(res$sqlite$success)
    testthat::expect_true(grepl("requires `data` to be an `sf` object", res$sqlite$error))
  })
})

testthat::test_that("archive_table: SQLite write succeeds for sf data (if sf available)", {
  testthat::skip_on_cran()
  if (!requireNamespace("sf", quietly = TRUE)) testthat::skip("sf not available")

  td = withr::local_tempdir()
  path_out = file.path(td, "geo/mydata")
  dir.create(dirname(path_out), recursive = TRUE, showWarnings = FALSE)

  # Build a tiny sf object
  coords = matrix(c(-120.1, 47.1,
                    -120.2, 47.2), ncol = 2, byrow = TRUE)
  sfg = sf::st_sfc(sf::st_point(coords[1, ]), sf::st_point(coords[2, ]), crs = 4326)
  data = sf::st_sf(data.frame(id = 1:2), geometry = sfg)

  testthat::with_mocked_bindings(
    file_version = function(p, increment = TRUE) p,
  {
    res = archive_table(
      data = data,
      path_out = path_out,
      do_csv = FALSE,
      do_rds = FALSE,
      do_sqlite = TRUE,
      do_xlsx = FALSE,
      increment = FALSE,
      table_nm = "points"
    )

    testthat::expect_true(res$sqlite$success)
    testthat::expect_true(file.exists(res$sqlite$path))
  })
})

testthat::test_that("archive_table: stop_on_error = TRUE fails fast on first writer error (CSV mocked)", {
  testthat::skip_on_cran()

  td = withr::local_tempdir()
  path_out = file.path(td, "mydata")
  data = data.frame(a = 1:2)

  # Mock file_version and write.csv to force a CSV failure
  testthat::with_mocked_bindings(
    file_version = function(p, increment = TRUE) p,
    write.csv = function(...) stop("csv write failed (mock)"),
  {
    testthat::expect_error(
      archive_table(
        data = data,
        path_out = path_out,
        do_csv = TRUE,
        do_rds = TRUE,
        do_sqlite = FALSE,
        do_xlsx = FALSE,
        increment = FALSE,
        stop_on_error = TRUE
      ),
      regexp = "csv write failed"
    )
  })
})

testthat::test_that("archive_table: with stop_on_error = FALSE a failing writer does not block others", {
  testthat::skip_on_cran()

  td = withr::local_tempdir()
  path_out = file.path(td, "mydata")
  data = data.frame(a = 1:2)

  # Fail CSV but allow RDS to succeed.
  testthat::with_mocked_bindings(
    file_version = function(p, increment = TRUE) p,
    write.csv = function(...) stop("csv write failed (mock)"),
  {
    res = archive_table(
      data = data,
      path_out = path_out,
      do_csv = TRUE,
      do_rds = TRUE,
      do_sqlite = FALSE,
      do_xlsx = FALSE,
      increment = FALSE,
      stop_on_error = FALSE
    )

    testthat::expect_false(res$csv$success)
    testthat::expect_true(grepl("csv write failed", res$csv$error))
    testthat::expect_true(res$rds$success)
    testthat::expect_true(file.exists(res$rds$path))
  })
})

testthat::test_that("archive_table: strips known extensions off path_out", {
  testthat::skip_on_cran()

  td = withr::local_tempdir()
  # Intentionally include extension in path_out; should be stripped
  path_out = file.path(td, "mydata.xlsx")
  data = data.frame(a = 1:2)

  testthat::with_mocked_bindings(
    file_version = function(p, increment = TRUE) p,
  {
    res = archive_table(
      data = data,
      path_out = path_out,
      do_csv = TRUE,
      do_rds = TRUE,
      do_sqlite = FALSE,
      do_xlsx = FALSE,
      increment = FALSE
    )

    testthat::expect_true(grepl("[.]csv$", res$csv$path))
    testthat::expect_true(grepl("[.]RDS$", res$rds$path))
    # And base name should be "mydata" not "mydata.xlsx"
    testthat::expect_true(grepl("mydata[.]csv$", basename(res$csv$path)))
    testthat::expect_true(grepl("mydata[.]RDS$", basename(res$rds$path)))
  })
})
