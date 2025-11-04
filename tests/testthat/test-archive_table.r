# tests/testthat/test-archive_table.R
# testthat edition 3

# Helper: find the environment where archive_table() is defined so we can mock there.
.get_archive_env <- function() {
  if (!exists("archive_table", mode = "function")) {
    testthat::skip("archive_table() is not available on the search path. Source it or load your package first.")
  }
  environment(get("archive_table", mode = "function"))
}

testthat::test_that("CSV write succeeds with minimal setup (no globals)", {
  testthat::skip_on_cran()
  td = withr::local_tempdir()
  path_out = file.path(td, "mydata")
  dat = data.frame(a = 1:3, b = letters[1:3])

  env_arch = .get_archive_env()

  testthat::with_mocked_bindings(
    file_version = function(path, increment = TRUE) {
      dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
      path
    },
    .env = env_arch,
  {
    res = archive_table(
      data = dat,
      path_out = path_out,
      do_csv = TRUE,
      do_rds = FALSE,
      do_sqlite = FALSE,
      do_xlsx = FALSE
    )
    testthat::expect_true(res$csv$success)
    testthat::expect_true(file.exists(res$csv$path))
  })
})

testthat::test_that("RDS write succeeds and file can be read back", {
  testthat::skip_on_cran()
  td = withr::local_tempdir()
  path_out = file.path(td, "mydata")
  dat = data.frame(x = 1:5)

  env_arch = .get_archive_env()

  testthat::with_mocked_bindings(
    file_version = function(path, increment = TRUE) {
      dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
      path
    },
    .env = env_arch,
  {
    res = archive_table(
      data = dat,
      path_out = path_out,
      do_csv = FALSE,
      do_rds = TRUE,
      do_sqlite = FALSE,
      do_xlsx = FALSE
    )
    testthat::expect_true(res$rds$success)
    testthat::expect_true(file.exists(res$rds$path))
    dat2 = readRDS(res$rds$path)
    testthat::expect_identical(dat2, dat)
  })
})

testthat::test_that("XLSX write sanitizes list/matrix/POSIXlt and applies options", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("openxlsx")

  td = withr::local_tempdir()
  path_out = file.path(td, "xlsx_case")
  # Build problematic columns
  lt = list(1, 2:3, 4)
  mx = cbind(m1 = 1:3, m2 = 4:6)
  t_lt = as.POSIXlt(Sys.time())
  t_vec = rep(t_lt, 3)
  dat = data.frame(id = 1:3)
  dat$lst = lt
  dat$mx = I(split(mx, row(mx))) # matrix-like per-row
  dat$when = t_vec

  env_arch = .get_archive_env()

  testthat::with_mocked_bindings(
    file_version = function(path, increment = TRUE) {
      dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
      path
    },
    .env = env_arch,
  {
    res = archive_table(
      data = dat,
      path_out = path_out,
      do_csv = FALSE,
      do_rds = FALSE,
      do_sqlite = FALSE,
      do_xlsx = TRUE,
      xlsx_collapse_lists = "auto",
      xlsx_matrix_collapse = "comma",
      xlsx_time_tz = "UTC",
      xlsx_datetime_format = "yyyy-mm-dd hh:mm"
    )
    testthat::expect_true(res$xlsx$success)
    testthat::expect_true(file.exists(res$xlsx$path))
    wb = openxlsx::loadWorkbook(res$xlsx$path)
    testthat::expect_true(length(openxlsx::sheets(wb)) >= 1)
  })
})

testthat::test_that("SQLite write succeeds with sf object when sf available", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("sf")

  td = withr::local_tempdir()
  path_out = file.path(td, "sqlite_case")

  # minimal sf point layer
  sf = getNamespace("sf")
  sfg = sf::st_sfc(sf::st_point(c(-120.5, 47.1)), sf::st_point(c(-120.6, 47.2)), crs = 4326)
  dat_sf = sf::st_sf(data.frame(id = 1:2), geometry = sfg)

  env_arch = .get_archive_env()

  testthat::with_mocked_bindings(
    file_version = function(path, increment = TRUE) {
      dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
      path
    },
    .env = env_arch,
  {
    res = archive_table(
      data = dat_sf,
      path_out = path_out,
      do_csv = FALSE,
      do_rds = FALSE,
      do_sqlite = TRUE,
      do_xlsx = FALSE,
      table_nm = "points"
    )
    testthat::expect_true(res$sqlite$success)
    testthat::expect_true(file.exists(res$sqlite$path))
  })
})

testthat::test_that("stop_on_error = FALSE isolates failures; TRUE fails fast", {
  testthat::skip_on_cran()
  td = withr::local_tempdir()
  path_out = file.path(td, "iso_case")
  dat = data.frame(a = 1:2)
  env_arch = .get_archive_env()

  # Isolate: CSV fails, RDS succeeds (stop_on_error = FALSE)
  testthat::with_mocked_bindings(
    file_version = function(path, increment = TRUE) {
      dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE); path
    },
    .env = env_arch,
  {
    testthat::with_mocked_bindings(
      `write.csv` = function(...) stop("csv write failed (mock)"),
      .package = "utils",
    {
      res = archive_table(
        data = dat,
        path_out = path_out,
        do_csv = TRUE,
        do_rds = TRUE,
        do_sqlite = FALSE,
        do_xlsx = FALSE,
        stop_on_error = FALSE
      )
      testthat::expect_false(res$csv$success)
      testthat::expect_match(res$csv$error, "csv write failed")
      testthat::expect_true(res$rds$success)
    })
  })

  # Fail-fast: stop_on_error = TRUE
  testthat::with_mocked_bindings(
    file_version = function(path, increment = TRUE) {
      dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE); path
    },
    .env = env_arch,
  {
    testthat::with_mocked_bindings(
      `write.csv` = function(...) stop("csv write failed (mock)"),
      .package = "utils",
    {
      testthat::expect_error(
        archive_table(
          data = dat,
          path_out = path_out,
          do_csv = TRUE,
          do_rds = TRUE,
          do_sqlite = FALSE,
          do_xlsx = FALSE,
          stop_on_error = TRUE
        ),
        regexp = "csv write failed"
      )
    })
  })
})

testthat::test_that("use_subdirs logic builds base_path correctly", {
  testthat::skip_on_cran()
  td = withr::local_tempdir()
  base = file.path(td, "abc", "def")
  dat = data.frame(a = 1:2)
  env_arch = .get_archive_env()

  testthat::with_mocked_bindings(
    file_version = function(path, increment = TRUE) {
      dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE); path
    },
    .env = env_arch,
  {
    res = archive_table(
      data = dat,
      path_out = base,
      do_csv = TRUE,
      do_rds = FALSE,
      do_sqlite = FALSE,
      do_xlsx = FALSE,
      use_subdirs = TRUE
    )
    expected_dir = file.path(base, basename(base))
    testthat::expect_true(grepl(expected_dir, res$csv$path, fixed = TRUE))
  })
})

testthat::test_that("duplicate column names are normalized in CSV output", {
  testthat::skip_on_cran()
  td = withr::local_tempdir()
  path_out = file.path(td, "dupcols")
  dat = data.frame(A = 1:3, a = 4:6, B = 7:9, b = 10:12, check.names = FALSE)
  env_arch = .get_archive_env()

  testthat::with_mocked_bindings(
    file_version = function(path, increment = TRUE) {
      dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE); path
    },
    .env = env_arch,
  {
    res = archive_table(
      data = dat,
      path_out = path_out,
      do_csv = TRUE,
      do_rds = FALSE,
      do_sqlite = FALSE,
      do_xlsx = FALSE
    )
    testthat::expect_true(res$csv$success)
    got = utils::read.csv(res$csv$path, check.names = FALSE)
    testthat::expect_identical(colnames(got), c("A", "a.2", "B", "b.2"))
  })
})
