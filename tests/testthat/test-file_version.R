library(testthat)

# Tests for R/file_version.R:
#   .version_name (internal), file_version (public), .update_paths (internal)
#   .tracking_get is exercised indirectly through file_version().

testthat::test_that(".version_name builds versioned file names", {
  skip_if_not_installed("stringr")
  testthat::expect_equal(
    RForInvt:::.version_name(
      "c:/temp/somedata.csv",
      v_num = 1, digits = 4, stamp = "20230530",
      markers = c(version = "VS_", date = "DT_"), sep = "_"
    ),
    "c:/temp/somedata.csv/somedata_VS_0001_DT_20230530.csv"
  )
  testthat::expect_equal(
    RForInvt:::.version_name(
      "c:/temp/somedata.csv",
      v_num = 2, digits = 3, stamp = "STAMP",
      markers = c(version = "VS", date = ""), sep = "_", nm_simple = TRUE
    ),
    "c:/temp/somedata.csv/VS002_STAMP.csv"
  )
  testthat::expect_equal(
    RForInvt:::.version_name(
      "c:/temp/FVSTest",
      v_num = 1, digits = 3, stamp = "STAMP",
      markers = c(version = "VS", date = ""), sep = "_"
    ),
    "c:/temp/FVSTest/FVSTest_VS001_STAMP"
  )
})

testthat::test_that("file_version increments versions and tracks them", {
  skip_if_not_installed("plyr")
  skip_if_not_installed("stringr")

  p <- file.path(tempdir(), paste0("fv_", as.integer(runif(1, 1, 1e9))))

  p1 <- file_version(file.path(p, "data.txt"), increment = TRUE,
                     note = "first", version_stamp = "S1")
  # must materialise the file so purge logic keeps the row on the next call
  writeLines("x", p1)
  testthat::expect_equal(basename(p1), "data_VS001_S1.txt")

  p2 <- file_version(file.path(p, "data.txt"), increment = TRUE,
                     note = "second", version_stamp = "S2")
  writeLines("y", p2)
  testthat::expect_equal(basename(p2), "data_VS002_S2.txt")

  allv <- file_version(file.path(p, "data.txt"),
                       return_all_versions = TRUE,
                       purge_missing_versions = FALSE)
  testthat::expect_equal(nrow(allv), 2)
  testthat::expect_equal(sort(allv$id), c(1, 2))
})

testthat::test_that(".update_paths is a no-op without a tracking file", {
  d <- file.path(tempdir(), paste0("up_", as.integer(runif(1, 1, 1e9))))
  dir.create(d, recursive = TRUE)
  testthat::expect_silent(RForInvt:::.update_paths(d))
})
