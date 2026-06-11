library(testthat)

# Tests for R/fia_clean_best_cds.R. Both behaviors WORK at the pre-refactor
# baseline. These require the sf package.

# build a small synthetic "best coordinates" data.frame with the columns the
# function reads: the 10 id_nms, plus PLOT, PC_COORD_METHOD_NAME,
# PC_LON_X/PC_LAT_Y and SP2/SP3/SP4 LON_X/LAT_Y.
make_df_best <- function(n = 4) {
  data.frame(
    INV_PLOT_ID = seq_len(n),
    STATECD = 41,
    REGION = 6,
    COUNTYCD = 1,
    PLOT = seq_len(n),
    PLOT_PUBLIC = seq_len(n),
    NFS_PLOT_NUMBER = seq_len(n),
    COORD_TYPE_NAME = "SP1",
    PC_COORD_METHOD_NAME = "HPGPS",
    PC_SRID = 6318,
    PC_LON_X = -120 - seq_len(n) / 100,
    PC_LAT_Y = 45 + seq_len(n) / 100,
    SP2_LON_X = -120 - seq_len(n) / 100 + 0.001,
    SP2_LAT_Y = 45 + seq_len(n) / 100 + 0.001,
    SP3_LON_X = -120 - seq_len(n) / 100 + 0.002,
    SP3_LAT_Y = 45 + seq_len(n) / 100 + 0.002,
    SP4_LON_X = -120 - seq_len(n) / 100 + 0.003,
    SP4_LAT_Y = 45 + seq_len(n) / 100 + 0.003,
    stringsAsFactors = FALSE
  )
}

test_that("fia_clean_best_cds returns long SUBPLOT data + sf geometry", {
  testthat::skip_if_not_installed("sf")

  n <- 4
  df_best <- make_df_best(n)
  res <- suppressWarnings(fia_clean_best_cds(df_best))

  testthat::expect_type(res, "list")

  # long format: 4 subplots per plot
  sub_df <- res[["SUBPLOT data.frame"]]
  testthat::expect_s3_class(sub_df, "data.frame")
  testthat::expect_equal(nrow(sub_df), 4L * n)

  # subplot points are an sf object
  testthat::expect_true(inherits(res[["SUBPLOT POINT"]], "sf"))

  # one multipoint feature per plot
  testthat::expect_equal(nrow(res[["PLOT POINT"]]), n)
})

test_that(".fn_combine collapses a single-id sf into a one-row MULTIPOINT", {
  testthat::skip_if_not_installed("sf")

  sf_in <- sf::st_as_sf(
    data.frame(unqid = 1, x = c(0, 1, 2), y = c(0, 1, 2)),
    coords = c("x", "y"),
    crs = 6339
  )
  res <- RForInvt:::.fn_combine(sf_in, 6339)
  testthat::expect_true(inherits(res, "sf"))
  testthat::expect_equal(nrow(res), 1L)
  testthat::expect_equal(
    as.character(sf::st_geometry_type(res)),
    "MULTIPOINT"
  )
})
