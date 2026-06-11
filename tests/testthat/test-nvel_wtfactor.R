library(testthat)

# Tests for NVEL_wtfactor (exported, WORKS) and internals .fn_fortran_wtf and
# .formatTL2NVEL2 (the buck-family standardizer, pure R).

# ---- NVEL_wtfactor (WORKS) -------------------------------------------------

testthat::test_that("NVEL_wtfactor returns weight factors", {
  testthat::skip_if_not(nvel_dll_available())
  res <- NVEL_wtfactor(region = 2, forest = "01", spcd = 951)
  testthat::expect_s3_class(res, "data.table")
  testthat::expect_true(all(c("WFGRN_LBSCFT", "WFDRY_LBSCFT") %in% names(res)))
  testthat::expect_gt(res$WFGRN_LBSCFT[1], 0)
})

testthat::test_that("NVEL_wtfactor is vectorized over spcd", {
  testthat::skip_if_not(nvel_dll_available())
  res <- NVEL_wtfactor(region = 2, forest = "01", spcd = rep(c(951, 201, 113), 2))
  testthat::expect_equal(nrow(res), 6L)
})

# ---- .fn_fortran_wtf (WORKS if DLL loaded) ---------------------------------

testthat::test_that(".fn_fortran_wtf returns named weight factors", {
  testthat::skip_if_not(nvel_dll_available())
  RForInvt:::.load_dll(nvel_dll_path(), nvel_dll_path())
  r <- RForInvt:::.fn_fortran_wtf(
    region            = 2L,
    forest            = "01",
    species           = 951L,
    dll_func_wtfactor = "getwtfactor_r"
  )
  testthat::expect_named(r, c("WFGRN_LBSCFT", "WFDRY_LBSCFT"))
  testthat::expect_gt(r$WFGRN_LBSCFT, 0)
})

# ---- .formatTL2NVEL2 (pure R, WORKS) ---------------------------------------

testthat::test_that(".formatTL2NVEL2 standardizes columns (no DLL)", {
  r <- RForInvt:::.formatTL2NVEL2(
    dfTL0     = data.frame(region = 6, forest = "01", district = "01",
                           dbh = 12, ht = 70, spcd = 202),
    voleq     = NA,
    region    = NA,
    forest    = NA,
    district  = NA,
    voleqNm   = "voleq",
    regionNm  = "region",
    forestNm  = "forest",
    districtNm = "district",
    spcdNm    = "spcd",
    dbhNm     = "dbh",
    htNm      = "ht",
    pulpDbNm  = "pulpDb",
    sawDbNm   = "sawDb",
    htPrd1Nm  = "htPrd1",
    htPrd2Nm  = "htPrd2",
    upHt1Nm   = "upHt1",
    upDb1Nm   = "upDb1",
    stumpHtNm = "stumpHt",
    fclassNm  = "fclass",
    dbtbhNm   = "dbtbh",
    btrNm     = "btr"
  )
  testthat::expect_true(all(c("pulpDb", "sawDb", "htPrd1", "fclass", "btr") %in% names(r)))
})
