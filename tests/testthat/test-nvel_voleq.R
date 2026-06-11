library(testthat)

# Tests for NVEL_voleq (exported) and internal .fn_fortran_voleq.

# ---- NVEL_voleq (BROKEN at baseline) ---------------------------------------

# FAILS at baseline: NVEL_voleq calls .load_dll(dll_64, dll_32, dll_func)
# (where `dll_func` is itself undefined), but the surviving .load_dll is the
# 2-arg version from NVEL_wtfactor.R, so it errors `unused argument`.
testthat::test_that("NVEL_voleq returns equation codes (intended)", {
  testthat::skip_if_not(nvel_dll_available())
  res <- NVEL_voleq(region = 2, forest = "01", district = "01", spcd = 951)
  testthat::expect_true("voleq" %in% names(res))
  testthat::expect_true(is.character(res$voleq) && nzchar(res$voleq[1]))
})

# FAILS at baseline: same .load_dll collision (unused argument) via dfTL path.
testthat::test_that("NVEL_voleq returns equation codes via dfTL (intended)", {
  testthat::skip_if_not(nvel_dll_available())
  tl <- data.frame(region = 6, forest = "01", district = "01",
                   spcd = c(951, 202))
  res <- NVEL_voleq(dfTL = tl)
  testthat::expect_true("voleq" %in% names(res))
  testthat::expect_equal(nrow(res), 2L)
  testthat::expect_true(is.character(res$voleq) && nzchar(res$voleq[1]))
})
