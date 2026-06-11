library(testthat)

# Tests for NVEL_voleq (exported) and internal .fn_fortran_voleq.

# ---- NVEL_voleq (FIXED) ----------------------------------------------------

# Now FIXED: NVEL_voleq uses the 2-arg RForInvt:::.nvel_load_dll(dll_64, dll_32)
# loader and returns equation codes.
testthat::test_that("NVEL_voleq returns equation codes", {
  testthat::skip_if_not(nvel_dll_available())
  res <- NVEL_voleq(region = 2, forest = "01", district = "01", spcd = 951)
  testthat::expect_true("voleq" %in% names(res))
  testthat::expect_true(is.character(res$voleq) && nzchar(res$voleq[1]))
})

testthat::test_that("NVEL_voleq returns equation codes via dfTL", {
  testthat::skip_if_not(nvel_dll_available())
  tl <- data.frame(region = 6, forest = "01", district = "01",
                   spcd = c(951, 202))
  res <- NVEL_voleq(dfTL = tl)
  testthat::expect_true("voleq" %in% names(res))
  testthat::expect_equal(nrow(res), 2L)
  testthat::expect_true(is.character(res$voleq) && nzchar(res$voleq[1]))
})
