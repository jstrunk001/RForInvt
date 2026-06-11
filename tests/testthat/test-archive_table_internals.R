library(testthat)

# Unit tests for the internal helpers of R/archive_table.R:
#   .normalize_colnames, .collapse_vec, .prepare_for_xlsx, .safe_write
# Internals accessed via RForInvt:::name.

# ---- .normalize_colnames -----------------------------------------------------

testthat::test_that(".normalize_colnames suffixes case-insensitive duplicates", {
  testthat::expect_equal(
    RForInvt:::.normalize_colnames(c("A", "a", "B", "b")),
    c("A", "a.2", "B", "b.2")
  )
  testthat::expect_equal(
    RForInvt:::.normalize_colnames(c("x", "X", "x", "Y")),
    c("x", "X.2", "x.3", "Y")
  )
})

testthat::test_that(".normalize_colnames leaves unique names unchanged", {
  testthat::expect_equal(
    RForInvt:::.normalize_colnames(c("one", "two")),
    c("one", "two")
  )
  testthat::expect_equal(
    RForInvt:::.normalize_colnames(c("z")),
    "z"
  )
})

# ---- .collapse_vec -----------------------------------------------------------

testthat::test_that(".collapse_vec comma joins with commas", {
  testthat::expect_equal(RForInvt:::.collapse_vec(1:3, "comma"), "1,2,3")
  testthat::expect_equal(RForInvt:::.collapse_vec(c("a", "b"), "comma"), "a,b")
})

testthat::test_that(".collapse_vec json wraps in c(...)", {
  testthat::expect_equal(RForInvt:::.collapse_vec(1:3, "json"), "c(1, 2, 3)")
})

testthat::test_that(".collapse_vec rejects unsupported methods", {
  testthat::expect_error(
    RForInvt:::.collapse_vec(1:3, "bogus"),
    "Unsupported collapse method: bogus"
  )
})

# ---- .prepare_for_xlsx -------------------------------------------------------

testthat::test_that(".prepare_for_xlsx auto collapses mixed-length list columns", {
  df <- data.frame(id = 1:3)
  df$lst <- I(list(1, 2:3, 4))
  out <- RForInvt:::.prepare_for_xlsx(df,
                                      drop_geometry   = FALSE,
                                      collapse_lists  = "auto",
                                      matrix_collapse = "comma",
                                      time_tz         = "UTC")
  testthat::expect_type(out$lst, "character")
  testthat::expect_equal(out$lst, c("1", "2,3", "4"))
})

testthat::test_that(".prepare_for_xlsx auto unlists all length-1 list columns", {
  df <- data.frame(id = 1:2)
  df$lst <- I(list(10, 20))
  out <- RForInvt:::.prepare_for_xlsx(df,
                                      drop_geometry   = FALSE,
                                      collapse_lists  = "auto",
                                      matrix_collapse = "comma",
                                      time_tz         = "UTC")
  testthat::expect_type(out$lst, "double")
  testthat::expect_equal(out$lst, c(10, 20))
})

testthat::test_that(".prepare_for_xlsx comma mode collapses list columns", {
  df <- data.frame(id = 1:2)
  df$lst <- I(list(1:2, 3:4))
  out <- RForInvt:::.prepare_for_xlsx(df,
                                      drop_geometry   = FALSE,
                                      collapse_lists  = "comma",
                                      matrix_collapse = "comma",
                                      time_tz         = "")
  testthat::expect_equal(out$lst, c("1,2", "3,4"))
})

testthat::test_that(".prepare_for_xlsx json mode collapses list columns", {
  df <- data.frame(id = 1:2)
  df$lst <- I(list(1:2, 3:4))
  out <- RForInvt:::.prepare_for_xlsx(df,
                                      drop_geometry   = FALSE,
                                      collapse_lists  = "json",
                                      matrix_collapse = "comma",
                                      time_tz         = "")
  testthat::expect_equal(out$lst, c("c(1, 2)", "c(3, 4)"))
})

testthat::test_that(".prepare_for_xlsx unlist mode errors on length != 1", {
  df <- data.frame(id = 1:2)
  df$lst <- I(list(1, 2:3))
  testthat::expect_error(
    RForInvt:::.prepare_for_xlsx(df,
                                 drop_geometry   = FALSE,
                                 collapse_lists  = "unlist",
                                 matrix_collapse = "comma",
                                 time_tz         = ""),
    "length != 1"
  )
})

testthat::test_that(".prepare_for_xlsx error mode rejects list columns", {
  df <- data.frame(id = 1:2)
  df$lst <- I(list(1, 2))
  testthat::expect_error(
    RForInvt:::.prepare_for_xlsx(df,
                                 drop_geometry   = FALSE,
                                 collapse_lists  = "error",
                                 matrix_collapse = "comma",
                                 time_tz         = ""),
    "list column encountered"
  )
})

testthat::test_that(".prepare_for_xlsx comma collapses matrix columns row-wise", {
  df <- data.frame(id = 1:3)
  df$m <- matrix(1:6, nrow = 3)
  out <- RForInvt:::.prepare_for_xlsx(df,
                                      drop_geometry   = FALSE,
                                      collapse_lists  = "auto",
                                      matrix_collapse = "comma",
                                      time_tz         = "")
  testthat::expect_equal(out$m, c("1,4", "2,5", "3,6"))
})

testthat::test_that(".prepare_for_xlsx error mode rejects matrix columns", {
  df <- data.frame(id = 1:3)
  df$m <- matrix(1:6, nrow = 3)
  testthat::expect_error(
    RForInvt:::.prepare_for_xlsx(df,
                                 drop_geometry   = FALSE,
                                 collapse_lists  = "auto",
                                 matrix_collapse = "error",
                                 time_tz         = ""),
    "matrix column encountered"
  )
})

testthat::test_that(".prepare_for_xlsx converts POSIXlt to POSIXct", {
  df <- data.frame(id = 1)
  df$col <- as.POSIXlt("2020-01-01 00:00:00", tz = "UTC")
  out <- RForInvt:::.prepare_for_xlsx(df,
                                      drop_geometry   = FALSE,
                                      collapse_lists  = "auto",
                                      matrix_collapse = "comma",
                                      time_tz         = "UTC")
  testthat::expect_true(inherits(out$col, "POSIXct"))
})

testthat::test_that(".prepare_for_xlsx truncates column names to 31 chars", {
  long_nm <- paste(rep("x", 40), collapse = "")
  df <- data.frame(a = 1)
  names(df) <- long_nm
  out <- RForInvt:::.prepare_for_xlsx(df,
                                      drop_geometry   = FALSE,
                                      collapse_lists  = "auto",
                                      matrix_collapse = "comma",
                                      time_tz         = "")
  testthat::expect_equal(nchar(names(out)), 31L)
})

# ---- .safe_write -------------------------------------------------------------

testthat::test_that(".safe_write returns success for a clean expression", {
  res <- RForInvt:::.safe_write(quote(1 + 1), "x", FALSE)
  testthat::expect_equal(res, list(success = TRUE, error = NULL))
})

testthat::test_that(".safe_write isolates errors when stop_on_error = FALSE", {
  res <- RForInvt:::.safe_write(quote(stop("boom")), "fmt", FALSE)
  testthat::expect_equal(res, list(success = FALSE, error = "boom"))
})

testthat::test_that(".safe_write rethrows with [fmt] prefix when stop_on_error = TRUE", {
  testthat::expect_error(
    RForInvt:::.safe_write(quote(stop("boom")), "fmt", TRUE),
    "[fmt] boom",
    fixed = TRUE
  )
})

testthat::test_that(".safe_write muffles warnings and treats the write as success", {
  # FAILS at baseline: the tryCatch(warning=) handler calls
  # invokeRestart("muffleWarning") after the protected expression's stack has
  # already unwound, so no muffleWarning restart is in scope. This raises
  # `no 'restart' 'muffleWarning' found` instead of muffling the warning and
  # returning success. The handler should muffle the warning and report success.
  res <- RForInvt:::.safe_write(quote(warning("w")), "fmt", FALSE)
  testthat::expect_true(res$success)
})

testthat::test_that(".safe_write currently errors on warnings (documents baseline bug)", {
  # Pins the current (buggy) behavior; see the FAILS-at-baseline test above for
  # the intended behavior.
  testthat::expect_error(
    RForInvt:::.safe_write(quote(warning("w")), "fmt", FALSE),
    "muffleWarning"
  )
})
