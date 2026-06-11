library(testthat)

# Tests for R/helper_functions.R:
#   nms_vec (exported), bs/fs (exported), reslash + write_clipboard (internal).
# Clipboard-backed functions are Windows-only; skip elsewhere.
# Returned-string literals below were captured by running the functions on
# this Windows machine (escaping for shQuote + backslashes is fiddly).

testthat::test_that("nms_vec returns a quoted c() vector of names", {
  testthat::expect_equal(
    nms_vec(data.frame(aa = 1, bb = 2, cc = 3)),
    "c('aa','bb','cc')"
  )
})

testthat::test_that("bs back-slashes and shQuotes a forward-slash path", {
  skip_on_os(c("mac", "linux", "solaris"))
  r <- bs("c:/temp/data.txt")
  # captured literal: shQuoted, double-backslashed path
  testthat::expect_equal(r, "\"c:\\\\temp\\\\data.txt\"")
})

testthat::test_that("fs forward-slashes and shQuotes a back-slash path", {
  skip_on_os(c("mac", "linux", "solaris"))
  p <- paste0("c:", "\\", "temp", "\\", "data.txt")  # literal c:\temp\data.txt
  testthat::expect_equal(fs(p), shQuote("c:/temp/data.txt"))
  testthat::expect_equal(fs(p), "\"c:/temp/data.txt\"")
})

testthat::test_that("reslash converts slash direction (internal)", {
  skip_on_os(c("mac", "linux", "solaris"))
  testthat::expect_equal(
    RForInvt:::reslash("c:/temp/a/b", slash = "back"),
    "\"c:\\\\temp\\\\a\\\\b\""
  )
  testthat::expect_equal(
    RForInvt:::reslash("c:/temp/a/b", slash = "forward"),
    "\"c:/temp/a/b\""
  )
})

testthat::test_that("write_clipboard runs silently (internal)", {
  skip_on_os(c("mac", "linux", "solaris"))
  testthat::expect_silent(RForInvt:::write_clipboard("hello"))
})
