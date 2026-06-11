library(testthat)

# Tests for small utility functions:
#   aggregate2, duplicated2, gsub_vec, dist_vec, file_stamp, RForInvt

testthat::test_that("aggregate2 single response splits matrix column into named cols", {
  ag <- aggregate2(
    x ~ y,
    data.frame(x = 1:50, y = 1:10),
    FUN = function(x, ...) c(sd = sd(x, ...), mean = mean(x, ...)),
    nm_sep = "_"
  )
  testthat::expect_equal(names(ag), c("y", "x_sd", "x_mean"))
  testthat::expect_equal(nrow(ag), 10)
  testthat::expect_equal(ag$x_mean, 21:30)
  testthat::expect_equal(ag$x_sd[1], 15.8113883008419, tolerance = 1e-6)
})

testthat::test_that("aggregate2 multi response uses nm_sep and orders columns", {
  ag <- aggregate2(
    cbind(x, z) ~ y,
    data.frame(x = 1:50, y = 1:10, z = 50:1),
    FUN = function(x, ...) c(sd = sd(x, ...), mean = mean(x, ...), n = length(x)),
    nm_sep = "."
  )
  testthat::expect_equal(
    names(ag),
    c("y", "x.sd", "x.mean", "x.n", "z.sd", "z.mean", "z.n")
  )
  testthat::expect_equal(ag$x.mean[1], 21)
  testthat::expect_equal(ag$z.mean[1], 30)
  testthat::expect_equal(ag$x.n[1], 5)
})

testthat::test_that("duplicated2 marks all duplicates or only later ones", {
  testthat::expect_equal(
    duplicated2(c(1, 2, 2, 3, 3, 3, 4)),
    c(FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE)
  )
  testthat::expect_equal(
    duplicated2(c(1, 2, 2, 3, 3, 3, 4), all = FALSE),
    c(FALSE, FALSE, TRUE, FALSE, TRUE, TRUE, FALSE)
  )
  testthat::expect_equal(
    duplicated2(data.frame(a = c(1, 1, 2, 3, 3), b = c("x", "x", "y", "z", "z"))),
    c(TRUE, TRUE, FALSE, TRUE, TRUE)
  )
})

testthat::test_that("gsub_vec applies vectors of patterns/replacements in order", {
  testthat::expect_equal(
    gsub_vec(c("a", "b"), c("X", "Y"), c("abc", "bca", "ab")),
    c("XYc", "YcX", "XY")
  )
  testthat::expect_equal(gsub_vec("a", "Z", "banana"), "bZnZnZ")
})

testthat::test_that("dist_vec computes pairwise distances for two point sets", {
  dat1 <- data.frame(id = 1:2, x = c(0, 3), y = c(0, 0))
  dat2 <- data.frame(id = c("A", "B"), x = c(0, 0), y = c(0, 4))
  res <- dist_vec(dat1, dat2)
  testthat::expect_equal(res$id, c("A", "B"))
  testthat::expect_equal(names(res), c("id", "1", "2"))
  m <- as.matrix(res[, -1])
  dimnames(m) <- NULL
  testthat::expect_equal(m, matrix(c(0, 4, 3, 5), 2, 2))
})

testthat::test_that("dist_vec supports a single reference point form", {
  res <- dist_vec(
    c(id = "P1", x = 0, y = 0),
    data.frame(id = c("A", "B"), x = c(0, 3), y = c(0, 4))
  )
  testthat::expect_equal(res$P1, c(0, 5))
})

testthat::test_that("file_stamp builds path with prefix/stamp/suffix", {
  testthat::expect_equal(
    file_stamp(path = "c:/temp/proj", stamp = "ABC", prefix = "P_", suffix = ".csv"),
    "c:/temp/proj/P_ABC.csv"
  )
  testthat::expect_equal(
    basename(file_stamp(path = "c:/temp/proj", stamp = "ABC", prefix = "P_", suffix = ".csv")),
    "P_ABC.csv"
  )
  testthat::expect_equal(file_stamp(path = "x", stamp = "S"), "x/logfile_VS.txt")
})

testthat::test_that("RForInvt package-doc stub returns NULL", {
  testthat::expect_null(RForInvt())
})
