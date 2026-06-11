library(testthat)

# Tests for R/make_strata.R : make_strata() and assign_strata().

testthat::test_that("make_strata (collapse=FALSE) returns the documented strata table", {
  testthat::skip_if_not_installed("plyr")

  set.seed(10)
  df <- data.frame(a = rnorm(200) * 10 + 50, b = rnorm(200) * 5 + 20)

  s <- make_strata(data = df, x1 = "a", x2 = "b",
                   split_x1 = "qt", split_x2 = "qt", nest_x2 = TRUE,
                   n1 = 2, n2 = 2, collapse = FALSE)

  testthat::expect_true(is.data.frame(s))
  nm_vec <- c("stratum", "stratum1", "n_obs", "nest_x2", "collapse",
              "nm_x1", "nm_x2", "stratum_x1", "x1.from", "x1.to", "Freq.x1",
              "stratum_x2", "x2.from", "x2.to", "Freq.x2")
  testthat::expect_equal(names(s), nm_vec)
  testthat::expect_gt(nrow(s), 0)
})

testthat::test_that("make_strata collapses small/empty strata when collapse=TRUE", {
  testthat::skip_if_not_installed("plyr")

  # FAILS at baseline: when collapse=TRUE and a defined stratum receives no
  # records (here an empty-interval x2 split makes missing_idx > 0), the
  # collapse block reaches a dead branch (L392-401) that references `type_x1`,
  # a variable that is never defined -> "object 'type_x1' not found".
  set.seed(1)
  df <- data.frame(
    a = c(rnorm(70, 10, 5), rnorm(70, 60, 5)),
    b = c(rnorm(70,  5, 4), rnorm(70, 40, 4))
  )

  s <- make_strata(data = df, x1 = "a", x2 = "b",
                   split_x1 = "qt", split_x2 = "eq", nest_x2 = TRUE,
                   n1 = 2, n2 = 2, collapse = TRUE)

  testthat::expect_true(is.data.frame(s))
  testthat::expect_true("stratum" %in% names(s))
})

testthat::test_that("assign_strata assigns in-range rows and NAs out-of-range rows", {
  testthat::skip_if_not_installed("plyr")

  set.seed(10)
  df <- data.frame(a = rnorm(200) * 10 + 50, b = rnorm(200) * 5 + 20)

  strata <- make_strata(data = df, x1 = "a", x2 = "b",
                        split_x1 = "qt", split_x2 = "qt", nest_x2 = TRUE,
                        n1 = 2, n2 = 2, collapse = FALSE)

  asn <- assign_strata(strata, df)

  testthat::expect_true("stratum" %in% names(asn))
  # every original (in-range) row gets a defined stratum
  testthat::expect_equal(sum(!is.na(asn$stratum)), nrow(df))
  testthat::expect_true(all(asn$stratum[!is.na(asn$stratum)] %in% strata$stratum))

  # a point outside the strata definition extents is left unassigned (NA)
  oor <- data.frame(a = max(strata$x1.to, na.rm = TRUE) + 1e6,
                    b = max(strata$x2.to, na.rm = TRUE) + 1e6)
  asn_oor <- assign_strata(strata, oor)
  testthat::expect_true(is.na(asn_oor$stratum[1]))
})
