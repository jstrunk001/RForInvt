library(testthat)

# Regression tests for R/make_strata.R covering the x1/x2 type combinations.
# Previously only numeric-x1 / numeric-x2 worked: two merges hard-coded
# by.y = "Var1", but table(<single symbol>) names that column after the symbol
# (e.g. "dat_x2_fct"), so non-nested / factor combinations errored with
# "'by' must specify a uniquely valid column". The factor-x1 paths also lacked a
# Freq.x1 column and a dat_x1_fct binning, and assign_strata had no both-factor
# branch. All four combinations below should now build strata and assign every
# in-range record. Quantile ("qt") splitting is used (the default).

make_strata_test_df <- function() {
  set.seed(1)
  n <- 400
  hts <- sample(1:150, n, TRUE)
  dbhs <- abs(hts / 5 + rnorm(n) * 2)
  data.frame(
    height     = hts,
    dbh        = dbhs,
    height_cat = cut(hts, 8),
    dbh_cat    = cut(dbhs, 8)
  )
}

combos <- list(
  "numeric x1 + numeric x2 (nested)"   = list(x1 = "height",     x2 = "dbh",     nest = TRUE),
  "numeric x1 + numeric x2 (crossed)"  = list(x1 = "height",     x2 = "dbh",     nest = FALSE),
  "factor x1 + numeric x2 (nested)"    = list(x1 = "height_cat", x2 = "dbh",     nest = TRUE),
  "numeric x1 + factor x2 (crossed)"   = list(x1 = "height",     x2 = "dbh_cat", nest = FALSE),
  "factor x1 + factor x2 (crossed)"    = list(x1 = "height_cat", x2 = "dbh_cat", nest = FALSE)
)

for (lbl in names(combos)) {
  local({
    cfg <- combos[[lbl]]
    testthat::test_that(paste0("make_strata/assign_strata handle: ", lbl), {
      testthat::skip_if_not_installed("plyr")

      df <- make_strata_test_df()
      s <- make_strata(df, x1 = cfg$x1, x2 = cfg$x2,
                       split_x1 = "qt", split_x2 = "qt", nest_x2 = cfg$nest,
                       n1 = 8, n2 = 8, min_recs = 7, precision = 0)

      testthat::expect_true(is.data.frame(s))
      testthat::expect_gt(nrow(s), 0)
      testthat::expect_true("Freq.x1" %in% names(s))

      a <- assign_strata(s, df)
      testthat::expect_true("stratum" %in% names(a))
      # every record falls within some stratum for these in-range data
      testthat::expect_equal(sum(!is.na(a$stratum)), nrow(df))
      testthat::expect_true(all(a$stratum[!is.na(a$stratum)] %in% s$stratum))
    })
  })
}
