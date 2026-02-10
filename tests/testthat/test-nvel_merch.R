library(testthat)
library(data.table)

# Setup a standard spec table for testing
test_specs <- data.table(
  Species = "AF",
  Label = c("LargeSaw", "SmallSaw", "Pulp"),
  Rank = c(1, 2, 3),
  MinDib = c(12, 8, 5),
  MinDbh = c(10, 10, 7),
  MinLen = c(8, 8, 4)
)

test_that("NVEL_merch handles MinDib boundaries", {
  logs <- data.table(
    id = c("AtThreshold", "BelowThreshold"),
    Species = "AF",
    dib_sm = c(8.0, 7.9),  # Threshold for SmallSaw is 8
    log_len = 10,
    dbh = 15
  )

  res <- NVEL_merch(logs, test_specs)

  expect_equal(res[id == "AtThreshold", Label], "SmallSaw")
  expect_equal(res[id == "BelowThreshold", Label], "Pulp")
})

test_that("NVEL_merch handles MinDbh (Tree Size) boundaries", {
  logs <- data.table(
    id = c("TreeAtLimit", "TreeBelowLimit"),
    Species = "AF",
    dib_sm = 15,
    log_len = 10,
    dbh = c(10.0, 9.9)  # Threshold for Saw is DBH 10
  )

  res <- NVEL_merch(logs, test_specs)

  expect_equal(res[id == "TreeAtLimit", Label], "LargeSaw")
  # Tree below 10 should fall into Pulp (which has MinDbh 7)
  expect_equal(res[id == "TreeBelowLimit", Label], "Pulp")
})

test_that("NVEL_merch handles MinLen boundaries", {
  logs <- data.table(
    id = c("LenAtLimit", "LenBelowLimit"),
    Species = "AF",
    dib_sm = 15,
    log_len = c(8.0, 7.9), # Saw limit is 8
    dbh = 15
  )

  res <- NVEL_merch(logs, test_specs)

  expect_equal(res[id == "LenAtLimit", Label], "LargeSaw")
  # Log shorter than 8 should fall into Pulp (which has MinLen 4)
  expect_equal(res[id == "LenBelowLimit", Label], "Pulp")
})

test_that("NVEL_merch handles Rank Hierarchy with identical sizes", {
  # A log that fits all three categories should ALWAYS take Rank 1
  logs <- data.table(Species = "AF", dib_sm = 20, log_len = 20, dbh = 20)

  res <- NVEL_merch(logs, test_specs)

  expect_equal(res$Rank, 1)
  expect_equal(res$Label, "LargeSaw")
})

test_that("NVEL_merch identifies absolute Culls", {
  # Log fails even the most lenient category (Pulp: MinDib 5, MinDbh 7, MinLen 4)
  logs <- data.table(
    id = c("TooThin", "TooShort", "TooSmallTree"),
    Species = "AF",
    dib_sm = c(4, 10, 10),
    log_len = c(10, 3, 10),
    dbh = c(10, 10, 6)
  )

  res <- NVEL_merch(logs, test_specs)

  expect_true(all(is.na(res$Label)))
})
