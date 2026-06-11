library(testthat)

# Tests for the plot-level compilation in R/compile_plots.R.
# Some paths WORK at the pre-refactor baseline; two are documented BROKEN and
# their tests assert intended behavior (EXPECTED TO FAIL at baseline).

# small compiled tree fixture: 2 plots, 2 trees each, with TPA expansion + ba_ft
make_df_tree <- function() {
  data.frame(
    plot = c(1, 1, 2, 2),
    id = 1:4,
    dbh = c(10, 10, 20, 20),
    ht = c(50, 70, 60, 60),
    spp = c("a", "b", "a", "b"),
    TPA = 10,
    ba_ft = 0.005454 * c(10, 10, 20, 20)^2,
    ntrees = 1
  )
}

tree_nms_std <- function() {
  list(
    plot_ids = "plot",
    tree_ids = "id",
    dbh = "dbh",
    ht = "ht",
    spp = "spp",
    expansion = "TPA"
  )
}

test_that("compile_plots in-memory no-filter path returns one row per plot", {
  df_tree <- make_df_tree()
  res <- suppressWarnings(compile_plots(
    df_tree = df_tree,
    df_plot = NA,
    tree_nms = tree_nms_std(),
    tree_filter = NA,
    plot_filter = NA,
    dir_out = NA,
    nclus = 1,
    return = TRUE,
    fns_compute = list(plot_lor_qmd, plot_wtsum),
    sum_nms = "ntrees"
  ))
  testthat::expect_s3_class(res, "data.frame")
  testthat::expect_equal(nrow(res), 2L)
  testthat::expect_setequal(res$plot, c(1, 2))
})

test_that("compile_plots filters trees (intended)", {
  # FAILS at baseline: tree_filter path calls sqldf(tree_filter,
  # envir = as.environment(data)) at L256, but `data` is undefined in scope,
  # so it errors with "invalid object for 'as.environment'".
  df_tree <- make_df_tree()
  res <- suppressWarnings(compile_plots(
    df_tree = df_tree,
    df_plot = NA,
    tree_nms = tree_nms_std(),
    tree_filter = "select * from df_tree where dbh > 2",
    plot_filter = NA,
    dir_out = NA,
    nclus = 1,
    return = TRUE,
    fns_compute = list(plot_lor_qmd),
    sum_nms = "ntrees"
  ))
  testthat::expect_s3_class(res, "data.frame")
  testthat::expect_equal(nrow(res), 2L)
})

test_that("plot_wtsum computes weighted sums col*expansion", {
  dft <- data.frame(plot = 1, ntrees = c(1, 1), ba_ft = c(2, 4), TPA = c(10, 10))
  tn <- list(plot_ids = "plot", expansion = "TPA")
  res <- plot_wtsum(dft, tree_nms = tn, sum_nms = c("ntrees", "ba_ft"))
  testthat::expect_equal(res$ntrees, 20)
  testthat::expect_equal(res$ba_ft, 60)
})

test_that("plot_wtsum errors when expansion is not in tree_nms", {
  dft <- data.frame(plot = 1, ntrees = c(1, 1), TPA = c(10, 10))
  testthat::expect_error(
    plot_wtsum(dft, tree_nms = list(plot_ids = "plot"), sum_nms = "ntrees")
  )
})

test_that("plot_lor_qmd computes weighted plot summaries", {
  dfl <- data.frame(plot = 1, dbh = c(10, 10), ht = c(50, 70), TPA = c(1, 1))
  tn <- list(plot_ids = "plot", dbh = "dbh", ht = "ht", expansion = "TPA")
  res <- plot_lor_qmd(dfl, tree_nms = tn)
  # assert by column (row name may be "95%")
  testthat::expect_equal(res$qmd, 10)
  testthat::expect_equal(res$mean_dbh, 10)
  testthat::expect_equal(res$lorht, 60)
  testthat::expect_equal(res$meanht, 60)
})

test_that("spp_y_plot returns dominant species per plot (intended)", {
  testthat::skip_if_not_installed("reshape2")
  # FAILS at baseline: dom_order <- order(dfi1[,-1], decreasing = TRUE) at
  # L593 passes a multi-column data.frame to order(), which errors with
  # "cannot xtfrm data frames".
  dfs <- data.frame(
    plot = c(1, 1, 2, 2),
    dbh = c(10, 12, 8, 9),
    spp = c("a", "b", "a", "b"),
    TPA = 1,
    ba_ft = c(1, 2, 3, 4)
  )
  tn <- list(
    plot_ids = "plot",
    dbh = "dbh",
    spp = "spp",
    expansion = "TPA",
    domspp_y = "ba_ft"
  )
  res <- suppressWarnings(
    spp_y_plot(dfs, tree_nms = tn, n_dom_spp = 2, spp_y = "ba_ft")
  )
  testthat::expect_s3_class(res, "data.frame")
  testthat::expect_equal(nrow(res), 2L)
})

test_that(".subset_cn subsets all data.frames in a list by PLT_CN", {
  res <- RForInvt:::.subset_cn(
    list(a = data.frame(PLT_CN = c(1, 1, 2, NA), v = 1:4), b = "x"),
    cn = 1
  )
  testthat::expect_equal(nrow(res$a), 2L)
  testthat::expect_equal(res$a$v, c(1L, 2L))
  testthat::expect_equal(res$b, "x")
})

test_that(".subset_ids (merge version) keeps rows matching supplied ids", {
  res <- RForInvt:::.subset_ids(
    data.frame(PLOT = 1:3, v = c(10, 20, 30)),
    data.frame(PLOT = c(1, 2))
  )
  testthat::expect_equal(nrow(res), 2L)
  testthat::expect_equal(res$v, c(10, 20))
})

test_that(".compile_1plot returns a one-row data.frame with plot id column", {
  dat <- data.frame(plot = c(1, 1), dbh = c(10, 10), ht = c(50, 70), TPA = c(1, 1))
  res <- RForInvt:::.compile_1plot(
    id = data.frame(plot = 1),
    df_tree = dat,
    tree_nms = list(plot_ids = "plot", expansion = "TPA", dbh = "dbh", ht = "ht"),
    plot_nms = list(plot_ids = "plot"),
    sql_tbl = "x",
    fns_compute = list(plot_lor_qmd),
    path_sqlite = NA,
    lock_name = NA,
    dir_logs = NA
  )
  testthat::expect_s3_class(res, "data.frame")
  testthat::expect_equal(nrow(res), 1L)
  testthat::expect_true("plot" %in% names(res))
  testthat::expect_equal(res$plot, 1)
})
