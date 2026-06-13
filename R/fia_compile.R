#'@name fia_compile
#'
#'@title Compile FIA tree and plot data (Phase 2 compilation layer)
#'
#'@description
#'  Thin FIA presets over \code{compile_trees()} and \code{compile_plots()},
#'  plus a few \code{compile_trees}-compatible compute functions. They take the
#'  frames produced by \code{fia_trees()} / \code{fia_plots()} and produce
#'  per-tree attributes (basal area, diameter class, volume/biomass) and then
#'  per-acre plot attributes, with FIA-correct zero-filling of plots that have
#'  no qualifying trees.
#'
#'  \itemize{
#'    \item \code{fia_compile_trees()} - \code{compile_trees()} with FIADB column
#'          maps and an FIA-aware default \code{fns_compute} list.
#'    \item \code{fia_vol()} - \code{compile_trees} plug-in that produces tidy
#'          per-tree volume/biomass columns from either FIADB columns
#'          (\code{vol_source = "fiadb"}) or recomputed NVEL volumes
#'          (\code{vol_source = "nvel"}).
#'    \item \code{fia_nvel_volume()} / \code{fia_nvel_biomass()} - plug-ins that
#'          call the \code{NVEL_*} family for custom merch specs.
#'    \item \code{fia_compile_plots()} - \code{compile_plots()} with FIA plot ids,
#'          per-acre weighted sums via \code{TPA_EXP}, and zero-filling against
#'          the full evaluation plot list.
#'  }
#'
#'@details
#'  The per-tree expansion \code{TPA_EXP} (from \code{fia_trees()}) is the
#'  per-acre weight: \code{compile_plots}' \code{plot_wtsum} multiplies each
#'  per-tree attribute by \code{TPA_EXP} and sums, so a per-tree \code{ba_ft}
#'  becomes basal area per acre, \code{vol_cf_net} becomes net cubic feet per
#'  acre, and a per-tree constant \code{stems = 1} becomes trees per acre.
#'
#'  \code{fia_vol()} standardizes volume/biomass onto three tidy per-tree columns
#'  so downstream summation and component breakdowns (by species / diameter
#'  class) work regardless of source:
#'  \itemize{
#'    \item \code{vol_cf_net} - net merchantable cubic feet
#'      (FIADB \code{VOLCFNET}; NVEL \code{MCFV_NET}).
#'    \item \code{vol_bf_net} - net board feet, Scribner
#'      (FIADB \code{VOLCSNET}; NVEL \code{BFV_NET}).
#'    \item \code{dry_bio} - dry aboveground biomass
#'      (FIADB \code{DRYBIO_AG}; NVEL \code{TBIODRY_LBS}).
#'  }
#'
#'  Zero-filling: \code{fia_compile_plots()} compiles only plots that carry
#'  trees, then left-joins the result onto the full set of plots in the
#'  evaluation (from \code{df_plot}) and fills the per-acre metric columns with
#'  0. This keeps forested-but-empty and nonforest plots in the estimate, which
#'  is required for unbiased totals.
#'
#'  This program is free software but it is provided WITHOUT WARRANTY and with
#'  ABSOLUTELY NO GUARANTEE of fitness or functionality for any purpose; you can
#'  redistribute it and/or modify it under the terms of the GNU General Public
#'  License as published by the Free Software Foundation; either version 2 of the
#'  License, or (at your option) any later version.
#'
#'\cr
#'Revision History
#' \tabular{ll}{
#'1.0 \tab 6/11/2026 Created (Phase 2 FIA compilation layer) \cr
#'}
#'
#'@author Jacob Strunk <someone@@somewhere.com>
#'
#'@param df_tree a tree frame from \code{fia_trees()} (\code{fia_compile_trees};
#'  must contain the columns named in \code{tree_nms}). For
#'  \code{fia_compile_plots()}, the compiled tree frame returned by
#'  \code{fia_compile_trees()}.
#'@param df_plot a condition- or plot-level frame from \code{fia_plots()}, used
#'  by \code{fia_compile_plots()} to define the full plot list and design columns.
#'@param tree_nms named map of logical tree column names onto \code{df_tree}.
#'  For \code{fia_compile_trees()} a named character vector; for
#'  \code{fia_compile_plots()} the list form expected by \code{compile_plots()}.
#'@param vol_source where per-tree volume comes from: \code{"fiadb"} (use the
#'  FIADB \code{VOLCFNET}/\code{VOLCSNET}/\code{DRYBIO_AG} columns) or
#'  \code{"nvel"} (recompute with \code{NVEL_volume()}; requires the bundled
#'  vollib DLL).
#'@param db_breaks diameter-class breakpoints passed to \code{dbcl()}; defaults
#'  to FIA 2-inch classes.
#'@param fns_compute optional list of \code{compile_*}-compatible functions;
#'  \code{NULL} uses the FIA defaults (\code{list(ba_ft, dbcl, fia_vol)} for
#'  trees; \code{list(plot_wtsum)} for plots). Add \code{plot_lor_qmd} to the
#'  plot list for QMD / Lorey's height when tree heights are populated (it errors
#'  on plots with all-NA heights, which would drop those plots).
#'@param region,forest NVEL region/forest used when \code{vol_source = "nvel"}
#'  (PNW default region 6).
#'@param voleq optional NVEL volume-equation code forced for all trees in
#'  \code{fia_nvel_volume()} (NA selects an equation per region/forest/species).
#'@param plot_nms named list mapping plot id columns for
#'  \code{fia_compile_plots()} (passed to \code{compile_plots()}; default
#'  \code{list(plot_ids = "PLT_CN")}).
#'@param sum_nms columns to weight-sum to per-acre values in
#'  \code{fia_compile_plots()}; \code{NULL} auto-detects the tidy per-tree
#'  metric columns (and any species/diameter-class component columns derived
#'  from them).
#'@param x,... passed through: \code{x} is the working tree frame for the
#'  compute plug-ins; \code{...} forwards arguments to \code{compile_*} and the
#'  compute functions.
#'
#'@return
#'  \code{fia_compile_trees()} returns the tree frame with added per-tree
#'  columns. \code{fia_compile_plots()} returns one row per plot with per-acre
#'  attributes plus the design columns carried from \code{df_plot}. The compute
#'  plug-ins return \code{x} with their columns appended.
#'
#'@examples
#'\donttest{
#'  db_path <- system.file("extdata", "FIADB_demo.db", package = "RForInvt")
#'  if (nzchar(db_path)) {
#'    db <- fia_db(db_path)
#'    ev <- fia_evalid(db, statecd = 53, eval_type = "EXPVOL", most_recent = TRUE)
#'
#'    tr <- fia_trees(db, evalid = ev)
#'    pl <- fia_plots(db, evalid = ev)
#'
#'    #per-tree: basal area, 2-inch diameter class, tidy volume columns
#'    tr_c <- fia_compile_trees(tr, vol_source = "fiadb")
#'    head(tr_c[, c("PLT_CN", "SPCD", "DIA", "ba_ft", "dbcl", "vol_cf_net")])
#'
#'    #per-acre plot attributes, with nonforest/empty plots zero-filled
#'    pl_c <- fia_compile_plots(tr_c, pl)
#'    head(pl_c[, c("PLT_CN", "COUNTYCD", "stems", "ba_ft", "vol_cf_net")])
#'  }
#'}
#'
#'
#'@seealso \code{\link{compile_trees}}\cr \code{\link{compile_plots}}\cr
#'  \code{\link{fia_trees}}\cr \code{\link{NVEL_volume}}\cr
NULL

# default FIADB -> logical name map for the tree compilers
.fia_tree_nms = c(
  tree_ids  = "TRE_CN",
  dbh       = "DIA",
  ht        = "HT",
  spp       = "SPCD",
  dbcl      = "dbcl",
  expansion = "TPA_EXP",
  vol_cf    = "VOLCFNET",
  vol_bf    = "VOLCSNET",
  bio       = "DRYBIO_AG"
)

# tidy per-tree metric column prefixes that compile_plots should weight-sum.
# component functions (dbcl_y / spp_y / dbcl_spp_y) extend these names, so we
# match on prefix.
.fia_metric_prefixes = c("stems", "ba_ft", "vol_cf_net", "vol_bf_net", "dry_bio")

# ---------------------------------------------------------------------------
# fia_vol(): compile_trees plug-in producing tidy per-tree volume/biomass cols
# ---------------------------------------------------------------------------

#'@export
#'@rdname fia_compile
fia_vol = function(x, tree_nms, vol_source = c("fiadb", "nvel"), ...){
  vol_source = match.arg(vol_source)
  if(vol_source == "nvel") return(fia_nvel_volume(x, tree_nms = tree_nms, ...))

  #fiadb: copy the FIADB volume/biomass columns onto tidy names when present
  x_in = x
  cp = function(df, to, from){
    if(!is.na(from) && from %in% names(df)) df[[to]] = df[[from]]
    df
  }
  x_in = cp(x_in, "vol_cf_net", tree_nms["vol_cf"])
  x_in = cp(x_in, "vol_bf_net", tree_nms["vol_bf"])
  x_in = cp(x_in, "dry_bio",    tree_nms["bio"])
  if(!any(c("vol_cf_net", "vol_bf_net", "dry_bio") %in% names(x_in)))
    warning("fia_vol(vol_source = 'fiadb'): none of the FIADB volume columns ",
            "(vol_cf/vol_bf/bio in tree_nms) were found in df_tree")
  x_in
}

# ---------------------------------------------------------------------------
# fia_nvel_volume(): compile_trees plug-in wrapping NVEL_volume()
# ---------------------------------------------------------------------------

#'@export
#'@rdname fia_compile
fia_nvel_volume = function(x, tree_nms, region = 6, forest = NA, voleq = NA, ...){
  res = NVEL_volume(
    dfTL    = as.data.frame(x),
    region  = region,
    forest  = forest,
    voleq   = voleq,
    spcdNm  = unname(tree_nms["spp"]),
    dbhNm   = unname(tree_nms["dbh"]),
    htNm    = unname(tree_nms["ht"]),
    load_dll = TRUE
  )
  res = as.data.frame(res)
  #map NVEL output onto the tidy names used across vol_source options
  if("MCFV_NET"    %in% names(res)) res[["vol_cf_net"]] = res[["MCFV_NET"]]
  if("BFV_NET"     %in% names(res)) res[["vol_bf_net"]] = res[["BFV_NET"]]
  if("TBIODRY_LBS" %in% names(res)) res[["dry_bio"]]    = res[["TBIODRY_LBS"]]
  res
}

# ---------------------------------------------------------------------------
# fia_nvel_biomass(): compile_trees plug-in wrapping NVEL_biomass()
# ---------------------------------------------------------------------------

#'@export
#'@rdname fia_compile
fia_nvel_biomass = function(x, tree_nms, ...){
  res = NVEL_biomass(
    dfTL   = as.data.frame(x),
    spcdNm = unname(tree_nms["spp"]),
    dbhNm  = unname(tree_nms["dbh"]),
    htNm   = unname(tree_nms["ht"]),
    load_dll = TRUE,
    ...
  )
  as.data.frame(res)
}

# ---------------------------------------------------------------------------
# fia_compile_trees(): compile_trees() preconfigured for FIADB
# ---------------------------------------------------------------------------

#'@export
#'@rdname fia_compile
fia_compile_trees = function(df_tree,
                             tree_nms   = .fia_tree_nms,
                             vol_source = c("fiadb", "nvel"),
                             db_breaks  = c(seq(1, 29, 2), 999),
                             fns_compute = NULL,
                             region = 6, forest = NA,
                             ...){
  vol_source = match.arg(vol_source)
  df_tree = as.data.frame(df_tree)

  #drop trees with no per-acre expansion (NA TPA_UNADJ -> NA TPA_EXP): FIADB has
  #such records (non-tally / volume-excluded trees) and they cannot carry a
  #per-acre weight. EVALIDator likewise sums only trees with a valid TPA.
  exp_col = unname(tree_nms["expansion"])
  if(!is.na(exp_col) && exp_col %in% names(df_tree)){
    bad = is.na(df_tree[[exp_col]])
    if(any(bad)){
      warning("fia_compile_trees: dropping ", sum(bad),
              " tree(s) with missing expansion (", exp_col, ")")
      df_tree = df_tree[!bad, , drop = FALSE]
    }
  }

  #a per-tree constant so plot_wtsum can produce trees-per-acre (stems)
  if(!"stems" %in% names(df_tree)) df_tree[["stems"]] = 1

  if(is.null(fns_compute)) fns_compute = list(ba_ft, dbcl, fia_vol)

  compile_trees(
    df_tree     = df_tree,
    tree_nms    = tree_nms,
    fns_compute = fns_compute,
    #forwarded to the compute functions via compile_trees(...)
    db_breaks   = db_breaks,
    vol_source  = vol_source,
    region      = region,
    forest      = forest,
    ...
  )
}

# ---------------------------------------------------------------------------
# fia_compile_plots(): compile_plots() preconfigured for FIA, with zero-fill
# ---------------------------------------------------------------------------

# collapse a condition-level fia_plots() frame to one row per plot, carrying the
# design columns (constant within a plot) and total forested proportion.
.fia_plot_design = function(df_plot){
  df = as.data.frame(df_plot)
  if(!"PLT_CN" %in% names(df)) stop("df_plot must contain a 'PLT_CN' column")

  const_cols = intersect(
    c("EVALID", "STATECD", "COUNTYCD", "UNITCD", "PLOT", "INVYR", "MEASYEAR",
      "PREV_PLT_CN", "ESTN_UNIT", "STRATUM_CN", "STRATUMCD", "EXPNS",
      "ADJ_FACTOR_SUBP", "ADJ_FACTOR_MICR", "ADJ_FACTOR_MACR", "AREA_USED",
      "LAT", "LON", "MACRO_BREAKPOINT_DIA"),
    names(df))

  #design columns are constant within a plot -> take the first condition row
  out = df[!duplicated(df$PLT_CN), c("PLT_CN", const_cols), drop = FALSE]

  #adjusted forested proportion per plot: sum over forest conditions of
  #CONDPROP_UNADJ * (condition adjustment factor, by PROP_BASIS). The adjustment
  #is what makes forest-area / per-acre estimates match FIADB EVALIDator.
  if(all(c("CONDPROP_UNADJ", "COND_STATUS_CD") %in% names(df))){
    fr = df[!is.na(df$COND_STATUS_CD) & df$COND_STATUS_CD == 1, , drop = FALSE]
    fa = setNames(numeric(0), character(0))
    if(nrow(fr)){
      adj = if("ADJ_FACTOR_SUBP" %in% names(fr)) fr$ADJ_FACTOR_SUBP else rep(1, nrow(fr))
      if(all(c("PROP_BASIS", "ADJ_FACTOR_MACR") %in% names(fr))){
        macr = !is.na(fr$PROP_BASIS) & fr$PROP_BASIS == "MACR"
        adj[macr] = fr$ADJ_FACTOR_MACR[macr]
      }
      fa = tapply(fr$CONDPROP_UNADJ * adj, as.character(fr$PLT_CN), sum, na.rm = TRUE)
    }
    out$CONDPROP_FOR = as.numeric(fa[as.character(out$PLT_CN)])
    out$CONDPROP_FOR[is.na(out$CONDPROP_FOR)] = 0   # fully nonforest -> 0
  }
  rownames(out) = NULL
  out
}

#'@export
#'@rdname fia_compile
fia_compile_plots = function(df_tree, df_plot,
                             tree_nms = list(plot_ids = "PLT_CN",
                                             tree_ids = "TRE_CN",
                                             dbh = "DIA", ht = "HT",
                                             spp = "SPCD", expansion = "TPA_EXP"),
                             plot_nms = list(plot_ids = "PLT_CN"),
                             fns_compute = NULL,
                             sum_nms = NULL,
                             ...){

  df_tree = as.data.frame(df_tree)

  #Default to plot_wtsum only. compile_plots drops an ENTIRE plot if any compute
  #function errors, and plot_lor_qmd fails on plots whose tree heights are all NA
  #(common in real FIADB, where HT is frequently modeled/absent) -- which would
  #silently drop those plots and bias estimates. Add plot_lor_qmd via fns_compute
  #when heights are populated and qmd/Lorey's height are wanted.
  if(is.null(fns_compute)) fns_compute = list(plot_wtsum)

  #auto-detect tidy per-tree metric columns (+ component columns) to weight-sum
  if(is.null(sum_nms)){
    pat = paste0("^(", paste(.fia_metric_prefixes, collapse = "|"), ")")
    sum_nms = grep(pat, names(df_tree), value = TRUE)
  }
  if(length(sum_nms) == 0)
    warning("fia_compile_plots: no metric columns found to sum; ",
            "did you run fia_compile_trees() first?")

  #compile per-acre values for plots that carry trees (df_plot = NA path)
  compiled = compile_plots(
    df_tree     = df_tree,
    df_plot     = NA,
    tree_nms    = tree_nms,
    plot_nms    = plot_nms,
    tree_filter = NA,
    plot_filter = NA,
    dir_out     = NA,
    fns_compute = fns_compute,
    return      = TRUE,
    nclus       = 1,
    sum_nms     = sum_nms,
    ...
  )
  compiled = as.data.frame(compiled)

  #zero-fill against the full evaluation plot list + attach design columns
  design = .fia_plot_design(df_plot)

  metric_cols = setdiff(names(compiled), tree_nms[["plot_ids"]])
  out = merge(design, compiled, by = tree_nms[["plot_ids"]], all.x = TRUE)
  for(cc in metric_cols){
    if(is.numeric(out[[cc]])) out[[cc]][is.na(out[[cc]])] = 0
  }

  out = out[order(out[[tree_nms[["plot_ids"]]]]), , drop = FALSE]
  rownames(out) = NULL
  out
}
