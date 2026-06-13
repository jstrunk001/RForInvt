#'@name fia_fvs
#'
#'@title FVS bridge for FIA data (Phase 4 projection layer)
#'
#'@description
#'  Three functions that connect the FIA data layer (\code{fia_plots()} /
#'  \code{fia_trees()}) to the Forest Vegetation Simulator wrappers
#'  (\code{fvs_make_keyfiles()} / \code{fvs_run()}) so a set of FIA plots can be
#'  projected through FVS and the projected tree lists flow back through the
#'  same compile + estimate pipeline as the current inventory.
#'
#'  \itemize{
#'    \item \code{fia_fvs_input()} - build an FVS-ready SQLite (tables
#'          \code{FVS_StandInit} / \code{FVS_TreeInit}) for the plots in an
#'          evaluation. If the source already ships the DataMart FVS tables
#'          (\code{FVS_STANDINIT_PLOT} / \code{FVS_TREEINIT_PLOT}) they are
#'          filtered to the chosen plots; otherwise the tables are built from
#'          \code{PLOT}/\code{TREE}/\code{COND}.
#'    \item \code{fia_fvs_run()} - chain
#'          \code{fvs_prototype_params()} -> \code{fvs_prototype_keyfile()} ->
#'          \code{fvs_make_keyfiles()} -> \code{fvs_run()} with FIA stand ids
#'          (\code{STAND_CN = PLT_CN}); returns the path to the merged FVS
#'          output database.
#'    \item \code{fia_fvs_compile()} - read an FVS output database
#'          (\code{FVS_TreeList}) for a projection year and rename columns onto
#'          the \code{fia_trees()} / \code{fia_compile_trees()} conventions, so a
#'          projected tree list is interchangeable with a measured one.
#'  }
#'
#'@details
#'  This makes "current estimate" and "projected estimate" symmetrical: both end
#'  in \code{fia_compile_trees()} -> \code{fia_compile_plots()} ->
#'  \code{fia_estimate()}. A projected tree list from \code{fia_fvs_compile()}
#'  carries \code{PLT_CN}, \code{SPCD}, \code{DIA}, \code{HT}, \code{TPA_EXP} and
#'  the FIADB-style volume columns, so it can be passed straight to
#'  \code{fia_compile_trees(vol_source = "fiadb")}. Design columns (EXPNS,
#'  adjustment/expansion factors, estimation unit) still come from
#'  \code{fia_plots()} at the \code{fia_compile_plots()} step.
#'
#'  \strong{Stand = plot.} Each FIA plot becomes one FVS stand with
#'  \code{STAND_CN = PLT_CN} (the unique key) and \code{STAND_ID} from the human
#'  plot number when available. Per-tree expansion is carried in the FVS
#'  \code{TREE_COUNT} column (= \code{TPA_UNADJ}), so FVS reproduces the FIA
#'  per-acre tree density directly and the plot-design fields
#'  (\code{INV_PLOT_SIZE}, \code{BASAL_AREA_FACTOR}, \code{BRK_DBH}) are nominal.
#'
#'  \strong{Species codes.} The build-from-FIA path writes the two-letter FVS
#'  alpha code (\code{"DF"}, \code{"WH"}, ...) into the FVS \code{SPECIES} column
#'  using a built-in Pacific Northwest Coast / westside (\code{PN}/\code{WC})
#'  crosswalk, and keeps the FIA numeric code in a bonus \code{SPCD} column. SPCD
#'  not covered by the crosswalk fall back to the FIA numeric (which FVS also
#'  accepts). Supply \code{spp_map} (a named character vector
#'  \code{c("202" = "DF", ...)} or a two-column data.frame of \code{SPCD} -> FVS
#'  code) to override or extend the crosswalk - useful for eastside / non-PN
#'  variants. \code{fia_fvs_compile()} carries both back: the FIA code in
#'  \code{SPCD} and the FVS alpha code in \code{SPP_FVS}.
#'
#'  \strong{Running FVS} requires a local FVS variant executable (e.g.
#'  \code{FVSpn.exe} for the Pacific Northwest Coast variant); the bundled demo
#'  fixture cannot exercise an actual projection, so \code{fia_fvs_run()}
#'  examples are wrapped in \code{\\dontrun}.
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
#'1.0 \tab 6/12/2026 Created (Phase 4 FIA FVS bridge) \cr
#'}
#'
#'@author Jacob Strunk <someone@@somewhere.com>
#'
#'@param db a FIA data source accepted by \code{fia_db()} (a \code{fia_db}
#'  handle, a SQLite path, a DBI connection, or a named list of data.frames).
#'@param evalid one or more EVALIDs, or the data.frame returned by
#'  \code{fia_evalid()}.
#'@param dir_out directory to write the FVS input database into (created if
#'  needed); defaults to a session temp directory.
#'@param db_name file name of the FVS input database written in \code{dir_out}.
#'@param variant FVS variant code stored in \code{FVS_StandInit.VARIANT}
#'  (e.g. \code{"PN"} = Pacific Northwest Coast, \code{"WC"} = West Cascades).
#'@param spp_map optional FIA \code{SPCD} -> FVS alpha-code map that overrides /
#'  extends the built-in westside crosswalk: a named character vector (names are
#'  SPCD as character, e.g. \code{c("202" = "DF")}) or a two-column data.frame
#'  (\code{SPCD}, FVS code). \code{NULL} uses the built-in crosswalk alone.
#'@param tree_filter tree \code{WHERE}-clause forwarded to \code{fia_trees()}
#'  when building from \code{TREE} (default keeps live trees).
#'@param cond_filter condition \code{WHERE}-clause forwarded to
#'  \code{fia_plots()} when building from \code{COND}.
#'@param overwrite if \code{TRUE} (default) replace any existing
#'  \code{FVS_StandInit}/\code{FVS_TreeInit} tables in the output database.
#'@param db_fvs_in path to an FVS input database produced by
#'  \code{fia_fvs_input()} (or any FVS-ready SQLite).
#'@param fvs_path path to the FVS variant executable (e.g.
#'  \code{"C:/FVSbin/FVSpn.exe"}).
#'@param stand_table,tree_table table names of the stand/tree init tables in
#'  \code{db_fvs_in}.
#'@param numcycle number of projection cycles passed to the keyword file.
#'@param timeint cycle length (years) passed to the keyword file.
#'@param processing_dir working directory for keyfiles and per-run output
#'  databases (passed to \code{fvs_make_keyfiles()}).
#'@param db_merge file name of the merged FVS output database.
#'@param cluster optional \code{parallel} cluster object for parallel FVS runs.
#'@param db_fvs_out path to a merged FVS output database (from
#'  \code{fia_fvs_run()} or an FVS GUI run).
#'@param treelist_table name of the FVS tree-list output table to read
#'  (case-insensitive; default \code{"FVS_TreeList"}).
#'@param year projection year(s) to keep from the tree list; \code{NULL} keeps
#'  all years (a \code{YEAR} column is always returned).
#'@param ... additional arguments forwarded to the underlying functions
#'  (\code{fia_trees}/\code{fia_plots}, \code{fvs_make_keyfiles}/\code{fvs_run}).
#'
#'@return
#'  \code{fia_fvs_input()} returns (invisibly) the path to the FVS input
#'  database, with attributes \code{variant}, \code{stand_table},
#'  \code{tree_table}, \code{n_stands}, \code{n_trees}.
#'  \code{fia_fvs_run()} returns (invisibly) the path to the merged FVS output
#'  database. \code{fia_fvs_compile()} returns a tree-level data.frame named to
#'  match \code{fia_trees()} output (\code{PLT_CN}, \code{SPCD}, \code{DIA},
#'  \code{HT}, \code{TPA_EXP}, \code{VOLCFNET}, ...), plus a \code{YEAR} column
#'  and an \code{SPP_FVS} FVS alpha-code column.
#'
#'@examples
#'\donttest{
#'  db_path <- system.file("extdata", "FIADB_demo.db", package = "RForInvt")
#'  if (nzchar(db_path)) {
#'    db <- fia_db(db_path)
#'    ev <- fia_evalid(db, statecd = 53, eval_type = "EXPVOL", most_recent = TRUE)
#'
#'    #build an FVS input db from PLOT/TREE/COND (the demo lacks FVS tables)
#'    fvs_in <- fia_fvs_input(db, evalid = ev, dir_out = tempdir(),
#'                            variant = "PN")
#'    fvs_in
#'  }
#'}
#'\dontrun{
#'  #run FVS (needs a local FVSpn.exe) and compile the projected tree list
#'  out_db <- fia_fvs_run(fvs_in, fvs_path = "C:/FVSbin/FVSpn.exe",
#'                        numcycle = 1, timeint = 10)
#'  tr_proj <- fia_fvs_compile(out_db, year = 2031)
#'
#'  #projected tree list flows through the same compile pipeline
#'  pl     <- fia_plots(db, evalid = ev)
#'  tr_c   <- fia_compile_trees(tr_proj, vol_source = "fiadb")
#'  pl_c   <- fia_compile_plots(tr_c, pl)
#'}
#'
#'@import DBI RSQLite
#'
#'@seealso \code{\link{fia_trees}}\cr \code{\link{fia_compile_trees}}\cr
#'  \code{\link{fvs_make_keyfiles}}\cr \code{\link{fvs_run}}\cr
NULL

# ---------------------------------------------------------------------------
# helpers
# ---------------------------------------------------------------------------

# the PLT_CNs assigned to one or more EVALIDs (the stands FVS will project).
.fia_evalid_plt_cns = function(src, ids){
  assgn = .fia_table(src, "POP_PLOT_STRATUM_ASSGN", "EVALID", ids)
  if(nrow(assgn) == 0) stop("no plots assigned to EVALID(s): ", paste(ids, collapse = ", "))
  unique(assgn$PLT_CN)
}

# FIA SPCD -> FVS alpha species-code crosswalk for the Pacific Northwest Coast /
# westside variants (PN, WC). These two-letter codes are what the DataMart
# FVS-ready tables carry in SPECIES. SPCD not listed here fall back to the FIA
# numeric code (which FVS also accepts); extend or override any entry via the
# spp_map argument. Source: FVS PN variant species table.
.fia_fvs_spp_xwalk = data.frame(
  SPCD = c(11, 15, 17, 19, 22, 42, 81, 93, 98, 108, 119, 122, 202, 211, 231,
           242, 263, 264, 312, 351, 375, 431, 492, 746, 747, 815, 920),
  FVS  = c("SF", "WF", "GF", "AF", "NF", "YC", "IC", "ES", "SS", "LP", "WP",
           "PP", "DF", "RW", "PY", "RC", "WH", "MH", "BM", "RA", "PB", "GC",
           "DG", "AS", "CW", "WO", "WI"),
  stringsAsFactors = FALSE
)

# coerce a named-vector or two-column-data.frame spp_map into a named character
# lookup (names = SPCD as character, values = FVS code).
.fia_spp_map_lut = function(spp_map){
  if(is.null(spp_map)) return(NULL)
  if(is.data.frame(spp_map)){
    if(ncol(spp_map) < 2) stop("spp_map data.frame needs two columns: SPCD, FVS code")
    return(stats::setNames(as.character(spp_map[[2]]), as.character(spp_map[[1]])))
  }
  stats::setNames(as.character(spp_map), as.character(names(spp_map)))
}

# FIA SPCD -> FVS alpha code: spp_map entries override/extend the built-in
# westside crosswalk; anything still unmapped falls back to the FIA numeric.
.fia_spp_alpha = function(spcd, spp_map = NULL){
  lut = stats::setNames(.fia_fvs_spp_xwalk$FVS, as.character(.fia_fvs_spp_xwalk$SPCD))
  user = .fia_spp_map_lut(spp_map)
  if(!is.null(user)) lut[names(user)] = user
  out = unname(lut[as.character(spcd)])
  out[is.na(out)] = as.character(spcd)[is.na(out)]
  out
}

# build FVS_StandInit / FVS_TreeInit from FIA PLOT/TREE/COND frames.
.fia_build_fvs_tables = function(pl, tr, variant, spp_map){

  #--- one stand per plot ----------------------------------------------------
  pl = as.data.frame(pl)
  keep_first = !duplicated(pl$PLT_CN)
  pld = pl[keep_first, , drop = FALSE]

  inv_year = if("MEASYEAR" %in% names(pld)) pld$MEASYEAR else pld$INVYR
  std_id   = if("PLOT" %in% names(pld)) pld$PLOT else seq_len(nrow(pld))

  getcol = function(df, nm, default = NA) if(nm %in% names(df)) df[[nm]] else rep(default, nrow(df))

  stand = data.frame(
    STAND_CN          = as.character(pld$PLT_CN),
    STAND_ID          = as.character(std_id),
    VARIANT           = variant,
    INV_YEAR          = inv_year,
    GROUPS            = "All_FIA_Plots",
    REGION            = getcol(pld, "RS_FVS_REGION", NA),
    FOREST            = getcol(pld, "RS_FVS_FOREST", NA),
    LATITUDE          = getcol(pld, "LAT"),
    LONGITUDE         = getcol(pld, "LON"),
    STATE             = getcol(pld, "STATECD"),
    COUNTY            = getcol(pld, "COUNTYCD"),
    AGE               = getcol(pld, "STDAGE", 0),
    ASPECT            = getcol(pld, "ASPECT", 0),
    SLOPE             = getcol(pld, "SLOPE", 0),
    ELEVATION         = getcol(pld, "ELEV", NA),
    BASAL_AREA_FACTOR = 0,    #fixed-area (negative BAF would be variable radius)
    INV_PLOT_SIZE     = 1,    #TREE_COUNT carries the per-acre expansion
    BRK_DBH           = 999,  #no large/small breakpoint; all on the fixed plot
    NUM_PLOTS         = 1,
    SITE_SPECIES      = getcol(pld, "SITE_SPECIES", NA),
    SITE_INDEX        = getcol(pld, "SITE_INDEX", NA),
    stringsAsFactors  = FALSE
  )

  #--- one record per tree ---------------------------------------------------
  tr = as.data.frame(tr)
  tree_id = if("TREE" %in% names(tr)) tr$TREE else seq_len(nrow(tr))
  tcount  = if("TPA_UNADJ" %in% names(tr)) tr$TPA_UNADJ else NA

  tree = data.frame(
    STAND_CN    = as.character(tr$PLT_CN),
    STAND_ID    = as.character(tr$PLT_CN),   #placeholder; reset from stand below
    PLOT_ID     = if("SUBP" %in% names(tr)) tr$SUBP else 1L,
    TREE_ID     = tree_id,
    TREE_COUNT  = tcount,
    HISTORY     = 1L,                        #live, standing (default fia_trees filter)
    SPECIES     = .fia_spp_alpha(tr$SPCD, spp_map),  #FVS alpha code (DF, WH, ...)
    SPCD        = tr$SPCD,                    #bonus: keep the FIA numeric code
    DIAMETER    = getcol(tr, "DIA"),
    HT          = getcol(tr, "HT"),
    stringsAsFactors = FALSE
  )

  #carry the human STAND_ID from the stand table onto the tree rows
  id_lut = stats::setNames(stand$STAND_ID, stand$STAND_CN)
  hit = tree$STAND_CN %in% names(id_lut)
  tree$STAND_ID[hit] = unname(id_lut[tree$STAND_CN[hit]])

  list(stand = stand, tree = tree)
}

# case-insensitive table/column lookups for FVS output (FVS varies the casing).
.fvs_find_table = function(con, name){
  tabs = DBI::dbListTables(con)
  hit = tabs[tolower(tabs) == tolower(name)]
  if(length(hit) == 0) return(NA_character_)
  hit[1]
}
.fvs_pick = function(df, candidates){
  lc = tolower(names(df))
  for(cand in candidates){
    i = which(lc == tolower(cand))
    if(length(i) > 0) return(names(df)[i[1]])
  }
  NA_character_
}

# ---------------------------------------------------------------------------
# fia_fvs_input()
# ---------------------------------------------------------------------------

#'@export
#'@rdname fia_fvs
fia_fvs_input = function(db, evalid, dir_out = tempdir(), variant = "PN",
                         db_name = "FVS_input.db", spp_map = NULL,
                         tree_filter = "STATUSCD == 1", cond_filter = NA,
                         overwrite = TRUE, ...){

  src = .fia_source(db); on.exit(.fia_close(src))
  ids = .fia_evalids(evalid)
  plt_cns = .fia_evalid_plt_cns(src, ids)

  has_fvs = .fia_has_table(src, "FVS_STANDINIT_PLOT") &&
            .fia_has_table(src, "FVS_TREEINIT_PLOT")

  if(has_fvs){
    #filter the DataMart FVS tables to the evaluation's plots (STAND_CN = PLT_CN)
    stand = .fia_table(src, "FVS_STANDINIT_PLOT", "STAND_CN", plt_cns)
    tree  = .fia_table(src, "FVS_TREEINIT_PLOT",  "STAND_CN", plt_cns)
  }else{
    #build from PLOT/TREE/COND
    pl = fia_plots(db, evalid = ids, cond_filter = cond_filter,
                   vars_keep = c("SLOPE", "ASPECT", "ELEV", "STDAGE",
                                 "SITE_SPECIES", "SITE_INDEX"))
    tr = fia_trees(db, evalid = ids, tree_filter = tree_filter)
    built = .fia_build_fvs_tables(pl, tr, variant = variant, spp_map = spp_map)
    stand = built$stand; tree = built$tree
  }

  if(!dir.exists(dir_out)) dir.create(dir_out, recursive = TRUE)
  out_path = file.path(dir_out, db_name)

  con = DBI::dbConnect(RSQLite::SQLite(), out_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "FVS_StandInit", stand, overwrite = overwrite, append = !overwrite)
  DBI::dbWriteTable(con, "FVS_TreeInit",  tree,  overwrite = overwrite, append = !overwrite)

  attr(out_path, "variant")     = variant
  attr(out_path, "stand_table") = "FVS_StandInit"
  attr(out_path, "tree_table")  = "FVS_TreeInit"
  attr(out_path, "n_stands")    = nrow(stand)
  attr(out_path, "n_trees")     = nrow(tree)
  message("fia_fvs_input: wrote ", nrow(stand), " stands / ", nrow(tree),
          " trees to ", out_path)
  invisible(out_path)
}

# ---------------------------------------------------------------------------
# fia_fvs_run()
# ---------------------------------------------------------------------------

#'@export
#'@rdname fia_fvs
fia_fvs_run = function(db_fvs_in, fvs_path,
                       variant = NULL,
                       stand_table = "FVS_StandInit",
                       tree_table  = "FVS_TreeInit",
                       numcycle = 1, timeint = 10,
                       processing_dir = file.path(tempdir(), "RForInvt_fvs"),
                       db_merge = "FVS_FIA_out.db",
                       cluster = NA, ...){

  if(missing(fvs_path) || length(fvs_path) != 1 || is.na(fvs_path) || !nzchar(fvs_path))
    stop("fvs_path must be the path to an FVS variant executable")

  #read the stand list (one FVS run parameter row per stand)
  con = DBI::dbConnect(RSQLite::SQLite(), db_fvs_in)
  on.exit(DBI::dbDisconnect(con))
  if(!DBI::dbExistsTable(con, stand_table))
    stop("stand table not found in db_fvs_in: ", stand_table)
  stands = DBI::dbGetQuery(con, paste0("select * from ", stand_table))
  DBI::dbDisconnect(con); on.exit()
  if(nrow(stands) == 0) stop("no stands in ", stand_table)

  inv_year = if("INV_YEAR" %in% names(stands)) stands$INV_YEAR else NA

  #assemble FVS run parameters (STAND_CN = PLT_CN is the unique stand key)
  param_df = fvs_prototype_params()
  param_df[seq_len(nrow(stands)), ] = NA
  param_df$std_cn      = as.character(stands$STAND_CN)
  param_df$std_id      = as.character(stands$STAND_ID)
  param_df$invyr       = inv_year
  param_df$timeint     = timeint
  param_df$numcycle    = numcycle
  param_df$input_db    = db_fvs_in
  param_df$fvs_path    = fvs_path
  param_df$tree_table  = tree_table
  param_df$stand_table = stand_table

  #prototype keyfile: select each stand by its (unique) Stand_CN; the StandCN
  #line is filled from the @std_cn@ token rather than defaulting to @std_id@.
  key_proto = fvs_prototype_keyfile(
    std_cn    = "@std_cn@",
    where_std = "WHERE Stand_CN = '%Stand_CN%'"
  )

  key_df = fvs_make_keyfiles(
    param_df,
    processing_dir = processing_dir,
    key_proto      = key_proto,
    cluster        = cluster,
    id             = "std_cn",
    ...
  )

  fvs_run(key_df, db_merge = db_merge, cluster = cluster, ...)

  #recompute the merged-db path the same way fvs_run() does
  out_db = if(db_merge == basename(db_merge))
    file.path(dirname(key_df$output_db[1]), db_merge) else db_merge
  message("fia_fvs_run: merged FVS output at ", out_db)
  invisible(out_db)
}

# ---------------------------------------------------------------------------
# fia_fvs_compile()
# ---------------------------------------------------------------------------

#'@export
#'@rdname fia_fvs
fia_fvs_compile = function(db_fvs_out, year = NULL,
                           treelist_table = "FVS_TreeList",
                           spp_map = NULL, ...){

  con = DBI::dbConnect(RSQLite::SQLite(), db_fvs_out)
  on.exit(DBI::dbDisconnect(con))

  tbl = .fvs_find_table(con, treelist_table)
  if(is.na(tbl))
    stop("tree-list table '", treelist_table, "' not found in ", db_fvs_out,
         " (tables: ", paste(DBI::dbListTables(con), collapse = ", "), ")")
  tl = DBI::dbGetQuery(con, paste0("select * from ", tbl))
  if(nrow(tl) == 0) stop("tree-list table '", tbl, "' is empty")

  #FVS output column names vary in case/spelling; pick the first match
  c_cn   = .fvs_pick(tl, c("StandCN", "Stand_CN"))
  c_id   = .fvs_pick(tl, c("StandID", "Stand_ID"))
  c_yr   = .fvs_pick(tl, c("Year"))
  c_tree = .fvs_pick(tl, c("TreeId", "TreeIndex", "TreeVal"))
  c_tpa  = .fvs_pick(tl, c("TPA"))
  c_dbh  = .fvs_pick(tl, c("DBH", "Dia"))
  c_ht   = .fvs_pick(tl, c("Ht", "Height"))
  c_spp  = .fvs_pick(tl, c("SpeciesFIA", "FIA"))
  c_sfvs = .fvs_pick(tl, c("SpeciesFVS", "Species"))
  c_vcf  = .fvs_pick(tl, c("MCuFt", "MerchCuFt"))
  c_vbf  = .fvs_pick(tl, c("BdFt", "MerchBdFt"))
  c_tcf  = .fvs_pick(tl, c("TCuFt", "TotCuFt"))

  if(is.na(c_cn)) stop("could not find a stand id (StandCN) column in ", tbl)
  if(is.na(c_yr)) stop("could not find a Year column in ", tbl)

  out = data.frame(
    PLT_CN = as.character(tl[[c_cn]]),
    YEAR   = tl[[c_yr]],
    stringsAsFactors = FALSE
  )
  if(!is.na(c_id))   out$STAND_ID   = as.character(tl[[c_id]])
  if(!is.na(c_tree)) out$TRE_CN     = paste(out$PLT_CN, tl[[c_tree]], tl[[c_yr]], sep = "_")
  if(!is.na(c_spp))  out$SPCD       = tl[[c_spp]]                  #FIA numeric
  #FVS alpha code: prefer the SpeciesFVS column, else derive from SPCD
  if(!is.na(c_sfvs)){
    out$SPP_FVS = as.character(tl[[c_sfvs]])
  }else if(!is.na(c_spp)){
    out$SPP_FVS = .fia_spp_alpha(tl[[c_spp]], spp_map)
  }
  if(!is.na(c_dbh))  out$DIA        = tl[[c_dbh]]
  if(!is.na(c_ht))   out$HT         = tl[[c_ht]]
  if(!is.na(c_tpa)){
    #FVS TPA is already per-acre for the stand (= plot), so it is the expansion
    out$TPA_UNADJ = tl[[c_tpa]]
    out$TPA_EXP   = tl[[c_tpa]]
  }
  if(!is.na(c_vcf)) out$VOLCFNET = tl[[c_vcf]]
  if(!is.na(c_vbf)) out$VOLCSNET = tl[[c_vbf]]
  if(!is.na(c_tcf)) out$VOLCFGRS = tl[[c_tcf]]

  if(!is.null(year)) out = out[out$YEAR %in% year, , drop = FALSE]
  if(nrow(out) == 0)
    warning("fia_fvs_compile: no tree-list rows for year(s) ",
            paste(year, collapse = ", "))

  out = out[order(out$YEAR, out$PLT_CN), , drop = FALSE]
  rownames(out) = NULL
  out
}
