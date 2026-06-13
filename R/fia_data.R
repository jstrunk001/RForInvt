#'@name fia_data
#'
#'@title Access FIADB data by evaluation (Phase 1 data layer)
#'
#'@description
#'  A small family of functions that take a FIA database (an FIADB SQLite from
#'  the FIA DataMart, an open \code{DBI} connection, or a named list of
#'  pre-loaded tables) and assemble analysis-ready plot and tree frames for a
#'  given FIA evaluation (EVALID). These are the inputs to the Phase 2
#'  compilation wrappers (\code{fia_compile_trees}, \code{fia_compile_plots}).
#'
#'  \itemize{
#'    \item \code{fia_db()} - wrap a path/connection/table-list in a light handle.
#'    \item \code{fia_evalid()} - look up evaluations (EVALID) for a state/years/type.
#'    \item \code{fia_plots()} - one row per condition with full post-stratification
#'          design columns (EXPNS, adjustment factors, estimation unit, stratum).
#'    \item \code{fia_trees()} - tree records joined to their plot/stratum, with the
#'          per-acre design expansion \code{TPA_EXP} already computed.
#'  }
#'
#'@details
#'  Estimation in FIA is organized around an \emph{evaluation}: the set of plots
#'  and post-stratification design (estimation units, strata, expansion factors,
#'  adjustment factors) identified by an EVALID. Selecting plots by EVALID -
#'  rather than by inventory year alone - and expanding each tree by
#'  \code{TPA_UNADJ} times the stratum adjustment factor are what make resulting
#'  estimates reproduce FIA EVALIDator. These functions own that bookkeeping so
#'  downstream compilation and estimation cannot get it wrong.
#'
#'  The tree expansion column \code{TPA_EXP} is the \emph{per-acre} adjusted
#'  expansion, \code{TPA_UNADJ * ADJ_FACTOR}, where the adjustment factor is
#'  chosen per tree by the plot design it was tallied on:
#'  \itemize{
#'    \item \code{DIA < 5.0} -> microplot factor (\code{ADJ_FACTOR_MICR})
#'    \item \code{DIA >= MACRO_BREAKPOINT_DIA} -> macroplot factor (\code{ADJ_FACTOR_MACR})
#'    \item otherwise -> subplot factor (\code{ADJ_FACTOR_SUBP})
#'  }
#'  The stratum expansion \code{EXPNS} (acres represented per plot) is carried on
#'  the plot/condition frame and applied at the estimation step, not here.
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
#'1.0 \tab 6/11/2026 Created (Phase 1 FIA data layer) \cr
#'}
#'
#'@author Jacob Strunk <someone@@somewhere.com>
#'
#'@param db a FIA data source: a \code{fia_db} handle (see \code{fia_db()}), a
#'  path to an FIADB SQLite file, an open \code{DBI} connection, or a named list
#'  of data.frames holding the FIADB tables (e.g. \code{list(PLOT=, COND=, TREE=,
#'  POP_STRATUM=, POP_PLOT_STRATUM_ASSGN=, ...)}).
#'@param path path to an FIADB SQLite file (\code{fia_db}); ignored when
#'  \code{con} or \code{tables} is supplied.
#'@param con an open \code{DBI} connection to use instead of \code{path}.
#'@param tables a named list of data.frames to use as the source instead of a
#'  database.
#'@param statecd FIA state FIPS code(s) to filter evaluations (e.g. 53 = WA,
#'  44 = RI).
#'@param years evaluation end inventory year(s) to keep; \code{NULL} keeps all.
#'@param eval_type evaluation type(s) following \code{POP_EVAL_TYP.EVAL_TYP},
#'  e.g. \code{"EXPVOL"} (volume), \code{"EXPCURR"} (area/current),
#'  \code{"EXPGROW"}/\code{"EXPREMV"}/\code{"EXPMORT"} (growth-removal-mortality).
#'  Case-insensitive; a few aliases are accepted (\code{"vol"}, \code{"area"},
#'  \code{"grow"}, \code{"mort"}, \code{"remv"}).
#'@param most_recent if \code{TRUE}, keep only the most recent (max end year)
#'  evaluation per state and type.
#'@param evalid one or more EVALIDs, or the data.frame returned by
#'  \code{fia_evalid()} (its \code{EVALID} column is used).
#'@param cond_filter optional SQL \code{WHERE}-clause string (e.g.
#'  \code{"COND_STATUS_CD == 1"}) applied to the assembled condition frame;
#'  \code{NA} for no filter.
#'@param tree_filter optional SQL \code{WHERE}-clause string applied to the
#'  assembled tree frame; default keeps live trees (\code{"STATUSCD == 1"}),
#'  \code{NA} for no filter.
#'@param vars_keep optional character vector of extra source columns to retain
#'  in the output beyond the standard set.
#'@param grm if \code{TRUE}, additionally join \code{TREE_GRM_COMPONENT} /
#'  \code{TREE_GRM_MIDPT} for growth-removal-mortality work (requires those
#'  tables in the source).
#'@param ... unused; reserved for future arguments.
#'
#'@return
#'  \code{fia_db()} returns a \code{fia_db} handle. \code{fia_evalid()} returns a
#'  data.frame of evaluations. \code{fia_plots()} returns a condition-level
#'  data.frame (one row per \code{PLT_CN} x \code{CONDID}) with design columns.
#'  \code{fia_trees()} returns a tree-level data.frame with \code{TPA_EXP}.
#'
#'@examples
#'\donttest{
#'  db_path <- system.file("extdata", "FIADB_demo.db", package = "RForInvt")
#'  if (nzchar(db_path)) {
#'    db <- fia_db(db_path)
#'
#'    #most recent volume evaluation for the (synthetic) state in the demo db
#'    ev <- fia_evalid(db, statecd = 53, eval_type = "EXPVOL", most_recent = TRUE)
#'    ev
#'
#'    #condition-level plot frame with design columns
#'    pl <- fia_plots(db, evalid = ev)
#'    head(pl)
#'
#'    #live-tree frame with per-acre expansion TPA_EXP = TPA_UNADJ * ADJ_FACTOR
#'    tr <- fia_trees(db, evalid = ev)
#'    head(tr[, c("PLT_CN", "SPCD", "DIA", "TPA_UNADJ", "TPA_EXP")])
#'  }
#'}
#'
#'@import DBI RSQLite sqldf
#'
#'@seealso \code{\link{compile_trees}}\cr \code{\link{compile_plots}}\cr
NULL

# ---------------------------------------------------------------------------
# source abstraction: resolve a `db` argument into something we can query, and
# expose a single table fetcher so SQLite and in-memory list sources share one
# downstream code path.
# ---------------------------------------------------------------------------

#'@export
#'@rdname fia_data
fia_db = function(path = NULL, con = NULL, tables = NULL){
  if(!is.null(tables)){
    if(!is.list(tables) || is.null(names(tables))) stop("'tables' must be a named list of data.frames")
    obj = list(kind = "list", tables = tables)
  }else if(!is.null(con)){
    if(!inherits(con, "DBIConnection")) stop("'con' must be a DBI connection")
    obj = list(kind = "con", con = con, own = FALSE)
  }else if(!is.null(path)){
    if(!file.exists(path)) stop("database file not found: ", path)
    obj = list(kind = "sqlite", path = path)
  }else{
    stop("supply one of 'path', 'con', or 'tables'")
  }
  class(obj) = "fia_db"
  obj
}

#'@export
print.fia_db = function(x, ...){
  msg = switch(x$kind,
               list   = paste0("<fia_db: ", length(x$tables), " in-memory tables>"),
               con    = "<fia_db: open DBI connection>",
               sqlite = paste0("<fia_db: ", x$path, ">"))
  cat(msg, "\n")
  invisible(x)
}

# normalize any accepted `db` argument into an internal source object.
.fia_source = function(db){
  if(inherits(db, "fia_db")){
    if(db$kind == "sqlite") return(list(kind = "con", con = DBI::dbConnect(RSQLite::SQLite(), db$path), own = TRUE))
    if(db$kind == "con")    return(list(kind = "con", con = db$con, own = FALSE))
    if(db$kind == "list")   return(list(kind = "list", tables = db$tables))
  }
  if(inherits(db, "DBIConnection")) return(list(kind = "con", con = db, own = FALSE))
  if(is.character(db) && length(db) == 1){
    if(!file.exists(db)) stop("database file not found: ", db)
    return(list(kind = "con", con = DBI::dbConnect(RSQLite::SQLite(), db), own = TRUE))
  }
  if(is.list(db) && !is.null(names(db)) && all(sapply(db, is.data.frame)))
    return(list(kind = "list", tables = db))
  stop("'db' must be a fia_db handle, a SQLite path, a DBI connection, or a named list of data.frames")
}

.fia_close = function(src){
  if(src$kind == "con" && isTRUE(src$own)) try(DBI::dbDisconnect(src$con), silent = TRUE)
  invisible(NULL)
}

# fetch a table, optionally restricted to rows where `where_col` is in `where_in`.
.fia_table = function(src, table, where_col = NULL, where_in = NULL){
  if(src$kind == "list"){
    if(!table %in% names(src$tables)) stop("table not found in source list: ", table)
    df = src$tables[[table]]
    if(!is.null(where_col) && !is.null(where_in)) df = df[df[[where_col]] %in% where_in, , drop = FALSE]
    return(df)
  }
  # SQLite / DBI
  if(!DBI::dbExistsTable(src$con, table)) stop("table not found in database: ", table)
  if(is.null(where_col) || is.null(where_in)){
    return(DBI::dbGetQuery(src$con, paste0("select * from ", table)))
  }
  where_in = unique(where_in)
  if(length(where_in) == 0) return(DBI::dbGetQuery(src$con, paste0("select * from ", table, " where 1 = 0")))
  # chunk to stay well under SQLite's expression/variable limits
  chunks = split(where_in, ceiling(seq_along(where_in) / 900))
  is_chr = is.character(where_in)
  parts = lapply(chunks, function(ch){
    vals = if(is_chr) paste0("'", gsub("'", "''", ch), "'") else as.character(ch)
    DBI::dbGetQuery(src$con, paste0("select * from ", table, " where ", where_col,
                                    " in (", paste(vals, collapse = ","), ")"))
  })
  do.call(rbind, parts)
}

# does the source have a given table?
.fia_has_table = function(src, table){
  if(src$kind == "list") return(table %in% names(src$tables))
  DBI::dbExistsTable(src$con, table)
}

# normalize the `evalid` argument (integer vector or fia_evalid() data.frame).
.fia_evalids = function(evalid){
  if(is.data.frame(evalid)){
    if(!"EVALID" %in% names(evalid)) stop("evalid data.frame must contain an 'EVALID' column")
    return(unique(evalid$EVALID))
  }
  unique(evalid)
}

# coerce the named columns (those present) to numeric. Real FIADB SQLite can
# hand back numeric columns as character (e.g. mostly-NULL columns), which breaks
# numeric comparisons/arithmetic downstream.
.fia_as_numeric = function(df, cols){
  for(cc in intersect(cols, names(df))){
    if(!is.numeric(df[[cc]])) df[[cc]] = suppressWarnings(as.numeric(as.character(df[[cc]])))
  }
  df
}

# apply an optional SQL WHERE-clause string to a data.frame via sqldf.
.fia_apply_filter = function(df, where, alias = "df"){
  if(length(where) == 0 || is.na(where[1]) || !nzchar(where[1])) return(df)
  assign(alias, df)
  sqldf::sqldf(paste0("select * from ", alias, " where ", where[1]), envir = environment())
}

# ---------------------------------------------------------------------------
# fia_evalid()
# ---------------------------------------------------------------------------

#'@export
#'@rdname fia_data
fia_evalid = function(db, statecd = NULL, years = NULL,
                      eval_type = "EXPVOL", most_recent = FALSE, ...){

  src = .fia_source(db); on.exit(.fia_close(src))

  pe  = .fia_table(src, "POP_EVAL")
  pet = .fia_table(src, "POP_EVAL_TYP")

  #join eval to its type(s); POP_EVAL.CN <-> POP_EVAL_TYP.EVAL_CN
  ev = merge(pe, pet, by.x = "CN", by.y = "EVAL_CN", all.x = TRUE)
  names(ev)[names(ev) == "CN"] = "EVAL_CN"

  #normalize requested eval types (aliases -> canonical EVAL_TYP)
  alias = c(vol = "EXPVOL", area = "EXPCURR", curr = "EXPCURR", grow = "EXPGROW",
            mort = "EXPMORT", remv = "EXPREMV", all = "EXPALL")
  et = toupper(eval_type)
  lc = tolower(eval_type)
  et[lc %in% names(alias)] = alias[lc[lc %in% names(alias)]]

  ev = ev[toupper(ev$EVAL_TYP) %in% et, , drop = FALSE]
  if(!is.null(statecd)) ev = ev[ev$STATECD %in% statecd, , drop = FALSE]
  if(!is.null(years))   ev = ev[ev$END_INVYR %in% years, , drop = FALSE]

  if(most_recent && nrow(ev) > 0){
    ord = ev[order(ev$STATECD, ev$EVAL_TYP, -ev$END_INVYR), , drop = FALSE]
    keep = !duplicated(ord[, c("STATECD", "EVAL_TYP")])
    ev = ord[keep, , drop = FALSE]
  }

  cols = c("EVALID", "EVAL_CN", "EVAL_TYP", "STATECD", "START_INVYR",
           "END_INVYR", "LOCATION_NM")
  ev = ev[, intersect(cols, names(ev)), drop = FALSE]
  ev = ev[order(ev$EVALID), , drop = FALSE]
  rownames(ev) = NULL
  ev
}

# ---------------------------------------------------------------------------
# fia_plots() : one row per condition with full design columns
# ---------------------------------------------------------------------------

#'@export
#'@rdname fia_data
fia_plots = function(db, evalid, cond_filter = NA, vars_keep = NULL, ...){

  src = .fia_source(db); on.exit(.fia_close(src))
  ids = .fia_evalids(evalid)

  #plots assigned to these evaluations
  assgn = .fia_table(src, "POP_PLOT_STRATUM_ASSGN", "EVALID", ids)
  if(nrow(assgn) == 0) stop("no plots assigned to EVALID(s): ", paste(ids, collapse = ", "))

  #strata design (expansion + adjustment factors)
  strat = .fia_table(src, "POP_STRATUM", "CN", assgn$STRATUM_CN)
  #plots + conditions for the assigned PLT_CNs
  plot  = .fia_table(src, "PLOT", "CN", assgn$PLT_CN)
  cond  = .fia_table(src, "COND", "PLT_CN", assgn$PLT_CN)

  #--- merge: cond <- plot <- assgn <- stratum -------------------------------
  #In FIADB, COND also carries plot identifiers (STATECD/COUNTYCD/PLOT/INVYR/
  #UNITCD). Only pull the PLOT columns COND lacks, so the merge doesn't create
  #COUNTYCD.x/.y and drop the plain identifier columns (which by= grouping needs).
  plot_keep = c("CN", "STATECD", "UNITCD", "COUNTYCD", "PLOT", "INVYR",
                "MEASYEAR", "PREV_PLT_CN", "LAT", "LON", "MACRO_BREAKPOINT_DIA")
  plot_keep = intersect(plot_keep, names(plot))
  plot_keep = c("CN", setdiff(plot_keep, c("CN", names(cond))))
  pl = plot[, plot_keep, drop = FALSE]
  names(pl)[names(pl) == "CN"] = "PLT_CN"

  out = merge(cond, pl, by = "PLT_CN", all.x = TRUE)

  assgn_keep = intersect(c("PLT_CN", "EVALID", "STRATUM_CN", "ESTN_UNIT", "STRATUMCD"), names(assgn))
  out = merge(out, assgn[, assgn_keep, drop = FALSE], by = "PLT_CN", all.x = TRUE)

  strat_keep = intersect(c("CN", "EXPNS", "ADJ_FACTOR_SUBP", "ADJ_FACTOR_MICR",
                           "ADJ_FACTOR_MACR", "AREA_USED", "ESTN_UNIT_CN"), names(strat))
  st = strat[, strat_keep, drop = FALSE]
  names(st)[names(st) == "CN"] = "STRATUM_CN"
  out = merge(out, st, by = "STRATUM_CN", all.x = TRUE)

  #rename COND.CN -> COND_CN for clarity (PLT_CN is the join key, not CN)
  if("CN" %in% names(out)) names(out)[names(out) == "CN"] = "COND_CN"

  #optional filter on the assembled condition frame
  out = .fia_apply_filter(out, cond_filter, alias = "df_cond")

  #coerce numeric design columns (real FIADB SQLite may return some as character)
  out = .fia_as_numeric(out, c("CONDPROP_UNADJ", "EXPNS", "ADJ_FACTOR_SUBP",
                               "ADJ_FACTOR_MICR", "ADJ_FACTOR_MACR", "AREA_USED",
                               "MACRO_BREAKPOINT_DIA", "LAT", "LON"))

  #standard column ordering, then any extras the caller asked to keep
  std = c("EVALID", "PLT_CN", "COND_CN", "CONDID", "COND_STATUS_CD",
          "CONDPROP_UNADJ", "PROP_BASIS", "STATECD", "COUNTYCD", "UNITCD",
          "PLOT", "INVYR", "MEASYEAR", "PREV_PLT_CN", "ESTN_UNIT", "STRATUM_CN",
          "STRATUMCD", "EXPNS", "ADJ_FACTOR_SUBP", "ADJ_FACTOR_MICR",
          "ADJ_FACTOR_MACR", "AREA_USED", "LAT", "LON", "MACRO_BREAKPOINT_DIA")
  keep = c(intersect(std, names(out)), intersect(vars_keep, names(out)))
  out = out[, unique(keep), drop = FALSE]
  out = out[order(out$EVALID, out$PLT_CN, out$CONDID), , drop = FALSE]
  rownames(out) = NULL
  out
}

# ---------------------------------------------------------------------------
# fia_trees() : tree records with per-acre design expansion TPA_EXP
# ---------------------------------------------------------------------------

#'@export
#'@rdname fia_data
fia_trees = function(db, evalid, tree_filter = "STATUSCD == 1",
                     vars_keep = NULL, grm = FALSE, ...){

  src = .fia_source(db); on.exit(.fia_close(src))
  ids = .fia_evalids(evalid)

  assgn = .fia_table(src, "POP_PLOT_STRATUM_ASSGN", "EVALID", ids)
  if(nrow(assgn) == 0) stop("no plots assigned to EVALID(s): ", paste(ids, collapse = ", "))

  strat = .fia_table(src, "POP_STRATUM", "CN", assgn$STRATUM_CN)
  plot  = .fia_table(src, "PLOT", "CN", assgn$PLT_CN)
  tree  = .fia_table(src, "TREE", "PLT_CN", assgn$PLT_CN)

  #apply tree filter (default: live trees) before the joins
  tree = .fia_apply_filter(tree, tree_filter, alias = "df_tree")
  if(nrow(tree) == 0){
    warning("no trees remain after tree_filter")
  }

  #attach plot design info (macro breakpoint, county, year). TREE also carries
  #plot identifiers in FIADB, so only pull PLOT columns TREE lacks (e.g.
  #MACRO_BREAKPOINT_DIA, MEASYEAR) to avoid COUNTYCD.x/.y collisions.
  plot_keep = intersect(c("CN", "COUNTYCD", "INVYR", "MEASYEAR",
                          "MACRO_BREAKPOINT_DIA", "STATECD"), names(plot))
  plot_keep = c("CN", setdiff(plot_keep, c("CN", names(tree))))
  pl = plot[, plot_keep, drop = FALSE]
  names(pl)[names(pl) == "CN"] = "PLT_CN"
  out = merge(tree, pl, by = "PLT_CN", all.x = TRUE)

  #attach assignment + stratum (adjustment factors)
  assgn_keep = intersect(c("PLT_CN", "EVALID", "STRATUM_CN", "ESTN_UNIT", "STRATUMCD"), names(assgn))
  out = merge(out, assgn[, assgn_keep, drop = FALSE], by = "PLT_CN", all.x = TRUE)

  strat_keep = intersect(c("CN", "EXPNS", "ADJ_FACTOR_SUBP", "ADJ_FACTOR_MICR",
                           "ADJ_FACTOR_MACR"), names(strat))
  st = strat[, strat_keep, drop = FALSE]
  names(st)[names(st) == "CN"] = "STRATUM_CN"
  out = merge(out, st, by = "STRATUM_CN", all.x = TRUE)

  #coerce numeric design/measurement columns: real FIADB SQLite can return
  #columns (notably the mostly-NULL MACRO_BREAKPOINT_DIA) as character, which
  #would make `DIA >= MACRO_BREAKPOINT_DIA` a STRING comparison ("7.8" >= "30.0"
  #is TRUE) and mis-assign the adjustment factor.
  out = .fia_as_numeric(out, c("DIA", "HT", "TPA_UNADJ", "VOLCFNET", "VOLCSNET",
                               "DRYBIO_AG", "MACRO_BREAKPOINT_DIA", "EXPNS",
                               "ADJ_FACTOR_SUBP", "ADJ_FACTOR_MICR", "ADJ_FACTOR_MACR"))

  #--- choose the per-tree adjustment factor by plot design ------------------
  adj = out$ADJ_FACTOR_SUBP
  micr = !is.na(out$DIA) & out$DIA < 5.0
  adj[micr] = out$ADJ_FACTOR_MICR[micr]
  if("MACRO_BREAKPOINT_DIA" %in% names(out)){
    macr = !is.na(out$MACRO_BREAKPOINT_DIA) & !is.na(out$DIA) &
           out$DIA >= out$MACRO_BREAKPOINT_DIA
    adj[macr] = out$ADJ_FACTOR_MACR[macr]
  }
  out$ADJ_FACTOR = adj
  out$TPA_EXP    = out$TPA_UNADJ * adj

  #optional GRM join for growth/removal/mortality work
  if(isTRUE(grm)){
    if(!.fia_has_table(src, "TREE_GRM_COMPONENT"))
      stop("grm = TRUE requires a TREE_GRM_COMPONENT table in the source")
    grm_c = .fia_table(src, "TREE_GRM_COMPONENT", "PLT_CN", assgn$PLT_CN)
    #TREE_GRM_COMPONENT keys on the tree record CN (named TRE_CN there)
    out = merge(out, grm_c, by.x = "CN", by.y = "TRE_CN", all.x = TRUE,
                suffixes = c("", "_GRM"))
  }

  #standard, compile_trees-friendly column set first, then extras
  std = c("EVALID", "PLT_CN", "CN", "CONDID", "SUBP", "TREE", "STATUSCD",
          "SPCD", "DIA", "HT", "TPA_UNADJ", "ADJ_FACTOR", "TPA_EXP",
          "VOLCFNET", "VOLCSNET", "DRYBIO_AG", "DIAHTCD", "PREV_TRE_CN",
          "COUNTYCD", "INVYR", "MEASYEAR", "ESTN_UNIT", "STRATUM_CN",
          "STRATUMCD", "EXPNS")
  keep = c(intersect(std, names(out)), intersect(vars_keep, names(out)))
  out = out[, unique(keep), drop = FALSE]
  #rename TREE record CN -> TRE_CN for clarity (PLT_CN already distinct)
  if("CN" %in% names(out)) names(out)[names(out) == "CN"] = "TRE_CN"
  out = out[order(out$EVALID, out$PLT_CN,
                  if("TREE" %in% names(out)) out$TREE else seq_len(nrow(out))), , drop = FALSE]
  rownames(out) = NULL
  out
}
