#' @title Predict log-level volume and dimensions using NVEL (vollib2_r)
#'
#' @description
#' Predict tree-level volumes and individual log-level dimensions and volumes using the
#' National Volume Estimator Library (NVEL) DLL.
#'
#' @details
#' \itemize{
#'   \item \code{LG_}: Individual log attributes (Length, Diameter, Volume).
#'   \item \code{TR_}: Whole-tree attributes (Total volume, Biomass, total log counts).
#' }
#'
#' @param dfTL data.frame. Tree list containing at least DBH, Height, and Species.
#' @param voleq character. Optional. A specific volume equation to force.
#' @param region integer. USFS Region (1-10).
#' @param forest character. USFS Forest code.
#' @param district character. USFS District code.
#' @param voleqNm character. Column name in \code{dfTL} for volume equations.
#' @param regionNm character. Column name in \code{dfTL} for region.
#' @param forestNm character. Column name in \code{dfTL} for forest.
#' @param districtNm character. Column name in \code{dfTL} for district.
#' @param spcdNm character. Column name in \code{dfTL} for USFS species code.
#' @param dbhNm character. Column name in \code{dfTL} for DBH (inches).
#' @param htNm character. Column name in \code{dfTL} for Height (feet).
#' @param pulpDbNm character. Optional. Column name for pulpwood top diameter.
#' @param sawDbNm character. Optional. Column name for sawtimber top diameter.
#' @param htPrd1Nm character. Optional. Column name for height to primary product top.
#' @param htPrd2Nm character. Optional. Column name for height to secondary product top.
#' @param upHt1Nm character. Optional. Column name for upper stem height.
#' @param upDb1Nm character. Optional. Column name for upper stem diameter.
#' @param stumpHtNm character. Optional. Column name for stump height.
#' @param fclassNm character. Optional. Column name for Girard Form Class.
#' @param dbtbhNm character. Optional. Column name for double bark thickness.
#' @param btrNm character. Optional. Column name for bark thickness ratio.
#' @param vol2biomass logical. If TRUE, computes biomass using NVEL weight factors.
#' @param dll_64 character. Path to the 64-bit vollib.dll.
#' @param dll_32 character. Path to the 32-bit vollib.dll.
#' @param load_dll logical. Whether to load the DLL into the R session.
#' @param dll_func_vol character. Entry point for volume calculation (default "vollib2_r").
#'
#' @return A data.frame in "long" format (one row per log).If a tree is non-merchantable,
#' it returns one row with \code{LG_NUM = 0}.
#'
#' @examples
#' \dontrun{
#'   library(RForInvt)
#'
#'   # 1. Prepare a sample tree list
#'   df_trees <- data.frame(
#'     tree_id = c(1, 2)
#'     , region = c(6, 6)
#'     , forest = c("01", "01")
#'     , district = c("01", "01")
#'     , spcd = c(202, 202) # Douglas-fir
#'     , dbh = c(12.5, 28.2)
#'     , ht = c(85, 140)
#'   )
#'
#'   # 2. Predict volumes and log dimensions
#'   # Note: vollib2_r is the default entry point
#'   res <- NVEL_buck1(
#'     dfTL = df_trees
#'     , dbhNm = "dbh"
#'     , htNm = "ht"
#'     , spcdNm = "spcd"
#'     , vol2biomass = TRUE
#'   )
#'
#'   # 3. View results
#'   # Tree 2 will have multiple rows (one for each 16ft or 32ft log)
#'   print(subset(res, tree_id == 2)[, c("LG_NUM", "LG_LEN", "LG_DIB_SMALL", "TR_TCFV")])
#'
#'   # 4. Forcing a specific volume equation
#'   res_forced <- NVEL_buck1(
#'     dfTL = df_trees
#'     , voleq = "F0162521"
#'   )
#' }
#'
#' @export
NVEL_buck = function(
  dfTL = data.frame(dbh = 5, ht = 5)
  , voleq = NA
  , region = 1
  , forest = NA
  , district = NA
  , voleqNm = "voleq"
  , regionNm = "region"
  , forestNm = "forest"
  , districtNm = "district"
  , spcdNm = "spcd"
  , dbhNm = "dbh"
  , htNm = "ht"
  , pulpDbNm = "pulpDb"
  , sawDbNm = "sawDb"
  , htPrd1Nm = "htPrd1"
  , htPrd2Nm = "htPrd2"
  , upHt1Nm = "upHt1"
  , upDb1Nm = "upDb1"
  , stumpHtNm = "stumpHt"
  , fclassNm = "fclass"
  , dbtbhNm = "dbtbh"
  , btrNm = "btr"
  , vol2biomass = TRUE
  , dll_64 = system.file('lib/VolLibDll20231106/vollib-64bits/vollib.dll', package = "RForInvt")
  , dll_32 = system.file('lib/VolLibDll20231106/vollib-32bits/vollib.dll', package = "RForInvt")
  , load_dll = TRUE
  , dll_func_vol = "vollib2_r" ### UPDATED: Default changed to vollib2_r
){

  options(stringsAsFactors = FALSE)

  if(load_dll){
    .load_dll(
      dll_64 = dll_64
      , dll_32 = dll_32
      , dll_func = dll_func_vol
    )
  }

  dfTL0_in = .formatTL2NVEL2(
    dfTL0 = dfTL
    , voleq = voleq[1]
    , region = region[1]
    , forest = forest[1]
    , district = district[1]
    , voleqNm = voleqNm
    , regionNm = regionNm
    , forestNm = forestNm
    , districtNm = districtNm
    , spcdNm = spcdNm
    , dbhNm = dbhNm
    , htNm = htNm
    , pulpDbNm = pulpDbNm
    , sawDbNm = sawDbNm
    , htPrd1Nm = htPrd1Nm
    , htPrd2Nm = htPrd2Nm
    , upHt1Nm = upHt1Nm
    , upDb1Nm = upDb1Nm
    , stumpHtNm = stumpHtNm
    , fclassNm = fclassNm
    , dbtbhNm = dbtbhNm
    , btrNm = btrNm
  )

  if(is.na(voleq[1]) & (!voleqNm %in% names(dfTL))){
    vol_eqns_in = NVEL_voleq(
      dfTL = dfTL
      , regionNm = regionNm
      , forestNm = forestNm
      , districtNm = districtNm
      , spcdNm = spcdNm
      , load_dll = FALSE
    )
    dfTL0_in[, "voleq"] = vol_eqns_in[, "voleq"]
    voleqNm = "voleq"
  }

  defaultW <- getOption("warn")
  options(warn = -1)
  dfTL0_in[is.na(dfTL0_in)] = 0

  ### mapply calls .fn_fortran_vol2_logs
  vol_list = mapply(
    .fn_fortran_vol2_logs
    , voleq = dfTL0_in[, voleqNm]
    , region = dfTL0_in[, regionNm]
    , forest = dfTL0_in[, forestNm]
    , district = dfTL0_in[, districtNm]
    , spcd = dfTL0_in[, spcdNm]
    , dbh = dfTL0_in[, dbhNm]
    , ht = dfTL0_in[, htNm]
    , pulpDb = dfTL0_in[, pulpDbNm]
    , sawDb = dfTL0_in[, sawDbNm]
    , htPrd1 = dfTL0_in[, htPrd1Nm]
    , htPrd2 = dfTL0_in[, htPrd2Nm]
    , upHt1 = dfTL0_in[, upHt1Nm]
    , upDb1 = dfTL0_in[, upDb1Nm]
    , stumpHt = dfTL0_in[, stumpHtNm]
    , fclass = dfTL0_in[, fclassNm]
    , dbtbh = dfTL0_in[, dbtbhNm]
    , btr = dfTL0_in[, btrNm]
    , MoreArgs = list(dll_func_vol2 = dll_func_vol)
    , SIMPLIFY = FALSE
  )

  options(warn = defaultW)

  ### handle multi-row log lists
  vol_pd0_df = plyr::ldply(
    1:length(vol_list)
    , function(i){
        res = vol_list[[i]]
        if(nrow(res) > 0) cbind(TreeIdx = i, res) else NULL
      }
  )

  ### merge trees with logs
  dfTL0_in$TreeIdx = 1:nrow(dfTL0_in)
  res_in = merge(
    x = dfTL0_in
    , y = vol_pd0_df
    , by = "TreeIdx"
    , all.x = TRUE
  )

  if(vol2biomass){
    wts_in = NVEL_wtfactor(
      dfTL = dfTL0_in[, c(regionNm, forestNm, spcdNm)]
      , spcdNm = spcdNm
    )
    wts_in$TreeIdx = 1:nrow(wts_in)
    res_in = merge(
      x = res_in
      , y = wts_in[, c("TreeIdx", "WFGRN_LBSCFT", "WFDRY_LBSCFT")]
      , by = "TreeIdx"
      , all.x = TRUE
    )

    ### Compute biomass as density times volume
    res_in[, "TR_TBIOGRN_LBS"] = res_in[, "WFGRN_LBSCFT"] * res_in[, "TR_TCFV_ALL"]
    res_in[, "TR_TBIODRY_LBS"] = res_in[, "WFDRY_LBSCFT"] * res_in[, "TR_TCFV_ALL"]
  }

  return(res_in)
}

### UPDATED: Rewritten helper for vollib2_r
.fn_fortran_vol2_logs = function(
  voleq
  , region
  , forest
  , district
  , spcd
  , dbh
  , ht
  , pulpDb
  , sawDb
  , htPrd1
  , htPrd2
  , upHt1
  , upDb1
  , stumpHt
  , fclass
  , dbtbh
  , btr
  , dll_func_vol2
){

  ### Placeholders for matrices and log count arguments
  res_vol0 = .Fortran(
    dll_func_vol2
    , as.character(voleq)     # 1
    , as.integer(region)       # 2
    , as.character(forest)    # 3
    , as.character(district)  # 4
    , as.integer(spcd)         # 5
    , as.double(dbh)           # 6
    , as.double(ht)            # 7
    , as.double(pulpDb)        # 8
    , as.double(sawDb)         # 9
    , as.double(htPrd1)        # 10
    , as.double(htPrd2)        # 11
    , as.double(upHt1)         # 12
    , as.double(upDb1)         # 13
    , as.double(stumpHt)       # 14
    , as.integer(fclass)       # 15
    , as.double(dbtbh)         # 16
    , as.double(btr)           # 17
    , as.double(rep(0, 15))    # 18: VOL(15) - Tree Totals
    , as.double(matrix(0, 7, 20)) # 19: LOGVOL(7, 20)
    , as.double(matrix(0, 3, 21)) # 20: LOGDIA(3, 21)
    , as.double(rep(0, 20))    # 21: LOGLEN(20)
    , as.double(rep(0, 21))    # 22: BOLHT(21)
    , as.integer(0)            # 23: LOGS (Total)
    , as.integer(0)            # 24: LOGS (Pulp)
    , as.integer(0)            # 25: LOGS (Saw)
    , as.integer(0)            # 26: Error code
    , PACKAGE = "vollib"
  )

  #pull critical attribues
  num_logs   <- res_vol0[[23]]
  tree_vols  <- res_vol0[[18]]
  err_code   <- res_vol0[[26]]

  if(num_logs > 0){

    ### Reconstruct matrices
    log_vols = matrix(res_vol0[[19]], nrow = 7, ncol = 20)
    log_dias = matrix(res_vol0[[20]], nrow = 3, ncol = 21)

    ### Construct log dataframe with LG_ prefix
    df_logs = data.frame(
          LG_NUM       = 1:num_logs,
          LG_LEN       = res_vol0[[21]][1:num_logs],
          LG_HT        = res_vol0[[22]][2:(num_logs+1)],
          LG_DIB_SMALL = log_dias[1, 1:num_logs],
          LG_DIB_LARGE = log_dias[2, 1:num_logs],
          LG_DOB_SMALL = log_dias[3, 1:num_logs],
          LG_MBFV   = log_vols[1, 1:num_logs],
          LG_MCFV   = log_vols[4, 1:num_logs],
          LG_MIBFV   = log_vols[7, 1:num_logs],
          LG_ERR    = res_vol0[[26]]
        )
    ### Construct tree dataframe with TR_ prefix
    df_tree = data.frame(
          TR_NLOGS_ALL = num_logs,
          TR_NLOGS_P = res_vol0[[24]],
          TR_NLOGS_S = res_vol0[[25]],
          TR_TCFV_ALL   = tree_vols[[1]],
          TR_MBFV   = tree_vols[[2]],
          TR_MCFV   = tree_vols[[4]],
          TR_MCDV   = tree_vols[[6]],
          TR_MCFV2   = tree_vols[[7]],
          TR_MBFV2   = tree_vols[[12]],
          TR_MCDV2   = tree_vols[[9]],
          TR_TCFV_stmp   = tree_vols[[14]],
          TR_TCFV_tip   = tree_vols[[15]]
        )

    return(cbind(df_logs, df_tree))

  } else {
    ### rows for trees and logs with no logs
    df_none = data.frame(
        LG_NUM = 0
      , LG_LEN = 0
      , LG_HT = 0
      , LG_DIB_SMALL = 0
      , LG_DIB_LARGE = 0
      , LG_DOB_SMALL = 0
      , LG_MBFV = 0
      , LG_MCFV = 0
      , LG_MIBFV = 0
      , LG_ERR = err_code
    )
    df_tree = data.frame(
          TR_NLOGS_ALL = 0,
          TR_NLOGS_P = 0,
          TR_NLOGS_S = 0,
          TR_TCFV_ALL  = 0,
          TR_MBFV   = 0,
          TR_MCFV   = 0,
          TR_MCDV   = 0,
          TR_MCFV2   = 0,
          TR_MBFV2   = 0,
          TR_MCDV2   = 0,
          TR_TCFV_stmp   = 0,
          TR_TCFV_tip   = 0
        )

    return(cbind(df_none, df_tree))
  }
}

### UPDATED: Simplified .formatTL2NVEL2
.formatTL2NVEL2 = function(
  dfTL0
  , voleq
  , region
  , forest
  , district
  , voleqNm
  , regionNm
  , forestNm
  , districtNm
  , spcdNm
  , dbhNm
  , htNm
  , pulpDbNm
  , sawDbNm
  , htPrd1Nm
  , htPrd2Nm
  , upHt1Nm
  , upDb1Nm
  , stumpHtNm
  , fclassNm
  , dbtbhNm
  , btrNm
){

  dfTL1 = dfTL0
  nms_in = c(
     voleq=voleqNm[1]
    ,region=regionNm[1]
    ,forest=forestNm[1]
    ,district=districtNm[1]
    ,spcd=spcdNm[1]
    ,dbh=dbhNm[1]
    ,ht=htNm[1]
    ,pulpDb=pulpDbNm
    ,sawDb=sawDbNm
    ,htPrd1=htPrd1Nm
    ,htPrd2=htPrd2Nm
    ,upHt1=upHt1Nm
    ,upDb1=upDb1Nm
    ,stumpHt=stumpHtNm
    ,fclass=fclassNm
    ,dbtbh=dbtbhNm
    ,btr=btrNm
  )

  for(n in names(nms_in)){
    if(!nms_in[n] %in% names(dfTL1)) dfTL1[[nms_in[n]]] = 0
  }

  if(!is.na(voleq))    dfTL1[[voleqNm]] <- voleq
  if(!is.na(region))   dfTL1[[regionNm]] <- region
  if(!is.na(forest))   dfTL1[[forestNm]] <- forest
  if(!is.na(district)) dfTL1[[districtNm]] <- district

  return(dfTL1)
}

.load_dll = function(
  dll_64
  , dll_32
  , dll_func
){
  arch_in = R.Version()$arch
  dll_loaded = "vollib" %in% names(getLoadedDLLs())
  if(!dll_loaded){
    if(arch_in == "x86_64") dyn.load(dll_64) else dyn.load(dll_32)
  }
}


