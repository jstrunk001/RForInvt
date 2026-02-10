#' @export
#' @export
NVEL_buck = function(
  dfTL = data.table::data.table(dbh = 5, ht = 5)
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
  , dll_func_vol = "vollib2_r"
  , ncore = 1
){

# Debugging check
# print(args(.load_dll))

  # 1. Standardize and Format Data (Once)
  dfTL0_in = .formatTL2NVEL2(
     dfTL0=dfTL
    ,voleq=voleq[1]
    ,region=region[1]
    ,forest=forest[1]
    ,district=district[1]
    ,voleqNm=voleqNm
    ,regionNm=regionNm
    ,forestNm=forestNm
    ,districtNm=districtNm
    ,spcdNm=spcdNm
    ,dbhNm=dbhNm
    ,htNm=htNm
    ,pulpDbNm=pulpDbNm
    ,sawDbNm=sawDbNm
    ,htPrd1Nm=htPrd1Nm
    ,htPrd2Nm=htPrd2Nm
    ,upHt1Nm=upHt1Nm
    ,upDb1Nm=upDb1Nm
    ,stumpHtNm=stumpHtNm
    ,fclassNm=fclassNm
    ,dbtbhNm=dbtbhNm
    ,btrNm=btrNm
  )

  data.table::setDT(dfTL0_in)
  dfTL0_in[, TreeIdx := .I]

  # Handle Volume Equation Lookup
  if(is.na(voleq[1]) & (!voleqNm %in% names(dfTL0_in))){
    vol_eqns_in = NVEL_voleq(
                        dfTL=dfTL
                      ,regionNm=regionNm
                      ,forestNm=forestNm
                      ,districtNm=districtNm
                      ,spcdNm=spcdNm
                      ,load_dll=FALSE
                         )
    dfTL0_in[, (voleqNm) := vol_eqns_in[["voleq"]]]
    voleqNm = "voleq"
  }

  # Fill NAs with 0
  for (j in names(dfTL0_in)) data.table::set(dfTL0_in, which(is.na(dfTL0_in[[j]])), j, 0)

  # 2. Execution Logic
  if(ncore > 1){
    if (!requireNamespace("parallel", quietly = TRUE)) stop("Package 'parallel' required.")

    cl_in <- parallel::makeCluster(ncore)
    # FIX: Ensure on.exit uses the correct cluster variable name
    on.exit(parallel::stopCluster(cl_in))

    # Split data into chunks
    chunks <- split(dfTL0_in, cut(seq_len(nrow(dfTL0_in)), ncore, labels = FALSE))

    # FIX: Export all necessary helper functions and path variables.
    # We use parent.frame() or .GlobalEnv if debugging as a script.
    helper_funcs = c(".fn_fortran_vol2_logs", ".load_dll", ".formatTL2NVEL2")
    parallel::clusterExport(cl_in,
                            varlist = c(
                                       "dll_64"
                                      ,"dll_32"
                                      ,"dll_func_vol"
                                      ,"voleqNm"
                                      ,"regionNm"
                                      ,"forestNm"
                                      ,"districtNm"
                                      ,"spcdNm"
                                      ,"dbhNm"
                                      ,"htNm"
                                      ,"pulpDbNm"
                                      ,"sawDbNm"
                                      ,"htPrd1Nm"
                                      ,"htPrd2Nm"
                                      ,"upHt1Nm"
                                      ,"upDb1Nm"
                                      ,"stumpHtNm"
                                      ,"fclassNm"
                                      ,"dbtbhNm"
                                      ,"btrNm"
                                      ,helper_funcs),
                            envir = environment())

    vol_list_raw <- parallel::parLapply(cl_in, chunks, function(dt_chunk) {
      # Load data.table inside worker
      library(data.table)

      #for debugging Use the local helper instead of RForInvt:::
      .load_dll(dll_64 = dll_64, dll_32 = dll_32, dll_func = dll_func_vol)

      mapply(
         .fn_fortran_vol2_logs
        ,voleq=dt_chunk[[voleqNm]]
        ,region=dt_chunk[[regionNm]]
        ,forest=dt_chunk[[forestNm]]
        ,district=dt_chunk[[districtNm]]
        ,spcd=dt_chunk[[spcdNm]]
        ,dbh=dt_chunk[[dbhNm]]
        ,ht=dt_chunk[[htNm]]
        ,pulpDb=dt_chunk[[pulpDbNm]]
        ,sawDb=dt_chunk[[sawDbNm]]
        ,htPrd1=dt_chunk[[htPrd1Nm]]
        ,htPrd2=dt_chunk[[htPrd2Nm]]
        ,upHt1=dt_chunk[[upHt1Nm]]
        ,upDb1=dt_chunk[[upDb1Nm]]
        ,stumpHt=dt_chunk[[stumpHtNm]]
        ,fclass=dt_chunk[[fclassNm]]
        ,dbtbh=dt_chunk[[dbtbhNm]]
        ,btr=dt_chunk[[btrNm]]
        ,MoreArgs=list(dll_func_vol2=dll_func_vol)
        ,SIMPLIFY=FALSE
          )
    })
    vol_list = do.call(c, vol_list_raw)

  } else {
    # ncore == 1
    if(load_dll) .load_dll(dll_64, dll_32, dll_func_vol)

    vol_list = mapply(
                   .fn_fortran_vol2_logs
                  ,voleq=dfTL0_in[[voleqNm]]
                  ,region=dfTL0_in[[regionNm]]
                  ,forest=dfTL0_in[[forestNm]]
                  ,district=dfTL0_in[[districtNm]]
                  ,spcd=dfTL0_in[[spcdNm]]
                  ,dbh=dfTL0_in[[dbhNm]]
                  ,ht=dfTL0_in[[htNm]]
                  ,pulpDb=dfTL0_in[[pulpDbNm]]
                  ,sawDb=dfTL0_in[[sawDbNm]]
                  ,htPrd1=dfTL0_in[[htPrd1Nm]]
                  ,htPrd2=dfTL0_in[[htPrd2Nm]]
                  ,upHt1=dfTL0_in[[upHt1Nm]]
                  ,upDb1=dfTL0_in[[upDb1Nm]]
                  ,stumpHt=dfTL0_in[[stumpHtNm]]
                  ,fclass=dfTL0_in[[fclassNm]]
                  ,dbtbh=dfTL0_in[[dbtbhNm]]
                  ,btr=dfTL0_in[[btrNm]]
                  ,MoreArgs=list(dll_func_vol2=dll_func_vol)
                  ,SIMPLIFY=FALSE
                  )
  }

  # 3. Post-Processing
  vol_pd0_df = data.table::rbindlist(lapply(seq_along(vol_list), function(i) {
    res = vol_list[[i]]
    if (is.null(res) || nrow(res) == 0) return(NULL)
    data.table::setDT(res)[, TreeIdx := i]
    return(res)
  }), use.names = TRUE, fill = TRUE)

  res_in = merge(dfTL0_in, vol_pd0_df, by = "TreeIdx", all.x = TRUE)

  if(vol2biomass){
    wts_in = NVEL_wtfactor(dfTL = dfTL0_in[, .SD, .SDcols = c(regionNm, forestNm, spcdNm)], spcdNm = spcdNm)
    data.table::setDT(wts_in)[, TreeIdx := .I]
    res_in = merge(res_in, wts_in[, .(TreeIdx, WFGRN_LBSCFT, WFDRY_LBSCFT)], by = "TreeIdx", all.x = TRUE)
    res_in[, `:=`(TR_TBIOGRN_LBS = WFGRN_LBSCFT * TR_TCFV_ALL, TR_TBIODRY_LBS = WFDRY_LBSCFT * TR_TCFV_ALL)]
  }

  return(res_in)
}


### Simplified .formatTL2NVEL2
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

### helper for vollib2_r
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
