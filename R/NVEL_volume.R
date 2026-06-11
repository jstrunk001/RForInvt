#'@title
#'  Predict volume using National Volume Estimator Library (NVEL)
#'
#'@description
#'  Predict volume using National Volume Estimator Library (NVEL)
#'
#'@details
#'  Predict volume using National Volume Estimator Library (NVEL)
#'
#'  see:
#'  https://www.fs.fed.us/forestmanagement/products/measurement/volume/nvel/index.php
#'
#'\cr
#'Revision History
#' \tabular{ll}{
#'1.0 \tab 5/19/2020 Implemented \cr
#'1.1 \tab 5/21/2020 Add biomass and weight factors \cr
#'1.2 \tab 12/05/2023 Add lates NVEL and fix paths to dlls \cr
#'}
#'
#'@author
#'
#'Jacob Strunk <someone@@somewhere.com>
#'
#'
#'@param dfTL tree list with a minimum of diamter and height columns
#'@param voleq force a specific volume equation for all trees
#'@param region (optional) region,forest,district but these supercede values in dfTL columns regionNm,forestNm, districtNm
#'@param forest (optional) region,forest,district but these supercede values in dfTL columns regionNm,forestNm, districtNm
#'@param district (optional) region,forest,district but these supercede values in dfTL columns regionNm,forestNm, districtNm
#'@param voleqNm  (optional) column name in DFTL: provide region, forest, district for every tree in dfTL
#'@param regionNm (optional) column name in DFTL:provide region, forest, district for every tree in dfTL
#'@param forestNm (optional) column name in DFTL:provide region, forest, district for every tree in dfTL
#'@param districtNm (optional) column name in DFTL: provide region, forest, district for every tree in dfTL
#'@param spcdNm (required) column name in DFTL: USFS species code
#'@param dbhNm  (required) column name in DFTL: dbh in inches
#'@param htNm (required) column name in DFTL: height in feet
#'@param pulpDbNm (optional) column name in DFTL: upper db miminum for pulp (in)
#'@param sawDbNm  (optional) column name in DFTL: upper db miminum for saw (in)
#'@param htPrd1Nm (optional) column name in DFTL: Height to the minimum top diameter inside bark for primary product (ft)
#'@param htPrd2Nm (optional) column name in DFTL: Height to the minimum top diameter inside bark for secondary product (ft)
#'@param upHt1Nm  (optional) column name in DFTL: Upper stem height in feet where upper stem diameter was measured or where AVG1 is to be applied. OR For Region 8 the upper stem reference height defined by their profile model (ht0, ht4, ht7, ht9). OR For region 9 the UPSHT1 is the height to 7.6/9.6 top diameter when HT1PRD is not the height to 7.6/9.6 top diameter.
#'@param upDb1Nm  (optional) column name in DFTL: Upper stem diameter measured at
#'@param stumpHtNm  (optional) column name in DFTL: stump height (ft)
#'@param fclassNm (optional) column name in DFTL: Girard’s form class.  Diameter at the top of the first log given as a percent of DBH.
#'@param dbtbhNm  (optional) column name in DFTL: Double bark thickness at breast height in inches
#'@param btrNm  (optional) column name in DFTL: Bark thickness ratio given as the percent of diameter inside bark to diameter outside bark.  (dib/dob *100).
#'@param vol2biomass T/F Compute total drye and green biomass in lbs using weight factors and append weight factors (lbs / cubic foot)
#'@param dll_64  path to 64bit dll
#'@param dll_32  path to 32bit dll
#'@param load_dll T/F should dll be loaded (in case it is already loaded)
#'@param dll_func_vol name of volume prediction function call in NVEL .dll
#'@param dll_func_voleq name of volume equation chooser function call in NVEL .dll
#'
#'@return
#'  a data.table reformatted to include all of the original columns, any missing columns for the NVEL .dll, 15 columns of predicted volumes, and an error column
#'
#'  predicted volumes:
#'     1)	Total Cubic Volume from ground to tip
#'     2)	Gross Scribner board foot volume. Note: The VOL(2) is International ¼ board foot volume for Region 8 Forest 8, 9, 10, and 12 (except Andrew Pickens district); and Region 9 Forest 4,5,8,11,12,14,19,20,21,22,24, and 30 when using Clark profile equation.
#'     3)	Net Scribner board foot volume.
#'     4)	Gross merchantable cubic foot volume
#'     5)	Net merchantable cubic foot volume
#'     6)	Merchantable cordwood volume
#'     7)	Gross secondary product volume in cubic feet
#'     8)	Net secondary product volume in cubic feet
#'     9)	Secondary product in cordwood
#'     10)	Gross International ¼ board foot volume
#'     11)	Net International ¼ board foot volume
#'     12)	Gross secondary product in Scribner board feet
#'     13)	Net secondary product in Scribner board feet
#'     14)	Stump volume
#'     15)	Tip volume
#'
#'@examples
#'\donttest{
#'         #look up volume equations (requires the bundled vollib.dll)
#'         NVEL_voleq(region = 2, forest = "01", district = "01", spcd = 951)
#'         NVEL_voleq(region = 2, forest = "01", district = "01", spcd = rep(c(951, 201), 2))
#'
#'         #a small inline tree list (USFS species codes, dbh in inches, ht in feet)
#'         df_fake <- data.frame(
#'             trid     = 1:4,
#'             region   = 6,
#'             forest   = "01",
#'             district = "01",
#'             dbh      = c(8, 12, 18, 24),
#'             ht       = c(45, 70, 95, 110),
#'             spcd     = c(202, 122, 242, 263)
#'         )
#'
#'         #pick volume equations then predict the 15 NVEL volumes per tree
#'         NVEL_volume(dfTL = df_fake)
#'}
#'
#'@import plyr
#'

#
#'@seealso \code{\link{NVEL_voleq}}\cr

#Desired upgrades to this function:
#
#'@export

NVEL_volume=function(

  dfTL = data.frame(dbh=5,ht=5)

  #force a specific volume equation
  ,voleq = NA

  #optional, but these supercede values in dfTL columns regionNm,forestNm, districtNm
  ,region = NA
  ,forest = NA
  ,district = NA

  #optional, provide region, forest, district for every tree
  ,voleqNm = c("voleq")
  ,regionNm = c("region")
  ,forestNm = c("forest")
  ,districtNm = c("district")

  ,spcdNm = "spcd"
  ,dbhNm = "dbh"
  ,htNm = "ht"

  ,pulpDbNm = c("pulpDb")
  ,sawDbNm = c("sawDb")

  ,htPrd1Nm = c("htPrd1")
  ,htPrd2Nm = c("htPrd2")

  ,upHt1Nm = c("upHt1")
  ,upDb1Nm = c("upDb1")
  ,stumpHtNm = "stumpHt"

  ,fclassNm = c("fclass")
  ,dbtbhNm = c("dbtbh")
  ,btrNm = c("btr")

  ,vol2biomass = T

  ,dll_64 = system.file('lib/VolLibDll20231106/vollib-64bits/vollib.dll', package="RForInvt")
  ,dll_32 = system.file('lib/VolLibDll20231106/vollib-32bits/vollib.dll', package="RForInvt")
  ,load_dll = T

  ,dll_func_vol = "vollib_r"
  ,dll_func_voleq = "getvoleq_r"

  #,ncore = 4

  ){

  #load dll if needed
  if(load_dll) .nvel_load_dll(dll_64,dll_32 )

  #work in data.frame semantics so single-name `[,` indexing returns vectors
  #(a data.table input would otherwise mis-interpret dfTL0_in[,voleqNm])
  dfTL = as.data.frame(dfTL)

  #deal with column names: add any missing NVEL columns (filled 0), preserve all
  #original columns, and apply any scalar region/forest/district/voleq overrides.
  dfTL0_in = .formatTL2NVEL2(
                     dfTL0 = dfTL
                     ,voleq  = voleq[1]
                     ,region  = region[1]
                     ,forest  = forest[1]
                     ,district  = district[1]
                     ,voleqNm = voleqNm
                     ,regionNm = regionNm
                     ,forestNm  =  forestNm
                     ,districtNm  =  districtNm
                     ,spcdNm = spcdNm
                     ,dbhNm  = dbhNm
                     ,htNm  = htNm

                     ,pulpDbNm = pulpDbNm
                     ,sawDbNm = sawDbNm

                     ,htPrd1Nm = htPrd1Nm
                     ,htPrd2Nm = htPrd2Nm

                     ,upHt1Nm = upHt1Nm
                     ,upDb1Nm = upDb1Nm
                     ,stumpHtNm = stumpHtNm

                     ,fclassNm = fclassNm
                     ,dbtbhNm = dbtbhNm
                     ,btrNm = btrNm

                     )

  #get volume equations for trees lacking a forced/column equation. Use the
  #FORMATTED table and forward the user column names so scalar
  #region/forest/district and custom *Nm mappings reach the chooser.
  if( is.na(voleq[1]) & (!voleqNm[1] %in% names(dfTL)) ){
    vol_eqns_in = NVEL_voleq(
                      dfTL = dfTL0_in
                      ,regionNm = regionNm[1]
                      ,forestNm = forestNm[1]
                      ,districtNm = districtNm[1]
                      ,spcdNm = spcdNm[1]
                      ,dll_func_voleq = dll_func_voleq
                      ,load_dll = F
                      )
    dfTL0_in[,voleqNm[1]] = vol_eqns_in[,"voleq"]
  }


    #turn of warnings temporarily, this generates scads of them
  defaultW <- getOption("warn")
  options(warn = -1)
  on.exit(options(warn = defaultW), add = TRUE)

  #fix bad values
  dfTL0_in[is.na(dfTL0_in)] = 0

  #compute volumes
  vol_pd0 = mapply(.fn_fortran_vol

                          ,voleq = dfTL0_in[,voleqNm[1]]
                          ,region = dfTL0_in[,regionNm[1]]
                          ,forest = dfTL0_in[, forestNm[1]]
                          ,district  = dfTL0_in[, districtNm[1]]
                          ,spcd = dfTL0_in[,spcdNm[1]]
                          ,dbh  = dfTL0_in[,dbhNm[1]]
                          ,ht  = dfTL0_in[,htNm[1]]
                          ,pulpDb = dfTL0_in[,pulpDbNm[1]]
                          ,sawDb = dfTL0_in[,sawDbNm[1]]

                          ,htPrd1 = dfTL0_in[,htPrd1Nm[1]]
                          ,htPrd2 = dfTL0_in[,htPrd2Nm[1]]

                          ,upHt1 = dfTL0_in[,upHt1Nm[1]]
                          ,upDb1 = dfTL0_in[,upDb1Nm[1]]
                          ,stumpHt= dfTL0_in[,stumpHtNm[1]]

                          ,fclass = dfTL0_in[,fclassNm[1]]
                          ,dbtbh = dfTL0_in[,dbtbhNm[1]]
                          ,btr = dfTL0_in[,btrNm[1]]

                          ,MoreArgs=list(dll_func_vol=dll_func_vol)
                          ,SIMPLIFY = F)
  #turn warnings back on
  options(warn = defaultW)

  #merge predictions together and add their names
  vol_pd0_df = plyr::rbind.fill(vol_pd0 )
  names(vol_pd0_df)[1:15] = c("TCFV","BFV_GRS","BFV_NET","CFV_GRS","MCFV_NET","CDVOL","CV2nd_GRS","CV2nd_NET","CDVOL2nd","BFV_GRS_INTL","BFV_NET_INTL","BFV2nd_GRS","BFV2nd_NET","VStump","VTip")

  #build return object
  res_in = data.frame(dfTL0_in,vol_pd0_df)

  if(vol2biomass){

    #compute total biomass; forward region/forest column names so custom
    #*Nm mappings reach NVEL_wtfactor (which otherwise defaults to region/forest)
    wts_in = NVEL_wtfactor(
                 dfTL = dfTL0_in[,c(regionNm[1],forestNm[1],spcdNm[1])]
                 , regionNm = regionNm[1]
                 , forestNm = forestNm[1]
                 , spcdNm = spcdNm[1]
                 , load_dll = FALSE
                 )
    res_in[,"TBIOGRN_LBS"] = wts_in[["WFGRN_LBSCFT"]]*res_in[,"TCFV"]
    res_in[,"TBIODRY_LBS"] = wts_in[["WFDRY_LBSCFT"]]*res_in[,"TCFV"]

    #append weight factors
    res_in[,"WFGRN_LBSCFT"] = wts_in[["WFGRN_LBSCFT"]]
    res_in[,"WFDRY_LBSCFT"] = wts_in[["WFDRY_LBSCFT"]]
  }

  #return formatted tree list with predicted volumes
  #standardize on data.table for consistency across the NVEL_* family
  data.table::as.data.table(res_in)

}

#call fortran
.fn_fortran_vol = function(
                    voleq
                    ,region
                    ,forest
                    ,district
                    ,spcd
                    ,dbh
                    ,ht
                    ,pulpDb
                    ,sawDb
                    ,htPrd1
                    ,htPrd2
                    ,upHt1
                    ,upDb1
                    ,stumpHt
                    ,fclass
                    ,dbtbh
                    ,btr
                    ,dll_func_vol
                    ){


    res_vol0 =.Fortran(
              dll_func_vol
             ,as.character(voleq)
             ,as.integer(region)
             ,as.character(forest)
             ,as.character(district)
             ,as.integer(spcd)
             ,as.double(dbh)
             ,as.double(ht)
             ,as.double(pulpDb)
             ,as.double(sawDb)
             ,as.double(htPrd1)
             ,as.double(htPrd2)
             ,as.double(upHt1)
             ,as.double(upDb1)
             ,as.double(stumpHt)
             ,as.integer(fclass)
             ,as.double(dbtbh)
             ,as.double(btr)
             ,as.double(c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0))
             ,as.integer(0)
             ,PACKAGE = "vollib"
             )
    #return
    data.frame(t(res_vol0[[18]] ),err=res_vol0[[19]])


}

# NOTE: the former .formatTL2NVEL() helper has been removed. NVEL_volume() now
# shares the single, sound formatter .formatTL2NVEL2() defined in NVEL_buck.R,
# which preserves all original columns and keys by the user-supplied *Nm names.
