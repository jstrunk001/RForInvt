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
#'Jacob Strunk <Jacob.strunk@@usda.gov>
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
#'@param dll_32  path to 64bit dll
#'@param load_dll T/F should dll be loaded (in case it is already loaded)
#'@param dll_func_vol name of volume prediction function call in NVEL .dll
#'@param dll_func_voleq name of volume equation chooser function call in NVEL .dll
#'@param nclus number of cores to use
#'
#'@return
#'  a data.frame reformatted to include all of the original columns, any missing columns for the NVEL .dll, 15 columns of predicted volumes, and an error column
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
#'
#'         #look up volume equations
#'          library(RForInvt)
#'          NVEL_voleq(region = 2, forest = "01",district = "01", spcd=951)
#'          NVEL_voleq(region = 2, forest = "01",district = "01", spcd=951)
#'          NVEL_voleq(region = 2, forest = "01",district = "01", spcd=rep(c(951,201),2))
#'          NVEL_voleq(dfTL=data.frame(region = 6, forest = "01",district = "01", spcd=rep(c(951,201),2)))
#'
#'         #grab list of species
#'         if(!"dfSpp" %in% ls()){
#'           library(RSQLite)
#'           db0 = dbConnect(RSQLite::SQLite(), system.file("misc/NBEL/BiomassEqns.db", package="RForInvt"))
#'           dfSpp = dbGetQuery(db0, paste("select * from tblspp"))
#'           dfCoeff = dbGetQuery(db0, paste("select * from BM_EQCoefs"))
#'           dbDisconnect(db0)
#'         }
#'
#'         #build a fake tree list
#'         if("df_fake" %in% ls()){
#'           set.seed=111
#'           nfake=length(unique(dfCoeff$species_code))
#'
#'           df_fake = data.frame(
#'             trid=1:(nfake)
#'             ,region = 6
#'             ,forest = "01"
#'             ,district = "01"
#'             ,dbh=10*abs(rnorm(nfake))
#'             ,ht=100*abs(rnorm(nfake))
#'             ,spcd = unique(dfCoeff$species_code)#'     sample(c("a","b","c","d") , nfake , T)
#'           )
#'
#'         }
#'
#'         #get volumes
#'         NVEL_volume( dfTL = df_fake )
#'
#'
#'
#'@import plyr
#'
#'@export
#
#'@seealso \code{\link{NVEL_voleq}}\cr

#Desired upgrades to this function:
#
#

NVEL_volume=function(

  dfTL = data.frame(dbh=5,ht=5)

  #force a specific volume equation
  ,voleq = NA

  #optional, but these supercede values in dfTL columns regionNm,forestNm, districtNm
  ,region = 1
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

  options(stringsAsFactors = F)
  #load dll if needed
  if(load_dll) .load_dll(dll_64,dll_32,dll_func_vol )

  #test for existence of voleq
  #get_voleq = T
  #if(!is.na(voleq)) get_voleq = F
  #if(!is.na(voleqNm)[1]) if((voleqNm)[1] %in% names(dfTL)) if( sum(sapply(dfTL[,voleqNm],nchar)==0 ) == 0 ) get_voleq = F

  #deal with column names, add all columns / rename
  dfTL0_in = .formatTL2NVEL(
                     dfTL0 = dfTL
                     ,voleq  = voleq[1]
                     ,region  = region[1]
                     ,forest  = forest[1]
                     ,district  = district[1]
                     ,voleqNm = voleqNm[1]
                     ,regionNm = regionNm[1]
                     ,forestNm  =  forestNm[1]
                     ,districtNm  =  districtNm[1]
                     ,spcdNm = spcdNm[1]
                     ,dbhNm  = dbhNm[1]
                     ,htNm  = htNm[1]

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



  #get volume equations
  if( is.na(voleq[1]) & (!voleqNm %in% names(dfTL)) ){
    vol_eqns_in = NVEL_voleq(
                      dfTL = dfTL
                      #optional, provide region, forest, district for every tree
                      ,regionNm = "region"
                      ,forestNm = "forest"
                      ,districtNm = "district"
                      ,spcdNm = "spcd"
                      #,dll_func_voleq = dll_func_voleq
                      ,load_dll = F
                      )
    dfTL0_in[,"voleq"] = vol_eqns_in[,"voleq"]
    voleqNm = "voleq"
  }


    #turn of warnings temporarily, this generates scads of them
  defaultW <- getOption("warn")
  options(warn = -1)

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

    #compute total biomass
    wts_in = NVEL_wtfactor( dfTL = dfTL0_in[,c(regionNm,forestNm,spcdNm)])
    res_in[,"TBIOGRN_LBS"] = wts_in[,"WFGRN_LBSCFT"]*res_in[,"TCFV"]
    res_in[,"TBIODRY_LBS"] = wts_in[,"WFDRY_LBSCFT"]*res_in[,"TCFV"]

    #append weight factors
    res_in = data.frame(res_in,wts_in[,c("WFGRN_LBSCFT","WFDRY_LBSCFT")])
  }

  #return formatted tree list with predicted volumes
  res_in

}

#load dll if needed
.load_dll = function(dll_64,dll_32,dll_func ){

  arch_in = R.Version()$arch
  loaded_dlls_in = names(getLoadedDLLs())
  dll_loaded = "vollib" %in% loaded_dlls_in
  if(arch_in == "x86_64" & !dll_loaded) dyn.load(dll_64)
  if(arch_in == "x86_32" & !dll_loaded) dyn.load(dll_32)

}

#call fortran
.fn_fortran_vol = function(
                    voleq
                    ,region
                    ,forest
                    ,district
                    ,spcd
                    ,dbh
                    ,merchDb
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



#data tree list and format it for these functions
#typically not needed by users
.formatTL2NVEL = function(
  dfTL0

  ,voleq
  ,region
  ,forest
  ,district

  ,voleqNm
  ,regionNm
  ,forestNm
  ,districtNm
  ,spcdNm
  ,dbhNm
  ,htNm

  ,pulpDbNm
  ,sawDbNm

  ,htPrd1Nm
  ,htPrd2Nm

  ,upHt1Nm
  ,upDb1Nm
  ,stumpHtNm

  ,fclassNm
  ,dbtbhNm
  ,btrNm



){

  dfTL1 = dfTL0

  nms_in = c(
    voleq= voleqNm[1]
    ,region = regionNm[1]
    ,forest  =  forestNm[1]
    ,district  =  districtNm[1]
    ,spcd = spcdNm[1]
    ,dbh  = dbhNm[1]
    ,ht  = htNm[1]

    ,pulpDb = pulpDbNm
    ,sawDb = sawDbNm

    ,htPrd1 = htPrd1Nm
    ,htPrd2 = htPrd2Nm

    ,upHt1 = upHt1Nm
    ,upDb1 = upDb1Nm
    ,stumpHt = stumpHtNm

    ,fclass = fclassNm
    ,dbtbh = dbtbhNm
    ,btr = btrNm
  )

  #add columns with na in nms
  na_nms = is.na(nms_in)
  nms_fill_NA = names(nms_in)[na_nms]
  dfTL1[,nms_fill_NA] = 0

  #add missing columns
  na_nms1 =! nms_in %in% names(dfTL1)
  nms_fill_NA1 = names(nms_in)[na_nms1]
  dfTL1[,nms_fill_NA1] = 0

  #update existing column names with fixed column names
  nms_in[na_nms] = names(nms_in)[na_nms]
  nms_in[na_nms1] = names(nms_in)[na_nms1]
  names(dfTL1[,nms_in]) = names(nms_in)

  #overwrite names
  if(!is.na(voleq)) dfTL1[,"voleq"] = voleq
  if(!is.na(region)) dfTL1[,"region"] = region
  if(!is.na(forest)) dfTL1[,"forest"] = forest
  if(!is.na(district)) dfTL1[,"district"] = district

  #return correctly named data
  dfTL1[,nms_in]

}
#names(nms_in)[! names(nms_in)  %in% names(dfTL1) ]
#(nms_in)[! (nms_in)  %in% names(dfTL1) ]

NVEL_ht2topd = function(){


}


NVEL_calcdob = function(){


}

NVEL_biomass = function(){


}




#Testing
if(F){

  #library(RForInvt)

  if(!"dfSpp" %in% ls()){

    library(RSQLite)
    db0 = dbConnect(RSQLite::SQLite(), system.file("/NVEL/BiomassEqns.db", package="RForInvt"))
    dfSpp = dbGetQuery(db0, paste("select * from tblspp"))
    dfCoeff = dbGetQuery(db0, paste("select * from BM_EQCoefs"))
    dbDisconnect(db0)

    set.seed=111
    nfake=length(unique(dfCoeff$species_code))

    dbhs = runif(nfake, 8,20 )
    df_fake = data.frame(
      trid=1:(nfake)
      ,region = 6
      ,forest = "01"
      ,district = "01"
      ,dbh=dbhs
      ,ht=70*(dbhs/12)
      ,spcd = unique(dfCoeff$species_code)# sample(c("a","b","c","d") , nfake , T)
    )

    #get FIA volumes equations
    df_fake1 =   NVEL_nvbeq( dfTL = df_fake )

    #get standard volume equations
    df_fake2 =   NVEL_voleq( dfTL = df_fake )

  }

  #FIA volume
  NVEL_volume( dfTL = df_fake1 )

  #standard volume equations - both should be the same below
  NVEL_volume( dfTL = df_fake )
  NVEL_volume( dfTL = df_fake2 )

}
