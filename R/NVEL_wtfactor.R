#'@title
#'  Select Appropriate Volume equation for Region, Forest, District, Tree species from National Volume Estimator Library (NVEL)
#'
#'@description
#'  Select Appropriate Volume equation for Region, Forest, District, Tree species from National Volume Estimator Library (NVEL)
#'
#'@details
#'  Select Appropriate Volume equation for Region, Forest, District, Tree species from National Volume Estimator Library (NVEL)
#'
#'\cr
#'Revision History
#' \tabular{ll}{
#'1.0 \tab 5/19/2020 Function created \cr
#'}
#'
#'@author
#'
#'Jacob Strunk <someone@@somewhere.com>
#'
#'
#'@param dfTL data.frame with tree records
#'@param region (optional) region,forest,district but these supercede values in dfTL columns regionNm,forestNm
#'@param forest (optional) region,forest,district but these supercede values in dfTL columns regionNm,forestNm
#'@param district (optional) region,forest,district but these supercede values in dfTL columns regionNm,forestNm
#'@param regionNm (optional) column name in DFTL:provide region, forest, district for every tree in dfTL
#'@param forestNm (optional) column name in DFTL:provide region, forest, district for every tree in dfTL
#'@param spcdNm (required) column name in DFTL: USFS species code
#'@param dll_64  path to 64bit dll
#'@param dll_32  path to 64bit dll
#'@param load_dll T/F should dll be loaded (in case it is already loaded)
#'@param dll_func_wtfactor name of volume equation chooser function call in NVEL .dll
#'
#'@return
#'  reformatted tree list with with a new column volume equation codes for NVEL - "voleq"
#'
#'@examples
#'
#'         library(RSForInvt)
#'          NVEL_voleq(region = 2, forest = "01",district = "01", spcd=951)
#'          NVEL_voleq(region = 2, forest = "01",district = "01", spcd=951)
#'          NVEL_voleq(region = 2, forest = "01",district = "01", spcd=rep(c(951,201),2))
#'          NVEL_voleq(dfTL=data.frame(region = 6, forest = "01",district = "01", spcd=rep(c(951,201),2)))
#'
#'         #grab list of species
#'         if(!"dfSpp" %in% ls()){
#'           library(RSQLite)
#'           db0 = dbConnect(RSQLite::SQLite(), "code/BiomassEqns.db")
#'           dfSpp = dbGetQuery(db0, paste("select * from tblspp"))
#'           dfCoeff = dbGetQuery(db0, paste("select * from BM_EQCoefs"))
#'           dbDisconnect(db0)
#'         }
#'
#'         #build a fake tree list
#'         if(!"dfSpp" %in% ls()){
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
#'         NVEL_voleq( dfTL = df_fake )
#'
#'
#'
#'
#'
#'@export
#
#'@seealso \code{\link{NVEL_volume}}\cr \code{\link{NVEL_biomass}}\cr

#Desired upgrades to this function:
#
#

NVEL_wtfactor = function(

  dfTL = list(NA, data.frame(spcd=201, dbh=5 ,ht=5 ,region = 0 , forest = "01") )[[1]]

  #optional, but these supercede values in dfTL columns regionNm,forestNm
  ,region = NA
  ,forest = NA
  ,spcd = NA

  #optional, provide region, forest, district for every tree
  ,regionNm = "region"
  ,forestNm = "forest"
  ,spcdNm = "spcd"

  ,dll_64 = system.file('lib/VolLibDll20231106/vollib-64bits/vollib.dll', package="RForInvt")
  ,dll_32 = system.file('lib/VolLibDll20231106/vollib-32bits/vollib.dll', package="RForInvt")
  ,dll_func_wtfactor = "getwtfactor_r"
  ,load_dll = T

){

  #load dll if needed
  if(load_dll) .load_dll(dll_64,dll_32,dll_func )

  #figure out how we are generating volume equations
  if(class(dfTL) == "logical"){

    if( is.na(region[1]) | is.na(forest[1]) |  is.na(spcd[1]) ) warning("dfTL not provided, and missing region,forest,district, or spcd - generic equation(s) likely returned")

    dfTL = data.frame(region=region,forest=forest,spcd=spcd)

    regionNm = c("region")
    forestNm = c("forest")
    spcdNm = c("spcd")

  }

  #get volume equation
  #catch warning about portabiliyt of passing a char vector
  defaultW <- getOption("warn")
  options(warn = -1)
  res_wf0 = mapply(.fn_fortran_wtf,dfTL[,regionNm],dfTL[,forestNm],dfTL[,spcdNm] , MoreArgs = list(dll_func_wtfactor = dll_func_wtfactor), SIMPLIFY = F)
  options(warn = defaultW)

  #merge predictions together
  return(data.frame(dfTL,plyr::rbind.fill(res_wf0)))

}
#call fortran
.fn_fortran_wtf = function(region,forest,species,dll_func_wtfactor){

  #browser()
  res_wf0 = .Fortran(dll_func_wtfactor,as.integer(region),as.character(forest),as.integer(species),as.double(0),as.double(0), PACKAGE="vollib")
  data.frame(WFGRN_LBSCFT = res_wf0[[4]] , WFDRY_LBSCFT = res_wf0[[5]] )

}

#load dll if needed
.load_dll = function(dll_64,dll_32,dll_func ){

  arch_in = R.Version()$arch
  loaded_dlls_in = names(getLoadedDLLs())
  dll_loaded = "vollib" %in% loaded_dlls_in
  if(arch_in == "x86_64" & !dll_loaded) dyn.load(dll_64)
  if(arch_in == "x86_32" & !dll_loaded) dyn.load(dll_32)

}


#Testing
if(F){


  library(RSForInvt)

  NVEL_wtfactor(region = 2, forest = "01", spcd=951)
  NVEL_wtfactor(region = 2, forest = "01", spcd=201)
  NVEL_wtfactor(region = 2, forest = "01", spcd=rep(c(951,201,113),2))

  if(!"dfSpp" %in% ls()){
    library(RSQLite)
    db0 = dbConnect(RSQLite::SQLite(), system.file("misc/NBEL/BiomassEqns.db", package="RSForInvt"))
    dfSpp = dbGetQuery(db0, paste("select * from tblspp"))
    dfCoeff = dbGetQuery(db0, paste("select * from BM_EQCoefs"))
    dbDisconnect(db0)

    set.seed=111
    nfake=length(unique(dfCoeff$species_code))

    df_fake = data.frame(
      trid=1:(nfake)
      ,region = 6
      ,forest = "01"
      ,dbh=10*abs(rnorm(nfake))
      ,ht=100*abs(rnorm(nfake))
      ,spcd = unique(dfCoeff$species_code)# sample(c("a","b","c","d") , nfake , T)
    )

  }

  NVEL_wtfactor( dfTL = df_fake )


}
