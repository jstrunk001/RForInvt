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
#'@param region scalar region; used only when dfTL is not supplied (otherwise the regionNm column is used)
#'@param forest scalar forest; used only when dfTL is not supplied (otherwise the forestNm column is used)
#'@param district scalar district; used only when dfTL is not supplied (otherwise the districtNm column is used)
#'@param spcd scalar USFS species code; used only when dfTL is not supplied (otherwise the spcdNm column is used)
#'@param regionNm (optional) column name in dfTL holding the region for every tree
#'@param forestNm (optional) column name in dfTL holding the forest for every tree
#'@param districtNm (optional) column name in dfTL holding the district for every tree
#'@param spcdNm (required) column name in dfTL: USFS species code
#'@param dll_64  path to 64bit dll
#'@param dll_32  path to 32bit dll
#'@param load_dll T/F should dll be loaded (in case it is already loaded)
#'@param dll_func_voleq name of volume equation chooser function call in NVEL .dll
#'
#'@return
#'  reformatted tree list with with a new column volume equation codes for NVEL - "voleq"
#'
#'@examples
#'\donttest{
#'         #look up volume equations (requires the bundled vollib.dll)
#'         NVEL_voleq(region = 2, forest = "01", district = "01", spcd = 951)
#'         NVEL_voleq(region = 2, forest = "01", district = "01", spcd = rep(c(951, 201), 2))
#'         NVEL_voleq(dfTL = data.frame(region = 6, forest = "01", district = "01",
#'                                      spcd = rep(c(951, 201), 2)))
#'
#'         #a small inline tree list (USFS species codes)
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
#'         #choose a volume equation for every tree
#'         NVEL_voleq(dfTL = df_fake)
#'}
#'
#'@export
#
#'@seealso \code{\link{NVEL_volume}}\cr \code{\link{NVEL_biomass}}\cr

#Desired upgrades to this function:
#
#

NVEL_voleq = function(

  dfTL = NA

  #optional, but these supercede values in dfTL columns regionNm,forestNm, districtNm
  ,region = NA
  ,forest = NA
  ,district = NA
  ,spcd = NA

  #optional, provide region, forest, district for every tree
  ,regionNm = "region"
  ,forestNm = "forest"
  ,districtNm = "district"
  ,spcdNm = "spcd"

  ,dll_64 =  system.file('lib/VolLibDll20231106/vollib-64bits/vollib.dll', package="RForInvt")
  ,dll_32 = system.file('lib/VolLibDll20231106/vollib-32bits/vollib.dll', package="RForInvt")
  ,dll_func_voleq = "getvoleq_r"
  ,load_dll = T

){

  #load dll if needed
  if(load_dll) .nvel_load_dll(dll_64,dll_32 )

  #figure out how we are generating volume equations
  if(is.null(dfTL) || is.logical(dfTL)){

    if( is.na(region[1]) | is.na(forest[1]) | is.na(district[1]) | is.na(spcd[1]) ) warning("dfTL not provided, and missing region,forest,district, or spcd - generic equation(s) likely returned")

    dfTL = data.frame(region=region,forest=forest,district=district,spcd=spcd)

    regionNm = c("region")
    forestNm = c("forest")
    districtNm = c("district")
    spcdNm = c("spcd")

  }

  #get volume equation
  #catch warning about portabiliyt of passing a char vector
  defaultW <- getOption("warn")
  options(warn = -1)
  dfTL[,"voleq"] = mapply(.fn_fortran_voleq,dfTL[,regionNm],dfTL[,forestNm],dfTL[,districtNm],dfTL[,spcdNm] , MoreArgs = list(dll_func_voleq = dll_func_voleq), SIMPLIFY = T)
  options(warn = defaultW)

  return(dfTL)

}
#call fortran
.fn_fortran_voleq = function(region,forest,district,species,dll_func_voleq){

  #browser()
  .Fortran(dll_func_voleq,as.integer(region),as.character(forest),as.character(district),as.integer(species),as.character("          "),as.integer(0), PACKAGE="vollib")[[5]]

}

