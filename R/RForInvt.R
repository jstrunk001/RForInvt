#'@title
#'  Tools for forest inventory estimation
#'
#'@description
#'  Tools for forest inventory estimation
#'
#'@details
#'  Tools for forest inventory estimation
#'  currently in a preliminary stage, but some functionality present
#'
#'  - compilePlots
#'  - compileTrees
#'  - ols_modeling
#'  - knn_tools
#'  - yai_r2
#'  - sampleSystematic
#'  - NVEL...() related functions
#'  - fvs..() related functions
#'
#' Eventually
#'
#' - estimate()
#'
#' the estimate() functionality (currently use .estimate() )likely won't be any greater than the survey package
#' but it will be tailored to typical forest inventory scenarios.
#'
#'
#'\cr
#'Revision History
#' \tabular{ll}{
#'1.0 \tab 2021 10 18 Created \cr
#'}
#'
#'@author
#'
#'Jacob Strunk <strunky@@gmail.com>
#'
#'
#'@return
#'  depends on function
#'
#'@examples
#'
#'#FVS EXAMPLE
#' clus1 = parallel::makeCluster(4)
#'
#'#assume a typical inventory dataset and prepare fvs parameters
#' stand_data = data.frame(stand=1:10, year=2000:2009)#'
#' df_params = fvs_protype_params()
#' df_params[1:nrow(stand_data),]=NA
#' df_params[,"stand_id"] = stand_data$stand
#' df_params[,"stand_cn"] = stand_data$stand
#' df_params[,"invyr"] = stand_data$year
#' df_params[,"timeint"] = 1
#' df_params[,"numcycle"] = 1
#' df_params[,"input_db"] = "c:/temp/fordata.db"
#' df_params[,"fvs_path"] = "C:/FVSbin/FVSca.exe"
#' df_params[,"tree_table"] = "fvs_treeinit"
#' df_params[,"stand_table"] = "fvs_standinit"
#' df_params
#'
#'#prepare prototype key file
#' key_proto = fvs_prototype_keyfile(invyr = "InvYear       2001", notriple=NULL)
#'
#'#convert prototype key file into series of key files associated with each cn
#' df_keys = fvs_make_keyfiles(df_params, key_proto = key_proto, cluster = clus1)
#'
#'#lastly, actually run fvs
#' fvs_run(df_keys, cluster = clus1)
#'
#' parallel::stopCluster(clus1)
#'
#'
#'#NVEL EXAMPLE
#'
#'         #look up volume equations
#'          library(RSForInvt)
#'          NVEL_voleq(region = 2, forest = "01",district = "01", spcd=951)
#'          NVEL_voleq(region = 2, forest = "01",district = "01", spcd=951)
#'          NVEL_voleq(region = 2, forest = "01",district = "01", spcd=rep(c(951,201),2))
#'          NVEL_voleq(dfTL=data.frame(region = 6, forest = "01",district = "01", spcd=rep(c(951,201),2)))
#'
#'         #grab list of species
#'         if(!"dfSpp" %in% ls()){
#'           library(RSQLite)
#'           db0 = dbConnect(RSQLite::SQLite(), system.file("misc/NBEL/BiomassEqns.db", package="RSForInvt"))
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
#'@seealso \code{\link{lidR}}\cr \code{\link{RSForInvt}}\cr
#'
RForInvt=function(){}


