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
#'  - compile_plots
#'  - compile_trees
#'  - ols_modeling
#'  - knn_tools
#'  - yai_r2
#'  - sample_systematic
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
#'Jacob Strunk <someone@@somewhere.com>
#'
#'
#'
#'@return
#'  depends on function
#'
#'@importFrom grDevices dev.off pdf
#'@importFrom graphics layout lines mtext par
#'@importFrom stats aggregate as.formula coef fitted getCall lm median na.omit quantile residuals rnorm update var
#'@importFrom utils download.file file_test read.csv unzip write.csv
#'
#'@examples
#'\dontrun{
#'#FVS EXAMPLE (requires a local FVS executable and input database)
#'
#'           clus1 = parallel::makeCluster(2)
#'
#'           #assume a typical inventory dataset and prepare fvs parameters
#'           stand_data = data.frame(stand=1:10, year=2000:2009)
#'           df_params = fvs_prototype_params()
#'           df_params[1:nrow(stand_data),]=NA
#'           df_params[,"std_id"] = stand_data$stand
#'           df_params[,"invyr"] = stand_data$year
#'           df_params[,"timeint"] = 1
#'           df_params[,"numcycle"] = 1
#'           df_params[,"input_db"] = "c:/temp/fordata.db"
#'           df_params[,"fvs_path"] = "C:/FVSbin/FVSca.exe"
#'           df_params[,"tree_table"] = "fvs_treeinit"
#'           df_params[,"stand_table"] = "fvs_standinit"
#'           df_params
#'
#'           #prepare prototype key file
#'           key_proto = fvs_prototype_keyfile(invyr = "InvYear       2001", notriple=NULL)
#'
#'           #convert prototype key file into series of key files associated with each cn
#'           df_keys = fvs_make_keyfiles(df_params, key_proto = key_proto, cluster = clus1, id="std_id")
#'
#'           #lastly, actually run fvs
#'           fvs_run(df_keys, cluster = clus1)
#'           parallel::stopCluster(clus1)
#'
#'#NVEL EXAMPLE (requires the bundled vollib.dll)
#'
#'         #look up volume equations
#'         NVEL_voleq(region = 2, forest = "01", district = "01", spcd = 951)
#'         NVEL_voleq(region = 2, forest = "01", district = "01", spcd = rep(c(951, 201), 2))
#'
#'         #a small inline tree list (USFS species codes, dbh in inches, ht in feet)
#'         df_fake = data.frame(
#'             trid     = 1:4
#'             ,region   = 6
#'             ,forest   = "01"
#'             ,district = "01"
#'             ,dbh      = c(8, 12, 18, 24)
#'             ,ht       = c(45, 70, 95, 110)
#'             ,spcd     = c(202, 122, 242, 263)
#'           )
#'
#'         #get volumes
#'         NVEL_volume( dfTL = df_fake )
#'}
#'
#'@seealso \code{\link{NVEL_volume}}\cr \code{\link{compile_trees}}\cr \code{\link{fvs_run}}\cr
#'
RForInvt=function(){}


