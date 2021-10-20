#'@title
#'  Tools for forest inventory with Remote Sensing
#'
#'@description
#'  Tools for forest biometrics
#'
#'@details
#'  Tools for forest biometrics
#'
#'\cr
#'Revision History
#' \tabular{ll}{
#'1.0 \tab 2021 10 15 Created \cr
#'}
#'
#'@author
#'
#'Jacob Strunk <strunky@@gmail.com>
#'
#'
#'@return
#'  #stuff
#'
#'@examples
#'@examples
#'
#' clus1 = parallel::makeCluster(4)
#'
#'#assume a typical inventory dataset and prepare fvs parameters
#' stand_data = data.frame(stand=1:10, year=2000:2009)#'
#' df_params = fvs_protype_params()
#' df_params[1:nrow(stand_data),]=NA
#' df_params[,"cn"] = stand_data$stand
#' df_params[,"invyr"] = stand_data$year
#' df_params[,"timint"] = 1
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
#'
#'
RForBio=function(){}
