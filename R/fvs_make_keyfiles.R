#'@title
#'make keyfiles for each row in the input data frame
#'
#'@description
#' make keyfiles for each row in the input data frame
#'
#'@details
#' make keyfiles for each row in the input data frame
#'
#'\cr
#'Revision History
#' \tabular{ll}{
#'1.0 \tab 2021 Oct 15 Created\cr
#'}
#'
#'@author
#'
#'Jacob Strunk <someone@@somewhere.com>
#'
#'
#'@param param_df parameters substituted into keyfile
#'@param processing_dir where do keyfiles and output data go
#'@param path_key_proto (optional) template key file - e.g. use FVS gui
#'@param clear_keys T/F delete all old key files
#'@param cluster (Optional) parallel cluster object
#'@param id select column to use as id field - column should be present in param_df
#'@param quotes_db T/F - recent versions of FVS variants don't accept quotes around db names in key files
#'
#'@return
#'  NULL
#'
#'
#'@examples
#'
#' clus1 = parallel::makeCluster(4)
#'
#'#assume a typical inventory dataset and prepare fvs parameters
#' stand_data = data.frame(stand=1:10, year=2000:2009)#'
#' df_params = fvs_protype_params()
#' df_params[1:nrow(stand_data),]=NA
#' df_params[,"std_id"] = stand_data$stand
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
#' df_keys = fvs_make_keyfiles(df_params, key_proto = key_proto, cluster = clus1, id="std_id")
#'
#'#lastly, actually run fvs
#' fvs_run(df_keys, cluster = clus1, db_merge = "FVS_AllRunsB.db")
#'
#' parallel::stopCluster(clus1)
#'
#'
#'
#'
#'@import RSQLite parallel DBI
#'
#'@export
#
#'@seealso \code{\link{fvs_run}}\cr \code{\link{fvs_keyfile_prototype}}\cr

fvs_make_keyfiles = function(
  param_df
  ,processing_dir = "c:\\temp\\RForInvt\\fvs"
  ,path_key_proto = NA
  ,key_proto = NA
  ,clear_db = T
  ,clear_keys = T
  ,cluster = NA
  ,id=c("plt_id")
  ,quotes_db = F
){

  #add quotes to input/output paths
  if(quotes_db) param_df[,"input_db_q"] = shQuote(param_df[,"input_db"])
  if(!quotes_db) param_df[,"input_db_q"] = param_df[,"input_db"]

  if(!dir.exists(processing_dir)) dir.create(processing_dir,recursive=T)

  ##divide by cluster if necessary
  param_df$cluster = 1
  #dir_indb = paste0(processing_dir,"/fvs_in/")
  dir_outdb = paste0(processing_dir,"/fvs_out/")
  key_dir = paste0(processing_dir,"/keyfiles/")

  ###prepare number of clusters
  n_cluster = length(cluster)
  if(n_cluster>1){
    clus_n = rep(1:n_cluster,times=(trunc(nrow(param_df)/n_cluster) + 1))
    param_df$cluster = clus_n[1:nrow(param_df)]
  }
  param_df$fvs_dir = processing_dir
  param_df$output_db = paste0(dir_outdb,"db",param_df$cluster,".db")

  if(quotes_db) param_df$output_db_q = shQuote(paste0(dir_outdb,"db",param_df$cluster,".db"))
  if(!quotes_db) param_df$output_db_q = paste0(dir_outdb,"db",param_df$cluster,".db")
  #param_df$output_db_q = shQuote(paste0(dir_outdb,"db",param_df$cluster,".db"))

  ###create directories if they don't exits
  for(this_dir in c(processing_dir,dir_outdb,key_dir)){
    if(!dir.exists(this_dir)){
      dir.create(this_dir,recursive=T)
    }
  }

  ###create output dbs if they don't exist
  sapply(unique(param_df$output_db),function(x){
    if(!file.exists(x)){
      RSQLite::dbDisconnect(RSQLite::dbConnect(RSQLite::SQLite(),x))
    }
  })

  ###clear current keyfiles
  if(clear_keys){
    paths = dir(key_dir,pattern="key$",full.names=T)
    file.remove(paths)
    paths = dir(key_dir,pattern="out$",full.names=T)
    file.remove(paths)
  }

  ##clear current output db
  if(clear_db){
    sapply(unique(param_df$output_db),function(x){
      output_con = RSQLite::dbConnect(RSQLite::SQLite(),x)
      tabs = RSQLite::dbListTables(output_con)
      sapply(tabs,function(x){
        RSQLite::dbExecute(output_con,sprintf("delete from %s",x))
      })
      try(RSQLite::dbCommit(output_con),silent=T)
      RSQLite::dbDisconnect(output_con)
    })
  }

  ##add input_db to param_df
  ##add the keyfile path
  param_df$key_path = paste0(key_dir,"/",gsub("(\\\\|/)",".", param_df[,id[1]]),".key")

  ##read the key prototype
  if(!is.na(path_key_proto)) key_proto_in = readLines(path_key_proto)
  if(is.na(path_key_proto) & !is.na(key_proto[1])) key_proto_in = key_proto
  if(is.na(path_key_proto) & is.na(key_proto[1])) key_proto_in = fvs_prototype_keyfile()

  #split and write key files
  lapply(split(param_df,1:nrow(param_df)),.write_batch, key_proto_in)

  return(param_df)
}

#write a single batch file
.write_batch = function(this_line, key_proto_in){
  this_proto = key_proto_in
  ##these are the values that are substituted in the key file
  this_parm_list = names(this_line)
  #for(this_parm in this_parm_list){
  for(i in 1:length(this_parm_list)){
    this_parm = this_parm_list[i]
    this_sub = gsub("\\\\","/",this_line[[this_parm ]])
    this_proto = gsub(paste0("@",this_parm,'@'),this_sub,this_proto)
  }
  writeLines(this_proto,this_line$key_path)
  return(this_line$key_path)
}
