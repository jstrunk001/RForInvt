#'
#'@title
#' run fvs after preparing key files
#'
#'@description
#' run fvs after preparing key files
#' 1. prepare parameters
#' 2. prepare prototype key file
#' 3. generate key files for all cns
#' 4. run fvs with run_fvs()
#'
#'@details
#'run fvs after preparing key files
#' 1. prepare parameters
#' 2. prepare prototype key file
#' 3. generate key files for all cns
#' 4. run fvs with run_fvs()
#'
#'\cr
#'Revision History
#' \tabular{ll}{
#'1.0 \tab 2021 Oct 08 Created\cr
#'}
#'
#'@author
#'
#'Jacob Strunk <strunky@@gmail.com>
#'
#'@param key_df input dataframe with key file parameters
#'@param cluster optional parallel cluster object
#'@param db_merge merge output parallel databases
#'@param merge_dbs T/F should temp dbs be merged into a single databse
#'@param clear_db T/F delete all old inputs
#'@param delete_temp_db T/F delete temp db's used for parallel processing
#'@param append T/F if the output db already exists, should it be appended or wiped
#'
#'@return
#'  NULL
#'
#'@examples
#'
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
#'@import RSQLite
#'
#'@export
#
#'@seealso \code{\link{fvs_load}}\cr \code{\link{fvs_make_keyfiles}}\cr \code{\link{fvs_keyfile_prototype}}\cr \code{\link{fvs_param_prototype}}\cr

fvs_run = function(
  key_df
  ,db_merge = "FVSOut.db"
  ,fvs_commands = "FVS_commands.txt"
  ,merge_dbs= T
  ,cluster=NA
  ,clear_db=T
  ,delete_temp_db = T
  ,append=F
){

  t1 = Sys.time()
  print(paste("FVS runs started at",t1))
  ##clear output DBs if specified

  if(merge_dbs & db_merge == basename(db_merge) ) out_db = file.path(dirname(key_df$output_db[1]), db_merge)
  else out_db = db_merge
    
  if(clear_db){
    sapply(unique(key_df$output_db),function(x){
      output_con =  RSQLite::dbConnect( RSQLite::SQLite(),x)
      tabs =  RSQLite::dbListTables(output_con)
      sapply(tabs,function(x){
        RSQLite::dbExecute(output_con,sprintf("delete from %s",x))
      })
      try( RSQLite::dbCommit(output_con),silent=T)
      RSQLite::dbDisconnect(output_con)
    })
  }

  #prepare fvs commands
  fvs_runs = paste0(key_df$fvs_path," --keywordfile=",gsub("\\\\\\\\", "\\\\",gsub("/","\\\\",key_df$key_path)  ) ) 
  if(!is.na(fvs_commands) ){
    browser()
    dir_cmds = file.path(key_df$output_dir, "commands"))
    dir.create(dir_cmds)
    dir_cmds_file = file.path(dir_cmds, paste0(,",csv"))
    writeLines(fvs_runs, file.path() )
    
  }
    
    
  ##run in series
  if(is.na(cluster[1])){
    res_fvs = lapply(fvs_runs,function(x){
      system(x)
    })
  }

  ###run in parallel but split so each cluster uses the same DB in series
  if(!is.na(cluster[1])){
    res_fvs =  parallel::parLapplyLB(cluster,fvs_runs, system)
  }

  #merge multiple databases into single database
  if(merge_dbs){

    unq_db = noquote(unique(key_df$output_db))

    #make database for all
    if(!append & file.exists(out_db)) unlink(out_db)
    con_dbmrg =  DBI::dbConnect( RSQLite::SQLite(),out_db)

    for(i in 1:length(unq_db)){

      #connect and get lists from dbs
      unq_db_i = unq_db[i]
      con_dbi =  RSQLite::dbConnect( RSQLite::SQLite(),unq_db_i)
      tbs_i = dbListTables(con_dbi)

      if(length(tbs_i) == 0) stop("input db empty, nothing to merge")
      
      #iterate through tables and write to merge db
      for(j in 1:length(tbs_i)){
        tbj = dbReadTable(con_dbi ,tbs_i[j])
        dbWriteTable(con_dbmrg,tbs_i[j], tbj ,append=T )
      }
      RSQLite::dbDisconnect( con_dbi)

    }
    RSQLite::dbDisconnect( con_dbmrg)

  }

  #remove temporary dbs
  if(delete_temp_db) sapply(unq_db,unlink)

  t2 = Sys.time()
  print(paste("FVS runs finished at",t2))
  print(difftime(t2,t1,units="mins"))
}

