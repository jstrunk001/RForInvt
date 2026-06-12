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
#'Jacob Strunk <someone@@somewhere.com>
#'
#'
#'@param key_df input dataframe with key file parameters
#'@param cluster optional parallel cluster object
#'@param db_merge merge output parallel databases
#'@param fvs_commands optional file name to write the FVS command lines to (NA to skip)
#'@param merge_dbs T/F should temp dbs be merged into a single databse
#'@param clear_db T/F delete all old inputs
#'@param delete_temp_db T/F delete temp db's used for parallel processing
#'@param append T/F if the output db already exists, should it be appended or wiped
#'@param skip_empty T/F skip empty per-run output databases when merging (otherwise error)
#'
#'@return
#'  invisibly, a list of the captured \code{system()} outputs for each FVS run
#'
#'@examples
#'\dontrun{
#' # requires a local FVS executable and input database
#' clus1 = parallel::makeCluster(2)
#'
#' #assume a typical inventory dataset and prepare fvs parameters
#' stand_data = data.frame(stand=1:10, year=2000:2009)
#' df_params = fvs_prototype_params()
#' df_params[1:nrow(stand_data),]=NA
#' df_params[,"std_id"] = stand_data$stand
#' df_params[,"invyr"] = stand_data$year
#' df_params[,"timeint"] = 1
#' df_params[,"numcycle"] = 1
#' df_params[,"input_db"] = "c:/temp/fordata.db"
#' df_params[,"fvs_path"] = "C:/FVSbin/FVSca.exe"
#' df_params[,"tree_table"] = "fvs_treeinit"
#' df_params[,"stand_table"] = "fvs_standinit"
#'
#' #prepare prototype key file
#' key_proto = fvs_prototype_keyfile(invyr = "InvYear       2001", notriple=NULL)
#'
#' #convert prototype key file into series of key files associated with each stand
#' df_keys = fvs_make_keyfiles(df_params, key_proto = key_proto, cluster = clus1, id="std_id")
#'
#' #lastly, actually run fvs
#' fvs_run(df_keys, cluster = clus1, db_merge = "FVS_AllRunsB.db")
#'
#' parallel::stopCluster(clus1)
#'}
#'
#'@import RSQLite
#'
#'@export
#
#'@seealso \code{\link{fvs_load}}\cr \code{\link{fvs_make_keyfiles}}\cr \code{\link{fvs_prototype_keyfile}}\cr \code{\link{fvs_prototype_params}}\cr

fvs_run = function(
  key_df
  ,db_merge = "FVSOut.db"
  ,fvs_commands = "FVS_commands.txt"
  ,merge_dbs= T
  ,cluster=NA
  ,clear_db=T
  ,delete_temp_db = T
  ,append=F
  ,skip_empty = T
){

  t1 = Sys.time()
  print(paste("FVS runs started at",t1))
  ##clear output DBs if specified

  if(merge_dbs & db_merge == basename(db_merge) ){ out_db = file.path(dirname(key_df$output_db[1]), db_merge)
  }else out_db = db_merge

  #the unique per-run output DBs. Computed once up front so the temp-DB cleanup
  #below works even when merge_dbs = FALSE (previously unq_db was defined only
  #inside the merge block, so cleanup with merge_dbs=FALSE threw 'object unq_db
  #not found' AFTER FVS had already run).
  unq_db = unique(key_df$output_db)

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

  #prepare fvs commands. Quote the executable and keyword-file paths so paths
  #containing spaces (e.g. under "Program Files") do not break system().
  key_paths_win = gsub("\\\\\\\\", "\\\\", gsub("/","\\\\",key_df$key_path) )
  fvs_runs = paste0(shQuote(key_df$fvs_path)," --keywordfile=",shQuote(key_paths_win) )
  if(!is.na(fvs_commands)  ){

    dir_cmds = file.path(key_df$fvs_dir[1], "commands")
    if(!file.exists(dir_cmds)) err_in = try(dir.create(dir_cmds) , silent = T)
    dir_cmds_file = file_stamp(path = dir_cmds, prefix = "fvs_commands_v" , suffix = ".txt")
    writeLines(fvs_runs,dir_cmds_file )

  }


  ##run in series
  if(is.na(cluster[1])){
    res_fvs = lapply(fvs_runs,function(x){
      try(system(x,intern=T))
    })
  }

  ###run in parallel. Group commands by their output DB and run each group as
  ###one serial task on a worker, so no two FVS processes write the same SQLite
  ###file concurrently (the per-cluster-DB design only prevents corruption if
  ###each DB is written serially - parLapplyLB over individual commands did not
  ###enforce that).
  if(!is.na(cluster[1])){
    groups = split(fvs_runs, key_df$output_db)
    res_grp = parallel::parLapply(cluster, groups, function(g) lapply(g, function(x) try(system(x,intern=T))))
    res_fvs = unlist(res_grp, recursive = FALSE)
  }

  #merge multiple databases into single database
  if(merge_dbs){

    #make database for all
    if(!append & file.exists(out_db)){
      warning("fvs_run: overwriting existing merge database (append=FALSE): ", out_db)
      unlink(out_db)
    }
    con_dbmrg =  DBI::dbConnect( RSQLite::SQLite(),out_db)

    for(i in 1:length(unq_db)){

      #connect and get lists from dbs; always disconnect this iteration's
      #connection (an open SQLite handle also blocks unlink() of the temp db on
      #Windows). The previous code only closed con_dbi inside the non-empty branch.
      unq_db_i = unq_db[i]
      con_dbi =  RSQLite::dbConnect( RSQLite::SQLite(),unq_db_i)
      tbs_i = dbListTables(con_dbi)

      if((length(tbs_i) == 0) & !skip_empty){
        RSQLite::dbDisconnect( con_dbi)
        stop("input db empty, nothing to merge")
      }

      if((length(tbs_i) > 0) ){

        #iterate through tables and write to merge db
        for(j in 1:length(tbs_i)){
          tbj = dbReadTable(con_dbi ,tbs_i[j])

          #write records if there are any
          if(nrow(tbj)>0) dbWriteTable(con_dbmrg,tbs_i[j], tbj ,append=T )

        }

      }

      #disconnect from intermediate database (every iteration)
      RSQLite::dbDisconnect( con_dbi)

    }
    #disconnect from merge database
    RSQLite::dbDisconnect( con_dbmrg)

  }

  #remove temporary dbs
  if(delete_temp_db) sapply(unq_db,unlink)

  t2 = Sys.time()
  print(paste("FVS runs finished at",t2))
  print(difftime(t2,t1,units="mins"))

  return(res_fvs)
}

