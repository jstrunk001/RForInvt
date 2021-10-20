#'@title
#'create template key file that is applied to each record or stand - meant to be used with fvs_make_keyfiles()
#'
#'@description
#'create template key file that is applied to each record or stand - meant to be used with fvs_make_keyfiles()
#'
#'@details
#'create template key file that is applied to each record or stand - meant to be used with fvs_make_keyfiles()
#'items surrounded by @@ symbols are replaced by matching column names in fvs_make_keyfiles(), e.g. @@cn@@
#'
#'
#'\cr
#'Revision History
#' \tabular{ll}{
#'1.0 \tab 2021 Oct 15 Created\cr
#'}
#'
#'@author
#'
#'Jacob Strunk <strunky@@gmail.com>
#'
#'@param title item in keyword file to replace - st to NULL to omit keyword
#'@param invyr item in keyword file to replace
#'@param timeint item in keyword file to replace
#'@param numcycle item in keyword file to replace
#'@param notriple item in keyword file to replace
#'@param nodgl item in keyword file to replace
#'@param dgstdev item in keyword file to replace
#'@param treelist item in keyword file to replace
#'@param compute item in keyword file to replace
#'@param other_keywords add additional keyword as separate strings, e.g, other_keywords = c("thiskey 1","thatkey 0")
#'
#'@return
#'  a single updated keyword prototype
#'
#'@examples
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
#'@export
#
#'@seealso \code{\link{fvs_run}}\cr \code{\link{fvs_load_rfvs}}\cr

fvs_prototype_keyfile = function(
  title = "!!title: FVS key prototype - RForBio"
  ,std_id = "@cn@               Run 1"
  ,std_cn = "@cn@"
  ,mgmt_id = "A001"
  ,invyr = "InvYear       @invyr@"
  ,timeint = "TimeInt                 @timeint@"
  ,numcycle = "NumCycle     @numcycle@"
  ,notriple = NULL #"NoTriple"
  ,nodgl =    NULL #"NODGL"
  ,dgstdev =   NULL #"DGSTDEV           0"
  ,treelist =    "Treelist       0                   0"
  ,cutlist = "Cutlist        0                   0"
  ,compute =    "COMPUTE           0"
  ,other_keywords = c(NULL)
){
  return(
    c(
        title
        ,paste("!!built:",as.character(Sys.time()))
        ,"StdIdent"
        ,std_id
        ,"StandCN"
        ,std_cn
        ,"MgmtId"
        ,mgmt_id
        ,invyr
        ,timeint
        ,numcycle
        ,notriple
        ,nodgl
        ,dgstdev
        ,"DataBase"
        ,"DSNOut"
        ,"@output_db@"
        ,"* FVS_Summary, FVS_Compute"
        ,"Summary        2"
        ,"Compute            0         1"
        ,"End"
        ,"* FVS_TreeList, FVS_Cutlist "
        ,treelist
        ,cutlist
        ,"Database"
        ,"Treelist       2"
        ,"Cutlist        2"
        ,"End"
        ,"DelOTab            1"
        ,"DelOTab            2"
        ,"DelOTab            4"
        ,"!Exten:base Name:From: FVS_GroupAddFilesAndKeywords"
        ,"Database"
        ,"DSNIn"
        ,"@input_db@"
        ,"StandSQL"
        ,"SELECT * FROM FVS_StandInit"
        ,"WHERE Stand_ID= '%StandID%'"
        ,"EndSQL"
        ,"TreeSQL"
        ,"SELECT * FROM FVS_TreeInit"
        ,"WHERE Stand_ID= '%StandID%'"
        ,"EndSQL"
        ,"END"
        ,"SPLabel"
        ,"  All_Stands"
        ,"Process"
        ,"Stop"
      )
  )
}



