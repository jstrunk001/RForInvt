#'@name compile_plots
#'
#'@title Compile tree data by plot
#'
#'@description
#'  Supply data.frame of tree data with plot ids and a data.frame of plot records. The function works by iterating through the tree
#'  data plot by plot. The compile_plots() function accepts a list of functions which compute attributes. Several example functions
#'  are provided, but they are probably not sufficient for operational usage. Hopefully more will be added over time. This function
#'  is meant to work hand in hand with compile_trees() but is still a work in progress.
#'
#'@details
#'  This program is free software but it is provided WITHOUT WARRANTY
#'  and with ABSOLUTELY NO GUARANTEE of fitness or functionality for any purpose;
#'  you can redistribute it and/or modify it under the terms of the GNU
#'  General Public License as published by the Free Software Foundation;
#'  either version 2 of the License, or (at your option) any later version.
#'
#'\cr
#'Revision History
#' \tabular{ll}{
#'1.0 \tab 6/12/2020 Created \cr
#'1.1 \tab 8/23/2024 Modify checks on data.frame inputs to be more generic (e.g., allow tibbles) \cr
#'1.1 \tab 8/23/2024 Add more debug checks \cr
#'1.2 \tab 10/09/2024 complete overhaul, simpler and consistent with compile_trees \cr
#'}
#'
#'@author
#'
#'Jacob Strunk <someone@@somewhere.com>
#'
#'
#'@param df_tree data frame of tree records
#'@param df_plot data frame of plot records
#'@param tree_nms map expected tree column names onto df_tree. These are the minimum expected names: c( plot_ids = "?", tr_ids = c("?","?") , dbh = "?" , ht = "?" , spp = "?" , expansion  = "?"). Feel free to provide others used by custom functions.
#'@param plot_nms map expected plot column names onto df_plot. These are the minimum expected names: c( plot_ids = c("?","?"), plt_wt = c(NA,"?")[1] ). Feel free to provide others used by custom functions.
#'@param plot_filter sqldf string to filter out plots
#'@param tree_filter sqldf string to filter out trees
#'@param dir_out where to write results of plot level compilation
#'@param fns_compute list of optional functions to compute plot level attributes.
#' Optional functions must accept "df_tree" and "..." arguments and generally also (typically) accept "tree_nms"
#' argument, e.g.,
#' \cr
#' \cr
#'      >fn_ba_ac = function(df_tree, tree_nms, ...){ sum(df_tree[,tree_nms["ba_ac"]]) }
#' \cr
#' \cr
#' This example also uses a custom value "ba_ac" value in the tree_nms argument, e.g.,
#' \cr
#' \cr
#'      >tree_nms = c( ba_ac = "BA_AC") )
#' \cr
#' \cr
#'  where "BA_AC" is presumably equal to ba_tree*TPA_tree ...
#'
#'@param return return compiled data
#'@param nclus if nclus > 1 then nclus parallel nodes are split off to process in parallel
#'@param do_debug stop and debug function
#'
#'@param ... additional arguments provided to fns_compute e.g textString="hello" argument in silly function someFun = function(tl,tlNms,textString){data.frame(note=textString)}
#'@param sum_nms <...> argument passed to optional plot_wtsum function,  passed generically by compile_plots through '...'
#'@param n_dom_spp <...>  argument passed to optional spp_y_plot function,  passed generically by compile_plots through '...'
#'@param spp_y <...>  argument passed to optional spp_y_plot function,  passed generically by compile_plots through '...'

#'
#'@return
#'  NULL (return=F) or a data.frame of plot attributes (return=T)
#'
#'@examples
#'
#'
#'    #generate data
#'       test0 = data.frame(plot=1:10, id=1:50,dbh=1:50,spp=sample(letters[1:5],50,T),acres=0.1,ntrees=1)
#'       test0$ht = abs(75*test0$dbh/12 + rnorm(nrow(test0))*3)
#'
#'     #compile trees : processes A and B are equivalent
#'      #A
#'       test1 = ba_ft(test0,tree_nms=c(tree_ids="id",dbh="dbh",dbcl="dbcl",spp="spp"))
#'       test2 = dbcl(test1,tree_nms=c(tree_ids="id",dbh="dbh",dbcl="dbcl",spp="spp") )
#'       test3 = dbcl_y(test2,tree_nms=c(tree_ids="id",dbh="dbh",dbcl="dbcl"), vars_group= c("ba_ft","dbh"))
#'       test4 = spp_y(test3,tree_nms=c(tree_ids="id",dbh="dbh",dbcl="dbcl",spp="spp"), vars_group= c("ba_ft"))
#'
#'     #B
#'     test5 =
#'       compile_trees(
#'         #
#'         test0
#'         ,tree_nms = c(tree_ids="id",dbh="dbh",dbcl="dbcl",spp="spp",acres="acres")
#'         ,vars_group = c("ba_ft")
#'         #optional functions to run against tree data must accept elipsis argument: "..."
#'         ,fns_compute =
#'           list(
#'             tpa
#'             ,ba_ft
#'             ,dbcl
#'             ,dbcl_y
#'             ,spp_y
#'             ,dbcl_spp_y
#'           )
#'
#'       )
#'
#'   test5
#'
#'   #compile plots
#'   res_pl =   compile_plots(
#'     df_tree = test5
#'     , tree_nms = list(plot_ids = c("plot") , tr_ids = c("id") , dbh = "dbh" , ht = "ht" , spp = "spp" , expansion = "TPA" )
#'     , plot_nms = list( plot_ids = c( "plot" ), plt_wt = NA )
#'     , dir_out= file.path("c:/temp/RSForInvt/Compile",format(Sys.Date()))
#'     , fns_compute = list(
#'       plot_lor_qmd
#'        ,plot_wtsum
#'     )
#'     ,return = T
#'     ,do_debug = F
#'     ,nclus = 1
#'     #arguments to custom functions - in this case plot_wtsum
#'     ,sum_nms = c("ntrees",grep("^ba",names(test5),value=T))
#'     ,append = F
#'
#'   )
#'
#'
#'
#'
#'@import plyr parallel sqldf reshape2 RSQLite flock DBI
#'
#'@seealso \code{\link{compile_trees}}\cr \code{\link{parLapplyLB}}\cr

#updates to do:
#	write FIA data processing wrappers
# enable partial completion - e.g. let processing fail on plot ??? and then recover remaining plots - write to db!!
# !! read / write from sqlite database !!

#'@export
#'@rdname compile_plots
compile_plots=function(

  cl = NA
  ,df_tree = list( NA, data.frame(PLT_CN = 1, year = 2020 , dbh = NA , spp = NA , ac = 0.1  ))[[1]]
  ,df_plot = list( NA, data.frame(PLT_CN=c(1,2) , PLOT = 1 , YEAR = 2020, STATE =1 , COUNTY = 1 , PROJECT = "Test"))[[1]]

  ,tree_nms = list( plot_ids = c("PLOT","YEAR") , tree_ids = c("tr_cn") , dbh = "DIA" , ht = "HT" , spp = "SPCD" , expansion = "TPA")
  ,plot_nms = list( plot_ids = c( "PLOT","YEAR" ))

  ,tree_filter = c(NA, "select * from df_tree where dbh > 2 ")
  ,plot_filter = c(NA, "select * from df_plot where YEAR = 2018 and STATE = 'WA' and CONDITION = 1")

  ,dir_out= file.path("c:/temp/RSForInvt/Compile",format(Sys.Date()))
  ,nm_out = "compile_plots"
  ,sql_tbl = "plot_metrics"
  ,append = T #check existing db and see which records need appending

  #functions to compute on tree lists
  #functions must accept ... argument e.g. fn1 = function(x,...) data.frame(mean(x)) ???
	,fns_compute = list(
	   plot_wtsum
	   ,plot_lor_qmd
	 )
	,return = F
	,do_debug = F
	,nclus = 4


  ,... #arguments to custom functions, all custom functions need to accept ...

){

  warning("developer note: add compile summary table to sqlite outputs (date of load, arguments, #records, # errors etc)!!")
  warning("sqlite output approach still new: check your results!!")
  warning("sqlite output approach still new: check your results!!")
  warning("option to append to existing table still being debugged - results!!")


  proc_start = format(Sys.time(),"%Y%b%d_%H%M%S")

  #initial setup
  no_df_tree = !"data.frame" %in% class(df_tree)
  no_df_plot = !"data.frame" %in% class(df_plot)
	if(no_df_tree) stop("Must include \"df_tree\" table - \"df_tree\" was not found")
	if(no_df_plot) warning("\"df_plot\" table not found.\nIf only \"df_tree\" table is provided, please make sure their are NA df_tree records for plots with no trees on them.\n"
																			 ,"If plots without trees are omitted, you will have biased (too high) landscape estimates typically.\nA clearcut is an example of a"
																			 ,"'Forest' plot without any trees on it.")
  if(do_debug) print("completed test function arguments")

  if(is.na(dir_out)) path_sqlite = NA
  if(!is.na(dir_out)){

    #create log directory
    dir_logs = file.path(dir_out,paste0("logs/messages_loading_sqlite_",proc_start))
    dir.create(dir_logs,recursive=T)

    #database setup
    path_sqlite = file.path(dir_out,paste0(nm_out,".sqlite"))
    if(!append) if(file.exists(path_sqlite)) unlink(path_sqlite, force=T)

    #filter records that are already in the sqlite data base
    if(append & file.exists(path_sqlite)){

        #read existing ID records
        con_i = DBI::dbConnect(RSQLite::SQLite(), path_sqlite)
          qry_ids = paste("select",paste(tree_nms[["plot_ids"]],collapse=", "),"from",sql_tbl)
          df_ids_sqlite = DBI::dbGetQuery(con_i,qry_ids)
        DBI::dbDisconnect(con_i)

        #filter duplicates
        unq_id_db = unique(df_ids_sqlite)

        #filter new trees
        #from https://stat.ethz.ch/pipermail/r-help/2007-January/123488.html
        df_tree = df_tree[!is.element(interaction(df_tree[,tree_nms[["plot_ids"]]],drop=T), interaction(unq_id_db[,tree_nms[["plot_ids"]]],drop=T) ) , ]
        if(!no_df_plot) df_plot = df_plot[!is.element(interaction(df_plot[,plot_nms[["plot_ids"]]],drop=T), interaction(unq_id_db[,tree_nms[["plot_ids"]]],drop=T) ) , ]

    } #end filter records from data.base
    if(do_debug) print("completed filter existing records")

    if((nrow(df_tree)==0) & no_df_plot){
      warning("No new trees to load")
      return(NULL)
    }

  }

	#make sure only one out directory is provided
  dir_out = dir_out[1]

	#grab IDs for tree records
	plot_ids_in = tree_nms[["plot_ids"]]

	if(!is.na(dir_out)) if(!dir.exists(dir_out)) dir.create( dir_out , recursive = T )

	#check that tree / plot datasets have matching id fields
	tr_ids_ok = tree_nms[["plot_ids"]] %in% names(df_tree)
	if(sum(!tr_ids_ok) > 0 ) stop(paste("df_tree does not have some plot_ids:",paste(tree_nms[["plot_ids"]][!tr_ids_ok], collapse=", ")))

	#check for matching tree an dplot ids
	if(!no_df_plot){
  	pl_ids_ok = plot_nms[["plot_ids"]] %in% names(df_plot)
  	if(mean(pl_ids_ok) < 1 ) stop(paste("df_plot does not have some plot_ids:",paste(plot_nms[["plot_ids"]][!pl_ids_ok], collapse=", ")))
	}

	#check that tree dataset matches all names in tree_nms
	tree_nms_ok = unlist(tree_nms) %in% names(df_tree)
	if(sum(!tree_nms_ok) > 0 ) stop(paste("column names in df_tree don't match names provided in tree_nms:",paste(unlist(tree_nms)[!tree_nms_ok], collapse=", ")))

	if(do_debug) print("completed test for agreement between plot and tree tables")

	#subset data before compilation
	if(!is.na(plot_filter[1]) & !no_df_plot){
		df_plot_in = sqldf(plot_filter, envir=as.environment(data))
	}
	if((!"df_plot_in" %in% ls()) & !no_df_plot){
		df_plot_in = df_plot
	}
	if(!is.na(tree_filter[1])){
		df_tree_in = sqldf(tree_filter, envir=as.environment(data))
	}
	if((!"df_tree_in" %in% ls())){
		df_tree_in = df_tree
	}

	if(do_debug) print("completed sql subset queries on data")

	#merge trees and plots
	if(("df_plot_in" %in% ls()) & ("df_tree_in" %in% ls())){
		dat_in = merge(x=df_plot, y = df_tree, by = plot_ids_in, all.x=T, all.y=T)
		rm("df_tree_in");gc()
		rm("df_plot_in");gc()
	}
	if((!"df_plot_in" %in% ls()) & ("df_tree_in" %in% ls())){
		dat_in = df_tree_in
		rm("df_tree_in");gc()
	}

	if(do_debug) print("completed merge of plots with trees")

	#get unique id fields
	ids_uniq = unique(dat_in[,plot_ids_in,drop=F])
	if(do_debug) print("completed grab unique trees")

	warning("please update compile_plots function: need to log bad trees, trees without plots or trees with bad tree ids (e.g., tree_id == NA)")
	#stub
	#to do - update code to not compile plots with bad tree ids ?
	# if(F){
	# 	no_tr = is.na(dat_in[tree_nms["id"],])
	# 	dat_notree = dat_in[no_tr,]
	# 	df_tree = dat_in[!no_tr,]
	# }

	if(do_debug){  print("completed -unimplemented - filter / fail trees with no plots") }

	spl_ids = split(ids_uniq,1:nrow(ids_uniq),drop=T)
	if( do_debug && length(spl_ids) > 1000 ) spl_ids = spl_ids[sample( length(spl_ids) , 1000)]

	if(do_debug) print("completed - split trees by plots, on debug use at most 1000 plots ")
#browser()

	if(nclus == 1){

  		res_i = lapply(
  		    spl_ids
  		  , .compile_1plot
  		  , df_tree = dat_in
  		  , tree_nms = tree_nms
  		  , plot_nms = plot_nms
  		  , fns_compute = fns_compute
  		  , path_sqlite = path_sqlite
  		  , sql_tbl = sql_tbl
  		  , dir_logs = dir_logs
  		  , lock_name=tempfile()
  		  , ...
  		  )

		if(F) res_i = lapply(spl_ids[1:3] , .compile_1plot , df_tree = dat_in,  tree_nms = tree_nms , plot_nms = plot_nms, fns_compute = fns_compute ,dir_out=dir_out, ... )

		if(do_debug) print("completed - process tree lists by plot in linear mode (not parallel) ")

	}
	if(nclus >1){


 		clus_in=parallel::makeCluster(nclus)

 		  #create a data.base connection in each environment

  		#process plots
  		res_i = parallel::parLapply(
  		    clus_in
  		  , spl_ids
  		  , .compile_1plot
  		  , df_tree = dat_in
  		  , tree_nms = tree_nms
  		  , plot_nms = plot_nms
  		  , fns_compute = fns_compute
  		  , path_sqlite = path_sqlite
  		  , sql_tbl = sql_tbl
  		  , dir_logs = dir_logs
  		  , lock_name=tempfile()
  		  , ...
  		  )

		parallel::stopCluster(clus_in)
		closeAllConnections()

		if(do_debug) print("completed - process tree lists by plot in parallel")

	}

	#bind plots after compilation
	res_in = plyr::rbind.fill(res_i[sapply(res_i,is.data.frame)])

	if(do_debug) print("completed - merge compile plots using rbind.fill")

	if(!is.na(dir_out)){
		out_csv = file.path(dir_out,paste(nm_out,".csv",sep=""))
		out_rds = file.path(dir_out,paste(nm_out,".rds",sep=""))
		write.csv(res_in,out_csv)
		saveRDS(res_in,out_rds)
	}

	if(do_debug) print("completed - write csv to file")
	gc()
	if(return) return(res_in)

}

#compile tree list form one plot
.compile_1plot=function(
  id
  ,df_tree
  ,tree_nms
  ,plot_nms
  ,sql_tbl
  ,fns_compute
  ,path_sqlite
  ,lock_name
  ,dir_logs

  ,...
){

  #this plot ids with trees, eliminating trees not on this plot
  trs_i = merge(y=df_tree ,x=id, by = tree_nms[["plot_ids"]], all.x = T, all.y=F)

  #get grouping variables as seed columns with id fields

    trs_id = trs_i[1, tree_nms[["plot_ids"]]]

    #iterate through compute functions and append dataframes horizontally
    for(i in 1:length(fns_compute)){

      fni = fns_compute[[i]]
      #if(nrow(trs_i) > 0 ) browser()
      resi = try(fni(df_tree = trs_i, tree_nms = tree_nms , ... ))
      if(class(resi) == "try-error") return(NULL)

      #append tree id fields onto single plot's results
      if(i==1) res_in = data.frame(id, resi)
      if(i>1) res_in = data.frame(res_in, resi)

    }

    #write data and errors to data.base and log files
    if(!is.na(path_sqlite)){
      #make database connection
      con_i = DBI::dbConnect(RSQLite::SQLite(), path_sqlite)

        #lock database
        ll = flock::lock(lock_name)

        #try to write dat to ouput table
        err_i=try(DBI::dbWriteTable(con_i,sql_tbl,res_in,append=T))

        #unlock database
        flock::unlock(ll)

      #disconnect from data.base
      DBI::dbDisconnect(con_i)

      #test for write error
      is_ok = as.character( !class(err_i) == "try-error" )

      #write error results to log file
      file_log_pid = paste0(dir_logs,"/load_sqlite_messages_", Sys.getpid(), ".txt")
      con_in = file(file_log_pid, "a+b")
        write.csv(id,con_in,row.names=F)
        write(paste0("plot loaded to sqlite db ok: ",is_ok),con_in)
        write(paste0("db name: ",path_sqlite),con_in)
        write(paste0("table name: ",sql_tbl,"\n"),con_in)
      close(con_in)
    }
    return(res_in)

}


#subset all dataframes in a list based on cnd
.subset_cn = function(data,cn){
	classes_df = which(sapply(data,is.data.frame))
	for(i in classes_df){
		data_i = data[[i]][!is.na(data[[i]][,"PLT_CN"]) ,	]
		data[[i]] = data_i [data_i[,"PLT_CN"] == cn,	]
	}
	return(data)
}


.subset_ids = function(data,ids){
	classes_df = which(sapply(data,is.data.frame))
	for(i in classes_df){
		ids_ok_i = names(ids)[names(ids) %in% names(data[[i]])]
		data[[i]] = merge(ids,data[[i]],by = ids_ok_i)
	}
	return(data)
}


.subset_ids = function(data,ids){

	mrgi = merge(y=data,x=ids, by = names(ids), all.x = T, all.y=F)
	return(mrgi)
}





#compute attributes from trees by plot
#each function must return a dataframe with 1 row:  zeros or NAs as appropriate if there are no trees
#functions must have elipsis argument ...
#'@export
#'@rdname compile_plots
plot_wtsum = function(
	df_tree
	,tree_nms
	,sum_nms
	,...
){

	#catch records with bad id fields
  if( length(tree_nms[["plot_ids"]]) > 1 ) bad_ids = apply(df_tree[,tree_nms[["plot_ids"]]],1,function(x)sum(is.na(x))>0)
	if( length(tree_nms[["plot_ids"]]) ==1 ) bad_ids = is.na(df_tree[,tree_nms[["plot_ids"]]])
	tr_in = df_tree[!bad_ids,]
	sum_nm_in = sum_nms[sum_nms %in% names(tr_in)]

	#catch empty tree weight (counts) field
	if(! "expansion" %in% names(tree_nms)) stop("must provide expansion factor for each tree, e.g., tpa= 1/acres for imperial measurements")
	#catch bad tree weights
	if(sum(is.na(tr_in[,tree_nms[["expansion"]]]))>0 ) stop("some of the expansions for trees are missing or NA")

	#catch bad columns
	if(length(sum_nms) != length(sum_nm_in)) warning("not all columns provided to plot_wtsum with argument 'sum_nms' are present in table 'df_tree' - only columns present were summed")
	#compute weighted sum - fix this unwieldy code
	if(nrow(tr_in) > 0){
		sum_in = lapply( sum_nm_in , function(col_nm,wt_nm,x,...) data.frame( sum(x[,col_nm]*x[,wt_nm],na.rm=T)), wt_nm = tree_nms[["expansion"]] , x = tr_in )
		sum_in = data.frame(matrix(unlist(sum_in),nrow = 1))
		names(sum_in) = sum_nm_in
	}else{
		sum_in = tr_in[1,sum_nm_in]
		sum_in[1,sum_nm_in] = 0
	}
	return(sum_in)
	gc()
}




#'@export
#'@rdname compile_plots
plot_lor_qmd = function(
	df_tree
	,tree_nms
	,...
){

	#remove trees without ID fields
	bad_ids = apply(is.na(df_tree[,tree_nms[["plot_ids"]],drop=F]),1,function(x) TRUE %in% x )
	tr_in = df_tree[!bad_ids,]

	#catch empty tree weight (counts) field
	if(! "expansion" %in% names(tree_nms)) tree_nms[["expansion"]] = NA
	if(is.na(tree_nms[["expansion"]])){
	  tree_nms[["expansion"]] = "tree_weight"
    df_tree[,tree_nms[["expansion"]]] = 1
	}
	#fix bad tree weights
	tr_in[is.na(tr_in[,tree_nms[["expansion"]]]),] = 1

	if(nrow(tr_in) > 0){

		smry_in = data.frame(
		  mean_dbh = sum(tr_in[,tree_nms[["expansion"]]]*tr_in[,tree_nms[["dbh"]]],na.rm=T) / sum(tr_in[,tree_nms[["expansion"]]],na.rm=T)
			,qmd = sqrt(sum(tr_in[,tree_nms[["expansion"]]]*tr_in[,tree_nms[["dbh"]]]^2,na.rm=T) / sum(tr_in[,tree_nms[["expansion"]]],na.rm=T))
			,lorht = sum(tr_in[,tree_nms[["ht"]]] * tr_in[,tree_nms[["expansion"]]] * tr_in[,tree_nms[["dbh"]]]^2,na.rm=T)/ sum(tr_in[,tree_nms[["expansion"]]] * tr_in[,tree_nms[["dbh"]]]^2,na.rm=T)
			,meanht = sum(tr_in[,tree_nms[["ht"]]] * tr_in[,tree_nms[["expansion"]]] ,na.rm=T)/ sum(tr_in[,tree_nms[["expansion"]]] ,na.rm=T)
			,medianht = median(rep(tr_in[,tree_nms[["ht"]]], tr_in[,tree_nms[["expansion"]]]) ,na.rm=T)
			,ht95 = quantile(rep(tr_in[,tree_nms[["ht"]]], tr_in[,tree_nms[["expansion"]]]),0.95 ,na.rm=T)
			)

	}else{

		smry_in = data.frame(ntree = 0, qmd = NA, lorht = NA )

	}

	return( smry_in )

}

#'@export
#'@rdname compile_plots
#spp_y_plot = function(x,spp_y,sppNm,plot_ids,wt_nm=NA,...){
spp_y_plot = function(
  df_tree
  ,tree_nms
  ,n_dom_spp = 3
  ,spp_y
  #x,sppNm,plot_ids,wt_nm=NA,...
  ,...
){

  require("reshape2")

  #remove trees without ID fields
  bad_ids = apply(is.na(df_tree[,tree_nms[["plot_ids"]],drop=F]),1,function(x) TRUE %in% x )
  if(sum(bad_ids) > 0){
    warning("some records have bad plot_ids fields:", unique(df_tree[bad_ids,tree_nms[["plot_ids"]]]) )
  }
  tr_in = df_tree[!bad_ids,]

  #correct for NA weights
  tr_in[is.na(tr_in[,tree_nms[["dbh"]]]),tree_nms[["dbh"]]] = 0
  tr_in[is.na(tr_in[,tree_nms[["expansion"]]]),tree_nms[["expansion"]]] = 0

  #get data holder for results
  res_in = tr_in[1,tree_nms[["plot_ids"]],drop=F]

  #iterate across response fields
  for(i in 1:length(tree_nms[["domspp_y"]])){

    #compute weighted values
    if(!is.na(tree_nms[["expansion"]])) tr_in[,tree_nms[["domspp_y"]][i]] = tr_in[,tree_nms[["domspp_y"]][i]] * tr_in[,tree_nms[["expansion"]]]

    #cast and aggregate
    mi = reshape2::melt( tr_in[,c(tree_nms[["plot_ids"]],tree_nms[["spp"]],tree_nms[["domspp_y"]][i]) ] , id.vars = c(tree_nms[["plot_ids"]],tree_nms[["spp"]]) )
    fi = as.formula(paste("variable  + ", paste(tree_nms[["plot_ids"]],collapse = "+")," ~ ",tree_nms[["spp"]], sep=""))
    dfi = reshape2::dcast( mi , formula =  fi , fun.aggregate = sum )[,-1]
    dfi1=dfi

    #get dominant species by y
    n_dom = min(ncol(dfi1)-1, n_dom_spp)
    dom_order = order(dfi1[,-1] , decreasing = T)
    spp_nms = names(dfi)[-1]
    nms_mx = paste("dom", tree_nms[["spp"]], tree_nms[["domspp_y"]][i],1:n_dom, sep="_")
    nms_mx_prop = paste("dom_prop", tree_nms[["spp"]], tree_nms[["domspp_y"]][i],1:n_dom, sep="_")
    dfi[,nms_mx] = spp_nms[dom_order][1:n_dom]

    #get proportion by species
    nms_mx_p = paste(spp_nms, tree_nms[["domspp_y"]][i],"p", sep="_")
    y_ord_ndom = dfi1[,-1][dom_order][1:n_dom]
    dfi[,nms_mx_p] = y_ord_ndom / sum(y_ord_ndom)

    #merge data
    res_in = merge(res_in, dfi[,c(tree_nms[["plot_ids"]],nms_mx,nms_mx_p)] , by=tree_nms[["plot_ids"]])

  }
  return(res_in)
}









