#'@name compile_plots
#'
#'@title Compile tree data by plot
#'
#'@description
#'  Supply data.frame of tree data with plot ids and a data.frame of plot records. The function works by iterating through the tree
#'  data plot by plot. The compile_plots() function accepts a list of functions which compute attributes. Several example functions
#'  are provided, but they are probably not sufficient for operational usage. Hopefully more will be added over time. This function
#'  is meant to work hand in hand with compileTrees() but is still a work in progress.
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
#'}
#'
#'@author
#'
#'Jacob Strunk <Jstrunk@@fs.fed.us>
#'
#'
#'@param df_tree data frame of tree records
#'@param df_plot data frame of plot records
#'@param df_tree_nms map expected tree column names onto df_tree. These are the expected names: c( plot_ids = "?", tr_ids = c("?","?") , dbh = "?" , ht = "?" , spcd = "?" , tr_wt = "?"). Feel free to provide others used by custom functions.
#'@param df_plot_nms map expected plot column names onto df_plot. These are the expected names: c( plot_ids = c("?","?"), plt_wt = c(NA,"?")[1] ). Feel free to provide others used by custom functions.
#'@param plot_filter sqldf string to filter out plots
#'@param tree_filter sqldf string to filter out trees
#'@param dir_out where to write results of plot level compilation
#'@param fns_compute list of functions to compute plot level attributes. Functions typically accepting plot level tree list and list of tree / plot names and must return a data.frame. Functions must accept ... argument.
#'@param ... additional arguments provided to fns_compute e.g textString="hello" argument in silly function someFun = function(tl,tlNms,textString){data.frame(note=textString)}
#'@param return return compiled data
#'@param nclus if nclus > 1 then nclus parallel nodes are split off to process in parallel
#'
#'@param do_debug stop and debug function
#'
#'@return
#'  NULL (return=F) or a data.frame of plot attributes (return=T)
#'
#'@examples
#'
#'   #build fake tree list
#'   set.seed=111
#'   nfake=50
#'   dbh_fk = 10*abs(rnorm(nfake))
#'   df_fake = data.frame(
#'     pltId = sample((1:7),nfake,replace=T)
#'     ,trid=1:50
#'     ,db= dbh_fk
#'     ,ht=75*dbh_fk + rnorm(nfake)*10
#'     ,spp = sample(c("df","wh","cw","ra") , nfake , T)
#'     ,acres = 0.1
#'     ,trees = round(1+ abs(rnorm(nfake)/3))
#'
#'   )
#'
#'   #compute tree level attributes for fake tree list
#'   testTL =
#'     compileTrees(
#'       df_fake
#'       ,trID = "trid"
#'       ,sppNm = "spp"
#'       ,dbNm = "db"
#'       ,htNm = "ht"
#'       ,dbclNm = "dbcl"
#'       ,dbcl = c(seq(0,32,4),50,1000)
#'       ,dbclY = c("ba_ft")
#'       ,spp_y = c("ba_ft")
#'       ,sppDbclY = c("ba_ft")
#'       ,acresNm = "acres"
#'       ,nTreesNm = NA
#'
#'       ,fns_compute =
#'         list(
#'           tpa
#'           ,ba_ft
#'           ,dbcl
#'           ,dbclY
#'           ,spp_y
#'           ,dbclspp_y
#'         )
#'     )
#'
#'   testTL
#'
#'   #compute plot level attributes from fake tree list
#'   res_pl =   compile_plots(
#'
#'     df_tree = testTL
#'     ,df_tree_nms = c(plot_ids = c("pltId") , tr_ids = c("trid") , dbh = "db" , ht = "ht" , spcd = "spp" , tr_wt = "TPA" )
#'     ,dir_out= file.path("c:/temp/RSForInvt/Compile",format(Sys.Date()))
#'     ,fns_compute = list(
#'       plot_wtmn
#'       ,plot_wtsum
#'     )
#'
#'     ,return = T
#'     ,do_debug = F
#'
#'     ,nclus = 1
#'
#'     #' arguments to custom functions - in this case plot_wtsum
#'     ,vSumNm = c("TPA",grep("^ba",names(testTL),value=T))
#'
#'   )
#'
#'
#'
#'
#'@import plyr parallel sqldf reshape2 RSQLite flock DBI
#'
#'@seealso \code{\link{compileTrees}}\cr \code{\link{parLapplyLB}}\cr

#updates to do:
#	write FIA data processing wrappers
# enable partial completion - e.g. let processing fail on plot ??? and then recover remaining plots - write to db!!
# !! read / write from sqlite database !!

#'@export
#'@rdname compile_plots
compile_plots=function(

  cl = NA
  ,df_tree = data.frame(PLT_CN = 1, year = 2020 , dbh = NA , spcd = NA , ac = 0.1  )
  ,df_plot = list( NA, data.frame (PLT_CN=c(1,2) , PLOT = 1 , YEAR = 2020, STATE =1 , COUNTY = 1 , PROJECT = "Test"))[1]
  ,df_tree_nms = list( plot_ids = c("PLOT", "YEAR") , tr_ids = c("tr_cn") , dbh = "DIA" , ht = "HT" , spcd = "SPCD" , tr_wt = "TPA" )
  ,df_plot_nms = list(plot_ids = c( "STATE" , "COUNTY" , "PROJECT" , "PLOT" , "YEAR" ), plt_wt = NA )
  ,plot_filter = c(NA, "select * from df_plot where YEAR = 2018 and STATE = 'WA' and CONDITION = 1")
  ,tree_filter = c(NA, "select * from df_tree where dbh > 2 ")

  ,dir_out= file.path("c:/temp/RSForInvt/Compile",format(Sys.Date()))
  ,nm_out = "compile_plots"
  ,sql_tbl = "plot_metrics"
  ,append = T #check existing db and see which records need appending

  #functions to compute on tree lists
  #functions must accept ... argument e.g. fn1 = function(x,...) data.frame(mean(x)) ???
	,fns_compute = list(
	   plot_wtmn
	   ,plot_wtsum
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
          qry_ids = paste("select",paste(df_tree_nms$plot_ids,collapse=", "),"from",sql_tbl)
          df_ids_sqlite = DBI::dbGetQuery(con_i,qry_ids)
        DBI::dbDisconnect(con_i)

        #filter duplicates
        unq_id_db = unique(df_ids_sqlite)

        #filter new trees
        #from https://stat.ethz.ch/pipermail/r-help/2007-January/123488.html
        df_tree = df_tree[!is.element(interaction(df_tree[,df_tree_nms$plot_ids],drop=T), interaction(unq_id_db[,df_tree_nms$plot_ids],drop=T) ) , ]
        df_plot = df_plot[!is.element(interaction(df_plot[,df_tree_nms$plot_ids],drop=T), interaction(unq_id_db[,df_tree_nms$plot_ids],drop=T) ) , ]

    } #end filter records from data.base
    if(do_debug) print("completed filter existing records")
  }

	#make sure only one out directory is provided
  dir_out = dir_out[1]

	#grab IDs for tree records
	plot_ids_in = df_tree_nms[["plot_ids"]]

	if(!is.na(dir_out)) if(!dir.exists(dir_out)) dir.create( dir_out , recursive = T )

	#check that tree / plot datasets have matching id fields
	tr_ids_ok = df_tree_nms[["plot_ids"]] %in% names(df_tree)
	if(sum(!tr_ids_ok) > 0 ) stop(paste("df_tree does not have some plot_ids:",df_tree_nms[["plot_ids"]][!tr_ids_ok]))

	#check for matching tree an dplot ids
	if(!no_df_plot){
  	pl_ids_ok = df_plot_nms[["plot_ids"]] %in% names(df_plot)
  	if(mean(pl_ids_ok) < 1 ) stop(paste("df_plot does not have some plot_ids:",df_plot_nms[["plot_ids"]][!pl_ids_ok]))
	}

	#check that tree dataset matches all names in df_tree_nms
	tr_nms_ok = unlist(df_tree_nms) %in% names(df_tree)
	if(sum(!tr_nms_ok) > 0 ) stop(paste("column names in df_tree don't match names provided in df_tree_nms:",unlist(df_tree_nms)[!tr_nms_ok]))

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
	# 	no_tr = is.na(dat_in[tr_nms["id"],])
	# 	dat_notree = dat_in[no_tr,]
	# 	dat_tree = dat_in[!no_tr,]
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
  		  , trs = dat_in
  		  , tr_nms = df_tree_nms
  		  , plot_nms = df_plot_nms
  		  , fns_compute = fns_compute
  		  , path_sqlite = path_sqlite
  		  , sql_tbl = sql_tbl
  		  , dir_logs = dir_logs
  		  , lock_name=tempfile()
  		  , ...
  		  )

		if(F) res_i = lapply(spl_ids[1:3] , .compile_1plot , trs = dat_in,  tr_nms = df_tree_nms , plot_nms = df_plot_nms, fns_compute = fns_compute ,dir_out=dir_out, ... )

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
  		  , trs = dat_in
  		  , tr_nms = df_tree_nms
  		  , plot_nms = df_plot_nms
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
  ,trs
  ,tr_nms
  ,plot_nms
  ,sql_tbl
  ,fns_compute
  ,path_sqlite
  ,lock_name
  ,dir_logs

  ,...
){

  #this plot ids with trees, eliminating trees not on this plot
  trs_i = merge(y=trs ,x=id, by = tr_nms[["plot_ids"]], all.x = T, all.y=F)

  #get grouping variables as seed columns with id fields

    trs_id = trs_i[1, tr_nms[["plot_ids"]]]

    #iterate through compute functions and append dataframes horizontally
    for(i in 1:length(fns_compute)){
      fni = fns_compute[[i]]
      #if(nrow(trs_i) > 0 ) browser()
      resi = try(fni(trs = trs_i, tr_nms = tr_nms , ... ))
      if(class(resi) == "try-error") return(NULL)

      #append tree id fields onto single plot's results
      if(i==1) res_in = data.frame(id, resi)
      if(i>1) res_in = data.frame(id, resi)
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
	trs
	,tr_nms
	,vSumNm
	,...
){
	#catch records without any actual trees - and set to zero
	bad_ids = is.na(trs[,tr_nms[["plot_ids"]]])
	tr_in = trs[!bad_ids,]
	sumNm_in = vSumNm[vSumNm %in% names(tr_in)]

	if(length(vSumNm) != length(sumNm_in)) warning("not all columns provided to plot_wtsum with argument 'vSumNm' are present in table 'trs' - only columns present were summed")
	if(nrow(tr_in) > 0){
		sum_in = lapply( sumNm_in , function(colNm,wtNm,x,...) data.frame( sum(x[,colNm]*x[,wtNm],na.rm=T)), wtNm = tr_nms[["tr_wt"]] , x = tr_in )
		sum_in = data.frame(matrix(unlist(sum_in),nrow = 1))
		names(sum_in) = sumNm_in
	}else{
		sum_in = tr_in[1,sumNm_in]
		sum_in[1,sumNm_in] = 0
	}
	return(sum_in)
	gc()
}




#'@export
#'@rdname compile_plots
plot_wtmn = function(
	trs
	,tr_nms
	,...
){

	#remove trees without ID fields
	bad_ids = apply(is.na(trs[,tr_nms[["plot_ids"]],drop=F]),1,function(x) TRUE %in% x )
	tr_in = trs[!bad_ids,]

	#correct for NA weights
	tr_in[is.na(tr_in[,tr_nms[["dbh"]]]),] = 0
	tr_in[is.na(tr_in[,tr_nms[["tr_wt"]]]),] = 0

	if(nrow(tr_in) > 0){

		wtmn = data.frame(
			ntree = nrow(tr_in)
			,qmd = sqrt(sum(tr_in[,tr_nms[["tr_wt"]]]*tr_in[,tr_nms[["dbh"]]]^2,na.rm=T) / sum(tr_in[,tr_nms[["tr_wt"]]],na.rm=T))
			,lorht = sum(tr_in[,tr_nms[["ht"]]] * tr_in[,tr_nms[["tr_wt"]]] * tr_in[,tr_nms[["dbh"]]]^2,na.rm=T)/ sum(tr_in[,tr_nms[["tr_wt"]]] * tr_in[,tr_nms[["dbh"]]]^2,na.rm=T)
		)

	}else{

		wtmn = data.frame(ntree = 0, ba_ftac = 0, ba_ftac_ge3 = 0, ba_mh_ge3 = 0, qmd = NA, lorht = NA)

	}

	return( wtmn )

}

#'@export
#'@rdname compile_plots
#spp_y_plot = function(x,spp_y,sppNm,plot_ids,wtNm=NA,...){
spp_y_plot = function(
  trs
  ,tr_nms
  ,n_dom_spp = 3
  ,spp_y
  #x,sppNm,plot_ids,wtNm=NA,...
  ,...
){

  require("reshape2")

  #remove trees without ID fields
  bad_ids = apply(is.na(trs[,tr_nms[["plot_ids"]],drop=F]),1,function(x) TRUE %in% x )
  if(sum(bad_ids) > 0){
    warning("some records have bad plot_ids fields:", unique(trs[bad_ids,tr_nms[["plot_ids"]]]) )
  }
  tr_in = trs[!bad_ids,]

  #correct for NA weights
  tr_in[is.na(tr_in[,tr_nms[["dbh"]]]),tr_nms[["dbh"]]] = 0
  tr_in[is.na(tr_in[,tr_nms[["tr_wt"]]]),tr_nms[["tr_wt"]]] = 0

  #get data holder for results
  res_in = tr_in[1,tr_nms[["plot_ids"]],drop=F]

  #iterate across response fields
  for(i in 1:length(tr_nms[["domspp_y"]])){

    #compute weighted values
    if(!is.na(tr_nms[["tr_wt"]])) tr_in[,tr_nms[["domspp_y"]][i]] = tr_in[,tr_nms[["domspp_y"]][i]] * tr_in[,tr_nms[["tr_wt"]]]

    #cast and aggregate
    mi = reshape2::melt( tr_in[,c(tr_nms[["plot_ids"]],tr_nms[["spcd"]],tr_nms[["domspp_y"]][i]) ] , id.vars = c(tr_nms[["plot_ids"]],tr_nms[["spcd"]]) )
    fi = as.formula(paste("variable  + ", paste(tr_nms[["plot_ids"]],collapse = "+")," ~ ",tr_nms[["spcd"]], sep=""))
    dfi = reshape2::dcast( mi , formula =  fi , fun.aggregate = sum )[,-1]
    dfi1=dfi

    #get dominant species by y
    n_dom = min(ncol(dfi1)-1, n_dom_spp)
    dom_order = order(dfi1[,-1] , decreasing = T)
    spp_nms = names(dfi)[-1]
    nms_mx = paste("dom", tr_nms[["spcd"]], tr_nms[["domspp_y"]][i],1:n_dom, sep="_")
    nms_mx_prop = paste("dom_prop", tr_nms[["spcd"]], tr_nms[["domspp_y"]][i],1:n_dom, sep="_")
    dfi[,nms_mx] = spp_nms[dom_order][1:n_dom]

    #get proportion by species
    nms_mx_p = paste(spp_nms, tr_nms[["domspp_y"]][i],"p", sep="_")
    y_ord_ndom = dfi1[,-1][dom_order][1:n_dom]
    dfi[,nms_mx_p] = y_ord_ndom / sum(y_ord_ndom)

    #merge data
    res_in = merge(res_in, dfi[,c(tr_nms[["plot_ids"]],nms_mx,nms_mx_p)] , by=tr_nms[["plot_ids"]])

  }
  return(res_in)
}









