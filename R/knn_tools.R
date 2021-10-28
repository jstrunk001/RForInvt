#'@name knn_tools
#'@title
#' helpers for tree list imputation, knn
#'
#'@description
#'
#' Series of helpers which in combination with the package yaImpute can be used to
#' perform tree-list imputation.
#'
#'@details
#'
#'yaImpute has some funkiness (e.g using row names as an ID field... bad...) and cannot support
#'tree list imputation. The functions here facilitate that process
#'
#' yai_id: Update of yai that replaces row names with record ids
#' newtargets_id: wrapper for newtargets that handles yai_id object
#' impute_id: impute response values to target locations - wrapper for 'impute()' in yaImpute handles id field more explicitly
#' yai_weights: Get weights associated with imputation objects
#' tl_impute: function to impute tree list based on knn model fitted between aerial attributes
#' yai_cv: peform crossvalidation on knn model
#'
#'
#'
#'
#'  This program is free software but it is provided WITHOUT WARRANTY
#'  and with ABSOLUTELY NO GUARANTEE of fitness or functionality for any purpose;
#'  you can redistribute it and/or modify it under the terms of the GNU
#'  General Public License as published by the Free Software Foundation;
#'  either version 2 of the License, or (at your option) any later version.
#'
#'
#'
#'\cr
#'
#'Revision History
#' \tabular{ll}{
#'1.0 \tab  2014 July 04 Import from Jacob's R Library\cr
#'1.0 \tab  2015  Jan 06 Add roxygen header \cr
#'}
#'
#'
#'@author
#'Jacob Strunk <Jacob.strunk@@usgda.gov>
#'
#'@param id_x vector of auxiliary variable column names led by id variable
#'@param id_y vector of response variable column names led by id variable
#'@param omity optional: records to omit
#'@param data data to use for knn
#'@param ... additional arguments to yai in yaImpute package
#'
#'\cr\cr
#' \bold{newtargets_id() parameters:}
#'@param yai_mod model returned by yai_id
#'@param id id column
#'@param data dataset with new xy variables
#'@param ann T/Fsee yai documentation
#'
#' \cr\cr
#' \bold{impute_id() parameters:}
#'@param newtargs_id ?
#'@param ...
#'
#'\cr\cr
#'\bold{yai_weights() parameters:}
#'@param yaimod model returned by yai_id
#'@param dtype distance type
#'@param zero_dist() how to deal with zero distances (e.g. when everything is zero)
#'
#'\cr\cr
#'\bold{tl_impute parameters:}
#'@param wts weights from ?
#'@param env environment with weights in it
#'@param idNm name of id field
#'@param cols_knn_id col names of knn ids
#'@param cols_knn_wt col names of knn wts
#'@param tr_pl tree records
#'@param debug T/F debug?
#'
#'\cr\cr
#'\bold{yai_cv() parameters:}
#'@param omit observation to omit at time i
#'@param idNm name of id field
#'@param x ?
#'@param yNm ?names of response fields
#'@param data input data
#'@param iter_max number of iterations max
#'@param max_comb ??
#'@param k ?number of neighbors?
#'@param debug T/F
#'@param method ? distance approach?
#'@param ... other arguments to ??
#'
#'
#'@return
#'
#'yai_id: \cr
#'newtargets_id: \cr
#'impute_id: \cr
#'yai_weights: \cr
#'tl_impute: \cr
#'yai_cv: \cr
#'
#'@import yaImpute
#'
#'@examples
#'
#'<Delete and Replace>
#'
#'
#'
#'
#'@seealso \code{\link{yaImpute}}\cr \code{\link{fit_pdf}}\cr
#'
#'
#'
#Function 1: Update of yai that replaces row names with record ids
#'@export
#'@rdname knn_tools
yai_id=function(
	xNms = NULL
	,yNms = NULL
	,idNm = NULL
	,data = NULL
	,omity=NULL

	,dup_ids_remove=T
	,...
){


	requireNamespace("yaImpute")
	if(is.null(xNms)){ stop("xNms is required")}
	if(is.null(yNms)){ stop("yNms is required")}
	if(is.null(idNm)){ stop("idNm is required")}
	if(is.null(data)){ stop("data is required")}


	#update formulae
	fx = as.formula(paste("~",paste(xNms,collapse = " + ")))
	fy = as.formula(paste("~",paste(yNms,collapse = " + ")))

	#remove plots with duplicate ids
	if(dup_ids_remove)if(sum(duplicated(data[,idNm]))>0){
		warning("duplicated ids found (this breaks yaImpute), records removed ",paste(data[duplicated(data[,idNm]),idNm],collapse=", "))
		data=data[!duplicated(data[,idNm]),]
	}

	#assign plot ids to rows
	row.names(data)=data[,idNm]

	if(!is.null(omity)) yai_in=yai( x = fx , y = fy , data = data[ , omity ] , ... ) #for crossvalidation
	else yai_in=yai( x = fx , y = fy , data = data , ... )

	#yai_in$updateID=names(id_y)[1]
	yai_in$updateID=idNm
	yai_in$x_call=fx
	yai_in$y_call=fy

	return(yai_in)

}

#'@export
#'@rdname knn_tools
newtargets_id=function(
	yai_mod
	,idNm
	,data
	,k=NULL
	,ann = NULL

){

	requireNamespace("yaImpute")

	#update column names with idNm column
	row.names(data)=data[,idNm]

	#get new targets
	yai_in=newtargets(object=yai_mod,newdata=data,k=k,ann=ann)

	yai_in$update_id=idNm

	yai_in

}


#'@export
#'@rdname knn_tools
impute_id = function(
	newtargs_id
	,...
){

	requireNamespace("yaImpute")
browser()
	#update column names with idNm column
	imp_in=data.frame(cbind(names_in=newtargs_id[["trgRows"]],impute(newtargs_id,...)))
	names(imp_in)[1]=newtargs_id[["update_id"]]
	row.names(imp_in)=NULL
	imp_in

}



#'@export
#'@rdname knn_tools
yai_weights=function(
	yaimod
	,dtype=c("invdist","invdist2","eq")
	,zero_dist=c("small","min","NA")
){

  k_in = yaimod$k
  
  if( k_in > 1 ){
    
  	if(zero_dist[1]=="min") zero_fn=function(x){x[x==0]=min(x[x>0]);x}
  	if(zero_dist[1]=="NA") zero_fn=function(x){x[x==0]=NA;x}
  	if(zero_dist[1]=="small") zero_fn=function(x){x[x==0]=.001;x}
  	
  	#select group for which to calculate weights
  	dist_in=apply(yaimod[["neiDstTrgs"]],2,as.numeric)
  	if(nrow(yaimod[["neiDstTrgs"]])==1) dist_in = data.frame(t(dist_in))
  	dist_id = yaimod$neiIdsTrgs
  
  	ndist=nrow(dist_in)
  
  	#if weight is zero (??):
  	dist_in = data.frame(t(apply(dist_in,1,zero_fn)) )
  
  	#problem with names
  	#if(length(yaimod[["neiDstTrgs"]][1,])==1) names(dist_in) = dimnames(yaimod[["neiDstTrgs"]])[[2]]
  
  	#calculate distances
  	if(dtype[1]=="invdist") wt_in = (1/dist_in)/apply(1/dist_in,1,sum,na.rm=TRUE)
  	if(dtype[1]=="invdist2") wt_in = (1/dist_in^2)/apply(1/dist_in^2,1,sum,na.rm=TRUE)
  	if(dtype[1]=="eq") wt_in = rep(1/(ncol(dist_in)),ndist,na.rm=TRUE)
  	#wt_in=data.frame(dist_id,wt_in,stringsAsFactors=FALSE)
  	names(wt_in)=gsub("Dst[.]","wt.",names(wt_in))
  	
  	#number of columns of weights and references
  	#nwt=ncol(wt_in)
  	
  	#add row for target ids
  	dat_in=data.frame(target=yaimod[["trgRows"]],wt_in,dist_in,yaimod[["neiIdsTrgs"]],row.names=NULL)
  	
  }else{
    
    dat_in=data.frame(target=yaimod[["trgRows"]],wt.k1 = 1,yaimod[["neiDstTrgs"]],yaimod[["neiIdsTrgs"]],row.names=NULL)
    
  }
    
  


	#wt_in[,reord]

	list(
		k = k_in
		, col_dist = grep("Dst[.]k",names(dat_in),value=T)
		, col_wt = grep("wt[.]k",names(dat_in),value=T)
		, col_id = grep("Id[.]k",names(dat_in),value=T)
		, wts = dat_in
	)

}


#impute a tree list
#'@export
#'@rdname knn_tools
tl_impute = function(
	wts
	,idNm
	#,env=parent.env(environment())
	,cols_knn_id = "col_id"
	,cols_knn_wt = "col_wt"
	,trees #tree list with idNm matching wts idNm
	,sort_targets = F
	,debug=F
){
	requireNamespace("data.table")
  requireNamespace("reshape2")
	if(debug) browser()

	#prepare imputed tree list

	#go from horizontal to vertical, facilitate merging with tree list
	m1_dst = reshape2::melt(wts$wts[,c("target",wts$col_dist)],id.vars = c("target"),value.name = "distance",variable.name="dist.k")
	m1_wt = reshape2::melt(wts$wts[,c("target",wts$col_wt)],id.vars = c("target"),value.name = "weight",variable.name="wt.k")
	m1_id = reshape2::melt(wts$wts[,c("target",wts$col_id)],id.vars = c("target"),value.name = "source_id",variable.name="id.k")

	#put together vertical distances, weights, and ids
	res_in = data.frame( target_id = m1_dst[,1] , m1_dst[,-1] , m1_wt[,-1] , m1_id[,-1] )[ order(m1_dst$target) , ]

	#merge weights and trees together
	tl_pd = merge(x = res_in, y = trees , by.x = "source_id", by.y = idNm)
	if(sort_targets) tl_pd = tl_pd[order(tl_pd$target_id),]

	return(tl_pd)
}


#impute a tree list
#'@export
#'@rdname knn_tools
tl_impute_2 = function(
	wts
	,idNm
	,cols_knn_id = "col_id"
	,cols_knn_wt = "col_wt"
	,trees #tree list with idNm matching wts idNm
	,debug=F
){
	requireNamespace("data.table")
	requireNamespace("reshape2")
	requireNamespace("dplyr")

	if(debug) browser()

	#go from horizontal to vertical, facilitate merging with tree list
	m1_dst = reshape2::melt(wts$wts[,c("target",wts$col_dist)],id.vars = c("target"),value.name = "distance",variable.name="dist.k")
	m1_wt = reshape2::melt(wts$wts[,c("target",wts$col_wt)],id.vars = c("target"),value.name = "weight",variable.name="wt.k")
	m1_id = reshape2::melt(wts$wts[,c("target",wts$col_id)],id.vars = c("target"),value.name = "source_id",variable.name="id.k")

	#put together vertical distances, weights, and ids
	res_in = data.frameQ( target_id = m1_dst[,1] , m1_dst[,-1] , m1_wt[,-1] , m1_id[,-1] )

	#merge weights and trees together
	tl_pd = dplyr::left_join(x = res_in, y , y = trees, by = c("source_id" = idNm) )

	return(tl_pd)
}


#rmse cv resampling

#'@export
#'@rdname knn_tools
yai_cv=function(
	omit=5
	,idNm
	,xNm
	,yNm
	,pdNm = NA
	,data
	,iter_max=500
	,k=5
	,debug=FALSE
	,method="msn"
	,min_rows=15
	,method_impute=c("closest","mean","median","dstWeighted")
	,...
){

  if(is.na(pdNm[1])) pdNm = yNm

	#make sure that there are a minimum number of observations
	if(!is.null(data))if(nrow(data)>min_rows){

		requireNamespace("plyr")
		if(method=="rf") requireNamespace("randomForest")

		#remove plots with duplicate ids
		if(sum(duplicated(data[,idNm]))>0){
			warning("duplicated ids found, records removed ",paste(data[duplicated(data[,idNm]),idNm],collapse=", "))
			data=data[!duplicated(data[,idNm]),]
		}

		#assign plot ids to rows
		row.names(data)=data[,idNm]

		nrows_in=nrow(data)
		samples_in=mapply(function(x,size,replace){z=list(rows_omit=sample(x,size,replace));z[["rows_keep"]]=(1:x)[-z[[1]]];z},x=rep(nrows_in,iter_max),size=omit,replace=F,SIMPLIFY = F)

		.fn_cv=function(xNm,yNm,pdNm,k,idNm,dat_cv,method,samp,method_impute){

		  #fit model without cv observations
		  fx = as.formula(paste("~",paste(xNm,collapse = " + ")))
		  fy = as.formula(paste("~",paste(yNm,collapse = " + ")))
			yai_i = yai( x = fx , y = fy , data = dat_cv[samp[["rows_keep"]],]  , k=k ,method=method)

			#impute to holdout observations
			dati_o = dat_cv[samp[["rows_omit"]],]
			targs_i = newtargets( yai_i , newdata = dati_o , k=k , ann=T )
			preds = impute(targs_i, ancillaryData = dat_cv[,pdNm] ,observed=T,k=k,method=method_impute)
      preds[,idNm] = dat_cv[samp[["rows_omit"]],idNm]

			preds
		}

		data_in = data.frame(data,row.names = 1:nrow(data))
		cv_df=plyr::rbind.fill(mapply(.fn_cv,samp=samples_in,MoreArgs = list(xNm=xNm,yNm=yNm,pdNm=pdNm,k=k,idNm=idNm,dat_cv=data_in,method=method,method_impute=method_impute),SIMPLIFY = F))
		nm_obs=grep(".o$",names(cv_df),value=T)
		nm_pred=gsub(".o$","",nm_obs)

		fn_err=function(
			i
			,nms_y
			,nms_y0
			,yy0            #paired predictons and observations
		){

			y=yy0[,nms_y[i]]
			y0=yy0[,nms_y0[i]]
			ei=y-y0
			table_y=table(yy0[,idNm])
			p_i=(table_y/sum(table_y))[as.character(yy0[,idNm])]/sum((table_y/sum(table_y))[as.character(yy0[,idNm])])

			data.frame(
				varb=nms_y[i]
				,bias=sum(ei/p_i,na.rm=T)/sum(1/p_i,na.rm=T)
				,bias_pct_mn=sum(ei/p_i,na.rm=T)/sum(1/p_i,na.rm=T)/mean(y0,na.rm=T)*100
				,rmse=sqrt(sum(1/p_i*(ei-mean(ei,na.rm=T))^2)/sum(1/p_i,na.rm=T))
				,rmse_pct_mn=sqrt(sum(1/p_i*(ei-mean(ei,na.rm=T))^2,na.rm=T)/sum(1/p_i,na.rm=T))/mean(y0,na.rm=T)*100
				,rsq=1-(sum(1/p_i*(ei-mean(ei,na.rm=T))^2,na.rm=T)/sum(1/p_i,na.rm=T))/var(y0,na.rm=T)
				,rmse_pct_sd=sqrt(sum(1/p_i*(ei-mean(ei,na.rm=T))^2,na.rm=T)/sum(1/p_i,na.rm=T))/sd(y0,na.rm=T)*100,meany=mean(y0,na.rm=T),sdy=sd(y0,na.rm=T)
			)

		}

		df_in=data.frame(n=nrow(data),k=k,xNm=paste(xNm,collapse=","),method=method,yy0=do.call(rbind,lapply(1:length(nm_pred),fn_err,nm_pred,nm_obs,cv_df)))
		return(df_in)

	}else{

		data.frame(
			n=nrow(data)
			,k=k
			,xNm=paste(xNm,collapse=","),yNm=paste(yNm,collapse=",")
			,method=method
			,varb=yNm
			,bias=NA
			,bias_pct_mn=NA
			,rmse=NA
			,rmse_pct_mn=NA
			,rsq=NA
			,rmse_pct_sd=NA
		)
	}


}


##  Author(s):
##
##              Jacob L Strunk,
##
##  Date:
##
##             2/28/2013 2:25:24 PM
##
##  Type:
##
##               analysis / function / function list / data processing template
##
##  Function(s) :
##
##              yaiDB
##
##  examples:
##
##    dbDirTest="f:\\AM_Ht1_2012Nov\\69223_USFS_Savannah_River_2009\\Products\\LidarMetricsCSV\\Database\\SRData.sqlite"
##    conTest=dbConnect(SQLite(), dbname=dbDirTest)
##
##    yaiDB(conTest ,yVars=c("ba_hs_l_tp","vol_hs_l_tp","den_hs_l_tp")
##          ,xVars=c("Elev_P20","Elev_P80","Percentage_first_returns_above_1_50","Percentage_all_returns_above_1_50")
##          ,ID="Unqid" ,k=5 ,method="mahalanobis" ,verbose=TRUE )
##
##
##  Project:
##
##          Initiated for USFS Savanna River project
##
##  Stage of Completion:
##
##          Functional
##
##  Description:
##
##          Fit imputation model using database
##          1) Fits an initial imputation model using training data
##          2) iterates through auxiliary data in chunks
##          3) uses imputation model to get wts for aux. chunks
##          4) saves wts to DB
##
##
##
##  Dependencies:
##
##      "C:/SkyDrive/R/Functions/Under Development/KNN/YaiFunctions.r"
##

#
#yaiDB=function(db
#              ,tabRef="plotMetricsPoly"
#                ,filterRef="" #"FOREST_TYP = 0"
#              ,tabTarg="lidarMetricsPoly"
#                ,filterTarg="" #"FOREST_TYP = 0"
#              ,ID
#              ,tabWts="KNNwts"
#              ,indexWtId=TRUE
#              ,doN=1E5
#              ,yVars
#              ,xVars,k=5
#              ,method="mahalanobis"
#              ,annT=TRUE
#              ,yaiMod
#              ,testN
#              ,verbose=TRUE
#              ,row.names=FALSE
#              ,fast=TRUE
#              ,dropOld=TRUE
#              ,refsOnly=FALSE
#              ,append=FALSE
#              ,inMemory=FALSE
#              ){
#  require(yaImpute)
#
#  #function to process chunk of data
#  fnProcIn=function(i,ids,yaiMod,conIn,tabTarg,filterTarg,tabWts,annT,row.names,verbose,refsOnly,append,inMemory,yaiID,newtargetsID,yai_weights){
#
#
#        if(verbose) print(i)
#
#        if(!inMemory){
#
#          #prepare query to rows
#          qryIni=paste("select * from",tabTarg, "where rowid between",(min(ids[[i]]) -1 ),"and",max(ids[[i]]),ifelse(nchar(filterTarg)>0,paste("and",filterTarg),""))
#
#          #read in chunk of data
#          xi=dbGetQuery(conIn,qryIni)
#
#        }else xi=tabTarg
#
#        if(nrow(xi) > 0){
#
#           #convert to numeric if necessary
#           allNms=c(names(as.data.frame(yaiMod[["xRefs"]])),names(as.data.frame(yaiMod[["yRefs"]])))
#           allNms=allNms[allNms %in% names(xi)]
#           #xi[,allNms]=sapply(xi[,allNms],function(x){mode(x)="numeric";x}) #mode(refDat[,c(xVars,yVars)][,sapply(refDat[,c(xVars,yVars)],mode)!="numeric"])="numeric"
#           mode(xi[,allNms][,sapply(xi[,allNms],mode)!="numeric"])="numeric"
#
#
#
#           #the following 2 steps could be made parallel...
#           if(!refsOnly){
#
#             #check for duplicates
#             xi=xi[!duplicated(xi[,ID]),]
#
#             #update yaiMod with chunk of data
#             yaidModNew=newtargetsID(yaiMod,data=xi,ID=ID,ann=annT)
#
#             #get weights from yaidModNew
#             ywIn=yai_weights(yaidModNew,dgroup="targ",)
#
#           } else {
#
#             #get weights for reference group
#             ywIn=yai_weights(yaiMod,dgroup="ref")
#
#           }
#
#         if(!inMemory){
#           #add weights to tabWts
#           if(i==1 & append == FALSE){
#            dbWriteTable(conn=conIn, name=tabWts, value=ywIn, append=FALSE,overwrite=TRUE,row.names=row.names)
#           }else{
#            dbWriteTable(conn=conIn, name=tabWts, value=ywIn, append=TRUE,row.names=row.names)
#           }
#         }else{
#
#          return(ywIn)
#
#         }
#
#
#        }else{
#
#          print("no auxiliary data for which to impute")
#
#        }
#
#
#      }
#
#
#     if(!inMemory){
#        #connect to database
#        if(is.character(db)){
#          conIn=dbConnect("SQLite", dbname = db)
#        }else{ conIn=db}
#
#        #run faster
#        if(fast){
#
#          dbGetQuery(conIn,paste("pragma synchronous = 0"))
#          dbGetQuery(conIn,paste("PRAGMA journal_mode = OFF"))
#          dbGetQuery(conIn,paste("PRAGMA cache_size = 1000000"))
#          dbGetQuery(conIn,paste("PRAGMA temp_store = 2"))
#
#         }
#     }
#
#      #Fit yai model to target data
#      if(missing(yaiMod)){
#
#           #Read in data
#           if(!inMemory) refDat=dbGetQuery(conIn,paste("SELECT * FROM",tabRef,ifelse(nchar(filterRef)>0,paste("where",filterRef),"")))
#           else refDat=tabRef
#
#           #convert to numeric if necessary
#           mode(refDat[,c(xVars,yVars)][,sapply(refDat[,c(xVars,yVars)],mode)!="numeric"])="numeric"
#
#           #make sure that there are some reference data
#           if(nrow(refDat)<10){print("Fewer than 10 reference observations. Perhaps Merge strata first, returning"); return(FALSE)}
#
#           #fit knn models
#           yaiIn=yaiID(IDx=c(ID,xVars),IDy=c(ID,yVars),data=refDat,k=k,method=method)
#
#      }else{
#           yaiIn=yaiMod
#      }
#
#
#      if(!inMemory){
#
#          #get list of tables
#          tabs=dbGetQuery(conIn,paste("select * from sqlite_master"))
#
#          #drop old table
#          if(dropOld) dbGetQuery(conIn,paste("drop table if exists ",tabWts))
#
#          #Get number of rows in database
#          if(verbose) print("nrows")
#          nRows=unlist(dbGetQuery(conIn,paste("SELECT COUNT(*) FROM",tabTarg)))
#          if(verbose) print(paste("nRows=",nRows))
#
#          #subset for testing
#          if(verbose) print("testN")
#          if(!missing(testN)) if(testN<nRows) nRows=testN
#
#          #number of chunks
#           if(doN<nRows){
#              Nchunk=ceiling(nRows/doN)
#              UnqIn=2:nRows
#              splUnq=split(UnqIn,cut(UnqIn,Nchunk))
#           }else{
#              Nchunk=1
#              splUnq=list(A=2:nRows[[1]])
#           }
#           if(verbose){ print("Nchunk"); print(Nchunk) }
#
#      }else{
#              Nchunk=NULL
#              splUnq=NULL
#      }
#
#      #process chunks of data
#      res=lapply(1:Nchunk,fnProcIn,ids=splUnq,yaiMod=yaiIn,conIn=conIn
#             ,tabTarg=tabTarg,filterTarg=filterTarg,tabWts=tabWts
#             ,annT=annT,row.names=row.names,verbose=verbose
#             ,refsOnly=refsOnly,append=append,inMemory=inMemory
#             ,yaiID=yaiID,newtargetsID=newtargetsID,yai_weights=yai_weights)
#
#
#      #index wt Columns
#      if(indexWtId & ! inMemory){
#
#        if(dropOld)  dbGetQuery(conIn,paste("drop index if exists idx_KNN_Wt_ID"))
#
#
#        #prepary query
#        tempWt=dbGetQuery(conIn,paste("SELECT * FROM",tabWts,"limit 5"))
#        idxWtId=paste(grep("Id_k",names(tempWt),value=TRUE),sep="",collapse=", ")
#        qryIdx=paste("create index if not exists idx_KNN_Wt_ID on",tabWts,"(",idxWtId,")")
#
#        #run query
#        dbGetQuery(conIn,qryIdx)
#
#      }
#
#      if(is.character(db)) dbDisconnect(conIn)
#
#      if(inMemory) return(data.frame(do.call(rbind,res)))
#      else return(TRUE)
#
#}
#
#

