
#compile tree list form one plot
.compile_1plot=function(
  id
  ,trs
  ,trNms
  ,plotNms
  ,fnCompute
  ,...
){

  trs_i = .subsetIDs(trs,id)

  #get grouping variables as seed columns with id fields
  trs_ID = trs_i[1, trNms[["plotIDs"]]]

  #iterate through compute functions and append dataframes horizontally
  for(i in 1:length(fnCompute)){
    fni = fnCompute[[i]]
    #if(nrow(trs_i) > 0 ) browser()
    resi = try(fni(trs = trs_i, trNms = trNms , ... ))
    if(class(resi) == "try-error") return(NULL)
    if(i==1) res_in = data.frame(trs_ID, resi)
    if(i>1) res_in = data.frame(res_in, resi)
  }
  return(res_in)

}

.compile_1plot=function(
  ...
){

  trs_i = .subsetIDs(trs,id)

  #get grouping variables as seed columns with id fields
  trs_ID = trs_i[1, trNms[["plotIDs"]]]

  #iterate through compute functions and append dataframes horizontally
  for(i in 1:length(fnCompute)){
    fni = fnCompute[[i]]
    #if(nrow(trs_i) > 0 ) browser()
    resi = try(fni( ... ))
    if(class(resi) == "try-error") return(NULL)
    if(i==1) res_in = data.frame(trs_ID, resi)
    if(i>1) res_in = data.frame(res_in, resi)
  }
  return(res_in)

}

# .compile_1plot=function(...){
#
#   trs_i = .subsetIDs(list(...)$trs,list(...)$id)
#
#   #get grouping variables as seed columns with id fields
#   trs_ID = trs_i[1, trNms[["plotIDs"]]]
#
#   #iterate through compute functions and append dataframes horizontally
#   for(i in 1:length(fnCompute)){
#     fni = fnCompute[[i]]
#     #if(nrow(trs_i) > 0 ) browser()
#     resi = try(fni(trs = trs_i, trNms = trNms , ... ))
#     if(class(resi) == "try-error") return(NULL)
#     if(i==1) res_in = data.frame(trs_ID, resi)
#     if(i>1) res_in = data.frame(res_in, resi)
#   }
#   return(res_in)
#
# }


#subset all dataframes in a list based on cnd
.subset_cn = function(data,cn){
	classes_df = which(sapply(data,is.data.frame))
	for(i in classes_df){
		data_i = data[[i]][!is.na(data[[i]][,"PLT_CN"]) ,	]
		data[[i]] = data_i [data_i[,"PLT_CN"] == cn,	]
	}
	return(data)
}


.subset_IDs = function(data,IDs){
	classes_df = which(sapply(data,is.data.frame))
	for(i in classes_df){
		IDsOK_i = names(IDs)[names(IDs) %in% names(data[[i]])]
		data[[i]] = merge(IDs,data[[i]],by = IDsOK_i)
	}
	return(data)
}


.subsetIDs = function(data,IDs){

	mrgi = merge(y=data,x=IDs, by = names(IDs), all.x = T, all.y=F)
	return(mrgi)
}





#compute attributes from trees by plot
#each function must return a dataframe with 1 row:  zeros or NAs as appropriate if there are no trees
#functions must have elipsis argument ...
#'@export
#'@rdname compilePlots
plotWtSum = function(
	trs
	,trNms
	,vSumNm
	,...
){
	#catch records without any actual trees - and set to zero
	bad_ids = is.na(trs[,trNms[["plotIDs"]]])
	tr_in = trs[!bad_ids,]
	sumNm_in = vSumNm[vSumNm %in% names(tr_in)]

	if(length(vSumNm) != length(sumNm_in)) warning("not all columns provided to plotWtSum with argument 'vSumNm' are present in table 'trs' - only columns present were summed")
	if(nrow(tr_in) > 0){
		sum_in = lapply( sumNm_in , function(colNm,wtNm,x,...) data.frame( sum(x[,colNm]*x[,wtNm],na.rm=T)), wtNm = trNms[["trWt"]] , x = tr_in )
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
#'@rdname compilePlots
plotWtMn = function(
	trs
	,trNms
	,...
){

	#remove trees without ID fields
	bad_ids = apply(is.na(trs[,trNms[["plotIDs"]],drop=F]),1,function(x) TRUE %in% x )
	tr_in = trs[!bad_ids,]

	#correct for NA weights
	tr_in[is.na(tr_in[,trNms[["dbh"]]]),] = 0
	tr_in[is.na(tr_in[,trNms[["trWt"]]]),] = 0

	if(nrow(tr_in) > 0){

		wtmn = data.frame(
			ntree = nrow(tr_in)
			,ba_ftac = sum(.005454*tr_in[,trNms[["dbh"]]]^2*tr_in[,trNms[["trWt"]]],na.rm=T)
			,ba_ftac_ge3 = sum((.005454*tr_in[,trNms[["dbh"]]]^2*tr_in[,trNms[["trWt"]]])[tr_in[,trNms[["dbh"]]] > 3],na.rm=T)
			,ba_m3ha_ge3 = sum((.005454*tr_in[,trNms[["dbh"]]]^2*tr_in[,trNms[["trWt"]]])[tr_in[,trNms[["dbh"]]] > 3],na.rm=T)*(0.3048^2)/0.404686
			,qmd = sqrt(sum(tr_in[,trNms[["trWt"]]]*tr_in[,trNms[["dbh"]]]^2,na.rm=T) / sum(tr_in[,trNms[["trWt"]]],na.rm=T))
			,lorht = sum(tr_in[,trNms[["ht"]]] * tr_in[,trNms[["trWt"]]] * tr_in[,trNms[["dbh"]]]^2,na.rm=T)/ sum(tr_in[,trNms[["trWt"]]] * tr_in[,trNms[["dbh"]]]^2,na.rm=T)
		)

	}else{

		wtmn = data.frame(ntree = 0, ba_ftac = 0, ba_ftac_ge3 = 0, ba_mh_ge3 = 0, qmd = NA, lorht = NA)

	}

	return( wtmn )

}

#'@export
#'@rdname compilePlots
#sppYplot = function(x,sppY,sppNm,plotIDs,wtNm=NA,...){
sppYplot = function(
  trs
  ,trNms
  ,nDomSpp = 3
  ,sppY
  #x,sppNm,plotIDs,wtNm=NA,...
  ,...
){

  require("reshape2")

  #remove trees without ID fields
  bad_ids = apply(is.na(trs[,trNms[["plotIDs"]],drop=F]),1,function(x) TRUE %in% x )
  if(sum(bad_ids) > 0){
    warning("some records have bad plotIDs fields:", unique(trs[bad_ids,trNms[["plotIDs"]]]) )
  }
  tr_in = trs[!bad_ids,]

  #correct for NA weights
  tr_in[is.na(tr_in[,trNms[["dbh"]]]),trNms[["dbh"]]] = 0
  tr_in[is.na(tr_in[,trNms[["trWt"]]]),trNms[["trWt"]]] = 0

  #get data holder for results
  res_in = tr_in[1,trNms[["plotIDs"]],drop=F]

  #iterate across response fields
  for(i in 1:length(trNms[["domSppY"]])){

    #compute weighted values
    if(!is.na(trNms[["trWt"]])) tr_in[,trNms[["domSppY"]][i]] = tr_in[,trNms[["domSppY"]][i]] * tr_in[,trNms[["trWt"]]]

    #cast and aggregate
    mi = reshape2::melt( tr_in[,c(trNms[["plotIDs"]],trNms[["spcd"]],trNms[["domSppY"]][i]) ] , id.vars = c(trNms[["plotIDs"]],trNms[["spcd"]]) )
    fi = as.formula(paste("variable  + ", paste(trNms[["plotIDs"]],collapse = "+")," ~ ",trNms[["spcd"]], sep=""))
    dfi = reshape2::dcast( mi , formula =  fi , fun.aggregate = sum )[,-1]
    dfi1=dfi

    #get dominant species by y
    n_dom = min(ncol(dfi1)-1, nDomSpp)
    dom_order = order(dfi1[,-1] , decreasing = T)
    spp_nms = names(dfi)[-1]
    nmsMx = paste("dom", trNms[["spcd"]], trNms[["domSppY"]][i],1:n_dom, sep="_")
    nmsMxProp = paste("dom_prop", trNms[["spcd"]], trNms[["domSppY"]][i],1:n_dom, sep="_")
    dfi[,nmsMx] = spp_nms[dom_order][1:n_dom]

    #get proportion by species
    nmsMx_p = paste(spp_nms, trNms[["domSppY"]][i],"p", sep="_")
    y_ord_ndom = dfi1[,-1][dom_order][1:n_dom]
    dfi[,nmsMx_p] = y_ord_ndom / sum(y_ord_ndom)

    #merge data
    res_in = merge(res_in, dfi[,c(trNms[["plotIDs"]],nmsMx,nmsMx_p)] , by=trNms[["plotIDs"]])

  }
  return(res_in)
}

#'
##@export
##@rdname compilePlots
#' plotSppDom = function(
#'   trs
#'   ,trNms
#'   ,...
#' ){
#'
#'   #remove trees without ID fields
#'   bad_ids = is.na(trs[,trNms[["plotIDs"]]])
#'   tr_in = trs[!bad_ids,]
#'
#'   #correct for NA weights
#'   tr_in[is.na(tr_in[,trNms[["dbh"]]]),] = 0
#'   tr_in[is.na(tr_in[,trNms[["trWt"]]]),] = 0
#'
#'   if(nrow(tr_in) > 0){
#'
#'     wtmn = data.frame(
#'       ntree = nrow(tr_in)
#'       ,ba_ftac = sum(.005454*tr_in[,trNms[["dbh"]]]^2*tr_in[,trNms[["trWt"]]],na.rm=T)
#'       ,ba_ftac_ge3 = sum((.005454*tr_in[,trNms[["dbh"]]]^2*tr_in[,trNms[["trWt"]]])[tr_in[,trNms[["dbh"]]] > 3],na.rm=T)
#'       ,ba_m3ha_ge3 = sum((.005454*tr_in[,trNms[["dbh"]]]^2*tr_in[,trNms[["trWt"]]])[tr_in[,trNms[["dbh"]]] > 3],na.rm=T)*(0.3048^2)/0.404686
#'       ,qmd = sqrt(sum(tr_in[,trNms[["trWt"]]]*tr_in[,trNms[["dbh"]]]^2,na.rm=T) / sum(tr_in[,trNms[["trWt"]]],na.rm=T))
#'       ,lorht = sum(tr_in[,trNms[["ht"]]] * tr_in[,trNms[["dbh"]]]^2,na.rm=T)/ sum(tr_in[,trNms[["dbh"]]]^2,na.rm=T)
#'     )
#'
#'   }else{
#'
#'     wtmn = data.frame(ntree = 0, ba_ftac = 0, ba_ftac_ge3 = 0, ba_mh_ge3 = 0, qmd = NA, lorht = NA)
#'
#'   }
#'
#'   return( wtmn )
#'
#' }


# plotWtMn_B = function(
# 	trs
# 	,trNms
# 	,fnArg
# 	,...
# ){
#
# 	if(nrow(trs)>1)browser()
# 	bad_ids = is.na(trs[,trNms[["id"]]])
# 	tr_in = trs[!bad_ids,]
#
# 	wtmn = data.frame(
#
# 		ntree = nrow(tr_in)
# 		,ba_ftac = sum(.005454*tr_in[,trNms[["dbh"]]]^2*tr_in[,trNms[["trWt"]]],na.rm=T)
# 		,ba_ftac_ge3 = sum((.005454*tr_in[,trNms[["dbh"]]]^2*tr_in[,trNms[["trWt"]]])[tr_in[,trNms[["dbh"]]] > 3],na.rm=T)
# 		,ba_m3ha_ge3 = sum((.005454*tr_in[,trNms[["dbh"]]]^2*tr_in[,trNms[["trWt"]]])[tr_in[,trNms[["dbh"]]] > 3],na.rm=T)*(0.3048^2)/0.404686
# 		,qmd = sqrt(sum(tr_in[,trNms[["trWt"]]]*tr_in[,trNms[["dbh"]]]^2,na.rm=T) / sum(tr_in[,trNms[["trWt"]]],na.rm=T))
# 		,lorht = sum(tr_in[,trNms[["ht"]]] * tr_in[,trNms[["dbh"]]]^2,na.rm=T)/ sum(tr_in[,trNms[["dbh"]]]^2,na.rm=T)
#
# 	)
#
# 	if(nrow(tr_in) > 0){
# 	}else{
# 		wtmn = data.frame(ntree = 0, ba_ftac = 0, ba_ftac_ge3 = 0, ba_mh_ge3 = 0, qmd = NA, lorht = NA)
# 	}
#
# 	return( wtmn )
#
# }



##@export
##@rdname compileTrees
#' dbclSppY_id = function(x,ID,sppY,dbclNm,sppNm,...){
#'
#'   require("reshape2")
#'   x_in = x
#'
#'   for(i in 1:length(sppY)){
#'
#'     #cross dbcl with response attributes
#'     mi = reshape2::melt(x_in[,c(ID,sppNm,dbclNm,sppY[i])],id.vars=c(ID,sppNm,dbclNm) )
#'
#'     #append spp and dbcl to improve readability of final columns
#'     mi[,sppNm] = paste(sppY[i],sppNm,mi[,sppNm],sep="_")
#'     mi[,dbclNm] = paste(dbclNm,mi[,dbclNm],sep="_")
#'
#'     #merge data
#'     fi = as.formula(paste("variable +",ID,"~",sppNm,"+",dbclNm))
#'     dfi = reshape2::dcast(mi, formula =  fi)[,-1]
#'
#'     #merge back in
#'     x_in = merge(x_in, dfi,  by = ID)
#'   }
#'   return(x_in)
#' }


#
# #test this code with fake trees
# if(F){
#
#   set.seed=111
#   nfake=50
#   dbh_fk = 10*abs(rnorm(nfake))
#   df_fake = data.frame(
#                         pltId = sample((1:7),nfake,replace=T)
#                        ,trid=1:50
#                        ,db= dbh_fk
#                        ,ht=75*dbh_fk + rnorm(nfake)*10
#                        ,spp = sample(c("df","wh","cw","ra") , nfake , T)
#                        ,acres = 0.1
#                        ,trees = round(1+ abs(rnorm(nfake)/3))
#
#   )
#
#   testTL =
#     compileTrees(
#       df_fake
#       ,trID = "trid"
#       ,sppNm = "spp"
#       ,dbNm = "db"
#       ,htNm = "ht"
#       ,dbclNm = "dbcl"
#       ,dbcl = c(seq(0,32,4),50,1000)
#       ,dbclY = c("ba_ft")
#       ,sppY = c("ba_ft")
#       ,sppDbclY = c("ba_ft")
#       ,acresNm = "acres"
#       ,nTreesNm = NA
#
#       ,fnCompute =
#         list(
#           tpa
#           ,ba_ft
#           ,dbcl
#           ,dbclY
#           ,sppY
#           ,dbclSppY
#         )
#     )
#
#   testTL
#
#    res_pl =   compilePlots(
#
#       dfTree = testTL
#       ,dfTreeNms = c(plotIDs = c("pltId") , trIDs = c("trid") , dbh = "db" , ht = "ht" , spcd = "spp" , trWt = "TPA" )
#       ,dir_out= file.path("c:/temp/RSForInvt/Compile",format(Sys.Date()))
#       ,fnCompute = list(
#         plotWtMn
#         ,plotWtSum
#       )
#
#       ,return = T
#       ,doDebug = F
#
#       ,nclus = 1
#
#       #arguments to custom functions
#       ,vSumNm = c("TPA",grep("^ba",names(testTL),value=T))
#
#     )
#
# }


#
#
# if(F){
#
#   tltest = readRDS("D:\\Box\\VMARS\\Projects\\2019 Savannah River\\R\\Jacob Post-Stratification Evaluation\\data/tlManuscript_20200515.RDS")
#
#   compilePlots(
#
#     data = list(
#       dfTree = tltest
#       #, dfPlot = data.frame()
#     )
#     ,trNms = c( trIDs = c("UNQ_TR") , plotIDs = "Plot" , dbh = "DIA" , ht = "HT" , spcd = "SPCD" , trWt = "TPA" )
#     ,plotNms = c(plotIDs = c( "STATE" , "COUNTY" , "PROJECT" , "PLOT" , "YEAR" ) )
#
#     ,plot_filter = c(NA, "select * from dfPlot where YEAR = 2018 and STATE = 'WA' and CONDITION = 1")
#     ,tree_filter = c(NA, "select * from dfTree where dbh > 2 ")
#
#     ,dir_out= file.path("c:/temp/RSForInvt/Compile",format(Sys.Date()))
#
#     ,fnCompute = list(
#       plotWtMn(dfTree_in , trNms , dbh_units = c("in","cm") , ht_units = c("ft","m") )
#       ,plotWtSum( dfTree_in , trNms , vSumNm = c('DRYBIOM',"VOLBFNET" , "VOLCFGRS", "VOLCFNET", "CARBON_AG", "DRYBIO_AG","DRYBIOT") )
#       #, other custom tree compilation functions
#     )
#
#     ,return = F
#     ,doDebug = F
#
#     ,nclus = 4
#
#   )
#
# }
#
#
# if(F){
#
# 	source("compileTrees")
# 	if(!"tr1" %in% ls()) tr1 = readRDS("D:\\data\\RFIA\\NIMS\\2018-10-24\\tr.rds")
# 	if(!"tr2" %in% ls()) tr2 = compileTrees(tr1)
#
# }
#
# if(F){
#
#
# 	#load and fix FIA data
# 	if(!"fiaDat" %in% ls()){
#
# 		dat_paths = list.files("D:\\data\\RFIA\\NIMS\\2018-10-24\\",full.names=T,pattern="[.]rds$")
# 		tree0 = compileTrees(readRDS("D:\\data\\RFIA\\NIMS\\2018-10-24\\tr.rds"))
# 		plot0 = readRDS("D:\\data\\RFIA\\NIMS\\2018-10-24\\pl_snp.rds")
# 		plot0[,c("PLT_CN")] = plot0[,c("CN")]	#fix weird cn inconsistency
# 		cond0 = readRDS("D:\\data\\RFIA\\NIMS\\2018-10-24\\cond.rds")
#
# 		fiaDat = list(
# 			plot = plot0
# 			,tree = tree0
# 			,cond = cond0
# 		)
#
# 	}
# 	#these are columns added to tree0 by compileTrees
# 	vSumNm = c('DRYBIOM',"VOLBFNET" , "VOLCFGRS", "VOLCFNET", "CARBON_AG", "DRYBIO_AG","DRYBIOT",'ba_ft','ba_ft_dbcl2','ba_ft_dbcl6','ba_ft_dbcl10','ba_ft_dbcl14','ba_ft_dbcl18','ba_ft_dbcl22','ba_ft_dbcl26','ba_ft_dbcl30','ba_ft_dbcl41','ba_ft_dbcl525','ba_ft_dbclNA','ba_ft_SPGRPCD10','ba_ft_SPGRPCD11','ba_ft_SPGRPCD12','ba_ft_SPGRPCD13','ba_ft_SPGRPCD15','ba_ft_SPGRPCD17','ba_ft_SPGRPCD18','ba_ft_SPGRPCD19','ba_ft_SPGRPCD21','ba_ft_SPGRPCD22','ba_ft_SPGRPCD24','ba_ft_SPGRPCD44','ba_ft_SPGRPCD45','ba_ft_SPGRPCD46','ba_ft_SPGRPCD47','ba_ft_SPGRPCD48'
# 						 ,'SPGRPCD_dbcl_10_2','SPGRPCD_dbcl_10_6','SPGRPCD_dbcl_10_10','SPGRPCD_dbcl_10_14','SPGRPCD_dbcl_10_18','SPGRPCD_dbcl_10_22','SPGRPCD_dbcl_10_26','SPGRPCD_dbcl_10_30','SPGRPCD_dbcl_10_41','SPGRPCD_dbcl_10_525','SPGRPCD_dbcl_10_NA','SPGRPCD_dbcl_11_2','SPGRPCD_dbcl_11_6','SPGRPCD_dbcl_11_10','SPGRPCD_dbcl_11_14','SPGRPCD_dbcl_11_18','SPGRPCD_dbcl_11_22','SPGRPCD_dbcl_11_26','SPGRPCD_dbcl_11_30','SPGRPCD_dbcl_11_41','SPGRPCD_dbcl_11_525','SPGRPCD_dbcl_11_NA'
# 						 ,'SPGRPCD_dbcl_12_2','SPGRPCD_dbcl_12_6','SPGRPCD_dbcl_12_10','SPGRPCD_dbcl_12_14','SPGRPCD_dbcl_12_18','SPGRPCD_dbcl_12_22','SPGRPCD_dbcl_12_26','SPGRPCD_dbcl_12_30','SPGRPCD_dbcl_12_41','SPGRPCD_dbcl_12_525','SPGRPCD_dbcl_12_NA','SPGRPCD_dbcl_13_2','SPGRPCD_dbcl_13_6','SPGRPCD_dbcl_13_10','SPGRPCD_dbcl_13_14','SPGRPCD_dbcl_13_18','SPGRPCD_dbcl_13_22','SPGRPCD_dbcl_13_26','SPGRPCD_dbcl_13_30','SPGRPCD_dbcl_13_41','SPGRPCD_dbcl_13_525','SPGRPCD_dbcl_13_NA'
# 						 ,'SPGRPCD_dbcl_15_2','SPGRPCD_dbcl_15_6','SPGRPCD_dbcl_15_10','SPGRPCD_dbcl_15_14','SPGRPCD_dbcl_15_18','SPGRPCD_dbcl_15_22','SPGRPCD_dbcl_15_26','SPGRPCD_dbcl_15_30','SPGRPCD_dbcl_15_41','SPGRPCD_dbcl_15_525','SPGRPCD_dbcl_15_NA','SPGRPCD_dbcl_17_2','SPGRPCD_dbcl_17_6','SPGRPCD_dbcl_17_10','SPGRPCD_dbcl_17_14','SPGRPCD_dbcl_17_18','SPGRPCD_dbcl_17_22','SPGRPCD_dbcl_17_26','SPGRPCD_dbcl_17_30','SPGRPCD_dbcl_17_41','SPGRPCD_dbcl_17_525','SPGRPCD_dbcl_17_NA'
# 						 ,'SPGRPCD_dbcl_18_2','SPGRPCD_dbcl_18_6','SPGRPCD_dbcl_18_10','SPGRPCD_dbcl_18_14','SPGRPCD_dbcl_18_18','SPGRPCD_dbcl_18_22','SPGRPCD_dbcl_18_26','SPGRPCD_dbcl_18_30','SPGRPCD_dbcl_18_41','SPGRPCD_dbcl_18_NA','SPGRPCD_dbcl_19_2','SPGRPCD_dbcl_19_6','SPGRPCD_dbcl_19_10','SPGRPCD_dbcl_19_14','SPGRPCD_dbcl_19_18','SPGRPCD_dbcl_19_22','SPGRPCD_dbcl_19_26','SPGRPCD_dbcl_19_30','SPGRPCD_dbcl_19_41','SPGRPCD_dbcl_19_525','SPGRPCD_dbcl_19_NA','SPGRPCD_dbcl_21_2'
# 						 ,'SPGRPCD_dbcl_21_6','SPGRPCD_dbcl_21_10','SPGRPCD_dbcl_21_14','SPGRPCD_dbcl_21_18','SPGRPCD_dbcl_21_22','SPGRPCD_dbcl_21_26','SPGRPCD_dbcl_21_30','SPGRPCD_dbcl_21_NA','SPGRPCD_dbcl_22_2','SPGRPCD_dbcl_22_6','SPGRPCD_dbcl_22_10','SPGRPCD_dbcl_22_14','SPGRPCD_dbcl_22_18','SPGRPCD_dbcl_22_22','SPGRPCD_dbcl_22_26','SPGRPCD_dbcl_22_30','SPGRPCD_dbcl_22_41','SPGRPCD_dbcl_22_525','SPGRPCD_dbcl_22_NA','SPGRPCD_dbcl_24_2','SPGRPCD_dbcl_24_6','SPGRPCD_dbcl_24_10'
# 						 ,'SPGRPCD_dbcl_24_14','SPGRPCD_dbcl_24_18','SPGRPCD_dbcl_24_22','SPGRPCD_dbcl_24_26','SPGRPCD_dbcl_24_30','SPGRPCD_dbcl_24_41','SPGRPCD_dbcl_24_525','SPGRPCD_dbcl_24_NA','SPGRPCD_dbcl_44_2','SPGRPCD_dbcl_44_6','SPGRPCD_dbcl_44_10','SPGRPCD_dbcl_44_14','SPGRPCD_dbcl_44_18','SPGRPCD_dbcl_44_22','SPGRPCD_dbcl_44_26','SPGRPCD_dbcl_44_30','SPGRPCD_dbcl_44_41','SPGRPCD_dbcl_44_525','SPGRPCD_dbcl_44_NA','SPGRPCD_dbcl_45_2','SPGRPCD_dbcl_45_6','SPGRPCD_dbcl_45_10'
# 						 ,'SPGRPCD_dbcl_45_14','SPGRPCD_dbcl_45_18','SPGRPCD_dbcl_45_22','SPGRPCD_dbcl_45_26','SPGRPCD_dbcl_45_30','SPGRPCD_dbcl_45_41','SPGRPCD_dbcl_45_NA','SPGRPCD_dbcl_46_2','SPGRPCD_dbcl_46_6','SPGRPCD_dbcl_46_10','SPGRPCD_dbcl_46_14','SPGRPCD_dbcl_46_18','SPGRPCD_dbcl_46_26','SPGRPCD_dbcl_46_41','SPGRPCD_dbcl_46_NA','SPGRPCD_dbcl_47_2','SPGRPCD_dbcl_47_6','SPGRPCD_dbcl_47_10','SPGRPCD_dbcl_47_14','SPGRPCD_dbcl_47_18','SPGRPCD_dbcl_47_22','SPGRPCD_dbcl_47_26'
# 						 ,'SPGRPCD_dbcl_47_30','SPGRPCD_dbcl_47_41','SPGRPCD_dbcl_47_525','SPGRPCD_dbcl_47_NA','SPGRPCD_dbcl_48_NA')
#
#
# 	plDat = compilePlots(
#
# 		data=fiaDat
# 		,dir_out = c(file.path("d:/data/RFIA/Compile/",format(Sys.Date())),NA)
# 		,doDebug = F
# 		,fnArg = list(
# 			vSumNm = vSumNm
# 			#vSumNm = c('DRYBIOM',"VOLBFNET" , "VOLCFGRS", "VOLCFNET", "CARBON_AG", "DRYBIO_AG","DRYBIOT")
# 		)
#
# 	)
#
#
# }
#
# #only runs if outside of a function call
# #this allows sourcing to re-load functions that are being debugged while inside of another function
# if(identical(environment(),.GlobalEnv) & F){
#
# 	#load and fix FIA data
# 	if(!"tree" %in% ls() | T){
#
# 		tree = read.csv("D:\\data\\RFIA\\mergeFIA\\2019-08-26\\mergeFIA.csv")
# 		#cond0 = readRDS("D:\\data\\RFIA\\NIMS\\2018-10-24\\cond.rds")
#
# 	}
# 	compilePlots( data = list(tree = tree), return = T, tree_filter = "select * from tree where INVYR > 2013 and STATECD = 53", doDebug = F,nclus=5
# 								#,plotIDs = c("PLT_CN","PLOT","INVYR","STATECD","COUNTYCD","CTY_CN","PLOT_STATUS_CD","EVAL_GRP")
# 								,plotIDs = c("PLT_CN","PLOT","INVYR","STATECD","COUNTYCD","CTY_CN","PLOT_STATUS_CD")
# 	)
#
#
# }

if(F){

  cl2 = makeCluster(2)

  fn1=function(clarg, x, ...){

    res1 = parLapply(clarg,x,fn2,...)

    print(res1)

    res2 = parLapply(clarg,x,fn3,...)

    print(res2)

  }

  fn2=function(id,y,...){

    y

  }

  fn3=function(id,z,...){

    z

  }


  fn1(cl2, x=1:5, y=10 , z=20)

}else{

  cl2 = makeCluster(2)

  fn1=function(clarg, x, ...){

    res1 = parLapply(clarg,x,fn2,...)

    print(res1)

    res2 = parLapply(clarg,x,fn3,...)

    print(res2)

  }

  fn2=function(id,y,...){

    y

  }

  fn3=function(id,z,...){

    z

  }


  fn1(cl2, x=1:5, y=10 , z=20)



}

