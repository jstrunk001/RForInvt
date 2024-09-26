#'@name compile_trees
#'
#'@title Compile tree data
#'
#'@description
#'  Compute tree level attributes using optional and custom functions, e.g., ba, volume, Lorey's height
#'
#'@details
#'  Accepts a list of trees and a series of functions (some provided) and additively applies the
#'  functions to the tree list. Something like computeVolume(computeBA(computeTPA(treeList))) ... A number
#'  of functions are provided - feel free to modify or update them to meet your needs
#'
#'  ba_ft(x, db_nm, ...)
#'
#'  tpa(x, acres_nm = NA, ntrees_nm = NA, ...)
#'
#'  tph(x, haNm = NA, ntrees_nm = NA, ...)
#'
#'  dbcl(x, db_nm = "dbh", dbcl = c(seq(0, 32, 4), 50, 1000), dbcl_nm = "dbcl", ...)
#'
#'  dbcl_y(x, trID, dbcl_nm, dbcl_y, ...)
#'
#'  spp_y(x, trID, spp_y, spp_nm, ...)
#'
#'  dbcl_spp_y(x, trID, spp_y, dbcl_nm, spp_nm, ...)
#'
#'  This program is free software but it is provided WITHOUT WARRANTY
#'  and with ABSOLUTELY NO GUARANTEE of fitness or functionality for any purpose;
#'  you can redistribute it and/or modify it under the terms of the GNU
#'  General Public License as published by the Free Software Foundation;
#'  either version 2 of the License, or (at your option) any later version.
#'
#'\cr
#'Revision History
#' \tabular{ll}{
#'1.0 \tab 6/10/2020 Created  \cr
#'}
#'
#'@author
#'
#'Jacob Strunk <Jstrunk@@fs.fed.us>
#'
#'@param tr_df data frame of tree records
#'@param fn_compute sequential list of functions to
#'apply to tree data, earlier results (e.g. ba) are available to later
#'functions. Every function should accept an elipsis
#'@param ... arguments to functions in fn_compute
#'
#'
#'@return
#'  typically an updated tr_df data.frame (compile_trees function argument) with additional columns. This behavior can be broken using fn_compute
#'  functions that behave improperly or have "features" to meet specific user objectives.
#'
#'@examples
#'
#'
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
#'   testTL =
#'     compile_trees(
#'       df_fake
#'
#'       #arguments to fn_compute functions
#'       ,tr_id = "trid"
#'       ,spp_nm = "spp"
#'       ,db_nm = "db"
#'       ,htNm = "ht"
#'       ,dbcl_nm = "dbcl"
#'       ,dbcl = c(seq(0,32,4),50,1000)
#'       ,dbcl_y = c("ba_ft")
#'       ,spp_y = c("ba_ft")
#'       ,spp_dbcl_y = c("ba_ft")
#'       ,acres_nm = "acres"
#'       ,ntrees_nm = NA
#'
#'       #optional functions to run against tree data
#'       #must accept ...
#'       ,fn_compute =
#'         list(
#'           tpa
#'           ,ba_ft
#'           ,dbcl
#'           ,dbcl_y
#'           ,spp_y
#'           ,dbcl_spp_y
#'         )
#'
#'     )
#'
#'   testTL
#'
#'
#'
#'@import reshape2
#'

#'@seealso \code{\link{dcast}}\cr \code{\link{melt}}\cr \code{\link{compilePlots}}\cr

#updates to do:
# more examples


#'@export
#'@rdname compile_trees
compile_trees=function(

  tr_df
  ,fn_compute = list(
    tpa
    ,ba_ft
    ,dbcl
    ,dbcl_y
    ,spp_y
    ,dbcl_spp_y
  )
  ,do_debug=F

  ,...

){

  tr_df_in = tr_df

  #iterate through fn_compute and assign names or use internal DF names
  for(i in 1:length(fn_compute)){

    if(do_debug) print(paste("starting aggregate function number",i))

    fni = fn_compute[[i]]
    tr_df_in = fni( tr_df_in , ... )
    if(class( tr_df_in) != "data.frame" & class( tr_df_in) != "data.table") stop("All functions provided in 'fnsCompute = list()' argument must return a dataframe composed of tr_df and any new columns created.")

  }

  return( tr_df_in)


}

#'@export
#'@rdname compile_trees
ba_ft = function(x,db_nm,...) data.frame(x, ba_ft = 0.005454 * (x[,db_nm]^2))


#'@export
#'@rdname compile_trees
tpa = function(x,acres_nm=NA,ntrees_nm=NA,...){
  if(is.na(acres_nm)) stop("acres_nm not provided when using RSForInvt::tpa probably from compile_trees" )
  if(is.na(ntrees_nm))  res_df = data.frame(x, TPA = 1 / x[,acres_nm])
  if(!is.na(ntrees_nm))  res_df = data.frame(x, TPA = x[,ntrees_nm]  / x[,acres_nm] )
  return(res_df)
}

#'Optional compilation function to be supplied in fn_compute list argument: fn_compute = list(ba_ft,...)
#'@export
#'@rdname compile_trees
tph = function(x,haNm=NA,ntrees_nm=NA,...){
  if(is.na(acres_nm)) stop("haNm not provided when using RSForInvt::tph probably from compile_trees" )
  if(is.na(ntrees_nm))  res_df = data.frame(x, TPH = 1 / x[,haNm])
  if(!is.na(ntrees_nm))  res_df = data.frame(x, TPH = x[,ntrees_nm]  /x[,acres_nm] )
  return(res_df)
}


#'@export
#'@rdname compile_trees
dbcl = function(x , db_nm="dbh" , dbcl=c(seq(0,32,4),50,1000) , dbcl_nm = "dbcl", ...){
  labels_dbcl = (dbcl[-1] + dbcl[-length(dbcl)]) / 2
  res_dbcl = data.frame(labels_dbcl[cut(x[,db_nm],dbcl,labels=FALSE)])
  names(res_dbcl) = dbcl_nm
  res_df = data.frame(x,res_dbcl)
  return(res_df)
}


#'@export
#'@rdname compile_trees
dbcl_y = function(x,tr_id,dbcl_nm,dbcl_y,...){

  require("reshape2")
  x_in = x

  for(i in 1:length(dbcl_y)){

    #cross dbcl with response attributes
    mi = reshape2::melt(x_in[,c(tr_id,dbcl_nm,dbcl_y[i])],id.vars=c(tr_id,dbcl_nm) )
    fi = as.formula(paste("variable +",tr_id,"~",dbcl_nm))
    dfi = reshape2::dcast(mi, formula =  fi)[,-1]
    names(dfi)[-1] = paste(dbcl_y[i], paste(dbcl_nm,names(dfi)[-1],sep=""),sep="_")

    #merge back in
    x_in = merge(x_in, dfi, by = tr_id)
  }
  return(x_in)
}


#'@export
#'@rdname compile_trees
spp_y = function(x,tr_id,spp_y,spp_nm,...){

  require("reshape2")
  x_in = x

  for(i in 1:length(spp_y)){

    #cross dbcl with response attributes
    mi = reshape2::melt(x_in[,c(tr_id,spp_nm,spp_y[i])],id.vars=c(tr_id,spp_nm) )
    fi = as.formula(paste("variable +",tr_id,"~",spp_nm))
    dfi = reshape2::dcast(mi, formula =  fi)[,-1]
    names(dfi)[-1] = paste(spp_y[i], paste(spp_nm,names(dfi)[-1],sep="_"),sep="_")

    #merge back in
    x_in = merge(x_in, dfi,  by = tr_id)
  }
  return(x_in)
}

#'@export
#'@rdname compile_trees
dbcl_spp_y = function(x,tr_id,spp_y,dbcl_nm,spp_nm,...){

  require("reshape2")
  x_in = x

  for(i in 1:length(spp_y)){

    #cross dbcl with response attributes
    mi = reshape2::melt(x_in[,c(tr_id,spp_nm,dbcl_nm,spp_y[i])],id.vars=c(tr_id,spp_nm,dbcl_nm) )

    #append spp and dbcl to improve readability of final columns
    mi[,spp_nm] = paste(spp_y[i],spp_nm,mi[,spp_nm],sep="_")
    mi[,dbcl_nm] = paste(dbcl_nm,mi[,dbcl_nm],sep="_")

    #merge data
    fi = as.formula(paste("variable +",tr_id,"~",spp_nm,"+",dbcl_nm))
    dfi = reshape2::dcast(mi, formula =  fi)[,-1]

    #merge back in
    x_in = merge(x_in, dfi,  by = tr_id)
  }
  return(x_in)
}

#test this code
# if(F){
#
#   set.seed=111
#   nfake=50
#   dbh_fk = 10*abs(rnorm(nfake))
#   df_fake = data.frame(
#     pltId = sample((1:7),nfake,replace=T)
#     ,trid=1:50
#     ,db= dbh_fk
#     ,ht=75*dbh_fk + rnorm(nfake)*10
#     ,spp = sample(c("df","wh","cw","ra") , nfake , T)
#     ,acres = 0.1
#     ,trees = round(1+ abs(rnorm(nfake)/3))
#
#   )
#
#   testTL =
#     compile_trees(
#       df_fake
#       ,tr_id = "trid"
#       ,spp_nm = "spp"
#       ,db_nm = "db"
#       ,htNm = "ht"
#       ,dbcl_nm = "dbcl"
#       ,dbcl = c(seq(0,32,4),50,1000)
#       ,dbcl_y = c("ba_ft")
#       ,spp_y = c("ba_ft")
#       ,spp_dbcl_y = c("ba_ft")
#       ,acres_nm = "acres"
#       ,ntrees_nm = NA
#
#       ,fn_compute =
#         list(
#           tpa
#           ,ba_ft
#           ,dbcl
#           ,dbcl_y
#           ,spp_y
#           ,dbcl_spp_y
#         )
#     )
#
#   testTL
#
# }

