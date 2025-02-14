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
#'Jacob Strunk <jacob@@somewhere.something>
#'
#'@param df_tree data frame of tree records
#'@param tree_nms map expected tree column names onto df_tree. These are the minimum expected names: c( tree_id = "tr_id", dbh = "DBH" , ht = "Ht" , spp = "spcd" ). Feel free to provide others used by custom functions.
#'@param fns_compute sequential list of functions to
#'apply to tree data, earlier results (e.g. ba) are available to later
#'functions. Every function should accept an elipsis
#'@param ... arguments to functions in fns_compute
#'@param db_breaks <...>  argument passed to optional dbcl_y function,  passed generically by compile_plots through '...'
#'
#'@return
#'  typically an updated df_tree data.frame (compile_trees function argument) with additional columns. This behavior can be broken using fns_compute
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
#'   test_tl =
#'     compile_trees(
#'       df_fake
#'
#'       #arguments to fns_compute functions
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
#'       ,fns_compute =
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
#'   test_tl
#'
#'   res_pl =   compile_plots(
#'
#'     df_tree = testTL
#'     ,tree_nms = c(plot_ids = c("pltId") , tr_ids = c("trid") , dbh = "db" , ht = "ht" , spp = "spp"  )
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
#'     ,sum_nms = c("TPA",grep("^ba",names(testTL),value=T))
#'
#'   )
#'
#'
#'
#'@import reshape2
#'

#'@seealso \code{\link{dcast}}\cr \code{\link{melt}}\cr \code{\link{compile_plots}}\cr

#updates to do:
# more examples


#'@export
#'@rdname compile_trees
compile_trees=function(

  df_tree
  ,tree_nms = c( tree_id ="tree", dbh = "DIA" , ht = "HT" , spp = "SPCD" , nstems = "nstems", acres="acres", dbcl="dbcl" )
  ,fns_compute = list(
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

  tr_df_in = df_tree

  #iterate through fns_compute and assign names or use internal DF names
  for(i in 1:length(fns_compute)){

    if(do_debug) print(paste("starting aggregate function number",i))

    fni = fns_compute[[i]]
    tr_df_in = fni( tr_df_in ,tree_nms=tree_nms, ... )
    if(class( tr_df_in) != "data.frame" & class( tr_df_in) != "data.table") stop("All functions provided in 'fns_compute = list()' argument must return a dataframe composed of df_tree and any new columns created.")

  }

  return( tr_df_in)


}

#'@export
#'@rdname compile_trees
ba_ft = function(x,tree_nms,...){
  x_in = x
  x_in[,"ba_ft"] = 0.005454 * (x[,tree_nms["dbh"]]^2)
  x_in
}

 # test=data.frame(id=1:50,dbh=1:50)
 # ba_ft(test,tree_nms=c(dbh="dbh",dbcl="dbcl") )

#'@export
#'@rdname compile_trees
tpa = function(x,tree_nms,...){
  if(!"nstems" %in% tree_nms)  res_df = data.frame(x, TPA = 1 / x[,tree_nms["acres"]])
  if("nstems" %in% tree_nms)  res_df = data.frame(x, TPA = x[,tree_nms["nstems"]]  / x[,tree_nms["acres"]] )
  return(res_df)
}

#'Optional compilation function to be supplied in fns_compute list argument: fns_compute = list(ba_ft,...)
#'@export
#'@rdname compile_trees
tph = function(x,tree_nms,...){
  if(!"nstems" %in% tree_nms)  res_df = data.frame(x, TPA = 1 / x[,tree_nms["hectares"]])
  if("nstems" %in% tree_nms)  res_df = data.frame(x, TPA = x[,tree_nms["nstems"]]  / x[,tree_nms["hectares"]] )
  return(res_df)
}


#'@export
#'@rdname compile_trees
dbcl = function(x , tree_nms, dbcl=c(seq(0,32,4),50,1000) , ...){
  x_in = x
  #get dbcls
  labels_dbcl = (dbcl[-1] + dbcl[-length(dbcl)]) / 2
  x_in[,tree_nms["dbcl"]] = labels_dbcl[cut(x_in[,tree_nms["dbh"]],dbcl,labels=FALSE)]
  return(x_in)
}


#'@export
#'@rdname compile_trees
dbcl_y = function(x,tree_nms,vars_group,...){

  require("reshape2")
  x_in = x

  for(i in 1:length(vars_group)){

    #cross dbcl with response attributes
    mi = reshape2::melt(x_in[,c(tree_nms[c("tree_id","dbcl")],vars_group[i])],id.vars= tree_nms[c("tree_id","dbcl")] )
    fi = as.formula(paste("variable +",tree_nms["tree_id"],"~",tree_nms["dbcl"]))
    dfi = reshape2::dcast(mi, formula =  fi)[,-1]
    names(dfi)[-1] = paste(vars_group[i], paste(tree_nms["dbcl"],names(dfi)[-1],sep=""),sep="_")

    #merge back in
    x_in = merge(x_in, dfi, by = tree_nms["tree_id"])
  }
  return(x_in)
}

 # test=data.frame(id=1:50,dbh=1:50)
 # test1 = ba_ft(test,tree_nms=c(dbh="dbh",dbcl="dbcl") )
 # test2 = dbcl(test1,tree_nms=c(dbh="dbh",dbcl="dbcl") )
 # test3 = dbcl_y(test2,tree_nms=c(tree_id="id",dbh="dbh",dbcl="dbcl"), vars_dbcl= c("ba_ft","dbh"))

#'@export
#'@rdname compile_trees
spp_y = function(x,tree_nms,vars_group,...){#tr_id,spp_y,spp_nm,...){

  require("reshape2")
  x_in = x

  for(i in 1:length(vars_group)){

    #cross dbcl with response attributes
    mi = reshape2::melt(x_in[,c(tree_nms[c("tree_id","spp")],vars_group[i])],id.vars=tree_nms[c("tree_id","spp")] )
    fi = as.formula(paste("variable +",tree_nms["tree_id"],"~",tree_nms["spp"]))
    dfi = reshape2::dcast(mi, formula =  fi)[,-1]
    names(dfi)[-1] = paste(vars_group[i], paste(tree_nms["spp"],names(dfi)[-1],sep="_"),sep="_")

    #merge back in
    x_in = merge(x_in, dfi,  by = tree_nms["tree_id"])
  }
  return(x_in)
}

  # test=data.frame(id=1:50,dbh=1:50,spp=sample(letters[1:5],50,T))
  # test1 = ba_ft(test,tree_nms=c(dbh="dbh",dbcl="dbcl") )
  # test2 = dbcl(test1,tree_nms=c(dbh="dbh",dbcl="dbcl") )
  # test3 = dbcl_y(test2,tree_nms=c(tree_id="id",dbh="dbh",dbcl="dbcl"), vars_dbcl= c("ba_ft","dbh"))
  # test4 = spp_y(test3,tree_nms=c(tree_id="id",dbh="dbh",dbcl="dbcl",spp="spp"), vars_group= c("ba_ft"))


#'@export
#'@rdname compile_trees
dbcl_spp_y = function(x,tree_nms,vars_group,...){

  require("reshape2")
  x_in = x

  for(i in 1:length(vars_group)){

    #cross dbcl with response attributes
    mi = reshape2::melt(x_in[,c(tree_nms[c("tree_id","spp","dbcl")],vars_group[i])],id.vars=tree_nms[c("tree_id","spp","dbcl")] )

    #append spp and dbcl to improve readability of final columns
    mi[,tree_nms["spp"]] = paste(vars_group[i],tree_nms["spp"],mi[,tree_nms["spp"]],sep="_")
    mi[,tree_nms["dbcl"]] = paste(tree_nms["dbcl"],mi[,tree_nms["dbcl"]],sep="_")

    #merge data
    fi = as.formula(paste("variable +",tree_nms["tree_id"],"~",tree_nms["spp"],"+",tree_nms["dbcl"]))
    dfi = reshape2::dcast(mi, formula =  fi)[,-1]

    #merge back in
    x_in = merge(x_in, dfi,  by = tree_nms["tree_id"])
  }
  return(x_in)
}

if(F){
  test=data.frame(id=1:50,dbh=1:50,spp=sample(letters[1:5],50,T))
  test1 = ba_ft(test,tree_nms=c(dbh="dbh",dbcl="dbcl") )
  test2 = dbcl(test1,tree_nms=c(dbh="dbh",dbcl="dbcl") )
  test3 = dbcl_y(test2,tree_nms=c(tree_id="id",dbh="dbh",dbcl="dbcl"), vars_dbcl= c("ba_ft","dbh"))
  test4 = spp_y(test3,tree_nms=c(tree_id="id",dbh="dbh",dbcl="dbcl",spp="spp"), vars_group= c("ba_ft"))
  test5 = dbcl_spp_y(test3,tree_nms=c(tree_id="id",dbh="dbh",dbcl="dbcl",spp="spp"), vars_group= c("ba_ft"))
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
#       ,fns_compute =
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

