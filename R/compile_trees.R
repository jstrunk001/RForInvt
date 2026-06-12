#'@name compile_trees
#'
#'@title Compile tree data
#'
#'@description
#'  Compute tree level attributes using optional and custom functions, e.g., ba, volume, Lorey's height
#'
#'@details
#'  Accepts a list of trees and a series of functions (some provided) and additively applies the
#'  functions to the tree list. Something like spp_y(dbcl_y(dbcl(ba_ft(tpa(treeList))))) ... A number
#'  of functions are provided - feel free to modify or update them to meet your needs
#'
#'  ba_ft(x, tree_nms, ...)
#'
#'  tpa(x, tree_nms, ...)
#'
#'  tph(x, tree_nms, ...)
#'
#'  dbcl(x, tree_nms, db_breaks = c(seq(0, 32, 4), 50, 1000), ...)
#'
#'  dbcl_y(x, tree_nms, vars_group, ...)
#'
#'  spp_y(x, tree_nms, vars_group, ...)
#'
#'  dbcl_spp_y(x, tree_nms, vars_group, ...)
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
#'1.1 \tab 10/09/2024 updated to be consistent with compile_plots \cr
#'}
#'
#'@author
#'
#'Jacob Strunk <jacob@@somewhere.something>
#'
#'@param df_tree data frame of tree records
#'@param tree_nms map expected tree column names onto df_tree. These are the minimum expected names: c( tree_ids = "tr_id", dbh = "DBH" , ht = "Ht" , spp = "spcd" ).
#' Feel free to provide others used by custom functions.
#'
#'@param fns_compute sequential list of functions to apply to tree data, earlier
#' results (e.g. ba) are available to later functions. Every function should accept an elipsis.
#'
#'@param ... arguments to functions in fns_compute
#'@param do_debug if TRUE, print progress as each compute function is applied
#'@param x (compute-function helpers ba_ft/tpa/tph/dbcl/...) the tree data.frame passed in by compile_trees
#'@param db_breaks <...>  argument passed to optional dbcl_y function,  passed generically by compile_trees through '...'
#'@param vars_group <...>  argument passed to optional dbcl_y, dbcl_spp_y, spp_y functions,  passed generically by compile_trees through '...'
#'
#'@return
#'  typically an updated df_tree data.frame (compile_trees function argument) with additional columns. This behavior can be broken using fns_compute
#'  functions that behave improperly or have "features" to meet specific user objectives.
#'
#'@examples
#'
#'    #generate data
#'       test0 = data.frame(plot=1:10, id=1:50,dbh=1:50,spp=sample(letters[1:5],50,replace=TRUE),acres=0.1,ntrees=1)
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
#'     , dir_out= NA #set to a directory path to also write csv/rds/sqlite outputs
#'     , fns_compute = list(
#'       plot_lor_qmd
#'        ,plot_wtsum
#'     )
#'     ,return = TRUE
#'     ,do_debug = FALSE
#'     ,nclus = 1
#'     #arguments to custom functions - in this case plot_wtsum
#'     ,sum_nms = c("ntrees",grep("^ba",names(test5),value=TRUE))
#'     ,append = FALSE
#'
#'   )
#'
#'
#'
#'@import reshape2
#'

#'@seealso \code{\link[reshape2]{dcast}}\cr \code{\link[reshape2]{melt}}\cr \code{\link{compile_plots}}\cr

#updates to do:
# more examples


#'@export
#'@rdname compile_trees
compile_trees = function(

  df_tree
  ,tree_nms = c( tree_ids ="tree", dbh = "DIA" , ht = "HT" , spp = "SPCD" , nstems = "nstems", acres="acres", dbcl="dbcl" )
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
    if(!inherits(tr_df_in, c("data.frame","data.table"))) stop("All functions provided in 'fns_compute = list()' argument must return a dataframe composed of df_tree and any new columns created.")

  }

  return( tr_df_in)


}

#'@export
#'@rdname compile_trees
ba_ft = function(x,tree_nms,...){
  x_in = x
  #basal area (ft^2) = (pi/4)/144 * DBH^2 ; the exact constant is 0.005454154
  x_in[,"ba_ft"] = 0.005454154 * (x[,tree_nms["dbh"]]^2)
  x_in
}

 # test=data.frame(id=1:50,dbh=1:50)
 # ba_ft(test,tree_nms=c(dbh="dbh",dbcl="dbcl") )

#'@export
#'@rdname compile_trees
tpa = function(x,tree_nms,...){
  #test for a `nstems` MAPPING (a name in tree_nms), not a value of tree_nms
  if(!"nstems" %in% names(tree_nms))  res_df = data.frame(x, TPA = 1 / x[,tree_nms["acres"]])
  if("nstems" %in% names(tree_nms))  res_df = data.frame(x, TPA = x[,tree_nms["nstems"]]  / x[,tree_nms["acres"]] )
  return(res_df)
}

#'Optional compilation function to be supplied in fns_compute list argument: fns_compute = list(ba_ft,...)
#'@export
#'@rdname compile_trees
tph = function(x,tree_nms,...){
  #test for a `nstems` MAPPING (a name in tree_nms), not a value of tree_nms
  if(!"nstems" %in% names(tree_nms))  res_df = data.frame(x, TPH = 1 / x[,tree_nms["hectares"]])
  if("nstems" %in% names(tree_nms))  res_df = data.frame(x, TPH = x[,tree_nms["nstems"]]  / x[,tree_nms["hectares"]] )
  return(res_df)
}


#'@export
#'@rdname compile_trees
dbcl = function(x , tree_nms, db_breaks=c(seq(0,32,4),50,1000) , ...){
  x_in = x
  #get dbcls
  labels_dbcl = (db_breaks[-1] + db_breaks[-length(db_breaks)]) / 2
  x_in[,tree_nms["dbcl"]] = labels_dbcl[cut(x_in[,tree_nms["dbh"]],db_breaks,labels=FALSE)]
  return(x_in)
}


#'@export
#'@rdname compile_trees
dbcl_y = function(x,tree_nms,vars_group,...){

  x_in = x

  for(i in 1:length(vars_group)){

    #cross dbcl with response attributes
    mi = reshape2::melt(x_in[,c(tree_nms[c("tree_ids","dbcl")],vars_group[i])],id.vars= tree_nms[c("tree_ids","dbcl")] )
    fi = as.formula(paste("variable +",tree_nms["tree_ids"],"~",tree_nms["dbcl"]))
    dfi = reshape2::dcast(mi, formula =  fi)[,-1]
    names(dfi)[-1] = paste(vars_group[i], paste(tree_nms["dbcl"],names(dfi)[-1],sep=""),sep="_")

    #merge back in
    x_in = merge(x_in, dfi, by = tree_nms["tree_ids"])
  }
  return(x_in)
}

 # test=data.frame(id=1:50,dbh=1:50)
 # test1 = ba_ft(test,tree_nms=c(dbh="dbh",dbcl="dbcl") )
 # test2 = dbcl(test1,tree_nms=c(dbh="dbh",dbcl="dbcl") )
 # test3 = dbcl_y(test2,tree_nms=c(tree_ids="id",dbh="dbh",dbcl="dbcl"), vars_dbcl= c("ba_ft","dbh"))

#'@export
#'@rdname compile_trees
spp_y = function(x,tree_nms,vars_group,...){#tr_id,spp_y,spp_nm,...){

  x_in = x

  for(i in 1:length(vars_group)){

    #cross dbcl with response attributes
    mi = reshape2::melt(x_in[,c(tree_nms[c("tree_ids","spp")],vars_group[i])],id.vars=tree_nms[c("tree_ids","spp")] )
    fi = as.formula(paste("variable +",tree_nms["tree_ids"],"~",tree_nms["spp"]))
    dfi = reshape2::dcast(mi, formula =  fi)[,-1]
    names(dfi)[-1] = paste(vars_group[i], paste(tree_nms["spp"],names(dfi)[-1],sep="_"),sep="_")

    #merge back in
    x_in = merge(x_in, dfi,  by = tree_nms["tree_ids"])
  }
  return(x_in)
}

  # test=data.frame(id=1:50,dbh=1:50,spp=sample(letters[1:5],50,T))
  # test1 = ba_ft(test,tree_nms=c(dbh="dbh",dbcl="dbcl") )
  # test2 = dbcl(test1,tree_nms=c(dbh="dbh",dbcl="dbcl") )
  # test3 = dbcl_y(test2,tree_nms=c(tree_ids="id",dbh="dbh",dbcl="dbcl"), vars_dbcl= c("ba_ft","dbh"))
  # test4 = spp_y(test3,tree_nms=c(tree_ids="id",dbh="dbh",dbcl="dbcl",spp="spp"), vars_group= c("ba_ft"))


#'@export
#'@rdname compile_trees
dbcl_spp_y = function(x,tree_nms,vars_group,...){

  x_in = x

  for(i in 1:length(vars_group)){

    #cross dbcl with response attributes
    mi = reshape2::melt(x_in[,c(tree_nms[c("tree_ids","spp","dbcl")],vars_group[i])],id.vars=tree_nms[c("tree_ids","spp","dbcl")] )

    #append spp and dbcl to improve readability of final columns
    mi[,tree_nms["spp"]] = paste(vars_group[i],tree_nms["spp"],mi[,tree_nms["spp"]],sep="_")
    mi[,tree_nms["dbcl"]] = paste(tree_nms["dbcl"],mi[,tree_nms["dbcl"]],sep="_")

    #merge data
    fi = as.formula(paste("variable +",tree_nms["tree_ids"],"~",tree_nms["spp"],"+",tree_nms["dbcl"]))
    dfi = reshape2::dcast(mi, formula =  fi)[,-1]

    #merge back in
    x_in = merge(x_in, dfi,  by = tree_nms["tree_ids"])
  }
  return(x_in)
}


