#'@name compileTrees
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
#'  ba_ft(x, dbNm, ...)
#'
#'  tpa(x, acresNm = NA, nTreesNm = NA, ...)
#'
#'  tph(x, haNm = NA, nTreesNm = NA, ...)
#'
#'  dbcl(x, dbNm = "dbh", dbcl = c(seq(0, 32, 4), 50, 1000), dbclNm = "dbcl", ...)
#'
#'  dbclY(x, trID, dbclNm, dbclY, ...)
#'
#'  sppY(x, trID, sppY, sppNm, ...)
#'
#'  dbclSppY(x, trID, sppY, dbclNm, sppNm, ...)
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
#'@param tlDF data frame of tree records
#'@param fnCompute sequential list of functions to
#'apply to tree data, earlier results (e.g. ba) are available to later
#'functions. Every function should accept an elipsis
#'@param ... arguments to functions in fnCompute
#'
#'
#'@return
#'  typically an updated tlDF data.frame (compileTrees function argument) with additional columns. This behavior can be broken using fnCompute
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
#'     compileTrees(
#'       df_fake
#'
#'       #arguments to fnCompute functions
#'       ,trID = "trid"
#'       ,sppNm = "spp"
#'       ,dbNm = "db"
#'       ,htNm = "ht"
#'       ,dbclNm = "dbcl"
#'       ,dbcl = c(seq(0,32,4),50,1000)
#'       ,dbclY = c("ba_ft")
#'       ,sppY = c("ba_ft")
#'       ,sppDbclY = c("ba_ft")
#'       ,acresNm = "acres"
#'       ,nTreesNm = NA
#'
#'       #optional functions to run against tree data
#'       #must accept ...
#'       ,fnCompute =
#'         list(
#'           tpa
#'           ,ba_ft
#'           ,dbcl
#'           ,dbclY
#'           ,sppY
#'           ,dbclSppY
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
#'@rdname compileTrees
compileTrees=function(

  tlDF
  ,fnCompute = list(
    tpa
    ,ba_ft
    ,dbcl
    ,dbclY
    ,sppY
    ,dbclSppY
  )
  ,do_debug=F

  ,...

){

  tlDF_in = tlDF

  #iterate through computers and assign names or use internal DF names
  for(i in 1:length(fnCompute)){

    if(do_debug) print(paste("starting aggregate function number",i))

    fni = fnCompute[[i]]
    tlDF_in = fni(tlDF_in,...)
    if(class(tlDF_in) != "data.frame" & class(tlDF_in) != "data.table") stop("All functions provided in 'fnsCompute = list()' argument must return a dataframe composed of tlDF and any new columns created.")

  }

  return(tlDF_in)


}

#'@export
#'@rdname compileTrees
ba_ft = function(x,dbNm,...) data.frame(x, ba_ft = 0.005454 * (x[,dbNm]^2))


#'@export
#'@rdname compileTrees
tpa = function(x,acresNm=NA,nTreesNm=NA,...){
  if(is.na(acresNm)) stop("acresNm not provided when using RSForInvt::tpa probably from compileTrees" )
  if(is.na(nTreesNm))  res_df = data.frame(x, TPA = 1 / x[,acresNm])
  if(!is.na(nTreesNm))  res_df = data.frame(x, TPA = x[,nTreesNm]  / x[,acresNm] )
  return(res_df)
}

#'Optional compilation function to be supplied in fnCompute list argument: fnCompute = list(ba_ft,...)
#'@export
#'@rdname compileTrees
tph = function(x,haNm=NA,nTreesNm=NA,...){
  if(is.na(acresNm)) stop("haNm not provided when using RSForInvt::tph probably from compileTrees" )
  if(is.na(nTreesNm))  res_df = data.frame(x, TPH = 1 / x[,haNm])
  if(!is.na(nTreesNm))  res_df = data.frame(x, TPH = x[,nTreesNm]  /x[,acresNm] )
  return(res_df)
}


#'@export
#'@rdname compileTrees
dbcl = function(x , dbNm="dbh" , dbcl=c(seq(0,32,4),50,1000) , dbclNm = "dbcl", ...){
  labelsDBCL = (dbcl[-1] + dbcl[-length(dbcl)]) / 2
  res_dbcl = data.frame(labelsDBCL[cut(x[,dbNm],dbcl,labels=FALSE)])
  names(res_dbcl) = dbclNm
  res_df = data.frame(x,res_dbcl)
  return(res_df)
}


#'@export
#'@rdname compileTrees
dbclY = function(x,trID,dbclNm,dbclY,...){

  require("reshape2")
  x_in = x

  for(i in 1:length(dbclY)){

    #cross dbcl with response attributes
    mi = reshape2::melt(x_in[,c(trID,dbclNm,dbclY[i])],id.vars=c(trID,dbclNm) )
    fi = as.formula(paste("variable +",trID,"~",dbclNm))
    dfi = reshape2::dcast(mi, formula =  fi)[,-1]
    names(dfi)[-1] = paste(dbclY[i], paste(dbclNm,names(dfi)[-1],sep=""),sep="_")

    #merge back in
    x_in = merge(x_in, dfi, by = trID)
  }
  return(x_in)
}


#'@export
#'@rdname compileTrees
sppY = function(x,trID,sppY,sppNm,...){

  require("reshape2")
  x_in = x

  for(i in 1:length(sppY)){

    #cross dbcl with response attributes
    mi = reshape2::melt(x_in[,c(trID,sppNm,sppY[i])],id.vars=c(trID,sppNm) )
    fi = as.formula(paste("variable +",trID,"~",sppNm))
    dfi = reshape2::dcast(mi, formula =  fi)[,-1]
    names(dfi)[-1] = paste(sppY[i], paste(sppNm,names(dfi)[-1],sep="_"),sep="_")

    #merge back in
    x_in = merge(x_in, dfi,  by = trID)
  }
  return(x_in)
}

#'@export
#'@rdname compileTrees
dbclSppY = function(x,trID,sppY,dbclNm,sppNm,...){

  require("reshape2")
  x_in = x

  for(i in 1:length(sppY)){

    #cross dbcl with response attributes
    mi = reshape2::melt(x_in[,c(trID,sppNm,dbclNm,sppY[i])],id.vars=c(trID,sppNm,dbclNm) )

    #append spp and dbcl to improve readability of final columns
    mi[,sppNm] = paste(sppY[i],sppNm,mi[,sppNm],sep="_")
    mi[,dbclNm] = paste(dbclNm,mi[,dbclNm],sep="_")

    #merge data
    fi = as.formula(paste("variable +",trID,"~",sppNm,"+",dbclNm))
    dfi = reshape2::dcast(mi, formula =  fi)[,-1]

    #merge back in
    x_in = merge(x_in, dfi,  by = trID)
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
# }

