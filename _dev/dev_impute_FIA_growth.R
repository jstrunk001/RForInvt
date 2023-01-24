#'@title
#'  Impute growth from FIA trees
#'
#'@description
#'  <Delete and Replace>
#'
#'@details
#'
#'
#'  This program is free software but it is provided WITHOUT WARRANTY
#'  and with ABSOLUTELY NO GUARANTEE of fitness or functionality for any purpose;
#'  you can redistribute it and/or modify it under the terms of the GNU
#'  General Public License as published by the Free Software Foundation;
#'  either version 2 of the License, or (at your option) any later version.
#'
#'
#'  Impute growth from FIA trees to a dataset. Use auxiliary information
#'  collected at plot level to guide imputed growth. Trees are not actively killed,
#'  mortality is retained at the plot level as an increasing proportion.
#'  E.g., TPA_live_2030 = TPA_2030 - TPA_2030*proportion_dead_2030
#'      , TPA_dead_2030 = TPA_2030*proportion_dead_2030
#'
#'  This means that the tree list for a plot can only grow, but the proportion of
#'  dead trees on a plot will also continue to increase. Dead trees are accumulated
#'  from FIA mortality trees (natural causes).
#'
#'
#'
#'
#'\cr
#'Revision History
#' \tabular{ll}{
#'1.0 \tab 1/22/2023 Started \cr
#'1.1 \tab date and revisions.. \cr
#'}
#'
#'@author
#'
#'Jacob Strunk <strunky@@gmail.com>
#'
#'@param
#'@param
#'@param
#'@param
#'@param
#'@param
#'@param
#'@param
#'@param
#'@param
#'@param
#'@param
#'@param
#'@param
#'@param
#'@param
#'@param
#'@param
#'
#'@return
#'  <Delete and Replace>
#'
#'@examples
#'  <Delete and Replace>
#'
#'@import yaImpute
#'
#'@export
#
#'@seealso \code{\link{another_function}}\cr \code{\link{yet_another_function}}\cr

#Desired upgrades to this function:
#
#

# x = function(x){}

#copy function arguments and use this code to format arguments
# writeClipboard(paste(gsub("^[[:space:]]*[,]*","#'@param ",gsub("=.*"," ?",readClipboard())),collapse="\n"))
#
#various helpers:
# bs<- function(){path <- shQuote(gsub("\\", "\\\\", readClipboard(), fixed = TRUE)); writeClipboard(path); return(path)}
# fs<- function(){path <- shQuote(gsub("\\", "/", readClipboard(), fixed = TRUE)); writeClipboard(path); return(path)}
# nmsVec=function(x){x=paste("c('",paste(names(x),collapse="','"),"')",sep="");writeClipboard(x);return(x)}

impute_fia_growth=function(
  data_tl
  ,data_pl
  ,data_st
  ,colnms = c(
       stID = "plid"
      ,plID = "plid"
      ,trID = "trid"
      ,sppNm = "spp"
      ,dbNm = "db"
      ,htNm = "ht"
      ,acresNm = "acres"
      ,nTreesNm = NA
      ,siNm = NA
      ,lonNm = NA
      ,latNm = NA
      ,elvNm = NA
      ,precipNm = NA
      ,growDaysNm = NA
      ,pl.baNm = NA
      ,pl.qmdNm = NA
      ,pl.tpaNm = NA
    )
  ,knn_method = "randomForest"
  ,k = 5
  ,knn_metrics_tree = c("ht","dbh","age")
  ,knn_metrics_plot = c("ba.pl","qmd.pl","tpa.pl","lorht.pl","elv.pl","lon.pl","lat.pl")
  ){

  #iterate tht
  #impute nearest tre


}


#example only
#R can be used for any level of complexity of management on tree lists
thin = function(){



}


plant = function(){



}

