#'@title
#'  function to clean FIA data for working with impute_fia_growth()
#'
#'@description
#'  There are too many tables / fields in the FIA data
#'  this functions subsets / compiles data to work with impute_fia_growth()
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
#'  subset plots with growth? or with age?
#'
#'\cr
#'Revision History
#' \tabular{ll}{
#'1.0 \tab date and revisions.. \cr
#'1.1 \tab date and revisions.. \cr
#'}
#'
#'@author
#'
#'Jacob Strunk <someone@@somewhere.com>
#'
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
#'@import some_package some_package2
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

#subset tables and fields needed for impute_fia_growth()
subset_tables = function(
  tables_keep = c()
  fields_keep =
    list(
      tree = c()
      ,subplot = c()

    )

  ){


}

#'@export
#get / clean paired growth data
compile_tables = function(){}


