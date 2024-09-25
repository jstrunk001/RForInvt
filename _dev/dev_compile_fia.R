#'@title
#'  compile FIA tree lists with RForInvt
#'
#'@description
#'  Automatically parse FIADB or NIMS tables and compute a suite of generic metrics.
#'  The user should supply their tabulation scale - subplot, plot, subplot/condition, plot/condition
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
#'\cr
#'Revision History
#' \tabular{ll}{
#'1.0 \tab date and revisions.. \cr
#'1.1 \tab date and revisions.. \cr
#'}
#'
#'@author
#'
#'Some Body <some.body@@somewhere.com>
#'
#'@param fiadb_tree   ?
#'@param fiadb_plot  ?
#'@param fiadb_subplot  ?
#'@param fiadb_plot_cond  ?
#'@param fiadb_subplot_cond  ?
#'
#'@param nims_tree  ?
#'@param nims_plot  ?
#'@param nims_subplot  ?
#'@param nims_plot_cond  ?
#'@param nims_subplot_cond  ?
#'
#'@param compile_by names of attributes to compile by - if CONDID is provided, then function autocollates the CONDID to unique CONDIDs across plots or subplots ... ?

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

#the code below with a single comment symbol is not part of the official roxygen code
#
#Desired upgrades to this function (keep track of planned updates):
#1.
#2.
#3.
#4.
#

#Help to set up param documentation above
# 1. Select function parameters, in this example case just "x,y,...", arguments should be on separate line
# 2. select the code starting with writeClipboard(...) below without comment (hash symbol) and run
# 3. paste parameters into @param section above
#
# writeClipboard(paste(gsub("^[[:space:]]*[,]*","#'@param ",gsub("=.*"," ?",readClipboard())),collapse="\n"))


#rename function and add guts
compile_fia = function(

                    fiadb_tree = NA
                    ,fiadb_plot = NA
                    ,fiadb_subplot = NA
                    ,fiadb_plot_cond = NA
                    ,fiadb_subplot_cond = NA

                    ,nims_tree = NA
                    ,nims_plot = NA
                    ,nims_subplot = NA
                    ,nims_plot_cond = NA
                    ,nims_subplot_cond = NA

                    ,do_fuels = F

                    #,compile_by = c("plot","subplot","plot_condition","subplot_condition")
                    ,compile_by = list(subplot=c("SUBP","INVYR"),plot=c("PLOT","INVYR"),subplot_condition = c("SUBP","CONDID"),plot_condition = c("PLOT","CONDID"))[[1]]

                    ,plot_filter = c(NA, "select * from dfPlot where YEAR = 2018 and STATE = 'WA' and CONDITION = 1")
                    ,tree_filter = c(NA, "select * from dfTree where dbh > 2 ")


                    ){

    #check for nims or fiadb tree table
    do_fiadb = "data.frame" %in% class(fiadb_tree)
    do_nims = "data.frame" %in% class(nims_tree)
    if(!do_fiadb & ! do_nims) stop("must provide a tree list to either fiadb_tree or nims_tree")
    if(do_fiadb & do_nims) stop("must provide only one tree list")

    #clean up condid to make unique condids
    if("CONDID" %in% compile_by){

    }

    if("plot" %in% compile_by){



    }

  }



 create_unq_cond=function(){


  }




