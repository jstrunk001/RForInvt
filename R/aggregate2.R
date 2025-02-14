#'@title
#'  wrapper for stats::aggregate that forces aggregate to return a nice data.frame
#'
#'@description
#'
#'  wrapper for stats::aggregate that forces aggregate to return a nice data.frame. When
#'  the user passes FUN = c(mean, sd) it causes aggregate(y~id, ..., FUN = c(mean, sd) )
#'  to create a nested matrix inside of the y column. This is annoying to work
#'  with. This wrapper explicitly handles this bizarre behavior and offers an
#'  option to modify the concatenation between function name and y
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
#'1.0 \tab 10/1/2024 implemented \cr
#'1.1 \tab date and revisions.. \cr
#'}
#'
#'@author
#'
#'Jacob Strunk <some.body@@somewhere.com>
#'
#'@param x an R object. For the formula method a formula, such as y ~ x or cbind(y1, y2) ~ x1 + x2, where the y variables are numeric data to be split into groups according to the grouping x variables (usually factors).
#'@param ... other arguments to aggregate
#'@param nm_sep a character used to separate y variables from functions
#'
#'@return
#'  typical aggregate output - UNLESS "FUN" argument returns multiple values
#'  if "FUN" argument returns multiple values, then the y column(s) are converted to
#'  a data.frame
#'
#'@examples
#'
#' #test datasets
#' test1 = data.frame(x=1:50,y=1:10)
#' test2 = data.frame(x=1:50,y=1:10, z=50:1)
#'
#' #traditional aggregate with strange x column that is a nested matrix
#' ag1 = aggregate(x~y, test1, FUN = function(x,...){ c(sd=sd(x,...), mean=mean(x,...)) } )
#' str(ag1)
#'
#' #upgraded aggregate function with simple data.frame output
#' ag2 = aggregate2(x~y, test1, FUN = function(x,...){ c(sd=sd(x,...), mean=mean(x,...)) } , nm_sep="_" )
#' str(ag2)
#'
#' #upgraded aggregate function with simple data.frame output - multiple response summations
#' ag3 = aggregate2(cbind(x,z)~y, test2, FUN = function(x,...){ c(sd=sd(x,...), mean=mean(x,...), n=length(x)) } , nm_sep="." )
#' str(ag3)
#'
#'
#'
#'
#'@export
#
#'@seealso \code{\link{aggregate}}\cr \code{\link{data.frame}}\cr \code{\link{paste}}\cr

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
aggregate2 = function(
                    x,
                    ...
                    ,nm_sep="_"
                    ){

  #tradition aggregate
  ag_in = aggregate(x,...)

  #identify problematic columns
  cl_in = sapply(ag_in, class)
  mat_in = sapply(cl_in,function(x) "matrix" %in% x)

  #handle problematic columns
  if( sum(mat_in) > 0 ){

    mat_nms = names(ag_in)[which(mat_in)]
    for(i in 1:length(mat_nms)){
      mat_nm_i = mat_nms[i]
      #grab matrix columns and add new names
      df_mat_i = as.data.frame(ag_in[,mat_nm_i])
      names(df_mat_i) = paste(mat_nm_i,names(df_mat_i),sep=nm_sep)

      #modify original agggregation, remove aggregates
      ag_in[,mat_nm_i] = NULL

      #merge on columns with modified names
      ag_in = data.frame(ag_in, df_mat_i)
    }
  }

  #return
  ag_in

}

if(F){

  #test datasets
  test1 = data.frame(x=1:50,y=1:10)
  test2 = data.frame(x=1:50,y=1:10, z=50:1)

  #traditional aggregate with strange x column that is a nested matrix
  ag1 = aggregate(x~y, test1, FUN = function(x,...){ c(sd=sd(x,...), mean=mean(x,...)) } )
  str(ag1)

  #upgraded aggregate function with simple data.frame output
  ag2 = aggregate2(x~y, test1, FUN = function(x,...){ c(sd=sd(x,...), mean=mean(x,...)) } , nm_sep="_" )
  str(ag2)

  #upgraded aggregate function with simple data.frame output - multiple response summations
  ag3 = aggregate2(cbind(x,z)~y, test2, FUN = function(x,...){ c(sd=sd(x,...), mean=mean(x,...), n=length(x)) } , nm_sep="." )
  str(ag3)

}

