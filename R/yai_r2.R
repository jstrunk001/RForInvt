#'@title
#'  Compute R2 values from yaImpute::yai() object or data frame returned by yaImpute::impute(...)
#'
#'@description
#'  Compute R2 values from yaImpute::yai() object or data frame returned by yaImpute::impute(...)
#'
#'@details
#'
#' This function is a repurposing of the yaImpute::rmsd() function to compute R.squared instead.
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
#'1.0 \tab 5/27/2020 Created \cr
#'}
#'
#'@author
#'
#'Jacob Strunk <strunky@@gmail.com>
#'
#'@param x yaImpute::yai() object or data frame returned by yaImpute::impute(...)
#'@param vars subset of variables
#'@param ... additional arguments to impute() if yaImpute::yai() object is provided
#'
#'@return
#'  Data.frame with 1 column of R2 values
#'
#'@examples
#'
#'  #fit model using one set of responses
#'  yaimod1 = yaImpute::yai( x = dat6[,c(vEnvironment1)	], y = dat6[,vBA1] , method = "gnn" )
#'
#'  #impute a second set
#'  imp1 = impute(yaimod3,dat6[,vResp3])
#'
#'  #compute R2
#'  yai_r2(imp1)
#'
#'
#'@export
#
#'@seealso \code{\link{yaImpute::yai}}\cr \code{\link{yaImpute::impute}}\cr

#Desired upgrades to this function:
#
#

yai_r2 = function (x, vars = NULL, ...){
  if (missing(x))
    stop("x required.")
  if (class(x)[1] == "yai")
    x = impute.yai(x, vars = vars, observed = TRUE,...)
  if (is.null(x))
    stop("no imputations found using this x")
  nuke = unlist(lapply(x, function(x) all(is.na(x))))
  nuke = nuke[nuke]
  if (length(nuke) > 0)
    x = x[, -match(names(nuke), names(x)),
                    drop = FALSE]
  x = na.omit(x)
  if (is.null(vars))
    vars = names(x)
  vi = paste(unique(strsplit(vars, ".o", fixed = TRUE)))
  vi = intersect(vi, names(x))
  notFound = setdiff(vars, names(x))
  if (length(notFound) > 0)
    warning("variables not found or had missing values: ",
            paste(notFound, collapse = ", "))
  if (length(vi) == 0)
    stop("nothing to compute")
  vo = paste(vi, "o", sep = ".")
  notFound = setdiff(vo, names(x))
  if (length(notFound) > 0)
    warning("variables not found or had missing values: ",
            paste(notFound, collapse = ", "))
  vo = intersect(vo, names(x))
  both = intersect(paste(unique(strsplit(vo, ".o", fixed = TRUE))),
                   vi)
  if (length(both) == 0)
    stop("nothing to compute")
  vo = paste(both, "o", sep = ".")
  R2_in = data.frame(rep(NA, length(vo)), row.names = both)
  names(R2_in) = "R2_in"
  for (i in 1:length(both)) {
    if (!is.factor(x[, both[i]])) {

      R2_in[i, 1] = 1- var(x[, both[i]])  / var(x[,vo[i]])

    }
  }
  R2_in
}
