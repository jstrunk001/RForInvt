#'@title
#'  update on the pander functions to do what I want to lm and simex objects including lists
#'
#'@description
#'  simple output, and accepts a list of lm objects
#'
#'@details
#'  <Delete and Replace>
#'
#'\cr
#'Revision History
#' \tabular{ll}{
#'1.0 \tab 2017 Aug 22 \cr
#'}
#'
#'@author
#'
#'Jacob Strunk <Jstrunk@@fs.fed.us>
#'
#'@param x inpute model or list of models (lm objects)
#'@param df0 optional dataframe for validation (out of box validation)
#'@param return_df T/F should the function return a data.frame

#'
#'@return
#'  data.frame or pander table
#'
#'@examples
#'  <Delete and Replace>
#'
#'@import plyr pander knitr
#'
#'@export
#
#'@seealso \code{\link{pander}}\cr

pandermod=function(x,...){

  UseMethod('pandermod', x)

}

#'@export
pandermod.list=function(x
                        ,df0=NA
                        ,return_df=F
){
  require(plyr)
  require(pander)
  require(knitr)

  df_in=rbind.fill(lapply(x,pandermod,df0=df0,return_df=T))
  if(return_df) return(df_in)
  if(!return_df) pander(df_in)

}
#'@export
pandermod.lm=function(x
                       ,df0=NA
                       ,return_df=F
){

  require(plyr)
  require(pander)
  require(knitr)

  y=summary(x)

  if(class(df0)!="data.frame"){

    df_in=data.frame(
      Resp.            = row.names(attr(y$terms,"factors"))[1],
      'n'        = length(y$residuals),
      'Resid. Std. Error' = y$sigma,
      '$R^2$'               = y$r.squared,
      'Adjusted $R^2$'      = y$adj.r.squared,
      '$DE^{-1}$'         = var(x$model[,1])/y$sigma^2,
      Predictors           = paste(as.character(attr(y$terms,"term.labels")),collapse=" + "),
      check.names = FALSE)
  }

  if(class(df0)=="data.frame"){

    resp_nm=row.names(attr(y$terms,"factors"))[1]
    pd_in=predict(x,newdata=df0)
    y0=df0[,resp_nm]
    er_in=pd_in - y0
    n=length(y$residuals)
    n0=length(er_in)
    n_param=y$df[1]
    degfr=n0-n_param
    sigma_in= sqrt(sum(er_in^2,na.rm=T)/degfr)
    r2=1-var(er_in,na.rm=T)/var(y0,na.rm=T)
    #r2=1-var(er_in^2,na.rm=T)/var(x$model[,1]^2,na.rm=T)
    r2_adj=1-(1-r2)*(n0-1)/(n0-n_param)
    definv=round(var(y0,na.rm=T)/sigma_in^2,2)

    df_in=data.frame(
      Resp.               = resp_nm,
      'n'                 = n,
      n0                  = n0,
      'Resis. Std. Error' = sigma_in,
      '$R^2$'             = round(r2,3),
      'Adjusted $R^2$'    = round(r2_adj,3),
      '$DE^{-1}$'         = definv,
      Predictors          = paste(as.character(attr(y$terms,"term.labels")),collapse=" "),
      check.names = FALSE)
  }

  if(return_df) return(df_in)
  if(!return_df) pander(df_in)
}
#'@export
pandermod.simex=function(x
                         ,df0=NA
                         ,return_df=F
){

  y=summary(x$model)

  resp_nm=row.names(attr(y$terms,"factors"))[1]
  pd_in=predict(x,newdata=df0)
  y0=df0[,resp_nm]
  er_in=pd_in - y0
  n=length(y$residuals)
  n0=length(er_in)
  n_param=y$df[1]
  degfr=n0-n_param
  sigma_in= sqrt(sum(er_in^2,na.rm=T)/degfr)
  r2=1-var(er_in,na.rm=T)/var(y0,na.rm=T)
  #r2=1-var(er_in^2,na.rm=T)/var(x$model[,1]^2,na.rm=T)
  r2_adj=1-(1-r2)*(n0-1)/(n0-n_param)
  definv=round(var(y0,na.rm=T)/sigma_in^2,2)

  df_in=data.frame(
    Resp.               = resp_nm,
    'n'                 = n,
    n0                  = n0,
    'Resis. Std. Error' = sigma_in,
    '$R^2$'             = round(r2,3),
    'Adjusted $R^2$'    = round(r2_adj,3),
    '$DE^{-1}$'         = definv,
    Predictors          = paste(as.character(attr(y$terms,"term.labels")),collapse=" "),
    check.names = FALSE)


}

# pandermod_old=function(x
#                       ,df0=NA
#                       ,return_df=F
# ){
#   require(plyr)
#   require(pander)
#   require(knitr)
#
#   df_in=NULL
#
#   if(class(x)!="lm")if(class(x)=="list")
#     df_in=rbind.fill(lapply(x,pandermod,df0=df0,return_df=T))
#
#   if(class(x)=="lm"){
#     y=summary(x)
#
#     if(class(df0)!="data.frame"){
#
#       df_in=data.frame(
#         Resp.            = row.names(attr(y$terms,"factors"))[1],
#         'n'        = length(y$residuals),
#         'Resid. Std. Error' = y$sigma,
#         '$R^2$'               = y$r.squared,
#         'Adjusted $R^2$'      = y$adj.r.squared,
#         '$DE^{-1}$'         = var(x$model[,1])/y$sigma^2,
#         Predictors           = paste(as.character(attr(y$terms,"term.labels")),collapse=" + "),
#         check.names = FALSE)
#     }
#
#     if(class(df0)=="data.frame"){
#
#       resp_nm=row.names(attr(y$terms,"factors"))[1]
#       pd_in=predict(x,newdata=df0)
#       y0=df0[,resp_nm]
#       er_in=pd_in - y0
#       n=length(y$residuals)
#       n0=length(er_in)
#       n_param=y$df[1]
#       degfr=n0-n_param
#       sigma_in= sqrt(sum(er_in^2,na.rm=T)/degfr)
#       r2=1-var(er_in,na.rm=T)/var(y0,na.rm=T)
#       #r2=1-var(er_in^2,na.rm=T)/var(x$model[,1]^2,na.rm=T)
#       r2_adj=1-(1-r2)*(n0-1)/(n0-n_param)
#       definv=round(var(y0,na.rm=T)/sigma_in^2,2)
#
#       df_in=data.frame(
#         Resp.               = resp_nm,
#         'n'                 = n,
#         n0                  = n0,
#         'Resis. Std. Error' = sigma_in,
#         '$R^2$'             = round(r2,3),
#         'Adjusted $R^2$'    = round(r2_adj,3),
#         '$DE^{-1}$'         = definv,
#         Predictors          = paste(as.character(attr(y$terms,"term.labels")),collapse=" "),
#         check.names = FALSE)
#
#     }
#
#   }
#   if(return_df) return(df_in)
#   if(!return_df) pander(df_in)
#
#   UseMethod('pander', x)
#
# }
