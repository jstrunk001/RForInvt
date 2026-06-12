#'@title
#'  update on the pander functions to do what I want to lm and simex objects including lists
#'
#'@description
#'  simple output, and accepts a list of lm objects
#'
#'@details
#'  Builds a compact one-row summary (response, n, residual std error, R^2, adjusted R^2,
#'  inverse design effect, and predictor list) for a fitted model and either returns it as a
#'  data.frame or prints it via \code{pander}. Methods are provided for \code{lm}, \code{simex},
#'  and a \code{list} of models (the list method row-binds the per-model summaries).
#'
#'  Method arguments: \code{df0} is an optional validation data.frame - when
#'  supplied the statistics are computed out-of-sample (\code{pander_mod.simex}
#'  requires it); \code{return_df} (T/F) controls whether the one-row summary is
#'  returned as a data.frame or printed via \code{pander}.
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
#'@param x input model or list of models (lm objects)
#'@param ... additional arguments passed to methods (e.g. \code{df0}, \code{return_df}; see Details)
#'
#'@return
#'  data.frame or pander table
#'
#'@examples
#'  fit1 = lm(mpg ~ wt + hp, data = mtcars)
#'  fit2 = lm(mpg ~ disp, data = mtcars)
#'
#'  # one-row summary as a data.frame
#'  pander_mod(fit1, return_df = TRUE)
#'
#'  # summarize a list of models (row-bound)
#'  pander_mod(list(a = fit1, b = fit2), return_df = TRUE)
#'
#'@import plyr pander knitr
#'
#'@export
#
#'@seealso \code{\link[pander]{pander}}\cr

pander_mod=function(x,...){

  UseMethod('pander_mod', x)

}

#'@export
pander_mod.list=function(x
                        ,df0=NA
                        ,return_df=F
                        ,...
){

  df_in=plyr::rbind.fill(lapply(x,pander_mod,df0=df0,return_df=T))
  if(return_df) return(df_in)
  if(!return_df) pander::pander(df_in)

}
#'@export
pander_mod.lm=function(x
                       ,df0=NA
                       ,return_df=F
                       ,...
){

  y=summary(x)

  if(!inherits(df0,"data.frame")){

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

  if(inherits(df0,"data.frame")){

    resp_nm=row.names(attr(y$terms,"factors"))[1]
    pd_in=predict(x,newdata=df0)
    y0=df0[,resp_nm]
    er_in=pd_in - y0
    n=length(y$residuals)
    n0=sum(!is.na(er_in))
    n_param=y$df[1]
    degfr=n0-n_param
    sigma_in= sqrt(sum(er_in^2,na.rm=T)/degfr)
    r2=1-var(er_in,na.rm=T)/var(y0,na.rm=T)
    r2_adj=1-(1-r2)*(n0-1)/(n0-n_param)
    definv=round(var(y0,na.rm=T)/sigma_in^2,2)

    df_in=data.frame(
      Resp.               = resp_nm,
      'n'                 = n,
      n0                  = n0,
      'Resid. Std. Error' = sigma_in,
      '$R^2$'             = round(r2,3),
      'Adjusted $R^2$'    = round(r2_adj,3),
      '$DE^{-1}$'         = definv,
      Predictors          = paste(as.character(attr(y$terms,"term.labels")),collapse=" + "),
      check.names = FALSE)
  }

  if(return_df) return(df_in)
  if(!return_df) pander::pander(df_in)
}
#'@export
pander_mod.simex=function(x
                         ,df0=NA
                         ,return_df=F
                         ,...
){

  if(!inherits(df0,"data.frame"))
    stop("pander_mod.simex requires an out-of-sample validation data.frame 'df0'")

  y=summary(x$model)

  resp_nm=row.names(attr(y$terms,"factors"))[1]
  pd_in=predict(x,newdata=df0)
  y0=df0[,resp_nm]
  er_in=pd_in - y0
  n=length(y$residuals)
  n0=sum(!is.na(er_in))
  n_param=y$df[1]
  degfr=n0-n_param
  sigma_in= sqrt(sum(er_in^2,na.rm=T)/degfr)
  r2=1-var(er_in,na.rm=T)/var(y0,na.rm=T)
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

  if(return_df) return(df_in)
  if(!return_df) pander::pander(df_in)
}
