
#'@name OLS_modeling
#'@title
#'functions related to modeling
#'
#'@description
#'
#'  A family of helpers for ordinary-least-squares (OLS) modeling: best-subsets
#'  variable selection (via \code{leaps::regsubsets}), \code{.632} bootstrap error
#'  estimation, fitting/predicting many response models at once, and summarising
#'  cross-validated fit statistics.
#'
#'@details
#'
#'  The functions are designed to be chained together when building many models
#'  (one per response) from a shared set of candidate predictors:
#'
#'  \itemize{
#'    \item \code{reg_model} - extract a single ranked \code{lm} from a \code{regsubsets} object.
#'    \item \code{reg_multi} - run \code{regsubsets} for several responses.
#'    \item \code{lm_multi}  - fit a plain \code{lm} for several responses.
#'    \item \code{mod_multi} - turn a \code{reg_multi} result into fitted \code{lm} objects.
#'    \item \code{lm_boot}   - \code{.632} bootstrap RMSE / R2 for one model.
#'    \item \code{multi_bs}  - \code{lm_boot} applied across a list of models.
#'    \item \code{many_boots}- bootstrap properties across a range of sample sizes.
#'    \item \code{pred_multi}- predict a list of models onto new (e.g. wall-to-wall) data.
#'    \item \code{lm_summary}- apparent + leave-one-out cross-validated fit statistics.
#'  }
#'
#'\cr
#'
#'Revision History
#' \tabular{ll}{
#'1.0 \tab 2014 Mar 06 Ported from prior work \cr
#'}
#'
#'
#'@author
#'
#'Jacob Strunk <someone@@somewhere.com>
#'
#'
#'@param dat_yx data.frame whose first column is the response and the remaining columns are candidate predictors
#'@param reg_obj an object returned by \code{leaps::regsubsets} for the response
#'@param rank which ranked model to extract (1 = best by \code{rank_by})
#'@param rank_by criterion used to rank models: one of "bic","rsq","rss","adjr2","cp"
#'@param debug if TRUE, drop into \code{browser()}
#'@param ... additional arguments passed on to the underlying function (\code{lm}, \code{regsubsets}, or \code{lm_boot})
#'\cr\cr
#' \bold{lm_boot() parameters:}
#'@param model an \code{lm} object, or a formula to be fit with \code{data}
#'@param y response vector (defaults to the model response)
#'@param data data.frame used to fit the model (defaults to the model frame)
#'@param n_boot number of bootstrap replicates for the \code{.632} estimator
#'\cr\cr
#' \bold{many_boots() parameters:}
#'@param n_range vector of sample sizes to evaluate
#'@param r_boots number of bootstrap repetitions per sample size
#'\cr\cr
#' \bold{reg_multi() / lm_multi() parameters:}
#'@param y_vars character vector of response column names
#'@param form_y model-form template using "y" as the response placeholder (e.g. "y~." or "y~.*.")
#'@param n_v_max maximum number of predictors (\code{regsubsets} nvmax)
#'@param n_best number of best models retained per subset size (\code{regsubsets} nbest)
#'@param really_big passed to \code{regsubsets} \code{really.big}
#'@param n_clus number of cores to use (>1 runs in parallel)
#'@param verbose if TRUE, print the response being fit
#'\cr\cr
#' \bold{mod_multi() parameters:}
#'@param mods_list a list of models: a \code{reg_multi} result (for \code{mod_multi}) or fitted \code{lm} objects (for \code{pred_multi}/\code{multi_bs}/\code{lm_summary})
#'\cr\cr
#' \bold{pred_multi() parameters:}
#'@param dat0 optional original response values used to clamp implausible predictions
#'@param id_col name of the unique observation-id column in \code{data}
#'@param clus optional pre-built cluster object (otherwise one is created)
#'@param out_dir optional directory to write predictions to
#'@param se_fit if TRUE, also compute standard errors of prediction
#'@param return if TRUE, return the predictions
#'@param fix_outliers if TRUE, clamp implausible predictions with \code{outlier_fun}
#'@param outlier_fun function used to clamp out-of-range predictions
#'\cr\cr
#' \bold{multi_bs() parameters:}
#'@param lm_boot the \code{lm_boot} function (passed explicitly so parallel workers can find it)
#'\cr\cr
#' \bold{lm_summary() parameters:}
#'@param resids if TRUE, also return per-observation residuals
#'
#'
#'@return
#'
#'  \code{reg_model} / \code{mod_multi} return fitted \code{lm} object(s);
#'  \code{reg_multi} returns a named list of \code{regsubsets} objects;
#'  \code{lm_multi} returns a named list of \code{lm} objects;
#'  \code{lm_boot} returns a list of \code{.632} error statistics;
#'  \code{multi_bs}, \code{many_boots} and \code{lm_summary} return data.frames of
#'  bootstrap / cross-validated fit statistics; \code{pred_multi} returns a
#'  data.frame of predictions keyed by \code{id_col}.
#'
#'@examples
#'
#'  #simulated data: two responses, three predictors
#'  set.seed(1)
#'  n <- 60
#'  dat <- data.frame(
#'    y1 = rnorm(n), y2 = rnorm(n),
#'    x1 = rnorm(n), x2 = rnorm(n), x3 = rnorm(n)
#'  )
#'
#'  #fit a linear model per response (named list, keyed by response)
#'  mods <- lm_multi(y_vars = c("y1", "y2"), data = dat, form_y = "y~.", verbose = FALSE)
#'
#'  #apparent + leave-one-out cross-validated fit statistics
#'  lm_summary(mods, data = dat)
#'
#'  \donttest{
#'  #.632+ bootstrap error for a single model
#'  lm_boot(mods[["y1"]], n_boot = 20)
#'
#'  #bootstrap a whole list of models
#'  multi_bs(mods, n_boot = 20, n_clus = 1, lm_boot = lm_boot)
#'  }
#'
#'@seealso \code{\link[leaps]{regsubsets}}\cr
#'
#'@import leaps bootstrap parallel
#'
#Desired upgrades to this function:
#
#

######## BEGIN HEADER
##
##  Author(s): Jacob Strunk
##  Date:  3/6/2014 1:03:43 PM
##  Version and changes :
##
##     0.1 Code Originating from Jacob's previous work - ported to our ruleset
##  Brief Description:  Series of functions to work with models
##  File Type: series of inter-related functions
##  Desired updates to function:
##  Dependencies: leaps package - regSubsets function is a common requirement
##
####### END HEADER

#main function body

#'@export
#'@rdname OLS_modeling
  reg_model=function(
                  dat_yx
                 ,reg_obj
                 ,rank=1
                 ,rank_by=c("bic","rsq","rss","adjr2","cp")
                 ,debug=FALSE
                 ,...             #additional parameters for lm
                 ){
    if(debug) browser()
    #rank-1 should be the BEST model. Only rsq and adjr2 are "larger is better";
    #bic, rss and cp are error/penalty measures where smaller is better.
    desc=F
    if(rank_by[1] %in% c("rsq","adjr2") ) desc=T
    bic_id=sort(summary(reg_obj)[[rank_by[1]]],decreasing=desc, index.return=TRUE)$ix
    if(length(bic_id)==0){
      print("Regobj has no models...")
      return()
    }

    x_names=names(coef(reg_obj,bic_id[rank]))[-1]

    if(!is.null(reg_obj[["reorder"]])) {

      #leaps fails if "reorder" is necessary
      x_id = reg_obj[["xnames"]][reg_obj[["reorder"]]] %in% x_names
      x_names = reg_obj[["xnames"]][x_id]
    }

    model=paste(names(dat_yx)[1],"~",paste(x_names,collapse=" + "),collapse="")
    l_min=lm(as.formula(model),data=dat_yx,x=T)

    l_min$call = call("lm",formula=as.formula(model),data=getCall(reg_obj)$data,x=T)
    l_min$summary = summary(lm(as.formula(model),data=dat_yx))
    l_min
}

      # attr(reg_model,"info") = list(
      #   arg_def = data.frame(
      #     arg=c(
      #             "dat_yx"
      #            ,"reg_obj"
      #            ,"rank"
      #            ,"rank_by"
      #            ,"debug"
      #           )
      #     ,def=c(
      #           "data frame in which y is the first variable and the remainder are predictors"
      #           ,"object returned by regSubsets"
      #           ,"rank of model desired"
      #           ,"which should the model be sorted by c(\"bic\",\"rsq\",\"rss\",\"adjr2\",\"cp\")"
      #           ,"run in debug modes?"
      #           )
      #     ),
      #   description = "this function extracts a best model for the object returned by regSubsets in the 'leaps' package "
      # )

#example code

  #knidat3=knidat[,c(vars[["contresp"]][1],vars[["clm_762"]])]
  #rg1=regsubsets(biolivekg~.*., data=knidat3[,1:5], nvmax=4,nbest=1, really.big=TRUE)
  #regmodel(knidat3,rg1)

#'@export
#'@rdname OLS_modeling
   lm_boot=function(
                    model
                    ,y = NA
                    ,data = NA
                    ,n_boot=50
                    ){

     if(!inherits(model, "lm")){
       model=lm(model,data=data)
      }

      if(!is.data.frame(data)){
        data=model[["model"]]
      }

      if(length(y) == 1 && is.na(y)){
       y=model$model[,1]
      }


      #functions required for crossvalidation
      theta_fit=function(x,y,model,...)update(model,data=data.frame(x,row.names=NULL))
      theta_predict = function(fit,x,...) predict(fit, newdata=data.frame(x,row.names=NULL) )
      sq_err <- function(y,y_hat)   (y-y_hat)^2

      #perform .632 crossvalidation
      bp1=bootstrap::bootpred(y=y,nboot=n_boot,x=data,err.meas=sq_err,theta.predict=theta_predict,theta.fit=theta_fit,model=model)
      bp1[1:3]=lapply((bp1[1:3]),sqrt)

      names(bp1)=c("app_err","optim","err_632","call")

      #set up output
      bp1[["Sy"]]=sd(y)
      bp1[["meany"]]=mean(y)
      bp1[["err_632_pct"]]=bp1[["err_632"]]/bp1[["meany"]]*100
      bp1[["err_632_pct_sdy"]]=bp1[["err_632"]]/bp1[["Sy"]]*100
      bp1[["err_632_rsq"]]=max(0,1-(bp1[["err_632"]]/bp1[["Sy"]])^2)
      bp1[["n"]]=sum(!is.na(y))
      bp1[["n_gt_0"]]=sum(y > 0, na.rm = TRUE)
      bp1[["predictors"]]=paste(attr(model[["terms"]],"term.labels"),collapse=" ")


      bp1
    }
#
#       attr(lm_boot,"info") = list(
#         arg_def = data.frame(
#           arg=c(
#                 "model"
#                 ,"y"
#                 ,"data"
#                 ,"n_boot"
#                 )
#           ,def=c(
#                 "ols model returns by lm"
#                 ,"response vector"
#                 ,"all of the data"
#                 ,"number of desired bootstraps"
#                 )
#           ),
#         description = "bootstraps a linear regression model and computes relevant statistics "
#       )
#
#       #example code



#'@export
#'@rdname OLS_modeling
   #function to iterate over many sample sizes and look at their bootstrap properties
   many_boots=function(
                      model
                      ,n_range
                      ,r_boots=10
                      ,n_clus=1
                      ,lm_boot
                      ,...
                      ){

     #internal copy of data
      dat_in=model[["model"]]
     #generic index
      idx_all=1:nrow(dat_in)
     #size of subboot
      boots=rep(n_range,r_boots)

     #internal wrapper for lmBoot
       lm_boot_in=function(ni,model,idx_all,data,...){

        mod_k=update(model,data=data[sample(idx_all,ni,replace=FALSE),])
        data.frame(n=ni,err_632=lm_boot(mod_k,...)[["err_632"]])

       }

     #run bootstraps
       if(n_clus>1){

           clus_in=makeCluster(n_clus)
           bs_err_in=do.call(rbind,parLapply(clus_in,boots,lm_boot_in,model,idx_all,dat_in,...))
           stopCluster(cl=clus_in)

       }else{

           bs_err_in=do.call(rbind,lapply(boots,lm_boot_in,model,idx_all,dat_in,...))

       }

     #compute bs stats
       res_in=data.frame(do.call(rbind,lapply(split(bs_err_in[],bs_err_in[,"n"]),apply,2,median,na.rm=TRUE)))
       res_in[,"rsq_err_632"]=1-res_in[,"err_632"]^2/var(dat_in[,1],na.rm=TRUE)
       res_in[res_in[,"rsq_err_632"]<0,"rsq_err_632"]=0
       res_in
   }

      # attr(many_boots,"info") = list(
      #   arg_def = data.frame(
      #     arg=c(
      #           "model"
      #           ,"n_range"
      #           ,"r_boots"
      #           ,"n_clus"
      #           ,"lm_boot"
      #           ,"..."
      #           )
      #     ,def=c(
      #           "ols model returns by lm"
      #           ,"vector of sample sizes to examine"
      #           ,"number of bootstraps per sample size"
      #           ,"number of clusters to work on"
      #           ,"actual function'lm_boot'"
      #           ,"additional arguments passed to lm_boot()"
      #           )
      #     ),
      #   description = "iterate over many sample sizes and look at their bootstrap properties"
      # )

      #example code
      #
      #
      #
#'@export
#'@rdname OLS_modeling
   reg_multi=function(
                     y_vars
                     ,data
                     ,form_y=c("y~.","y~.*.")
                     ,n_v_max=3
                     ,n_best=1
                     ,really_big=FALSE
                     ,n_clus=1
                     ,...
                     ){
    form_y=form_y[1]
    x_vars=names(data)[!names(data)%in% y_vars ]

    c_fn=function(y,data,x_vars,form_y,n_v_max,n_best,really_big,...){

        print(paste("response is ",y))
        #substitute ONLY the leading response placeholder, not every literal "y"
        #(gsub("y", y, ...) would corrupt predictors/templates containing "y")
        rg_i=leaps::regsubsets(x=as.formula(sub("^\\s*y\\s*~", paste0(y, " ~"), form_y)),data=data[,c(y,x_vars)],nvmax=n_v_max,nbest=n_best,really.big=really_big,...)
        rg_i[["response"]]=y
        rg_i

       }

     #run bootstraps
       if(n_clus>1){

           clus_in=makeCluster(n_clus)

           rgs=parLapply(clus_in,y_vars,c_fn,data,x_vars,form_y,n_v_max=n_v_max,n_best=n_best,really_big=really_big,...)

           stopCluster(cl=clus_in)

       }else {

           rgs=lapply(y_vars,c_fn,data,x_vars,form_y,n_v_max=n_v_max,n_best=n_best,really_big=really_big,...)

       }

      names(rgs)=y_vars
      rgs
   }

#'@export
#'@rdname OLS_modeling
   lm_multi=function(
                     y_vars
                     ,data
                     ,form_y=c("y~.","y~.*.")
                     ,verbose=T
                     ,...
                     ){

    #get first model form
    form_y=form_y[1]

    #get x variables if needed
    x_vars=names(data)[!names(data)%in% y_vars ]

    #internal modeling function
    mod_fn=function(y,data,x_vars,form_y,n_v_max,n_best,verbose,...){

        if(verbose) print(paste("response is ",y))
        #substitute ONLY the leading response placeholder, not every literal "y"
        form_i = as.formula(sub("^\\s*y\\s*~", paste0(y, " ~"), form_y))
        lm_i=lm(form_i,data=data[,c(y,x_vars)],...)
        #embed the actual formula object in the stored call so that update()
        #(used by lm_summary / lm_boot) can re-evaluate the model in any frame -
        #otherwise the call references the local symbol 'form_i' and fails
        lm_i$call$formula = form_i
        lm_i[["response"]]=y
        lm_i

       }

     #fit and name all models
      lm_mods = lapply(y_vars,mod_fn,data,x_vars,form_y,verbose=verbose,...)
      names(lm_mods)=y_vars

     #return models
      return(lm_mods)
   }

   #accepts reg_multi or lm_multi object
#'@export
#'@rdname OLS_modeling
mod_multi=function(
                      mods_list
                      ,data
                      ,...
                      ){

    x_vars=names(data)[!names(data)%in% names(mods_list) ]
    reg_model_in=function(reg_obj,data,x_vars,...)reg_model(data[,c(reg_obj[["response"]],x_vars)],reg_obj,...)

    mods=lapply(mods_list,reg_model_in,data=data,x_vars=x_vars,...)
    names(mods)=names(mods_list)

    mods

   }


      # attr(mod_multi,"info") = list(
      #   arg_def = data.frame(
      #           c(
      #             "reg_multi_obj"
      #             ,"data"
      #             ,"..."
      #           )
      #     ,def=c(
      #             "object returned by reg_multi function"
      #             ,"data used to create reg_multi object"
      #             ,"additional arguments to reg_model"
      #           )
      #     )
      #     ,description = "accepts object returned by reg_multi (function to compute regSubsets for multiple variables) and returns actual model objects"
      # )
#example code
#
# mods=mod_multi(rg_mult,pl_dat[,c(resp,aux_few)])



   #2013 December 08
   #include raster creation component of predictions
   #note: "rasterize" is not functional yet

#'@export
#'@rdname OLS_modeling
pred_multi=function(
                    mods_list                                                        #list of models
                    ,data
                    ,dat0
                    ,id_col
                    ,n_clus=1
                    ,clus=NA
                    ,out_dir=NA
                    ,se_fit=FALSE
                    ,return=TRUE
                    ,fix_outliers=F
                    ,outlier_fun=function(y0,yhat,yhat_se)
                                    {
                                        large=yhat>max(y0)*1.5
                                        small=yhat<0
                                        yhat[small]=0
                                        yhat[large]=quantile(y0,.99)*1.5
                                        yhat_se[large]=max(yhat_se[!large & !small])
                                        yhat_se[small]=max(yhat_se[!large & !small])
                                        data.frame(pd=yhat,se_pd=yhat_se)
                                    }
                    ){

        #test and create directory
        if(!is.na(out_dir))if(!file_test("-d",out_dir)) dir.create(out_dir)

        #internal pediction function where work actually happens
        fn_pred_in=function(
                          i
                          ,mods_list
                          ,data
                          ,dat0
                          ,id_col
                          ,se_fit
                          ,out_files
                          ,return
                          ,fix_outliers
                          ,outlier_fun

                          ){
        print(i)

        #create dataframe with values
        if(!se_fit){
          pdi = predict(object=mods_list[[i]],newdata=get("data",envir=parent.env(environment())),se.fit=F)
          dfi=data.frame(get("data",envir=parent.env(environment()))[,id_col,drop=FALSE],pd=pdi )
        }
        if(se_fit){
            pdi = predict(object=mods_list[[i]],newdata=get("data",envir=parent.env(environment())),se.fit=T)
            dfi=data.frame(get("data",envir=parent.env(environment()))[,id_col,drop=FALSE],pd=pdi[["fit"]],se_pd= pdi[["se.fit"]])
        }
         rm("pdi")
         gc()

        #correct too large and too small values
        if(fix_outliers){

            #select the original response values for this model
            if(is.data.frame(dat0) | is.matrix(dat0)) y0 = dat0[,names(mods_list)[i]]
            else if(is.list(dat0)) y0 = dat0[[names(mods_list)[i]]]
            else { y0 = NULL; warning("fix_outliers: unrecognized dat0 type in pred_multi; skipping outlier correction") }

            if(!is.null(y0)){
              if(se_fit){
                dfi[,c("pd","se_pd")] = outlier_fun(y0, dfi[,"pd"], dfi[,"se_pd"])
              } else {
                #no SE available: clamp predictions only
                dfi[,"pd"] = outlier_fun(y0, dfi[,"pd"], rep(NA_real_, nrow(dfi)))[["pd"]]
              }
            }

        }

        #update names
         #names(dfi)[1]=IDcol
         #get fit and se.fit
         pd_nm=grep("pd",names(dfi))
         names(dfi)[pd_nm]=paste(names(mods_list)[i],names(dfi)[pd_nm],sep="_")
         gc()

        #return and write predictions
         #write.csv(dfi,paste(out_files[i],".csv",sep=""),row.names=F)
         #gc()

         return(dfi[,grep("pd",names(dfi)),drop=F])
         rm(list=ls())
         gc()

      }

      #create file names

       if(!is.na(out_dir)) out_files=paste(out_dir,names(mods_list),sep="")
       else out_files=NULL

      #get rid of extra columns
        fn_vars=function(x)names(x[["model"]])[-1]
        x_vars=unique(unlist(lapply(mods_list,fn_vars)) )
        data=data[,names(data)[(names(data) %in% c(id_col,x_vars))],drop=F]
        gc()


      #run pred function
       if(n_clus>1){


           if(is.na(clus[1]))clus_in=makeCluster(n_clus)
           else clus_in=clus

           res=parLapply(clus_in,1:length(mods_list),fn_pred_in,mods_list=mods_list,data=data,id_col=id_col
                         ,se_fit=se_fit,out_files=out_files,return=return
                         ,fix_outliers=fix_outliers,outlier_fun=outlier_fun,dat0=dat0)

           if(is.na(clus[1])) stopCluster(cl=clus_in)

       }else {

           res=lapply(1:length(mods_list),fn_pred_in,mods_list=mods_list,data=data,id_col=id_col
                      ,se_fit=se_fit,out_files=out_files,return=return
                      ,fix_outliers=fix_outliers,outlier_fun=outlier_fun,dat0=dat0)

       }


      cell_predictions=do.call(cbind,list(data[,c(id_col),drop=FALSE],res))
      rm("res")
      gc()
      if(!is.na(out_dir) ){
        write.csv(cell_predictions, paste(out_dir,"cell_predictions.csv",sep=""),row.names=FALSE)
        save(cell_predictions, file=paste(out_dir,"cell_predictions.rda",sep=""))
      }
        if(return) return(cell_predictions)

   }

#       attr(pred_multi,"info") = list(
#         arg_def = data.frame(
#                 c(  names(formals(pred_multi))
#
#                 )
#           ,def=c(
#                     "list of models"                                                        #list of models
#                     ,"auxiliary data for target area"
#                     ,"optional - include dataframe of original response values and correct outliers"
#                     ,"name of column with unique observation ids - not matching training data"
#                     ,"number of cores to use"
#                     ,"where to place predictions"
#                     ,"should standard errors be fit"
#                     ,"should the data also be returned?"
#                 )
#           )
#           ,description = "accepts list of models and corresponding data and provides predictions from each model"
#       )

#Example:

#    pdi=predMulti(
#      mods[]                             #list of models
#      ,data=datLid#[(1*50000):(2*50000),]                     #input aux data for landscape - names must match data used to fit "mods"
#      ,dat0=dat1
#      ,n_clus=5                        #for parallel computations
#      ,outDir="c:/temp/chugachmiut/"   #where to send files
#      ,IDcol="XY"                      #name of id column
#
#    )




   #accepts a list of models
#'@export
#'@rdname OLS_modeling
multi_bs=function(
                    mods_list
                    ,n_boot=50
                    ,n_clus
                    ,lm_boot
                    ){

      lm_boot_in=function(
                          i
                          ,mods_list
                          ,n_boot
                          ,lm_boot
                          ){

        l_in=lm_boot(mods_list[[i]],n_boot=n_boot)
        l_in=l_in[names(l_in) != "call"]
        #keep the character `predictors` out of the numeric unlist, otherwise the
        #whole vector is coerced to character (err_632 etc. become strings).
        #Return a one-row data.frame so rbind() preserves per-column types.
        preds = l_in[["predictors"]]
        num_in = l_in[names(l_in) != "predictors"]
        data.frame(t(unlist(num_in)), predictors = preds, stringsAsFactors = FALSE)
      }
      mods_list=mods_list[!sapply(mods_list,is.null)]

      #run pred function
       if(n_clus>1){

           clus_in=makeCluster(n_clus)

           res=parLapply(clus_in,1:length(mods_list),lm_boot_in,mods_list,n_boot,lm_boot)

           stopCluster(cl=clus_in)

       }else {

           res=lapply(1:length(mods_list),lm_boot_in,mods_list,n_boot,lm_boot)

       }

      data.frame(variable=names(mods_list),n_boot=n_boot,as.data.frame(do.call(rbind, res)))
   }

     attr(multi_bs,"info") = list(
        arg_def = data.frame(
                c(  names(formals(multi_bs))

                )
          ,def=c(
                    "list of models"
                    ,"number of bootstraps"
                    ,"number of cores to use when processing"
                    ,"lm_boot function"
                )
          )
          ,description = "bootstrap a list of models"
      )

   #eventually  multiBSmany needs to arange the data vertically... too many columns...


#compute summary on list of models
#'@export
#'@rdname OLS_modeling
lm_summary=function(
    mods_list
    ,data
    ,resids=FALSE
    ){ #,max_sims=NA



       #recursive version in the case of multiple supplied models.
       #(an lm object is also a list, so exclude it here.)
        if(is.list(mods_list) && !inherits(mods_list, "lm") && !inherits(mods_list, "try-error")){

          #loop across models
          res_iters= lapply(mods_list,function(...)try(lm_summary(...)),data=data,resids=FALSE)

          #compile results
          res_df=data.frame(do.call(rbind,res_iters[sapply(res_iters,is.data.frame)]),row.names=NULL)

          #append bad rows
          bad_ids=sapply(res_iters,function(x)!is.data.frame(x))

          if(sum(bad_ids)>0){

              bad_df=res_df[0,]
              bad_df[1:sum(bad_ids),1]=names(res_iters)[bad_ids]
              res_df=data.frame(rbind(res_df,bad_df))

          }
          #return data
          return(res_df)

        }else if(inherits(mods_list, "try-error")){

          df_in=data.frame(
                 y=NA
                 ,RMSE=NA
                 ,RMSE_cv=NA
                 ,RMSE_cv_pct=NA
                 ,RMSE_optmsm_pct=NA
                 ,R2=NA
                 ,R2_cv=NA
                 ,R2_optmsm_pct=NA
                 ,n=NA
                 ,SE_cv=NA
                 ,SE_cv_pct=NA
                 ,mean_y=NA
                 ,sd_y=NA
                 ,sd_e=NA
                 ,pred_nms=NA
                 ,notes="model provided was class 'try-error'"
               )

          return(df_in)

        }else if(inherits(mods_list, "lm")){

          #apparent model statistics
            n_in=nrow(mods_list$model)
            sigma_in=sd(residuals(mods_list),na.rm=T)*sqrt((n_in-1)/(n_in-length(mods_list$coefficients)))
            rsq_in=1-var(residuals(mods_list),na.rm=T)/var((residuals(mods_list)+fitted(mods_list)),na.rm=T)

          #leave-one-out CV, computed on the model's OWN fitting frame so the
          #observed value and the held-out prediction always refer to the same
          #row (the previous version paired model$model[i] with an external
          #`data` row i, which silently misaligned when `data` had dropped/NA rows).
          md = mods_list$model
          fn_e=function(i,model,md){

              lmi=try(update(model, data=md[-i,,drop=FALSE]))
              if(!inherits(lmi,"try-error"))
              data.frame(y=md[i,1],e=unlist(md[i,1]-predict(lmi,newdata=md[i,,drop=FALSE])))
              else
              data.frame(y=md[i,1],e=NA)

          }

          ei=do.call(rbind,lapply(seq_len(nrow(md)),fn_e,model=mods_list,md=md))
          ei=ei[!is.na(ei[,"e"]),]
          rmse_cv=sqrt(sum(ei$e^2,na.rm=T)/(nrow(ei)-length(names(mods_list$model)[-1])))

          df_in=data.frame(
                 y=names(mods_list$model)[1]
                 ,RMSE=sigma_in
                 ,RMSE_cv=rmse_cv
                 ,RMSE_cv_pct=rmse_cv/mean(ei$y,na.rm=T)*100
                 ,RMSE_optmsm_pct=round((rmse_cv-sigma_in)/(rmse_cv)*100,1)
                 ,R2=rsq_in*100
                 ,R2_cv=(1-var(ei$e,na.rm=T)/var(ei$y,na.rm=T))*100
                 ,R2_optmsm_pct=round((rsq_in-(1-var(ei$e,na.rm=T)/var(ei$y,na.rm=T)))/((1-var(ei$e,na.rm=T)/var(ei$y,na.rm=T)))*100,0)
                 ,n=nrow(data)
                 ,SE_cv=rmse_cv/sqrt(n_in)
                 ,SE_cv_pct=rmse_cv/sqrt(n_in)/mean(ei$y,na.rm=T)*100
                 ,mean_y=mean(ei$y,na.rm=T)
                 ,sd_y=sd(ei$y,na.rm=T)
                 ,sd_e=sd(ei$e,na.rm=T)
                 ,pred_nms=paste(names(mods_list$model)[-1],collapse=",",sep=",")
                 ,notes="successful validation"
               )
          if(!resids)return(df_in)
          else return(list(res=df_in,resids=ei))
       }else{

         stop("unknown object class - mods_list should be 1) a list of lm objects or 2) an lm object or 3) an object of class 'try-error' e.g. when an lm fit fails")


       }

      }
