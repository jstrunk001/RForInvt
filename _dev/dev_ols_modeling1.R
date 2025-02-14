
#'@name OLS_modeling
#'@title
#'functions related to modeling
#'
#'@description
#'
#'<Delete and Replace>
#'
#'@details
#'
#'<Delete and Replace>
#'
#'\cr
#'
#'Revision History
#' \tabular{ll}{
#'1.0 \tab date and revisions.. \cr
#'}
#'
#'
#'@author
#'
#'Jacob Strunk <someone@@somewhere.com>
#'
#'
#'@param dat_yx tbd
#'@param reg_obj tbd
#'@param rank tbd
#'@param rank_by tbd
#'@param debug tbd
#'@param ... tbd
#'\cr\cr
#' \bold{lm_boot() parameters:}
#'@param model tbd
#'@param y tbd
#'@param data tbd
#'@param n_boot tbd
#'\cr\cr
#' \bold{many_boots() parameters:}
#'@param model tbd
#'@param n_range tbd
#'@param r_boots tbd
#'@param n_clus tbd
#'@param lm_boot tbd
#'@param ... tbd
#'\cr\cr
#' \bold{reg_multi() parameters:}
#'@param y_vars tbd
#'@param data tbd
#'@param form_y tbd
#'@param n_v_max tbd
#'@param n_best tbd
#'@param really_big tbd
#'@param n_clus tbd
#'@param ... tbd
#'\cr\cr
#' \bold{mod_multi() parameters:}
#'@param mods_list tbd
#'@param data tbd
#'@param ... tbd
#'\cr\cr
#' \bold{pred_multi() parameters:}
#'@param mods_list tbd
#'@param data tbd
#'@param dat0 tbd
#'@param id_col tbd
#'@param n_clus tbd
#'@param clus tbd
#'@param out_dir tbd
#'@param se_fit tbd
#'@param return tbd
#'@param fix_outliers tbd
#'@param outlier_fun tbd
#'\cr\cr
#' \bold{multi_bs() parameters:}
#'@param mods  tbd
#'@param n_boot tbd
#'@param n_clus tbd
#'@param lm_boot tbd
#'\cr\cr
#' \bold{lm_summary() parameters:}
#'@param mods_list list of models where each element is named with response name
#'@param data tbd
#'@param resids tbd
#'
#'
#'@return
#'
#'reg_model:
#'lm_boot:
#'many_boots:
#'reg_multi:
#'mod_multi:
#'pred_multi:
#'multi_bs:
#'lm_summary:
#'
#'@examples
#'
#'<Delete and Replace>
#'
#'
#'
#'
#'@seealso \code{\link{regsubsets}}\cr
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
    desc=F
    if(rank_by[1] %in% c("rsq","rss","adjr2") ) desc=T
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

      require(bootstrap)

     if(class(model) != "lm"){
       model=lm(model,data=data)
      }

      if(class(data) != "data.frame"){
        data=model[["model"]]
      }

      if(is.na(y)){
       y=model$model[,1]
      }


      #functions required for crossvalidation
      theta_fit=function(x,y,model,...)update(model,data=data.frame(x,row.names=NULL))
      theta_predict = function(fit,x,...) predict(fit, newdata=data.frame(x,row.names=NULL) )
      sq_err <- function(y,y_hat)   (y-y_hat)^2

      #perform .632+ crossvalidation
      bp1=bootpred(y=y,nboot=n_boot,x=data,err.meas=sq_err,theta.predict=theta_predict,theta.fit=theta_fit,model=model)
      bp1[1:3]=lapply((bp1[1:3]),sqrt)

      names(bp1)=c("app_err","optim","err_632","call")

      #set up output
      bp1[["Sy"]]=sd(y)
      bp1[["meany"]]=mean(y)
      bp1[["err_632_pct"]]=bp1[["err_632"]]/bp1[["meany"]]*100
      bp1[["err_632_pct_sdy"]]=bp1[["err.632"]]/bp1[["Sy"]]*100
      bp1[["err_632_rsq"]]=max(0,1-(bp1[["err_632"]]/bp1[["Sy"]])^2)
      bp1[["n"]]=sum(!is.na(y))
      bp1[["n_gt_0"]]=sum(is.numeric(y) & y >0)
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
        data.frame(n=ni,err_632=lm_boot(modk,...)[["err.632"]])

       }

     #run bootstraps
       if(n_clus>1){

           require(parallel)
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
    require(leaps)
    x_vars=names(data)[!names(data)%in% y_vars ]

    c_fn=function(y,data,x_vars,form_y,n_v_max,n_best,really_big,...){

        print(paste("response is ",y))
        require(leaps)
        rg_i=regsubsets(x=as.formula(gsub("y",y,form_y)),data=data[,c(y,x_vars)],nvmax=n_v_max,nbest=n_best,really.big=really_big,...)
        rg_i[["response"]]=y
        rg_i

       }

     #run bootstraps
       if(n_clus>1){

           require(parallel)
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
        form_i = as.formula(gsub("y",y,form_y))
        lm_i=lm(form_i,data=data[,c(y,x_vars)],...)
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

    x_vars=names(data)[!names(data)%in% names(reg_multi_obj) ]
    models_in=function(reg_obj,data,x_vars,...)reg_model(data[,c(reg_obj[["response"]],x_vars)],reg_obj,...)

    mods=lapply(reg_multi_obj,reg_model_in,data=data,x_vars=x_vars,...)
    names(mods)=names(reg_multi_obj)

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
            dfi=data.frame(get("data",envir=parent.env(environment()))[,id_col,drop=FALSE],pd=pdi[["fit"]],se.pd= pdi[["se.fit"]])
        }
         rm("pdi")
         gc()

        #correct too large and too small values
        #if(!missing(dat0)){
        if(fix_outliers){

            if(is.data.frame(dat0) | is.matrix(dat0))dfi[,c("pd","se_pd")]=outlier_fun(dat0[,names(mods_list)[i]],dfi[,c("pd")],dfi[,c("se_pd")])
            else if(is.list(dat0))dfi[,c("pd","se_pd")]=outlier_fun(dat0[[names(mods_list)[i]]],dfi[,c("pd")],dfi[,c("se_pd")])
            else warning("unrecognized data type on line 388 of script 'OLS_modeling' in fix_outliers component of function pred_multi")

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

           require(parallel)

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
        unlist(l_in)
      }
      mods_list=mods_list[!sapply(mods_list,is.null)]

      #run pred function
       if(n_clus>1){

           require(parallel)
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



       #recursive version in the case of multiple supplied models
        if(class(mods_list)=="list"){

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

        }else if(class(mods_list)=="try-error"){

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

        }else if(class(mods_list)=="lm"){

          #apparent model statistics
            n_in=nrow(mods_list$model)
            sigma_in=sd(residuals(mods_list),na.rm=T)*sqrt((n_in-1)/(n_in-length(mods_list$coefficients)))
            rsq_in=1-var(residuals(mods_list),na.rm=T)/var((residuals(mods_list)+fitted(mods_list)),na.rm=T)

          #sm_in=lm.summary(x)
          fn_e=function(model,x,i){

              lmi=try(update(model, data=x[-i,]))
              if(!class(lmi)=="try-error")
              data.frame(y=model$model[i,1],e=unlist(model$model[i,1]-predict(lmi,newdata=x)[i]))
              else
              data.frame(y=model$model[i,1],e=NA)

          }

          ei=do.call(rbind,lapply(1:nrow(data),fn_e,x=data,model=mods_list))
          ei=ei[!is.na(ei[,"e"]),]
          rmse_cv=sqrt(sum(ei$e^2,na.rm=T)/(n_in-length(names(mods_list$model)[-1])))

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
          else return(list(res=dfIn,ei))
       }else{

         stop("unknown object class - mods_list should be 1) a list of lm objects or 2) an lm object or 3) an object of class 'try-error' e.g. when an lm fit fails")


       }

      }
# demo
#      models=list(
#       #tpa
#       ba=lm(BA.acre~Elev.P25:Percentage.first.returns.above.5.00 +Elev.maximum*Percentage.first.returns.above.5.00,data=dat1)
#       ,tcf=lm(TCF.Acre~Elev.P25:Elev.maximum+Canopy.relief.ratio:Percentage.first.returns.above.5.00,data=dat1)
#       ,bf= lm(BF.acre~Elev.P20*Percentage.first.returns.above.5.00+Elev.P95*Percentage.first.returns.above.5.00,data=dat1)
#       ,mcf=lm(MCF.acre~Elev.P20*Percentage.first.returns.above.5.00+Elev.P95*Percentage.first.returns.above.5.00,data=dat1)
#       ,bdt=lm(BDT.acre~Elev.P20*Percentage.first.returns.above.5.00+Elev.P95*Percentage.first.returns.above.5.00,data=dat1)
#      )
#
#     stats=do.call(rbind,lapply(models,lmSummary,dat1))
#     stats


###################################
##################################
#                                   FUNCTIONS BELOW NOT YET IMPLEMENTED IN DNR FRAMEWORK
##################################
###################################

#
#   #accepts a list of models
#   multi_many_boots=function(
#                             mods
#                             ,n_range
#                             ,r_boots=50
#                             ,n_clus=1
#                             ,tempOut="c:\\temp\\"
#                             ){
#
#      if(!file.exists(tempOut)) file.create(tempOut)
#
#      tempIn=paste(tempOut,"\\Temp_MultiBSmany",gsub(":","_",gsub(c("[ -]"),"_",Sys.time()) ),".csv",sep="")
#
#      lm_boot_in_many=function(
#                               i
#                               ,mods
#                               ,n_range
#                               ,rboot
#                               ,many_boots
#                               ,lm_boot
#                               ,temp_in
#                               ,n_clus
#                               ){
#        l_in=data.frame(many_boots(models[[i]],n_range,r_boots=r_boots,lm_boot=lm_boot))
#        names(l_in)=paste(names(l_in),names(models)[i],sep=".")
#        tempIn=paste(tempIn,"_par",i,".csv",sep="")
#        write.csv(l_in[,-1],file=temp_in,append=TRUE)
#        l_in[,-1]
#      }
#
#      #extract models with data
#     mods[!sapply(mods,is.null)]
#
#      #run bootstrap function
#      #run pred function
#       if(n_clus>1){
#
#           require(parallel)
#           clus_in=makeCluster(n_clus)
#
#           res_in=parLapply(clus,1:length(multiMods),lm_boot_in_many,mods,n_range=n_range,r_boot=rboot,many_boots=many_boots,lm_boot=lm_boot,tempIn)
#
#           stopCluster(cl=clus_in)
#
#       }else {
#
#           res_in=lapply(1:length(mods),lm_boot_in_many,mods,n_range,r_boot,many_boots,lm_boot=lm_boot,tempIn)
#
#       }
#
#      #organize data as desired
#      fn_rot=function(x,n_range){
#        df_in=data.frame(t(unlist(x)))
#        names(df_in)=as.vector(sapply(c("SE_ybar","rsq_err_632"),paste,n_range,sep=".") )
#        df_in[,paste("SE_ybar",n_range,sep=".")]=dfin[,paste("SE_ybar",n_range,sep=".")]/sqrt(n_range)
#        df_in
#      }
#
#      df_res=do.call(rbind,lapply(res_in,fn_rot,n_range))
#
#      get_n=function(x) data.frame(n=nrow(x[["model"]]),ngt0=sum(x[["model"]][,1] >0))
#      n_vals=do.call(rbind,lapply(mods,get_n))
#
#      #prepare output dataframe
#      df_in=data.frame(names(mods),r_boot,df_Res)
#      names(dfin)[1:2]=c("variable","r_boot")
#      df_in[,c("n","ngt0")]=n_vals
#
#      #set negative R2 values to 0
#      rsq_names=grep("rsq",names(df_in))
#      dfin[,rsq_names]=(df_in[,rsq_names])*(df_in[,rsq_names]<1)
#
#      #get predictors
#      pred_fn=function(x) paste(attr(x[["terms"]],"term.labels"),collapse=" ")
#      dfin[,"predictors"]=sapply(mods,pred_fn)
#
#      #return results
#      dfin
#   }
#
#
#   #example code
#   #multiBSmanyStrata(modsBase,strX=fw_horz2[,"STAND_NO"],nrange=seq(20,150,20), rboot=50)#,clus=clus)
#
#  #Calculate stratified sampling SE of mean
#  stratSE=function(y,stratVec,areaVec,Only1=c("Omit1","Merge1")){
#
#    SE_In=function(yVec,stratVec,areaVec,Only1=c("Omit1","Merge1")){
#
#      #split response and area
#      splitY=split(yVec,stratVec)
#      splitArea=split(areaVec,stratVec)
#
#      #get length, sd, and area of strata
#      lengthVec=sapply(splitY,length)
#      sdVec=sapply(splitY,sd,na.rm=TRUE)
#      aVec=sapply(splitArea,mean,na.rm=TRUE)
#
#      #strata with only 1 observation
#      nm1=names(lengthVec)[lengthVec<2]
#
#      #Calculate SEs
#        #prepare container with SE
#        SE=data.frame()
#
#        #SEs Omit groups with 1 observations
#        if(Only1[1]=="Omit1" | Only1[1]=="Both" & sum(lengthVec<2,na.rm=TRUE)>0){
#
#        keep=which(lengthVec>1)
#        SE[1,"Omit1"]=sqrt(sum(sdVec[keep]^2/lengthVec[keep]*aVec[keep]^2/(sum(aVec[keep],na.rm=TRUE)^2)))
#
#        }
#        if(Only1[1]=="Merge1" | Only1[1]=="Both" & sum(lengthVec<2,na.rm=TRUE)>0 ){
#        #Merge groups with 1 observations
#
#        #create new strata vector
#        stratVecMrg=stratVec
#        stratVecMrg[stratVec %in% nm1]=min(nm1)
#
#        #create splits based upon merged strata
#        splitY2=split(yVec,stratVecMrg)
#        splitArea2=split(areaVec,stratVecMrg)
#
#        lengthVec2=sapply(splitY2,length)
#        sdVec2=sapply(splitY2,sd,na.rm=TRUE)
#        aVec2=sapply(splitArea2,mean,na.rm=TRUE)
#
#        SE[1,"Merge1"]=sqrt(sum(sdVec2^2/lengthVec2*aVec2^2/(sum(aVec2,na.rm=TRUE)^2),na.rm=TRUE))
#
#        }
#        if(sum(lengthVec<2)==0 ){
#
#        SE[,"AllGT1"]=sqrt(sum(sdVec^2/lengthVec*areaVec^2/(sum(aVec)^2)))
#
#        }
#
#      unlist(SE)
#    }
#
#    if(!is.null(y))resIn=apply(y,2,SE_In,stratVec,areaVec,"Merge1")
#    else SE_In(y,stratVec,areaVec,"Merge1")
#
#    #else resIn=SE_In(ydf,stratVec,Only1)
#    resIn
#
#  }
#
#
#    #use same primary models for subgroups (helps when there are few observations to fit models in subgroups ...)
#    updateSubVars=function(newVars,modsPrimary,data,elimZeroVec=TRUE,minLength=5){
#       newVarsIn1=unique(newVars)
#
#      if(elimZeroVec){
#       newVarsIn1=unique(newVars[apply(data[,newVars]>minLength,2,sum)>0])
#      }
#
#      #match newVars with modsPrimary
#      matchIn=lapply(names(modsPrimary),grep,newVarsIn1)
#
#      #fit new set of response variables to existing set of predictors
#      fnModIn=function(i,Vars_i,mods,data,newVarsIn2){
#
#       #update single model
#       fnini=function(vari,modi,data){
#         #modin=
#         update(modi,paste(vari,"~.",sep=""),data=data)
#         #names(modin)=vari
#         #modin
#       }
#
#       #update all models corresponding to single primary response
#       lapply(newVarsIn2[Vars_i[[i]]],fnini,mods[[i]],data)
#      }
#
#      mods=unlist(lapply(1:length(matchIn),fnModIn,matchIn,modsPrimary,data,newVarsIn1),recursive=FALSE)
#
#      names(mods)=newVarsIn1[unlist(matchIn)]
#      mods
#    }
#
#    matchPrimarys=function(data,primary,secondary){
#
#      #get all variables
#      namesIn=names(data)
#
#      grep2=function(x,y){
#          g_in=sapply(y,grep,x)
#          !is.na(g_in==1)
#          }
#      #match primary and secondary variable
#      primMatch=do.call(rbind,lapply(namesIn,grep2,primary))
#      secMatch=do.call(rbind,lapply(namesIn,grep2,secondary))
#
#      #get unique combinations of primary and seccnary variables
#      matchesIn=data.frame(primMatch,secMatch)
#      unqmatch=as.matrix(matchesIn*1) %*% (1:ncol(matchesIn))
#
#      #match all variables to primary variables
#
#      match1=lapply(primary,grep,namesIn)
#
#      #match all variables to primary variables
#      match2=lapply(secondary,grep,namesIn)
#
#      margMatch=function(i,data,match1,match2,primary){
#
#       prim1_i=data[data[,match1[i]]!= primary[i],match1[i]]
#
#
#      }
#
#      lapply(1:length(match1),margMatch,data,match1,match2)
#
#    }


