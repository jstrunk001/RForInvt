#'
#'  This program is free software but it is provided WITHOUT WARRANTY
#'  and with ABSOLUTELY NO GUARANTEE of fitness or functionality for any purpose;
#'  you can redistribute it and/or modify it under the terms of the GNU
#'  General Public License as published by the Free Software Foundation;
#'  either version 2 of the License, or (at your option) any later version.
#'
#'
#functions to perform estimation

.estimate=function(
  x
  ,resp_nm="volcfgrs"
  ,aux_nm="ht_p70"
  ,strata_nm="str"
  ,wt_nm=c(NA,"wt")
  ,ef_nm=c(NA,"ef")      # 1/wt
  ,su_nm=c(NA,"su") #sampling unit for two-stage / multi-stage
  ,reg_form= "ht_p20 + ht_p70 * percentage_first_returns_above_6_00"
  ,pop
  ,N
  ,strata_cuts
  ,type=c("ht","regression","stratified","two-stage","calibrate")
  ,var_type=c("asym","bs","svypkg")
){

  if(is.na(wt_nm[1]) & !is.na(ef_nm[1])){
    wt_nm="wt"
    x[,wt_nm] = x[,ef_nm] / nrow(x)

  }
  if(is.na(wt_nm[1]) & is.na(ef_nm[1])){
    wt_nm="wt"
    x[,wt_nm] = N
  }

  warning("calibrate, two-stage, Multi-stage Not Yet Implemented")

  if(length(resp_nm) > 1){stop("1 response at this time")}

  #always compute ht-srs
  res0=.rand(x=x,resp_nm=resp_nm,wt_nm=wt_nm[1],ef_nm=ef_nm[1],N=N,var_type=var_type[1])

  #compute estimates
  if(type[1]=="ht"){
    res=.rand(x, resp_nm=resp_nm, wt_nm=wt_nm[1], ef_nm=ef_nm[1], N=N, var_type=var_type[1], type=type[1])
  }
  if(type[1]=="regression"){
    res=.reg(x, resp_nm=resp_nm, wt_nm=wt_nm[1], reg_form=reg_form, ef_nm=ef_nm[1], pop=pop, N=N, var_type=var_type[1], type=type[1])
  }
  if(type[1]=="stratified"){
    res=.str(x, resp_nm=resp_nm, wt_nm=wt_nm[1], ef_nm=ef_nm[1], strata_nm=strata_nm, pop=pop, N=N, var_type=var_type[1], type=type[1])
  }
  if(type[1]=="two-stage"){
    stop("\"two-stage\" Not Yet Implemented")
    res=.ts(x,resp_nm,aux_nm,wt_nm,ef_name,pop,N,strata_cuts,var_type[1])
  }
  if(type[1]=="multi-stage"){
    stop("\"multi-stage\" Not Yet Implemented")
    res=.ms(x,resp_nm,aux_nm,wt_nm,ef_name,pop,N,strata_cuts,var_type[1])
  }
  if(type[1]=="calibrate"){
    stop("\"calibrate\" Not Yet Implemented")
    res=.cb(x,resp_nm,aux_nm,wt_nm,ef_name,pop,N,strata_cuts,var_type[1])
  }

  return(.summary(res,res0))

}


.summary=function(res,res0){

  #computations

  if("deff" %in% names(res)) if(!is.na(res$deff)) deff_in = res$deff
  if("deff" %in% names(res)) if(is.na(res$deff)) deff_in = res$se_m^2 / res0$se_m^2
  if(!"deff" %in% names(res)) deff_in = res$se_m^2 / res0$se_m^2
  deffinv_in = 1/ deff_in

  rmse_srs=deff_in * res$se_m
  rsq_in = round(100 * (1 - deff_in) , 1 )

  #res0$se_m^2 / res$se_m^2

  #return results
  summary_in=data.frame(
    type=res$type
    ,resp=res$resp_nm
    ,var_type=res$var_type
    ,wt_nm=res$wt_nm
    ,ef_nm=res$ef_nm
    ,su_nm=res$su_nm
    ,formula=res$formula
    ,mean=res$mean
    ,total=res$total
    ,se_m=res$se_m
    ,se_m_srs=res0$se_m
    ,se_t=res$se_t
    ,se_t_srs=res0$se_t
    ,t_srs=res0$total
    ,m_srs=res0$mean
    ,rmse=res$rmse
    ,rmse_srs=rmse_srs
    ,rsq = rsq_in
    ,deff = deff_in
    ,deffinv = deffinv_in
    ,n=res$n
    ,n_str=res$n_str
    ,n_clus=res$n_clus
    ,N=res$N
  )

  return(summary_in)
}

#
.rand=function(x,resp_nm,wt_nm,ef_nm,pop,N,type,var_type,n_bs=400){

  require(survey)
  n = as.numeric(nrow(x))
  N = as.numeric(N)

  if(var_type[1]=="asym"){

    #match weights to estimator
    x[, wt_nm] = x[, wt_nm] * N / mean(x[, wt_nm])

    #hansen-herwitz estimator
    v_t_rs = var(x[,resp_nm] * x[,wt_nm]) / n
    t_rs = sum(x[,resp_nm] * x[,wt_nm] ) / n
  }

  if(var_type[1]=="bs"){

    fn_bs=function(i,x,resp_nm,wt_nm){
      n=nrow(x)
      bs_ix=sample(nrow(x),prob=x[,wt_nm],replace=T)
     # bs_ix=sample(nrow(x),replace=T)
      sum(x[bs_ix,resp_nm]*x[bs_ix,wt_nm]) / n
      #N*mean(x[bs_ix,resp_nm])
    }

    v_t_rs = var(sapply(1:n_bs,fn_bs,x,resp_nm,wt_nm))
    t_rs = sum(x[,resp_nm] * x[,wt_nm] ) / n
  }
  if(var_type[1]=="svypkg"){

    #match weights to estimator
    x[, wt_nm] = x[, wt_nm] * (N / n) / mean(x[, wt_nm])

    #use survey package
    svy_srs = svydesign(ids=~1, data=x, weights = x[, wt_nm])
    form_svy=as.formula(paste(resp_nm, " ~ 1" ))
    t_svy=data.frame(svytotal(form_svy,svy_srs,deff=T))
    v_t_rs = t_svy[,2]^2
    t_rs = t_svy[,1]


  }

  #derivative calculations
  mn_rs = t_rs / N
  se_t_rs = sqrt(v_t_rs)
  se_m_rs = se_t_rs / N
  rmse_rs = se_m_rs * sqrt(n)

  res_in =
    list(type = "rand"
         ,resp_nm = resp_nm
         ,var_type = var_type
         ,wt_nm = wt_nm
         ,ef_nm = ef_nm
         ,su_nm = NA
         ,formula = NA
         ,mean = mn_rs
         ,total = t_rs
         ,se_m = se_m_rs
         ,se_t = se_t_rs
         ,rmse = rmse_rs
         ,n = n
         ,N = N
         ,n_str=NA
         ,n_clus=NA
         ,deff = NA
    )


  if(var_type[1] ==  "svypkg"){
    res_in$deff = t_svy[,"deff"]
  }
  return(res_in  )
}



.reg=function(x,resp_nm,wt_nm,ef_nm,reg_form,pop,N,type,var_type){

  n = nrow(x)
  pop_mn=pop/N

  if(var_type[1] ==  "asym"){

    lm_in=lm(reg_form, data=x)
    lm_summ=summary(lm_in)

    x_in=x
    x_in[,"resids"] = residuals(lm_in)

    #estimate total and variance - Hansen Hurwitz
    err_est = .rand(x=x_in,resp_nm="resids",wt_nm=wt_nm[1],ef_nm=ef_nm[1],N=N,var_type=var_type[1])
    v_t_reg = err_est$se_t^2
    t_e = err_est$total
    t_reg = N * predict(lm_in, newdata = pop / N ) + t_e

  }
  if(var_type[1] ==  "bs"){

    # .632 bootstrap prediction error - unweighted
    lm_in=lm(reg_form, data=x)

    t_e = sum( residuals(lm_in) * x[, wt_nm] ) /  n
    t_reg = N * predict(lm_in, newdata = pop / N ) + t_e

    lm_in$call = call("lm",formula=as.formula(lm_in),data=x,x=T)
    bs_res=lm_boot(model=lm_in,n_boot = 500)
    v_t_reg = (N*bs_res$err_632)^2 / n

  }
  if(var_type[1] ==  "svypkg"){

    require(survey)

    pop_svy=c("(Intercept)" = N  ,unlist(pop) )

    svy_wts=as.formula(paste("~",wt_nm))
    svy_srs = svydesign(ids=~1,weights=svy_wts,data=x)
    svy_reg = svyglm(reg_form,svy_srs)

    #pd_m = data.frame(predict(svy_reg , newdata= pop / N,deff=T))

    form_y=as.formula(paste("~",resp_nm))
    cb=try(calibrate(svy_srs,reg_form,population =  pop_svy))
    if(class(cb) == "try-error"){
      stop(cb,"\n\n","Cause of Error: \nYou likely included an interaction in your model,but not a column for the interaction in your population totals: \ne.g. pop = data.frame( x1 = 50, x2 = 60, 'x1:x2' = 5000 ) notice the required x1:x2 column... ")
    }
    pd_m = data.frame(svymean(form_y,cb,deff=T))

    v_t_reg = (N*pd_m[,2])^2
    t_reg=N*pd_m[,1]

  }

  mn_reg = t_reg / N
  se_t_reg = sqrt(v_t_reg)
  se_m_reg = se_t_reg / N
  rmse_reg = se_m_reg * sqrt(n)


  res_in =
    list(type = type[1]
         ,resp_nm = resp_nm
         ,var_type = var_type
         ,wt_nm = wt_nm
         ,ef_nm = ef_nm
         ,su_nm = NA
         ,formula = format(reg_form)
         ,mean = mn_reg
         ,total = t_reg
         ,se_m = se_m_reg
         ,se_t = se_t_reg
         ,rmse = rmse_reg
         ,n = n
         ,N = N
         ,n_str=NA
         ,n_clus=NA
         ,deff = NA
    )
  if(var_type[1] ==  "svypkg"){
    res_in$deff=pd_m[1,"deff"]
  }
    return(res_in  )

}


.str=function(x,resp_nm,strata_nm,wt_nm,ef_nm,pop,N,type,var_type){

  require("plyr")


  if(class(pop) != "data.frame") stop("pop should be a 2 column data frame with strata names and strata sizes: data.frame(str=c(1,3,4,5),Ni=c(500,5000,500,5000) ) ")
  if(names(pop)[1] != strata_nm) stop("pop should be a 2 column data frame with strata names and strata sizes: data.frame(str=c(1,3,4,5),Ni=c(500,5000,500,5000) ) ")

  #get name of auxiliary attribute
  Ni_nm = names(pop)[2]

  form1 = as.formula(paste(resp_nm,"~",strata_nm))
  ni = aggregate(form1, data=x, FUN=length )
  names(ni)[2]="ni"
  n=sum(ni[,"ni"])
  pop_in=merge(pop,ni,by=strata_nm)

  #merge strata
  #fix cases of too few observations to compute strata variance
  if(sum(ni[,"ni"]<2)>0){

    warning("Strata Merged due to insufficient ni ( < 2 )")

    has_one=which(ni[,"ni"]<2)
    has_more=which(ni[,"ni"]>2)

    for(i in 1:length(has_one)){

      x_which_i = x[,strata_nm] == ni[has_one[i],strata_nm]
      new_str = ni[has_more[which.min(abs(has_one-has_more))],strata_nm]
      x[x_which_i,strata_nm] = new_str

      pop_which_i = pop[,strata_nm] == ni[has_one[i],strata_nm]
      pop_which_new = pop[,strata_nm] == new_str
      pop[pop_which_new,Ni_nm] = pop[pop_which_i,Ni_nm] + pop[pop_which_new,Ni_nm]
      #pop = pop[!pop_which_i,]
    }

    ni = aggregate(form1, data=x, FUN=length )
    names(ni)[2] = "ni"
  }

  if(var_type[1] == "asym"){

    spl_x=split(x,x[,strata_nm])
    Ni_mt = pop[match(names(spl_x),pop[,strata_nm]),]
    spl_Ni=split(Ni_mt[,Ni_nm],Ni_mt[,strata_nm])

    #push variance estimation to .rand function for each stratum
    vars_str = rbind.fill(mapply(estimate,spl_x,N=spl_Ni,MoreArgs=list(resp_nm=resp_nm,wt_nm=wt_nm,var_type=var_type),SIMPLIFY = F))
    v_t_str = sum(vars_str$se_t^2)
    t_str = sum(vars_str$total)

  }

  if(var_type[1] == "svypkg"){

    #use survey package completely
    if(T){

      spl_x=split(x,x[,strata_nm])
      Ni_mt = pop[match(names(spl_x),pop[,strata_nm]),]
      spl_Ni=split(Ni_mt[,Ni_nm],Ni_mt[,strata_nm])


      wt_fn=function(xi,Ni,wt_nm){
        ni=nrow(xi)
        xi[,wt_nm] = xi[,wt_nm] * (Ni / ni) / mean(xi[, wt_nm])
        return(xi)
      }

      x1=rbind.fill(mapply(wt_fn,spl_x,spl_Ni,wt_nm,SIMPLIFY = F))


      form_str=as.formula(paste("~",strata_nm))
      form_y=as.formula(paste("~",resp_nm))
      svy_str = svydesign(ids=~1, strata=x1[,strata_nm], data=x1, weights = x1[, wt_nm]  )
      m_svy = data.frame(svymean(form_y,svy_str,deff=T))
      t_svy = N * m_svy
      v_t_str = t_svy[,2]^2
      t_str = t_svy[,1]

    }

    #use survey package for strata estimates
    if(F){
      spl_x=split(x,x[,strata_nm])
      Ni_mt = pop[match(names(spl_x),pop[,strata_nm]),]
      spl_Ni=split(Ni_mt[,Ni_nm],Ni_mt[,strata_nm])

      #push variance estimation to .rand function for each stratum
      vars_str = rbind.fill(mapply(estimate,spl_x,N=spl_Ni,MoreArgs=list(resp_nm=resp_nm,wt_nm=wt_nm,var_type=var_type),SIMPLIFY = F))
      v_t_str = sum(vars_str$se_t^2)
      t_str = sum(vars_str$total)

   }

  }

  if(var_type[1] == "bs"){

    stop("Bootstrap estimator not yet implemented for \"stratified\" design")

  }

  mn_str = t_str / N
  se_t_str = sqrt(v_t_str)
  se_m_str = se_t_str / N
  rmse_str = se_m_str * sqrt(n)
  n_str = nrow( ni )


  res_in=list(type = "stratified"
         ,resp_nm = resp_nm
         ,var_type = var_type
         ,wt_nm = wt_nm
         ,ef_nm = ef_nm
         ,su_nm = NA
         ,formula = paste(resp_nm,"~",strata_nm)
         ,mean = mn_str
         ,total = t_str
         ,se_m = se_m_str
         ,se_t = se_t_str
         ,rmse = rmse_str
         ,n = n
         ,N = N
         ,n_str=n_str
         ,n_clus=NA
         ,deff = NA
    )

  if(var_type[1] ==  "svypkg") {
    res_in["deff"] = m_svy[,"deff"]
  }

  return(
    res_in
  )

}

.cb=function(x,resp_nm,wt_nm,ef_name,reg_form,pop,N,type,var_type){

  stop("calibration Not Yet Implemented")
}

.ts=function(...){

  stop("two-stage Not Yet Implemented")

}

.ms=function(...){

  stop("Multi-stage Not Yet Implemented")

}

#generate data to test various functions
.pop_test=function(
  N=10000
  ,n=200
  ,wt=c("equal","rand","propx","propy")
  ,err=c("homoskedastic","heteroskedastic")
  ,nstrat=10
  ,nclus=10
  ,type=c("random","regression","stratified","two-stage")
){


  x_in=NULL
  str_in = NULL

  if(type[1]=="random"){

    x_in=abs(rnorm(N))*100
    y_in=x_in + rnorm(N)*100/5

    if(wt[1]=="equal") p_i= rep(1/N,N)
    if(wt[1]=="propx") p_i= 1/(2 - (x_in-min(x_in)+quantile(abs(x_in),.1))/diff(range(x_in)))
    if(wt[1]=="propy") p_i= 1/(2 - (y_in-min(y_in)+quantile(abs(y_in),.1))/diff(range(y_in)))

    p_i=p_i/sum(p_i)
    wti=1/p_i

    s_ix=sample(N,n,prob=p_i)

  }
  if(type[1]=="regression"){

    x_in=abs(rnorm(N))*100
    if(err[1] == "homoskedastic") y_in=abs(x_in + rnorm(N)*100/5)
    if(err[1] == "heteroskedastic") y_in=abs(x_in + rnorm(N)*100/15 + rnorm(N)*x_in/4)

    if(wt[1]=="equal") p_i= rep(1/N,N)
    if(wt[1]=="propx") p_i= 1/(2 - (x_in-min(x_in)+quantile(abs(x_in),.1))/diff(range(x_in)))
    if(wt[1]=="propy") p_i= 1/(2 - (y_in-min(y_in)+quantile(abs(y_in),.1))/diff(range(y_in)))

    p_i=p_i/sum(p_i)
    wti=1/p_i

    s_ix=sample(N,n,prob=p_i)
    #s_in=data.frame(dat_pop , wt=wti,pi=p_i)[s_ix,]

  }
  if(type[1]=="stratified"){

    x_in=abs(rnorm(N))*100
    if(err[1] == "homoskedastic") y_in=abs(x_in + rnorm(N)*100/5)
    if(err[1] == "heteroskedastic") y_in=abs(x_in + rnorm(N)*100/15 + rnorm(N)*x_in/4)
    cut_vals_in=quantile(x_in,(0:nstrat)/nstrat)
    str_in=cut(x_in,cut_vals_in, labels = FALSE, include.lowest = T)
    p_i=1/N
    wti=1/p_i
    spl_pop_ix=split(1:N,str_in)
    s_cut=cut(1:n,nstrat,labels = FALSE)
    s_spl=split(s_cut,s_cut)
    si_n=sapply(s_spl,length)

    #adjust weights to sample proportional to size
    if(wt=="propx"){
      spl_pop_x=split(x_in,str_in)
      pop_Ni=sapply(spl_pop_x,length)
      pop_mui=sapply(spl_pop_x,mean)

      wti_n= pop_mui*(si_n / pop_Ni)/sum(pop_mui) * nstrat
      si_n=round(pop_Ni*wti_n)
      si_n[si_n<2] = 2
      #force sample size to n - remove observations from largest group
      if(sum(si_n) > n) si_n[length(si_n)] = si_n[length(si_n)] - (sum(si_n) - n)

      p_i=(si_n/pop_Ni)[str_in]
      p_i=p_i/sum(p_i)
      wti = 1 / p_i

    }
    if(wt=="propy"){

      spl_pop_y=split(y_in,str_in)
      pop_Ni=sapply(spl_pop_y,length)
      pop_mui=sapply(spl_pop_y,mean)
      wti= pop_mui*(si_n / pop_Ni)/sum(pop_mui) * nstrat
      si_n=round(pop_Ni*wti)
      si_n[si_n<2] = 2
      #force sample size to n - remove observations from largest group
      if(sum(si_n) > n) si_n[length(si_n)] = si_n[length(si_n)] - (sum(si_n) - n)

      p_i=si_n/pop_Ni
      p_i=pi/sum(p_i)
      wti = 1 / p_i

    }

    s_ix=try(unlist(mapply(sample,spl_pop_ix,si_n,replace=F,SIMPLIFY = F)),silent=T)
    if(class(s_ix)=="try-error") s_ix=unlist(mapply(sample,spl_pop_ix,si_n,replace=T,SIMPLIFY = F))

  }
  if(type[1]=="two-stage"){

    stop("two-stage Not Yet Implemented")
    #res=list(type=type[1],pop=dat_pop,s=s_in,N=N,n=n,wt=wt[1],nstrat=nstrat,nclus=nclus)

  }
  if(type[1]=="multi-stage"){

    stop("multi-stage Not Yet Implemented")
    #res=list(type=type[1],pop=dat_pop,s=s_in,N=N,n=n,wt=wt[1],nstrat=nstrat,nclus=nclus)

  }

  #create population
  dat_pop=data.frame(y=y_in)
  dat_pop[,"x"]=x_in
  dat_pop[,"str"]=str_in
  dat_pop[,"wt"]=wti
  dat_pop[,"pi"]=p_i

  s_in=dat_pop[s_ix,]

  res=list(type=type[1],pop=dat_pop,s=s_in,N=N,n=n,wt=wt[1],nstrat=1,nclus=1)

  return(res)

}

if(F){

  #generate various populations
  p1=pop_test(type=c("random"),wt="equal")
  p2=pop_test(type=c("random"),wt="propx")
  p3=pop_test(type=c("random"),wt="propy")
  p4=pop_test(type=c("regression"),wt="equal")
  p5=pop_test(type=c("regression"),wt="propx")
  p6=pop_test(type=c("regression"),wt="propy")
  p7=pop_test(type=c("regression"),wt="equal",err="heteroskedastic")
  p8=pop_test(type=c("regression"),wt="propx",err="heteroskedastic")
  p9=pop_test(type=c("regression"),wt="propy",err="heteroskedastic")
  p10=pop_test(type=c("stratified"),wt="equal")
  p11=pop_test(type=c("stratified"),wt="propx")
  p12=pop_test(type=c("stratified"),wt="propy")
  p13=pop_test(type=c("stratified"),wt="equal",err="heteroskedastic")
  p14=pop_test(type=c("stratified"),wt="propx",err="heteroskedastic")
  p15=pop_test(type=c("stratified"),wt="propy",err="heteroskedastic")

  #plot resulting data
  plot(density(p1$pop$y),col="blue","simple random sample");lines(density(p1$s$y),col="red");legend("topright",legend=c("population","sample"),lty=1,col=c("blue","red"))
  plot(density(p2$pop$y),col="blue","random sample, wti ~ some x");lines(density(p2$s$y),col="red");lines(density(p2$s$y,weights=p2$s$wt/sum(p2$s$wt)),col="green");legend("topright",legend=c("population","sample","weighted sample"),lty=1,col=c("blue","red","green"))
  plot(density(p3$pop$y),col="blue" ,"random sample wti ~  y");lines(density(p3$s$y),col="red");lines(density(p3$s$y,weights=p3$s$wt/sum(p3$s$wt)),col="green");legend("topright",legend=c("population","sample","weighted sample"),lty=1,col=c("blue","red","green"))

  plot(p4$pop[,c("x","y")],pch=16,cex=.1,col="blue",main="simple random sample, wti ~ 1 , homoskedastic");points(p4$s[,c("x","y")],pch=16,cex=.8,col="red");legend("topleft",legend=c("population","sample"),pch=16,pt.cex=.8,col=c("blue","red"))
  plot(p5$pop[,c("x","y")],pch=16,cex=.1,col="blue",main="random sample, wti ~ some x, homoskedastic");points(p5$s[,c("x","y")],pch=16,cex=.8,col="red");legend("topleft",legend=c("population","sample"),pch=16,pt.cex=.8,col=c("blue","red"))
  plot(p6$pop[,c("x","y")],pch=16,cex=.1,col="blue",main="random sample wti ~  y , homoskedastic");points(p6$s[,c("x","y")],pch=16,cex=.8,col="red");legend("topleft",legend=c("population","sample"),pch=16,pt.cex=.8,col=c("blue","red"))

  plot(p7$pop[,c("x","y")],pch=16,cex=.1,col="blue",main="simple random sample, wti ~ 1 , heteroskedastic");points(p7$s[,c("x","y")],pch=16,cex=.8,col="red");legend("topleft",legend=c("population","sample"),pch=16,pt.cex=.8,col=c("blue","red"))
  plot(p8$pop[,c("x","y")],pch=16,cex=.1,col="blue",main="random sample, wti ~ some x, heteroskedastic");points(p8$s[,c("x","y")],pch=16,cex=.8,col="red");legend("topleft",legend=c("population","sample"),pch=16,pt.cex=.8,col=c("blue","red"))
  plot(p9$pop[,c("x","y")],pch=16,cex=.1,col="blue",main="random sample wti ~  y , heteroskedastic");points(p9$s[,c("x","y")],pch=16,cex=.8,col="red");legend("topleft",legend=c("population","sample"),pch=16,pt.cex=.8,col=c("blue","red"))

  plot(p10$pop[,c("str","y")],pch=16,cex=.5,col="blue",main="stratified random sample, wti ~ 1 , homoskedastic");points(p10$s[,c("str","y")],pch=16,cex=.8,col="red");legend("topleft",legend=c("population","sample"),pch=16,pt.cex=.8,col=c("blue","red"))
  plot(p11$pop[,c("str","y")],pch=16,cex=.5,col="blue",main="stratified random sample, wti ~ some x , homoskedastic");points(p11$s[,c("str","y")],pch=16,cex=.8,col="red");legend("topleft",legend=c("population","sample"),pch=16,pt.cex=.8,col=c("blue","red"))
  plot(p12$pop[,c("str","y")],pch=16,cex=.5,col="blue",main="stratified random sample, wti ~ y , homoskedastic");points(p12$s[,c("str","y")],pch=16,cex=.8,col="red");legend("topleft",legend=c("population","sample"),pch=16,pt.cex=.8,col=c("blue","red"))

  plot(p13$pop[,c("str","y")],pch=16,cex=.5,col="blue",main="stratified random sample, wti ~ 1 , heteroskedastic");points(p13$s[,c("str","y")],pch=16,cex=.8,col="red");legend("topleft",legend=c("population","sample"),pch=16,pt.cex=.8,col=c("blue","red"))
  plot(p14$pop[,c("str","y")],pch=16,cex=.5,col="blue",main="stratified random sample, wti ~ some x , heteroskedastic");points(p14$s[,c("str","y")],pch=16,cex=.8,col="red");legend("topleft",legend=c("population","sample"),pch=16,pt.cex=.8,col=c("blue","red"))
  plot(p15$pop[,c("str","y")],pch=16,cex=.5,col="blue",main="stratified random sample, wti ~ y , heteroskedastic");points(p15$s[,c("str","y")],pch=16,cex=.8,col="red");legend("topleft",legend=c("population","sample"),pch=16,pt.cex=.8,col=c("blue","red"))

}

#random sample
if(F){

  p1=pop_test(type=c("random"),wt="equal")
  p2=pop_test(type=c("random"),wt="propx")
  p3=pop_test(type=c("random"),wt="propy")

  ei=rbind(
    # estimate(x=p1$s,resp_nm="y",wt_nm="wt",N=nrow(p1$pop),var_type="asym")
    estimate(x=p3$s,resp_nm="y",wt_nm="wt",N=nrow(p3$pop),var_type="svypkg")
    ,estimate(x=p3$s,resp_nm="y",wt_nm="wt",N=nrow(p3$pop),var_type="asym")
    ,estimate(x=p3$s,resp_nm="y",wt_nm="wt",N=nrow(p3$pop),var_type="bs")
  )
  ei

  test_s=p3$s
  test_s$wt = 2 * p3$s$wt
  p3$s$wt - test_s$wt
  estimate(x=p3$s,resp_nm="y",wt_nm="wt",N=nrow(p3$pop),var_type="asym")
  estimate(x=test_s,resp_nm="y",wt_nm="wt",N=nrow(p3$pop),var_type="asym")

  estimate(x=p3$s,resp_nm="y",wt_nm="wt",N=nrow(p3$pop),var_type="svypkg")
  estimate(x=test_s,resp_nm="y",wt_nm="wt",N=nrow(p3$pop),var_type="svypkg")

  e_avg=apply(ei[,-c(1:7)],2,mean)
  round(e_avg,3)
  sqrt(var(p3$pop$y * p3$pop$wt ) / nrow(p3$s)) / nrow(p3$pop)

  #c(mean(p1$pop$y),mean(p2$pop$y),mean(p3$pop$y))
  #sd(p2$pop$y)/sqrt(nrow(p2$s))

}

#sample y with covariate x
if(F){

  p6=pop_test(type=c("regression"),wt="propy",n=500)

  e6a=estimate(x=p6$s,resp_nm="y",wt_nm="wt",reg_form = as.formula("y ~ x"), pop=data.frame(x=sum(p4$pop$x)) ,N=nrow(p6$pop),type="regression" ,var_type="asym")
  e6b=estimate(x=p6$s,resp_nm="y",wt_nm="wt",reg_form = as.formula("y ~ x"), pop=data.frame(x=sum(p4$pop$x)) ,N=nrow(p6$pop),type="regression" ,var_type="svypkg")
  e6c=estimate(x=p6$s,resp_nm="y",wt_nm="wt",reg_form = as.formula("y ~ x"), pop=data.frame(x=sum(p4$pop$x)) ,N=nrow(p6$pop),type="regression" ,var_type="bs")

  rbind(e6a,e6b,e6c)

  c(reg_mn=e6a$mean, pop_mn=mean(p6$pop$y),  diff_mn = mean(p6$pop$y) - e6a$mean)
  c(reg_mn=e6b$mean, pop_mn=mean(p6$pop$y),  diff_mn = mean(p6$pop$y) - e6b$mean)


  plot()


}

#stratification
if(F){

  p10=pop_test(type=c("stratified"),wt="propy",nstrat=20)
  pop_Ni = aggregate( x ~ str , data=p10$pop , FUN = length)

  e10a=estimate(x=p10$s,resp_nm="y",wt_nm="wt",strata_nm="str", pop = pop_Ni ,N=nrow(p10$pop),type="stratified" ,var_type="asym")
  e10b=estimate(x=p10$s,resp_nm="y",wt_nm="wt",strata_nm="str", pop = pop_Ni ,N=nrow(p10$pop),type="stratified" ,var_type="svypkg")
  #e10c=estimate(x=p10$s,resp_nm="y",wt_nm="wt",strata_nm="str", pop = pop_Ni ,N=nrow(p10$pop),type="stratified" ,var_type="bs")
  c(str_mn=e10a$mean, pop_mn=mean(p10$pop$y),  diff_mn = mean(p10$pop$y) - e10a$mean)
  c(str_mn=e10b$mean, pop_mn=mean(p10$pop$y),  diff_mn = mean(p10$pop$y) - e10b$mean)
}

#compare regression and stratification for various scenarios
if(F){

  res1=list()
  n=35000
  N=10^6
  #equal weights 5 strata
  p10=pop_test(type=c("stratified"),wt="equal",nstrat=5,n=n,N=N)
  pop_Ni = aggregate( x ~ str , data=p10$pop , FUN = length)
  res1[[1]]=estimate(x=p10$s,resp_nm="y",wt_nm="wt",strata_nm="str", pop = pop_Ni ,N=nrow(p10$pop),type="stratified" ,var_type="svypkg")
  res1[[2]]=estimate(x=p10$s,resp_nm="y",wt_nm="wt",reg_form = as.formula("y ~ x"), pop=data.frame(x=sum(p10$pop$x)) ,N=nrow(p10$pop),type="regression" ,var_type="svypkg")

  #equal weights 10 strata
  p10=pop_test(type=c("stratified"),wt="equal",nstrat=10,n=n,N=N)
  pop_Ni = aggregate( x ~ str , data=p10$pop , FUN = length)
  res1[[3]]=estimate(x=p10$s,resp_nm="y",wt_nm="wt",strata_nm="str", pop = pop_Ni ,N=nrow(p10$pop),type="stratified" ,var_type="svypkg")
  res1[[4]]=estimate(x=p10$s,resp_nm="y",wt_nm="wt",reg_form = as.formula("y ~ x"), pop=data.frame(x=sum(p10$pop$x)) ,N=nrow(p10$pop),type="regression" ,var_type="svypkg")


  #equal weights 30 strata
  p10=pop_test(type=c("stratified"),wt="equal",nstrat=30,n=n,N=N)
  pop_Ni = aggregate( x ~ str , data=p10$pop , FUN = length)
  res1[[5]]=estimate(x=p10$s,resp_nm="y",wt_nm="wt",strata_nm="str", pop = pop_Ni ,N=nrow(p10$pop),type="stratified" ,var_type="svypkg")
  res1[[6]]=estimate(x=p10$s,resp_nm="y",wt_nm="wt",reg_form = as.formula("y ~ x"), pop=data.frame(x=sum(p10$pop$x)) ,N=nrow(p10$pop),type="regression" ,var_type="svypkg")


  #propx weights 5 strata
  p10=pop_test(type=c("stratified"),wt="propx",nstrat=5,n=n,N=N)
  pop_Ni = aggregate( x ~ str , data=p10$pop , FUN = length)
  res1[[7]]=estimate(x=p10$s,resp_nm="y",wt_nm="wt",strata_nm="str", pop = pop_Ni ,N=nrow(p10$pop),type="stratified" ,var_type="svypkg")
  res1[[8]]=estimate(x=p10$s,resp_nm="y",wt_nm="wt",reg_form = as.formula("y ~ x"), pop=data.frame(x=sum(p10$pop$x)) ,N=nrow(p10$pop),type="regression" ,var_type="svypkg")

  #propx  weights 10 strata
  p10=pop_test(type=c("stratified"),wt="propx",nstrat=10,n=n,N=N)
  pop_Ni = aggregate( x ~ str , data=p10$pop , FUN = length)
  res1[[9]]=estimate(x=p10$s,resp_nm="y",wt_nm="wt",strata_nm="str", pop = pop_Ni ,N=nrow(p10$pop),type="stratified" ,var_type="svypkg")
  res1[[10]]=estimate(x=p10$s,resp_nm="y",wt_nm="wt",reg_form = as.formula("y ~ x"), pop=data.frame(x=sum(p10$pop$x)) ,N=nrow(p10$pop),type="regression" ,var_type="svypkg")


  #propx weights 30 strata
  p10=pop_test(type=c("stratified"),wt="propx",nstrat=30,n=n,N=N)
  pop_Ni = aggregate( x ~ str , data=p10$pop , FUN = length)
  res1[[11]]=estimate(x=p10$s,resp_nm="y",wt_nm="wt",strata_nm="str", pop = pop_Ni ,N=nrow(p10$pop),type="stratified" ,var_type="svypkg")
  res1[[12]]=estimate(x=p10$s,resp_nm="y",wt_nm="wt",reg_form = as.formula("y ~ x"), pop=data.frame(x=sum(p10$pop$x)) ,N=nrow(p10$pop),type="regression" ,var_type="svypkg")


  #propx weights 30 strata
  p10=pop_test(type=c("stratified"),wt="propx",nstrat=40,n=n,N=N)
  pop_Ni = aggregate( x ~ str , data=p10$pop , FUN = length)
  res1[[13]]=estimate(x=p10$s,resp_nm="y",wt_nm="wt",strata_nm="str", pop = pop_Ni ,N=nrow(p10$pop),type="stratified" ,var_type="svypkg")
  res1[[14]]=estimate(x=p10$s,resp_nm="y",wt_nm="wt",reg_form = as.formula("y ~ x"), pop=data.frame(x=sum(p10$pop$x)) ,N=nrow(p10$pop),type="regression" ,var_type="svypkg")


  #view results
  res2=rbind.fill(res1)
  print(
    cbind(res2[,c("type","n_str","n")],round(res2[,c("mean","se_m")],1)
        ,res2[,c("rsq"),drop=F]
        ,round(res2[,c("deffinv"),drop=F],1)
  )
  )
  mean(p10$pop$y)

}
