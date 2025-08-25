#'@name make_strata
#'@rdname make_strata
#'@title Make and assign strata based on two input columns
#'
#'@description
#'  Make and assign strata based on two input attributes and some parameters.
#'  Functions are make_strata and assign_strata.
#'
#'@details
#'  <Delete and Replace>
#'
#'\cr
#'Revision History
#' \tabular{ll}{
#'1.0 \tab 2019 01 24 created \cr
#'1.1 \tab 2020 06 25 updated to allow more complex inputs \cr
#'1.2 \tab 2025 08 25 repair help file for assign_strata \cr
#'}
#'
#'@author
#'
#'Jacob Strunk <jacob@@some.domain>
#'
#'@param data input data
#'@param x1 name of input x field
#'@param x2 name of input y field
#'@param split_x1 how to split - equal interval or quantile based
#'@param split_x2 how to split - equal interval or quantile based
#'@param  nest_x2  should x2 be nested in x1, or should x1 and x2 be split independently
#'@param  n1  number of strata for x1
#'@param  n2  number of strata for x2
#'@param  type_x1  is x1 numeric or factor
#'@param  type_x2 is x2 numeric or factor
#'@param  min_recs what is the minimum number of records in a stratum before collapse
#'@param  precision  what precision is retained for cutting numeric values into strata
#'
#'
#'@return
#'  make_strata returns a data.frame of strata boundaries
#'  assign_strata takes a data.frame and appends a column with the stratum name for each record
#'
#'@examples
#'
#'
#'
#'       #prepare pseudo data
#'       n=500
#'       hts = sample(1:150,n,T)
#'       dbhs = abs(hts/5 + rnorm(#' )*2)
#'       dat_test = data.frame(height = hts , dbh = dbhs, height_cat = cut(hts,10) , dbh_cat = cut(dbhs,10))
#'       plot(dbhs,hts)
#'
#'       #example 1 factor and numeric
#'       str_test = make_strata(
#'         dat_test
#'         , x1="height_cat"
#'         , x2="dbh"
#'         , split_x1 =c("qt","eq")[1]
#'         , split_x2 =c("qt","eq")[1]
#'         , nest_x2 =  T
#'         , n1 = 10
#'         , n2 = 10
#'         , type_x1 = "factor"
#'         , type_x2 = "numeric"
#'         , min_recs = 7
#'         , precision = 0
#'       )
#'
#'
#'       res = assign_strata(
#'        str_test
#'         ,dat_test
#'       )
#'
#'       res$stratum
#'
#'       summary(lm(height ~ dbh , data = res))
#'       summary(lm(height ~ dbh_cat , data = res))
#'       summary(lm(height ~ stratum , data = res))
#'
#'       #example 2 numeric and numeric
#'       str_test1 = make_strata(
#'         dat_test
#'         , x1="height"
#'         , x2="dbh"
#'         , split_x1 =c("qt","eq")[1]
#'         , split_x2 =c("qt","eq")[1]
#'         , nest_x2 =  T
#'         , n1 = 10
#'         , n2 = 10
#'         , type_x1 = "numeric"
#'         , type_x2 = "numeric"
#'         , min_recs = 7
#'         , precision = 0
#'       )
#'
#'       res1 = assign_strata(
#'         str_test1
#'         ,dat_test
#'       )
#'
#'       #example 3 numeric and factor
#'       str_test2 = make_strata(
#'         dat_test
#'         , x1="height"
#'         , x2="dbh_cat"
#'         , split_x1 =c("qt","eq")[1]
#'         , split_x2 =c("qt","eq")[1]
#'         , nest_x2 =  T
#'         , n1 = 10
#'         , n2 = 10
#'         , type_x1 = "numeric"
#'         , type_x2 = "factor"
#'         , min_recs = 7
#'         , precision = 0
#'       )
#'
#'       res2 = assign_strata(
#'         str_test2
#'         ,dat_test
#'       )
#'
#'
#'
#'@export
#
#@seealso \code{\link{another_function}}\cr \code{\link{yet_another_function}}\cr

#Desired upgrades to this function:
#
#

# x = function(x){}

#copy function arguments and use this code to format arguments
##writeClipboard(paste(gsub("^[[:space:]]*[,]*","#'@param ",gsub("=.*"," ?",readClipboard())),collapse="\n"))



#function takes a factor at the first level and then cuts based upon
# a numeric vector as the second level
make_strata = function(
  data
  , x1="PlotStrata1"
  , x2="Elev.P95"
  , split_x1=c("qt","eq")
  , split_x2=c("qt","eq")
  , nest_x2 = T
  , n1 = 10
  , n2 = 10
  , type_x1 = c("factor","numeric")[1]
  , type_x2 = c("numeric","factor")[1]
  , min_recs = 7
  , precision = 0
  , collapse=T
){

	x1_in = x1

	if( !type_x1 == "numeric"){

	  str_levels_x1 = levels(as.factor(data[,x1]))
	  dfStr0 = data.frame(stratum_x1 = str_levels_x1, x1.from=NA , x1.to=NA)

	}
	if( type_x1 == "numeric"){

	  dfStr0 = read.csv(text=paste("stratum_x1,x1.from,x1.to,n_x1"))

		r_x1 = range(data[,x1])
		dr_x1 = diff(r_x1)

		if(split_x1[1] == "qt" ){

			str_levels_x1 = c(
				r_x1[1]-dr_x1
				,quantile(data[,x1] ,seq(1/n1,1-1/n1,1/n1))
				,r_x1[1]+dr_x1
			)
		}
		if(split_x1[1] == "eq" ){
			str_levels_x1 = c(
				r_x1[1] - dr_x1
				,seq(r_x1[1],r_x1[2],dr_x1/n1)[-c(1,n1)]
				,r_x1[1]+ dr_x1
			)
		}

		#prep description of x1 stratum
		str_levels_x1 = unique(str_levels_x1)
		str_levels_x1[-c(1,length(str_levels_x1))] = round(str_levels_x1[-c(1,length(str_levels_x1))],precision)
		str_levels_x1 = unique(str_levels_x1)

		#prepare new factor version of attribute
		x1_in = paste(x1,"_ctg",sep="")
		data[,x1_in] = cut(data[,x1] , str_levels_x1 , labels = 1:(length(str_levels_x1)-1) )

		#assign bins
		dfStr0[1:(length(str_levels_x1)-1),"stratum_x1"] = 1:(length(str_levels_x1)-1)
		dfStr0[, "x1.from"] = str_levels_x1[-length(str_levels_x1)]
		dfStr0[, "x1.to"] = str_levels_x1[-1]
		#dfStr0[, "n_x1"] = table(data[,c(x1_in)])
	}



  if( nest_x2 & type_x2 == "numeric"){

    spl1 = split(data,data[,x1_in])

    if( type_x2 == "numeric"){

      for(i in 1:length(spl1)){

        dfStri = read.csv(text=paste("stratum,stratum_x1,stratum_x2,x2.from,x2.to"))

        dati = spl1[[i]]

        if(nrow(dati) / n2 < min_recs ) n2i = max(floor(nrow(dati) / min_recs),1)
        else n2i = n2

        ri = range(dati[,x2])
        dri = diff(ri)

        if(split_x2[1] == "qt" ){

          str_levels_x2 = c(
            ri[1]-dri
            ,quantile(dati[,x2] ,seq(1/n2i,1-1/n2i,1/n2i))
            ,ri[2]+dri
          )
        }
        if(split_x2[1] == "eq" ){
          str_levels_x2 = c(
            ri[1]-dri
            ,seq(ri[1],ri[2],dri/n2i)[-c(1,n2i)]
            ,ri[2]+dri
          )

        }

        #prep description of x2 stratum
        str_levels_x2 = unique(str_levels_x2)
        str_levels_x2[-c(1,length(str_levels_x2))] = round(str_levels_x2[-c(1,length(str_levels_x2))],precision)
        str_levels_x2 = unique(str_levels_x2)

        dfStri[1:(length(str_levels_x2)-1),"stratum_x2"] = 1:(length(str_levels_x2)-1)
        dfStri[, "stratum_x1" ] =  dati[1,x1_in]
        dfStri[, "x2.from"] = str_levels_x2[-length(str_levels_x2)]
        dfStri[, "x2.to"] = str_levels_x2[-1]
        dfStri[,"stratum"] = apply(dfStri[,c("stratum_x1","stratum_x2")],1,paste,collapse=".")

        df_ct = as.data.frame(table(as.factor(cut(dati[,x2], round(str_levels_x2,precision) , labels = F ))))
        if(i == 1) dfStr1 = merge(x=dfStri,df_ct,by.x="stratum_x2",by.y="Var1",all=T)
        if(i > 1) dfStr1 = plyr::rbind.fill(dfStr1, merge(x=dfStri,df_ct,by.x="stratum_x2",by.y="Var1",all=T))
      }

    }
  }
	if(!nest_x2 & type_x2 == "numeric"){

	  dfStr1 = read.csv(text=paste("stratum_x2,x2.from,x2.to,n_x2"))

	  r_x2 = range(data[,x2])
	  dr_x2 = diff(r_x2)

	  if(split_x2[1] == "qt" ){

	    str_levels_x2 = c(
	      r_x2[1]-dr_x2
	      ,quantile(data[,x2] ,seq(1/n1,1-1/n1,1/n1))
	      ,r_x2[1]+dr_x2
	    )
	  }
	  if(split_x2[1] == "eq" ){
	    str_levels_x2 = c(
	      r_x2[1] - dr_x2
	      ,seq(r_x2[1],r_x2[2],dr_x2/n1)[-c(1,n1)]
	      ,r_x2[1]+ dr_x2
	    )
	  }

	  #prep description of x2 stratum
	  str_levels_x2 = unique(str_levels_x2)
	  str_levels_x2[-c(1,length(str_levels_x2))] = round(str_levels_x2[-c(1,length(str_levels_x2))],precision)
	  str_levels_x2 = unique(str_levels_x2)

	  #prepare new factor version of attribute
	  x2_in = paste(x2,"_ctg",sep="")
	  data[,x2_in] = cut(data[,x2] , str_levels_x2 , labels = 1:(length(str_levels_x2)-1) )

	  #assign bins
	  str_levels_x1b = levels(as.factor(dfStr0[,"stratum_x1"]))
	  str_levels_x2b = levels(as.factor(data[,x2_in]))
	  dfStr1 = data.frame(stratum=NA,stratum_x1 = sort(rep(str_levels_x1b,length(str_levels_x2b))) , stratum_x2 = rep(str_levels_x2b , length(str_levels_x1b))  )
	  dfStr1[,"stratum"] = gsub(" ","",apply(dfStr1[,c("stratum_x1","stratum_x2")],1,paste,collapse="."))
	  dfStr1[, "x2.from"] = rep(str_levels_x2[-length(str_levels_x2)],length(str_levels_x1b))
	  dfStr1[, "x2.to"] = rep(str_levels_x2[-1],length(str_levels_x1b))

	}

	if( type_x2 == "factor"){

	  str_levels_x1b = levels(as.factor(dfStr0[,"stratum_x1"]))
	  str_levels_x2 = levels(as.factor(data[,x2]))
	  dfStr1 = data.frame(stratum=NA,stratum_x1 = sort(rep(str_levels_x1b,length(str_levels_x2))) , stratum_x2 = rep(str_levels_x2 , length(str_levels_x1b))  )
	  dfStr1[,"stratum"] = gsub(" ","",apply(dfStr1[,c("stratum_x1","stratum_x2")],1,paste,collapse="."))
	  dfStr1[,"x2.from"] = NA
	  dfStr1[,"x2.to"] = NA

	}

	#prep final data
  dfStr2 = merge(x=dfStr0, dfStr1 , by = 'stratum_x1',all.y=T)
  dfStr2[, "nm_x1"] = x1
  dfStr2[, "nm_x2"] = x2

  #collapse strata ?
  if(collapse){
    pdstrat_in = assign_strata(dfStr2,data[,c(x1,x2)])$stratum
    tbtest = table(data$strat_in)
    nmtest = names(tbtest)
    missing_idx = !dfStr2$stratum %in% pdstrat_in

    if( sum(missing_idx) > 0 ){

      warning("currently 'collapse=T' uses a cheap fix - eventually add something smarter")

      idx_bad = which(missing_idx)
      idx_update = which(missing_idx) - 1
      idx_update1 = which(missing_idx) + 1
      idx_update[idx_update < 1] = idx_update1[idx_update < 1]
      idx_update[idx_update %in% idx_bad] = idx_update1[idx_update %in% idx_bad]
      dfStr2[idx_update,c("x1.to","x2.to")] = dfStr2[idx_bad,c("x1.to","x2.to")]
      dfStr2 = dfStr2[-idx_bad,]


      #eventually add something smarter:
      if(type_x1 == "numeric" & type_x2 == "numeric"){



      }
      if(type_x1 != "numeric" & type_x2 == "numeric"){


      }
      if(type_x1 == "numeric" & type_x2 != "numeric"){


      }

    }

  }
  return( dfStr2[,c('stratum','stratum_x1','stratum_x2','x1.from','x1.to','x2.from','x2.to',"nm_x1","nm_x2")])

}

##' @rdname make_strata
##' @export
assign_strata = function(strata,data){

  x1numeric = !is.na(strata[1,"x1.from"])
  x2numeric = !is.na(strata[1,"x2.from"])

  for(i in 1:nrow(strata)){

    #two numeric inputs
    if(x1numeric & x2numeric){
      id_i =
        ( data[,strata[i,"nm_x1"]] > strata[i,"x1.from"] ) &
        ( data[,strata[i,"nm_x1"]] <= strata[i,"x1.to"] ) &
        ( data[,strata[i,"nm_x2"]] > strata[i,"x2.from"] ) &
        ( data[,strata[i,"nm_x2"]] <= strata[i,"x2.to"] )
      id_i[is.na(id_i)] = F
      data[ id_i , "stratum" ] = strata[ i , "stratum" ]
    }
    #numeric x1, factor x2
    if(x1numeric & !x2numeric){
      id_i =
        ( data[,strata[i,"nm_x2"]] == strata[i,"stratum_x2"] ) &
        ( data[,strata[i,"nm_x1"]] > strata[i,"x1.from"] ) &
        ( data[,strata[i,"nm_x1"]] <= strata[i,"x1.to"] )
      id_i[is.na(id_i)] = F
      data[ id_i , "stratum" ] = strata[ i , "stratum" ]
    }
    #numeric x2, factor x1
    if(!x1numeric & x2numeric){
      id_i =
      	( data[,strata[i,"nm_x1"]] == strata[i,"stratum_x1"] ) &
        ( data[,strata[i,"nm_x2"]] > strata[i,"x2.from"] ) &
        ( data[,strata[i,"nm_x2"]] <= strata[i,"x2.to"] )
      id_i[is.na(id_i)] = F
      data[ id_i , "stratum" ] = strata[ i , "stratum" ]
    }
  }

  data

}


if(F){
#stratify on vegetation type and height

		n=500
		hts = sample(1:150,n,T)
		dbhs = abs(hts/5 + rnorm(n)*2)
		dat_test = data.frame(height = hts , dbh = dbhs, height_cat = cut(hts,10) , dbh_cat = cut(dbhs,10))

		plot(dbhs,hts)

    str_test = make_strata(
    	dat_test
      , x1="height_cat"
      , x2="dbh"
    	, split_x1 =c("qt","eq")[1]
      , split_x2 =c("qt","eq")[1]
      , nest_x2 =  T
      , n1 = 10
      , n2 = 10
      , type_x1 = "factor"
      , type_x2 = "numeric"
      , min_recs = 7
      , precision = 0
    )


    res = assign_strata(
      str_test
      ,dat_test
    )

    res$stratum

    summary(lm(height ~ dbh , data = res))
    summary(lm(height ~ dbh_cat , data = res))
    summary(lm(height ~ stratum , data = res))

    str_test1 = make_strata(
    	dat_test
    	, x1="height"
    	, x2="dbh"
    	, split_x1 =c("qt","eq")[1]
    	, split_x2 =c("qt","eq")[1]
    	, nest_x2 =  T
    	, n1 = 10
    	, n2 = 10
    	, type_x1 = "numeric"
    	, type_x2 = "numeric"
    	, min_recs = 7
    	, precision = 0
    )

    res1 = assign_strata(
    	str_test1
    	,dat_test
    )

    str_test2 = make_strata(
      dat_test
      , x1="height"
      , x2="dbh_cat"
      , split_x1 =c("qt","eq")[1]
      , split_x2 =c("qt","eq")[1]
      , nest_x2 =  T
      , n1 = 10
      , n2 = 10
      , type_x1 = "numeric"
      , type_x2 = "factor"
      , min_recs = 7
      , precision = 0
    )

    res2 = assign_strata(
      str_test2
      ,dat_test
    )

}
