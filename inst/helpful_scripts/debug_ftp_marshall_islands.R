
getURI3=function(url1){
  
  library(RCurl,quietly = T)
  library(data.table,quietly = T)
  
  curl = getCurlHandle() 
  curlSetOpt(.opts=list(forbid.reuse=1),curl=curl) 
  folders_txt=sapply(url1,getURI, curl = curl) 

  #getCurlInfo(curl) # I skipped this step 
  rm(curl) # release the curl! 
  gc() # release the curl! 


  #prep / return data
  df_list=mapply(function(x,y){z=read.table(text=x,header = F);z$url=as.character(paste(y,z[,"V9",drop=T],"/",sep=""));z},folders_txt,url1,SIMPLIFY=F)
  folders_df=data.table::rbindlist(df_list)
  return(folders_df)
  
}

urls=c("ftp://ftp.ga.gov.au/geodesy-outgoing/gnss/data/highrate/2017/17343/"
  ,"ftp://ftp.ga.gov.au/geodesy-outgoing/gnss/data/highrate/2017/17344/"
  ,"ftp://ftp.ga.gov.au/geodesy-outgoing/gnss/data/highrate/2017/17345/"
  ,"ftp://ftp.ga.gov.au/geodesy-outgoing/gnss/data/highrate/2017/17346/"
)

test=getURI3(urls)


if(F){
  require(curl)
  req <- curl_fetch_memory("https://httpbin.org/redirect/3")
  parse_headers(req$headers)
  parse_headers(req$headers, multiple = TRUE)
  
  require(curl)
  curl=new_handle()
  req <- curl_fetch_memory("ftp://ftp.ga.gov.au/geodesy-outgoing/gnss/data/highrate/2017/17346/", handle = curl)
  read.table(text=parse_headers(req$content))
  parse_headers(req$content, multiple = TRUE)
  rm(curl)
  gc()
  
  
  require(curl)
  pool=new_pool()
  cb <- function(req){req$content}
  
  data <- list()
  success <- function(res){
    cat("Request done! Status:", res$status, "\n")
    data <<- c(data, parse_headers(res$content))
  }
  failure <- function(msg){
    cat("Oh noes! Request failed!", msg, "\n")
  }
  
  curl_fetch_multi(c("ftp://ftp.ga.gov.au/geodesy-outgoing/gnss/data/highrate/2017/17346/"), success, failure, pool = pool)
  curl_fetch_multi(c("ftp://ftp.ga.gov.au/geodesy-outgoing/gnss/data/highrate/2017/17345/"), success, failure, pool = pool)
  curl_fetch_multi(c("ftp://ftp.ga.gov.au/geodesy-outgoing/gnss/data/highrate/2017/17344/"), success, failure, pool = pool)
  req=multi_run(pool=pool)
  rm(pool)
  gc()  
  
  parse_headers(data, multiple = TRUE)

  }