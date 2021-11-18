library(plyr)

#download portal webpage with chrome...
setwd("D:/Box Sync/sync/R/help_scripts/download_webpage")
html0 = readLines("Washington Lidar Portal.html")
body = html0[[25]]

#initial prep, split by project
from = as.numeric(gregexpr("<li class=\"project-control-item\"",body,fixed=T)[[1]])
to = as.numeric(gregexpr("</ul></li>",body,fixed=T)[[1]])
datasets = mapply(function(x,from,to)substring(x,from,to),from = from, to = to,MoreArgs = list(x = body))

#parse geology website into links
fn_parse = function(x){
	
	x_in = x
	from_nm = as.numeric(regexpr(">",x_in)) + 1
	to_nm = as.numeric(regexpr("<ul",x_in)) - 1
	nm = substring(x_in,from_nm,to_nm)
	
	from_dtm1 = as.numeric(regexpr("DTM</a>",x_in,fixed = T))
	dtm_lnk1 = substring(x_in,from_dtm1)
	from_dtm2 = as.numeric(regexpr("http://lidarportal.dnr.wa.gov/download?ids=",dtm_lnk1,fixed = T))
	to_dtm2 = as.numeric(regexpr(" title=",dtm_lnk1,fixed = T))-2
	dtm_lnk2 = substring(dtm_lnk1,from_dtm2,to_dtm2)
	
	data.frame(project = nm, dtm_link = dtm_lnk2,stringsAsFactors = F)
	
}

df_dtmlnks =  rbind.fill(lapply(datasets,fn_parse))


#download files
out = "g:\\data\\wa_lidar_portal_dtm"

for(i in 1:nrow(df_dtmlnks)){
	
	proji = gsub("[ ]","_",df_dtmlnks[i,"project"])
	outi = file.path(out,proji)
	if(!dir.exists(outi)) dir.create(outi)
	outf = file.path(outi,paste(proji,"_DTM.zip",sep=""))
	print(paste(format(Sys.time()),"start:",outf))
	if(!file.exists(outf)) download.file(df_dtmlnks[i,"dtm_link"],outf,quiet=T)
	print(paste(format(Sys.time()),"end:",outf))
}



