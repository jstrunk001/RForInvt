dir_ri = "C:\\Users\\jstrunk\\Box\\sync\\R\\r_packages_dev\\my_packages\\RForInvt\\inst\\extdata\\FIADB_RI.db"
conSQL = DBI::dbConnect( RSQLite::SQLite() , dbname= dir_ri )
  tbs_ri = dbListTables(conSQL)
  sql_drp = paste0("drop table ", grep("^FVS",tbs_ri ,invert=T, value=T) )
  mapply( dbExecute , statement= sql_drp , MoreArgs = list(conn=conSQL) )
dbDisconnect( conSQL )
