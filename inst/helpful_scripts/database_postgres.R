
#set up database connection
if(!"con_inv" %in% ls()){
 library("RPostgreSQL")
 con_inv=dbConnect(dbDriver("PostgreSQL"),dbname="inventory",host="localhost",port="5432",user="postgres",password="0000")
}
