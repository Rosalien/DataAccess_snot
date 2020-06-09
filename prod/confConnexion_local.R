confConnexion <- function(dbWork){
if(dbWork=="prod"){
  # Configuration de la connexion vers la base
pool <- dbPool(
  drv = dbDriver("PostgreSQL", max.con = 100),
  dbname = "snotbdprod",
  host = "163.9.41.46",
  port = "5432",
  user = "********",
  password = "********",
  idleTimeout = 3600000)
}else if(dbWork=="prod_local"){
pool <- dbPool(
  drv = dbDriver("PostgreSQL", max.con = 100),
  dbname = "snotbdprod",
  host = "127.0.0.1",
  port = "5437",
  user = "********",
  password = "********",
  idleTimeout = 3600000)
}else{
  pool <- dbPool(
  drv = dbDriver("PostgreSQL", max.con = 100),
  dbname = "sno",
  host = "localhost",
  port = "5432",
  user = "snouser",
  password = "sno001",
  idleTimeout = 3600000)  
}
return(pool)
}