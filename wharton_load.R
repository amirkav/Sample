source('~/_My_Academics__General/R/R_Projects/Wharton/R_Codes/wharton_clean.R')
source('~/_My_Academics__General/R/R_Projects/Wharton/R_Codes/wharton_func.R')
require(RMySQL)
#library(RColorBrewer)
# baseDir = 'C:/Users/amir/Documents/_My_Academics__General/R/R_Projects/Wharton/R_Codes/'
setwd("C:/Users/amir/Documents/_My_Academics__General/R/R_Projects/Wharton"); work.fold <- "~/Documents/_My_Academics__General/R/R_Projects/Wharton"
wharton.wkfl <- "C:/Users/amir/Documents/_My_Academics__General/R/R_Projects/Wharton"

### Download Census data tables
# acs.econ <- read.csv("C:/Users/amir/Documents/_My_Academics__General/R/R_Projects/PGE/Data/ACS_11_5YR_DP03.csv")
# acs.housing <- read.csv("C:/Users/amir/Documents/_My_Academics__General/R/R_Projects/PGE/Data/ACS_11_5YR_DP04.csv")
# acs.social <- read.csv("C:/Users/amir/Documents/_My_Academics__General/R/R_Projects/PGE/Data/ACS_11_5YR_DP02.csv")
# acs.median.income <- read.csv("C:/Users/amir/Documents/_My_Academics__General/R/R_Projects/PGE/Data/ACS_11_5YR_B19326_with_ann.csv", as.is=T)
# acs.utility.payment <- read.csv("C:/Users/amir/Documents/_My_Academics__General/R/R_Projects/PGE/Data/ACS_11_5YR_B25069.csv")  

### Connect to MySQL database on Amazon AWS
db="pgefinal"
verbose=F
con <- dbConnect(dbDriver("MySQL"), 
                 host = "emacdb.c07mqwpylqtq.us-west-1.rds.amazonaws.com",
                 user = "amirk", password = "wharton20i3pg3", 
                 dbname = "pgefinal")
(tabs.list = dbListTables(con))
dbDisconnect(con)

# if AWS is down, use these codes to read the data from file
#   if(is.null(cons15min) || is.null(zip.weather)) {
#     print("Connection error. Reading from disc.")
#     cons15min <- read.csv(paste("C:/Users/amir/Documents/_My_Academics__General/R/R_Projects/Wharton/Data/15minCons/", zip, "kwh.csv", sep=""))
#     ids <- cons15min$sp_id
#     id <- ids[i]; print(id)  
#     zip.weather <- read.csv(paste("C:/Users/amir/Documents/_My_Academics__General/R/R_Projects/Wharton/Data/Weather/", zip, "weather.csv", sep=""))
#   }
###############################################################################
# opens a database
conf.dbCon = function(db='pgefinal') {
  if (is.null(db)) { db='pgefinal' }
  #con = dbConnect(MySQL(),default.file=paste(baseDir,'RDS_emacdb.cfg',sep=''),dbname="pgefinal")
  con <- dbConnect(dbDriver("MySQL"), 
                   host = "emacdb.c07mqwpylqtq.us-west-1.rds.amazonaws.com",
                   user = "amirk", password = "wharton20i3pg3", 
                   dbname = "pgefinal")
}

###############################################################################
# utility fn to close/clear all active db connections
clearCons = function() {
  all_cons <- dbListConnections(MySQL())
  for(con in all_cons) {
    res <- dbListResults(con)
    if(length(res)>0) { 
      dbClearResult(res[[1]]) 
      rm(res)
    }
    dbDisconnect(con) 
  }
}

###############################################################################
# use this when too many cons are open
showCons = function() {
  all_cons <- dbListConnections(MySQL())
  print(dim(all_cons))
  print(all_cons)
  s = dbGetQuery(all_cons[[1]], "show processlist") 
  print(s)
}

###############################################################################
# runs a query on the desired database
run.query = function(query,db=NULL) {
  if(verbose) { print(query) }  # verbose is an option set by user whether they want R to declare the process or be silent
  data <- c()
  tryCatch({
    con  <- conf.dbCon(db)
    res  <- dbGetQuery(con, query)
    if(length(res)>0) data  <- res
    
  },
           error = function(e) {print(e)},
           finally = {
             # close the results set if necessary
             resultSet <- dbListResults(con)
             if(length(resultSet)>0) { 
               dbClearResult(resultSet[[1]])
               rm(resultSet)
             }
             dbDisconnect(con)
             rm(con)
           } )
  return(data)
}

###############################################################################
# utility function that returns a list of all the zipcodes in the data set
db.getZips = function() {
  query    <- 'select distinct POSTAL_CODE as zip5 from 02premise order by POSTAL_CODE'
  return(run.query(query))
}

###############################################################################
# returns a list of service point ids in a given zipcode
db.getSPs = function(zip=NA) {
  zipQuery = 'SELECT 01customer.sp_id AS sp_id
  FROM 02premise INNER JOIN 01customer
  ON 01customer.premise_id = 02premise.premise_id 
  WHERE 02premise.postal_code='
  if(is.na(zip)) { query <- 'select distinct sp_id from 01customer' }
  else {           query <- paste(zipQuery,zip) }
  return(run.query(query))
}

###############################################################################
# returns 15-min consumption data for all users in a given zipcode
db.getAllData = function(zip=NULL) {
  where = ''
  if(!is.null(zip)) { where = paste('WHERE 02premise.POSTAL_CODE=',zip) }
  query = paste(
    'SELECT electric_interval_96.sp_id, `DATE`,',
    'w01,w02,w03,w04,w05,w06,w07,w08,w09,w10,w11,w12,w13,w14,w15,w16,w17,w18,w19,w20,w21,w22,w23,w24,',
    'w25,w26,w27,w28,w29,w30,w31,w32,w33,w34,w35,w36,w37,w38,w39,w40,w41,w42,w43,w44,w45,w46,w47,w48,',
    'w49,w50,w51,w52,w53,w54,w55,w56,w57,w58,w59,w60,w61,w62,w63,w64,w65,w66,w67,w68,w69,w70,w71,w72,',
    'w73,w74,w75,w76,w77,w78,w79,w80,w81,w82,w83,w84,w85,w86,w87,w88,w89,w90,w91,w92,w93,w94,w95,w96',
    'FROM 02premise INNER JOIN 01customer',
    'ON 01customer.premise_id = 02premise.premise_id', 
    'RIGHT JOIN electric_interval_96 ON electric_interval_96.sp_id = 01customer.sp_id',
    where,
    'ORDER BY sp_id, DATE')
  return(run.query(query))
}

###############################################################################
# returns elec cons data for a given user
db.getIntervalData = function(sp_id) {
  query = paste(
    'SELECT sp_id, `DATE`,',
    'w01,w02,w03,w04,w05,w06,w07,w08,w09,w10,w11,w12,w13,w14,w15,w16,w17,w18,w19,w20,w21,w22,w23,w24,',
    'w25,w26,w27,w28,w29,w30,w31,w32,w33,w34,w35,w36,w37,w38,w39,w40,w41,w42,w43,w44,w45,w46,w47,w48,',
    'w49,w50,w51,w52,w53,w54,w55,w56,w57,w58,w59,w60,w61,w62,w63,w64,w65,w66,w67,w68,w69,w70,w71,w72,',
    'w73,w74,w75,w76,w77,w78,w79,w80,w81,w82,w83,w84,w85,w86,w87,w88,w89,w90,w91,w92,w93,w94,w95,w96',
    'FROM electric_interval_96 where sp_id=',sp_id,
    'ORDER BY DATE')
  return(run.query(query))
}

###############################################################################
# returns elec cons data for a given user
db.getGasData = function(sp_id) {
  query = paste(
    'SELECT SP_ID, date, customer_type, therms, q_read',
    'FROM gas_daily where sp_id=',sp_id,
    'ORDER BY DATE')
  return(run.query(query))
}

###############################################################################
# returns hourly weather data for a given zip
db.getWeatherData = function(zip) {
  query = paste(
    'SELECT `date`, TemperatureF, Pressure, DewpointF, HourlyPrecip
    FROM weather_60 where zip5 =',zip,'ORDER BY DATE')
  return(run.query(query))
}

###############################################################################
# returns the zipcode for a given user
db.getZipForId = function(sp_id) {
  query = paste (
    'SELECT 02premise.postal_code AS zip5',
    'FROM 02premise INNER JOIN 01customer',
    'ON 01customer.premise_id = 02premise.premise_id', 
    'WHERE 01customer.sp_id=',sp_id)
  return(run.query(query))
}

###############################################################################
# return the available summary information for every zipcode
# including sp_id count, climate zone, weather station, and income stats
db.getZipData = function(zip=NULL) {
  where = ""
  if(!is.null(zip)) { where = paste('where zip5=',zip) }
  query = paste(
    'SELECT zip5, COUNT(DISTINCT sp_id), cecclmzn, climate, GCOUNTY, WTHRSTN,
    median_income, median_income_quantiles
    FROM', conf.accountTable(), where, 'GROUP BY zip5')
  print(query)
  return(run.query(query,conf.meterDB()))
}

#print(run.query('show tables'))


###############################################################################
# Functions: auiliary functions for configuring commonly-used databases
conf.weatherDB        = function()    { db = 'pge_res'}
conf.meterDB          = function()    { db = 'pge_res'}
conf.meterTable       = function(zip) { table = paste('pge_res_60_',zip,sep='')}
conf.weatherTable     = function()    { table = 'weather_60'}
conf.accountTable     = function()    { table = 'pge_res_final'}


###############################################################################
### Function: (one-time only) writes important zipcodes data to disc for use when AWS is not available
# List of zipcodes with the largest numnber of residences in each climate zone:
# 93308 (Bakersfield), 93722 (Fresno), 94062 (Woodside), 94109 (San Francisco),
# 94565 (Bay Point), 95035 (Milpitas), 95501 (Eureka), 95667 (Placerville), 
# 95959 (Nevada City), 95971 (Quincy), 
writeBackup <- function(zip.repr=NULL) {
  if (zip.repr == NULL) zip.repr <- c(93308, 93722, 94062, 94109, 94565, 95035, 95501, 95667, 95959, 95971)
  for (zip in zip.repr) {
    zip.cons <- db.getAllData(zip)
    write.csv(zip.cons, paste("C:/Users/amir/Documents/_My_Academics__General/R/R_Projects/Wharton/Data/15minCons/", zip,"kwh.csv", sep=""), row.names=F)
    weather <- db.getWeatherData(zip)
    write.csv(weather, paste("C:/Users/amir/Documents/_My_Academics__General/R/R_Projects/Wharton/Data/Weather/", zip,"weather.csv", sep=""), row.names=F)
  }
}

