require(RMySQL)
library(RColorBrewer)
setwd("~/_My_Academics__General/R/R_Projects/Wharton/R_Codes")


DatesRange <- function() {
  files <- list.files("C:/Users/amir/Documents/_My_Academics__General/R/R_Projects/Wharton/Data/LoadFeatures/")
  #date.ranges <- NULL
  for (f in 482:length(files)) {
    zip <- as.numeric(substr(files[f], 21, 25)); print(zip)
    data <- read.csv(paste("C:/Users/amir/Documents/_My_Academics__General/R/R_Projects/Wharton/Data/LoadFeatures/", files[f], sep=""))
    data$date <- as.Date(data$date)
    date.range <- range(data$date)
    date.ranges <- rbind(date.ranges, cbind.data.frame(zip, date.range[1], date.range[2]))
  }
  write.csv(date.ranges, "C:/Users/amir/Documents/_My_Academics__General/R/R_Projects/Wharton/Data/dbmg_zip-date-ranges10.csv")
}
    
###############################################################################
############################## Data Validation ################################
###############################################################################
### Find long periods of unoccupancy by plotting daily average consumption and temperature
# Inputs:
#   data:         a data frame containing energy consumption data
#   date1, date2: start and finish date of examination, respectively
# Outputs:
#   a plot of daily average electricity, gas, and temperature data 
ZoomInTS <- function(data=gas.elec.weather.merged, date1="2009-05-01", date2="2009-10-01") {
  #   data <- gas.elec.weather.merged
  data <- data[data$date > as.Date(date1) & data$date < as.Date(date2), ]
  elec.cons <- data$daily.total.orig
  gas.cons <- data$therms
  temp <- rowMeans(data[, which(names(gas.elec.weather.merged)=="temp1"):which(names(gas.elec.weather.merged)=="temp24")])
  dates <- data$date
  #   names(data)
  par(mfrow=c(1,1))
  DrawTimeSeries(elec.cons, gas.cons, temp, dates)
}
###############################################################################
# Based on the usage consumption, classifies the building as occupied or unoccupied
FindOccupied <- function(reg.data) {
  reg.data$number <- 1:nrow(reg.data)
  non.outliers <- NULL
  par(mfrow=c(1,1))
  r1 <- 5*floor(range(reg.data$temp)/5)[1]
  r2 <- 5*ceiling(range(reg.data$temp)/5)[2]
  rs <- seq(r1, r2, by=5)
  for (j in 1:(length(rs)-1)) {
    reg.data.inTempRange <- reg.data[(reg.data$temp > rs[j]) & (reg.data$temp < rs[j+1]), ] 
    reg.data.inTempRange.nonOutlier <- reg.data.inTempRange[reg.data.inTempRange$cons > mean(reg.data.inTempRange$cons)-sd(reg.data.inTempRange$cons), ]
    non.outliers <- c(non.outliers, reg.data.inTempRange.nonOutlier$number)
  }
  sort(non.outliers)
}

###############################################################################
### Function: examines the consumption readings for potential issues
# Args:
#   r: a data frame with two columns: kw, dates
# Returns:
#   issues: a data frame with one column for each issue and one row 
#           (send the consumption data one per user)
ValidateConsData = function(r) {
  issues = data.frame(id=999, days180=999, span270=999, bigdiff=999, lowmean=999, zerospct15=999)
  issues$valid <- 1
  if(is.null(r) || nrow(r)==0) {
    issues$valid <- 0
    return(issues)
  }
  issues$id <- unique(r$sp_id)[1]
  r$DATE <- as.Date(r$DATE)
  timeDiffs = diff(r$DATE)
  units(timeDiffs) <- "days"
  maxtd = max(timeDiffs) 
  span = difftime(tail(r$DATE, n=1),r$DATE[1],units='days') + 1
  zerospct = sum((r[,3:98] == 0)*1,na.rm=TRUE) / (nrow(r)*96)
  kwmean = mean(rowMeans(r[,3:98]), na.rm=TRUE)
  daylen = length(r$DATE)
  if( daylen < 180 )     {
    issues$days180    = daylen # less than 180 days (could be non-consecutive)
    issues$valid <- 0 
    #return(valid)
  }
  if( span < 270 )      {
    issues$span270    = span # spanning less than 270 days total
    issues$valid <- 0 
    #return(valid)
  }
  if( !(span == daylen) )      {
    issues$missingDates    = span - daylen # spanning less than 270 days total
  }
  
  if( maxtd > 60 )      {
    issues$bigdiff    = maxtd # more than 2 months of missing data
    issues$valid <- 0 
    #return(valid)
  }
  if( kwmean < 0.110 )  {
    issues$lowmean    = kwmean # mean less than 150W is almost always empty or bad readings
    issues$valid <- 0 
    #return(valid)
  }
  if( zerospct > 0.15 ) {
    issues$zerospct15 = zerospct # over 15% of readings are zero
    issues$valid <- 0 
    #return(valid)
  }
  return(issues)
  #return(valid)
}


###############################################################################
# Function: Reads all important data on users characteristics and saves them to csv files
UserDataTable <- function() {
  zips.summary <- read.csv(paste("C:/Users/amir/Documents/_My_Academics__General/R/R_Projects/Wharton/Data/dbmg_zip_summary.csv",sep=""))
  all.zips <- zips.summary$POSTAL_CODE
  all.users.vars <- NULL  # zip.summ.vars <- zip.hourly.cons.orig <- zip.hourly.cons.orig.norm <- 
  for (z in 1:length(all.zips)) {  # length(all.zips); total 823 zipcodes
    zip <- all.zips[z]; print(zip)
    ids <- db.getSPs(zip)[,1]  # all user ids in a zipcode
    users.in.zip.vars <- NULL
    for (i in 1:length(ids)) {
      id <- ids[i]; print(id)
      user.vars <- NULL
      ### Code for reading all important variables from different tables
      fields <- c("ACCOUNT_ID", "PER_ID", "SERVICE_AGREEMENT_ID", "SP_ID", "PREMISE_ID", "SVC_TYPE_CD")
      query <- paste('select ', toString(fields),  #ACCOUNT_ID, PER_ID, SERVICE_AGREEMENT_ID, SP_ID, PREMISE_ID'
                     'from 01customer', 
                     'WHERE 01customer.sp_id=', id)
      qr.customer <- run.query(query=query, db="pgefinal"); 
      if(is.null(qr.customer)) {
        qr.customer <- data.frame(t(rep(NA, length(fields)))); names(qr.customer) <- fields
      }
      fields <- c("POSTAL_CODE", "CLIMATE_BAND", "NEM_FLAG", "FUEL", "CITY", "COUNTY", "DIVISION_CODE", "AREA", "GEOGRAPHY")
      query <- paste('SELECT ', toString(fields),  #POSTAL_CODE, CLIMATE_BAND, NEM_FLAG' 
                     'FROM 02premise INNER JOIN 01customer',
                     'ON 01customer.premise_id = 02premise.premise_id',
                     'WHERE 01customer.sp_id=', id)
      qr.premise <- run.query(query=query, db="pgefinal"); 
      if(is.null(qr.premise)) {
        qr.premise <- data.frame(t(rep(NA, length(fields)))); names(qr.premise) <- fields
      }
      fields <- c("ELECTRIC_RATE", "GAS_RATE", "TOU_RATE", "ENVIRONMENT_OR_WILDLIFE", "ENVIRONMENTAL_ISSUES", "GREEN_LIVING", "HIGH_TECH_LIVING", "EDUCATION", "Cluster")
      query <- paste('SELECT ', toString(fields),  #TOU_RATE, ENVIRONMENT_OR_WILDLIFE, ENVIRONMENTAL_ISSUES, GREEN_LIVING, HIGH_TECH_LIVING, EDUCATION, Cluster
                     'FROM 03accounts INNER JOIN 01customer',
                     'ON 01customer.premise_id = 03accounts.premise_id',
                     'WHERE 01customer.sp_id=', id)
      qr.accounts <- run.query(query=query, db="pgefinal"); 
      if(is.null(qr.accounts)) {
        qr.accounts <- data.frame(t(rep(NA, length(fields)))); names(qr.accounts) <- fields
      } 
      fields <- c("TIMESENROLLED")
      query <- paste('SELECT ', toString(fields), #TIMESENROLLED
                     'FROM 05programs INNER JOIN 01customer',
                     'ON 01customer.account_id = 05programs.account_id',
                     'WHERE 01customer.sp_id=', id)
      qr.programs <- run.query(query=query, db="pgefinal"); 
      if(is.null(qr.programs)) {
        qr.programs <- data.frame(t(rep(NA, length(fields)))); names(qr.programs) <- fields
      }
      fields <- c("rebate_type")
      query <- paste('SELECT ', toString(fields), #rebate_type
                     'FROM 06rebates INNER JOIN 01customer',
                     'ON 01customer.service_agreement_id = 06rebates.service_agreement_id',
                     'WHERE 01customer.sp_id=', id)
      qr.rebates <- run.query(query=query, db="pgefinal"); 
      if(is.null(qr.rebates)) {
        qr.rebates <- data.frame(t(rep(NA, length(fields)))); names(qr.rebates) <- fields
      }
      # Fill in NAs for second, third, etc lines of sp_id with more than one line for a variable (same as na.pad)     
      mnr <- max(nrow(qr.customer), nrow(qr.premise), nrow(qr.accounts), nrow(qr.programs), nrow(qr.rebates)) 
      if(nrow(qr.customer) < mnr) {
        for (enum in 1:(mnr-nrow(qr.customer))) qr.customer <- rbind(qr.customer, rep(NA, ncol(qr.customer)))
      }
      if(nrow(qr.premise) < mnr) {
        for (enum in 1:(mnr-nrow(qr.premise))) qr.premise <- rbind(qr.premise, rep(NA, ncol(qr.premise)))
      }
      if(nrow(qr.accounts) < mnr) {
        for (enum in 1:(mnr-nrow(qr.accounts))) qr.accounts <- rbind(qr.accounts, rep(NA, ncol(qr.accounts)))
      }
      if(nrow(qr.programs) < mnr) {
        for (enum in 1:(mnr-nrow(qr.programs))) qr.programs <- rbind(qr.programs, rep(NA, ncol(qr.programs)))
      }
      if(nrow(qr.rebates) < mnr) {
        for (enum in 1:(mnr-nrow(qr.rebates))) qr.rebates <- rbind(qr.rebates, rep(NA, ncol(qr.rebates)))
      }
      ### Add user information to overall zip table
      user.vars <- cbind(qr.customer, qr.premise, qr.accounts, qr.programs, qr.rebates)  #, daily.average, daily.min, daily.max
      users.in.zip.vars <- rbind(users.in.zip.vars, user.vars)      
    }
    ### Write important variables to disk (user-level)
    write.csv(users.in.zip.vars, paste("C:/Users/amir/Documents/_My_Academics__General/R/R_Projects/Wharton/Data/UserVars/", zip,"_vars.csv",sep=""))
    all.users.vars <- rbind(all.users.vars, users.in.zip.vars)     
  }
  write.csv(all.users.vars, paste("C:/Users/amir/Documents/_My_Academics__General/R/R_Projects/Wharton/Data/UserVars/all_users_vars_93201-93401.csv",sep=""))
}

###############################################################################
### Function: (for all users in the DB)
#             normalizes user consumption data and writes important load features to disc
#             important features: daily average, daily max, normalized daily average, normalized daily max
#             important features averaged over all days of the experiment for each user
#             important features averaged daily over all users in a zipcode
#             important features averaged over all days of experiment and all users in a zipcode
UserLoadFeatures <- function() {
  con <- conf.dbCon(db="pgefinal")
  zips.summary <- read.csv(paste("C:/Users/amir/Documents/_My_Academics__General/R/R_Projects/Wharton/Data/dbmg_zip_summary.csv",sep=""))
  all.zips <- zips.summary$POSTAL_CODE
  users.alltime.load <- NULL
  for (z in 1:length(all.zips)) {  # length(all.zips); total 823 zipcodes
    zip <- all.zips[z]; print(zip)
    # read cons and weather data from MySQL on AWS
#     zip.weather <- db.getWeatherData(zip)  # hourly data on outside temp, pressure, dewpoint, and precipitation
    zip.weather <- ReadFile(paste("C:/Users/amir/Documents/_My_Academics__General/R/R_Projects/Wharton/Data/Weather/", zip, "weather.csv", sep=""))
    if( is.null(zip.weather) || nrow(zip.weather)==0 ) next  #|| zip.weather.error==1
    tout.matrix <- CalcToutMatrix(zip.weather)
    ids <- db.getSPs(zip)[,1]  # all user ids in a zipcode
    users.in.zip.daily.load <- NULL 
    for (i in 1:length(ids)) {
      id <- ids[i]; print(id)
      cons15min <- db.getIntervalData(id); cons15min[,3:98] <- cons15min[,3:98]*0.00025 # 15-min elec cons data for a given user id
      cons.data.issues <- ValidateConsData(cons15min)
      if(!cons.data.issues$valid) next
      cons15min$DATE <- as.Date(cons15min$DATE); 
      names(cons15min)[which(names(cons15min)=="DATE")] <- "date"
      cons15min <- cons15min[which(cons15min$date %in% unique(as.Date(zip.weather$date))), ]
      hourly.cons.orig <- CalcHourlyCons(cons15min)
      cons.weather.merged <- merge(hourly.cons.orig, tout.matrix, by="date")
      if(nrow(hourly.cons.orig)==0) next
      daily.cons.norm <- NormalizeForWeatherCP(cons.weather.merged, resolution="daily")$fitted.norm  # total daily consumption over average daily temperature
      daily.max.norm <- NormalizeForWeatherCP(cons.weather.merged, resolution="daily.max")$fitted.norm  # max daily cons over max daily temperature
      #hourly.cons.norm <- NormalizeForWeatherCP(cons.weather.merged, resolution="hourly")
      ### Calculating important load features (original values) <<All values in kWh>> 
      # daily central values (one for each user and each day, based on 15-min readings)
      daily.total.orig <- rowSums(cons.weather.merged[,which(names(cons.weather.merged)=="kwh1"):which(names(cons.weather.merged)=="kwh24")])  # total daily consumption
      daily.min.orig <- apply(cons.weather.merged[,which(names(cons.weather.merged)=="kwh1"):which(names(cons.weather.merged)=="kwh24")], 1, min)  # minimum hourly consumption
      daily.max.orig <- apply(cons.weather.merged[,which(names(cons.weather.merged)=="kwh1"):which(names(cons.weather.merged)=="kwh24")], 1, max)  # maximum hourly consumption
      # total central values (one for each user, averaged over the entire period)
      ave.daily.total.orig <- mean(daily.total.orig)
      ave.daily.min.orig <- mean(daily.min.orig)
      ave.daily.max.orig <- mean(daily.max.orig)      
      ### Calculating important load features (normalized values) <<All values in kWh>> 
      ave.daily.total.norm <- mean(daily.cons.norm)
      ave.daily.max.norm <- mean(daily.max.norm)
      ### create tables and write to disk
      user.daily.load <- cbind.data.frame(id, cons.weather.merged$date, daily.total.orig, daily.min.orig, daily.max.orig, daily.cons.norm, daily.max.norm)
      names(user.daily.load)[1:2] <- c("SP_ID", "date")#, "daily.total.orig", "daily.min.orig", "daily.max.orig", "daily.cons.norm", "daily.max.norm")
      users.in.zip.daily.load <- rbind(users.in.zip.daily.load, user.daily.load)
      user.alltime.load <- cbind(id, ave.daily.total.orig, ave.daily.min.orig, ave.daily.max.orig, ave.daily.total.norm, ave.daily.max.norm)
      users.alltime.load <- rbind(users.alltime.load, user.alltime.load)
    }
    write.csv(users.in.zip.daily.load, paste("C:/Users/amir/Documents/_My_Academics__General/R/R_Projects/Wharton/Data/LoadFeatures/daily_load_features_", zip,".csv",sep=""))
  }
  write.csv(users.alltime.load, paste("C:/Users/amir/Documents/_My_Academics__General/R/R_Projects/Wharton/Data/LoadFeatures/all_users_load_features.csv",sep=""))
}

IntegrateLoadFeatureFiles <- function() {
  files <- list.files("C:/Users/amir/Documents/_My_Academics__General/R/R_Projects/Wharton/Data/LoadFeatures")
  users.ave.load.features <- NULL
  for (f in 1:length(files)) {
    zip <- as.numeric(substr(files[f], 21, 25)); print(zip)
    data <- read.csv(paste("C:/Users/amir/Documents/_My_Academics__General/R/R_Projects/Wharton/Data/LoadFeatures/daily_load_features_", zip, ".csv", sep=""))
    ids <- unique(data$SP_ID)
    for (i in 1:length(ids)){
      id <- ids[i]
      data.user <- data[data$SP_ID==id,]
      ave.daily.total.orig <- mean(data.user$daily.total.orig)
      ave.daily.min.orig <- mean(data.user$daily.min.orig)
      ave.daily.max.orig <- mean(data.user$daily.max.orig)
      ave.daily.total.norm <- mean(data.user$daily.cons.norm)
      ave.daily.max.norm <- mean(data.user$daily.max.norm)
      features <- cbind(zip, id, ave.daily.total.orig, ave.daily.min.orig, ave.daily.max.orig, ave.daily.total.norm, ave.daily.max.norm)
      users.ave.load.features <- rbind(users.ave.load.features, features)
    }  
  }
  write.csv(users.ave.load.features, "C:/Users/amir/Documents/_My_Academics__General/R/R_Projects/Wharton/Data/all_users_load_features.csv")
}

###############################################################################
### Function: (for individual IDs) finds original and normalized cons data for a given id in a given zipcode
# Optional: to reduce code duplication, we can replace the code in the UserLoadFeatures() with this.
#           but that means it will read the zip weather data every time for every user which slows down the whole thing
GetConsWeatherNormData <- function(zip, id) {
#   zip.weather <- db.getWeatherData(zip)  # hourly data on outside temp, pressure, dewpoint, and precipitation
#   if(is.null(zip.weather)) {
#     print("No weather data available for this zipcode.")
#     stop
#   }
  zip.weather <- ReadFile(paste("C:/Users/amir/Documents/_My_Academics__General/R/R_Projects/Wharton/Data/Weather/", zip, "weather.csv", sep=""))
  if( is.null(zip.weather) || nrow(zip.weather)==0 ) {
    print("No valid weather data available for this zipcode.")
    stop
  }
  tout.matrix <- CalcToutMatrix(zip.weather)
  cons15min <- db.getIntervalData(id); cons15min[,3:98] <- cons15min[,3:98]*0.00025 # 15-min elec cons data for a given user id
  cons.data.issues <- ValidateConsData(cons15min)
  if(!cons.data.issues$valid) {
    print("No consumption data available for this user.")
    stop  
  }
  cons15min$DATE <- as.Date(cons15min$DATE); 
  names(cons15min)[which(names(cons15min)=="DATE")] <- "date"
  cons15min <- cons15min[which(cons15min$date %in% unique(as.Date(zip.weather$date))), ]
  hourly.cons.orig <- CalcHourlyCons(cons15min)
  cons.weather.merged <- merge(hourly.cons.orig, tout.matrix, by="date")
  if(nrow(hourly.cons.orig)==0) {
    print("No consumption data available for this user.")
    stop
  }
  daily.cons.norm <- NormalizeForWeatherCP(cons.weather.merged, resolution="daily")  # total daily consumption over average daily temperature
  daily.max.norm <- NormalizeForWeatherCP(cons.weather.merged, resolution="daily.max")  # max daily cons over max daily temperature
  #hourly.cons.norm <- NormalizeForWeatherCP(cons.weather.merged, resolution="hourly")
  ### Calculating important load features (original values) <<All values in kWh>> 
  # daily central values (one for each user and each day, based on 15-min readings)
  daily.total.orig <- rowSums(cons.weather.merged[,which(names(cons.weather.merged)=="kwh1"):which(names(cons.weather.merged)=="kwh24")])  # total daily consumption
  daily.min.orig <- apply(cons.weather.merged[,which(names(cons.weather.merged)=="kwh1"):which(names(cons.weather.merged)=="kwh24")], 1, min)  # minimum hourly consumption
  daily.max.orig <- apply(cons.weather.merged[,which(names(cons.weather.merged)=="kwh1"):which(names(cons.weather.merged)=="kwh24")], 1, max)  # maximum hourly consumption
  all.list <- list(daily.total.orig=daily.total.orig, daily.min.orig=daily.min.orig, 
                   daily.max.orig=daily.max.orig, daily.cons.norm=daily.cons.norm, 
                   daily.max.norm=daily.max.norm, cons.weather.merged=cons.weather.merged)
}

###############################################################################
############ Functions that are used in userDataTable #########################
###############################################################################
### Calculate hourly consumption and write to csv
CalcHourlyCons <- function(cons15min) {
  hd <- data.frame()
  cons15minSub <- cons15min[,3:98]
  hourly <- sapply(t(1:24),function(x) rowSums(cons15minSub[,(4*(x-1)+1):(4*x)]))
  hd <- cbind(cons15min[,1:2], hourly)
  names(hd)[3:26] <- paste("kwh", 1:24, sep="")
  hd$date <- as.Date(hd$date)
  #names(hd)[which(names(hd)=="DATE")] <- "date"
  hd
}

CalcToutMatrix <- function(zip.weather) {
  # Prep weather data for normalization
  zip.weather$date <- as.Date(zip.weather$date)
  # impute for missing temperature values
  while (sum(is.na(zip.weather$TemperatureF)) > 0) {
    zip.weather$TemperatureF[which(is.na(zip.weather$TemperatureF))] <- 
      unlist(lapply(which(is.na(zip.weather$TemperatureF)), FUN=ImputeWeather, weather=zip.weather))
  }
  tout.matrix <- data.frame(matrix(zip.weather$TemperatureF, ncol=24, byrow=T))
  tout.matrix$date <- unique(zip.weather$date)
  names(tout.matrix)[1:24] <- paste("temp", 1:24, sep="")
  tout.matrix
}    

###############################################################################
# Function: Impute for missing temperature values.
# Method: average over 1, 2, 24, 48 hours before and after the missing value 
# Args:
#   weather: the weather data frame for the selected zip code
#   j: index of the missing value
# Returns:
#   a vector with ONLY imputed values for missing values 
ImputeWeather <- function(j, weather) {
  mean(c(weather$TemperatureF[ifelse((j-24) > 0, j-24, j+24)], 
         weather$TemperatureF[ifelse((j-48) > 0, j-48, j+48)], 
         weather$TemperatureF[ifelse((j-1) > 0, j-1, j+1)], 
         weather$TemperatureF[ifelse((j-2) > 0, j-2, j+2)],  
         weather$TemperatureF[j+24], weather$TemperatureF[j+48], 
         weather$TemperatureF[j+1], weather$TemperatureF[j+2]), na.rm=T)
}


###############################################################################
# Function: normalized hourly electricity consumption for weather effect
# Method: piecewise linear regression (also called changepoint regression)
# Args:
#   cons: vector of energy consumption 
#   temp: vector of outside temperature readings
# Returns:
#   normalized: a vector of electricity consumption values, normalized for weather
NormalizeForWeatherCP <- function(cons.weather.merged, resolution="daily") {
  if (resolution=="hourly") {
    cons <- as.vector(t(cons.weather.merged[which(names(cons.weather.merged)=="kwh1"):which(names(cons.weather.merged)=="kwh24")]))
    temp <- as.vector(t(cons.weather.merged[which(names(cons.weather.merged)=="temp1"):which(names(cons.weather.merged)=="temp24")]))
  }
  if (resolution=="daily") {
    cons <- rowSums(cons.weather.merged[which(names(cons.weather.merged)=="kwh1"):which(names(cons.weather.merged)=="kwh24")])
    temp <- rowMeans(cons.weather.merged[which(names(cons.weather.merged)=="temp1"):which(names(cons.weather.merged)=="temp24")])
  }
  if (resolution=="daily.max") {
    cons <- apply(cons.weather.merged[which(names(cons.weather.merged)=="kwh1"):which(names(cons.weather.merged)=="kwh24")], 1, max)
    temp <- apply(cons.weather.merged[which(names(cons.weather.merged)=="temp1"):which(names(cons.weather.merged)=="temp24")], 1, max)
}
  ### regression
  library(segmented)  # segmented package fits two-stage models
  reg.data <- data.frame(cons, temp)
  lin.mod.selected <- one.seg.selected <- two.seg.selected <- F
  lin.fit <<- 1
  tryCatch(lin.mod <- lm(cons~temp, data=reg.data), error=function(err)  lin.fit <<- 0)
  
  # two change points
  two.seg <<- 1
  tryCatch(segmented.mod <- segmented(lin.mod, seg.Z = ~temp, psi=list(temp=c(40,65))), error=function(err)  two.seg <<- 0)

  # find normalized values
  temp.n <- temp - temp + 65
  if(two.seg == 1) {
    two.seg.selected <- T
    cps <- confint.segmented(segmented.mod)$temp
    coeffs <- summary(segmented.mod)[[4]]
    slopes <- slope(segmented.mod)$temp
    fitted.norm <- coeffs[1,1] + coeffs[2,1]*temp.n + pmax(temp.n-cps[1,1] , 0)*(coeffs[3,1]) + pmax(temp.n-cps[2,1] , 0)*(coeffs[4,1]) + segmented.mod$residuals
#     if(coeffs[4,1] < coeffs[3,1]) two.seg <- 0
    if( (slopes[1,1] < slopes[2,1]) || slopes[2,1] < slopes[3,1] ) {
      two.seg <- 0
      two.seg.selected <- F
    }
  }
  if(two.seg == 0) {
    one.seg <<- 1
    tryCatch(segmented.mod <- segmented(lin.mod, seg.Z = ~temp, psi=list(temp=c(65))), error=function(err)  one.seg <<- 0)
    if (one.seg == 1) {
    coeffs <- summary(segmented.mod)[[4]]
    cps <- confint.segmented(segmented.mod)$temp
    fitted.norm <- coeffs[1,1] + coeffs[2,1]*temp.n + pmax(temp.n-cps[1,1] , 0)*(coeffs[3,1]) + segmented.mod$residuals
    one.seg.selected <- T
    } else {
      lin.fit <<- 1
      fitted.norm <- predict(lin.mod, newdata=data.frame(temp=temp.n)) + lin.mod$residuals
      lin.mod.selected <- T
    }
  }
  if(two.seg.selected || one.seg.selected) {
    model <- segmented.mod
  } else {
    model <- lin.fit
  }
  
  results <- list(fitted.norm=fitted.norm, model=model)

  
}

###############################################################################
### A Wrapper function to get weather data from MySQL database and write it to csv files
WriteZipWeatherToFile <- function() {
  zips.summary <- read.csv(paste("C:/Users/amir/Documents/_My_Academics__General/R/R_Projects/Wharton/Data/dbmg_zip_summary.csv",sep=""))
  all.zips <- zips.summary$POSTAL_CODE
  for (z in 21:length(all.zips)) {
    zip <- all.zips[z]; print(zip)
    zip.weather.error <- 0
    tryCatch(zip.weather <- db.getWeatherData(zip), error=function(err)  zip.weather.error <- 1)  
    if(zip.weather.error) next
    write.csv(zip.weather, paste("C:/Users/amir/Documents/_My_Academics__General/R/R_Projects/Wharton/Data/Weather/", zip, "weather.csv", sep=""))
  }
}



###############################################################################
######################## Gas consumption analysis #############################
###############################################################################
### Function: normalizes user consumption data and writes important load features to disc
#             important features: daily average, daily max, normalized daily average, normalized daily max
#             important features averaged over all days of the experiment for each user
#             important features averaged daily over all users in a zipcode
#             important features averaged over all days of experiment and all users in a zipcode
UserGasFeatures <- function() {  #id=6919937610; zip=93204
  con <- conf.dbCon(db="pgefinal")
  users.info <- read.csv(paste("C:/Users/amir/Documents/_My_Academics__General/R/R_Projects/Wharton/Data/UserVars/all_users_vars.csv",sep=""))
  ids.with.gas.data <- read.csv("C:/Users/amir/Documents/_My_Academics__General/R/R_Projects/Wharton/Data/GasDaily/dbmg_ids_gas_data.csv")[,2]
  zips.with.gas.data <- unique(users.info$POSTAL_CODE[which(users.info$SP_ID %in% ids.with.gas.data)])
#   zips.summary <- read.csv(paste("C:/Users/amir/Documents/_My_Academics__General/R/R_Projects/Wharton/Data/dbmg_zip_summary.csv",sep=""))
#   all.zips <- zips.summary$POSTAL_CODE
  for (z in 1:length(zips.with.gas.data)) {  # length(all.zips); total 823 zipcodes
    zip <- zips.with.gas.data[z]; print(zip)
    ### read weather and elec cons data
#     zip.weather <- db.getWeatherData(zip)  # hourly data on outside temp, pressure, dewpoint, and precipitation
#     zip.weather.error <- 0
#     zip.weather <- NULL
#     tryCatch(zip.weather <- read.csv(paste("C:/Users/amir/Documents/_My_Academics__General/R/R_Projects/Wharton/Data/Weather/", zip, "weather.csv", sep="")), 
#              error=function(err)  zip.weather.error <- 1) 
    zip.weather <- ReadFile(paste("C:/Users/amir/Documents/_My_Academics__General/R/R_Projects/Wharton/Data/Weather/", zip, "weather.csv", sep=""))
    if( is.null(zip.weather) || nrow(zip.weather)==0 ) next  #|| zip.weather.error==1
    tout.matrix <- CalcToutMatrix(zip.weather)
    zip.load.feat.error <- 0
    zip.load.features <- NULL
    tryCatch(zip.load.features <- read.csv(paste("C:/Users/amir/Documents/_My_Academics__General/R/R_Projects/Wharton/Data/LoadFeatures/daily_load_features_", zip, ".csv", sep="")), 
    error=function(err)  zip.load.feat.error <- 1) 
    if( is.null(zip.load.features) || nrow(zip.load.features)==0 || zip.load.feat.error==1 ) next
    ### Select users who have both electricity and gas services, and whom for which we have both elec and gas cons data.
    zip.users.info <- users.info[which( (users.info$POSTAL_CODE==zip) & (users.info$FUEL=="Dual") ), ]
    zip.users.info <- zip.users.info[which( (zip.users.info$SP_ID %in% c(ids.with.gas.data, unique(zip.load.features$SP_ID)) )), ]# || (zip.users.info$SP_ID %in% unique(zip.load.features$SP_ID)) ), ]
    premise.ids <- unique(zip.users.info$PREMISE_ID)  # users in this zipcode who have both elec and gas services
    users.in.zip.daily.gas <- NULL 
    users.in.zip.gas.elec.degreedays <- NULL
#     for (i in 1:length(ids)) {
    for (i in 1:length(premise.ids)) { 
      #       id <- ids[i]; print(id)
      user.daily.gas <- NULL
      user.gas.elec.degreedays <- NULL
      prem.id <- premise.ids[i]; print(prem.id)  # with each prem.id two ids are associated: one for elec service and one for gas service
      ### Elec cons data
      elec.id <- zip.users.info$SP_ID[zip.users.info$PREMISE_ID == prem.id & zip.users.info$SVC_TYPE_CD == "E"][1]
      user.load.features <- zip.load.features[which(zip.load.features$SP_ID==elec.id),]
      if (is.null(user.load.features) || nrow(user.load.features)==0 ) next ## even though an ID may be present both in the elec consumption data and gas cons data, its data might have not passed the verification process and hence deleted from the database
      ### Gas cons data
      gas.id <- zip.users.info$SP_ID[zip.users.info$PREMISE_ID == prem.id & zip.users.info$SVC_TYPE_CD == "G"][1]
      if(is.null(gas.id) || length(gas.id)==0 || is.na(gas.id)) next
      gas.data.error <- 0
      tryCatch(gas.orig <- db.getGasData(gas.id), error=function(err)  gas.data.error <- 1)  
      if(gas.data.error) next
      ### Merge gas, elec, weather
      gas.elec.merged <- merge(user.load.features, gas.orig, by="date")
      gas.elec.merged$date <- as.Date(gas.elec.merged$date)
      gas.elec.weather.merged <- merge(gas.elec.merged, tout.matrix, by="date")
      if(nrow(gas.elec.weather.merged)==0) next
      ### Normalization call
      gas.daily.norm.model <- NormalizeForWeatherGas(gas.elec.weather.merged, temp.metric="average")
      #gas.daily.norm.model <- NormalizeForWeatherGas(gas.elec.weather.merged, temp.metric="min")  # two compartment: $fitted.norm.model , $ lin.mod , $temp: temperature values used in the regression
      if(is.null(gas.daily.norm.model)) next
      #VisualizeGasFit(gas.daily.norm.model, gas.elec.weather.merged, elec.id, elec.metric="daily.total.orig")
      gas.daily.norm <- gas.daily.norm.model$fitted.norm
#       length(gas.id) <- length(elec.id) <- length(prem.id) <- length(gas.elec.weather.merged$therms)
      user.daily.gas <- cbind.data.frame(gas.id=gas.id, elec.id=elec.id, prem.id=prem.id, date=as.Date(gas.elec.weather.merged$date),
                              gas.daily.orig=gas.elec.weather.merged$therms, gas.daily.norm=gas.daily.norm)
      users.in.zip.daily.gas <- rbind(users.in.zip.daily.gas, user.daily.gas)
      
      ### get degree days, elec cons, gas cons in Joules
      gas.elec.degreedays <- GetGasElecDegreeDays(data=gas.elec.weather.merged)
      user.gas.elec.degreedays <- cbind(gas.id=gas.id, elec.id=elec.id, prem.id=prem.id, gas.elec.degreedays)
      users.in.zip.gas.elec.degreedays <- rbind(users.in.zip.gas.elec.degreedays, user.gas.elec.degreedays)
    }
    write.csv(users.in.zip.daily.gas, paste("C:/Users/amir/Documents/_My_Academics__General/R/R_Projects/Wharton/Data/GasDaily/gas_daily_", zip, ".csv", sep=""))
    write.csv(users.in.zip.gas.elec.degreedays, paste("C:/Users/amir/Documents/_My_Academics__General/R/R_Projects/Wharton/Data/CDDHDD/gas_elec_cddhdd_", zip, ".csv", sep=""))
  }

}

###############################################################################
GetGasElecDegreeDays <- function(data=gas.elec.weather.merged) {
  elec.cons <- data$daily.total.orig
  gas.cons <- data$therms
  temp <- rowMeans(data[, which(names(gas.elec.weather.merged)=="temp1"):which(names(gas.elec.weather.merged)=="temp24")])
  dates <- data$date
  # plot
#   par(mfrow=c(1,1))
#   plot(y=elec.cons*3.6, x=temp-65, pch=19, col="blue", ylim=c(0, max(elec.cons*3.6, gas.cons*105.506)))  # 1 kWh = 3.6 MJ
#   points(y=gas.cons*105.506, x=temp-65, pch=19, col="red")  # 1 Therm = 105.506 MJ
#   legend("topright", c("Elec.", "Gas"), pch=c(19, 19), col=c("blue", "red"), bty="n")
  elec.cons.J <- sum(elec.cons*3.6)
  gas.cons.J <- sum(gas.cons*105.506)
  temp.base <- temp - 65
  CDD <- sum(temp.base[temp.base > 0])
  HDD <- abs(sum(temp.base[temp.base < 0]))
#   plot(y=elec.cons.J, x=CDD, xlim=c(7000, 9000), ylim=c(74000, 160000), pch=19, col="blue")
#   points(y=gas.cons.J, x=HDD, pch=19, col="red")
  results <- data.frame(elec.cons.J=elec.cons.J, gas.cons.J=gas.cons.J, CDD=CDD, HDD=HDD)
}


###############################################################################
NormalizeForWeatherGas <- function(gas.elec.weather.merged, temp.metric="average") {
  if("gas.daily.orig" %in% names(gas.elec.weather.merged)) names(gas.elec.weather.merged)[which(names(gas.elec.weather.merged)=="gas.daily.orig")] <- "therms"
  cons <- gas.elec.weather.merged$therms
  if (temp.metric=="average") temp <- rowMeans(gas.elec.weather.merged[which(names(gas.elec.weather.merged)=="temp1"):which(names(gas.elec.weather.merged)=="temp24")])
  if (temp.metric=="min") temp <- apply(gas.elec.weather.merged[which(names(gas.elec.weather.merged)=="temp1"):which(names(gas.elec.weather.merged)=="temp24")], 1, min)

#   cons <- therms
  library(segmented)  # segmented package fits two-stage models
  reg.data.full <- reg.data <- data.frame(cons, temp)
  # Find unoccupied days and restrict the segmented model to occupied days
  non.outliers <- FindOccupied(reg.data)
  reg.data <- reg.data[non.outliers, ]
  if (nrow(reg.data) < 5) return(NULL)
  cons <- reg.data$cons
  temp <- reg.data$temp
  
  # fit the model to occupied days
  lin.mod.selected <- one.seg.selected <- two.seg.selected <- F
  lin.fit <<- 1
  tryCatch(lin.mod <- lm(cons~temp, data=reg.data), error=function(err)  lin.fit <<- 0)
  
  # two change points
  two.seg <<- 1
  tryCatch(segmented.mod <- segmented(lin.mod, seg.Z = ~temp, psi=list(temp=c(40,65))), error=function(err)  two.seg <<- 0)
  
  # find normalized values
  temp.n <- temp - temp + 65
  if(two.seg == 1) {
    two.seg.selected <- T
    cps <- confint.segmented(segmented.mod)$temp
    coeffs <- summary(segmented.mod)[[4]]
    slopes <- slope(segmented.mod)$temp
    fitted.norm <- coeffs[1,1] + coeffs[2,1]*temp.n + pmax(temp.n-cps[1,1] , 0)*(coeffs[3,1]) + pmax(temp.n-cps[2,1] , 0)*(coeffs[4,1]) + segmented.mod$residuals
    if( (slopes[1,1] > slopes[2,1]) || slopes[2,1] > slopes[3,1] ) {
      two.seg <- 0
      two.seg.selected <- F
    }
  }
  if(two.seg == 0) {
    one.seg <<- 1
    tryCatch(segmented.mod <- segmented(lin.mod, seg.Z = ~temp, psi=list(temp=c(65))), error=function(err)  one.seg <<- 0)
    if (one.seg == 1) {
      coeffs <- summary(segmented.mod)[[4]]
      cps <- confint.segmented(segmented.mod)$temp
      fitted.norm <- coeffs[1,1] + coeffs[2,1]*temp.n + pmax(temp.n-cps[1,1] , 0)*(coeffs[3,1]) + segmented.mod$residuals
      one.seg.selected <- T
    } else {
      lin.fit <<- 1
      fitted.norm <- predict(lin.mod, newdata=data.frame(temp=temp.n)) + lin.mod$residuals
      lin.mod.selected <- T
    }
  }
  if(two.seg.selected || one.seg.selected) {
    model <- segmented.mod
  } else {
    model <- lin.fit
  }
  fitted.norm
  tt <- reg.data.full$cons
  tt[non.outliers] <- fitted.norm
  tt[which(tt < 0)] <- 0
  fitted.norm2 <- tt
  results <- list(fitted.norm=fitted.norm2, model=model, temp=reg.data.full$temp)
  
#   lin.fit <- 1
#   tryCatch(lin.mod <- lm(therms~temp), error=function(err)  lin.fit <<- 0)
#   if (lin.fit == 0) {
#     print("Linear model not valid.")
#     return
#   }
#   temp.n <- temp - temp + 65
#   fitted.norm <- predict(lin.mod, newdata=data.frame(temp=temp.n)) + lin.mod$residuals
#   list(fitted.norm=fitted.norm, lin.mod=lin.mod, temp=temp)
}

###############################################################################
############ Class structures to keep users and weather data ##################
###############################################################################
###############################################################################
# class structure based on example from
# http://bryer.org/2012/object-oriented-programming-in-r
WeatherClass = function(zipcode){
  raw = db.getWeatherData(zipcode)
  if(length(raw)==0) stop(paste('No data found for zipcode',zipcode))
  rawData = data.frame(
    dates = as.POSIXlt(raw[,1],tz="PST8PDT",'%Y-%m-%d %H:%M:%S'),
    tout = raw[,'TemperatureF'],
    pout = raw[,'Pressure'],
    rain = raw[,'HourlyPrecip'],
    dp   = raw[,'DewpointF']
  )  
  days = unique(as.Date(rawData$dates))
  # FYI, spring forward causes NA dates to find these:
  # which(is.na(dates))
  
  # TODO: do we need to do anything about the NA values?
  
  obj = list (
    days    = days,
    dates   = rawData$dates,
    tout    = rawData$tout,
    rawData = rawData,
    get     = function(x) obj[[x]],
    # Not sure why <<- is used here
    # <<- searches parent environments before assignment
    # http://stat.ethz.ch/R-manual/R-patched/library/base/html/assignOps.html
    set     = function(x, value) obj[[x]] <<- value,
    props   = list()  # a list of properties that we may want to add later for reference
  )
  
  # returns relative humidity as decimal from 0 to 1 given temperature and dewpoint
  # using August-Roche-Magnus approximation: http://andrew.rsmas.miami.edu/bmcnoldy/humidity_conversions.pdf
  obj$rh = function(tout,dp) {
    a = 17.271
    b = 237.7
    tout = (tout - 32) * 5/9
    dp   = (dp   - 32) * 5/9
    rh = exp(a*dp/(b + dp)) / exp(a*tout/(b + tout))
  }
  
  #   obj$add = function(name, value) {
  #     obj[[name]] = value
  #   }
  
  # note how list manipulation requires the use of assign
  # not sure why values can't be set in place, but it 
  # appears to have to do with variable scoping
  obj$addProp = function(name, value) {
    p <- obj$props
    p[[name]] <- value
    assign('props', p, envir=obj)
  }
  
  obj$resample = function(newDates,name='tout') {
    # approx returns both the newDates and the interpreted values
    # but we only need the values
    a = approx(obj$dates, obj$rawData[,name], newDates, method="linear")[[2]]
    #b = a[2]
    if (all(is.na(a))){ 
      print(paste(obj$dates[1],obj$dates[-1]))
      print(paste(newDates[1],newDates[-1]))
      stop("No weather data available") 
    }
    return(a)
  }
  #obj <- list2env(obj)
  class(obj) = "WeatherClass"
  return(obj)
}

###############################################################################
ResDataClass = function(sp_id,zip=NULL,weather=NULL,data=NULL,db='pgefinal'){
  if(is.null(data) || length(data) == 0) {
    data = db.getIntervalData(sp_id)  
  }
  if(length(data)==0) stop(paste('No data found for sp_id',sp_id))
  
  zipcode = db.getZipForId(sp_id)
  kwMat96 = as.matrix(data[,3:98])/1000  
  kwMat = sapply(t(1:24),function(x) rowMeans(kwMat96[,(4*(x-1)+1):(4*x)]))  # this is where it changes from 15-min data to hourly
  # reshape the kW readings into a vector matching the dates
  kw    = as.vector(t(kwMat))
  
  days = as.POSIXct(data[,'DATE'],tz="PST8PDT", '%Y-%m-%d')
  # create a row of hourly values for each day
  daySteps = 24
  dtDay = daySteps/24 * 60 * 60 # in seconds
  # sapply returns an array of numeric epoch seconds (for origin '1970-01-01')
  dateMat = sapply(days,FUN=function(x) x + (0:(daySteps-1) * dtDay))
  # flatten into a vector and re-convert into date objects
  dates = as.POSIXlt(as.vector(dateMat),origin='1970-01-01')
  
  if (is.null(weather)) weather = WeatherClass(zipcode) # todo: pass in the dates to interpolate ,dates)
  tout = weather$resample(dates,'tout')  # note that the object "weather" is a WeatherClass object and has a "resample" function
  
  # TODO: clear out obviously bad readings
  #keepers   = which(kw > 0)
  
  obj = list (
    id = sp_id,
    dates = dates,
    kw  = kw,
    kwMat = kwMat,
    days = days,
    zipcode = zipcode,
    weather = weather, # its a WeatherClass object with all associated data and functions
    tout = tout,
    toutMat = matrix(tout,ncol=24,byrow=TRUE),
    get = function(x) obj[[x]],
    set = function(x, value) obj[[x]] <<- value,
    props = list()
  )
  
  obj$w = function(name='tout') {
    return( obj$weather$resample(dates,name) )
  }
  
  obj$df = function() {
    return( data.frame(kw=obj$kw,
                       tout=obj$tout,
                       dates=obj$dates ) )
  }
  
  obj$norm = function(data) {
    # divide data by the 97th %ile
    return (data/quantile(data,0.97,na.rm=TRUE))
  }
  
  #   obj$add = function(name, value) {
  #     obj[[name]] = value
  #   }
  
  obj$addProp = function(name, value) {
    p <- obj$props
    p[[name]] <- value
    assign('props', p, envir=obj)
  }
  
  obj$matchDates = function(newDates) {
    a = approx(obj$dates, obj$tout, newDates, method="linear" )[[2]]
    return(a)
  }
  
  #obj <- list2env(obj)
  class(obj) = "ResDataClass"
  return(obj)
}


###############################################################################
############################### Other Codes ###################################
###############################################################################
### Function: creates a data table with important information at zipcode level
zipDataTable <- function(){
  zips.summary <- read.csv(paste("C:/Users/amir/Documents/_My_Academics__General/R/R_Projects/Wharton/Data/dbmg_zip_summary.csv",sep=""))
  all.zips <- zips.summary$POSTAL_CODE
  
  for (zip in all.zips) {
    # Read important variables from disk (user-level)
    zip.all.vars <- read.csv(zip.variables, paste("C:/Users/amir/Documents/_My_Academics__General/R/R_Projects/Wharton/Data/UserVars/", zip,"_user_vars.csv",sep=""))
    # Read weather data from disk (user-level)
    zip.all.weather <- read.csv(zip.weather.matrix, paste("C:/Users/amir/Documents/_My_Academics__General/R/R_Projects/Wharton/Data/WeatherData/", zip,"_weather.csv",sep=""))
    # Read hourly consumption from disk (user-level)
    zips.all.hourly <- read.csv(zip.hourly.cons.orig, paste("C:/Users/amir/Documents/_My_Academics__General/R/R_Projects/Wharton/Data/HourlyCons/", zip,"_weather.csv",sep=""))
    # Read normalized hourly consumption from disk (user-level)
    zip.all.norm.hourly <- read.csv(zip.hourly.cons.orig.norm, paste("C:/Users/amir/Documents/_My_Academics__General/R/R_Projects/Wharton/Data/NormHourlyCons/", zip,"_weather.csv",sep=""))
    # Read billing info (elec & gas, original & normal)
    zip.all.bills <- read.csv(zip.bills, paste("C:/Users/amir/Documents/_My_Academics__General/R/R_Projects/Wharton/Data/NormHourlyCons/", zip,"_bills.csv",sep=""))
    
  }
  
}



###############################################################################
################################## Notes ######################################
###############################################################################
### notes on multiple change points
# (1) an automatic procedure may be implemented by specifying psi=NA
# and stop.if.error=FALSE in seg.control. This automatic procedure, 
# however, is expected to overestimate the number of breakpoints.
#   seg.control(K=2, stop.if.error=F, it.max=1000)
#   seg.control(K=1, stop.if.error=T, it.max=100)
# (2) If it.max=0 segmented will estimate a new linear model with break-point(s) 
# fixed at the values reported in psi.

### manual verification
#   fitted <- coeffs[1,1] + coeffs[2,1]*temp + pmax(temp-cps[1,1] , 0)*(coeffs[3,1]) + pmax(temp-cps[2,1] , 0)*(coeffs[4,1])
#   points(y=fitted, x=temp, col="red", pch=19, cex=0.5)
#   intercept(segmented.mod2)$temp[1]
#   slope(segmented.mod2)
#   coeffs <- summary(segmented.mod2)[[4]]
#   cps <- confint.segmented(segmented.mod2)$temp
#   print.segmented(segmented.mod)
#   summary(segmented.mod)

### manually plotting (helpful for normalizing):
#   (cp <- segmented.mod$psi[2])  # change point
#   (interc1 <- segmented.mod$coeff[1])  # intercept of the first line
#   (slope1 <- segmented.mod$coeff[2])  # slope of the first line
#   (slope2 <- segmented.mod$coeff[2] + segmented.mod$coeff[3])  # slope of the second line
#   (interc2 <- interc1 + slope1*cp - slope2*cp)
#   clip(30, cp, 0, 10)
#   abline(a=interc1, b=slope1, col="red")
#   clip(cp, 100, 0, 10)
#   abline(a=interc2, b=slope2, col="red")  
