
# Function: quickly finds the estimates for a simple change point model with one cp
# Args:
#   cp: change point (the point of change in slope of the regression line)
#   const: intercept of the first segment of the regression line
#   lower: slope of the line in the first segment (i.e., lower slope)
#   upper: slope of the line in the second segment (i.e., higher slope)
# Returns:
#   a data frame with the first column x values and second column estimates of the
#   piecewise linear regression model
quickEst = function(cp,const,lower,upper,tlim=c(0,100)) {
  tlim=c(floor(tlim[1]),ceiling(tlim[2]))
  x = tlim[1]:tlim[2]
  y = c(c(const + tlim[1]:cp * lower), c(const + cp * lower + ((cp+1):tlim[2] -cp) * upper))
  return(cbind(x,y))
}

###############################################################################
########################## Load Feature Analysis ##############################
###############################################################################
FindBaseloadPercent <- function() {
  files <- list.files(paste("/Data/LoadFeatures/", sep=""))
  baseload.percent <- NULL
  date.ranges <- read.csv(paste("/Data/dbmg_zip-date-ranges.csv", as.is=T), as.is=T)
  date.ranges[,2] <- as.Date(date.ranges[,2], format="%m/%d/%Y"); date.ranges[,3] <- as.Date(date.ranges[,3], format="%m/%d/%Y")
  dates.all <- data.frame(date=seq(as.Date(min(date.ranges[,2])), as.Date(max(date.ranges[,3])), by="1 day"))
  for (f in 1:length(files)) {  #length(files)
    zip <- as.numeric(substr(files[f], 21, 25)); print(zip)
    data <- read.csv(paste(wd, "/Data/LoadFeatures/", files[f], sep=""))
    ids <- unique(data$SP_ID)
    data$date <- as.Date(data$date)
    percentages <- data.frame(date=dates.all$date)
    for (i in 1:length(ids)) {
      id <- ids[i]
      data.user <- data[data$SP_ID==id,]
      percentage <- cbind.data.frame(((data.user$daily.min.orig*24) / (data.user$daily.total.orig)), date=data.user$date)
      # take average of daily percentage over the entire period for each user (one number for each user). save to file
      baseload.percent <- rbind(baseload.percent, cbind(id=id, percentage=mean(percentage[,1], na.rm=T)))      
    }
    if(f %% 5 ==0) {
      write.csv(baseload.percent, paste(wd, "/Data/BaseloadPercentages/baseload_percentages_", zip, ".csv", sep=""))
      baseload.percent <- NULL
    }  
  }
}


###############################################################################
### Combines individual zipcode files to create one file containing zipcode, id, and average baseload percentage of each user
IntegrateBaseloadFiles <- function() {
  files <- list.files("C:/Users/amir/Documents/_My_Academics__General/R/R_Projects/Wharton/Results/BaseloadAnalysis")
  files <- files[-c(1,73)]
  data.all <- NULL
  for (f in 1:length(files)) {  #length(files)
    #zip <- as.numeric(substr(files[f], 21, 25)); print(zip)
    print(f)
    data <- read.csv(paste("C:/Users/amir/Documents/_My_Academics__General/R/R_Projects/Wharton/Results/BaseloadAnalysis/", files[f], sep=""))
    data.all <- rbind(data.all, data)
  }
  write.csv(data.all, paste("C:/Users/amir/Documents/_My_Academics__General/R/R_Projects/Wharton/Results/all_users_baseload_percentages.csv", sep=""))
}

###############################################################################
### Function: Integrates all data from different sources, merges data frames, and returns data frames ready for analysis.
CreateAnalysisTables <- function(analysis.type="", id=7902612905) {
  #id=6731398510
  wharton.wkfl <- "C:/Users/amir/Documents/_My_Academics__General/R/R_Projects/Wharton"
  ### Each user, averaged over the entire period
  # Load features:
  users.load.features <- read.csv(paste(wharton.wkfl, "/Data/all_users_load_features.csv", sep="")); dim(users.load.features)
  names(users.load.features)[which(names(users.load.features)=="id")] <- "SP_ID"
  # Variables 
  users.vars <- read.csv(paste(wharton.wkfl, "/Data/all_users_vars.csv", sep="")); dim(users.vars)
  # CDD/HDD and total consumption (elec and gas)
  users.elec.gas.dd <- read.csv(paste(wharton.wkfl, "/Data/all_users_gas_elec_dd.csv", sep="")); 
  dim(users.elec.gas.dd); names(users.elec.gas.dd)[which(names(users.elec.gas.dd)=="prem.id")] <- "PREMISE_ID"
  # baseload as a percent of total load 
  baseload.percent <- read.csv(paste(wharton.wkfl, "/Data/all_users_baseload_percentages.csv", sep="")); 
  dim(baseload.percent); names(baseload.percent)[which(names(baseload.percent)=="id")] <- "SP_ID"
  
  ### Merge data frames
  load.vars.data <- merge.data.frame(users.load.features, users.vars, by="SP_ID"); dim(load.vars.data)
  load.vars.data <- merge.data.frame(users.elec.gas.dd, users.vars, by="PREMISE_ID"); dim(load.vars.data)
  load.vars.data <- merge.data.frame(baseload.percent, users.vars, by="SP_ID"); dim(load.vars.data)

  ### Time-series data (requires zipcode to open the file)
  ### Find the zipcode of the ID of interest and read data files
  zip <- unique(users.vars$POSTAL_CODE[which(users.vars$SP_ID==id)])
  if (is.null(zip) || is.na(zip)) {
    print("User ID not found."); return
  } else {
    print(zip)
  }
  # Read gas consumption data
  gas.daily <- ReadFile(paste(wharton.wkfl, "/Data/GasDaily/gas_daily_", zip, ".csv", sep=""))
  gas.daily$date <- as.Date(gas.daily$date)
  gas.daily.user <- gas.daily[gas.daily$elec.id==id, ]; dim(gas.daily.user)
  if(nrow(gas.daily.user)==0) gas.daily.user <- gas.daily[gas.daily$gas.id==id, ]; dim(gas.daily.user)
  # Read degree days data
  degreedays.daily <- ReadFile(paste(wharton.wkfl, "/Data/CDDHDD/gas_elec_cddhdd_", zip, ".csv", sep=""))
  # Read elec cons data
  elec.daily <- ReadFile(paste(wharton.wkfl, "/Data/LoadFeatures/daily_load_features_", zip,".csv",sep=""))
  elec.daily.user <- elec.daily[elec.daily$SP_ID==id,]
  # Read weather data
  zip.weather <- read.csv(paste(wharton.wkfl, "/Data/Weather/", zip, "weather.csv", sep=""))
  zip.weather$date <- as.Date(zip.weather$date)
  dim(zip.weather); names(zip.weather)
  gas.elec.weather.merge <- merge(gas.daily.user, zip.weather, by="date"); dim(gas.elec.weather.merge)
  gas.elec.weather.merge$date
  
  ### Plot time-series by calling DrawTimeSeries
  # Plot time series of load features (no merging is necessary)
  plot.data <- cbind.data.frame(Daily.Average=elec.daily.user[,4]/24, Daily.Min=elec.daily.user[,5], Daily.Max=elec.daily.user[,6])
  dates <- elec.daily.user$date
  
  # Plot elec cons, gas cons, and weather
  plot.data <- cbind.data.frame(Daily.Gas=degreedays.daily$gas.cons.J, Daily.Elec=degreedays.daily$elec.cons.J, 
                                CDD=degreedays.daily$CDD, HDD=degreedays.daily$HDDH)
  dates <- degreedays.daily$date
  #plot.data <- cbind(Elec.Cons=elec.cons, Gas.Cons=gas.cons, Temperature=temp)
  DrawTimeSeries(plot.data=plot.data, dates=gas.elec.weather.merged$date,
                 main.t="Time series of gas and electricity consumption \nand temperature (normalized to (0,1))",
                 y.t="kWh/kWh; Therm/Therm; F/F")

  ### Plot
  png(paste(wharton.wkfl, "/Results/", id, ".png", sep=""), 
      width=1024, height=768, units="px", res=120, pointsize = 12,
      bg = "white", family = "", restoreConsole = TRUE)  #, type = c("windows", "cairo", "cairo-png"), antialias
  par(mar=c(4,4,1,1),xaxs = "i",yaxs = "i",cex.axis=1.0,cex.lab=1.0)
  DrawTimeSeries(plot.data=plot.data, dates=dates, 
                 main.t=paste("Elec. Cons. for User ", id, " in Zipcode ", zip , sep=""), 
                 y.t="kWh", colors=c("grey30", "blue", "red"), scale.cols=F)
  dev.off()
  # Notes on plot resolution
  #http://stackoverflow.com/questions/8399100/r-plot-size-and-resolution
  #http://ask.metafilter.com/73828/Optimal-output-settings-when-generating-images-for-use-in-PowerPoint
  
  
}

###############################################################################
ReadFile <- function(address) { 
  data <- NULL
  open.err <- 0
  if(file.exists(address)) {
    tryCatch(data <- read.csv(address), error=function(err)  open.err <- 1)
    if( (open.err) || !exists("data") || is.null(data) || (nrow(data)==0) ) {
      print("Data file not valid."); return(NULL)
    }
  } else {
    print("Data file not found."); return(NULL)    
  }
  print(dim(data)); print(names(data))
  data
}

###############################################################################
########################## Intervention analysis ##############################
###############################################################################
ZipAnalysis <- function() {
  # Zipcode-level CDD/HDD and consumption (elec and gas)
  # note that this table only includes dual-commodity users.
  zips.elec.gas.dd <- read.csv("C:/Users/amir/Documents/_My_Academics__General/R/R_Projects/Wharton/Data/all_zips_gas_elec_dd.csv"); 
  dim(zips.elec.gas.dd); names(zips.elec.gas.dd)
  

}

###############################################################################
############################## Visualization ##################################
###############################################################################
### Draws a time series overlay over an existing plot
DrawTimeSeries <- function(plot.data, dates, main.t="", y.t="", colors=NULL, scale.cols=T) {  #elec.cons, gas.cons, temp
  legends <- names(plot.data)
  k = c(.5,1,1,1,.5); k = k/sum(k) #  k is the vector of weights
  if (is.null(colors)) colors <- rainbow(ncol(plot.data))
  #fjj = filter(jj, sides=2, k)
  y.l <- c(0, max(plot.data, na.rm=T))
  if(scale.cols) {
    plot.data <- apply(plot.data, 2, function(x) x/max(x, na.rm=T))
    y.l <- c(0, 1)
  }
  plot(filter(plot.data[,1], sides=2, k), ylim=y.l, col=colors[1], lwd=1.5,
       main=main.t, ylab=y.t, xlab="Date", xaxt="n")
  dates <- as.POSIXlt(dates)  #gas.elec.weather.merged$date
  quarter.begins <- which((dates$mday == 1) & ((dates$mon+1) %% 3 == 0))
  abline(v=quarter.begins, col="grey", lty="dashed")
#   if(ncol(plot.data) > 1) {
    for (co in 1:ncol(plot.data)) {
      lines(filter(plot.data[,co], sides=2, k), col=colors[co], lwd=1.5)
      #       lines(filter(plot.data[,co]/max(plot.data[,co]), sides=2, k), col=colors[co], lwd=2)
    }  
#   }
  axis(1,at=quarter.begins,labels=paste(substr(months(dates),1,3), dates$year + 1900)[quarter.begins],
       mgp=c(1,0.2,0),tcl=0.5,tick=F, cex.axis=0.8, las=2)
  legend("topleft", legend=legends, lwd=2, col=c(colors), bty="n", cex=0.8)
}

###############################################################################
# Function: visualizes elec cons data
# Method: a 2x2 panel plot
# Args:
#   cons.weather.merged: a data frame containing cons and weather data 
#   sp.id: the id of the account of interest
# Returns:
#   plots a 2x2 panel plot:
#     panel 1: heatmap of consumption
#     panel 2: time-series of consumption overlayed on time-series of temperature
#     panel 3: consumption v temp with fitted regression and normalized values
#     panel 4: important variables info for that service id
VisualizeElecFit <- function(cons.weather.merged=NULL, sp.id=244276153, users.info=NULL, zips.summary=NULL, resolution="daily") {
  if(is.null(users.info)) users.info <- read.csv(paste("C:/Users/amir/Documents/_My_Academics__General/R/R_Projects/Wharton/Data/all_users_vars.csv",sep=""))
  if(is.null(zips.summary)) zips.summary <- read.csv(paste("C:/Users/amir/Documents/_My_Academics__General/R/R_Projects/Wharton/Data/dbmg_zip_summary.csv",sep=""))
  zip <- unique(users.info$POSTAL_CODE[which(users.info$SP_ID==sp.id)]); print(zip)
  cons.weather.norm <- GetConsWeatherNormData(zip=zip, id=sp.id)
  cons.weather.merged <- cons.weather.norm$cons.weather.merged
  tout.matrix <- cons.weather.merged[, (which(names(cons.weather.merged)=="temp1")):(which(names(cons.weather.merged)=="temp24"))]  #hourly
  cons.matrix <- cons.weather.merged[, (which(names(cons.weather.merged)=="kwh1")):(which(names(cons.weather.merged)=="kwh24"))]  #hourly
  
  ### Plots
  par(mfrow=c(2,2))  # par(mfrow=c(1,1))
  # (1) heatmap of consumption (need to fix this to show dates on vertical axis)
  color.map <- colorRampPalette(c("blue", "orange", "red"))(100)
  #  hmapHourly(tout.matrix[,1:24], colorMap=color.map, dates=cons.weather.merged$date)
  hmapHourly(cons.matrix[,1:24], colorMap=color.map, dates=cons.weather.merged$date,
       main.title="Hourly Electricity Consumption")
    
  # (2) time-series of consumption overlayed on time-series of temperature (may want to make it daily values, especially since we are working with daily values all the time)
  if (resolution=="daily") {
    cons <- rowSums(cons.weather.merged[which(names(cons.weather.merged)=="kwh1"):which(names(cons.weather.merged)=="kwh24")])
    temp <- rowMeans(cons.weather.merged[which(names(cons.weather.merged)=="temp1"):which(names(cons.weather.merged)=="temp24")])
  }
  if (resolution=="daily.max") {
    cons <- apply(cons.weather.merged[which(names(cons.weather.merged)=="kwh1"):which(names(cons.weather.merged)=="kwh24")], 1, max)
    temp <- apply(cons.weather.merged[which(names(cons.weather.merged)=="temp1"):which(names(cons.weather.merged)=="temp24")], 1, max)
  }
  plot(y=cons/max(cons), x=cons.weather.merged$date, type="l", col="blue",
       main="Time series of consumption and \ntemperature (normalized to (0,1))",
       ylab="kWh/kWh and F/F", xlab="Date")
  lines(temp/max(temp), x=cons.weather.merged$date, col="red")
  legend("bottomleft", paste("Correlation:", round(cor(cons, temp), 2)), bty="n")
  
  # (3) consumption v temp with fitted regression and normalized values
  norm.results <- NormalizeForWeatherCP(cons.weather.merged, resolution="daily")
  fitted.values <- norm.results$fitted.norm
  segmented.mod <- norm.results$model
  plot(y=cons, x=temp, pch=19, cex=0.5, col="grey30", main="Temperature model and \nnormalized consumption values",
       ylab="kWh", xlab="F")
#   points(y=fitted.values, x=temp, col="green", pch=3, cex=0.5)
  plot(segmented.mod, add=T, col="red", lwd=2)
  lines.segmented(segmented.mod, col="red", lwd=2)  # draws the location of cp and its confidence interval on current plot
  legend("topleft", legend=c("Original values", "Normalized values"), pch=c(19, 3), col=c("grey30", "green"), bty="n", cex=0.8)
  
  # (4) a panel of all important information about this user
  ln <- which(users.info$SP_ID==sp.id)
  
  text = c(paste("Fuel:", users.info[ln,"FUEL"],
           "\nZipcode:", users.info[ln,"POSTAL_CODE"],
           "\nClimate Zone:", users.info[ln,"CLIMATE_BAND"],
           "\nCity:", users.info[ln,"CITY"],
           "\nCounty:", users.info[ln,"COUNTY"],
           "\nEnvironmentally conscious:", users.info[ln,"ENVIRONMENT_OR_WILDLIFE"],
           "\nAttention to environ. issues:", users.info[ln,"ENVIRONMENTAL_ISSUES"],
           "\nGreen living:", users.info[ln,"GREEN_LIVING"],
           "\nHigh tech living:", users.info[ln,"HIGH_TECH_LIVING"],
           "\nEducation:", users.info[ln,"EDUCATION"],
           "\nCluster:", users.info[ln,"Cluster"],
           "\nNet Metering:", users.info[ln,"NEM_FLAG"],
           "\nEnrollment in EE programs:", users.info[ln,"TIMESENROLLED"],
           "\nRebate type:", users.info[ln,"rebate_type"]
  ))
  library(grid)
  grid.text(text, x=unit(0.55, "npc"), y=unit(0.45, "npc"), just=c("left", "top"), 
            gp=gpar(fontsize=10, col="grey10"))

}

###############################################################################
# Function: an auxiliary function for plotting functions where we want to highlight points by their values
#           it creates a color map for displaying a vector of data
# Args:
#   data:     a vector of the data to be visualized
#   colorMap: the colors that are desired to the be used on the plot
#   log:      a binary specifying whether log scale is to be used for assigning colors to different values
# Returns:
#   a vector of color values, each corresponding to respective value in the data vector
mapColors = function(data, colorMap=NA, log=FALSE) {
  if(length(colorMap) < 2) { colorMap = heat.colors(100) } # default colors if user does not provide colors
  mn = min(data, na.rm=TRUE)
  data_mn = data - mn + 0.001
  if(log) {
    data_mn = log(data_mn + 1) # nothing below zero after we take the log
  }
  idx = ceiling(data_mn / max(data_mn, na.rm=TRUE) * length(colorMap))  # intensity of each value
  return(colorMap[idx])  # based on the intensity of each value, they get a specific color of the color map
}

###############################################################################
# Function: plots a heatmap of the values of a given matrix
# Args:
#   data:     a vector of the data to be visualized
#   colorMap: the colors that are desired to the be used on the plot
# Returns:
#   creates a plot
hmapHourly = function(data, colorMap=NA, yvals=NA, xvals=NA, log=FALSE, dates=dates, main.title="") {
  n = dim(data)[1]
  m = dim(data)[2]
  # default values
  if(length(colorMap) < 2) { colorMap = heat.colors(100) }
  if(length(xvals)    < 2) { xvals=1:m } # xvals and yvals create the grid on which the heatmap will be drawn
  if(length(yvals)    < 2) { yvals=1:n }
  
  cols = rep(xvals, n) # duplicate column position values across all rows
  rows = rep(yvals, each=m) # duplicate y values across a whole row of data
  vals = as.vector(t(as.matrix(data))) # linearize the matrix of data
  plot(cols,rows,col=mapColors(vals, colorMap, log=log),
       ylim=c(min(yvals), max(yvals)), xlim=c(min(xvals), max(xvals)),
       axes=F, cex=5, pch=15, xlab='Hour of day',ylab='', main=main.title)  #,...
  axis(1,at=(0:23)+0.5,labels=(1:24),mgp=c(1,0.5,0),tcl=0.5,tick=F, cex.axis=1.0, las=2) # 1 = xaxis, labels in the center of each range
  axis(1,at=(0:24),labels=F,tick=T,mgp=c(1,0,0.3),tcl=0.5, col="grey60")          # ticks at boundaries
  first.day.month <- which(as.POSIXlt(dates)$mday==1)
  axis(2, at=first.day.month, labels=dates[first.day.month], mgp=c(1,0.1,0.5), tcl=0.5, las=2, cex.axis=0.7, col="grey60") # yaxis
  ### Notes:
  #   http://www.programmingr.com/content/controlling-margins-and-axes-oma-and-mgp/
  #   display.brewer.all()
  #   colors <- brewer.pal(4, "YlOrRd")
  #   color.map <- colorRampPalette(colors)(100)
  #   http://simplystatistics.org/2011/10/17/colors-in-r/
  #   http://stat.ethz.ch/R-manual/R-patched/library/grDevices/html/colorRamp.html
}

###############################################################################
### Function: visualize gas weather normalization
# Method: a 2x2 panel plot
# Args:
#   gas.daily.norm.model: 
#   gas.elec.weather.merged: 
#   elec.id
#   elec.metric
# Returns:
#   plots a 2x2 panel plot:
#     panel 1: scatterplot of daily gas consumption versus elec consumption
#     panel 2: scatterplot of gas consumption versus temperature
#     panel 3: time-series of gas cons, outside temperature, and elec overlayed
#     panel 4: important variables info for that service id
VisualizeGasFit <- function(gas.daily.norm.model, gas.elec.weather.merged, elec.id, elec.metric="daily.total.orig") {
  ##### We need to make sure we collect all the data that is needed from this point on in the above code
  if("gas.daily.orig" %in% names(gas.elec.weather.merged)) names(gas.elec.weather.merged)[which(names(gas.elec.weather.merged)=="gas.daily.orig")] <- "therms"
  fitted.norm <- gas.daily.norm.model$fitted.norm
  model <- gas.daily.norm.model$model
  temp <- gas.daily.norm.model$temp
  jitt <- rnorm(0, 0.05, n=length(gas.elec.weather.merged$therms))
  gas.cons <- gas.elec.weather.merged$therms
  elec.cons <- with(gas.elec.weather.merged, 
                    switch(elec.metric, daily.total.orig=daily.total.orig, daily.min.orig=daily.min.orig,
                           daily.max.orig=daily.max.orig, daily.cons.norm=daily.cons.norm, daily.max.norm=daily.max.norm)
  )
  
  par(mfrow=c(2,2))
  #par(mfrow=c(1,1))
  # (a) scatterplot of daily elec cons versus gas cons
  plot(x=elec.cons, y=gas.cons + jitt, pch=19, cex=0.5, col="grey30"
       , main="Daily Gas Cons. v. Elec. Cons.", xlab="kWh", ylab="Therms")
  # (b) scatterplot of daily gas cons versus average daily temp
  segmented.mod <- gas.daily.norm.model$model
  plot(y=gas.cons, x=temp, pch=19, cex=0.5, col="grey30", main="Temperature model and \nnormalized consumption values",
       ylab="Therms", xlab="F")
#   points(y=fitted.norm, x=temp, col="green", pch=3, cex=0.5)
  plot(segmented.mod, add=T, col="red", lwd=2)
  lines.segmented(segmented.mod, col="red", lwd=2)  # draws the location of cp and its confidence interval on current plot
  legend("topright", legend=c("Original values", "Normalized values"), pch=c(19, 3), col=c("grey30", "green"), bty="n", cex=0.8)
  
  # (c) time-series of gas cons, outside temperature, and elec overlayed
  plot.data <- cbind.data.frame(Elec.Cons=elec.cons, Gas.Cons=gas.cons, Temperature=temp)
  DrawTimeSeries(plot.data=plot.data, dates=gas.elec.weather.merged$date,
                 main.t="Time series of gas and electricity consumption \nand temperature (normalized to (0,1))",
                 y.t="kWh/kWh; Therm/Therm; F/F", color=NULL, scale.cols=T)
  
  # (d) important variables
  ln <- which(users.info$SP_ID==elec.id)
  text = c(paste("SP ID:", users.info[ln,"SP_ID"],
                 "\nFuel:", users.info[ln,"FUEL"],
                 "\nZipcode:", users.info[ln,"POSTAL_CODE"],
                 "\nClimate Zone:", users.info[ln,"CLIMATE_BAND"],
                 "\nCity:", users.info[ln,"CITY"],
                 "\nCounty:", users.info[ln,"COUNTY"],
                 "\nEnvironmentally conscious:", users.info[ln,"ENVIRONMENT_OR_WILDLIFE"],
                 "\nAttention to environ. issues:", users.info[ln,"ENVIRONMENTAL_ISSUES"],
                 "\nGreen living:", users.info[ln,"GREEN_LIVING"],
                 "\nHigh tech living:", users.info[ln,"HIGH_TECH_LIVING"],
                 "\nEducation:", users.info[ln,"EDUCATION"],
                 "\nCluster:", users.info[ln,"Cluster"],
                 "\nNet Metering:", users.info[ln,"NEM_FLAG"],
                 "\nEnrollment in EE programs:", users.info[ln,"TIMESENROLLED"],
                 "\nRebate type:", users.info[ln,"rebate_type"]
  ))
  library(grid)
  #grid.newpage()
  grid.text(text, x=unit(0.55, "npc"), y=unit(0.45, "npc"), just=c("left", "top"), 
            gp=gpar(fontsize=10, col="grey10"))
  
}


###############################################################################
### Function: draws an oveview of gas consumption of a given user
# Args:
#   id: SP_ID of interest
#   elec.feat: electricity load feature of interest. One of the following choices:
#     "daily.total.orig" "daily.min.orig"   "daily.max.orig"   "daily.cons.norm"  "daily.max.norm"
#   gas.feat: gas consumption feature of interest. One of the following choices:
#     "gas.daily.orig" "gas.daily.norm"
#   temp.feat: outside temperature of interest. One of the following choices:
#     "average" "maximum" "minimum"
# Returns:
#   plots a 2x2 panel plot:
#     panel 1: scatterplot of daily gas consumption versus elec consumption
#     panel 2: scatterplot of gas consumption versus temperature
#     panel 3: time-series of gas cons, outside temperature, and elec overlayed
#     panel 4: important variables info for that service id
VisualizeGasCons <- function(id=6731398510, elec.feat="daily.total.orig", gas.feat="gas.daily.orig", temp.feat="average") {  
  if(is.null(users.info)) users.info <- read.csv(paste("C:/Users/amir/Documents/_My_Academics__General/R/R_Projects/Wharton/Data/all_users_vars.csv",sep=""))
  zip <- unique(users.info$POSTAL_CODE[which(users.info$SP_ID==sp.id)]); print(zip)
  prem.id <- users.info$PREMISE_ID[which(users.info$SP_ID == id)][1]; print(prem.id)
  gas.id <- users.info$SP_ID[which((users.info$PREMISE_ID == prem.id) & (users.info$SVC_TYPE_CD == "G"))][1]; print(gas.id)
  elec.id <- users.info$SP_ID[which((users.info$PREMISE_ID == prem.id) & (users.info$SVC_TYPE_CD == "E"))][1]; print(elec.id)
  if( (is.null(gas.id) || length(gas.id)==0 || is.na(gas.id)) || 
        (is.null(elec.id) || length(elec.id)==0 || is.na(elec.id)) ||
        (is.null(prem.id) || length(prem.id)==0 || is.na(prem.id)) ) {
    print("Data not available.")
    return(NULL)
  }
  
  # Read weather data
  zip.weather <- ReadFile(paste("C:/Users/amir/Documents/_My_Academics__General/R/R_Projects/Wharton/Data/Weather/", zip, "weather.csv", sep=""))
  if( is.null(zip.weather) || nrow(zip.weather)==0 ) {
    print("No weather data on file")
    return(NULL)  #|| zip.weather.error==1
  }
  tout.matrix <- CalcToutMatrix(zip.weather)

  # Read elec data (maybe delete this later?)
  zip.load.features <- ReadFile(paste("C:/Users/amir/Documents/_My_Academics__General/R/R_Projects/Wharton/Data/LoadFeatures/daily_load_features_", zip, ".csv", sep="")) 
  if( is.null(zip.load.features) || nrow(zip.load.features)==0 ) {
    print("No electricity data.")
    return(NULL)
  }
  user.load.features <- zip.load.features[which(zip.load.features$SP_ID==elec.id),]  
  if (is.null(user.load.features) || nrow(user.load.features)==0 ) {  # ## even though an ID may be present both in the elec consumption data and gas cons data, its data might have not passed the verification process and hence deleted from the database
    print("Load features not available.")
    return(NULL)
  }  
  
  # get gas data
  gas.daily <- ReadFile(paste("C:/Users/amir/Documents/_My_Academics__General/R/R_Projects/Wharton/Data/GasDaily/gas_daily_", zip, ".csv", sep="")) 
  if( is.null(gas.daily) || nrow(gas.daily)==0 ) {
    print("No gas data on file")
    return(NULL) 
  }
  
  ### Merge gas, elec, weather
  gas.elec.merged <- merge(user.load.features, gas.daily, by="date")  # , all.x=T, all.y=T
  gas.elec.merged$date <- as.Date(gas.elec.merged$date)
  gas.elec.weather.merged <- merge(gas.elec.merged, tout.matrix, by="date")  # , all.x=T, all.y=T
  print(paste("Merged elec, gas, temp data size:", toString(dim(gas.elec.weather.merged))))
  if(nrow(gas.elec.weather.merged)==0) {
    print("Merging not successful.")
    return(NULL) 
  }

  # Call gas weather normalization function
  model.error <- 0  
  tryCatch(gas.daily.norm.model <- NormalizeForWeatherGas(gas.elec.weather.merged=gas.elec.weather.merged, temp.metric="average"), error=function(err)  model.error <- 1) 
  if(model.error) {
    print("Gas weather normalization model did not coverge.")
  } else {
    print("Gas weather normalization model coverged. Plotting now.")    
  }

  # Call respective function to plot the consumption data
  VisualizeGasFit(gas.daily.norm.model, gas.elec.weather.merged, elec.id=id, elec.metric="daily.total.orig")
  
}


###############################################################################
### Function: plots total gas consumption versus total HDD; and total elec cons v total CDD for all users on the same plot
#   Method:   loops through all users. Reads their elec, gas, and temp data from disk, merges all the data, and call GetGasElecDegreeDays() function
#             Once it reads all the data, it writes the data to disk (elec.cons.J, gas.cons.J, CDD, HDD for each user) and finally plots them.
PlotGasElecDegreeDays <- function() {
  all.gas.elec.dd <- NULL
  zips.gas.elec.dd <- NULL  
  files <- list.files("C:/Users/amir/Documents/_My_Academics__General/R/R_Projects/Wharton/Data/CDDHDD/")
  for (f in 1:length(files)) {
    zip <- as.numeric(substr(files[f], 17, 21)); print(zip)
    data <- read.csv(paste("C:/Users/amir/Documents/_My_Academics__General/R/R_Projects/Wharton/Data/CDDHDD/", files[f], sep=""))
    zip.data <- cbind(zip, elec.cons.J=mean(data$elec.cons.J), gas.cons.J=mean(data$gas.cons.J), cdd=mean(data$CDD), hdd=mean(data$HDD))  
    all.gas.elec.dd <- rbind(all.gas.elec.dd, data)
    zips.gas.elec.dd <- rbind(zips.gas.elec.dd, zip.data)
  }
  write.csv(all.gas.elec.dd, "C:/Users/amir/Documents/_My_Academics__General/R/R_Projects/Wharton/Data/all_users_gas_elec_dd.csv")
  write.csv(zips.gas.elec.dd, "C:/Users/amir/Documents/_My_Academics__General/R/R_Projects/Wharton/Data/all_zips_gas_elec_dd.csv")
  plot(y=all.gas.elec.dd$elec.cons.J, x=all.gas.elec.dd$CDD, col="blue",
       main="Elec. and Gas Consumption v. CDD/HDD", xlab="CDD and HDD", ylab="Elec. Cons. and Gas Cons. (Joules)")
  points(y=all.gas.elec.dd$gas.cons.J, x=all.gas.elec.dd$HDD, col="red")
  legend("bottomright", legend=c("Elec. Cons.", "Gas Cons."), col=c("blue", "red"), pch=1, bty="n")
  plot(y=zips.gas.elec.dd[,2], x=zips.gas.elec.dd[,4], col="blue",
       main="Zipcode-level Elec. and Gas Consumption v. CDD/HDD", xlab="CDD and HDD", ylab="Elec. Cons. and Gas Cons. (Joules)")
  points(y=zips.gas.elec.dd[,3], x=zips.gas.elec.dd[,5], col="red")
  legend("bottomright", legend=c("Elec. Cons.", "Gas Cons."), col=c("blue", "red"), pch=1, bty="n")
}

###############################################################################
###############################################################################
###############################################################################
# Args:
#   estimates: a list containing results of piecewose regression fits for each hour of the day (each element of the list is for one hour)
#   colorMap: a vector containing color values for each data point in the vector
#   type: an indicator for the type of plot. One of these three options: 
#     summary   a 2x2 plot containing: (a) heatmap of consumption values; (b) consumption time series; 
#               (c) time series of daily average outside temperature with overlay daily consumption ; (d) consumption versus outside temperature 
#     temp      plots consumption versus outside temperature with different colors for each hour of the day
#     hourly    
#   r, a list with following components:
#     r$id      unique identifier (just for the title)
#     r$zip     zipcode for the title
#     r$days    (1 date per day of data)
#     r$kw      (vector of kw readings)
#     r$kwMat   (matrix of kw readings with 24 columns)
#     r$toutMat (matrix of Tout readings with 24 columns)

plot.ResDataClass = function(r, colorMap=NA, main=NA, type='summary', estimates=NA) {
  if(type=='summary') {
    # General canvas settings
    if(is.na(main)) { main <- paste(r$id,' (',r$zip,') summary info',sep='') }
    if(length(colorMap) < 2) { colorMap = rev(colorRampPalette(brewer.pal(11,"RdBu"))(100)) } #colorMap = heat.colors(100)
    op <- par(no.readonly = TRUE)
    par( mfrow=c(2,2), oma=c(2,0,3,0),mar=c(2,2,2,2))# Room for the title
    
    # plot (a) the heatmap of consumption values
    #plot(r$kw,xlab='Date',ylab='kWh/h',main='Raw usage')
    image(t(as.matrix(r$kwMat)), col=colorMap, axes = FALSE, main='kW')
    axis(1, at = seq(0, 1, by = 1/6),labels=0:6 * 4,mgp=c(1,0,0),tcl=0.5)
    axis(2, at = seq(1,0, by = -1/15),labels=format(r$days[seq(1/16, 1, by = 1/16) * length(r$days)],'%m.%d'),las=1,mgp=c(1,0,0),tcl=0.5)
    #hmapHourly(,yvals=r$days,colorMap=colorMap,log=TRUE,main='Heatmap',mgp=c(1,0,0),tcl=0.5) # axis label on row 1, axis and ticks on 0, with ticks facing in
    
    # plot (b) consumption time series 
    end = min(length(r$kw), 240)  # at least show 240 days on the plot
    plot(r$dates[1:end], r$kw[1:end], type='l', xlab='Date', ylab='kWh/h', main='Raw usage zoom', mgp=c(1,0,0), tcl=0.5)
    
    # plot (c) daily average outside temperature time series
    plot(r$days, rowMeans(r$toutMat), col='grey', axes=F, ylab='', xlab='', mgp=c(1,0,0), tcl=0.5)
    axis(4, pretty(c(0, 1.1*rowMeans(r$toutMat)), n=5), col='grey', col.axis='grey', mgp=c(1,0,0), tcl=0.5)
    mtext("T out (F)", side=4, line=1, cex=0.9, col='grey')
    par(new=T) # plot the next plot call on the same figure as previous
    # overlay daily consumption on daily outside temperature plot
    plot(r$days, rowSums(r$kwMat), ylab='kWh/day', xlab='Day', main='kWh/day', mgp=c(1,0,0), tcl=0.5)
    
    # plot (d) consumption versus outside temperature    
    plot(rowMeans(r$toutMat), rowSums(r$kwMat), main='kWh/day vs mean outside temp (F)',
         xlab='mean T (degs F)', ylab='kWh/day', mgp=c(1,0,0), tcl=0.5)
    #par(op) # Leave the last plot
    mtext(main, line=0, font=2, cex=1.2,outer=TRUE)
    par(new=F)
    par(op)
    #heatmap(as.matrix(r$kwMat),Rowv=NA,Colv=NA,labRow=NA,labCol=NA)

  } else if(type=='temp') {
    if(is.na(main)) { main <- paste(r$id,' (',r$zip,') temperature info',sep='') }
    colors = rainbow(24)
    plot(r$tout, r$kw, xlab='Tout', ylab='kW', main=main, type='p', col=mapColors(r$dates$hour, colors)) # rainbow(n, start=2/6, end=1)
    legend('right', paste("hr ", 1:24), fill=colors, ncol = 1, cex = 0.8)

    # plots consumption versus temperature. uses different colors for different hours of day
  } else if(type=='hourly') {
    if(is.na(main)) { main <- paste(r$id,' (',r$zip,') hourly info',sep='') }
    op <- par(no.readonly = TRUE)
    grid = cbind(matrix(c(1:24), nrow=4, ncol=6, byrow=TRUE), c(25,25,25,25))
    layout(grid, widths=c(rep(2,6), 1))
    par(oma=c(2,3,3,0), mar=c(1,0,1,0))# Room for the title
    
    pallete = rainbow(12) #,start=2/6, end=1)
    colvals = r$dates$mon
    #colvals = r$dates$wday
    #colvals = r$dates$wday == 0 | r$dates$wday == 6 # 0 = Sun, 6 = Sat
    colors = mapColors(colvals, pallete)  # different months of the year, each with one color
    xlm = c(min(r$tout,na.rm=TRUE), max(r$tout, na.rm=TRUE))
    ylm = c(min(r$kw, na.rm=TRUE), max(r$kw, na.rm=TRUE))
    for(i in 0:23) {
      yax = 'n' # supress axis plotting
      xax = 'n'
      if(i %%  6 == 0) yax = 's' # standard y-axis for first of row
      if(i %/% 6 == 3) xax = 's' # standard x-axis for bottom row
      sub = r$dates$hour==i
      # kwh versus temperature, each hour a different color
      plot(subset(r$tout,sub),subset(r$kw,sub),
           col=subset(colors,sub),
           xlim=xlm, ylim=ylm, yaxt=yax, xaxt=xax 
      )
      if(length(estimates) > 1) {  # when estimates is a data.frame, length() returns number of rows
        if(length(estimates[,i+1]) > 1) {  # one column for each hour (remember that it is one day per row)
          fit = estimates[,i+1]
          color = 'black'
          if (fit['AIC_0'] < fit['AIC_cp']) color = 'gray'
          if (fit['nullModelTest'] > 0.1)   color = 'red'
          par(new=T)
          qe = quickEst(fit['cp'],fit['(Intercept)'],fit['lower'],fit['upper'],
                        c(min(subset(r$tout, sub), na.rm=T), max(subset(r$tout, sub), na.rm=T)))
          # plots the piecewise regression line
          plot(qe[,1], qe[,2], type='l', xlim=xlm, ylim=ylm, axes=F, lwd=2, col=color )
          par(new=F)
        }
      }
      grid()
      text(mean(xlm), ylm[2] * 0.95, paste('hr',i), font=2, cex=1.2)
    }
    mtext(main, line=0, font=2, cex=1.2,outer=TRUE)
    #par(xpd=TRUE)
    #plot.new()
    plot(1, type = "n", axes=FALSE, xlab="", ylab="")
    legend('center', month.abb, fill=pallete, ncol = 1, cex = 1)
    #par(xpd=FALSE)
    par(op)
  }
  
}

