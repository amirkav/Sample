###############################################################################
######## Log-Normal, Auto-Regressive, Cross-Validated Regression Model ########
###############################################################################
# wrapper function. goes thorugh all buildings, fits the model, predicts into future 
# and calculates savings
# saves MAPE, APE, R2, and Savings of the models to disk
PredictionModel <- function(month.start=13, month.end=49, wd, ks.all) {  #, month.forward=12, mo.ahead.deviations
  month.back <- 12 #min((m-2), month.back)  # how many months back to use for creating the model  #12  # max(12, month.start - 2)
  savings <- data.frame(matrix(rep(0, nrow(ms.data)*(month.end-month.start+1)), nrow=nrow(ms.data)))
  names(savings) <- paste(substr(names(site.ener.inten[month.start:month.end]), 5, 15), sep=".")
  for (ks in ks.all) {  #1:nrow(ms.data)
    if(length(ks) == 1) {
      bldgname <- ms.data$Building.Name[ks]
    } else {
      bldgname <- "Multiple Buildings."
    }
    print(bldgname)
    for (month in month.start:month.end) {  #month.start:month.end
      # Fit
      fit <- SingleBldgFit_Step2(ks=ks, month=month, month.back=month.back)
      summary(fit[[1]])  # fit <- fit[[1]]
      # Predict
      if (fit[[2]] > -1) {
        pred.meas <- PredictionError2(month, month.back=month.back, ks=ks, fit=fit[[1]], 
                                     bldgname=bldgname)
        savings[ks, (month-month.start)+1] <- pred.meas$Predicted[1] - pred.meas$Actual[1]
      }
    }
  
  }
  write.csv(savings, paste(wd, "/Results/Savings_v2.csv", sep=""))
}

###############################################################################
### Estimates prediction error of a model by cross-validation
CrossVal_Step <- function(k=10, fit.data, fit) {
  test.size <- nrow(fit.data)  # max(floor(nrow(fit.data)/k),1)
  rmses <- rep(0, k)
  # codes that are commented out are for sampled cross-validation. 
  # new codes are leave-one-out. 
  # for small data sets use leave-one-out; if the data set gets too big for LOO, use sampling 
  for (i in 1:nrow(fit.data)) {  # (i in 1:k)
    ind <- i # sample(x=1:nrow(fit.data), size=test.size, replace=T)
    train <- fit.data[-ind, ]
    test <- fit.data[ind, ]
    
    fit.train <- lm(formula=fit$call, data=train)  
    fitted.test <- predict(fit.train, newdata=test)
    rmses[i] <- sqrt(sum((test$cons-fitted.test)^2)/test.size)
  }
  mean(rmses)
}

###############################################################################
### Finds best changepoint to be used in degree-days calculations by cross-validation
FindBestCP_CV <- function(ks=NULL, month=12) {
  cps <- seq(51, 79, by=2)
  rmses <- rep(0, length(cps))
  for (i in 1:length(cps)) {
    fit <- SingleBldgFit_Step2(ks=ks, month=month, cp=cps[i])  
    rmses[i] <- fit[[2]]
  }
  cp.best <- cps[which.min(rmses)]
  print(paste("Best change point is", cp.best, "F."))
  cp.best
}

FindBestCP <- function(bldgname, year) {
  cps <- seq(51, 79, by=2)
  r2s <- rep(0, length(cps))
  for (i in 1:length(cps)) {
    fit <- SingleBldgFit(bldg.name=bldgname, year=year, cp=cps[i])
    r2s[i] <- summary(fit)$r.squared
  }
  cp.best <- cps[which.max(r2s)]
}

###############################################################################
### Simple imputation function
ImputeForNA <- function(data) {
  for (i in 1:ncol(data)) {
    data[!is.finite(data[,i]),i] <- NA
    if (sum(is.na(data[,i])) > 0) {
      data[which(is.na(data[,i])), i] <- mean(data[max(1, which(is.na(data[,i]))-1), i],
                                              data[max(1, which(is.na(data[,i]))-2), i],
                                              data[which(is.na(data[,i]))+1, i],
                                              data[which(is.na(data[,i]))+2, i], 
                                              na.rm=T)
    }
  }
  data
}

###############################################################################
# Regression model that chooses variables by stepwise selection
# same function as SingleBldgFit, but with flexibility to use any month not just fiscal years
SingleBldgFit_Step2 <- function(ks=1, month=13, cp=NULL, month.back=12) {  # bldg.name=NULL,   
  na.remove <- (length(ks) > 0)
  if(is.null(cp)) cp <- FindBestCP_CV(ks=ks, month=month)
  m <- month  # current month. i.e., the month at which we want to make the prediction
  m.back <- min((m-2), month.back)  # how many months back to use for creating the model
  
  # Degree Days 
  tot.dd <- cdd.hdd.reftemp[[paste("X", cp, ".HDD", sep="")]] + 
    cdd.hdd.reftemp[[paste("X", cp, ".CDD", sep="")]]
  
  # Compile fit data
  fit.data <- data.frame(
    cons          = log(as.numeric(colSums(site.ener.inten[ks, (m-m.back):(m-1)], na.rm=na.remove))), #log
    cons.d1       = log(as.numeric(colSums(site.ener.inten[ks, (m-m.back-1):(m-2)], na.rm=na.remove))), #log
    hc            = as.numeric(colSums(hc[ks, (m-m.back):(m-1)], na.rm=na.remove)),
    dd            = as.numeric(tot.dd[(m-m.back):(m-1)]),
    dd1           = as.numeric(tot.dd[(m-m.back-1):(m-2)]),
    temp.max      = weather.detailed$Max.TemperatureF[(m-m.back):(m-1)],
    hum.mean      = weather.detailed$Mean.Humidity[(m-m.back):(m-1)],
    press.mean    = weather.detailed$Mean.Sea.Level.PressureIn[(m-m.back):(m-1)],
    precipitation = weather.detailed$Precipitation[(m-m.back):(m-1)],
    cloud.cover   = weather.detailed$CloudCover[(m-m.back):(m-1)]
  )
  fit.data <- ImputeForNA(fit.data)
  # Model selection
  if (sum(is.na(fit.data)) > 0) {
    cv.rmse <- -1
    return(list(Step.Fit=NULL, CV.RMSE=cv.rmse))
  }
  
  stepwise.model.err <- 0
  full.model <- lm(cons ~ ., data=fit.data)
  base.model <- lm(cons ~ 1, data=fit.data)  
  tryCatch(fit <- fit.step <- step(base.model, scope=list(upper=full.model, lower=~1 ), 
                                   direction = "forward", trace=F),
           error=function(err)  stepwise.model.err <- 1)
  if(!stepwise.model.err) {
    cv.rmse <- CrossVal_Step(k=20, fit.data, fit)  
    list(Step.Fit=fit, CV.RMSE=cv.rmse)
  } else {
    cv.rmse <- -1
    return(list(Step.Fit=NULL, CV.RMSE=cv.rmse))
  }
}

### Helper function to add NA in the beggining or end of a vector
PadNA <- function(vec, length, side="right") {
  vec.temp <- rep(NA, length)
  if (side=="left") {
    vec.temp[(length-length(vec)+1):length] <- vec
  } else {
    vec.temp[1:length(vec)] <- vec
  }
  vec.temp
}

###############################################################################
### Predicts consumption values on future months based on the given fit, and 
# calculates error values MAPE, APE
PredictionError2 <- function(month, month.back, ks, fit, bldgname, res.adj=0,
                            step.by.step=F) {
  na.remove <- (length(ks) > 0)
  months.pred <- month# c((month+1):(min((month+month.forward), ncol(site.ener.inten)))) ##*
  m.back <- min((month-2), month.back)
  months.fit <- c((month-m.back):(month-1)) # c((month-m.back+1):month)
  months.all <- c(months.fit, months.pred)
  
  pred.data <- data.frame(
    cons          = log(as.numeric(site.ener.inten[ks, months.pred], na.rm=na.remove)), #log
    cons.d1       = log(as.numeric(site.ener.inten[ks, months.pred-1], na.rm=na.remove)), #log  #unrealistic because we never have them in advance
    hc            = as.numeric(hc[ks, months.pred], na.rm=na.remove),
    dd            = as.numeric(tot.dd[months.pred]),
    dd1           = as.numeric(tot.dd[months.pred-1]),  # unrealistic because we never have them in advance
    temp.max      = weather.detailed$Max.TemperatureF[months.pred],
    hum.mean      = weather.detailed$Mean.Humidity[months.pred],
    press.mean    = weather.detailed$Mean.Sea.Level.PressureIn[months.pred],
    precipitation = weather.detailed$Precipitation[months.pred],
    cloud.cover   = weather.detailed$CloudCover[months.pred]
  )
  if (is.null(fit)) { # if prediction model fails, use past data as predictions for future
    pred <- as.numeric(site.ener.inten[ks, (month-1)], na.rm=na.remove)
    return()
  }
  # if the fit is acceptable, then calculate predicted values and compare with measured
  measure.pred <- as.numeric(site.ener.inten[ks, months.pred], na.rm=na.remove)
  measure.fit <- as.numeric(site.ener.inten[ks, months.fit], na.rm=na.remove)
  measure.all <- as.numeric(site.ener.inten[ks, months.all], na.rm=na.remove)

  pred <- predict(fit, newdata=pred.data) #+ res.adj
  pred <- exp(pred) #log (use exp())
  
  ### Converting from energy intensity to total energy consumption
  floor.area <- sum(ms.data$Office.Space[ks], na.rm=na.remove)
  residuals <- (measure.fit - exp(fit$fitted.values))*floor.area  # fit residuals
  deviations <- (pred - measure.pred)*floor.area  # prediction error
  
  se <- sd(residuals)
  predicted <- pred*floor.area
  measured.fit <- measure.fit*floor.area
  measured.pred <- measure.pred*floor.area
  measured.all <- measure.all*floor.area
  predicted.upper <- predicted + se
  predicted.lower <- predicted - se
  fitted.vals <- exp(fit$fitted.values)*floor.area
  y.m <- 100*(ceiling((max(c(predicted.upper, measured.all), na.rm=T) + se)/100))
  
  ### Diagnostic plot to check if the residuals or prediction errors are serially correlated
  png(file=paste(wd, "/Results/", 
                 bldgname, "_", substr(names(site.ener.inten), 5, 15)[month], 
                 "_ResidualsPredictions_Diagnostics_v2.png", sep=""), 
      width=9, height=5.58, units="in", res=300)
  par(mfrow=c(2,2))
  plot(residuals/(max(measure.fit, na.rm=T)*floor.area), type="b", ylim=c(-1,1),
       main=paste("Normalized Residuals for", bldgname), ylab="Residuals / Max(Actual)",
       xlab="Month")
  abline(h=c(-0.1, 0.1), lty="dashed", col="grey60")
  plot(deviations/(max(measure.pred, na.rm=T)*floor.area), type="b", ylim=c(-1,1),
       main=paste("Normalized Prediction Error for", bldgname), ylab="Prediction Error / Max(Actual)",
       xlab="Month")  
  abline(h=c(-0.1, 0.1), lty="dashed", col="grey60")
  acf(residuals)
  acf(deviations)
  dev.off()
  
  ### GGPLOT of actual, fitted, predicted, confidence interval
  months <- c("07-2008", "08-2008", "09-2008", "10-2008", "11-2008", "12-2008",
              "01-2009", "02-2009", "03-2009", "04-2009", "05-2009", "06-2009",
              "07-2009", "08-2009", "09-2009", "10-2009", "11-2009", "12-2009",
              "01-2010", "02-2010", "03-2010", "04-2010", "05-2010", "06-2010",
              "07-2010", "08-2010", "09-2010", "10-2010", "11-2010", "12-2010",
              "01-2011", "02-2011", "03-2011", "04-2011", "05-2011", "06-2011",
              "07-2011", "08-2011", "09-2011", "10-2011", "11-2011", "12-2011",
              "01-2012", "02-2012", "03-2012", "04-2012", "05-2012",
              "06-2012", "07-2012"
  )
  
  plot.data <- data.frame(months.all=months.all,
                          predicted=PadNA(predicted, length(months.all), "left"),
                          predicted.upper=PadNA(predicted.upper, length(months.all), "left"),
                          predicted.lower=PadNA(predicted.lower, length(months.all), "left"),
                          measured.all=measured.all, 
                          fitted.vals=PadNA(fitted.vals, length(months.all), "right"),
                          months.pred=PadNA(months.pred, length(months.all), "left"),
                          months.fit=PadNA(months.fit, length(months.all), "right")
                          )

  last.touch.fit <- data.frame(ys=c(fitted.vals[length(months.fit)], predicted[1]),
                            xs=c(months.fit[length(months.fit)],months.fit[length(months.fit)]+1)) 
  last.touch.meas <- data.frame(ys=c(measured.all[length(months.fit)], measured.all[length(months.fit)+1]),  # measured[length(months.fit)+1]
                            xs=c(months.fit[length(months.fit)],months.fit[length(months.fit)]+1))
  
  n.month <- length(months.all)
  breaks <- months.all[1] - 1 + c(1+3*(0:(round(n.month/3,0)+1)))
  
      
  # Setting up the canvas and axes
  a <- ggplot(data=plot.data, aes(x=months.all, y=predicted))
  a <- a + scale_y_continuous(limits = c(0,y.m), name="Monthly Energy Consumption (MBtu)", labels=comma)
  a <- a + scale_x_continuous(limits = c(months.all[1], months.all[length(months.all)]), name="Time",  #months.all #c(1,n.month)
                              breaks = breaks,
                              labels = months[breaks])
  # Line colors and legend
  a <- a + scale_linetype_manual(values = 
                                   c("Measured" = "solid",
                                     "Fitted or Predicted" = "dashed", 
                                     "Energy Conservation Measure" = "dotted"
                                   ))
  a <- a + geom_line(data=plot.data, aes(y = measured.all, x=months.all, 
                                         linetype = "Measured"), size=1.0, show_guide=F) #, colour="Measured" 
  a <- a + geom_line(data=plot.data, aes(y = predicted, x=months.all,  #months.all 
                                         linetype = "Fitted or Predicted"), size=1.0, show_guide=F) #, colour="Measured" 
  a <- a + geom_line(data=plot.data, aes(y = fitted.vals, x=months.all, 
                                         linetype = "Fitted or Predicted"), size=0.8, show_guide=F) #, colour="Fitted or Predicted" 
  a <- a + geom_line(data=last.touch.fit, aes(y=ys, x=xs, 
                                              linetype = "Fitted or Predicted"), size=0.8, show_guide=F) #, colour="Fitted or Predicted"
  a <- a + geom_line(data=last.touch.meas, aes(y=ys, x=xs, 
                                               linetype = "Measured"), size=1.0, show_guide=F) #, colour="Fitted or Predicted"
  # title and other plot settings
  a <- a + ggtitle(paste("Energy Consumption (Actual, Fitted, Predicted) for", bldgname)) +
    theme(legend.title=element_blank(),
          legend.direction="vertical",  # horizontal
          legend.box="vertical",  # horizontal
          legend.position="right")  # bottom 
  #   a <- a + guides(fill=guide_legend(keywidth=1.5, keyheight=1.5))
  a
  ggsave(paste(wd, "/Results/",
               bldgname, "_", substr(names(site.ener.inten), 5, 15)[month], 
               "_PredictionError_ggp_v2.png",sep=""), 
                plot = a, width=9, height=5.58, dpi=600)
  
  list(RMSE = rmse, Predicted = predicted, Actual = measured.pred, APE.next = ape.next)
}

###############################################################################
### Fits a monthly consumption model to all the buildings whose index is passed to the function. 
### Each model is fit to time-series monthly consumption of a single building in baseline year
### and subsequent years are predicted using that model.
### Calls PredictPlot() to visualize the fit.
### Saves predicted and measured monthly values to a csv file.
### Saves R2 and coefficients values of each models to a csv file.
VerificationModel <- function(bldgs.ind, site.ener.nolab.mbtu, elec.cons.lab.mbtu, 
                      ms.data, best.changepoint, cdd.hdd.reftemp, hc, lognormal=F, wd) {
  # preparing variables and tables
  bldgs.ind <- 1:nrow(ms.data)
  lognormal=F
  site.ener.inten <- site.ener.nolab.mbtu / ms.data$Office.Space; print(dim(site.ener.inten))  # Total.Floor.Area     
  elec.cons.lab.mbtu <- data.frame(elec.cons.lab.mbtu); print(dim(elec.cons.lab.mbtu))
  names(elec.cons.lab.mbtu) <- names(site.ener.nolab.mbtu)
  lab.load.inten <- elec.cons.lab.mbtu / ms.data$Total.Floor.Area; print(dim(lab.load.inten))  # Lab.Space
  # preparing tables to store regression results
  column.names <- c("Building.Name", "R2.10", "Adj.R2.10", "ChangePoint.10", "Intercept.10", "DD.coeff.10", "HC.coeff.10", "Lab.coeff.10",
                    "R2.11", "Adj.R2.11", "ChangePoint.11", "Intercept.11", "DD.coeff.11", "HC.coeff.11", "Lab.coeff.11",
                    "R2.12", "Adj.R2.12", "ChangePoint.12", "Intercept.12", "DD.coeff.12", "HC.coeff.12", "Lab.coeff.12")
  n.col <- (length(column.names)-1)/3
  pred.meas <- data.frame(row.names=c(month.abb, month.abb)[7:18])  # financial year from July to July
  models.summ <- data.frame(matrix(nrow=length(bldgs.ind), ncol=length(column.names)))  ###<<<TODO>>> ncol=7 change to parametrized
  models.summ[,1] <- ms.data$Building.Name[bldgs.ind]
  names(models.summ) <- column.names
  base.years <- c("2009", "2010", "2011", "2012")
  
  # loops over all buildings, fits a model to the baseline year, 
  # calculates the energy savings for subsequent years, 
  # plots and saves the model results and fit metrics to disk 
  for (j in 1:length(bldgs.ind)) {  
    k <- bldgs.ind[j]; print(bldgname <- ms.data$Building.Name[k])
    ### Prepare fit data (whether data will be added later)
    # 2009
    site.ener.inten.bldg.09 <- as.numeric(site.ener.inten[k, which(names(site.ener.inten)=="kWh.July.08"):which(names(site.ener.inten)=="kWh.Jun.09")])  # kWh.Jan.09; kWh.Dec.09
    hc.bldg.09              <- as.numeric(hc[k, which(names(hc)=="Total.HC.July.08"):which(names(hc)=="Total.HC.Jun.09")])
    lab.perc.bldg.09        <- as.numeric(lab.load.inten[k, which(names(lab.load.inten)=="kWh.July.08"):which(names(lab.load.inten)=="kWh.Jun.09")])
    # 2010
    site.ener.inten.bldg.10 <- as.numeric(site.ener.inten[k, which(names(site.ener.inten)=="kWh.Jul.09"):which(names(site.ener.inten)=="kWh.Jun.10")])
    hc.bldg.10              <- as.numeric(hc[k, which(names(hc)=="Total.HC.Jul.09"):which(names(hc)=="Total.HC.Jun.10")])
    lab.perc.bldg.10        <- as.numeric(lab.load.inten[k, which(names(lab.load.inten)=="kWh.Jul.09"):which(names(lab.load.inten)=="kWh.Jun.10")])
    # 2011
    site.ener.inten.bldg.11 <- as.numeric(site.ener.inten[k, which(names(site.ener.inten)=="kWh.Jul.10"):which(names(site.ener.inten)=="kWh.Jun.11")])
    hc.bldg.11              <- as.numeric(hc[k, which(names(hc)=="Total.HC.Jul.10"):which(names(hc)=="Total.HC.Jun.11")])
    lab.perc.bldg.11        <- as.numeric(lab.load.inten[k, which(names(lab.load.inten)=="kWh.Jul.10"):which(names(lab.load.inten)=="kWh.Jun.11")])
    # 2012
    site.ener.inten.bldg.12 <- as.numeric(site.ener.inten[k, which(names(site.ener.inten)=="kWh.Jul.11"):which(names(site.ener.inten)=="kWh.Jun.12")])
    hc.bldg.12              <- as.numeric(hc[k, which(names(hc)=="Total.HC.Jul.11"):which(names(hc)=="Total.HC.Jun.12")])
    lab.perc.bldg.12        <- as.numeric(lab.load.inten[k, which(names(lab.load.inten)=="kWh.Jul.11"):which(names(lab.load.inten)=="kWh.Jun.12")])
    
    # vectors to keep predicted and measured values for plotting 
    pred.all <- as.numeric()  
    meas.all <- as.numeric()
    # Loops through available years, sets the baseline to year i-1, and calculates savings for the year i 
    for (y in 1:(length(base.years)-1)) {  
      base.year <- base.years[y]; print(base.year) #toString(best.changepoint$Best.BaseYear[j]); print(base.year)      
      # Adding whether effect (Degree Days)
      cp.err <- 0
      tryCatch(cp <- FindBestCP(bldgname=bldgname, year=base.year),
               error=function(err)  cp.err <- 1)
      if(cp.err) cp <- 65
      tot.dd <- cdd.hdd.reftemp[[paste("X", cp, ".HDD", sep="")]] + 
        cdd.hdd.reftemp[[paste("X", cp, ".CDD", sep="")]]
      # fiscal years
      dd.09 <- as.numeric(tot.dd[1:12])
      dd.10 <- as.numeric(tot.dd[13:24])
      dd.11 <- as.numeric(tot.dd[25:36])
      dd.12 <- as.numeric(tot.dd[37:48])
      
      ### Fit OLS
      fit.data <- switch(base.year, 
                         "2009"=cbind.data.frame(energy=site.ener.inten.bldg.09, dd=dd.09, hc=hc.bldg.09, lab=lab.perc.bldg.09), 
                         "2010"=cbind.data.frame(energy=site.ener.inten.bldg.10, dd=dd.10, hc=hc.bldg.10, lab=lab.perc.bldg.10) , 
                         "2011"=cbind.data.frame(energy=site.ener.inten.bldg.11, dd=dd.11, hc=hc.bldg.11, lab=lab.perc.bldg.11) ,
                         "2012"=cbind.data.frame(energy=site.ener.inten.bldg.12, dd=dd.12, hc=hc.bldg.12, lab=lab.perc.bldg.12) )
      if(lognormal) fit.data$energy <- log(fit.data$energy) # lognormal usually works for cross-section data, not lingitudinal models
      
      ####<<<TODO>>> FIX
      fit <- NA
      fit <- tryCatch(fit <- lm(energy ~ ., data=fit.data),
               error=function(err)  fit.err <- NA)
      if(length(fit) == 1) {
        pred <- fitted.values <- rep(NA, nrow(fit.data))
        r2 <- NA
        r2.adj <- NA
        coeffs <- rep(NA, 4)
        pred <- rep(NA, 12)
      } else {
        fitted.values <- fit$fitted.values
        r2 <- summary(fit)$r.squared#; print(r2)
        r2.adj <- summary(fit)$adj.r.squared
        coeffs <- t(summary(fit)$coefficients[,1])
        fit.coeffs <- rep(0, 4)
        fit.coeffs[1:length(coeffs)] <- t(summary(fit)$coefficients[,1])
        
        # Predict
        new.data <- switch(base.year,
                           "2009" = cbind.data.frame(energy=site.ener.inten.bldg.10, dd=dd.10, hc=hc.bldg.10, lab=lab.perc.bldg.10), 
                           "2010" = cbind.data.frame(energy=site.ener.inten.bldg.11, dd=dd.11, hc=hc.bldg.11, lab=lab.perc.bldg.11), 
                           "2011" = cbind.data.frame(energy=site.ener.inten.bldg.12, dd=dd.12, hc=hc.bldg.12, lab=lab.perc.bldg.12)) 
        pred <- predict(fit, newdata=new.data, na.action=na.pass) + resid(fit)
        # the model predicts building consumption (lab excluded). To plot total load, we add the lab cons here
        lab.inten <- switch(base.year, 
                            "2009"=cbind.data.frame(lab=lab.perc.bldg.10), 
                            "2010"=cbind.data.frame(lab=lab.perc.bldg.11) , 
                            "2011"=cbind.data.frame(lab=lab.perc.bldg.12) )
        if(lognormal) pred <- exp(pred)
        pred <- pred*ms.data$Office.Space[k] + lab.inten*ms.data$Total.Floor.Area[k]
        pred <- as.numeric(pred[,1])
      }  # end of if statement on linear model returning valid results
      ### write model summary to disk 
      meas <- switch(base.years[y+1], 
                     "2009" = site.ener.inten.bldg.09*ms.data$Office.Space[k] + lab.perc.bldg.09*ms.data$Total.Floor.Area[k],
                     "2010" = site.ener.inten.bldg.10*ms.data$Office.Space[k] + lab.perc.bldg.10*ms.data$Total.Floor.Area[k],
                     "2011" = site.ener.inten.bldg.11*ms.data$Office.Space[k] + lab.perc.bldg.11*ms.data$Total.Floor.Area[k],
                     "2012" = site.ener.inten.bldg.12*ms.data$Office.Space[k] + lab.perc.bldg.12*ms.data$Total.Floor.Area[k])
      pred.meas <- cbind.data.frame(pred.meas, pred, meas, avoided=(pred-meas))
      names(pred.meas)[(ncol(pred.meas)-2):ncol(pred.meas)] <- 
        paste(names(pred.meas)[(ncol(pred.meas)-2):ncol(pred.meas)], ".", as.numeric(base.year)+1, ".", bldgname, sep="")
      # A table for a csv file containing model fit summaries
      base.year.ind <- switch(base.year,
                              "2009"=1, 
                              "2010"=2 , 
                              "2011"=3 )
      models.summ[j,(2+(base.year.ind-1)*n.col):(2+(base.year.ind-1)*n.col+n.col-1)] <- c(r2, r2.adj, cp, as.numeric(fit.coeffs))
      
      # put all pred and meas values for this building in one data frame to plot later
      pred.all <- c(pred.all, pred)
      meas.all <- c(meas.all, meas)      
    }  # end of loop on base years
    PredictPlotLinear(pred=pred.all, meas=meas.all, year=as.numeric(base.year)+1, bldgname=bldgname, 
                      r2=r2, ulim=ceiling(max(c(pred.all, meas.all), na.rm=T)/100)*100, #ulim=ceiling(max(site.ener.nolab.mbtu, na.rm=T)*1000)/1000,
                      llim=floor(min(c(pred.all, meas.all), na.rm=T)/100)*100,
                      sqft=ms.data$Office.Space[k])
#     dev.off()
    
  }  # end of loop on buildings
  
  write.csv(pred.meas, paste(wd, "/Results/pred_meas_R_model.csv", sep=""))
  write.csv(models.summ, paste(wd, "/Results/models_summ", round(rnorm(1, 1000, 300)), ".csv", sep=""))
      
  # Saving annual saving values
  nb <- nrow(ms.data) # number of buildings
  n.years <- length(base.years) - 1  # the first year is only baseline. the subsequent years can be baseline and guaranteed period
  avoided.annual <- colSums(pred.meas[, n.years*1:(3*nb)])   # every third column is the avoided value hence multiplier 3.
  predicted.annual <- colSums(pred.meas[,(n.years*1:(3*nb)-2)])   # every third column is the avoided value hence multiplier 3.
  measured.annual <- colSums(pred.meas[,(n.years*1:(3*nb)-1)])   # every third column is the avoided value hence multiplier 3.
  names <- unlist(strsplit(names(avoided.annual), "\\."))
  n <- length(names)
  bldgnames <- names[n.years*c(1:(n/3))][3*1:nb]
  write.csv(data.frame(bldgnames, 
                       "Avoided.2010"=avoided.annual[n.years*1:nb-2], 
                       "Avoided.2011"=avoided.annual[n.years*1:nb-1], 
                       "Avoided.2012"=avoided.annual[n.years*1:nb], 
                       "Predicted.Site.wLab.KBtu.10"=predicted.annual[n.years*1:nb-2], 
                       "Predicted.Site.wLab.KBtu.11"=predicted.annual[n.years*1:nb-1],
                       "Predicted.Site.wLab.KBtu.12"=predicted.annual[n.years*1:nb],
                       "Actual.Site.wLab.KBtu.10"=measured.annual[n.years*1:nb-2], 
                       "Actual.Site.wLab.KBtu.11"=measured.annual[n.years*1:nb-1], 
                       "Actual.Site.wLab.KBtu.12"=measured.annual[n.years*1:nb]),
            paste(wd, "/Results/pred_meas_R_model_annual.csv", sep=""), 
            row.names=F)
  
  pred.meas
}

###############################################################################
PredictPlotLinear <- function(pred, meas, r2=1.0, dd.new=dd.10, bldgname,
                              hc.new=hc.bldg.10, lab.new=lab.perc.bldg.10, year=2010, ulim, llim,
                              sqft=100000, dates=NULL) {
  month.start = 13
  month.end = 48
  avoided <- pred - meas
  plotdata <- data.frame(Month=c(month.start:month.end),
                         Measured=meas,
                         Baseline=pred,
                         Energy.Star=as.numeric(estar.ratings[which(ms.data$Building.Name == bldgname), c(1:36)]),
                         SEEF=as.numeric(seef.ratings[which(ms.data$Building.Name == bldgname), c(1:36)]),
                         Avoided=avoided,
                         Cum.Avoided=cumsum(avoided))
  a <- ggplot(data=plotdata, aes(x=Month, y=Measured, ymin=Baseline, ymax=Measured, group=1))
  a <- a + scale_y_continuous(name="Energy Consumption (MBtu)")
  a <- a + scale_x_continuous(limits =c(month.start, month.end),name="Time",
                              breaks = c(month.start + 6*c(0:((month.end-month.start)/6))),
                              labels=months[c(month.start + 6*c(0:((month.end-month.start)/6)))])
  a <- a + scale_linetype_identity() 
  a <- a + scale_linetype_manual(values = 
                                 c("Measured" = "solid",
                                   "Baseline" = "dashed" 
                                   ))
  a <- a + geom_line(aes(y = Measured, linetype = "Measured"), size=1.0, color="steelblue") 
  a <- a + geom_line(aes(y = Baseline, linetype = "Baseline"), size=1.0, color="steelblue") 
  a <- a + geom_ribbon(alpha=0.3)  
  a <- a + ggtitle(paste("Energy Consumption")) + theme(legend.title=element_blank()) 
  a

  b <- ggplot(data=plotdata, aes(x=Month, y=SEEF, group=1))
  b <- b + scale_y_continuous(name="Energy Efficiency Rating", limits=c(0,100))
  b <- b + scale_x_continuous(limits =c(month.start, month.end),name="Time",
                              breaks = c(month.start + 6*c(0:((month.end-month.start)/6))),
                              labels=months[c(month.start + 6*c(0:((month.end-month.start)/6)))])
  b <- b + geom_line(aes(y = Energy.Star, col = "Energy Star"), size=1.0) 
  b <- b + geom_line(aes(y = SEEF, col = "SEEF"), size=1.0) 
  
  b <- b + ggtitle(paste("Energy Efficiency Rating")) + theme(legend.title=element_blank())  #element_blank() 
  b
  
  max.cumsum <- max(abs(cumsum(avoided)), na.rm=T)
  if(!(is.finite(max.cumsum) & !is.na(max.cumsum))) max.cumsum <- 1000
  c <- ggplot(data=plotdata, aes(x=Month, y=Avoided, group=1))
  c <- c + scale_y_continuous(name="Energy Savings (MBtu)", 
                              limits=c(-max.cumsum, max.cumsum))
  c <- c + scale_x_continuous(limits =c(month.start, month.end),name="Time",
                              breaks = c(month.start + 6*c(0:((month.end-month.start)/6))),
                              labels=months[c(month.start + 6*c(0:((month.end-month.start)/6)))])
  c <- c + geom_line(aes(y = Avoided, linetype = "Monthly"), size=1.0) 
  c <- c + geom_line(aes(y = Cum.Avoided, linetype = "Cumulative"), size=1.0) 
  c <- c + geom_hline(aes(yintercept=0), col="grey30")
  c <- c + ggtitle(paste("Energy Savings Compared to Baseline")) + theme(legend.title=element_blank()) 
  c
  
  
  library(gridExtra)
  png(file=paste(wd, "/Results/Pred_Meas_EStar_SEEF_Avoided_", bldgname, ".png", sep=""), 
      width=9, height=9, units="in", res=300) 
  all <- grid.arrange(a,c,b)
  dev.off()
    
  ### return predicted values
  pred
}


