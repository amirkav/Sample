# Loading data. functions, and R packages. Set the options beloe and run once before starting the analysis.
remove(list=ls())
##### OPTIONS #####
# change to the folder where the files are saved:
wd <- "C:/Users/amir/Documents/_My_Academics__General/R/R_Projects/R_Codes_Amir"
setwd(wd)
# select the time span of the analysis. month=1 is July 2008
month.start <- 13 
month.end <- 54
##### end of options #####

# INSTALLING PACKAGES AND PREPARING DATA
# install.packages("ggplot2"); 
library(ggplot2)
# install.packages("scales"); 
library(scales)
# install.packages("reshape"); 
library(reshape)
# cleansing data and loading functions
# source(paste(wd, '/func.R', sep=""))
# Reading data
ms.data <- ms.data.all     <- read.csv(paste(wd, "/Data/MS_Data_3.csv", sep=""), as.is=T)
cdd.hdd.all <- read.csv(paste(wd, "/Data/cdd_hdd_98052.csv", sep=""), as.is=T)
cdd.hdd.reftemp <- read.csv(paste(wd, "/Data/HDD_CDD_base_temps.csv", sep=""), as.is=T)
weather.detailed <- read.csv(paste(wd, "/Data/Seattle_Weather_Detailed.csv", sep=""), as.is=T)

eff.tab.all <- read.csv(paste(wd, "/Data/SEEF_EStarComparison_scores_all.csv", sep=""))
eff.bc.all <- eff.tab.all[, 3*(1:43)]

###############################################################################
### Cleansing
active.bldgs <- read.csv(paste(wd, "/Data/active_bldgs.csv", sep=""), as.is=T)[,2]
ms.data <- ms.data[ms.data$Building.Name %in% active.bldgs, ]

### Load features and building variables
kwh.tot     <- ms.data[, which(names(ms.data)=="kWh.July.08"):which(names(ms.data)=="kWh.Dec.12")]
therms.tot  <- ms.data[, which(names(ms.data)=="Therms.July.08"):which(names(ms.data)=="Therms.Dec.12")]
kwh.lab     <- ms.data[, which(names(ms.data)=="Lab.kWh.July.08"):which(names(ms.data)=="Lab.kWh.Dec.12")]
hc          <- ms.data[, which(names(ms.data)=="Total.HC.July.08"):which(names(ms.data)=="Total.HC.Dec.12")]
sc          <- ms.data[, which(names(ms.data)=="Total.SC.July.08"):which(names(ms.data)=="Total.SC.Dec.12")]
ncomp       <- ms.data[, which(names(ms.data)=="NComp.July.08"):which(names(ms.data)=="NComp.Feb.13")]
cdd.hdd     <- cdd.hdd.all[7:60, ]
cdd.hdd.reftemp     <- cdd.hdd.reftemp[7:64, ]
kwh.lab.mat <- as.matrix(kwh.lab)
kwh.lab.mat2 <- matrix(as.numeric(kwh.lab.mat), ncol=ncol(kwh.lab), byrow=F)
### site energy in MBtu
elec.cons.mbtu          <- (kwh.tot*0.00341214163)
elec.cons.lab.mbtu      <- (kwh.lab.mat2*0.00341214163)
gas.cons.mbtu           <- (therms.tot*0.1)
elec.cons.bldg.mbtu     <- elec.cons.mbtu - elec.cons.lab.mbtu
elec.cons.bldg.mbtu[elec.cons.bldg.mbtu < 0] <- min(elec.cons.bldg.mbtu[elec.cons.bldg.mbtu > 0])
site.ener.withlab.mbtu  <- elec.cons.mbtu + gas.cons.mbtu
site.ener.nolab.mbtu    <- elec.cons.bldg.mbtu + gas.cons.mbtu
### source energy in MBtu
elec.cons.source.mbtu     <- (elec.cons.mbtu*3.34)
elec.cons.lab.source.mbtu <- (elec.cons.lab.mbtu*3.34)
gas.cons.source.mbtu      <- (gas.cons.mbtu*1.047)
bldg.load.source.mbtu     <- elec.cons.source.mbtu - elec.cons.lab.source.mbtu
source.ener.nolab.mbtu    <- bldg.energy.cons.tot.source.mbtu <- 
  elec.cons.source.mbtu - elec.cons.lab.source.mbtu + gas.cons.source.mbtu  # total source energy including gas
source.ener.withlab.mbtu  <- bldg.energy.cons.tot.source.mbtu <- 
  elec.cons.source.mbtu + gas.cons.source.mbtu  # total source energy including gas
### Important energy intensity indices
site.ener.inten <- site.ener.nolab.mbtu / ms.data$Office.Space; print(dim(site.ener.inten))  # Total.Floor.Area
site.ener.inten.withlab <- site.ener.withlab.mbtu / ms.data$Total.Floor.Area; print(dim(site.ener.inten))  # Total.Floor.Area
elec.cons.lab.mbtu <- data.frame(elec.cons.lab.mbtu); print(dim(elec.cons.lab.mbtu))
names(elec.cons.lab.mbtu) <- names(site.ener.nolab.mbtu)
lab.load.inten <- elec.cons.lab.mbtu / ms.data$Total.Floor.Area; print(dim(lab.load.inten))  # Lab.Space
elec.cons.inten <- elec.cons.bldg.mbtu / ms.data$Office.Space
gas.cons.inten <- gas.cons.mbtu / ms.data$Office.Space
# Degree days
cp <- 65  # best.changepoint$Best.Changepoint[j]; print(cp)
tot.dd <- cdd.hdd.reftemp[[paste("X", cp, ".HDD", sep="")]] + 
  cdd.hdd.reftemp[[paste("X", cp, ".CDD", sep="")]]; length(tot.dd)
tot.hdd <- cdd.hdd.reftemp[[paste("X", cp, ".HDD", sep="")]]
tot.cdd <- cdd.hdd.reftemp[[paste("X", cp, ".CDD", sep="")]]
  
estar.results <- read.csv(paste(wd, "/Data/EStar_Results.csv", sep=""))
estar.ratings <- data.frame(matrix(estar.results$score2, ncol=(month.end-13+1), byrow=F))
names(estar.ratings) <- months[unique(estar.results$month)]
seef.ratings <- eff.bc.all[,2:38]*100   ###<<<TODO>>> un-comment after fixing SEEF scores
