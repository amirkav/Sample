# User interface file. 
source(paste(wd, '/ms_viz.R', sep=""))
source('~/_My_Academics__General/R/R_Projects/Wharton/R_Codes/wharton_func.R')  # need it for colorMapfunction
###############################################################################
### Heatmap plot
colors <- c("light green", "yellow", "orange", "red")
color.map <- colorRampPalette(colors)(100)
nf <- layout(matrix(c(1,2), 1, 2, byrow=T), widths=c(8,1), heights=c(5.58,5.58), respect=TRUE)
layout.show(nf)
hmap.var <- site.ener.inten/max(site.ener.inten, na.rm=T)
hmapMonthly(hmap.var, colorMap=color.map, bldg.names=ms.data$Building.Name,
     main.title="Bldg Site Energy Consumption Intensity (MBtu/Sqft Normalized)")
color.bar(colorRampPalette(colors)(100), min=0, max=1)

### Dashboard plot
bldgname <- "50"  # choose building names from ms.data$Building.Name
PlotBldgTS(bldg.name=bldgname)

###############################################################################
########################### Regression Analysis ###############################
###############################################################################
### Validation model (rolling-year baseline starting with 2009, includes data center load as a regressor)
# Args:
#   bldgs.ind:            the index of buildings of interest. 
#   site.ener.nolab.mbtu: site energy consumption minus data center load (is already calculated)
#   elec.cons.lab.mbtu:   electricity consumption by data center
#   ms.data:              data set including building information and raw consumption data
#   best.changepoint:     a data frame including the best temperature changepoint for heating/cooling load analysis
#   cdd.hdd.reftemp:      a data frame containing cooling degree days and heating degress days for different reference temperatures
#   hc:                   a data frame containing headcount (occupancy) information by month
#   lognormal:            a logical value indicating whether the regression model should use log of consumption
#   wd:                   working directory for reading and writing files
# Outputs:
#   creates a baseline model for energy consumption, predicts the next 12 months consumption based on the baseline,
#   compares actual consumption with the baseline, and calculates savings.
#   saves the savings to disk and plots them along with Energy Star scores and SEEF scores (a stochastic benchmarking method)
ks <- 1:nrow(ms.data)
# WARNING: this will take a few minutes to run
pred.meas <- VerificationModel(bldgs.ind=ks, site.ener.nolab.mbtu, elec.cons.lab.mbtu, 
                       ms.data, best.changepoint, cdd.hdd.reftemp, hc, lognormal=T, wd=wd)

### Prediction model (one month advance prediction)
### Log-normal, auto-regressive, cross-validated monthly model
# wrapper function. goes thorugh all buildings, fits the model, 
# predicts future values and calculates savings.
# Outputs:
  # Fitted and predicted values are shown in plots under Folder Results\ 
  # Diagnostic plots of residuals and prediction errors are under folder Results\
  # Diagnostic plots of Absolute Prediction Error and Mean Abosulute Prediction Error and R2 are under folder Results\Model_Results\Average_MAPE_APE_R2
  # Saving values are saved in Results\Savings.csv
month.start = 13
month.end = 49  
ks <- 1:2  # nrow(ms.data)
# WARNING: this will take a few minutes to run
PredictionModel(month.start=month.start, month.end=month.end, wd=wd, ks.all=ks)


