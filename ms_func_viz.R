###############################################################################
############################ Time Series Plots ################################
###############################################################################
# Helper function to draw time axis on a monthly time-series plot
DrawTimeAxis <- function(dates=NULL, imp.dates=NULL, imp.dates.type.ind=NULL, lag=0) {
  library(zoo)
  if(is.null(dates)) dates <- as.yearmon(seq(as.Date("2008/7/1"), as.Date("2012/12/1"), by="mon") )
  dates <- as.POSIXlt(dates, origin=1900)  
  quarter.begins <- which((dates$mday == 1) & ((dates$mon) %% 3 == 0))  # or maybe (dates$mon+1) %% 3 == 0)
  quarter.begins <- quarter.begins + lag
  abline(v=quarter.begins, col="grey", lty="dashed")
  axis(1,at=quarter.begins,labels=paste(substr(months(dates),1,3), dates$year + 1900)[quarter.begins],
       mgp=c(1,0.2,0),tcl=0.5,tick=F, cex.axis=0.8, las=2)
  if(!is.null(imp.dates)) {
    imp.dates <- as.POSIXlt(imp.dates, origin=1900)  
    imp.date.loc <- which( (dates$mon %in% imp.dates$mon) & (dates$year %in% imp.dates$year) )
    if(!is.null(imp.dates.type.ind)) {
      ### TODO: update the number of colors based on the number of ECM types
      #       c("HVAC Systems", "HVAC Controls", "Lighting Systems", "Lighting Controls", 
      #         "Chiller Plant Recomm", "RetroComm", "Cafe", "Behavior Change", "Electrical Other")
      colors <- c("cadetblue1", "cadetblue2", "darkgoldenrod1", "darkgoldenrod2", 
                  "cyan", "Chartreuse4", "brown", "comsilk", "grey30")[imp.dates.type.ind]
    } else {
      colors=rep("darkred", length(imp.dates))
    }
    abline(v=imp.date.loc, col=colors, lty=4, lwd=2)
  }
}

###############################################################################
# For each building:
# 2x2 Plot:
#   (a) time-series: kwh, therm; bldg load
#   (b) time-series: kwh - lab.kwh, therm; hdd, cdd
#   (c) time:series: kwh - lab.kwh; therms; ncomp; headcount
#   (d) building info: Building.ID; Building.Type; System.Type; Total.Floor.Area; percent.non.office.space
PlotBldgTS <- function(bldg.name="Redwest D") {
  #   if(!is.null(dev.list())) dev.off()
  par(mfrow=c(2,2), mar=rep(4,4))
  k <- which(ms.data$Building.Name==bldg.name)
  # find the dates of major ECMs
  imp.dates.cur <- imp.dates.hist <- imp.dates.type.cur <- imp.dates.type.hist <- imp.dates.type.ind <- NULL
  imp.dates <- c(imp.dates.cur, imp.dates.hist)
  if (!is.null(imp.dates)) imp.dates <- as.Date(imp.dates, origin="1970-01-01")
  imp.dates.type <- c(imp.dates.type.cur, imp.dates.type.hist)
  
  imp.dates.type.ind <- which(c("HVAC Systems", "HVAC Controls", "Lighting Systems", "Lighting Controls", 
                                "Chiller Plant Recomm", "RetroComm", "Cafe", "Behavior Change", "Electrical Other") %in% imp.dates.type)
  
  # (a) time-series: kwh, therm; lab.kwh
  y.max <- max(max(elec.cons.mbtu[k,], na.rm=T), 
               max(as.numeric(gas.cons.mbtu[k,]), na.rm=T), na.rm=T)
  plot(as.numeric(elec.cons.mbtu[k,]), type="l", col="darkgreen", lwd=3, 
       ylim=c(0, y.max), xaxt="n", 
       main="Tot. Elec. Cons., Gas Cons., Lab Elec. Cons.", ylab="MBtu / SqFt", xlab="Time", cex.main=0.8)
  ### TODO: assign an index to each imp.date based on the type of ECM
  DrawTimeAxis(dates=NULL, imp.dates=imp.dates, imp.dates.type.ind=imp.dates.type.ind)
  par(new=T)
  plot(as.numeric(gas.cons.mbtu[k,]), ylim=c(0, y.max), type="l", col="darkorange", lwd=3, xaxt="n", yaxt="n", xlab="", ylab="")
  par(new=T)
  plot(as.numeric(elec.cons.lab.mbtu[k,]), ylim=c(0, y.max), type="l", col="chartreuse", lwd=3, xaxt="n", yaxt="n", xlab="", ylab="")
  #   legend("bottomleft", c("Tot. Elec. Cons.", "Gas Cons.", "Lab Elec. Cons."), col=c("darkgreen", "darkorange", "chartreuse"), 
  #          lty=c(1,1,1), lwd=c(2,2,2), bty="n", cex=0.8)  #
  
  # (b) time-series: kwh - lab.kwh, therm; hdd, cdd
  plot(as.numeric(elec.cons.bldg.mbtu[k,]), type="l", col="aquamarine3", lwd=3, 
       ylim=c(0, y.max) , xaxt="n",
       main="Building Elec. Cons., Gas Cons., HDD, CDD", cex.main=0.8, 
       ylab="MBtu / SqFt", xlab="Time")
  DrawTimeAxis()
  par(new=T)
  plot(as.numeric(gas.cons.mbtu[k,]), ylim=c(0, y.max), type="l", col="darkorange", lwd=3, xaxt="n", yaxt="n", xlab="", ylab="")
  par(new=T)
  plot(as.numeric(cdd.hdd$HDD), type="l", col="blue", lty=2, lwd=2, xaxt="n", yaxt="n", xlab="", ylab="")
  par(new=T)
  plot(as.numeric(cdd.hdd$CDD), type="l", col="red", lty=2, lwd=2, xaxt="n", yaxt="n", xlab="", ylab="")
  axis(4)
  mtext("CDD, HDD", side=4, line=3)
  # (c) time-series: kwh - lab.kwh, therm; hc; n.comp
  plot(as.numeric(elec.cons.bldg.mbtu[k,]), type="l", col="aquamarine3", lwd=3, 
       ylim=c(0, y.max) , xaxt="n",
       main="Building Elec. Cons., Gas Cons., Headcount, No.Comp.", cex.main=0.8,
       ylab="MBtu / SqFt", xlab="Time")
  DrawTimeAxis()
  y.max.err <- 0
  tryCatch(y.max2 <- max(100*ceiling(max(as.numeric(ncomp[k,]), na.rm=T)/100), 
                         100*ceiling(max(as.numeric(hc[k,]), na.rm=T)/100), na.rm=T),
           error=function(err)  y.max.err <- 1)
  par(new=T)
  plot(as.numeric(gas.cons.mbtu[k,]), ylim=c(0,y.max), type="l", col="darkorange", lwd=3, xaxt="n", yaxt="n", xlab="", ylab="")
  par(new=T)
  plot(as.numeric(hc[k,]), ylim=c(0,y.max2), type="l", col="azure4", lty=3, lwd=2, xaxt="n", yaxt="n", xlab="", ylab="")  # /max(as.numeric(hc[k,]))
  par(new=T)
  plot.err <- 0
  tryCatch(
    plot(as.numeric(ncomp[k,]), ylim=c(0,y.max2), type="l", col="brown", lty=3, lwd=2, xaxt="n", yaxt="n", xlab="", ylab=""), 
    error=function(err)  plot.err <- 1)# /max(as.numeric(ncomp[k,]))
  axis(4)
  mtext("Count", side=4, line=3)
  
  # (e) building info
  text = c(paste("Name:",      ms.data[k, "Building.Name"],
                 "\nID:",      ms.data[k,"Building.ID"],
                 "\nType:",    ms.data[k,"Building.Type"],
                 "\nSystem Type:",      ms.data[k,"System.Type"],
                 "\nOffice Space:",     ms.data[k,"Office.Space"],
                 "\nLab Space:",        ms.data[k,"Lab.Space"],
                 "\nCafe Space:",       ms.data[k,"Cafe.Space"],
                 "\nWarehouse:",        ms.data[k,"Warehouse"],
                 "\nTot. Fl. Area:", ms.data[k,"Total.Floor.Area"]
  ))
  library(grid)
  grid.text(text, x=unit(0.55, "npc"), y=unit(0.45, "npc"), just=c("left", "top"), 
            gp=gpar(fontsize=10, col="grey10"))
  grid.lines(x=c(0.76, 0.76),y=c(0.45, 0.23), 
             gp=gpar(fontsize=10, col="grey10", lwd=0.5))  # Finish
  
  grid.lines(x=c(0.78, 0.81),y=c(rep(0.45, 2)), 
             gp=gpar(fontsize=10, col="darkgreen", lwd=3, lty=1))  # Finish
  grid.lines(x=c(0.78, 0.81),y=c(rep(0.42, 2)), 
             gp=gpar(fontsize=10, col="aquamarine3", lwd=3, lty=1))  # Finish
  grid.lines(x=c(0.78, 0.81),y=c(rep(0.39, 2)), 
             gp=gpar(fontsize=10, col="chartreuse", lwd=3, lty=1))  # Finish
  grid.lines(x=c(0.78, 0.81),y=c(rep(0.36, 2)), 
             gp=gpar(fontsize=10, col="darkorange", lwd=2, lty=1))  # Finish
  grid.lines(x=c(0.78, 0.81),y=c(rep(0.33, 2)), 
             gp=gpar(fontsize=10, col="red", lwd=2, lty=2))  # Finish
  grid.lines(x=c(0.78, 0.81),y=c(rep(0.30, 2)), 
             gp=gpar(fontsize=10, col="blue", lwd=2, lty=2))  # Finish
  grid.lines(x=c(0.78, 0.81),y=c(rep(0.27, 2)), 
             gp=gpar(fontsize=10, col="azure4", lwd=2, lty=3))  # Finish
  grid.lines(x=c(0.78, 0.81),y=c(rep(0.24, 2)), 
             gp=gpar(fontsize=10, col="brown", lwd=2, lty=3))  # Finish
  
  grid.text("Tot. Elec. Cons.", x=unit(0.82, "npc"), y=unit(0.46, "npc"), 
            just=c("left", "top"), gp=gpar(fontsize=10, col="grey10"))
  grid.text("Bldg Elec. Cons.", x=unit(0.82, "npc"), y=unit(0.43, "npc"), 
            just=c("left", "top"), gp=gpar(fontsize=10, col="grey10"))
  grid.text("Lab Elec. Cons.", x=unit(0.82, "npc"), y=unit(0.40, "npc"), 
            just=c("left", "top"), gp=gpar(fontsize=10, col="grey10"))
  grid.text("Gas Cons.", x=unit(0.82, "npc"), y=unit(0.37, "npc"), 
            just=c("left", "top"), gp=gpar(fontsize=10, col="grey10"))
  grid.text("CDD", x=unit(0.82, "npc"), y=unit(0.34, "npc"), 
            just=c("left", "top"), gp=gpar(fontsize=10, col="grey10"))
  grid.text("HDD", x=unit(0.82, "npc"), y=unit(0.31, "npc"), 
            just=c("left", "top"), gp=gpar(fontsize=10, col="grey10"))
  grid.text("Headcount", x=unit(0.82, "npc"), y=unit(0.28, "npc"), 
            just=c("left", "top"), gp=gpar(fontsize=10, col="grey10"))
  grid.text("No. of Computers", x=unit(0.82, "npc"), y=unit(0.25, "npc"), 
            just=c("left", "top"), gp=gpar(fontsize=10, col="grey10"))  
  
  par(mfrow=c(1,1), mar=rep(4,4), xpd=F)
}

###############################################################################
# Function: plots a heatmap of the values of a given matrix
# Args:
#   data:     a vector of the data to be visualized
#   colorMap: the colors that are desired to the be used on the plot
# Returns:
#   creates a plot
hmapMonthly <- function(data, colorMap=NA, yvals=NA, xvals=NA, log=FALSE, bldg.names=NULL, main.title="") {
  n = dim(data)[1]
  m = dim(data)[2]
  # default values
  if(length(colorMap) < 2) { colorMap = heat.colors(100) }
  if(length(xvals)    < 2) { xvals=1:m } # xvals and yvals create the grid on which the heatmap will be drawn
  if(length(yvals)    < 2) { yvals=1:n }
  
  cols = rep(xvals, n) # duplicate column position values across all rows
  rows = rep(yvals, each=m) # duplicate y values across a whole row of data
  vals = as.vector(t(as.matrix(data))) # linearize the matrix of data
  par(mar=c(4,6,2,2))
  plot(cols,rows,col=mapColors(vals, colorMap, log=log),
       ylim=c(min(yvals), max(yvals)), xlim=c(min(xvals), max(xvals)),
       axes=F, cex=5, pch=15, xlab='',ylab='', main=main.title)  #,...
  axis(1,at=(0:(ncol(data)-1))+0.5,labels=substr(names(data), 5, 20),mgp=c(1,0.5,0),tcl=0.5,tick=F, cex.axis=0.8, las=2) # 1 = xaxis, labels in the center of each range
  axis(1,at=(0:(ncol(data)-1)),labels=F,tick=T,mgp=c(1,0,0),tcl=0.5, col="grey60")          # ticks at boundaries
  axis(2, at=0:(nrow(data)-1)-0.5, labels=bldg.names, mgp=c(0.5,0.1,0), tcl=0.5, las=2, cex.axis=0.6, col="grey60") # yaxis
  par(mar=rep(1,4))
}

###############################################################################
## Function: an aux function for heatmaps to create a legend
color.bar <- function(lut, min, max=-min, nticks=11, ticks=seq(min, max, len=nticks), title='') {
  scale = (length(lut)-1)/(max-min)
  #   dev.new(width=1.75, height=5)
  #   par(new=T)
  plot(c(0,10), c(min,max), type='n', bty='n', xaxt='n', xlab='', yaxt='n', ylab='', main=title)
  axis(2, ticks, las=1)
  for (i in 1:(length(lut)-1)) {
    y = (i-1)/scale + min
    rect(0,y,10,y+1/scale, col=lut[i], border=NA)
  }
}

###############################################################################
############# GGPLOTS ##############
###############################################################################
### 1. Average campus SEEF scores
PlotAverageCampusSeefTS <- function(eff.tab.all, eff.bc.all) {
  n.month <- ncol(eff.bc.all)
  plotdata <- data.frame(seef=apply(eff.tab.all[, 3*(1:n.month)], 2, mean), ind=1:n.month)
  while(!is.null(dev.list())) dev.off()
  breaks <- c(1+6*(0:(round(n.month/6,0)+1)))
  a <- ggplot()
  a <- a + scale_y_continuous(limits =c(0,1),name="SEEF Score")
  a <- a + scale_x_continuous(limits =c(1,n.month),name="Time",
                              breaks = breaks,
                              labels=months[breaks])
  a <- a + geom_line(aes(y = seef, x=ind), data=plotdata)
  a <- a + ggtitle(paste("Time Series of SEEF Scores for the Entire Campus")) + 
    theme(legend.title=element_blank())    
  a
  ggsave(paste("~/_My_Academics__General/R/R_Projects/MS/Results/SEEF_Results/EfficiencyScores/SEEF_TimeSeries_Campus.png",sep=""), 
         plot = a, width=9, height=5.58)
}


###############################################################################
# 2. number of buildings with SEEF scores of 0.75+
Plot75PlusCountTS <- function(eff.tab.all, eff.bc.all) {
  n.month <- ncol(eff.bc.all)
  library("scales")
  integer_breaks <- function(n = 5, ...) {
    breaker <- pretty_breaks(n, ...)  
    function(x) {
      breaks <- breaker(x)
      breaks[breaks == floor(breaks)]
    }
  }
  
  eff.bc.all <- eff.tab.all[, 3*(1:n.month)]
  seef75 <- apply(eff.bc.all, 2, function(x) sum(x > 0.75))
  plotdata <- data.frame(freq=seef75, month=months[12:54])
  while(!is.null(dev.list())) dev.off()
  breaks <- c(1+6*(0:(round(n.month/6,0)+1)))
  a <- ggplot(data=plotdata, aes(x=1:n.month, y=freq))
  a <- a + scale_y_continuous(breaks=integer_breaks(), name = "Building Count")
  a <- a + scale_x_continuous(limits=c(1,n.month), name="Time", 
                              breaks = breaks, 
                              labels=months[11+breaks])
  a <- a + geom_bar(stat="identity") # use stat="bin" for histogram
  a <- a + ggtitle("Number of Buildings with SEEF Scores Larger than 0.75")
  a <- a + theme(axis.text.x = element_text(angle=90, vjust=1))
  a
  ggsave("~/_My_Academics__General/R/R_Projects/MS/Results/SEEF_Results/EfficiencyScores/SEEF_75+buildings_No_Labs.png", 
         plot = a, width=9, height=5.58)
}


###############################################################################
### Comparing energy consumption of buildings with RCx against those without
CompareRCxkWh <- function(eff.tab.all, eff.bc.all, rcx.bldgs) {
  n.month <- ncol(eff.bc.all)
  msdata.rcx                   <- ms.data[eff.tab.all$name %in% rcx.bldgs, ]
  msdata.nonrcx                <- ms.data[!(eff.tab.all$name %in% rcx.bldgs), ]
  results.averages.rcx  <- data.frame(month= integer(), 
                                      kwh.mean = numeric(), 
                                      kwh.Rcx = numeric(), 
                                      kwh.NonRcx = numeric())
  kwh.start <- which(names(ms.data)=="kWh.July.08")
  kwh.end <- which(names(ms.data)=="kWh.Dec.12")
  for (m in kwh.start:kwh.end) {
    kwh.mean   <- mean(ms.data[,m], na.rm=T)
    kwh.Rcx    <- mean(msdata.rcx[,m], na.rm=T)
    kwh.NonRcx    <- mean(msdata.nonrcx[,m], na.rm=T)
    results.averages.rcx <- rbind(results.averages.rcx, data.frame(month=(m-kwh.start+1), 
                                                      kwh.mean = kwh.mean, 
                                                      kwh.Rcx = kwh.Rcx, 
                                                      kwh.NonRcx = kwh.NonRcx))
  }
  
  while(!is.null(dev.list())) dev.off()
  breaks <- c(1+6*(0:(round(n.month/6,0)+1)))
  a <- ggplot(data=results.averages.rcx, aes(x=month, y=kwh.mean))
  a <- a + scale_y_continuous(limits =c(0,1000000),name="Energy Consumption")
  a <- a + scale_x_continuous(limits =c(1,54),name="Time",
                              breaks = breaks,
                              labels=months[breaks])
  a <- a + scale_linetype_identity() 
  a <- a + scale_linetype_manual(values = 
                                   c("All" = "solid",
                                     "Non-RCx" = "dotted", 
                                     "RCx" = "dotdash"))
  a <- a + geom_line(aes(y = kwh.mean, linetype = "All"))
  a <- a + geom_line(aes(y = kwh.Rcx, linetype = "RCx")) 
  a <- a + geom_line(aes(y = kwh.NonRcx, linetype = "Non-RCx"))
  a <- a + ggtitle("Average Energy Consumption") + theme(legend.title=element_blank()) 
  a
  ggsave("~/_My_Academics__General/R/R_Projects/MS/Results/Savings_Results/RCx_Comparison_kwh.png", 
         plot = a, width=9, height=5.58)
}


# pie charts in ggplot: http://docs.ggplot2.org/current/coord_polar.html
# more pie charts in ggplot: http://www.r-chart.com/2010/07/pie-charts-in-ggplot2.html
###############################################################################
####################### Prepare data for analysis #############################
library(googleVis)
Motion=gvisMotionChart(googdata, idvar="Building.Name", 
                       timevar="Date", options=list(height=350, width=400))
# Display chart
plot(Motion) 
# Create Google Gadget (change the file extension to html and put it on your Stanford webpage)
cat(createGoogleGadget(Motion), file="motionchart.xml")
cat(createGoogleGadget(Motion), file="motionchart.html")


###############################################################################
###############################################################################
### Stacked barplot: Count of different project types per year
plotdata <- melt(projdata, id.vars=c("Type", "Year"))
gg.proj.ct <- ggplot(plotdata, aes(x=factor(1), fill=factor(Type)))  #x=factor(Year), 
gg.proj.ct <- gg.proj.ct + geom_bar(width=1)
gg.proj.ct <- gg.proj.ct + scale_fill_brewer(palette = "Paired")  # http://learnr.wordpress.com/2009/03/17/ggplot2-barplots/
gg.proj.ct <- gg.proj.ct + facet_grid(. ~ Year)  # http://docs.ggplot2.org/current/facet_grid.html
# gg.proj.ct <- gg.proj.ct + theme(panel.grid.major = element_line(colour = "grey90"),
#                       panel.grid.minor = element_blank(), panel.background = element_blank(),
#                       axis.ticks = element_blank())  # http://learnr.wordpress.com/2009/03/17/ggplot2-barplots/
gg.proj.ct <- gg.proj.ct + scale_y_continuous(name="Count of Projects")
gg.proj.ct <- gg.proj.ct + scale_x_discrete(labels="", name="", breaks=NULL)  # http://www.cookbook-r.com/Graphs/Axes_(ggplot2)/#setting-and-hiding-tick-markers
gg.proj.ct <- gg.proj.ct + ggtitle(paste("Count of Energy Efficiency Projects")) + 
  theme(legend.title=element_blank(),
        legend.text = element_text(size = 10), 
        plot.title = element_text(size = rel(1.1))) 
gg.proj.ct
ggsave(paste(fp.mv, "Projects_Count_ByYear.png", sep=""), 
       plot = gg.proj.ct, width = 9, height = 6)

###############################################################################
### Stacked barplot: Expected savings of different project types per year
plotdata <- melt(projdata, id.vars=c("Year", "Type"))
plotdata <- plotdata[with(plotdata, order(Year, Type)), ]
plotdata$value <- as.numeric(plotdata$value)
gg.proj.savings <- ggplot(plotdata, aes(x=factor(1), fill=factor(Type), y=value, weight=value))  #x=factor(Year), 
gg.proj.savings <- gg.proj.savings + geom_bar(width=1, stat="identity")  # , position="stack", weight=Type, aes(weight=value), 
# gg.proj.savings <- gg.proj.savings + geom_area(data=plotdata, aes(fill=factor(Type)), position="stack")  # color=variable, 
gg.proj.savings <- gg.proj.savings + scale_fill_brewer(palette = "Paired")  # http://learnr.wordpress.com/2009/03/17/ggplot2-barplots/
gg.proj.savings <- gg.proj.savings + facet_grid(. ~ Year)  # http://docs.ggplot2.org/current/facet_grid.html
# gg.proj.savings <- gg.proj.savings + facet_grid(Type ~ Year)  # facets in two dimensions
# gg.proj.savings <- gg.proj.savings + theme(panel.grid.major = element_line(colour = "grey90"),
#                       panel.grid.minor = element_blank(), panel.background = element_blank(),
#                       axis.ticks = element_blank())  # http://learnr.wordpress.com/2009/03/17/ggplot2-barplots/
gg.proj.savings <- gg.proj.savings + scale_y_continuous(name="Expected Saving (kWh/Yr)", labels=comma)  #, limits=c(0, 200)
gg.proj.savings <- gg.proj.savings + scale_x_discrete(labels="", name="", breaks=NULL)  # http://www.cookbook-r.com/Graphs/Axes_(ggplot2)/#setting-and-hiding-tick-markers
gg.proj.savings <- gg.proj.savings + ggtitle(paste("Total Saving from Energy Efficiency Projects")) + 
  theme(legend.title=element_blank(),
        legend.text = element_text(size = 10), 
        plot.title = element_text(size = rel(1.1))) 
gg.proj.savings
ggsave(paste(fp.mv, "Projects_TotalSaving_ByYear.png", sep=""), 
       plot = gg.proj.savings, width = 9, height = 6)
