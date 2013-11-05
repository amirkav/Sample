# SEEF (Stochastic Energy Efficiency Frontier) is a benchmarking method based on Pareto-efficient frontier methods
# see http://ascelibrary.org/doi/abs/10.1061/(ASCE)CP.1943-5487.0000327
#     Energy Star calculation:
#     34.17*(log(grossarea)-9.535) +
#       17.28*(computers/(grossarea/1000)-2.231) +
#       55.96*(log(operatinghours)-3.972) + 
#       10.34*(log(workers/(grossarea/1000))-0.5616) +
#       0.0077*(HDD-4411) +
#       0.0144*(CDD-1157)

###############################################################################
### Wrapper function to go through all bldgs and all months and produce eff.tab.all
### Calculate SEEF scores
CalculateSeefScores <- function(ms.data, ks=NULL, buildings="all", rts="drs") {
  if(is.null(ks)) ks <- 1:nrow(ms.data)
  ms.data2 <- ms.data[ks, ]
  
  eff.tab.all <- data.frame(name=ms.data2$Building.Name, sys.type=ms.data2$System.Type)
  for (m in 12:ncol(site.ener.inten)) {
    print(m)
    site.ener.inten.bldg  <- site.ener.inten[ks, (m-11):m]
    hc.bldg               <- hc[ks, (m-11):m]
    lab.perc.bldg         <- lab.load.inten[ks, (m-11):m]
    dd                    <- as.numeric(tot.dd[(m-11):m])
    dd.m                  <- matrix(rep(dd, length(ks)), nrow=length(ks), ncol=12, byrow=T)
    # Energy Star variables
    cdd                   <- as.numeric(tot.cdd[(m-11):m])
    cdd.m                 <- matrix(rep(cdd, length(ks)), nrow=length(ks), ncol=12, byrow=T)
    hdd                   <- as.numeric(tot.hdd[(m-11):m])
    hdd.m                 <- matrix(rep(hdd, length(ks)), nrow=length(ks), ncol=12, byrow=T)
    log.area              <- log(ms.data2$Total.Floor.Area)
    log.area.m            <- matrix(rep(log.area, 12), nrow=length(ks), ncol=12, byrow=F)
    ncomp.persqft         <- ncomp[ks, (m-11):m] / ms.data2$Total.Floor.Area
    operatinghours        <- log(55)
    operatinghours.m      <- matrix(rep(operatinghours, 12*length(ks)), nrow=length(ks), ncol=12, byrow=F)
    hc.persqft            <- hc.bldg / ms.data2$Total.Floor.Area    
    
    seef.type <- "EstarComparison"  # just for naming output files and plots
    inputs=site.ener.inten.bldg# + 1
    nas <- as.numeric(which(apply(inputs, 1, function(x) sum(is.na(x))) > 0))
    inputs=list(inputs[-nas,])
    ks2 <- ks[-nas]
    outputs=list(log.area.m[-nas,], 
                 ncomp.persqft[-nas,], 
                 #operatinghours.m[-nas,], 
                 hc.persqft[-nas,],
                 #,hdd.m[-nas,], cdd.m[-nas,], 
                 dd.m[-nas,])
    eff.tab <- seef(inputs, outputs, 
                    rts=rts, n.obs=12, year=names(site.ener.inten.bldg)[m], 
                    seef.type=seef.type, ks=ks2, buildings=buildings, month=m)
    eff.tab2 <- data.frame(name=ms.data$Building.Name,
                           sys.type=ms.data$System.Type,
                           eff.bc=rep(NA, 52),
                           eff.conf.up=rep(NA, 52), 
                           eff.conf.low=rep(NA, 52))
    eff.tab2[match(eff.tab$name, ms.data$Building.Name), 3:5] <- eff.tab[, 3:5]

    #     seef.type <- "OneInThreeOut"    
    #     eff.tab <- seef(inputs=list(site.ener.inten.bldg), 
    #                     outputs=list(hc.bldg, lab.perc.bldg, dd.m),  # 
    #                     rts="drs", n.obs=12, year=names(site.ener.inten.bldg)[m], 
    #                     seef.type=seef.type)

    eff.tab.all <- cbind(eff.tab.all, eff.tab2[, 3:5])
    rownames(eff.tab.all) <- ks
    names(eff.tab.all)[(ncol(eff.tab.all)-2):ncol(eff.tab.all)] <- 
      paste(c("eff.bc", "eff.conf.up", "eff.conf.low"), ".", m, sep="")
    
  }
  eff.bc.all <- eff.tab.all[, 3*(1:43)]
  # DO NOT change the address or file name of the following line
  write.csv(eff.tab.all, paste(ms.folder, "/Results/SEEF_Results/SEEF_EStarComparison_scores_all.csv", sep=""), row.names=F)
  eff.tab.all
}

# Args:
# inputs:   a list of data frames. Each row in the data frames is one user and each column one observation.
# outputs:  a list of data frames. Each row in the data frames is one user and each column one observation.
# n.obs:    number of observations for bootstrapping
# seef <- function(input1, input2=NULL, output1, output2, output3, rts="drs", n.obs=12, other.plots=F, year="2009") {
#   dea.res <- data.frame(matrix(rep(0, n.obs*nrow(output1)), nrow=nrow(output1)))
seef <- function(inputs, outputs, rts="drs", n.obs=12, other.plots=F, year=NULL, 
                 ks=NULL, main.t="", seef.type="OneInThreeOut", buildings="all", month) {
  if(is.null(ks)) ks <- 1:nrow(inputs[[1]])
  #   dea.res <- data.frame(matrix(rep(0, n.obs*nrow(output1)), nrow=nrow(output1)))
  library(Benchmarking)
  dea.res <- data.frame(matrix(rep(0, n.obs*nrow(outputs[[1]])), nrow=nrow(outputs[[1]])))
  dea.peers <- list()
  dea.lambda <- list()
  for (mo in 1:n.obs) {
    inputs.mo <- inputs[[1]][,mo]
    if(length(inputs) > 1) {
      for (k in 2:length(inputs)) {
        inputs.mo <- cbind(inputs.mo, inputs[[k]][,mo])
      }
    }
    outputs.mo <- outputs[[1]][,mo]
    if(length(outputs) > 1) {
      for (k in 2:length(outputs)) {
        outputs.mo <- cbind(outputs.mo, outputs[[k]][,mo])
      }
    }
    dea.results <- dea(X=inputs.mo, Y=outputs.mo, RTS=rts, ORIENTATION="in")
    dea.res[,mo] <- dea.results$eff
    dea.peers[[mo]] <- peers(dea.results)  # see Bogetoft & Otto p.98 for explanations of peers and lambda
    dea.lambda[[mo]] <- lambda(dea.results)
  }
  
  ### Form the table that stores efficiency values
  eff.bc        <- apply(dea.res,1,mean)
  eff.conf.up   <- apply(dea.res,1,mean) + apply(dea.res,1,sd)
  eff.conf.low  <- apply(dea.res,1,mean) - apply(dea.res,1,sd)
  eff.tab <- cbind.data.frame(eff.bc, eff.conf.up, eff.conf.low, 
                              name=ms.data$Building.Name[ks], 
                              sys.type=ms.data$System.Type[ks])
  
  # Plot CI
  eff.tab.sort <- eff.tab[order(eff.bc, decreasing=T), ]
  n.inputs <- length(inputs)
  n.outputs <- length(outputs)
  Plot.SEEF.CI(eff.tab.sort=eff.tab.sort, buildings=buildings, 
               n.inputs=n.inputs, n.outputs=n.outputs, month=month)
  eff.tab[,c(4,5,1,2,3)]
}

###############################################################################
### Plot conf int plot of SEEF scores, in decreasing order
Plot.SEEF.CI <- function(eff.tab.sort, buildings="", n.inputs, n.outputs, month) {
  library(plotrix)
  dim(eff.tab.sort)
  plotCI(x=1:nrow(eff.tab.sort), y=eff.tab.sort$eff.bc, ui=eff.tab.sort$eff.conf.up, li=eff.tab.sort$eff.conf.low, err="y", 
         main=paste("Bias-Corrected Efficiency Scores and Confidence Bands for", buildings, 
                    "\n", n.inputs, 
                    "Input(s) and", n.outputs, "Output(s) for 12 months ending in", months[month]),
         ylab="Efficiency Scores and Confidence Bands", xlab="",
         gap=0.005, sfrac=0.005, scol="grey60", col=rainbow(length(unique(eff.tab.sort$sys.type)))[as.factor(eff.tab.sort$sys.type)],  
         pch=20, pt.bg=par("bg"), cex.lab=1.0, cex.main=0.8, xaxt="n") #, slty="dashed", font.lab=2
  axis(1,at=1:nrow(eff.tab.sort),labels=eff.tab.sort$name,
       mgp=c(1,0.2,0),tcl=0.5,tick=F, cex.axis=0.8, las=2)
  legend("bottomleft", legend=unique(eff.tab.sort$sys.type), 
         col=rainbow(length(unique(eff.tab.sort$sys.type)))[unique(eff.tab.sort$sys.type)],
         pch=19, cex=0.6)

}

