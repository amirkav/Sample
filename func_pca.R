### function pca_prcomp
# draws a heatmap of the correlation matrix of the variables and draws dendrograms to show potentials for clustering
# calculates PCs of the data
# plots log(eigenvalue) which is log-scree plot of the PCs
# plots cumulative variance explained by first m PCs, identifies how many PCs are needed to explain 80% of the variance in variable values
# saves csv files of loadings, scores, and variances of PCs to hard drive
# calls function plot_factor_loadings
### plot_factor_loadings
# saves a pdf file with plots of loadings of all PCs to hard drive
##### <---- Start Function ----> #####
pca_prcomp <- function(data, data.title, variance.th=0.8){
    library(gplots)
    heatmap.2(cor(data),trace="n",margins=c(10,10))
    pca.results <- prcomp(data, retx=TRUE, center=TRUE, scale=TRUE)
        # log scree diagram
        # individual variances of the PCs and their correlation with the original coordinates
            par(mar=c(2,2,3,2), oma=c(2,2,2,2))
            plot(log(pca.results$sdev^2), xlab="Principle Component",ylab="log(variance)", 
                 main=paste("Log-Eigenvalue Diagram of PCA Results \nfor",data.title,"Behaviors"),type="b", pch=16)    
            write.csv(pca.results$sdev, file=paste("~/Documents/_My_Academics__General/R/R_Projects/06_PCA/",data.title,"_PCA_sdev_prcomp.csv",sep=""))
            correlations <- cor(pca.results$x, data)
        # PC importances (cumulative variance of the data explained by PCs)
            (s <- summary(pca.results))
            plot(s$importance[3,], type="b", pch=16, xlab="Principal Component",  
                 ylab="Cumulative Total Variance Explained", main=paste("Principal Component Importances \nfor",data.title,"Behaviors"))
            abline(h=0.8, col="red", lty="dashed"); 
            points(x=min(which(s$importance[3,]>variance.th)) ,y=s$importance[3,min(which(s$importance[3,]>variance.th))], col="red", pch=16)
            text(x=min(which(s$importance[3,]>variance.th)) ,y=s$importance[3,min(which(s$importance[3,]>variance.th))]+0.05,labels=toString(min(which(s$importance[3,]>variance.th))),col="red")
        # Factor loadings (rotations): multipliers that transform original coordinates to PCs
            write.csv(pca.results$rotation, file=paste("~/Documents/_My_Academics__General/R/R_Projects/06_PCA/",data.title,"_PCA_loadings_prcomp.csv",sep=""))   # or you can access them manually: pca.results$rotation[,1]
            
        # Scores: values of the data when transformed to PCs
            write.csv(pca.results$x, row.names=F,
                      file=paste("~/Documents/_My_Academics__General/R/R_Projects/06_PCA/",data.title,"_PCA_scores_prcomp.csv",sep=""))        
    pdf(file=paste("~/Documents/_My_Academics__General/R/R_Projects/06_PCA/",data.title,"_PCA_loadings_prcomp.pdf",sep=""))
        for (i in 1:ncol(pca.results$rotation))  plot_factor_loadings(pca.results$rotation[,i],data.title,i)
    dev.off()
    #if (bi.plot) biplot(pca.results,cex=c(0.2,0.2))
    pca.results
}

plot_factor_loadings <- function(pc.loadings,data.title,pcNb) {
    col <- c(rep())
    y.up <- 1 #round(max(pc.loadings),1)+0.1
    y.dn <- -1 #round(min(pc.loadings),1)-0.1
    main.title <- paste("PC #",pcNb," Loadings for ",data.title,sep="")
    par(mar=c(12,2,1,2),oma=c(3,2,0,2))
    bp <- barplot(pc.loadings,ylim=c(y.dn,y.up), col=col, xlab="", ylab="Factor Loading", main=main.title, adj=0,sub="",axisnames=F)
    #bp <- barplot(pc.loadings,horiz=T, col=col, xlab="", ylab="Factor Loading", main="", adj=0,sub="",axisnames=F)
    abline(h=seq(from=y.dn+0.1, to=y.up-0.1, by=0.1), lty="dashed", col="gray")
    text(substr(names(pc.loadings),0,40),x=bp,y=y.dn-0.01,xpd=T,cex=0.8,srt=65,adj=1)
    barplot(pc.loadings,ylim=c(y.dn,y.up), col=col, xlab="", ylab="Factor Loading", main="", adj=0,sub="",axisnames=F,add=T)
}    

