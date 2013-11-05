################################################################################
# scale the clus.data
    # clus.data <- scale(cbind(fs.hi, fs.ea, fs.is),center=F, scale=T) 
    # clus.data <- clus.data[expl.data.all$UID %in% ids.marine,]
create.clust.data <- function(data) {
    clus.data <- scale(data, center=F, scale=T)
    # Consider using PCs of the data to improve fit
    dist.mat <- dist(clus.data, method = "euclidean") #"euclidean", "maximum", "manhattan", "canberra", "binary" or "minkowski"
    pc <- princomp(dist.mat, cor=T)
    plot(pc)
    num.pc = as.numeric(readline("how many PCs? "))
    clus.data = pc$scores[,c(1:num.pc)]
    clus.data
}
create_clus_var <- function() {
    panel.clus = panel.enviro[,which(names(panel.enviro) %in% clus.names)]
    panel.enviro = panel.enviro[,-which(names(panel.enviro) %in% clus.names)]
    clus.data = create.clust.data(panel.clus)
    fit.kmeans = kmeans_wrap(clus.data)
    clus_var = fit.kmeans$cluster
    clus_var
}    

################################################################################
### K-MEANS CLUSTERING
    # Determine number of clusters by finding the elbow in the within-group sum of squares plot
kmeans_wrap <- function(clus.data) {
    wss <- (nrow(clus.data)-1)*sum(apply(clus.data,2,var))
    for (i in 2:15) wss[i] <- sum(kmeans(clus.data, centers=i)$withinss)
    plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares", pch=19, main="K-means WSS v. No. of Clusters")
    # K-Means Cluster Analysis
    k = as.numeric(readline("how many clusters? "))
    fit.kmeans <- kmeans(clus.data, k) # specify number of clusters
    # get cluster means 
    clus.means = aggregate(clus.data,by=list(fit.kmeans$cluster),FUN=mean)
    plot(jitter(clus.data[,c(1,2)]))
    points(clus.means[,c(2,3)], col="red", pch=19)
    # append cluster assignment
    clus.data.and.fit <- data.frame(clus.data, fit.kmeans$cluster)
    print(clus.data.and.fit[1:10,])  # just for visual check of the clustering results
    #plot.clus.results(fit.kmeans) # several plots showing the results of the clustering
    fit.kmeans
}
### pamk()
pamk.wrap <- function(clus.data) {
    library(fpc)
    k = as.numeric(readline("how many clusters? "))
    fit.pamk <- pamk(clus.data, k) 
    mean(fit.kmeans$cluster - fit.pamk$pamobject[[3]]) # compare the two methods
}
#################################################################################
### HIERARCHICAL AGGLOMERATIVE CLUSTERING
### Ward Hierarchical Clustering
hac <- function(clus.data) {
    dist.mat <- dist(clus.data, method = "euclidean") # distance matrix
    fit.hclust <- hclust(dist.mat, method="ward") 
    plot(fit.hclust) # display dendogram
    k = as.numeric(readline("how many clusters? "))
    groups <- cutree(fit.hclust, k) # cut tree into k clusters
    # draw dendogram with red borders around the k clusters 
    rect.hclust(fit.hclust, k, border="red")
    # contingency table
    xtabs(~ groups + panel$Typ.Bldg)
    xtabs(~ groups.1 + groups.1)  
}
### Using pvclust
    # The pvclust( ) function in the pvclust package provides p-values for hierarchical clustering based on multiscale bootstrap resampling. 
    # Clusters that are highly supported by the clus.data will have large p values. 
    # Interpretation details are provided Suzuki.
    # NOTE: pvclust() clusters columns not rows.

### Ward Hierarchical Clustering with Bootstrapped p values
    # NOTE: Takes a long time to run ...
ward.clus <- function(clus.data) {
    library(pvclust)
    fit <- pvclust(t(clus.data), method.hclust="ward", method.dist="euclidean")
    plot(fit) # dendogram with p values
    # add rectangles around groups highly supported by the clus.data
    pvrect(fit, alpha=.95)
}
#################################################################################
### MODEL-BASED CLUSTERING
    # Model based approaches assume a variety of clus.data models and apply maximum likelihood estimation and Bayes criteria 
    # to identify the most likely model and number of clusters. 
    # Specifically, the Mclust( ) function in the mclust package selects the optimal model 
    # according to BIC for EM initialized by hierarchical clustering for parameterized Gaussian mixture models.
    # One chooses the model and number of clusters with the largest BIC.
model.base.clustering <- function(clus.data) {
    library(mclust)
    fit.mclust <- Mclust(clus.data)
    plot(fit.mclust, clus.data, col=c("red","blue")) # plot results 
    print(fit.mclust) # display the best model
}
######################################################################################
##### Plotting Cluster Fits ######
plot.clus.results <- function(fit) {
    ### Cluster Plot against 1st 2 principal components
    library(cluster) 
    clusplot(clus.data, fit$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)

    ### Centroid Plot against 1st 2 discriminant functions
    library(fpc)
    plotcluster(clus.data, fit$cluster)

    ### Plot on the projection of the points on first two PCs
    dist.mat <- dist(clus.data, method = "manhattan",) #"euclidean", "maximum", "manhattan", "canberra", "binary" or "minkowski"
    pc <- princomp(dist.mat, cor=T)
    xlim <- range(pc$scores[,1])
    plot(pc$scores[,1:2], type="n") # cut from the call ,xlim=xlim, ylim=xlim
    labels <- cutree(fit.hclust, h=3) # cutree() works with fits of hclust()
    text(pc$scores[,1:2], labels = labels, cex=0.6)

    ### Plot a two-cluster clus.data and illustrate the correspondence of the clusters with a binary attribute
    pr <- princomp(dist.mat)$scores[,1:2]
    plot(pr, pch=(1:2[cutree(fit.hclust, k=2)]), col=c("black", "dark gray")[clus.data$X], 
         xlim=range(pr)*c(1,1.5))
    legend("topright", col=c("black", "black", "dark gray", "dark gray"), 
           legend=c("xxx", "yyy", "zzz","hhh"), pch=c(1:2, 1:2), title="xxx/yyy",bty="n")

    ### Neighborhood Graph
    library(flexclust)
    library(mvtnorm)
    fit.cclust <- cclust(clus.data, k=5, save.data=T)
    plot(fit.cclust, hull=F, project=prcomp(clus.data),col=rep("black",5),xlab="x", ylab="y", main="Neighborhood Graph")

    ### Stripes plot
    library(flexclust)
    fit.clust <- cclust(clus.data, k=5, save.data=T)
    stripes(fit.cclust, type="second", col=1)
}
######################################################################################
### Validating Cluster Solutions
# The function cluster.stats() in the fpc package provides a mechanism for comparing the similarity of two cluster solutions 
# using a variety of validation criteria (Hubert's gamma coefficient, the Dunn index and the corrected rand index)
# comparing 2 cluster solutions
validate.clust.results <- function(fit1, fit2) {
    library(fpc)
    cluster.stats(dist.mat, fit1$cluster, fit2$pamobject[[3]]) #d is a distance matrix among objects, and fit1$cluster and fit$cluster are integer vectors containing classification results from two different clusterings of the same data.
}


