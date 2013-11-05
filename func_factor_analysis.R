### INPUTS AND MODEL SELECTION
find_optimal_Nb_fact_factanal <- function(iter=3,user.data) {
    for (i in 1:iter) {
        print(paste(i,factanal(user.data,factors=i,method="mle")$PVAL))
    }
}

#################################################################################################
### ANALYSIS
### (3-1) Factor Analysis using factanal()
get_factanal <- function(user.data, data.title="", k=2, rot.method="varimax") {
    library(stats)
    out.factanal <- factanal(~ ., data=user.data, factors=k, rotation=rot.method, 
                             method="mle", scores="regression", na.action=na.omit) 
    factanal.scores <- out.factanal$scores
    plot(factanal.scores, main=paste("Factor Analysis Scores for",data.title,"Behaviors"))
    write.csv(out.factanal$loadings, file=paste("~/Documents/_My_Academics__General/R/R_Projects/06_PCA/",data.title,"_FA_loadings.csv",sep=""))   # or you can access them manually: pca.results$rotation[,1]
    write.csv(out.factanal$scores, file=paste("~/Documents/_My_Academics__General/R/R_Projects/06_PCA/",data.title,"_FA_scores.csv",sep=""))   # or you can access them manually: pca.results$rotation[,1]
    write.csv(out.factanal$uniquenesses, file=paste("~/Documents/_My_Academics__General/R/R_Projects/06_PCA/",data.title,"_FA_uniquenesses.csv",sep=""))   # or you can access them manually: pca.results$rotation[,1]
    write.csv(out.factanal$factors, file=paste("~/Documents/_My_Academics__General/R/R_Projects/06_PCA/",data.title,"_FA_factors.csv",sep=""))   # or you can access them manually: pca.results$rotation[,1]
    print(paste("P-Value:",out.factanal$PVAL))
    # source('~/Documents/_My_Academics__General/R/R_Projects/06_pca_prcomp.R')
    pdf(file=paste("~/Documents/_My_Academics__General/R/R_Projects/06_PCA/",data.title,"_FA_loadings.pdf",sep=""))
        for (i in 1:ncol(out.factanal$loadings))  plot_factor_loadings(out.factanal$loadings[,i],data.title,i)
    dev.off()
    out.factanal
}



get_alpha <- function(user.data,data.title,fa.output) {
        
    # (1) alpha of original data
    alph <- alpha(user.data)
    print(paste("Original survey data total alpha:                          ",round(alph$total[1],2)))  # original data internal reliability
    print(paste("Original survey data items alpha:                          ",round(alph$item.stats$r,2)))  # original data internal reliability
    
    alph <- alpha(fa.output$scores)
    print(paste("Initial scores reported by factanal total alpha:           ",round(alph$total[1],2)))  # factors internal reliability
    print(paste("Initial scores reported by factanal items alpha:           ",round(alph$item.stats$r,2)))  # factors internal reliability
    
    # (2) alpha of full factors
    if(data.title=="Home Improvements") loadings.m <- read.csv("~/Documents/_My_Academics__General/R/R_Projects/06_PCA/Home Improvements_FA_loadings_modified.csv",row.names=1)
    if(data.title=="Info Seeking") loadings.m <- read.csv("~/Documents/_My_Academics__General/R/R_Projects/06_PCA/Info Seeking_FA_loadings_modified.csv",row.names=1)
    if(data.title=="Energy Actions") loadings.m <- read.csv("~/Documents/_My_Academics__General/R/R_Projects/06_PCA/Energy Action_FA_loadings_modified.csv",row.names=1)
    loadings.o <- fa.output$loadings  # loadings[loadings < 0.30] <- 0
    scores.o <- as.matrix(user.data) %*% as.matrix(loadings.o)  # compute scores based on factor loadings
    
    alph <- alpha(cor(scores.o))
    print(paste("Full scores manually calculated total alpha:               ",round(alph$total[1],2)))  # compare internal reliability of scores with orignial data  
    print(paste("Full scores manually calculated items alpha:               ",round(alph$item.stats$r,2)))  # compare internal reliability of scores with orignial data  
    
    # (3) alpha of modified factors
    scores.m <- as.matrix(user.data) %*% as.matrix(loadings.m)
    scores.m <- as.matrix(user.data) %*% as.matrix(loadings.m)
    scores.m <- as.matrix(user.data) %*% as.matrix(loadings.m)
    
    alph <- alpha(cor(scores.m))
    print(paste("Modified scores from reduced factor loadings total alpha:  ",round(alph$total[1],2)))  # compare internal reliability of scores with orignial data
    print(paste("Modified scores from reduced factor loadings items alpha:  ",round(alph$item.stats$r,2)))  # compare internal reliability of scores with orignial data
    
    write.csv(scores.m,file=paste("~/Documents/_My_Academics__General/R/R_Projects/06_PCA/",data.title,"_FA_scores_calculated.csv",sep=""))
}


get_factor_scores <- function(data.title,factors.names) {    
    factor.scores <- read.csv(file=paste("~/Documents/_My_Academics__General/R/R_Projects/06_PCA/",data.title,"_FA_scores_calculated.csv",sep=""), header=T)
    factor.scores <- factor.scores[,-1]
    names(factor.scores) <- factors.names[[data.title]]
    factor.scores
}




##################################################################################################
### EVALUATION
# function to evaluate the factor analysis results by checking how close the resulted covariance matrix is to the original covariance matrix
# can be used for informal intuitive evaluation of the number of factors
orig_covmat_v_factanal_covmat <- function(user.data,nf) {
    fa <- factanal(covmat = cov(user.data), factors = nf, method="mle", n.obs=nrow(user.data))
    est <- tcrossprod(fa$loadings) + diag(fa$uniquenesses)
    ret <- round(cov(user.data) - est, 3)
    colnames(ret) <- rownames(ret) <- 
    abbreviate(rownames(ret), 3)
    ret
}

################################################################################################
### PLOTTING AND REPORTING
# 3D plot of the factor scores
plot3d_factanal <- function(out.factanal) {
    library(lattice)  # loading the lattice package
    Factor1 <- out.factanal$scores[,1];Factor2 <- out.factanal$scores[,2];Factor3 <- out.factanal$scores[,3]  
    cloud(Factor3 ~ Factor1 * Factor2, xlim=range(Factor1), ylim=range(Factor2), zlim=range(Factor3), 
          pch=19, scales = list(distance = rep(1, 3), arrows = FALSE))
}

# Plots a scree plot to help determine Number of Factors to Extract
scree_plot_fa <- function(user.data) {
    
        library(nFactors)
        user.data <- sa.home.imp
        ev <- eigen(cor(user.data)) # get eigenvalues
        ap <- parallel(subject=nrow(user.data),var=ncol(user.data),rep=100,cent=.05,)
        nS <- nScree(ev$values, ap$eigen$qevpea)
        plotnScree(nS)
}

