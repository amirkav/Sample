########################################################################################
### (1) MODEL SELECTION
### (1-1) Forward Stepwise Selection

    get_step <- function(data, analysis.type, plot.details=F, user.class="All", attribute.subset="All Model Variables", plot.latex=T, model.type="Forward") {        
        if(analysis.type=="Time-of-Use")  analysis.type <- paste(analysis.type, tou.names[tou.ind])
        full.model <- lm(response~.,data=data)  # note that when you specify the fit formula in this way, response is simply the name of the column that you want to regress the rest of the dataset on. so you do not need to provide a separate array for response values. but you need to make sure that the values of the response column in the data dataframe are accurate
        base.model <- lm(response~1,data=data)
        if (model.type=="Forward") step.o <- step(base.model, scope=list(upper=full.model, lower=~1 ), direction = "forward", trace=F)
        if (model.type=="Backward") step.o <- step(full.model, scope=list(upper=full.model, lower=~1 ), direction = "backward", trace=F)
        write.csv(file=paste("~/Documents/_My_Academics__General/R/R_Projects/04_Model_Selection/Step_Coeffs_and_P-Values_",model.type,"_",analysis.type,"_" ,user.class,"_", attribute.subset,".csv",sep=""),summary(step.o)[[4]])
        write.csv(file=paste("~/Documents/_My_Academics__General/R/R_Projects/04_Model_Selection/Step_ANOVA_Table_",model.type,"_", analysis.type,"_",user.class,"_", attribute.subset,".csv",sep=""),cbind(Step=paste("'",step.o$anova[,1]),step.o$anova[,-1]))
        sink(file=paste("~/Documents/_My_Academics__General/R/R_Projects/04_Model_Selection/Step_Fit_Evaluation_",model.type,"_",analysis.type,"_",user.class,"_", attribute.subset,".txt",sep=""))
            print(analysis.type); print(classification.type); print(user.class); print(floor.area.adj); print(app.anal.type); print(attribute.subset); 
            print(paste("Number of variables:", extractAIC(step.o)[1])); print(paste("Final AIC:",round(extractAIC(step.o)[2],2)))
            print(paste("Adjusted R-Squared:",round(summary(step.o)$adj.r.squared,2))) ; print(paste("Mean Squared Error:",round(mean(summary(step.o)$residuals^2),2))) 
        sink()
        plot_step(step.o,analysis.type,user.class, attribute.subset, latex.output=plot.latex, model.type=model.type)
        if(plot.details) plot(step.o)  # interactive multiple plots
        step.o
    }
    
    plot_step <- function(step.o, analysis.type, user.class="All", attribute.subset, latex.output=T, model.type="Forward") {
        # (1) plot regression coefficients
        latex.folder <- "C:/Users/kavousian/Documents/_My_Research/_Me/2011-01to09_ARPA-E/ARPA-E_Paper/"
        if (latex.output) pdf(paste(latex.folder,"plot_step_coefficients_",model.type,"_",analysis.type,"_",user.class,"_",attribute.subset,".pdf",sep=""), height=4, width=6)
        plot(summary(step.o)[[4]][,1],type="h",col="blue",cex.main=0.6,
             main=paste("Regression Coefficients for ",model.type," Stepwise Regression\non",analysis.type,"kWh for",user.class,"Users", "|", attribute.subset, "| R2=",round(summary(step.o)$adj.r.squared,2)),
             ylab="Regression Coefficient", xlab="", xaxt="n")
        labels <- substr(row.names(summary(step.o)[[4]]),start=1,stop=20)
        text(c(1:length(labels)), par("usr")[3]-0.01, srt=45, adj=1, labels=labels, xpd=TRUE, cex=0.55)
        mtext(1, text = "Regression Variables", line = 4)
        abline(v=c(1:length(summary(step.o)[[4]][,1])), col="gray80", lty=3)
        lines(summary(step.o)[[4]][,1],type="h",col="blue",lwd=3)
        if (latex.output) dev.off()        
        
        # (2) plot residuals
        if (latex.output) pdf(paste(latex.folder,"plot_step_residuals_",analysis.type,"_",user.class,"_",attribute.subset,".pdf",sep=""), height=4, width=6)
        plot(step.o$residuals, type="l", col="blue", ylab="Residuals", xlab="Households",cex.main=0.6,
             main=paste("Residuals for ",model.type," Stepwise Regression\non",analysis.type,"kWh for",user.class,"Users", "|", attribute.subset, "| R2=",round(summary(step.o)$adj.r.squared,2))
             )
        abline(h=0)
        if (latex.output) dev.off()        
             
        # (3) plot variable ranks and deviance
        if (latex.output) pdf(paste(latex.folder,"plot_step_deviances_",analysis.type,"_",user.class,"_",attribute.subset,".pdf",sep=""), height=4, width=6)
        par(mar=c(9,5,3,3))
        #y.top=ceiling(max(step.o$anova[,5])*1000)/1000; y.bot=0 #10*floor(min(step.o$anova[,5])/10)
        relative.deviance <- 100 * step.o$anova[,5] / step.o$anova[1,5]  # step.o$anova[1,5] is the deviance of the data itself, which is the same as the deviance of a model if we only fit the mean value as the fitted model
        plot(relative.deviance, type="b", pch=19, ylab="% Residual Deviance",cex.main=0.7,
             main=paste("% Residual Deviance for ",model.type," Stepwise Regression\non",analysis.type,"kWh for",user.class,"Users", "|", attribute.subset, "| R2=",round(summary(step.o)$adj.r.squared,2)),
            xlab="",xaxt="n", cex=0.8)
        text(c(1:length(relative.deviance)),0.90*min(relative.deviance),xpd=TRUE, srt=55, adj=1, labels=c("Intercept",step.o$anova[,1][-1]), cex=0.8)  # param indices at the foot of the bars
        #abline(h=seq(from=0, to=y.top, by=10), lty="dashed", col="gray")
        abline(v = c(1:length(relative.deviance)), lty=3, col="gray80")
        lines(relative.deviance, type="b", pch=19, col="blue")
        if (latex.output) dev.off()        
    }

    two_level_stepwise <- function(data.non.area.intensive, data.area.intensive, 
                                   analysis.type=analysis.type,plot.details=F,
                                   user.class=user.class, sa, fit.enum) {
        attribute.subset <- "Non Floor Area Intensive Variables"
        fit <<- step.fit.n.area <- get_step(data=data.non.area.intensive, analysis.type=analysis.type,
                            plot.details=F,user.class=user.class, attribute.subset)
        colnames(var.ranks)[fit.enum] <- analysis.type
        var.ranks <- rank_vars(var.ranks=var.ranks, fit=step.fit.n.area, fit.enum, analysis.type)
        
        resid.n.area <- step.fit.n.area$residuals / (sa$Floor.Area/1000)
        data.area.intensive$response <- resid.n.area
        attribute.subset <- "Floor Area Intensive Variables"
        fit <<- step.fit.area <- get_step(data=data.area.intensive, analysis.type=analysis.type,
                            plot.details=F,user.class=user.class, attribute.subset)
        var.ranks <- rank_vars(var.ranks=var.ranks, fit=step.fit.area, fit.enum, analysis.type)
        var.ranks        
        ### return the fit evaluation and rankings of variables
        
    }
        
    rank_vars <- function(var.ranks, fit, fit.enum, analysis.type) {
        print(paste("fit.enum start of rank_vars:", fit.enum))
        var.names <- substr(fit$anova[,1],3,100)
        nb = c(1:length(var.names))
        ranks <- list(vars=var.names, rank=I(nb))
        for (i in 1:nrow(var.ranks)) {
            if (rownames(var.ranks)[i] %in% ranks$vars){
                var.ranks[i,fit.enum] <- as.numeric(ranks$rank[which(ranks$vars == rownames(var.ranks)[i])])  # just an example
            }
        }
        colnames(var.ranks)[fit.enum] <- analysis.type
        var.ranks
    }
      
    non_outlier_analysis <- function(res, sa, data, analysis.type, user.class, plot.details=F, take.log=T) {
        # fit a model WITHOUT outliers
            print("Outlier IDs:"); print(outlier.ids <- sa$UID[res > summary(res)[5] + 1.5*IQR(res)] )
            write.csv(file=paste("~/Documents/_My_Academics__General/R/R_Projects/04_Model_Selection/Outlier_User_Ids_",analysis.type, user.class,".csv",sep=""),outlier.ids, row.names=F, col.names=F)
            remove.outliers <- T; only.outliers <- !remove.outliers
            source('~/Documents/_My_Academics__General/R/R_Projects/04_01_create_tables_for_regression.R')
            step.fit.no.outliers <- get_step(data, analysis.type=analysis.type,plot.details=plot.details,user.class=user.class)
            step.fit.no.outliers
    }
    outlier_analysis <- function(res, sa, data, analysis.type, user.class, plot.details=F) {
            # fit a model ONLY to outliers        
            print(outlier.ids <- sa$UID[res > summary(res)[5] + 1.5*IQR(res)] )
            write.csv(file=paste("~/Documents/_My_Academics__General/R/R_Projects/04_Model_Selection/Outlier_User_Ids_",analysis.type, user.class,".csv",sep=""),outlier.ids, row.names=F, col.names=F)
            only.outliers <- T; remove.outliers <- !only.outliers
            source('~/Documents/_My_Academics__General/R/R_Projects/04_01_create_tables_for_regression.R')
            step.fit.to.outliers <- get_step(data, analysis.type=analysis.type,plot.details=F,user.class=user.class)
            step.fit.to.outliers
    }

        
### (1-2) Backward Stepwise
    # basically the same code above, just changing the parameter to go backwards
        
        
### (1-3) Subset Selection
    regsubsets_wrap <- function(response, data, meth="forward") {
        library(leaps)
        regsubsets.mot <- regsubsets(x=response~.,data=data, method=meth)
        summary(regsubsets.mot)
        plot(regsubsets.mot)
        regsubsets.mot
    }    
### (1-4) Subset selection using leap() function
    
### (1-5) Generalized Additive Model (GAM)
    gam_wrap <- function(expl.data, response) {
        library(gam)
        #x1 = expl.data[,1]; x2 <- expl.data[,2];x3=expl.data[,3];x4=expl.data[,4];x5=expl.data[,5];x6=expl.data[,6];x7=expl.data[,7];x8=expl.data[,8];x9=expl.data[,9];x10=expl.data[,10];x11=expl.data[,11];x12=expl.data[,12];x13=expl.data[,13];x14=expl.data[,14];
        gam.fit <- gam(response~., data=expl.data)
        plot(gam.fit,se=T)
        #plot(x1,fitted(gam.fit))
        predict(gam.fit)
    }    
 
### (1-6) Best subset GLM
    glm_wrap <- function(response, data, fam="gaussian") {
        if(fam=="gaussian-identity")    glm_fit <- glm(response~., data=data, family=gaussian(link = "identity"))    
        if(fam=="gaussian-log")         glm_fit <- glm(response~., data=data, family=gaussian(link = "log"))    
        if(fam=="gamma")                glm_fit <- glm(response~., data=data, family=Gamma(link = "inverse"))    
        if(fam=="poisson")              glm_fit <- glm(response~., data=data, family=poisson(link = "log"))    
        if(fam=="binomial")             glm_fit <- glm(response~., data=data, family=binomial(link = "logit"))  # only accepts 0/1 response vectors
        if(fam=="inverse.gaussian")     glm_fit <- glm(response~., data=data, family=inverse.gaussian(link = "1/mu^2"))    
        if(fam=="quasi")                glm_fit <- glm(response~., data=data, family=quasi(link = "identity", variance = "constant"))    
        if(fam=="quasibinomial")        glm_fit <- glm(response~., data=data, family=quasibinomial(link = "logit"))    
        if(fam=="quasipoisson")         glm_fit <- glm(response~., data=data, family=quasipoisson(link = "log"))    
        
        glm.sum <- summary(glm_fit)        
        #print(glm.sum$coefficients) 
        print(glm.sum)
        print(paste("R-Squared:", round((1 - glm.sum$deviance/glm.sum$null.deviance),2) ))  # R-squared
        print(paste("Mean Squared Error:",round(mean(glm_fit$residuals^2),2)))
        glm_fit
    }
    
    best_glm <- function(data.glm) {
        library(bestglm)
        names(data.glm) <- c("y", paste("X",1:(ncol(data.glm)-1),sep=""))
        bglm.fit <- bestglm(data.glm)
    }

#######################################################################################################
### (2) MODEL SHRINKAGE
### (2-1) LASSO
    lasso_wrap <- function(model.matx, response ) {
        library(lars)
        lasso.fit <- lars(model.matx,response, type="lasso")
            plot(lasso.fit)   # plots variable coefficients against different values of lambda (penalty factor)
            summary.lars(lasso.fit)
        cv.lasso.fit <- cv.lars(model.matx,response,mode="step")   # finding the optimal lambda by cross-validation
        min.plus.se.cv.err <- min(cv.lasso.fit$cv) + cv.lasso.fit$cv.error[which(cv.lasso.fit$cv==min(cv.lasso.fit$cv))]   # the min+1se standard deviation. we will choose the largest lambda for which our cv error is still within 1 sd of the minimum error
        cv.lasso.fit$cv < min.plus.se.cv.err
        pred.lasso <- predict.lars(lasso.fit, s=min.plus.se.cv.err, type = "coefficients", mode ="lambda")  # the optimum factor loadings based on lasso 
        # examine, plot and export the results
            print(paste(sum(pred.lasso$coefficients==0), "questions receive a coeff of zero"))
            par(mar=c(10,2,2,2),oma=c(2,2,2,2))
            barplot.m <- barplot(pred.lasso$coefficients, ylab="",sub="", main="",axisnames=F)
            abline(v=barplot.m, lty="dashed", col="gray")
            text(x=barplot.m,y=-0.3, substr(names.coef,0,40), srt=90, adj=1, cex=0.6, xpd=TRUE)
            par(new=T); barplot.m <- barplot(pred.lasso$coefficients, ylab="",sub="", main="",axisnames=F)
            write.csv(pred.lasso$coefficients, file="Lasso_Coefficients.csv")
        lasso.fit
    }
    
### (2-2) LAR (Least Angle Regression)
    lars_wrap <- function(model.matx, response) {
        library(lars)
        lars.fit <- lars(model.matx,response,type="lar",trace=TRUE)
            plot(lars.fit)
        cv.lars.fit <- cv.lars(model.matx, response, type="lar",mode="step")
        min.plus.se.cv.err <- min(cv.lars.fit$cv) + cv.lars.fit$cv.error[which(cv.lars.fit$cv==min(cv.lars.fit$cv))]
        cv.lars.fit$cv < min.plus.se.cv.err
        pred.lars <- predict.lars(lars.fit, s=min.plus.se.cv.err, type = "coefficients", mode ="lambda") 
        method.title <- "LAR"
        write.csv(pred.lars$coefficients, row.names=T,
                          file=paste("~/Documents/_My_Academics__General/R/R_Projects/06_Model_Selection/",method.title,"_",data.title,"_coefficients.csv",sep=""))        
        lars.fit
    }
    
### (2-3) glmnet (is the generalization of the penalty-based shrinkage models. see ESLA)
# (if alpha=1: is equivalent to Lasso, but this code implements a faster algorithm)
    glmnet_wrap <- function(x,y, fam="gaussian") {
        library(glmnet)
        glmnet.fit <- glmnet(x=x,y=y,alpha=1, family=fam)
        plot(glmnet.fit)
        cv.glmnet.fit <- cv.glmnet(x=x,y=y, alpha=1)  # find the value of lambda by cross validation
        plot(cv.glmnet.fit) # the vertical line is drawn at minimum lambda not the minimum lambda within 1se  
                            # $glmnet.fit will have the model fit on all the data
        lambda.1se <- cv.glmnet.fit$lambda.1se    # this is the lambda selected using cross-validation (see figure 3.7 in Hastie and Tibshirani ESLA)
                                    # we calculate the parameter values at this lambda
        predict(glmnet.fit,s=lambda.1se ,type="coefficients")  # model for 1se within lowest cv error
        coef(cv.glmnet.fit)  # full model 
        glmnet.fit
    }
    
### (2-4) Ridge Regression
    ridge.reg <- function(response, data, lam) {
        library(MASS)
        lam = seq(0,10000,len=5000)
        ridge.fits <- lm.ridge(response~., data=data,lam=lam)
        plot(ridge.fits)
    }
##############################################################################################
