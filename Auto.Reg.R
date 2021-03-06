Auto.Reg <-
function(Dataset,D_Var,Ind_Var,Dev_Val_Split_Per=70,Imputation=TRUE,Imputation_Per=5,Outlier_Treatment=TRUE){
   
    ####Summary Statistics Computation
    summary_stats <- function(x)
    {return(c(
        nrow(Dataset),nrow(Dataset)-NROW(na.omit(x)),min(x,na.rm=T),quantile(x,c(.01,.05,.10,.25,.50,.75,.9,.95,.99),na.rm=T),
        max(x,na.rm=T),mean(x,na.rm=T),median(x,na.rm=T),sd(x,na.rm=T),100*sd(x,na.rm=T)/mean(x,na.rm=T),max(x,na.rm=T)- min(x,na.rm=T),
        IQR(x,na.rm=T),skewness(x,na.rm=T),kurtosis(x,na.rm=T)
    ))}
    Summary_Data <- as.data.frame(apply(Dataset[,c(D_Var,Ind_Var)], 2, summary_stats))
    Summary_Data$stat <-c("OBS","NMIS","0% Min","1%","5%","10%","25% Q1","50% Median","75% Q3","90%","95%","99%","100% Max","Mean","Median","SD","CV (Mean/SD)","Range (Max-Min)","IQR (Q3-Q1)","Skewness(-1,+1)","Kurtosis(0,1)")
    Summary_Data<-data.frame(Summary_Data, row.names=NULL)
    Summary_Data <- cbind(Summary_Data$stat,Summary_Data[,(names(Summary_Data) != "stat")] )
    names(Summary_Data)[names(Summary_Data) == "Summary_Data$stat"] <- "Stats"
    Summary_Data[,-1] <-round(Summary_Data[,-1],2) 
    Univar_Stats_Pre_Outlier <- Summary_Data
    
    #### Missing Value Imputation
    #complete.cases(Dataset[,c(D_Var,Ind_Var)])
    imp_Flag <- sum(as.numeric(Univar_Stats_Pre_Outlier[2,-1]) > 0) > 0
    if (imp_Flag ==TRUE){
        if (Imputation == TRUE){
            Out_Names <- colnames(Summary_Data)[which(Summary_Data[2,2:ncol(Summary_Data)] > Imputation_Per)+1]
            Dataset <- Dataset[,!names(Dataset) %in% Out_Names]
            Dataset <- knnImputation(Dataset)
        }
    }
    
    #### Outlier Treatment
    a <- colnames(Dataset)[colnames(Dataset) %in% c(D_Var,Ind_Var)]
    if (Outlier_Treatment == TRUE){
        for (i in c(1:length(a))){
            if (length(unique(Dataset[,a[i]])) > 20) {
                Dif_95_99  <- round(((Summary_Data[12,a[i]]-Summary_Data[11,a[i]])/Summary_Data[11,a[i]]),3)
                Dif_99_100 <- round(((Summary_Data[13,a[i]]-Summary_Data[12,a[i]])/Summary_Data[12,a[i]]),3)
                if (Dif_95_99  >= .25){Dataset[(Dataset[,a[i]] > Summary_Data[11,a[i]]),a[i]] <- Summary_Data[11,a[i]]}
                if (Dif_99_100 >= .25){Dataset[(Dataset[,a[i]] > Summary_Data[12,a[i]]),a[i]] <- Summary_Data[12,a[i]]}
            }
        }
    }
    
    #Summary Statistics Post Imputation & Outlier Treatment
    Summary_Data <- as.data.frame(apply(Dataset[,colnames(Dataset) %in% c(D_Var,Ind_Var)], 2, summary_stats))
    Summary_Data$stat <- c("OBS","NMIS","0% Min","1%","5%","10%","25% Q1","50% Median","75% Q3","90%","95%","99%","100% Max","Mean","Median","SD","CV (Mean/SD)","Range (Max-Min)","IQR (Q3-Q1)","Skewness(-1,+1)","Kurtosis(0,1)")
    Summary_Data <- data.frame(Summary_Data, row.names=NULL)
    Summary_Data <- cbind(Summary_Data$stat,Summary_Data[,(names(Summary_Data) != "stat")] )
    names(Summary_Data)[names(Summary_Data) == "Summary_Data$stat"] <- "Stats"
    Summary_Data[,-1] <- round(Summary_Data[,-1],2)  
    Univar_Stats_Post_Outlier <- Summary_Data
    
    #### Check for Perfect MultiCollinearity
    varlist <- colnames(Dataset)[colnames(Dataset) %in% Ind_Var]
    Formula <- formula(paste(paste(D_Var,"~ "), paste(varlist, collapse=" + ")))
    fit <- lm(Formula, data=Dataset)
    n <- rownames(alias(fit)$Complete)
    tempNames <- varlist[!varlist %in% n]
    
    #### Check Degress of Freedom
    if (length(tempNames) >= nrow(Dataset)-1){tempNames <- tempNames[c(1-(nrow(Dataset)-2))]}
   
    ####   Variable Reduction Using VIF
    if (length(tempNames) >1) {
        for (i in c(1:length(tempNames))){        
            Formula <- formula(paste(paste(D_Var,"~ "), paste(tempNames, collapse=" + ")))
            fit <- lm(Formula, data=Dataset[,c(D_Var,tempNames)])
            VIF_Data <- as.data.frame(vif(fit))
            VIF_Data$Vars <- rownames(VIF_Data, do.NULL = TRUE, prefix = "row")
            VIF_Data <- data.frame(VIF_Data[order(-VIF_Data[,1]), ],row.names=NULL)
            if(VIF_Data[1,1] <= 5)break
            tempNames = VIF_Data[-1,2]
            if(length(tempNames) == 1)break
        }
    }
    
    if (length(tempNames) >8) {
      for (i in c(1:length(tempNames))){        
        Formula <- formula(paste(paste(D_Var,"~ "), paste(tempNames, collapse=" + ")))
        fit <- lm(Formula, data=Dataset[,c(D_Var,tempNames)])
        VIF_Data <- as.data.frame(vif(fit))
        VIF_Data$Vars <- rownames(VIF_Data, do.NULL = TRUE, prefix = "row")
        VIF_Data <- data.frame(VIF_Data[order(-VIF_Data[,1]), ],row.names=NULL)
        if(VIF_Data[1,1] <= 3)break
        tempNames = VIF_Data[-1,2]
        if(length(tempNames) == 1)break
      }
    }
        
    #Calculating the Relative Importance Of Varaible
    if (length(tempNames) > 8 & length(tempNames) <= 15){
    Formula <- formula(paste(paste(D_Var,"~ "), paste(tempNames, collapse=" + ")))
    x <- calc.relimp(Formula, Dataset, type = c("lmg"),  rela = TRUE) 
    x <- as.data.frame(x$lmg)
    x$names <- rownames(x) 
    x$Rank <- rank(-x[1], ties.method = "first")
    tempNames <- x[x$Rank <= 8 ,"names"]}
    
    if (length(tempNames) >15) {
      for (i in c(1:length(tempNames))){        
        Formula <- formula(paste(paste(D_Var,"~ "), paste(tempNames, collapse=" + ")))
        fit <- lm(Formula, data=Dataset[,c(D_Var,tempNames)])
        VIF_Data <- as.data.frame(vif(fit))
        VIF_Data$Vars <- rownames(VIF_Data, do.NULL = TRUE, prefix = "row")
        VIF_Data <- data.frame(VIF_Data[order(-VIF_Data[,1]), ],row.names=NULL)
        tempNames = VIF_Data[-1,2]
        if(length(tempNames) == 8)break
      }
    }
    
#     Random Forest
#     if (length(tempNames) > 10){
#     x <- cforest(formula(paste0(D_Var,"~.")) , data= Dataset[,c(D_Var,tempNames)], control=cforest_unbiased(mtry=2,ntree=50))
#     x <- as.data.frame(cbind(VarNames = row.names(as.data.frame(varimp(x))), Accuracy =varimp(x)))
#     x$Accuracy <- as.numeric(as.character(x$Accuracy))
#     x <- data.frame(x[order(-x[,2]),],row.names=NULL)
#     tempNames <-  as.character(x$VarNames[1:8])
#     }
       
#     Stepwise Regression
#     if (length(tempNames) > 8){
#     library(MASS)
#     Formula <- formula(paste(paste(D_Var,"~ "), paste(tempNames, collapse=" + ")))
#     fit <- lm(Formula, data=Dataset[,c(D_Var,tempNames)])
#     step <- stepAIC(fit, direction="both")
#     step$anova # display results   
#     x <- (step$anova[-1,])
#     if (length(x$Step) > 8){tempNames <- as.character(x$Step)[1:8]}
#     if (length(x$Step) <= 8){tempNames <- as.character(x$Step)}
#     tempNames <- gsub("-+\\s", "",tempNames)
#     }    
        
    #Split Data into Development and Validation
    set.seed(123)
    indexes = sample(1:nrow(Dataset), size=(Dev_Val_Split_Per/100)*nrow(Dataset))
    Dataset_D <- Dataset[indexes,]
    Dataset_V <- Dataset[-indexes,]
    Model = list()
    ##### Defining All (2^n-1) Possible Combinations of Selected Variables
    for (m in c(1:length(tempNames))){          
        y <- combinations(length(tempNames),m,tempNames,repeats=FALSE)
        for (n in c(1:length(y[,1]))){
            y1 <- c(y[n,])
            tempColumn <- NA
            if(length(y1) > 0) {
                for(i in 1:length(y1)) {
                    tempColumn[i] <- which(colnames(Dataset) == y1[i])
                }
            }
            Var_List <- paste(y1,collapse=" + ")
            
            #### Linear Regression
            Formula   <- formula(paste(paste(D_Var,"~ "), paste(y1, collapse=" + ")))
            fit       <- lm(Formula, data=Dataset_D)  
            Adj_R_Sqr <- round(summary(fit)$adj.r.squared,3)*100
            AIC       <- round(AIC(fit),2)
            BIC       <- round(BIC(fit),2)
            f         <- summary(fit)$fstatistic
            Model_p   <- pf(f[1],f[2],f[3],lower.tail=F)
            p_vals    <- as.data.frame(summary(fit)$coefficients)[, 4]
            p_vals    <- round(c(Model_p,p_vals),6)
            sig       <- ifelse (sum(ifelse(p_vals <= 0.1,0,1)) == 0,"Significant","Not_Sig") 
            Model[[length(Model)+1]] = fit
            Model_Fit <- predict(fit,Dataset_D)
            Forecast  <- predict(fit,Dataset_V)
            
            a <- as.data.frame(summary(fit)$coefficients)
            a$names <- rownames(a) 
            a$equ <- paste0("(",round(a$Estimate,3),"*",a$names,")")
            Equation <- paste(a$equ, collapse=" + ")
            Equation <- gsub(Equation ,pattern = "[*][(]Intercept[)]",replacement = "")
            
            g <- round(abs(Dataset_D[,D_Var]- Model_Fit)/Dataset_D[,D_Var],4)
            g[is.infinite(g)]<-NA
            Training_MAPE <- round(100*(sum(g,na.rm=TRUE)/length(which(!is.na(g)))),2)
            
            g <- round(abs(Dataset_V[,D_Var]- Forecast)/Dataset_V[,D_Var],4)
            g[is.infinite(g)]<-NA
            Test_MAPE <- round(100*(sum(g,na.rm=TRUE)/length(which(!is.na(g)))),2)
            
            a <- numeric(length(Model_Fit))
            for (i in c(1:length(a))){a[i] <-  round((min(Dataset_D[,D_Var][i],Model_Fit[i]) / max(Dataset_D[,D_Var][i],Model_Fit[i])), 3)}
            Training_Min_Max <- round(mean(a)*100,1)
            a <- numeric(length(Forecast))
            for (i in c(1:length(a))){a[i] <-  round((min(Dataset_V[,D_Var][i],Forecast[i]) / max(Dataset_V[,D_Var][i],Forecast[i])), 3)}
            Test_Min_Max <- round(mean(a)*100,1)
            
            X_Reg <- as.data.frame(cbind(significance = sig,Adj_R_Sqr = Adj_R_Sqr, AIC = AIC, BIC =BIC,Equation=Equation, Training_MAPE = Training_MAPE, Test_MAPE=Test_MAPE, Training_Accuracy=Training_Min_Max, Test_Accuracy=Test_Min_Max))
            ifelse(n==1 ,X_Reg_1 <- X_Reg, X_Reg_1 <-rbind(X_Reg_1,X_Reg))
        }
        ifelse(m==1 ,All_Models <- X_Reg_1, All_Models<-rbind(All_Models,X_Reg_1))
        list(All_Models,Model)
    }
    
    All_Models_Specs <- Model[which(All_Models$significance == "Significant")]
    All_Models <- subset(All_Models, significance=="Significant")
    
	  x <- which(All_Models$BIC == (min(as.numeric(as.character(All_Models$BIC)))))
    Best_Model_Specs <- All_Models_Specs[x]
    Best_Model_Specs <- Best_Model_Specs[[1]]
    Best_Model <- All_Models[x,]
   
    return (list("Best_Model"= Best_Model ,"All_Models"=All_Models, "Univar_Stats_Pre_Outlier"=Univar_Stats_Pre_Outlier,"Univar_Stats_Post_Outlier"=Univar_Stats_Post_Outlier,"Best_Model_Specs" = Best_Model_Specs,"All_Models_Specs" =All_Models_Specs ))
}


