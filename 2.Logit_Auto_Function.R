
Logit_Auto <- function(Dataset,D_Var,Ind_Var,Dev_Val_Split_Per=25,Imputation=TRUE,Imputation_Per=5,Outlier_Treatment=TRUE){
  ####Required Libraries
  wants <- c("moments", "DMwR", "Boruta", "car", "gtools","aod")
  has   <- wants %in% rownames(installed.packages())
  if(any(!has)) install.packages(wants[!has])
  
  library(moments)
  library(DMwR)
  library(Boruta)
  library(car)
  library(gtools)
  library(aod)
  
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
  Summary_Data$stat <-c("OBS","NMIS","0% Min","1%","5%","10%","25% Q1","50% Median","75% Q3","90%","95%","99%","100% Max","Mean","Median","SD","CV (Mean/SD)","Range (Max-Min)","IQR (Q3-Q1)","Skewness(-1,+1)","Kurtosis(0,1)")
  Summary_Data<-data.frame(Summary_Data, row.names=NULL)
  Summary_Data <- cbind(Summary_Data$stat,Summary_Data[,(names(Summary_Data) != "stat")] )
  names(Summary_Data)[names(Summary_Data) == "Summary_Data$stat"] <- "Stats"
  Summary_Data[,-1] <-round(Summary_Data[,-1],2)  
  Univar_Stats_Post_Outlier <- Summary_Data
  
  #### Check for Perfect MultiCollinearity
  varlist <- colnames(Dataset)[colnames(Dataset) %in% Ind_Var]
  Formula <- formula(paste(paste(D_Var,"~ "), paste(varlist, collapse=" + ")))
  fit <- lm(Formula, data=Dataset)
  n <- rownames(alias(fit)$Complete)
  tempNames <- varlist[!varlist %in% n]
    
  ####   Variable Reduction Using VIF
  if (length(tempNames) >1) {
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

  ####   Variable Reduction Using Baruta
  Boruta(formula(paste0(D_Var,"~.")),data=Dataset[,c(D_Var,tempNames)],doTrace=2)->Bor.son;
  stats<-subset(attStats(Bor.son),decision == "Confirmed");
  tempNames <- rownames(stats, do.NULL = TRUE, prefix = "row")
  
  #Calculating the Relative Importance Of Varaible
  if (length(tempNames) > 8){
    Formula <- formula(paste(paste(D_Var,"~ "), paste(tempNames, collapse=" + ")))
    x <- calc.relimp(Formula, Dataset_D, type = "lmg", rank=TRUE, rela = TRUE) 
    x <- as.data.frame(x$lmg)
    x$names <- rownames(x) 
    x$Rank <- rank(-x[1], ties.method = "first")
    tempNames <- x[x$Rank <= 8 ,"names"]
  }
  
  ##### Defining All (2^n-1) Possible Combinations of Selected Variables
  for (m in c(1:length(tempNames))){
    y <- combinations(length(tempNames),m,tempNames,repeats=FALSE)
    
    for (n in c(1:length(y[,1]))){
      y1 <- c(y[n,])
      Var_List <- paste(y1,collapse=" + ")
      
      #Split Data into Development and Validation
      Dataset_D <- Dataset[c(1:700),]
      Dataset_V <- Dataset[c(701:1000),]
      
      CalculateConcordance <- function (myMod){       
        fitted <- data.frame (cbind (Dataset_D[,D_Var], myMod$fitted.values)) # actuals and fitted       
        colnames(fitted) <- c('response','score') # rename columns   
        ones <- fitted[fitted$response==1, ] # Subset ones        
        zeros <- fitted[fitted$response==0, ] # Subsetzeros        
        totalPairs <- nrow (ones) * nrow (zeros) # calculate total number of pairs to check        
        conc <- sum (c (vapply (ones$score, function(x) {((x > zeros$score))}, FUN.VALUE=logical(nrow(zeros)))))        
        disc <- totalPairs - conc
        # Calc concordance, discordance and ties
        concordance <- conc/totalPairs        
        discordance <- disc/totalPairs        
        tiesPercent <- (1-concordance-discordance)        
        return(list("Concordance"=concordance, "Discordance"=discordance,"Tied"=tiesPercent, "Pairs"=totalPairs))        
      }
      
      #### Linear Regression
      Formula    <- formula(paste(paste(D_Var,"~ "), paste(y1, collapse=" + ")))
      fit      <- glm(Formula, data = Dataset_D, family = "binomial")  
      AIC        <- round(AIC(fit),2)
      BIC        <- round(BIC(fit),2)
      #f          <- summary(fit)$fstatistic
      #Model_p    <- pf(f[1],f[2],f[3],lower.tail=F)
      p_vals     <- as.data.frame(summary(fit)$coefficients)[, 4]
      p_vals     <- round(c(p_vals),6)
      sig        <- ifelse (sum(ifelse(p_vals <= 0.1,0,1)) == 0,"Sig","Not_Sig")
      wald_test  <- wald.test(b = coef(fit), Sigma = vcov(fit),Terms = 2:(length(y1)+1))
      wald_chi2  <- round((wald_test$result$chi2[1]),2)
      wald_chi2_Pval  <- round((wald_test$result$chi2[3]),4)
      Concordance<- round((as.numeric(CalculateConcordance(fit)[1])*100),2)
      Discordance<- round((as.numeric(CalculateConcordance(fit)[2])*100),2)
      Ties       <- round((as.numeric(CalculateConcordance(fit)[3])*100),2)
      Pairs      <- round((as.numeric(CalculateConcordance(fit)[4])*100),2)
      Model_Fit <- fit$fitted.values
      Forecast   <- predict(fit,Dataset_V)
      X_Reg <- as.data.frame(cbind(significance = sig, AIC = AIC, BIC =BIC,Var_list=Var_List, Concordance = Concordance,Discordance = Discordance,Ties=Ties,wald_chi2=wald_chi2,wald_chi2_Pval=wald_chi2_Pval))
      ifelse(n==1 ,X_Reg_1 <- X_Reg, X_Reg_1 <-rbind(X_Reg_1,X_Reg))
    }
    ifelse(m==1 ,All_Models <- X_Reg_1, All_Models<-rbind(All_Models,X_Reg_1))
  }
  All_Models <- subset(All_Models, significance=="Sig")
  return (list("All_Models"=All_Models, "Univar_Stats_Pre_Outlier"=Univar_Stats_Pre_Outlier,"Univar_Stats_Post_Outlier"=Univar_Stats_Post_Outlier))
}

Logit_Model_Data <- read.csv("D:\\1.My_Data\\R\\Reg_Model\\Logit_Data.csv")
Output <- Logit_Auto(Logit_Model_Data,"ATTRITE",colnames(Logit_Model_Data)[2:(ncol(Logit_Model_Data)-1)])
attributes(Output)
All_Models <- Output$All_Models
Univar_Stats_Pre_Outlier <- Output$Univar_Stats_Pre_Outlier
Univar_Stats_Post_Outlier <- Output$Univar_Stats_Post_Outlier
