set.seed(123)
N     <- 100
X1    <- rnorm(N, 175, 7)
X2    <- rnorm(N,  30, 8)
Ycont <- 0.5*X1 - 0.3*X2 + 10 + rnorm(N, 0, 6)
Yord  <- cut(Ycont, breaks=quantile(Ycont), include.lowest=TRUE,
             labels=c("1", "2", "3", "4"), ordered=TRUE)
Ordinal_Model_Data <- data.frame(X1, X2, Yord)
#dfOrd$Yord <- as.numeric(dfOrd$Yord)
Dataset <- Ordinal_Model_Data
D_Var <- "Yord"
Ind_Var <- colnames(Dataset)[c(1,2)]
Dev_Val_Split_Per <- 25
Imputation <- TRUE
Imputation_Per <- 5
Outlier_Treatment <- TRUE

Ord_Logit_Auto <- function(Dataset,D_Var,Ind_Var,Dev_Val_Split_Per=25,Imputation=TRUE,Imputation_Per=5,Outlier_Treatment=TRUE){
  ####Required Libraries
  wants <- c("moments", "DMwR", "Boruta", "car", "gtools","aod","VGAM")
  has   <- wants %in% rownames(installed.packages())
  if(any(!has)) install.packages(wants[!has])
  
  library(moments)
  library(DMwR)
  library(Boruta)
  library(car)
  library(gtools)
  library(aod)
  library(VGAM)
  
  ####Summary Statistics Computation
  summary_stats <- function(x)
  {return(c(
    nrow(Dataset),nrow(Dataset)-NROW(na.omit(x)),min(x,na.rm=T),quantile(x,c(.01,.05,.10,.25,.50,.75,.9,.95,.99),na.rm=T),
    max(x,na.rm=T),mean(x,na.rm=T),median(x,na.rm=T),sd(x,na.rm=T),100*sd(x,na.rm=T)/mean(x,na.rm=T),max(x,na.rm=T)- min(x,na.rm=T),
    IQR(x,na.rm=T),skewness(x,na.rm=T),kurtosis(x,na.rm=T)
  ))}
  Summary_Data <- as.data.frame(apply(Dataset[,c(Ind_Var)], 2, summary_stats))
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
  Summary_Data <- as.data.frame(apply(Dataset[,colnames(Dataset) %in% c(Ind_Var)], 2, summary_stats))
  Summary_Data$stat <-c("OBS","NMIS","0% Min","1%","5%","10%","25% Q1","50% Median","75% Q3","90%","95%","99%","100% Max","Mean","Median","SD","CV (Mean/SD)","Range (Max-Min)","IQR (Q3-Q1)","Skewness(-1,+1)","Kurtosis(0,1)")
  Summary_Data<-data.frame(Summary_Data, row.names=NULL)
  Summary_Data <- cbind(Summary_Data$stat,Summary_Data[,(names(Summary_Data) != "stat")] )
  names(Summary_Data)[names(Summary_Data) == "Summary_Data$stat"] <- "Stats"
  Summary_Data[,-1] <-round(Summary_Data[,-1],2)  
  Univar_Stats_Post_Outlier <- Summary_Data
  
  #### Check for Perfect MultiCollinearity
  Dataset_1 <- cbind(Dataset,D_Var_1 = as.numeric(Dataset[,D_Var]))
  D_Var_1 <- "D_Var_1"
  
  varlist <- colnames(Dataset)[colnames(Dataset) %in% Ind_Var]
  Formula <- formula(paste(paste(D_Var_1,"~ "), paste(varlist, collapse=" + ")))
  fit <- lm(Formula, data=Dataset_1)
  n <- rownames(alias(fit)$Complete)
  tempNames <- varlist[!varlist %in% n]
  
  ####   Variable Reduction Using Baruta
  Boruta(formula(paste0(D_Var,"~.")),data=Dataset[,c(D_Var,tempNames)],doTrace=2)->Bor.son;
  stats<-subset(attStats(Bor.son),decision == "Confirmed");
  tempNames <- rownames(stats, do.NULL = TRUE, prefix = "row")
  
  ####   Variable Reduction Using VIF
  if (length(tempNames) >1) {
    for (i in c(1:length(tempNames))){        
      Formula <- formula(paste(paste(D_Var_1,"~ "), paste(tempNames, collapse=" + ")))
      fit <- lm(Formula, data=Dataset_1)
      VIF_Data <- as.data.frame(vif(fit))
      VIF_Data$Vars <- rownames(VIF_Data, do.NULL = TRUE, prefix = "row")
      VIF_Data <- data.frame(VIF_Data[order(-VIF_Data[,1]), ],row.names=NULL)
      if(VIF_Data[1,1] <= 3)break
      tempNames = VIF_Data[-1,2]
      if(length(tempNames) == 1)break
    }
  }
  
  ##### Defining All (2^n-1) Possible Combinations of Selected Variables
  for (m in c(1:length(tempNames))){
    y <- combinations(length(tempNames),m,tempNames,repeats=FALSE)
    
    for (n in c(1:length(y[,1]))){
      y1 <- c(y[n,])
      Var_List <- paste(y1,collapse=" + ")
      
      #Split Data into Development and Validation
      Dataset_D <- Dataset[c(1:70),]
      Dataset_V <- Dataset[c(71:100),]
      
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

Formula <- formula(paste(paste(D_Var,"~ "), paste(y1, collapse=" + ")))
vglmFit <- vglm(Formula, family=propodds, Dataset=Dataset)
#Predict The Order
Pred_vglmFit <- as.data.frame(VGAM::predict(vglmFit, Dataset, type="response"))
Pred_vglmFit$Predicted <- levels(Dataset$Yord)[max.col(Pred_vglmFit)]
#Correct Classification of Order
Pred_vglmFit$Predicted <- factor(Pred_vglmFit$Predicted, levels=levels(Dataset$Yord))
cTab   <- xtabs(~ Yord + Pred_vglmFit$Predicted, data=Dataset)
#addmargins(cTab)
CCR <- sum(diag(cTab)) / sum(cTab)
#McFadden_pseudo_R2 :: Log-likelihoods for full model and 0-model without predictors
Formula <- formula(paste0(D_Var," ~ 1"))
vglm0 <- vglm(Formula, family=propodds, data=Dataset)
LLf   <- VGAM::logLik(vglmFit)
LL0   <- VGAM::logLik(vglm0)
McFadden_pseudo_R2 <- round(as.vector(1 - (LLf / LL0)),3)
#AIC of the Model
AIC <- round(AIC(vglmFit),2)
BIC <- round(BIC(vglmFit),2)
sumOrd   <- summary(vglmFit)
coefOrd <- coef(sumOrd)
sig <- ifelse(length(coefOrd[,4]) == sum(coefOrd[,4] < 0.1*100),"Sig","Not_Sig")
X_Reg <- as.data.frame(cbind(significance=sig,CCR=CCR,McFadden_pseudo_R2=McFadden_pseudo_R2,AIC=AIC,BIC=BIC,Var_list=Var_List))
ifelse(n==1 ,X_Reg_1 <- X_Reg, X_Reg_1 <-rbind(X_Reg_1,X_Reg))
    }
ifelse(m==1 ,All_Models <- X_Reg_1, All_Models<-rbind(All_Models,X_Reg_1))
  }
#All_Models <- subset(All_Models, significance=="Sig")
return (list(All_Models,Univar_Stats_Pre_Outlier,Univar_Stats_Post_Outlier))
}


set.seed(123)
N     <- 1000
X1    <- rnorm(N, 175, 7)
X2    <- rnorm(N,  30, 8)
Ycont <- 0.5*X1 - 0.3*X2 + 10 + rnorm(N, 0, 6)
Yord  <- cut(Ycont, breaks=quantile(Ycont), include.lowest=TRUE,
             labels=c("1", "2", "3", "4"), ordered=TRUE)
Ordinal_Model_Data <- data.frame(X1, X2, Yord)

Output <- Ord_Logit_Auto(Ordinal_Model_Data,"Yord",colnames(Ordinal_Model_Data)[c(1,2)])
All_Models <- as.data.frame(Output[1])
Univar_Stats_Pre_Outlier <- as.data.frame(Output[2])
Univar_Stats_Post_Outlier <- as.data.frame(Output[3])














