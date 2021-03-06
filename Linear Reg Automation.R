
Reg_Model_Data <- read.csv("C:\\Users\\rkj49482\\Desktop\\Reg_Model\\Reg_Model_Data.csv")

Dataset <- Reg_Model_Data[c(1:60),]
D_Var <- "Dep_Var"
Ind_Var <- colnames(Dataset)[3:ncol(Dataset)]
Dev_Val_Split_Per <- 25
Imputation <- TRUE
Imputation_Per <- 5
Outlier_Treatment <- TRUE

#Required Libraries
library(moments)
library(DMwR)
library(Boruta)
library(car)
library(gtools)

#Summary Statistics Computation
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

### Missing Value Imputation
if (Imputation == TRUE){
Out_Names <- colnames(Summary_Data)[which(Summary_Data[2,2:ncol(Summary_Data)] > Imputation_Per)+1]
Dataset <- Dataset[,!names(Dataset) %in% Out_Names]
Dataset <- knnImputation(Dataset)
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
summary_stats <- function(x)
{return(c(
  nrow(Dataset),nrow(Dataset)-NROW(na.omit(x)),min(x,na.rm=T),quantile(x,c(.01,.05,.10,.25,.50,.75,.9,.95,.99),na.rm=T),
  max(x,na.rm=T),mean(x,na.rm=T),median(x,na.rm=T),sd(x,na.rm=T),100*sd(x,na.rm=T)/mean(x,na.rm=T),max(x,na.rm=T)- min(x,na.rm=T),
  IQR(x,na.rm=T),skewness(x,na.rm=T),kurtosis(x,na.rm=T)
))}
Summary_Data <- as.data.frame(apply(Dataset[,colnames(Dataset) %in% c(D_Var,Ind_Var)], 2, summary_stats))
Summary_Data$stat <-c("OBS","NMIS","0% Min","1%","5%","10%","25% Q1","50% Median","75% Q3","90%","95%","99%","100% Max","Mean","Median","SD","CV (Mean/SD)","Range (Max-Min)","IQR (Q3-Q1)","Skewness(-1,+1)","Kurtosis(0,1)")
Summary_Data<-data.frame(Summary_Data, row.names=NULL)
Summary_Data <- cbind(Summary_Data$stat,Summary_Data[,(names(Summary_Data) != "stat")] )
names(Summary_Data)[names(Summary_Data) == "Summary_Data$stat"] <- "Stats"
Summary_Data[,-1] <-round(Summary_Data[,-1],2) 

# Check for Perfect MultiCollinearity
varlist <- colnames(Dataset)[colnames(Dataset) %in% Ind_Var]
Formula <- formula(paste(paste(D_Var,"~ "), paste(varlist, collapse=" + ")))
fit <- lm(Formula, data=Dataset)
n <- rownames(alias(fit)$Complete)
tempNames <- varlist[!varlist %in% n]

####   Variable Reduction Using Baruta
Boruta(formula(paste0(D_Var,"~.")),data=Dataset[,c(D_Var,tempNames)],doTrace=2)->Bor.son;
stats<-subset(attStats(Bor.son),decision == "Confirmed");
tempNames <- rownames(stats, do.NULL = TRUE, prefix = "row")

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

##### Step-4 Defining All (2^n-1) Possible Combinations of Selected Variables From Step-1 and Step-3
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
    
    #Split Data into Development and Validation
    Dataset_D <- Dataset[c(1:48),]
    Dataset_V <- Dataset[c(49:60),]
    
    #### Linear Regression
    Formula <- formula(paste(paste(D_Var,"~ "), paste(y1, collapse=" + ")))
    fit       <- lm(Formula, data=Dataset_D)  
    Adj_R_Sqr <- round(summary(fit)$adj.r.squared,3)*100
    AIC       <- round(AIC(fit),2)
    BIC       <- round(BIC(fit),2)
    f         <- summary(fit)$fstatistic
    Model_p   <- pf(f[1],f[2],f[3],lower.tail=F)
    p_vals    <- as.data.frame(summary(fit)$coefficients)[, 4]
    p_vals    <- round(c(Model_p,p_vals),6)
    sig       <- ifelse (sum(ifelse(p_vals <= 0.1,0,1)) == 0,"Sig","Not_Sig") 
    Model_Fit <- predict(fit,Dataset_D)
    Forecast  <- predict(fit,Dataset_V)
    Training_MAPE <- 100-(round((100*sum(abs(Dataset_D[,D_Var]- Model_Fit)/Dataset_D[,D_Var],na.rm=TRUE))/length(Model_Fit),1))
    Forecast_MAPE <- 100-(round((100*sum(abs(Dataset_V[,D_Var]- Forecast) /Dataset_V[,D_Var],na.rm=TRUE))/length(Forecast),1))
    X_Reg <- as.data.frame(cbind(significance = sig,Adj_R_Sqr = Adj_R_Sqr, AIC = AIC, BIC =BIC,Var_list=Var_List, Training_MAPE = Training_MAPE,Forecast_MAPE=Forecast_MAPE))
    ifelse(n==1 ,X_Reg_1 <- X_Reg, X_Reg_1 <-rbind(X_Reg_1,X_Reg))
      }
  ifelse(m==1 ,All_Models <- X_Reg_1, All_Models<-rbind(All_Models,X_Reg_1))
}
All_Models <- subset(All_Models, significance=="Sig")
rm(list=ls()[! ls() %in% c("Reg_Model_Data","Dataset","Summary_Data","All_Models")])


