
library(smbinning) 
data(chileancredit) 
# Training and testing samples 
logit.train=subset(chileancredit,FlagSample==1) 
logit.test=subset(chileancredit,FlagSample==0)

data <- logit.train
y <- "FlagGB"
x <- c("Performance","CatGBI","InsAccts01","TOB","Bal01","Bal02","Bal03","Bal04","Bal05")
File_Path = "D:\\1.My_Data\\R\\testing"

Logit <- function(data,y,x,File_Path){
#Checking if the class of x Vars is a character
for (i in 1:length(x)){
  if (class(data[, x[i]]) == "character") {stop(paste0(x[i],'is not a numeric or factor'))}  
}

# Converting Indepemdent Numeric or Integer Varaibles to Numeric 
for(i in which(colnames(data) %in% x)) {
  if(class(data[, i]) == 'integer' | class(data[, i]) == 'numeric') 
  {data[,i] <- as.numeric(data[, i])} }

# Classifying the Variables into Continuous, Categorical Variables
Continuous  <- c("NA")
Categorical <- c("NA")
for(i in 1: length(x)) {
  if ((class(data[, x[i]]) == 'numeric' | class(data[, x[i]]) == 'factor') & length(unique(data[, x[i]])) >=10){
    a <- x[i]
    Continuous <- unique(c(Continuous,a))}
  if ((class(data[, x[i]]) == 'numeric' | class(data[, x[i]]) == 'factor') & length(unique(data[, x[i]])) < 10){
    c <- x[i]
    Categorical <- unique(c(Categorical,c)) 
    data[,x[i]] <- as.factor(data[,x[i]]) }
}
Continuous  <- Continuous[-1]
Categorical <- Categorical[-1]

#PDFPath <- paste0(File_Path,"\\IV_Continuous.pdf")
#pdf(file=PDFPath)  
#par(mfrow = c(2,1)) 

# Binning the Variables 
for (i in 1:length(x)){
 
  if (length(Continuous) > 0 & x[i] %in% Continuous & class(data[, x[i]]) == 'numeric')
  { result <- smbinning(df=data,y=y,x=x[i],p=0.05)
    data <- smbinning.gen(data,result,paste0("Cat_",x[i]))}
  
  if (length(Continuous) > 0 & x[i] %in% Continuous & class(data[, x[i]]) == 'factor')
  { result <- smbinning.factor(df=data,y=y,x=x[i])
    data <- smbinning.gen(data,result,paste0("Cat_",x[i]))}
  
  if (length(Categorical) > 0 & x[i] %in% Categorical & class(data[, x[i]]) == 'factor')
  { result <- smbinning.factor(df=data,y=y,x=x[i])
    data <- smbinning.gen(data,result,paste0("Cat_",x[i]))}
    
  data <- data[,-(which(names(data) == x[i]))]
  names(data)[names(data) %in% paste0("Cat_",x[i])] <- x[i]
    
  ivtable <- as.data.frame(cbind(Var = x[i] ,result$ivtable))
  
  if (i == 1) {IV_WOE_Table <- ivtable}
  if (ceiling(ivtable[ivtable$Cutpoint == "Total","IV"]) != 0) {
  IV_WOE_Table<-rbind(IV_WOE_Table,ivtable)
  #smbinning.plot(result,option="WoE",sub=paste0(y,"~",x[i]))
  }
}
#dev.off() 

IV_WOE_Table$Var <- as.character(IV_WOE_Table$Var)
a <- which(IV_WOE_Table$Cutpoint == "Total")[ceiling(IV_WOE_Table[which(IV_WOE_Table$Cutpoint == "Total"),"IV"]) == 0]
b <- IV_WOE_Table[a,"Var"]
IV_WOE_Table <- IV_WOE_Table[-(which(IV_WOE_Table$Var == b)),]

#col_names <- IV_WOE_Table[IV_WOE_Table$Cutpoint == "Total",c("Var","IV")]
#col_names <- col_names[order(-col_names$IV),] 
#col_names <- unique(col_names$Var)
#data <- data[,c(y,col_names$Var)]
return(list(IV_WOE_Table = IV_WOE_Table,data = data))
}

test <- Logit(data = logit.train,
      y = "FlagGB",
      x = c("Performance","CatGBI","InsAccts01","TOB","Bal01","Bal02","Bal03","Bal04","Bal05"),
      File_Path = "D:\\1.My_Data\\R\\testing")

output <- test$data
IV_WOE_Table <- test$IV_WOE_Table

