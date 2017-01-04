
x<-rnorm(1000, mean=0, sd=50)
summary(x)
bins<-10
cutpoints<-quantile(x,(0:bins)/bins)
binned <-cut(x,cutpoints,include.lowest=TRUE)
table(binned)
summary(binned)


# Load package and its data 
library(smbinning) 
data(chileancredit) 
# Training and testing samples 
chileancredit.train=subset(chileancredit,FlagSample==1) 
chileancredit.test=subset(chileancredit,FlagSample==0) 
# Run and save results 
result=smbinning(df=chileancredit.train,y="FlagGB",x="TOB",p=0.05) 

table(chileancredit.train$FlagGB)

# Relevant plots (2x2 Page) 
par(mfrow=c(2,2)) 
boxplot(chileancredit.train$TOB~chileancredit.train$FlagGB, 
        horizontal=T, frame=F, col="lightgray",main="Distribution") 
mtext("Time on Books (Months)",3) 
smbinning.plot(result,option="dist",sub="Time on Books (Months)") 
smbinning.plot(result,option="badrate",sub="Time on Books (Months)") 
smbinning.plot(result,option="WoE",sub="Time on Books (Months)")

