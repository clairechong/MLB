library(readr)
library(ggplot2)
library(dplyr)


mlbdata=read_csv("MLBSprint2Data.csv",col_name=TRUE,na=c("","NA","#NA")) 
peryear=count(mlbdata,"yearID")
peryear
recent=c(2015,2016)
recent_b = c(2014, 2015)
recent_c = c(2013, 2014)
recent_d = c(2012, 2013)
recent_e = c(2011, 2012)

#look at most recent years
mlbdata1=filter(mlbdata,yearID %in% recent)

mlbdata_b=filter(mlbdata,yearID %in% recent_b)

mlbdata_c=filter(mlbdata,yearID %in% recent_c)

mlbdata_d=filter(mlbdata,yearID %in% recent_d)

mlbdata_e=filter(mlbdata,yearID %in% recent_e)

#focus on certain columns
#mlbdata2=subset(mlbdata1, select=c(playerID, yearID, teamID,lgID.x,AB,R,H,'1B','2B','3B',HR,RBI,SB.x,CS.x,BB,SO,IBB,HBP,SH,SF,GIDP,BA,OPS,BAClass,OPSClass))

#order by player ID, then by year
mlbdata2=mlbdata1[order(mlbdata1$playerID,mlbdata2$yearID),]
head(mlbdata2)

#filter out 2015 & 2016 into their own tables
firstyr = filter(mlbdata1, yearID == 2015)
missfirstyr=function(x){sum(is.na(x))}
apply(firstyr,2,missfirstyr)

secyr=filter(mlbdata1, yearID == 2016)

head(firstyr)
head(secyr)

#filter out 2014 & 2015 into their own tables
firstyr_b = filter(mlbdata_b, yearID == 2014)
missfirstyr=function(x){sum(is.na(x))}
apply(firstyr_b,2,missfirstyr)

secyr_b=filter(mlbdata_b, yearID == 2015)

head(firstyr_b)
head(secyr_b)

#filter out 2013 & 2014 into their own tables
firstyr_c = filter(mlbdata_c, yearID == 2013)
missfirstyr=function(x){sum(is.na(x))}
apply(firstyr_c,2,missfirstyr)

secyr_c=filter(mlbdata_c, yearID == 2014)

head(firstyr_c)
head(secyr_c)

#filter out 2012 & 2013 into their own tables
firstyr_d = filter(mlbdata_d, yearID == 2012)
missfirstyr=function(x){sum(is.na(x))}
apply(firstyr_d,2,missfirstyr)

secyr_d=filter(mlbdata_d, yearID == 2013)

head(firstyr_d)
head(secyr_d)

#filter out 2011 & 2012 into their own tables
firstyr_e = filter(mlbdata_e, yearID == 2011)
missfirstyr=function(x){sum(is.na(x))}
apply(firstyr_e,2,missfirstyr)

secyr_e=filter(mlbdata_e, yearID == 2012)

head(firstyr_e)
head(secyr_e)

#join 2015 and 2016 based on players in 2015
new=left_join(firstyr,secyr,by=c("playerID","teamID"),na=c("","NA","#NA"))
head(new)
head(new$OPSClass.x)
write.csv(new,file="2015-2016players.csv") 

#join 2014 and 2015 based on players in 2014

new_b=left_join(firstyr_b,secyr_b,by=c("playerID","teamID"),na=c("","NA","#NA"))
head(new_b)
write.csv(new_b,file="2014-2015players.csv") 

#join 2013 and 2014 based on players in 2013

new_c=left_join(firstyr_c,secyr_c,by=c("playerID","teamID"),na=c("","NA","#NA"))
head(new_c)
write.csv(new_c,file="2013-2014players.csv") 

#join 2012 and 2013 based on players in 2012

new_d=left_join(firstyr_d,secyr_d,by=c("playerID","teamID"),na=c("","NA","#NA"))
head(new_d)
write.csv(new_d,file="2012-2013players.csv") 

#join 2011 and 2012 based on players in 2011

new_e=left_join(firstyr_e,secyr_e,by=c("playerID","teamID"),na=c("","NA","#NA"))
head(new_e)
write.csv(new_e,file="2011-2012players.csv") 

#check for missing values
missv=function(x){sum(is.na(x))}
apply(new_b,2,missv)

#remove rows where there are NAs - represents instances where data not avaiable in 2016 to compare to
new1=new[!is.na(new$yearID.y),]
summary(new1) #checked no more NAs in BA
head(new1)

new1_b=new_b[!is.na(new_b$yearID.y),]
summary(new1_b) #checked no more NAs in BA
head(new1_b)

new1_c=new_c[!is.na(new_c$yearID.y),]
summary(new1_c) #checked no more NAs in BA
head(new1_c)

new1_d=new_d[!is.na(new_d$yearID.y),]
summary(new1_d) #checked no more NAs in BA
head(new1_d)

new1_e=new_e[!is.na(new_e$yearID.y),]
summary(new1_e) #checked no more NAs in BA
head(new1_e)

#add new class - whether or not improved from 2015 to 2016
new1 = filter(new1, OPS.x<0.800)
new1$Improved=ifelse(new1$OPSClass.x=="AvgPoor"&new1$OPSClass.y=="Good","Yes","No")
head(new1$Improved)

#add new class - whether or not improved from 2014 to 2015
new1_b = filter(new1_b, OPS.x<0.800)
new1_b$Improved=ifelse(new1_b$OPSClass.x=="AvgPoor"&new1_b$OPSClass.y=="Good","Yes","No")

head(new1_b$Improved)

#add new class - whether or not improved from 2013 to 2014
new1_c = filter(new1_c, OPS.x<0.800)
new1_c$Improved=ifelse(new1_c$OPSClass.x=="AvgPoor"&new1_c$OPSClass.y=="Good","Yes","No")
head(new1_c$Improved)

#add new class - whether or not improved from 2012 to 2013
new1_d = filter(new1_d, OPS.x<0.800)
new1_d$Improved=ifelse(new1_d$OPSClass.x=="AvgPoor"&new1_d$OPSClass.y=="Good","Yes","No")
head(new1_d$Improved)

#add new class - whether or not improved from 2011 to 2012
new1_e = filter(new1_e, OPS.x<0.800)
new1_e$Improved=ifelse(new1_e$OPSClass.x=="AvgPoor"&new1_e$OPSClass.y=="Good","Yes","No")
head(new1_e$Improved)

# no. and proportion of improved players 2015 to 2016
freq_imp=table(new1$Improved)
prop.table(freq_imp)
barplot(freq_imp) 

# no. and proportion of improved players 2014 to 2015
freq_imp_b=table(new1_b$Improved)
prop.table(freq_imp_b)
barplot(freq_imp_b) 

# no. and proportion of improved players 2013 to 2014
freq_imp_c=table(new1_c$Improved)
prop.table(freq_imp_c)
barplot(freq_imp_c) 

# no. and proportion of improved players 2012 to 2013
freq_imp_d=table(new1_d$Improved)
prop.table(freq_imp_d)
barplot(freq_imp_d) 

# no. and proportion of improved players 2011 to 2012
freq_imp_e=table(new1_e$Improved)
prop.table(freq_imp_e)
barplot(freq_imp_e) 

#combining all data into one dataset called total

total = rbind(new1, new1_b, new1_c, new1_d, new1_e)
nrow(total)
summary(total)

######################################################modified by Emma


#visualization of Improved class with bins
bindata = cbind(new1)
head(bindata)

install.packages("Hmisc")

library(Hmisc)
#Runs in 2015
fact_R = cut2(bindata$R.x, g=10, minmax=TRUE, oneval=TRUE)
R_binned=cbind(bindata,fact_R)
ggplot(R_binned, aes(fact_R, ..count..)) + geom_bar(aes(fill = new1$Improved), position = "stack")+labs(title=paste("Improvement in 2016 by Runs in 2015"))

#RBIs
fact_RBI = cut2(bindata$RBI.x, g=10, minmax=TRUE, oneval=TRUE)
RBI_binned=cbind(bindata,fact_RBI)
ggplot(RBI_binned, aes(fact_RBI, ..count..)) + geom_bar(aes(fill = new1$Improved), position = "stack")+labs(title=paste("Improvement in 2016 by RBIs in 2015"))

#IBB
fact_IBB = cut2(bindata$IBB.x, g=10, minmax=TRUE, oneval=TRUE)
IBB_binned=cbind(bindata,fact_IBB)
ggplot(IBB_binned, aes(fact_IBB, ..count..)) + geom_bar(aes(fill = new1$Improved), position = "stack")+labs(title=paste("Improvement in 2016 by IBBs in 2015"))

#SF
fact_SF = cut2(bindata$SF.x, g=10, minmax=TRUE, oneval=TRUE)
SF_binned=cbind(bindata,fact_SF)
ggplot(SF_binned, aes(fact_SF, ..count..)) + geom_bar(aes(fill = new1$Improved), position = "stack")+labs(title=paste("Improvement in 2016 by SFs in 2015"))

#BA
fact_BA = cut2(bindata$BA.x, g=10, minmax=TRUE, oneval=TRUE)
BA_binned=cbind(bindata,fact_BA)
ggplot(BA_binned, aes(fact_BA, ..count..)) + geom_bar(aes(fill = new1$Improved), position = "stack")+labs(title=paste("Improvement in 2016 by BA in 2015"))

#OPS
fact_OPS = cut2(bindata$OPS.x, g=10, minmax=TRUE, oneval=TRUE)
OPS_binned=cbind(bindata,fact_OPS)
ggplot(OPS_binned, aes(fact_OPS, ..count..)) + geom_bar(aes(fill = new1$Improved), position = "stack")+labs(title=paste("Improvement in 2016 by OPS in 2015"))

#Hits
fact_H = cut2(bindata$H.x, g=10, minmax=TRUE, oneval=TRUE)
H_binned=cbind(bindata,fact_H)
ggplot(H_binned, aes(fact_H, ..count..)) + geom_bar(aes(fill = new1$Improved), position = "stack")+labs(title=paste("Improvement in 2016 by Hits in 2015"))

#MODELLING

#split data into train and test 70-30% split
require(caTools)
set.seed(101) 
sampleimp = sample.split(total$Improved, SplitRatio = .70)
trainimp = subset(total, sampleimp == TRUE)
testimp = subset(total, sampleimp == FALSE)

#decision tree

library(rpart)
fitimp <- rpart(Improved ~R.x+RBI.x+BA.x+H.x+HR.x+IBB.x+SF.x+BB.x+AB.x+`2B.x`,data=trainimp,method="class")

library(rpart.plot)
library(rattle)
fancyRpartPlot(fitimp)

#determine confusion matrix and accuracy score
#on training data
tree_predict=predict(fitimp,trainimp, type="class")
(conf_matrix=table(tree_predict,trainimp$Improved))

accuracy<-(conf_matrix[1,1]+conf_matrix[2,2])/(sum(conf_matrix))
accuracy

#validate on test data
validpred=predict(fitimp,testimp,type="class")
conf_matrix_val<-table(validpred,testimp$Improved)
conf_matrix_val
accuracy_val<-(conf_matrix_val[1,1]+conf_matrix_val[2,2])/(sum(conf_matrix_val))
print(paste('Accuracy',accuracy_val))
accuracy_val

##################################################oversampling

install.packages("ROSE") 
library(ROSE)

table(trainimp$Improved)
prop.table(table(trainimp$Improved))

#trainimp$G_p.x = trainimp$G_defense.x =trainimp$`2B.x` =trainimp$`3B.x`= trainimp$G_ph.x= trainimp$G_of.x = trainimp$G_rf.x= trainimp$G_cf.x= trainimp$G_lf.x= trainimp=NULL
trainimp = trainimp[c("R.x", "RBI.x", "H.x", "HR.x","IBB.x", "SF.x","BB.x", "AB.x","BA.x","HR.x","2B.x","3B.x","SB.x.x","CS.x.x","SO.x","HBP.x","SH.x","GIDP.x","Improved")]
testimp = testimp[c("R.x", "RBI.x", "H.x", "HR.x","IBB.x", "SF.x","BB.x", "AB.x", "Improved")]

colnames(trainimp) <- c("R", "RBI", "H", "HR","IBB", "SF","BB", "AB",  "Improved")
colnames(testimp) <- c("R", "RBI", "H", "HR","IBB", "SF","BB", "AB",  "Improved")

#oversampling of class YES, removed 2B column
data_balanced_over <- ovun.sample(as.formula("Improved ~ R+RBI+H+HR+IBB+SF+BB+AB") , data = trainimp, method = "over", p = 0.5, seed = 1)$data
table(data_balanced_over$Improved)


#or use ROSE
data_balanced_rose <- ROSE(as.formula("Improved ~ R+RBI+H+HR+IBB+SF+BB+AB") , data = trainimp, seed = 1)$data
table(data_balanced_rose$Improved)


#random forrest, which doesn't require cross validation, will use caret instead with cv

install.packages("caret")
library("caret")

train_control<- trainControl(method="cv", number=10, classProbs = TRUE, summaryFunction = twoClassSummary)

model<- train(Improved~., data=data_balanced_rose, trControl=train_control, method="rpart", metric = "ROC")
model

head(model$pred)
tail(model$pred)

testimp$pred = predict(model, newdata = testimp, type = "raw")

confusionMatrix(data = testimp$pred, reference = testimp$Improved,  mode = "prec_recall")

library(rpart.plot)
rpart.plot(model$finalModel)

#LOGISTIC REGRESSION
#impmodel <- glm(new1$Improved~R.x+RBI.x+BA.x+OPS.x+IBB.x+SF.x,family=binomial(link='logit'),data=trainimp)
#summary(impmodel)
#anova(impmodel,test="Chisq")

#predimp <- predict(impmodel,new1=subset(testimp),type='response') 
#predimp <- ifelse(predimp > 0.5,1,0)

#misclasserror <- mean(redimp != new1$Improved) 
#print(paste('Accuracy',1-misclasserror)) 


