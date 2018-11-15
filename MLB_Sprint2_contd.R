setwd("C:/Users/jpano/Documents/DATA ANALYTICS COURSE/CSDA1010/My R Stuff")
library(readr)
library(ggplot2)
library(dplyr)

mlbdata=read_csv("MLBSprint2Data.csv",col_name=TRUE,na=c("","NA","#NA")) 
peryear=count(mlbdata,yearID)
peryear

yr15=filter(mlbdata, yearID == 2015)
yr16 = filter(mlbdata, yearID == 2016)

head(yr15)
head(yr16)

yr15yr16=left_join(yr15,yr16,by=c("playerID", "teamID"),na=c("","NA","#NA"))

#write.csv(yr14yr15,file = "yr14yr15.csv")

#check for missing values
missv=function(x){sum(is.na(x))}
apply(yr15yr16,2,missv)


years1516=yr15yr16[!is.na(yr15yr16$yearID.y),]
summary(years1516) #checked no more NAs in BA
head(years1516)

#add new class - whether or not improved from one year to next
years1516$Improved=ifelse(years1516$OPSClass.x=="AvgPoor"&years1516$OPSClass.y=="Good","Yes","No")
head(years1516$Improved)

# no. and proportion of improved players
freq_imp_1516=table(years1516$Improved)
head(freq_imp_1516)
prop.table(freq_imp_1516)
barplot(freq_imp_1516) 




###########################################
###########################################
##look at whether players improved in OPS for other years
################################################


################
#2014 to 2015
#################
yr14 = filter(mlbdata, yearID == 2014)
yr15=filter(mlbdata, yearID == 2015)

head(yr14)
head(yr15)

yr14yr15=left_join(yr14,yr15,by=c("playerID", "teamID"),na=c("","NA","#NA"))

#write.csv(yr14yr15,file = "yr14yr15.csv")

#check for missing values
missv=function(x){sum(is.na(x))}
apply(yr14yr15,2,missv)


years1415=yr14yr15[!is.na(yr14yr15$yearID.y),]
summary(years1415) #checked no more NAs in BA
head(years1415)

#add new class - whether or not improved from 2014 to 2015
years1415$Improved=ifelse(years1415$OPSClass.x=="AvgPoor"&years1415$OPSClass.y=="Good","Yes","No")
head(years1415$Improved)

# no. and proportion of improved players
freq_imp_1415=table(years1415$Improved)
prop.table(freq_imp_1415)
barplot(freq_imp_1415) 

################
#2013 to 2014
#################
yr13 = filter(mlbdata, yearID == 2013)

head(yr13)

yr13yr14=left_join(yr13,yr14,by=c("playerID","teamID"),na=c("","NA","#NA"))

#check for missing values
apply(yr13yr14,2,missv)

years1314=yr13yr14[!is.na(yr13yr14$yearID.y),]
summary(years1314) #checked no more NAs in BA
head(years1314)

#add new class - whether or not improved from 2014 to 2015
years1314$Improved=ifelse(years1314$OPSClass.x=="AvgPoor"&years1314$OPSClass.y=="Good","Yes","No")
head(years1314$Improved)

# no. and proportion of improved players
freq_imp_1314=table(years1314$Improved)
prop.table(freq_imp_1314)
barplot(freq_imp_1314) 

################
#2012 to 2013
#################
yr12 = filter(mlbdata, yearID == 2012)

head(yr12)

yr12yr13=left_join(yr12,yr13,by=c("playerID","teamID"),na=c("","NA","#NA"))

#check for missing values
apply(yr12yr13,2,missv)

years1213=yr12yr13[!is.na(yr12yr13$yearID.y),]
summary(years1213) #checked no more NAs in BA
head(years1213)

#add new class - whether or not improved from 2014 to 2015
years1213$Improved=ifelse(years1213$OPSClass.x=="AvgPoor"&years1213$OPSClass.y=="Good","Yes","No")
head(years1213$Improved)

# no. and proportion of improved players
freq_imp_1213=table(years1213$Improved)
prop.table(freq_imp_1213)
barplot(freq_imp_1213) 

freq_tot=freq_imp_1213+freq_imp_1314+freq_imp_1415
head(freq_tot)
prop.table(freq_tot)
barplot(freq_tot) 

################
#2011 to 2012
#################
yr11 = filter(mlbdata, yearID == 2011)

head(yr11)

yr11yr12=left_join(yr11,yr12,by=c("playerID","teamID"),na=c("","NA","#NA"))

#check for missing values
apply(yr11yr12,2,missv)

years1112=yr11yr12[!is.na(yr11yr12$yearID.y),]
summary(years1112) #checked no more NAs in BA
head(years1112)

#add new class - whether or not improved from 2014 to 2015
years1112$Improved=ifelse(years1112$OPSClass.x=="AvgPoor"&years1112$OPSClass.y=="Good","Yes","No")
head(years1112$Improved)

# no. and proportion of improved players
freq_imp_1112=table(years1112$Improved)
prop.table(freq_imp_1112)
barplot(freq_imp_1112) 

freq_tot=freq_imp_1112+freq_imp_1213+freq_imp_1314+freq_imp_1415+freq_imp_1516
head(freq_tot)
prop.table(freq_tot)
barplot(freq_tot) 

years11to16=bind_rows(years1112,years1213,years1314,years1415,years1516)
head(years11to16)
nrow(years11to16)
ncol(years11to16)

write.csv(years11to16,file = "yrs2011-2016.csv")

######################################################
#visualization of Improved class with bins

library(Hmisc)
bindata = cbind(years11to16)
head(bindata)

#Runs
fact_R = cut2(bindata$R.x, g=10, minmax=TRUE, oneval=TRUE)
R_binned=cbind(bindata,fact_R)
ggplot(R_binned, aes(fact_R, ..count..)) + geom_bar(aes(fill = years11to16$Improved), position = "stack")+labs(title=paste("Improvement in OPS by Runs"))

#RBIs
fact_RBI = cut2(bindata$RBI.x, g=10, minmax=TRUE, oneval=TRUE)
RBI_binned=cbind(bindata,fact_RBI)
ggplot(RBI_binned, aes(fact_RBI, ..count..)) + geom_bar(aes(fill = years11to16$Improved), position = "stack")+labs(title=paste("Improvement in OPS by RBIs"))

#IBB
fact_IBB = cut2(bindata$IBB.x, g=10, minmax=TRUE, oneval=TRUE)
IBB_binned=cbind(bindata,fact_IBB)
ggplot(IBB_binned, aes(fact_IBB, ..count..)) + geom_bar(aes(fill = years11to16$Improved), position = "stack")+labs(title=paste("Improvement in OPS by IBBs"))

#SF
fact_SF = cut2(bindata$SF.x, g=10, minmax=TRUE, oneval=TRUE)
SF_binned=cbind(bindata,fact_SF)
ggplot(SF_binned, aes(fact_SF, ..count..)) + geom_bar(aes(fill = years11to16$Improved), position = "stack")+labs(title=paste("Improvement in OPS by SFs"))

#BA
fact_BA = cut2(bindata$BA.x, g=10, minmax=TRUE, oneval=TRUE)
BA_binned=cbind(bindata,fact_BA)
ggplot(BA_binned, aes(fact_BA, ..count..)) + geom_bar(aes(fill = years11to16$Improved), position = "stack")+labs(title=paste("Improvement in OPS by BA"))

#Hits
fact_H = cut2(bindata$H.x, g=10, minmax=TRUE, oneval=TRUE)
H_binned=cbind(bindata,fact_H)
ggplot(H_binned, aes(fact_H, ..count..)) + geom_bar(aes(fill = years11to16$Improved), position = "stack")+labs(title=paste("Improvement in OPS by Hits"))

#MODELLING

#split data into train and test 70-30% split
require(caTools)
set.seed(101) 
sampleimp = sample.split(years11to16$Improved, SplitRatio = .70)
trainimp = subset(years11to16, sampleimp == TRUE)
testimp = subset(years11to16, sampleimp == FALSE)

#decision tree

library(rpart)
fitimp <- rpart(Improved~R.x+RBI.x+BA.x+H.x+HR.x+IBB.x+SF.x+BB.x,data=trainimp,method="class")
fitimp

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

#MODELLING

#split data into train and test 70-30% split
require(caTools)
set.seed(101) 
sampleimp = sample.split(new1$Improved, SplitRatio = .70)
trainimp = subset(new1, sampleimp == TRUE)
testimp = subset(new1, sampleimp == FALSE)

#decision tree

library(rpart)
fitimp <- rpart(Improved ~R.x+RBI.x+BA.x+H.x+HR.x+IBB.x+SF.x+BB.x,data=trainimp,method="class")

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



#LOGISTIC REGRESSION
#impmodel <- glm(new1$Improved~R.x+RBI.x+BA.x+OPS.x+IBB.x+SF.x,family=binomial(link='logit'),data=trainimp)
#summary(impmodel)
#anova(impmodel,test="Chisq")

#predimp <- predict(impmodel,new1=subset(testimp),type='response') 
#predimp <- ifelse(predimp > 0.5,1,0)

#misclasserror <- mean(redimp != new1$Improved) 
#print(paste('Accuracy',1-misclasserror)) 


