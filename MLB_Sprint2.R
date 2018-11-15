setwd("C:/Users/jpano/Documents/DATA ANALYTICS COURSE/CSDA1010/My R Stuff")
library(readr)
library(ggplot2)
library(dplyr)

appearances=read_csv("Appearances.csv", col_name=TRUE,na=c("","NA","#NA"))
batting=read_csv("Batting.csv",col_name=TRUE,na=c("","NA","#NA"))
fielding=read_csv("Fielding.csv",col_name=TRUE,na=c("","NA","#NA"))
appear_bat=merge(appearances,batting,by=c("yearID","playerID","teamID"))
appear_bat_field=merge(appear_bat,fielding,by=c("yearID","playerID","teamID"))

baseball <- appear_bat_field
head(baseball)

#Data Prep

#get rid of stats only applicable to catchers which is a very small subset of players (lots of NAs)
baseball$PB=baseball$WP=baseball$SB.y=baseball$CS.y=baseball$ZR=NULL
head(baseball)

#identify any other duplicate columns:
baseball[duplicated(as.list(baseball))]# shows that lgID.y and lgID are duplicate so remove
baseball$lgID=baseball$lgID.y=NULL

head(baseball) #checked columns removed

#remove rows prior to 1960 after post-war era and rows related to pitchers since they skew the batting stats
baseball_sub <- filter (baseball, yearID >=1960)
head(baseball_sub)


#remove pitchers from data

baseball_sub2 = filter(baseball_sub, POS != "P")
nrow(baseball_sub2)
ncol(baseball_sub2)


# create a new dataset with a new column with batting average
baseball_sub2$BA=baseball_sub2$H/baseball_sub2$AB

#check that new column added
head(baseball_sub2$BA)
summary(baseball_sub2)

# identify missing values
missv=function(x){sum(is.na(x))}
apply(baseball_sub2,2,missv)

#remove rows where BA is NA
baseball_sub2=baseball_sub2[!is.na(baseball_sub2$BA),]
summary(baseball_sub2) #checked no more NAs in BA

missv=function(x){sum(is.na(x))}
apply(baseball_sub2,2,missv) #checked no more NAs in BA

#convert datatypes from chr to int
cols = c(6,19,20,21,35,36,37,38,39,43,44)    
baseball_sub2[,cols] = apply(baseball_sub2[,cols], 2, function(x) as.integer(as.character(x)))

#extract appearances and batting separately
appear_bat_sub <- baseball_sub2[,c(1:39,49)]
field_sub <- baseball_sub2[,c(1:4,40:49)]
names(appear_bat_sub)
names(field_sub)

# find out # of duplicate rows
nrow(appear_bat_sub[duplicated(appear_bat_sub),])
#remove duplicates (or pull out distinct rows)
AppearBatSub_Clean=appear_bat_sub %>% distinct


##################
#######calculate OPS; refer to https://en.wikipedia.org/wiki/On-base_plus_slugging
#####################

#calculate singles as it is not in the dataset (needed for OPS calculation)
AppearBatSub_Clean$'1B' <- (AppearBatSub_Clean$H-AppearBatSub_Clean$'2B'-AppearBatSub_Clean$'3B'-AppearBatSub_Clean$HR)

#calculate OPS
AppearBatSub_Clean$OPS=((AppearBatSub_Clean$H+AppearBatSub_Clean$BB+AppearBatSub_Clean$HBP)/
                          (AppearBatSub_Clean$AB+AppearBatSub_Clean$BB+AppearBatSub_Clean$SF+AppearBatSub_Clean$HBP))+
                          (AppearBatSub_Clean$'1B'+2*AppearBatSub_Clean$'2B'+3*AppearBatSub_Clean$'3B'+4*AppearBatSub_Clean$'HR')/
                            AppearBatSub_Clean$AB
head(AppearBatSub_Clean)

#check BA without pitchers
summary(AppearBatSub_Clean$BA)
hist(AppearBatSub_Clean$BA)
ggplot(AppearBatSub_Clean)+geom_histogram(mapping=aes(x=BA),binwidth=.01)+coord_cartesian(xlim=c(0,1),ylim=c(0,1000))
boxplot(AppearBatSub_Clean$BA)
boxplot(AppearBatSub_Clean$H)
boxplot(AppearBatSub_Clean$AB)

#check distribution of AB
summary(AppearBatSub_Clean$AB)

#remove outliers from BA using percentile capping
qnt1 <- quantile(AppearBatSub_Clean$BA, probs=c(.25, .75), na.rm = T)
caps1 <- quantile(AppearBatSub_Clean$BA, probs=c(.05, .95), na.rm = T)
H1 <- 1.5 * IQR(AppearBatSub_Clean$BA, na.rm = T)
AppearBatSub_Clean$BA[AppearBatSub_Clean$BA < (qnt1[1] - H1)] <- caps1[1]
AppearBatSub_Clean$BA[AppearBatSub_Clean$BA > (qnt1[2] + H1)] <- caps1[2]
head(AppearBatSub_Clean$BA)
summary(AppearBatSub_Clean$BA)
boxplot(AppearBatSub_Clean$BA)
hist(AppearBatSub_Clean$BA)

#check OPS
summary(AppearBatSub_Clean$OPS)
ggplot(AppearBatSub_Clean)+geom_histogram(mapping=aes(x=OPS),binwidth=.005)+coord_cartesian(xlim=c(0,6),ylim=c(0,20))
hist(AppearBatSub_Clean$OPS)
quantile(AppearBatSub_Clean$OPS, na.rm = TRUE)
boxplot(AppearBatSub_Clean$OPS)


#treatment of outliers in OPS through percentile capping at 5% and 95% percentile
qnt <- quantile(AppearBatSub_Clean$OPS, probs=c(.25, .75), na.rm = T)
caps <- quantile(AppearBatSub_Clean$OPS, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(AppearBatSub_Clean$OPS, na.rm = T)
AppearBatSub_Clean2=AppearBatSub_Clean
AppearBatSub_Clean2$OPS[AppearBatSub_Clean$OPS < (qnt[1] - H)] <- caps[1]
AppearBatSub_Clean2$OPS[AppearBatSub_Clean$OPS > (qnt[2] + H)] <- caps[2]
head(AppearBatSub_Clean2$OPS)
summary(AppearBatSub_Clean2$OPS)
boxplot(AppearBatSub_Clean2$OPS)
hist(AppearBatSub_Clean2$OPS)

#plot H and AB against BA
ggplot(AppearBatSub_Clean2,aes(AppearBatSub_Clean2$H,AppearBatSub_Clean2$BA),na.rm=TRUE)+geom_point()+
  labs(x="Hits", y="Batting Average", title=paste("Hits vs Batting Average"))
ggplot(AppearBatSub_Clean2,aes(AppearBatSub_Clean2$AB,AppearBatSub_Clean2$BA),na.rm=TRUE)+geom_point()+
  labs(x="At Bats", y="Batting Average", title=paste("At Bats vs Batting Average"))
ggplot(AppearBatSub_Clean2,aes(AppearBatSub_Clean2$yearID,AppearBatSub_Clean2$BA),na.rm=TRUE)+geom_point()+
  labs(x="Year", y="Batting Average", title=paste("Batting Average Over Time"))

#plot H and AB against OPS
ggplot(AppearBatSub_Clean2,aes(AppearBatSub_Clean2$H,AppearBatSub_Clean2$OPS),na.rm=TRUE)+geom_point()+
  labs(x="Hits", y="OPS", title=paste("Hits vs OPS"))
ggplot(AppearBatSub_Clean2,aes(AppearBatSub_Clean2$AB,AppearBatSub_Clean2$OPS),na.rm=TRUE)+geom_point()+
  labs(x="At Bats", y="OPS", title=paste("At Bats vs OPS"))
ggplot(AppearBatSub_Clean2,aes(AppearBatSub_Clean2$yearID,AppearBatSub_Clean2$OPS),na.rm=TRUE)+geom_point()+
  labs(x="Year", y="OPS", title=paste("OPS Over Time"))

#***********************************************************************************

#group BA mean, OPS mean by year
BAgb=aggregate(AppearBatSub_Clean2$BA, by=list(AppearBatSub_Clean2$yearID), FUN=mean)
OPSgb=aggregate(AppearBatSub_Clean2$OPS, by=list(AppearBatSub_Clean2$yearID), FUN=mean)
ggplot(BAgb,aes(BAgb[,1],BAgb[,2]),na.rm=TRUE)+geom_line()+
  labs(x="Year", y="Mean of Batting Average", title=paste("Average Batting Average by Year"))
ggplot(OPSgb,aes(OPSgb[,1],OPSgb[,2]),na.rm=TRUE)+geom_line()+
  labs(x="Year", y="Mean of On Base Plus Slugging", title=paste("Average OPS by Year"))


# add binary variable for BA and OPS
AppearBatSub_Clean2$BAClass=ifelse(AppearBatSub_Clean$BA<.280,"AvgPoor","Good")
head(AppearBatSub_Clean2)
AppearBatSub_Clean2$OPSClass=ifelse(AppearBatSub_Clean$OPS<.800,"AvgPoor","Good")
head(AppearBatSub_Clean2)

#write to CSV file 
write.csv(AppearBatSub_Clean2,file="MLBSprint2Data.csv") 
#export to file and read it in a new script to identify players who do well in one year and whether they will do well in the next

##############################################

# Get only numeric colums
list_of_numcols1 = sapply(AppearBatSub_Clean2, is.numeric)
numcols1 = AppearBatSub_Clean2[ , list_of_numcols1]
#Melt data --> Turn it into skinny LONG table - good to aggregate

install.packages("reshape2")
library(reshape2)
melt_data1 = melt(numcols1, id.vars=c("yearID")) 

#This data structure is now suitable for a multiplot function
ggplot(data = melt_data1, mapping = aes(x = value)) + geom_histogram(bins = 10) + facet_wrap(~variable, scales = 'free_x')


#binning on BA
##################
library(Hmisc)
Newdata = cbind(AppearBatSub_Clean2, TARGET_CLASS=ifelse(AppearBatSub_Clean2$BA<.280, "AvgPoor", "Good"))
head(Newdata)

#Homeruns
factor_HR = cut2(Newdata$HR, g=10, minmax=TRUE, oneval=TRUE)
HR_binned=cbind(Newdata,factor_HR)
head(HR_binned)

ggplot(HR_binned, aes(factor_HR, ..count..)) + geom_bar(aes(fill = TARGET_CLASS), position = "stack")+labs(title=paste("Avg value of Target Batting Avg by HR Bin"))

#Runs
factor_R = cut2(Newdata$R, g=10, minmax=TRUE, oneval=TRUE)
R_binned=cbind(Newdata,factor_R)
head(R_binned)

ggplot(R_binned, aes(factor_R, ..count..)) + geom_bar(aes(fill = TARGET_CLASS), position = "stack")+labs(title=paste("Avg value of Target Batting Avg by R Bin"))

#RBIs
factor_RBI = cut2(Newdata$RBI, g=10, minmax=TRUE, oneval=TRUE)
RBI_binned=cbind(Newdata,factor_RBI)
head(RBI_binned)

ggplot(RBI_binned, aes(factor_RBI, ..count..)) + geom_bar(aes(fill = TARGET_CLASS), position = "stack")+labs(title=paste("Avg value of Target Batting Avg by RBI Bin"))

#Walks
factor_BB = cut2(Newdata$BB, g=10, minmax=TRUE, oneval=TRUE)
BB_binned=cbind(Newdata,factor_BB)
head(BB_binned)

ggplot(BB_binned, aes(factor_BB, ..count..)) + geom_bar(aes(fill = TARGET_CLASS), position = "stack")+labs(title=paste("Avg value of Target Batting Avg by BB Bin"))

#binning on OPS
##################
Newdata1 = cbind(AppearBatSub_Clean2, TARGET_CLASS1=ifelse(AppearBatSub_Clean2$OPS<.800, "AvgPoor", "Good"))
head(Newdata1)

#visualize against variables not used in the calculation of OPS
#Runs
factor_R = cut2(Newdata1$R, g=10, minmax=TRUE, oneval=TRUE)
R_binned=cbind(Newdata1,factor_R)
head(R_binned)

ggplot(R_binned, aes(factor_R, ..count..)) + geom_bar(aes(fill = TARGET_CLASS1), position = "stack")+labs(title=paste("Avg value of Target OPS by R Bin"))

#RBIs
factor_RBI = cut2(Newdata1$RBI, g=10, minmax=TRUE, oneval=TRUE)
RBI_binned=cbind(Newdata1,factor_RBI)
head(RBI_binned)
ggplot(RBI_binned, aes(factor_RBI, ..count..)) + geom_bar(aes(fill = TARGET_CLASS1), position = "stack")+labs(title=paste("Avg value of Target OPS by RBI Bin"))

#IBB
factor_IBB = cut2(Newdata1$IBB, g=10, minmax=TRUE, oneval=TRUE)
IBB_binned=cbind(Newdata1,factor_IBB)
head(IBB_binned)
ggplot(IBB_binned, aes(factor_IBB, ..count..)) + geom_bar(aes(fill = TARGET_CLASS1), position = "stack")+labs(title=paste("Avg value of Target OPS by IBB Bin"))

#SF
factor_SF = cut2(Newdata1$SF, g=10, minmax=TRUE, oneval=TRUE)
SF_binned=cbind(Newdata1,factor_SF)
head(SF_binned)
ggplot(SF_binned, aes(factor_SF, ..count..)) + geom_bar(aes(fill = TARGET_CLASS1), position = "stack")+labs(title=paste("Avg value of Target OPS by SF Bin"))


#correlation matirx
CorrData= AppearBatSub_Clean2[,c("G.x","R","RBI","SB.x","SO","IBB", "GIDP","SF", "SH","HBP","BB", "BA", "OPS")]
cor(CorrData, method = c("pearson"),  use = "complete.obs")
cor_result=rcorr(as.matrix(CorrData))
cor_result$r #correlation coefficient
cor_result$P #p-values


#modelling

#split data into train and test 75-25% split
###########################################################################################
####### random split###############################
#######################################################################################
nrow(AppearBatSub_Clean2)

#predict OPS

require(caTools)
set.seed(101) 
sample = sample.split(AppearBatSub_Clean2$OPS, SplitRatio = .75)
train = subset(AppearBatSub_Clean2, sample == TRUE)
test  = subset(AppearBatSub_Clean2, sample == FALSE)
head(train)
head(test)

#decision tree
library(rpart)
attach(AppearBatSub_Clean2)

#on training set
opslm <- lm(OPS~RBI+IBB+HBP+GIDP,data = train)
summary(opslm)


#(fit <- (OPSClass~RBI+BA+IBB+GIDP,train,method="class"))
#library(rpart.plot)
#install.packages("rattle")
#library(rattle)
#fancyRpartPlot(fit)
#plotcp(fit)
#printcp(fit)

#validate on test data
validpred=predict(fit,test,type="class")
conf_matrix_val<-table(validpred,test$OPSClass)
conf_matrix_val
accuracy_val<-(conf_matrix_val[1,1]+conf_matrix_val[2,2])/(sum(conf_matrix_val))
print(paste('Accuracy',accuracy_val))

#predict BA
sample= sample.split(AppearBatSub_Clean2$BA, SplitRatio = .75)
train = subset(AppearBatSub_Clean2, sample == TRUE)
test  = subset(AppearBatSub_Clean2, sample == FALSE)
head(train)
head(test)

(fit1 <- rpart(BAClass~HR+R+RBI+BB+SB.x+CS.x+IBB+G.x+SO+HBP+SH+SF+GIDP,train,method="class"))
library(rpart.plot)
library(rattle)
fancyRpartPlot(fit1)
plotcp(fit1)
printcp(fit1)

#validate on test data
validpred1=predict(fit1,test,type="class")
conf_matrix_val1<-table(validpred1,test$BAClass)
conf_matrix_val1
accuracy_val1<-(conf_matrix_val1[1,1]+conf_matrix_val1[2,2])/(sum(conf_matrix_val1))
print(paste('Accuracy',accuracy_val1))


