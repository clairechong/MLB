#setwd("C:/Users/jpano/Documents/DATA ANALYTICS COURSE/CSDA1010/My R Stuff")
library(readr)
library(ggplot2)
library(dplyr)

#start from beginning since we may want to clean the data differently
batting=read_csv("Batting.csv",col_name=TRUE,na=c("","NA","#NA"))
salaries=read_csv("Salaries.csv", col_name=TRUE,na=c("","NA","#NA"))
mlbsalary=left_join(batting,salaries,by=c("yearID","playerID","teamID"),na=c("","NA","#NA"))
head(mlbsalary)

count(salaries, yearID)

#remove rows prior to 1985 since salary data starts in 1985
#try smaller dataset starting from 2005
mlbsalary1=filter(mlbsalary,yearID >=2005)
nrow(mlbsalary1)

#check missing values
check=function(x){sum(is.na(x))}
apply(mlbsalary1,2,check) 

mlbsalary1=mlbsalary1[!is.na(mlbsalary1$salary),]
check1=function(x){sum(is.na(x))}
apply(mlbsalary1,2,check1) 


#explore salaries data
summary(mlbsalary1$salary)
hist(mlbsalary1$salary)
boxplot(mlbsalary1$salary)

#convert datatypes from chr to int
cols = c("SF","GIDP")    
mlbsalary1[,cols] = apply(mlbsalary1[,cols], 2, function(x) as.integer(as.character(x)))

#calculate singles as it is not in the dataset (needed for OPS calculation)
attach(mlbsalary1)
mlbsalary1$'1B' <- (mlbsalary1$H-mlbsalary1$'2B'-mlbsalary1$'3B'-HR)

#calculate OPS
mlbsalary1$OPS=((H+BB+HBP)/(AB+BB+SF+HBP))+(mlbsalary1$'1B'+2*mlbsalary1$'2B'+3*mlbsalary1$'3B'+4*HR)/AB

quantile(mlbsalary1$OPS, na.rm = T)
quantile(mlbsalary1$AB, na.rm = T)


# check for NAs, 5520 NAs in OPS
summary(mlbsalary1$OPS) 
str(mlbsalary1$lgID.x)


quantile(mlbsalary1$G)
quantile(mlbsalary1$AB)
#filter by the games played and number of at bats
unique(mlbsalary1$teamID)
#removing teams with much lower salaries than typical teams
mlbsalary2<-mlbsalary1[!(mlbsalary1$teamID=="ANA" | mlbsalary1$teamID=="CAL" | mlbsalary1$teamID=="MIA" | mlbsalary1$teamID=="ML4" | mlbsalary1$teamID=="MON" | mlbsalary1$teamID=="FLO" |mlbsalary1$teamID=="TBA" |mlbsalary1$teamID=="WAS"),]
quantile(mlbsalary2$OPS, na.rm = T)

#removing players who didn't have enough AB
mlbsalary3= filter(mlbsalary2, AB >= 60 )
quantile(mlbsalary3$OPS)
quantile(mlbsalary3$salary)

#treatment of outliers in salary through percentile capping at 5% and 95% percentile
qnt <- quantile(mlbsalary3$salary, probs=c(.25, .75), na.rm = T)
caps <- quantile(mlbsalary3$salary, probs=c(.10, .90), na.rm = T)
H <- 1.5 * IQR(mlbsalary3$salary, na.rm = T)
#mlbsalary4=mlbsalary3
mlbsalary3$salary[mlbsalary3$salary < (qnt[1] - H)] <- caps[1]
mlbsalary3$salary[mlbsalary3$salary > (qnt[2] + H)] <- caps[2]

quantile(mlbsalary3$OPS)
quantile(mlbsalary3$salary)
#plot attributes vs salary
mlbsalary4 = mlbsalary3
salgb=aggregate(mlbsalary4$salary, by=list(mlbsalary4$yearID), FUN=median)

#due to inflation, salaries have increased significantly over the years. So corrected salaries w.r.t the median salary
#of a year is more relevant
names(salgb) = c("yearID", "mean_salary") 
#colnames(salgb)

#str(mlbsalary2$yearID)

#library(plyr)
#mlbsalary2 = mlbsalary1
#adding median salary values to the datafile
mlbsalary5 = left_join(mlbsalary4, salgb, by = "yearID")

#adding an attribute corrected salary by median salary of the year
mlbsalary5$corr_salary = mlbsalary5$salary/mlbsalary5$mean_salary

#check for NAs
apply(mlbsalary3,2,check) 

ggplot(salgb,aes(salgb[,1],salgb[,2]),na.rm=TRUE)+geom_line()+
  labs(x="Year", y="Salary Mean", title=paste("Average Salary by Year"))

ggplot(mlbsalary5,aes(mlbsalary5$OPS,mlbsalary5$corr_salary),na.rm=TRUE)+geom_point()+geom_smooth(method='lm')+
  labs(x="OPS", y="Corrected Salary", title=paste("OPS vs Corrected Salary"))

salgb1=aggregate(mlbsalary1$salary, by=list(mlbsalary1$OPS), FUN=mean)
ggplot(salgb1,aes(salgb1[,1],salgb1[,2]),na.rm=TRUE)+geom_point()+geom_smooth(method='lm')+
  labs(x="OPS", y="Salary Mean", title=paste("Average Salary by OPS"))

salgb2=aggregate(mlbsalary1$salary, by=list(mlbsalary1$teamID), FUN=mean)
ggplot(mlbsalary1,aes(teamID,salary),na.rm=TRUE)+geom_col()+geom_smooth(method='lm')+
  labs(x="Team", y="Salary Mean", title=paste("Average Salary by Team"))

ggplot(mlbsalary1,aes(H,salary),na.rm=TRUE)+geom_point()+geom_smooth(method='lm')+
  labs(x="Hits", y="Salary", title=paste("Hits vs Salary"))

ggplot(mlbsalary1,aes(AB,salary),na.rm=TRUE)+geom_point()+geom_smooth(method='lm')
ggplot(mlbsalary1,aes(R,salary),na.rm=TRUE)+geom_point()+geom_smooth(method='lm')
ggplot(mlbsalary1,aes(HR,salary),na.rm=TRUE)+geom_point()+geom_smooth(method='lm')
ggplot(mlbsalary1,aes(RBI,salary),na.rm=TRUE)+geom_point()+geom_smooth(method='lm')
ggplot(mlbsalary1,aes(SB,salary),na.rm=TRUE)+geom_point()+geom_smooth(method='lm')
ggplot(mlbsalary1,aes(SO,salary),na.rm=TRUE)+geom_point()+geom_smooth(method='lm')
ggplot(mlbsalary1,aes(BB,salary),na.rm=TRUE)+geom_point()+geom_smooth(method='lm')

#boxplot of salary by team and league
boxplot(mlbsalary1$salary~mlbsalary1$teamID)
boxplot(mlbsalary1$salary~mlbsalary1$lgID.x)
boxplot(mlbsalary1$salary ~ yearID, main="Salary by Year") 

#binned
Newdata = cbind(mlbsalary1)
head(Newdata)

library(Hmisc)
#salary
factor_sal = cut2(Newdata$salary, g=10, minmax=TRUE, oneval=TRUE)
sal_binned=cbind(Newdata,factor_salary)
ggplot(sal_binned, aes(factor_sal, ..count..)) + geom_bar()


#correlation matrix
CorrData=mlbsalary1[,c("G","AB","R","H","HR","RBI","SB","BB","SO","OPS","salary")]
cor(CorrData, method = c("pearson"),  use = "complete.obs")

library(outliers)
outlier_values <- boxplot.stats(mlbsalary1$salary)$out  # outlier values.
boxplot(mlbsalary1$salary, main="Salary", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier_values, collapse=", ")), cex=0.6)

#cap the outliers
x=OPS
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]


#  for(i in (sapply(mlbsalary1,is.numeric))){
#    quantiles <- quantile(x[,i],c(0.05,0.95),na.rm=TRUE)
#    x[,i]=ifelse(x[,i]<quantiles[1],quantiles[1],x[,i])
#    x[,i]=ifelse(x[,i]>quantiles[2],quantiles[2],x[,i])}}
       
#modelling
require(caTools)
set.seed(101) 
samplesal = sample.split(mlbsalary1$salary, SplitRatio = .75)
trainsal = subset(mlbsalary1, samplesal == TRUE)
testsal  = subset(mlbsalary1, samplesal == FALSE)

#linear regression using training data
fitsal <- lm(salary~R+H+HR+RBI+BB+SO,data = trainsal)
summary(fitsal)

