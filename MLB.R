setwd("C:/Users/jpano/Documents/DATA ANALYTICS COURSE/CSDA1010/My R Stuff")
library(readr)
library(ggplot2)
library(dplyr)

appearances=read_csv("Appearances.csv", col_name=TRUE,na=c("","NA","#NA"))
batting=read_csv("Batting.csv",col_name=TRUE,na=c("","NA","#NA"))
fielding=read_csv("Fielding.csv",col_name=TRUE,na=c("","NA","#NA"))
appear_bat=merge(appearances,batting,by=c("yearID","playerID","teamID"))
appear_bat_field=merge(appear_bat,fielding,by=c("yearID","playerID","teamID"))

# write to CSV to see full data set in Excel
write.csv(appear_bat_field,file='MLBMergedData.csv')

# read it back in for Data Prep
baseball=read_csv("MLBMergedData.csv", col_name=TRUE,na=c("","NA","#NA"))
head(baseball) #see what it looks like

#Data Prep


#get rid of stats only applicable to catchers which is a very small subset of players (lots of NAs)
#get rid of first column with row #
baseball$X1=baseball$PB=baseball$WP=baseball$SB.y=baseball$CS.y=baseball$ZR=NULL

#identify any other duplicate columns:
baseball[duplicated(as.list(baseball))]# shows that lgID.y and lgID are duplicate so remove
baseball$lgID=baseball$lgID.y=NULL

head(baseball) #checked columns removed
summary(baseball)

#remove rows prior to 1960 after post-war era
names(baseball)
baseball_sub <- subset(baseball, yearID >=1960)
head(baseball_sub)

# create a new dataset with a new column with batting average
baseball_sub$BA=baseball_sub$H/baseball_sub$AB

#check that new column added
head(baseball_sub$BA)

# identify missing values
missv=function(x){sum(is.na(x))/length(x)*100}
apply(baseball_sub,2,missv)

#impute zeros where BA is NA
baseball_sub$BA[is.na(baseball_sub$BA)] <- 0
summary(baseball_sub$BA) #checked no more NAs in BA

#convert datatypes from chr to int
cols = c(6,19,20,21,35,36,37,38,39,43,44)    
baseball_sub[,cols] = apply(baseball_sub[,cols], 2, function(x) as.integer(as.character(x)))

#write to CSV file for visualizations/modelling
write.csv(baseball_sub,file="MLBMergedData_1960.csv")


#extract appearances and batting separately
appear_bat_sub <- baseball_sub[,c(1:39,49)]
field_sub <- baseball_sub[,c(1:4,40:49)]
names(appear_bat_sub)
names(field_sub)
# remove duplicates (or pull out distinct rows)
AppearBatSub_Clean <- appear_bat_sub %>% distinct
write.csv(AppearBatSub_Clean,file="AppearBatLessDup.csv")

#simple stats
summary(AppearBatSub_Clean)
summary(field_sub)

#explore categorical variable
freq_tbl=table(field_sub$POS)
prop.table(freq_tbl)
freq_tbl1=table(field_sub$teamID)
prop.table(freq_tbl1)
freq_tbl2=table(field_sub$lgID.x)
prop.table(freq_tbl2)

barplot(freq_tbl)
barplot(freq_tbl1)
barplot(freq_tbl2)
mosaicplot(freq_tbl)
mosaicplot(freq_tbl1)
mosaicplot(freq_tbl2)

#more visualizations
hist(field_sub$E)
hist(field_sub$BA)
hist(AppearBatSub_Clean$GS.x)

# Get only numeric colums
list_of_numcols = sapply(field_sub, is.numeric)
list_of_numcols1 = sapply(AppearBatSub_Clean, is.numeric)
numcols = field_sub[ , list_of_numcols]
numcols1 = AppearBatSub_Clean[ , list_of_numcols1]
#Melt data --> Turn it into skinny LONG table - good to aggregate

install.packages("reshape2")
library(reshape2)
melt_data = melt(numcols, id.vars=c("yearID")) #keep ID
#This data structure is now suitable for a multiplot function
ggplot(data = melt_data, mapping = aes(x = value)) + geom_histogram(bins = 10) + facet_wrap(~variable, scales = 'free_x')

melt_data1 = melt(numcols1, id.vars=c("yearID")) 
#This data structure is now suitable for a multiplot function
ggplot(data = melt_data1, mapping = aes(x = value)) + geom_histogram(bins = 10) + facet_wrap(~variable, scales = 'free_x')


#binning
library(Hmisc)
Newdata = cbind(AppearBatSub_Clean, TARGET_CLASS=ifelse(AppearBatSub_Clean$BA>=.280, "Good", "OK"))
head(Newdata)
summary(AppearBatSub_Clean$BA)

#Homeruns
factor_HR = cut2(Newdata$HR, g=10, minmax=TRUE, oneval=TRUE)
HR_binned=cbind(Newdata,factor_HR)
head(HR_binned)

ggplot(HR_binned, aes(factor_HR, ..count..)) + geom_bar(aes(fill = TARGET_CLASS), position = "stack")

#Runs
factor_R = cut2(Newdata$R, g=10, minmax=TRUE, oneval=TRUE)
R_binned=cbind(Newdata,factor_R)
head(R_binned)

ggplot(R_binned, aes(factor_R, ..count..)) + geom_bar(aes(fill = TARGET_CLASS), position = "stack")

#RBIs
factor_RBI = cut2(Newdata$RBI, g=10, minmax=TRUE, oneval=TRUE)
RBI_binned=cbind(Newdata,factor_RBI)
head(RBI_binned)

ggplot(RBI_binned, aes(factor_RBI, ..count..)) + geom_bar(aes(fill = TARGET_CLASS), position = "stack")

#Walks
factor_BB = cut2(Newdata$BB, g=10, minmax=TRUE, oneval=TRUE)
BB_binned=cbind(Newdata,factor_BB)
head(BB_binned)

ggplot(BB_binned, aes(factor_BB, ..count..)) + geom_bar(aes(fill = TARGET_CLASS), position = "stack")

#correlation matirx
CorrData= AppearBatSub_Clean[,c("R","RBI","HR","BB","BA")]
cor(CorrData, method = c("pearson"),  use = "complete.obs")
cor_result=rcorr(as.matrix(CorrData))
cor_result$r #correlation coefficient
cor_result$P #p-values

ggplot(field_sub, aes(field_sub$BA,field_sub$POS), na.rm = TRUE) + geom_bar()
pairs(field_sub[,7:14], main = "Fielding Data")


# add binary variable for BA
AppearBatSub_Clean$BAClass=ifelse(AppearBatSub_Clean$BA>=.280,1,0)
head(AppearBatSub_Clean)
head(AppearBatSub_Clean$BAClass)


#modelling

#split data into train and test 70-30% split
train <- AppearBatSub_Clean[1:46090,]
test <- AppearBatSub_Clean[46091:61457,]


#decision tree
library(rpart)
attach(AppearBatSub_Clean)

#on training set
(fit <- rpart(BAClass~HR+R+RBI+BB+SB.x+CS.x+IBB,train,method="class"))
library(rpart.plot)
library(rattle)
fancyRpartPlot(fit)
xpred.rpart(fit)

#validate on test data
validpred=predict(fit,test,type="class")
conf_matrix_val<-table(validpred,test$BAClass)
conf_matrix_val
accuracy_val<-(conf_matrix_val[1,1]+conf_matrix_val[2,2])/(sum(conf_matrix_val))
print(paste('Accuracy',accuracy_val))
accuracy_val


#trying log regression
BAmodel <- glm(AppearBatSub_Clean$BAClass ~ AppearBatSub_Clean$H+AppearBatSub_Clean$HR+AppearBatSub_Clean$R+AppearBatSub_Clean$BB+AppearBatSub_Clean$HR+AppearBatSub_Clean$AB,family=binomial(link='logit'),data=train)

summary(BAmodel)
(anova(BAmodel,test="Chisq"))

fitted.results2 <- predict(BAmodel,newdata=subset(test),type='response') 
fitted.results2 <- ifelse(fitted.results2 > 0.5,1,0) 

misClasificError2 <- mean(fitted.results2 != test$BA) 
print(paste('Accuracy',1-misClasificError2)) 
conf_matrix_log<-table(fitted.results2,test$BA)
conf_matrix_log
