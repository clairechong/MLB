#setwd("C:/Users/jpano/Documents/DATA ANALYTICS COURSE/CSDA1010/My R Stuff")
library(readr)
library(ggplot2)
library(dplyr)

ks=read_csv("ks-projects-201801.csv", col_name=TRUE,na=c("","NA","#NA"))

summary(ks)
# write to CSV to see full data set in Excel
#write.csv(appear_bat_field,file='MLBMergedData.csv')

# read it back in for Data Prep
#baseball=read_csv("MLBMergedData.csv", col_name=TRUE,na=c("","NA","#NA"))

#see what it looks like
head(ks) 

#Data prep

summary(ks$launched)

#calculate # of days between date launched and deadline
ks$date_diff = as.Date(as.character(ks$deadline), format="%Y-%m-%d")-
  as.Date(as.character(ks$launched), format="%Y-%m-%d")

summary(ks$date_diff)
head(ks$date_diff)
quantile(ks$date_diff)

#the year projects were launched
ks$launch_year = year(as.Date(ks$launched, format="%Y-%m-%d"))

summary(ks$launch_year)
head(ks$launch_year)
quantile(ks$launch_year)

freq_tbl4 = table(ks$launch_year)
prop.table(freq_tbl4)
barplot(prop.table(freq_tbl4), cex.names = 1)



# identify missing values
missv=function(x){sum(is.na(x))/length(x)*100}
apply(ks,2,missv)

ks$fund_ratio = ks$usd_pledged_real/ks$usd_goal_real
summary(ks$fund_ratio)
boxplot(log10(ks$fund_ratio))
hist(ks$fund_ratio)

ks$log_fund_ratio = log(ks$fund_ratio)
ks$log_backers = log(ks$backers)

#remove NAs in log_fund_ratio and log_backers

is.infinite.data.frame <- function(x)
  do.call(cbind, lapply(x, is.infinite))

ks[is.infinite(ks)] <- NA

ks1 = na.omit(ks)

ncol(ks1)
nrow(ks1)


#write to CSV file for visualizations/modelling
write.csv(ks1,file="ks_fund_ratio.csv")

#
ncol(ks)
nrow(ks)

freq_tbl = table(ks$main_category)
prop.table(freq_tbl)
barplot(prop.table(freq_tbl), cex.names = 0.5)

freq_tbl2 = table(ks$state)
prop.table(freq_tbl2)
barplot(prop.table(freq_tbl2), cex.names = 1)

freq_tbl3 = table(ks$country)
prop.table(freq_tbl3)
barplot(prop.table(freq_tbl3), cex.names = 0.5)







#more visualizations
hist(log(ks$fund_ratio))
hist(log(ks$backers))
boxplot(log(ks$backers))
summary(ks$backers)

# Get only numeric colums
list_of_numcols = sapply(ks, is.numeric)
#list_of_numcols1 = sapply(AppearBatSub_Clean, is.numeric)
numcols = ks[ , list_of_numcols]
#numcols1 = AppearBatSub_Clean[ , list_of_numcols1]
#Melt data --> Turn it into skinny LONG table - good to aggregate

install.packages("reshape2")
library(reshape2)
melt_data = melt(numcols, id.vars=c("ID")) #keep ID
#This data structure is now suitable for a multiplot function
ggplot(data = melt_data, mapping = aes(x = value, log = 'x')) + geom_histogram(bins = 5) + facet_wrap(~variable, scales = 'free_x')


#binning
library(Hmisc)
Newdata = cbind(ks, TARGET_CLASS=ifelse(ks$state=="successful", "successful", "failed"))
head(Newdata)

#Homeruns
#factor_HR = cut2(Newdata$HR, g=10, minmax=TRUE, oneval=TRUE)
#table(factor_HR)
#HR_binned=cbind(Newdata,factor_HR)
#head(HR_binned)

ggplot(Newdata, aes(ks$main_category, ..count..)) + geom_bar(aes(fill = TARGET_CLASS), position = "stack")

ggplot(Newdata, aes(ks$launch_year, ..count..)) + geom_bar(aes(fill = TARGET_CLASS), position = "stack") + xlim(2009, 2018)



#correlation matirx
#CorrData= ks[,c("main_category","state","backers","date_diff","launch_year")]
#cor(CorrData, method = c("pearson"),  use = "complete.obs")
#cor_result=rcorr(as.matrix(CorrData))
#cor_result$r #correlation coefficient
#cor_result$P #p-values

library(lattice)
#ggplot(field_sub, aes(x = field_sub$POS, y = field_sub$BA), na.rm = TRUE) + geom_bar(stat = "identity")
#pairs(field_sub[,7:14], main = "Fielding Data")

#transform to numeric

#transform(ks1, date_diff = as.numeric(as.character(date_diff)))
str(ks1$launch_year)
#transform(ks1, launch_year = as.numeric(launch_year))

#subset of ks
#ks_sub = subset(ks1, select = c("log_backers","launch_year", "log_fund_ratio", "date_diff"))
#correlation of attributes
#cor(ks_sub)

summary(ks$date_diff)

#modelling

require(caTools)
set.seed(101) 
sample = sample.split(ks1$log_fund_ratio, SplitRatio = .75)
train = subset(ks1, sample == TRUE)
test  = subset(ks1, sample == FALSE)



##############################################################################
###########Emma##################################################
###########################################################


#linear regression
library(rpart)


#on training set


#str(ks$main_category)
ks$main_category <- factor(ks$main_category)
#levels(ks$main_category)
#contrasts(ks$main_category)

#str(ks$state)
ks$state <- factor(ks$state)
#contrasts(ks$state)


fit <- lm(train$log_fund_ratio~train$log_backers,data = train)

summary(fit)

#adding attribute date_diff

fit <- lm(train$log_fund_ratio~train$log_backers+train$date_diff,data = train)

summary(fit)

#adding attribute main_category

fit <- lm(train$log_fund_ratio~train$log_backers+train$main_category,data = train)

summary(fit)

#adding state to model
a = train$log_fund_ratio
b = train$log_backers
c = train$main_category
d = train$state

fit <- lm(log_fund_ratio~log_backers+main_category +state,data = train)

summary(fit)
coef(summary(fit))

#prediction using test


test$pred = predict(fit, test)

summary(test)

install.packages("effects")
library(effects)
plot(fit)

plot(test$pred,test$log_fund_ratio,xlab="predicted",ylab="actual")
abline(a=0,b=1)


################Modified by Emma#################################################
