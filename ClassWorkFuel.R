install.packages("readr")
install.packages("ggplot2")
install.packages("plyr")
install.packages("dplyr")
install.packages("reshape2")
install.packages("sqldf")
install.packages("Hmisc")
install.packages("corrplot")
install.packages("mice")
install.packages("VIM")
install.packages("DMwR")
install.packages("rpart")

library(readr)
library(ggplot2)
library(plyr)
library(dplyr)
library(reshape2)
library(sqldf)
library(Hmisc)
library(corrplot)
library(PerformanceAnalytics)
library(mice)
library(VIM)
library(DMwR)
library(rpart)

setwd("C:/Users/jpano/Documents/DATA ANALYTICS COURSE/CSDA1010/My R Stuff")
fuel=read_csv("PREMIUM_FUEL.csv", col_name=TRUE,na=c("","NA","#NA"));
ncol(fuel)
nrow(fuel)
head(fuel,10)
str(fuel)

#for numerical data
summary(fuel)

#for categorical vars - frequency table
freq_tbl=table(fuel$CLIENT_GENDER)
head(freq_tbl)

#absolute #s are hard to work with, move to proportion table
prop.table(freq_tbl) #results show that the data is balanced between males & females so keep going

#relationship between feature and target variable - bivariate analysis - cross tab for 2 categorical vars
attach(fuel) #will go to what was attached
freq_xtab=xtabs(~fuel$CLIENT_GENDER+fuel$TARGET)
head(freq_xtab)
prop.table(freq_xtab) #shows males more likely to buy premium 13% vs 8% female

#Approach #2: using base R APPLY
tapply(fuel$PARTNERS_SHOPPED, fuel$CLIENT_GENDER, FUN=mean, na.rm=TRUE)
#tapply helps with aggregations, mean is the aggregation function, remove NAs or missing values, always include na.rm in the code
#learn more here: https://www.r-bloggers.com/using-apply-sapply-lapply-in-r/
# avg # loyalty partners for males and females are about the same around 3

#################### VISUALIZATIONS ###############################
freq_tbl=table(CLIENT_GENDER)

#1A. barplot with absolute counts
barplot(freq_tbl)

#1B. barplot with proportions
barplot(prop.table(freq_tbl))

#2. create mosaic plot (plot of proportions) / useful for many levels
mosaicplot(freq_tbl)

#Becomes much more useful for categorial variables with many levels
mosaicplot(table(REWARD_HIST_SEGMENT))

#3 Visualize
#ggplot(data, aes(factor(CLIENT_GENDER), mean_PARTNERS_SHOPPED)) + geom_col()

#to learn more go here: http://www.unige.ch/ses/sococ/cl/r/categvar.e.html


########################################################################################
####################### DATA EXPLORATION: NUMERIC VARIABLES #####################
########################################################################################

################################ INDEPENDENT EXPLORATION ###############################
#1. Simple summary(data) is very useful for numeric
summary(fuel)

#2 Hisograms are you friend
hist(fuel$NUM_FMLY_MEMBERS) #distribution is usual, low and then high and then reduce

#3 What if we want to get histograms for all numeric variables?
#This is where DPLYR comes in very handy...
library(reshape2)
library(ggplot2)

#Step 1: Get only numeric colums
list_of_numcols = sapply(fuel, is.numeric)
numcols = fuel[ , list_of_numcols] #see that there are only 4 categorical vars

#Step 2: Melt data --> Turn it into skinny LONG table - good to aggregate
melt_data = melt(numcols, id.vars=c("ID")) #keep ID
head(melt_data, 10)
tail(melt_data,10)

#Step 3: This data structure is now suitable for a multiplot function
ggplot(data = melt_data, mapping = aes(x = value)) + geom_histogram(bins = 10) + facet_wrap(~variable, scales = 'free_x')
#note: need scales = 'free_x' given features are not all on same scale

###################### RELATIONSHIP WITH TARGET VARIABLE #############################
#What if I tried to just plot it as-is. Two numeric variables, use a regular X-Y scatter plot
ggplot(fuel, aes(TARGET,AVG_WKLY_SPND_ALL_PARTNERS), na.rm = TRUE) + geom_point()

#Approach to explore numeric variables vs binary target
#1. Bin numeric variables (ex: into 10 groups, this is called deciling) to create a factor variable
#2. And then:
#Create table that shows avg value of target by bin
#Or visualize on a graph

#1. We'll use cut2 function from Hmisc for binning
library(Hmisc)

#Before we start, create a categorical version of target variable as well instead of showing 0 and 1 all the time
fuel = cbind(fuel, TARGET_CLASS=ifelse(fuel$TARGET==1, "PREMIUM", "REGULAR"))
head(fuel)

#Binning - create factor
factor_AVG_WKLY_SPND_ALL_PARTNERS = cut2(fuel$AVG_WKLY_SPND_ALL_PARTNERS, g=10, minmax=TRUE, oneval=TRUE)
data_binned=cbind(fuel,factor_AVG_WKLY_SPND_ALL_PARTNERS)
head(data_binned)

#Create table that shows avg value of target by bin (in this case avg wkly sped)
sqldf('select factor_AVG_WKLY_SPND_ALL_PARTNERS, avg("TARGET") from data_binned group by factor_AVG_WKLY_SPND_ALL_PARTNERS')

#Or visualize on a graph
#Create categorical version of label as well (helps with filling)
data = cbind(fuel, TARGET_CLASS=ifelse(fuel$TARGET==1, "PREMIUM", "REGULAR"))
head(fuel)
ggplot(data_binned, aes(factor_AVG_WKLY_SPND_ALL_PARTNERS, ..count..)) + geom_bar(aes(fill = TARGET_CLASS), position = "stack")
#plot shows people who spend at the loyalty partners are more likely to spend on premium gas

#Between all numeric variables...
numeric_cols = sapply(fuel, is.numeric)
fuel_num_only=fuel[,numeric_cols]

#But I'll DO IT ONLY ON A SUBSET
fuel_num_subset = fuel[,c("NUM_CARS_HH", "NUM_FMLY_MEMBERS", "PROP_WKND", "PROP_WKDAY_DAY","FUEL_TXNS_L12", "AVG_FUEL_VOL_L12")]
head(fuel_num_subset)
