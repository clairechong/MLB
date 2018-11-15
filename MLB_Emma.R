#setwd("C:/Users/jpano/Documents/DATA ANALYTICS COURSE/CSDA1010/My R Stuff")
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

###################################################
########################Emma################################
##################################################################
#remove pitchers from data

baseball_sub2 = subset(baseball_sub, POS != "P")
nrow(baseball_sub2)
ncol(baseball_sub2)

######################Emma###########################################

# create a new dataset with a new column with batting average
baseball_sub$BA=baseball_sub$H/baseball_sub$AB

#check that new column added
head(baseball_sub$BA)

# identify missing values
missv=function(x){sum(is.na(x))/length(x)*100}
apply(baseball_sub2,2,missv)

#impute zeros where BA is NA
baseball_sub$BA[is.na(baseball_sub$BA)] <- 0
summary(baseball_sub$BA) #checked no more NAs

#convert datatypes from chr to int
cols = c(6,19,20,21,35,36,37,38,39,43,44)    
baseball_sub2[,cols] = apply(baseball_sub2[,cols], 2, function(x) as.integer(as.character(x)))

#write to CSV file for visualizations/modelling
write.csv(baseball_sub2,file="MLBMergedData_1960_lessPitcher.csv")


#extract appearances and batting separately
appear_bat_sub <- baseball_sub2[,c(1:39,49)]
field_sub <- baseball_sub2[,c(1:4,40:49)]
names(appear_bat_sub)
names(field_sub)
# remove duplicates (or pull out distinct rows)
AppearBatSub_Clean <- appear_bat_sub %>% distinct
write.csv(AppearBatSub_Clean,file="AppearBatLessDupLessPitcher.csv")

################################
#####Emma######calculate total bases TB = H +2B + (2x 3B) + 3x HR; refer to https://en.wikipedia.org/wiki/Baseball_statistics#Overall_player_value
###############################

AppearBatSub_Clean$TB = AppearBatSub_Clean$H + AppearBatSub_Clean$`2B` +2*AppearBatSub_Clean$`3B` + 3*AppearBatSub_Clean$HR
head(AppearBatSub_Clean)
head(AppearBatSub_Clean$TB)

##################
#####Emma######calculate OPS; refer to https://en.wikipedia.org/wiki/On-base_plus_slugging
#####################

AppearBatSub_Clean$OPS = (AppearBatSub_Clean$AB*(AppearBatSub_Clean$H+AppearBatSub_Clean$BB+AppearBatSub_Clean$HBP)+AppearBatSub_Clean$TB*(AppearBatSub_Clean$AB+AppearBatSub_Clean$BB+AppearBatSub_Clean$SF+AppearBatSub_Clean$HBP))/(AppearBatSub_Clean$AB*(AppearBatSub_Clean$AB+AppearBatSub_Clean$BB+AppearBatSub_Clean$SF+AppearBatSub_Clean$HBP))
head(AppearBatSub_Clean)
head(AppearBatSub_Clean$OPS)

summary(AppearBatSub_Clean$OPS)
freq_tbl3 = table(AppearBatSub_Clean$OPS)
prop.table(freq_tbl3)
barplot(prop.table(freq_tbl3))

summary(AppearBatSub_Clean$TB)
hist(AppearBatSub_Clean$TB)
hist(AppearBatSub_Clean$OPS)


# identify missing values
missv=function(x){sum(is.na(x))/length(x)*100}
apply(AppearBatSub_Clean,2,missv)


#distribution of OPS
quantile(AppearBatSub_Clean$OPS, na.rm = TRUE)
boxplot(AppearBatSub_Clean$OPS)

nrow(AppearBatSub_Clean)
ncol(AppearBatSub_Clean)

#remove missing values in OPS
AppearBatSub_Clean2 = subset(AppearBatSub_Clean, OPS > 0)
apply(AppearBatSub_Clean2,2,missv)
summary(AppearBatSub_Clean2$OPS)

nrow(AppearBatSub_Clean2)
ncol(AppearBatSub_Clean2)

#replace outliers with the median value in OPS2
install.packages("outliers")
library(outliers)
AppearBatSub_Clean2$OPS2 =  rm.outlier(AppearBatSub_Clean2$OPS, fill = TRUE, median = TRUE, opposite = FALSE)


summary(AppearBatSub_Clean2$OPS2)


boxplot(AppearBatSub_Clean2$OPS)

write.csv(AppearBatSub_Clean2,file="AppearBatSub_Clean2.csv")

# add binary variable for OPS2
AppearBatSub_Clean2$OPSClass=ifelse(AppearBatSub_Clean2$OPS2>=.833,"good","ok")
head(AppearBatSub_Clean2)
head(AppearBatSub_Clean2$OPSClass)


################Emma#######################################################################



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

#Add binary variable to determine whether player is good or OK based on batting average
#####################################################################
############################Emma############################################
##########################################################################

#binning
library(Hmisc)
Newdata = cbind(AppearBatSub_Clean2, TARGET_CLASS=ifelse(AppearBatSub_Clean2$OPS2>=.833, "Good", "OK"))
head(Newdata)

#Homeruns
factor_HR = cut2(Newdata$HR, g=10, minmax=TRUE, oneval=TRUE)
#table(factor_HR)
HR_binned=cbind(Newdata,factor_HR)
head(HR_binned)

ggplot(HR_binned, aes(factor_HR, ..count..)) + geom_bar(aes(fill = TARGET_CLASS), position = "stack")

#Runs
factor_R = cut2(Newdata$R, g=10, minmax=TRUE, oneval=TRUE)
#table(factor_R)
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

#GIDP  Grounded into Double Plays

factor_GIDP = cut2(Newdata$GIDP, g=10, minmax=TRUE, oneval=TRUE)
GIDP_binned=cbind(Newdata,factor_GIDP)
head(GIDP_binned)

ggplot(GIDP_binned, aes(factor_GIDP, ..count..)) + geom_bar(aes(fill = TARGET_CLASS), position = "stack")

#SH  Sacrifice Hits allowed

factor_SH = cut2(Newdata$SH, g=10, minmax=TRUE, oneval=TRUE)
SH_binned=cbind(Newdata,factor_SH)
head(SH_binned)

ggplot(SH_binned, aes(factor_SH, ..count..)) + geom_bar(aes(fill = TARGET_CLASS), position = "stack")

#IBB            Intentional Walks

factor_IBB = cut2(Newdata$IBB, g=10, minmax=TRUE, oneval=TRUE)
IBB_binned=cbind(Newdata,factor_IBB)
head(IBB_binned)

ggplot(IBB_binned, aes(factor_IBB, ..count..)) + geom_bar(aes(fill = TARGET_CLASS), position = "stack")

#SO             Strikeouts

factor_SO = cut2(Newdata$SO, g=10, minmax=TRUE, oneval=TRUE)
SO_binned=cbind(Newdata,factor_SO)
head(SO_binned)

ggplot(SO_binned, aes(factor_SO, ..count..)) + geom_bar(aes(fill = TARGET_CLASS), position = "stack")

#CS             Caught Stealing (by catcher)

factor_CS = cut2(Newdata$CS.x, g=10, minmax=TRUE, oneval=TRUE)
CS_binned=cbind(Newdata,factor_CS)
head(CS_binned)

ggplot(CS_binned, aes(factor_CS, ..count..)) + geom_bar(aes(fill = TARGET_CLASS), position = "stack")

#SB             Stolen Bases allowed (by catcher)

factor_SB = cut2(Newdata$SB.x, g=10, minmax=TRUE, oneval=TRUE)
SB_binned=cbind(Newdata,factor_SB)
head(SB_binned)

ggplot(SB_binned, aes(factor_SB, ..count..)) + geom_bar(aes(fill = TARGET_CLASS), position = "stack")

summary(AppearBatSub_Clean2$lgID.x)
barplot(prop.table(table(AppearBatSub_Clean2$lgID.x)))

#lgID.x

ggplot(Newdata, aes(Newdata$lgID.x, ..count..)) + geom_bar(aes(fill = TARGET_CLASS), position = "stack")

#teamID
ggplot(Newdata, aes(Newdata$teamID, ..count..)) + geom_bar(aes(fill = TARGET_CLASS), position = "stack")
#################################Emma########################################################

#correlation matirx
CorrData= AppearBatSub_Clean[,c("R","RBI","HR","BB","BA")]
cor(CorrData, method = c("pearson"),  use = "complete.obs")
cor_result=rcorr(as.matrix(CorrData))
cor_result$r #correlation coefficient
cor_result$P #p-values

library(lattice)
ggplot(field_sub, aes(x = field_sub$POS, y = field_sub$BA), na.rm = TRUE) + geom_bar(stat = "identity")
pairs(field_sub[,7:14], main = "Fielding Data")


# add binary variable for BA
AppearBatSub_Clean$BAClass=ifelse(AppearBatSub_Clean$BA>=.280,"good","ok")
head(AppearBatSub_Clean)
head(AppearBatSub_Clean$BAClass)


#modelling

#split data into train and test 70-30% split
###########################################################################################
####Emma#### random split###############################
#######################################################################################
require(caTools)
set.seed(101) 
sample = sample.split(AppearBatSub_Clean2$OPS2, SplitRatio = .75)
train = subset(AppearBatSub_Clean2, sample == TRUE)
test  = subset(AppearBatSub_Clean2, sample == FALSE)



##############################################################################
###########Emma##################################################
###########################################################


#decision tree
library(rpart)
attach(AppearBatSub_Clean2)

#on training set
(fit <- rpart(OPSClass~HR+R+RBI+BB+IBB+SH+AB+H+HBP+TB+SF,train,method="class"))
library(rpart.plot)
install.packages("rattle")
library(rattle)
fancyRpartPlot(fit)
#xpred.rpart(fit)

plotcp(fit)
printcp(fit)

#validate on test data
validpred=predict(fit,test,type="class")
conf_matrix_val<-table(validpred,test$OPSClass)
conf_matrix_val
accuracy_val<-(conf_matrix_val[1,1]+conf_matrix_val[2,2])/(sum(conf_matrix_val))
print(paste('Accuracy',accuracy_val))
accuracy_val

################Modified by Emma#################################################
