library(ggplot2)
library(dplyr)
library(readr)
library(lubridate)
setwd("C:/Users/jpano/Documents/DATA ANALYTICS COURSE/CSDA1010/My R Stuff")
weather=read_csv("seattleWeather_1948-2017.csv", col_name=TRUE, na=c("","NA","#NA"))

nrow(weather)
ncol(weather)
class(weather)
str(weather)
summary(weather)
summary(weather$RAIN)

# no. and proportion of rainy days and non-rainy days
freq_tbl=table(weather$RAIN)
head(freq_tbl)
prop.table(freq_tbl)
barplot(freq_tbl)



weather=na.omit(weather)
nrow(weather) #to check that NAs were removed

weather$DATE=as.Date(weather$DATE)
weather$MONTH=month(weather$DATE)
weather$YEAR=year(weather$DATE)
head(weather) #to check that new columns added

#convert temperatures and precipitation and drop the old columns
weather$TMAX_CELSIUS=((weather$TMAX-32)*5/9)
weather$TMIN_CELSIUS=((weather$TMIN-32)*5/9)
weather$PRCP_MM=(weather$PRCP/0.039370078740157)
weather$PRCP=NULL                 
weather$TMAX=NULL
weather$TMIN=NULL

#Reorder the columns
weather=weather[c("DATE","YEAR","MONTH","TMAX_CELSIUS","TMIN_CELSIUS","PRCP_MM","RAIN")]
count(weather,YEAR)
weather1 <- filter(weather, YEAR != 2017)
tail(weather1) #check that 2017 was removed
summary(weather1)

tapply(weather1$TMAX_CELSIUS, weather1$RAIN, FUN=mean, na.rm=TRUE)
tapply(weather1$TMIN_CELSIUS, weather1$RAIN, FUN=mean, na.rm=TRUE)


#PLOTS

pairs(weather1[,4:6], main = "Seattle Weather Data -- Max Temp, Min Temp, Precipitation")

#histograms and related counts
#TMAX_CELSIUS
ggplot(data = weather1) +
  geom_histogram(mapping = aes(x = TMAX_CELSIUS), binwidth = 1)
weather1 %>%
  count(cut_width(TMAX_CELSIUS, 1))
#TMIN_CELSIUS
ggplot(data = weather1) +
  geom_histogram(mapping = aes(x = TMIN_CELSIUS), binwidth = 1)
weather1 %>%
  count(cut_width(TMIN_CELSIUS, 1))
#PRCP_MM
ggplot(data = weather1) +
  geom_histogram(mapping = aes(x = PRCP_MM), binwidth = 1)
weather1 %>%
  count(cut_width(PRCP_MM, 1))
#RAIN or NO RAIN
ggplot(data = weather1) +
  geom_bar(mapping = aes(x = RAIN))+ggtitle("No of Days of Rain/No Rain, 1948-2016")

#binning the numeric variables
library(Hmisc)
weather_bins = cbind(weather1, TARGET_CLASS=ifelse(weather1$RAIN==1, "RAINY", "DRY"))
head(weather_bins)

factor_TMAX_CELSIUS = cut2(weather_bins$TMAX_CELSIUS, g=10, minmax=TRUE, oneval=TRUE)
TMAX_binned=cbind(weather_bins,factor_TMAX_CELSIUS)
head(TMAX_binned)

ggplot(TMAX_binned, aes(factor_TMAX_CELSIUS, ..count..)) + geom_bar(aes(fill = TARGET_CLASS), position = "stack")

factor_TMIN_CELSIUS = cut2(weather_bins$TMIN_CELSIUS, g=10, minmax=TRUE, oneval=TRUE)
TMIN_binned=cbind(weather_bins,factor_TMIN_CELSIUS)
head(TMIN_binned)

ggplot(TMIN_binned, aes(factor_TMIN_CELSIUS, ..count..)) + geom_bar(aes(fill = TARGET_CLASS), position = "stack")

#correlation matirx
weather1_subset= weather1[,c("TMAX_CELSIUS","TMIN_CELSIUS")]
cor(weather1_subset, method = c("pearson"),  use = "complete.obs")
cor_result=rcorr(as.matrix(weather1_subset))
cor_result$r #correlation coefficient
cor_result$P #p-values

boxplot(weather1$TMAX_CELSIUS, las = 1, ylim=c(-20, 45))
boxplot(weather1$TMIN_CELSIUS, las = 1, ylim=c(-20, 30))

#MODELLING

#drop features not needed for modelling (remaining: month, tmax, tmin, prcp, RAIN)
weather1$DATE=NULL
weather1$YEAR=NULL

#split data into train and test 70-30% split
train <- weather1[1:17640,]
test <- weather1[17641:25200,]

#LOGISTIC REGRESSION
rainmodel <- glm(RAIN~TMAX_CELSIUS+TMIN_CELSIUS+MONTH+PRCP_MM,family=binomial(link='logit'),data=train)
summary(rainmodel)
anova(rainmodel,test="Chisq")

fitted.results <- predict(rainmodel,newdata=subset(test),type='response') 
fitted.results <- ifelse(fitted.results > 0.5,1,0)

misClasificError <- mean(fitted.results != test$RAIN) 
print(paste('Accuracy',1-misClasificError)) 


#drop PRCP as well and then model again
weather1$PRCP_MM=NULL

#split data into train and test 70-30% split
train2 <- weather1[1:17640,]
test2 <- weather1[17641:25200,]

(rainmodel2 <- glm(RAIN ~.,family=binomial(link='logit'),data=train2))

summary(rainmodel2)

(anova(rainmodel2,test="Chisq"))

fitted.results2 <- predict(rainmodel2,newdata=subset(test2),type='response') 
fitted.results2 <- ifelse(fitted.results2 > 0.5,1,0) 
 
misClasificError2 <- mean(fitted.results2 != test2$RAIN) 
print(paste('Accuracy',1-misClasificError2)) 
conf_matrix_log<-table(fitted.results2,test2$RAIN)
conf_matrix_log

#decision tree

library(rpart)
(fit <- rpart(RAIN ~.,data=train2,method="class"))
printcp(fit)  # print the cptable

library(rpart.plot)
library(rattle)
fancyRpartPlot(fit)
xpred.rpart(fit)

#determine confusion matrix and accuracy score
#on training data
tree_predict=predict(fit,train2, type="class")
(conf_matrix=table(tree_predict,train2$RAIN))

accuracy<-(conf_matrix[1,1]+conf_matrix[2,2])/(sum(conf_matrix))
accuracy

#validate on test data
validpred=predict(fit,test2,type="class")
conf_matrix_val<-table(validpred,test2$RAIN)
conf_matrix_val
accuracy_val<-(conf_matrix_val[1,1]+conf_matrix_val[2,2])/(sum(conf_matrix_val))
print(paste('Accuracy',accuracy_val))
accuracy_val
