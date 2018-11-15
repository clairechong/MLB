setwd("C:/Users/jpano/Documents/DATA ANALYTICS COURSE/CSDA1010/My R Stuff")

library(readr)
library(ggplot2)
library(dplyr)
library(reshape2)

#read file
teams=read_csv("Teams.csv",col_name=TRUE,na=c("","NA","#NA"))

#explore data
head(teams)
str(teams)
summary(teams)

#rows & columns
dim(teams)

#data types
sapply(teams, class)

#convert datatypes from chr to int
cols = c("SF","HBP")    
teams[,cols] = apply(teams[,cols], 2, function(x) as.integer(as.character(x)))

#filter for 1960+
teamdata=filter(teams,yearID >=1960)

#check for missing values
colSums(is.na(teamdata)) 

#no divisions in earlier years, remove divID, DivWin, WCWin, LgWin, WSWin since not being used as dep/indep vars and they're categorical vars
#HBP, SF blanks means they weren't tracked then - remove
teamdata$divID=teamdata$DivWin=teamdata$WCWin=teamdata$LgWin=teamdata$WSWin=teamdata$HBP=teamdata$SF=NULL

#franchID + last 3 columns not needed as they are names of the team which we already have
teamdata$franchID=teamdata$teamIDBR=teamdata$teamIDlahman45=teamdata$teamIDretro=NULL

#checked NAs gone after removal of columns
colSums(is.na(teamdata)) 

#reduced # columns from 48 to 37, removal of 11 columns
dim (teamdata)

head(teamdata)
summary(teamdata)

#visualize
#plot avg # wins by year
winsgb=aggregate(teamdata$W, by=list(teamdata$yearID), FUN=mean)
ggplot(winsgb,aes(winsgb[,1],winsgb[,2]),na.rm=TRUE)+geom_point()+geom_line()+
  labs(x="Year", y="Mean of Wins", title=paste("Avg Wins by Year"))
winsgb 
#shows 1981 and 1994-95 were the years with abnormal # wins when there was a strike
#1981 = 713 games, 38% of the scheduled games were cancelled
#1994 to 1995 = 948 games were cancelled in rest of 1994 season and into 1995 season

#plot avg # wins by team
winsgb_team=aggregate(teamdata$W, by=list(teamdata$teamID), FUN=mean)
ggplot(winsgb_team,aes(winsgb_team[,1],winsgb_team[,2]),na.rm=TRUE)+geom_col()+
  labs(x="Team", y="Mean of Wins", title=paste("Avg Wins by Team"))

#plot attendance by year
attendgb_year=aggregate(teamdata$attendance, by=list(teamdata$yearID), FUN=mean)
ggplot(attendgb_year,aes(attendgb_year[,1],attendgb_year[,2]),na.rm=TRUE)+geom_col()+
  labs(x="Year", y="Mean of Attendance", title=paste("Avg Attendance by Year"))

#plot attendance by team
attendgb_team=aggregate(teamdata$attendance, by=list(teamdata$teamID), FUN=mean)
ggplot(attendgb_team,aes(attendgb_team[,1],attendgb_team[,2]),na.rm=TRUE)+geom_col()+
  labs(x="Team", y="Mean of Attendance", title=paste("Avg Attendance by Team"))

#remove the strike years so wins are comparable
teamdata1=filter(teamdata, yearID != 1981)
teamdata2=filter(teamdata1, yearID != 1994)
teamdata3=filter(teamdata2, yearID != 1995)

#check the 3 years were removed
unique(teamdata3$yearID)

# Get only numeric colums
list_of_numcols = sapply(teamdata3, is.numeric)
numcols = teamdata3[ , list_of_numcols]
melt_data = melt(numcols, id.vars=c("yearID")) 

#This data structure is now suitable for a multiplot function
ggplot(data = melt_data, mapping = aes(x = value)) + geom_histogram(bins = 10) + facet_wrap(~variable, scales = 'free_x')

#team proportion
percentage <- prop.table(table(teamdata3$name)) * 100
cbind(freq=table(teamdata$name), percentage=percentage)

#park proportion
percentage1 <- prop.table(table(teamdata3$park)) * 100
cbind(freq=table(teamdata$park), percentage=percentage1)

#league proportion
percentage2 <- prop.table(table(teamdata3$lgID)) * 100
cbind(freq=table(teamdata$lgID), percentage=percentage2)

#box plots - view outliers
teamdata4=teamdata3[c(7,9:35)]
head(teamdata4)
dim(teamdata4)

x <- teamdata4[c(1:28)]

par(mfrow=c(2,2)) 
for(i in 1:4) {
  boxplot(x[,i], main=names(teamdata4)[i])
}
for(i in 5:8) {
  boxplot(x[,i], main=names(teamdata4)[i])
}

for(i in 9:12) {
  boxplot(x[,i], main=names(teamdata4)[i])
}
for(i in 13:16) {
  boxplot(x[,i], main=names(teamdata4)[i])
}
for(i in 17:20) {
  boxplot(x[,i], main=names(teamdata4)[i])
}
for(i in 21:24) {
  boxplot(x[,i], main=names(teamdata4)[i])
}
for(i in 25) {
  boxplot(x[,i], main=names(teamdata4)[i])
}
boxplot(teamdata4$attendance, main="attendance")

#over time
par(mfrow=c(1,1)) 
boxplot(W ~ yearID, data=teamdata3, main="Wins across Years")
boxplot(attendance ~ yearID, data=teamdata3, main="Attendance across Years") 


#correlation matrix
teamdata3_subset= teamdata3[,c("W","attendance","R","RA","ERA","SV","SOA","E","FP")]
cor(teamdata3_subset, method = c("pearson"),  use = "complete.obs")
cor_result=rcorr(as.matrix(teamdata3_subset))
cor_result$r #correlation coefficient
cor_result$P #p-values


#histograms using selected parameters correlated with W and attendance

select_parameters=teamdata3_subset[,c("W","attendance","R","SV","SOA","FP","RA")]
head(select_parameters)

#select_parameters <- gsub(",", "", select_parameters)   # remove comma

str(select_parameters[,1])

par(mfrow=c(1,1))

for(i in 1:7) {
  #select_parameters[,i] <- as.numeric(select_parameters[,i])  
  hist(select_parameters[,i], main=names(select_parameters)[i])
  i=i+1
}
#histograms show all normally distributed, so good for linear regression

#boxplot trends
boxplot(W ~ R, data=teamdata3, main="Boxplot for Runs vs Wins")
boxplot(W ~ RA, data=teamdata3, main="Boxplot for Runs allowed vs Wins")
boxplot(attendance ~ R, data=teamdata3, main="Boxplot for Runs vs Attendance")
boxplot(attendance ~ FP, data=teamdata3, main="Boxplot for Fielding Percentage vs Attendance")

#should we treat outliers?

summary(select_parameters)
quantile(select_parameters$R)
quantile(select_parameters$RA)
quantile(select_parameters$SV)
quantile(select_parameters$SOA)
quantile(select_parameters$FP)

wins_parameters=select_parameters[,c("W","R","RA","SV")]
attend_parameters=select_parameters[,c("attendance","R","SV","SOA","FP")]

install.packages("DMwR")
library(DMwR)

#Outliers on wins and related parameters
outlier.scores <- lofactor(wins_parameters, k=5)
plot(density(outlier.scores))
# pick top 5 as outliers
outliers <- order(outlier.scores, decreasing=T)[1:5]
# who are outliers
print(outliers)
n <- nrow(wins_parameters)
labels <- 1:n
labels[-outliers] <- "."
biplot(prcomp(wins_parameters), cex=.8, xlabs=labels)
pch <- rep(".", n)
pch[outliers] <- "+"
col <- rep("black", n)
col[outliers] <- "red"
pairs(wins_parameters, pch=pch, col=col)

#outliers on attendance and related parameters
outlier.scores1 <- lofactor(attend_parameters, k=5)
plot(density(outlier.scores1))
# pick top 5 as outliers
outliers1 <- order(outlier.scores1, decreasing=T)[1:5]
# who are outliers
print(outliers1)
n <- nrow(attend_parameters)
labels <- 1:n
labels[-outliers1] <- "."
biplot(prcomp(attend_parameters), cex=.8, xlabs=labels)
pch <- rep(".", n)
pch[outliers1] <- "+"
col <- rep("black", n)
col[outliers1] <- "red"
pairs(attend_parameters, pch=pch, col=col)

# Run algorithms using 10-fold cross validation
install.packages("caret")
library(caret)
control <- trainControl(method="cv", number=10)


# Linear Model on wins 

par(mfrow=c(1,3))
attach(wins_parameters)
plot(R,W)+abline(lm(W~R))
plot(RA,W)+abline(lm(W~RA))
plot(SV,W)+abline(lm(W~SV))

require(caTools)
set.seed(101) 
samplew = sample.split(wins_parameters$W, SplitRatio = .70)
trainw = subset(wins_parameters, samplew == TRUE)
testw  = subset(wins_parameters, samplew == FALSE)

#simple linear regression

fitw_cv<- train(W~., data=trainw, trControl=control, method="lm")
fitw_cv
summary(fitw_cv)


testw$pred <- predict(fitw_cv,newdata=testw,type='raw')
t.test(testw$pred)

summary(testw$pred)

  #randomGLM

fitw_glm<- train(W~., data=trainw, method="randomGLM")
fitw_glm
summary(fitw_glm)

testw$pred2 <- predict(fitw_glm,newdata=testw,type='raw')

summary(testw$pred2)

#HYFIS, not used too slow

fitw_H<- train(W~., data=trainw, method="HYFIS")
fitw_H
summary(fitw_H)

testw$pred3 <- predict(fitw_H,newdata=testw,type='raw')

summary(testw$pred3)

#Multivariate Adaptive Regression Spline, earth
install.packages("earth")
library(earth)

fitw_e<- train(W~., data=trainw, method="earth")
fitw_e
summary(fitw_e)

testw$pred4 <- predict(fitw_e,newdata=testw,type='raw')

summary(testw$pred4)

#Penalized linear regression, penalized
install.packages("penalized")
library(penalized)

fitw_p<- train(W~., data=trainw, method="penalized")
fitw_p
summary(fitw_p)

testw$pred5 <- predict(fitw_p,newdata=testw,type='raw')

summary(testw$pred5)

#table with predicted values added
head(testw)

#adjusted R squared on test
cor(cbind(testw$W,testw$pred))[1,2]^2 

#plot actual vs predicted
par(mfrow=c(1,1))
plot(testw$W,testw$pred,xlab="actual",ylab="predicted")
abline(a=0,b=1)

#check normally distributed residuals
plot(density(resid(fitw_cv)))

#qq plot
plot(qqnorm(resid(fitw_cv))) 
qqline(resid(fitw_cv))

#Linear model on attendance

par(mfrow=c(2,2))
attach(attend_parameters)
plot(R,attendance)+abline(lm(attendance~R))
plot(SV,attendance)+abline(lm(attendance~SV))
plot(SOA,attendance)+abline(lm(attendance~SOA))
plot(FP,attendance)+abline(lm(attendance~FP))

set.seed(101) 

samplea = sample.split(attend_parameters$attendance, SplitRatio = .70)
traina = subset(attend_parameters, samplea == TRUE)
testa  = subset(attend_parameters, samplea == FALSE)

fita_cv<- train(attendance~., data=traina, trControl=control, method="lm")
fita_cv
#adjusted R squared on training
summary(fita_cv)

testa$pred <- predict(fita_cv,newdata=testa,type='raw')

summary(testa$pred) #not a good fit, not linearly correlated

#table with predicted values added
head(testa)

#adjusted R squared on test
cor(cbind(testa$attendance,testa$pred))[1,2]^2 

#plot actual vs predicted
par(mfrow=c(1,1))
plot(testa$attendance,testa$pred,xlab="actual",ylab="predicted")
abline(a=0,b=1)

#check normally distributed residuals
plot(density(resid(fita_cv)))

#qq plot
plot(qqnorm(resid(fita_cv))) 
qqline(resid(fita_cv))
