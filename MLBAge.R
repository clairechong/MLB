setwd("C:/Users/jpano/Documents/DATA ANALYTICS COURSE/CSDA1010/My R Stuff")
library(readr)
library(ggplot2)
library(dplyr)
library(reshape2)
#start from beginning since we may want to clean the data differently
batting=read_csv("Batting.csv",col_name=TRUE,na=c("","NA","#NA"))
master=read_csv("Master.csv", col_name=TRUE,na=c("","NA","#NA"))
playerdata=left_join(batting,master,by=c("playerID"),na=c("","NA","#NA"))
head(playerdata)
data1960=filter(playerdata,yearID >=1960)

colSums(is.na(data1960)) 

data1960$age=data1960$yearID-data1960$birthYear
head(data1960$age)
#too many columns, let's pick certain ones only

newdata=data1960[,c("playerID","nameFirst","nameLast","age","birthState","G","AB","R","H","HR","RBI","SB","CS","BB","SO","IBB","HBP","GIDP")]
head(newdata)               

par(mfrow=c(2,2))
attach(newdata)
plot(G,age)+abline(lm(age~G))
plot(AB,age)+abline(lm(age~AB))
plot(R,age)+abline(lm(age~R)) 
plot(H,age)+abline(lm(age~H))
plot(HR,age)+abline(lm(age~HR))
plot(age,RBI)+abline(lm(RBI~age)) 
