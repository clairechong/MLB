setwd("C:/Users/jpano/Documents/DATA ANALYTICS COURSE/CSDA1010/My R Stuff")
library(readr)
library(dplyr)
appearances=read_csv("Appearances.csv",  col_name=TRUE,na=c("","NA","#NA"))
batting=read_csv("Batting.csv",col_name=TRUE,na=c("","NA","#NA"))
fielding=read_csv("Fielding.csv",col_name=TRUE,na=c("","NA","#NA"))
salaries=read_csv("Salaries.csv",col_name=TRUE,na=c("","NA","#NA"))
pitching=read_csv("Pitching.csv",col_name=TRUE,na=c("","NA","#NA"))

fulldata=left_join(appearances,batting,by=c("yearID","playerID","teamID"))
fulldata1=left_join(fulldata,fielding,by=c("yearID","playerID","teamID"))
fulldata2=left_join(fulldata1,pitching,by=c("yearID","playerID","teamID"))
fulldata3=left_join(fulldata2,salaries,by=c("yearID","playerID","teamID"))

write.csv(fulldata3,file='MLBBigData.csv')

