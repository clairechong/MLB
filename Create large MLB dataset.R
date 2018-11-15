setwd("C:/Users/jpano/Documents/DATA ANALYTICS COURSE/CSDA1010/My R Stuff")
appearances=read.csv("Appearances.csv", header = TRUE)
batting=read.csv("Batting.csv",header=TRUE)
fielding=read.csv("Fielding.csv",header=TRUE)
fulldata=merge(appearances,batting,by="YearPlayerTeam")
fulldata1=merge(fulldata,fielding,by="YearPlayerTeam")
write.csv(fulldata1,file='MLBfulldata.csv')

