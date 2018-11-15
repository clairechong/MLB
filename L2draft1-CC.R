#loading packages
install.packages("formattable")

#loading library
library(readr)
library(ggplot2)
library(plyr)
library(dplyr)
library(mice)
library(lubridate)
library(formattable)

#loading data

ks=read_csv("ks-projects-201801.csv", col_name=TRUE,na=c("","NA","#NA"))
head(ks)
attach(KS)
str(KS)
summary(KS)
colnames(KS)

#looking for na values
ks.na <-- sapply(KS, function(x) sum(is.na(x)))

#convert launched to date
ks$launched=as.Date(ks$launched)

#add variable for the year projects were launched
ks$launch_year = year(as.Date(ks$launched, format="%Y-%m-%d"))
head(ks$launch_year)

#frequency tables
freq_tbl = table(ks$main_category)
prop.table(freq_tbl)
barplot(prop.table(freq_tbl), cex.names = 0.5)

freq_tbl2 = table(ks$launch_year)
prop.table(freq_tbl2)
barplot(prop.table(freq_tbl2), cex.names = 1)

hist(ks$backers)
hist(log(ks$backers))
summary(log(ks$backers))
boxplot(log(ks$backers))

#looking into the different KS project states number
#state.freq <- KS %>%
#  group_by(state) %>%
#  summarize(count=n()) %>%
#  arrange(desc(count))
#state.freq$state <- factor(state.freq$state, levels=state.freq$state)

#ggplot(state.freq, aes(state, count, fill=count)) + geom_bar(stat="identity") + 
#  ggtitle("Distribution of Project Status") + xlab("Project Status") + ylab("Frequency") + 
#  geom_text(aes(label=count), vjust=0) + theme_classic() + 
#  theme(plot.title=element_text(hjust=.5), axis.title=element_text(size=8, face="bold"), 
#        axis.text.x=element_text(size=8), legend.position="null") + 
#  scale_fill_gradient(low="darkorange", high="darkorange4")

#looking into the different KS project states by percentage
#state.pct <- KS %>%
#  group_by(state) %>%
#  summarize(count=n()) %>%
#  mutate(pct=count/sum(count)) %>%
#  arrange(desc(pct))
#state.pct$state <- factor(state.pct$state, levels=state.pct$state)
#state.pct


#ggplot(state.pct, aes(state, pct, fill=count)) + geom_bar(stat="identity") + 
#  ggtitle("Distribution of Project Status") + xlab("Project Status") + ylab("Percentage") + 
#  geom_text(aes(label=percent(pct)), vjust=0) + theme_classic() + 
#  theme(plot.title=element_text(hjust=10), axis.title=element_text(size=8, face="bold"), 
#        axis.text.x=element_text(size=8), legend.position="null") + 
#  scale_fill_gradient(low="darkorange", high="darkorange4")

#histogram on category
hist(ks$goal)
hist(log(ks$goal))
boxplot(ks$goal ~ ks$main_category)
boxplot(pledged ~ main_category)
plot(log(pledged) ~ log(goal))

#removing usd pledged
ks1=ks
ks1$`usd pledged`=NULL
colnames(ks1)
nrow(ks1)

#filtering out incompleted states
target <- c("successful", "failed")
ks2 <- filter(ks1, state %in% target)
nrow(ks2)
boxplot(ks2$goal~ks2$state)

#date diff
ks2$date_diff = ks2$deadline-ks2$launched
head(ks2$date_diff)
summary(ks2$date_diff)
head(ks2$date_diff)
quantile(ks2$date_diff)

ks3=ks2
#adding percentage funded column
ks3$pctfunded=ks3$usd_pledged_real/ks3$usd_goal_real
colnames(ks3)
head(ks3$pctfunded)

#plot pct_funded
boxplot(ks3$pctfunded)
boxplot(log(ks3$pctfunded))
boxplot(log10(ks3$pctfunded))
summary(ks3$pctfunded)
hist(log(ks3$pctfunded))
plot()

#create log variables
ks3$logpctfunded = log(ks3$pctfunded)
ks3$logbackers = log(ks3$backers)

#increased to 19 variables
colnames(ks3)


#checking for na attributes in the new ks3 data
ks3.na <-- sapply(ks3, function(x) sum(is.na(x)))
ks3.na

#select the three where projects where neame is NA
ks3.na <- ks3[is.na(ks3$name),]
ks3.na

write.csv(ks3,file='ks3_19vars.csv')

#remove inf in log variables by making them NA first and then removing NA
is.infinite.data.frame <- function(x)
  do.call(cbind, lapply(x, is.infinite))
ks3[is.infinite(ks3)] <- NA
ks4 = na.omit(ks3)

ncol(ks4)
nrow(ks3)
nrow(ks4)


#visualize cleaned data
freq_tbl4 = table(ks4$launch_year)
prop.table(freq_tbl4)
barplot(prop.table(freq_tbl4), cex.names = 1)

library(Hmisc)
Newdata = cbind(ks4, ks4$state)
head(Newdata)

ggplot(Newdata, aes(ks4$main_category, ..count..)) + geom_bar(aes(fill = ks4$state), position = "stack")

ggplot(Newdata, aes(ks4$launch_year, ..count..)) + geom_bar(aes(fill = ks4$state), position = "stack") + xlim(2009, 2018)


ggplot(ks4, aes(logbackers,logpctfunded),na.rm=TRUE)+geom_point()
ggplot(ks4, aes(backers,pctfunded),na.rm=TRUE)+geom_point()

#correlation matirx
ks4$date_diff=as.numeric(ks4$date_diff)
CorrData= ks4[,c("backers","launch_year","usd_pledged_real","usd_goal_real","date_diff","logbackers","pctfunded","logpctfunded")]
cor(CorrData, method = c("pearson"),  use = "complete.obs")
cor_result=rcorr(as.matrix(CorrData))
cor_result$r #correlation coefficient
#cor_result$P #p-values
#visualize cleaned data from youtube
#ks3 %>%
#  ggplot(aes(x))

#visualize cleaned data from kernal
#cat.freq <- ksdata %>%
#  group_by(main_category) %>%
#  summarize(count=n()) %>%
#  arrange(desc(count))

#cat.freq$main_category <- factor(cat.freq$main_category, levels=cat.freq$main_category)

#ggplot(cat.freq, aes(main_category, count, fill=count)) + geom_bar(stat="identity") + 
#  ggtitle("Projects by Category") + xlab("Project Category") + ylab("Frequency") + 
#  geom_text(aes(label=count), vjust=-0.5) + theme_economist() + 
#  theme(plot.title=element_text(hjust=0.5), axis.title=element_text(size=12, face="bold"), 
#        axis.text.x=element_text(size=12, angle=90), legend.position="null") + 
#  scale_fill_gradient(low="skyblue1", high="royalblue4")

#visualization pct vs goal
#ks3 %>%
#  ggplot(aes(x=log(ks3$goal), y=pctfunded, col=main_category))+
#  geom_point(alpha=0.3)+
#  geom_smooth(method = lm)+
#  facet_wrap(~currency)

#modelling
require(caTools)
set.seed(101) 
sample = sample.split(ks4$logpctfunded, SplitRatio = .75)
train = subset(ks4, sample == TRUE)
test  = subset(ks4, sample == FALSE)

#linear regression
library(rpart)


#attribute logbackers using training data
fit <- lm(train$logpctfunded~train$logbackers,data = train)
summary(fit)

#adding attribute state and main_category
ks4$state <- factor(ks4$state)
ks4$main_category <- factor(ks4$main_category)
fit2 <- lm(logpctfunded~logbackers+main_category+state,data = train)
summary(fit2)


#prediction using test
test$pred <- predict(fit2,newdata=subset(test),type='response')

summary(test$pred)

cor(cbind(test$logpctfunded,test$pred))[1,2]^2 

install.packages("effects")
library(effects)
plot(fit2)

plot(test$logpctfunded,test$pred,xlab="actual",ylab="predicted")
abline(a=0,b=1)


