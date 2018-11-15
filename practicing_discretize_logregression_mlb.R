library(readr)
data=read_csv("AppearBatLessDup.csv", col_name=TRUE,na=c("","NA","#NA"))
data$X1=NULL
data$BAClass=ifelse(data$BA>=.250,"good","bad")
if(data$BA<=.150){
  data$BAClass="poor"
  }else(data$BA>.150 && data$BA<=.250){
    data$BAClass="OK"
    }else(data$BA>.250){
      data$BAClass="good"
    }

head(data)

#split data into train and test 70-30% split
train <- data[1:46090,]
test <- data[46091:61457,]


#decision tree
library(rpart)
attach(data)

#on training set
library(rpart.plot)
library(rattle)
(fit <- rpart(BAClass~R,train,method="class"))
fancyRpartPlot(fit)
xpred.rpart(fit)

#validate on test data
validpred=predict(fit,test,type="class")
conf_matrix_val<-table(validpred,test$BAClass)
conf_matrix_val
accuracy_val<-(conf_matrix_val[1,1]+conf_matrix_val[2,2])/(sum(conf_matrix_val))
print(paste('Accuracy',accuracy_val))



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
