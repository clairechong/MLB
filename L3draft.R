#loading library
library(readr)
library(ggplot2)
library(plyr)
library(dplyr)

#loading data
setwd("/Users/Wilson/Documents/School/CSDA1010/SandP/L3")
bc=read_csv("L3bc.csv", col_name=TRUE,na=c("","NA","#NA"))
head(bc)
str(bc)
summary(bc)
colnames(bc)

#dropping unnecessary columns
bc2=bc
bc2$'id'=NULL
bc2$'diagnosis'=NULL
bc2$'X33'=NULL
colnames(bc2)

#export data to explore in WEKA
write.csv(bc2, file = "/Users/Wilson/Documents/School/CSDA1010/SandP/L3v4.csv", row.names = FALSE)

#scale the variables
scaled_bc2 <- scale(bc2)
head(scaled_wd)

#Hierarchical Clustering
d <- dist(scaled_bc2,method = "euclidean") #distance matrix
h_clust <- hclust(d, method = "ward") #clustering
plot(h_clust,labels = bc2$radius_mean) #dendrogram
rect.hclust(h_clust,k=2)

#initial kmeans
fitk <- kmeans(scaled_bc2, 2)
fitk

#extract clusters
groups <- cutree(h_clust,k=2)
groups

#pca
pcmp <- princomp(scaled_bc2)
pred_pc <- predict(pcmp, newdata=scaled_bc2)[,1:2]

comp_dt <- cbind(as.data.table(pred_pc),cluster = as.factor(groups), Labels = bc2$radius_mean)
ggplot(comp_dt,aes(Comp.1,Comp.2))+
  geom_point(aes(color = cluster),size=3)


#kmeans
kclust <- kmeans(scaled_bc2,centers = 2,iter.max = 100)

ggplot(comp_dt,aes(Comp.1,Comp.2))+
  geom_point(aes(color = as.factor(kclust$cluster)),size=3)

kclust

tunek <- kmeansruns(scaled_bc2,krange = 1:10,criterion = "ch")
tunek$bestk #3
tunekw <- kmeansruns(scaled_bc2,krange = 1:10,criterion = "asw")
tunekw$bestk #4

plot(bc2)
