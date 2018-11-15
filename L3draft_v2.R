#loading library
library(readr)
library(ggplot2)
library(plyr)
library(dplyr)

#loading data
#setwd("/Users/Wilson/Documents/School/CSDA1010/SandP/L3")

bc=read_csv("L3bc.csv", col_name=TRUE,na=c("","NA","#NA"))
head(bc)
str(bc)
summary(bc)
colnames(bc)

#dropping unnecessary columns
bc2=bc
#bc2$'id'=NULL
#bc2$'diagnosis'=NULL, included diagonsis in bc2 in order to compare clusters with labeled data later
bc2$'X33'=NULL
colnames(bc2)

#export data to explore in WEKA
write.csv(bc2, file = "/Users/Wilson/Documents/School/CSDA1010/SandP/L3v4.csv", row.names = FALSE)

library (readr)
#importing file
#bc2 = read_csv("l3v4.csv")
summary(bc2)

#there are outliers in the data

boxplot(bc2$area_mean, main = "area_mean with outliers")
boxplot(bc2$compactness_mean, main = "compactness_mean with outliers")
boxplot(bc2$radius_se, main = "radius_se with outliers")
boxplot(bc2$perimeter_se, main = "perimeter_se with outliers")


# capping the outliers, not sure we should do it.

#compactness_mean
#x <- bc2$compactness_mean
#qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
#caps <- quantile(x, probs=c(.05, .95), na.rm = T)
#H <- 1.5 * IQR(x, na.rm = T)
#x[x < (qnt[1] - H)] <- caps[1]
#x[x > (qnt[2] + H)] <- caps[2]

#bc2$compactness_mean = x
#boxplot(x, main = "compactness_mean without outliers")

#area_mean
#x <- bc2$area_mean
#qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
#caps <- quantile(x, probs=c(.05, .95), na.rm = T)
#H <- 1.5 * IQR(x, na.rm = T)
#x[x < (qnt[1] - H)] <- caps[1]
#x[x > (qnt[2] + H)] <- caps[2]

#bc2$area_mean = x
#boxplot(x, main = "area_mean without outliers")

#taking subset of bc2 by removing diagonosis
bc3 = subset(bc2, select = -diagnosis)



#scale the variables
scaled_bc3 <- scale(bc3)
head(scaled_bc3)
summary(scaled_bc3)

#Hierarchical Clustering
d <- dist(scaled_bc3,method = "euclidean") #distance matrix
h_clust <- hclust(d, method = "ward.D") #clustering
plot(h_clust) #dendrogram
rect.hclust(h_clust,k=2)

table(cutree(h_clust,2), bc2$diagnosis)



#initial kmeans
fitk <- kmeans(scaled_bc3, 2)
fitk

#plotting kmeans

library(cluster)
library(HSAUR)
library(fpc)


dat <- bc3 # without known classification 
# Kmeans clustre analysis
clus <- kmeans(dat, centers=2)

plotcluster(dat, clus$cluster)

clusplot(dat, clus$cluster, color=TRUE, shade=TRUE, 
          lines=0)
table(clus$cluster, bc2$diagnosis)


#kmeans using scaled bc3

dat <- scaled_bc3 # without known classification 
# Kmeans clustre analysis
clus <- kmeans(dat, centers=2)

plotcluster(dat, clus$cluster)

clusplot(dat, clus$cluster, color=TRUE, shade=TRUE, 
         lines=0)
table(clus$cluster, bc2$diagnosis)

#extract clusters
groups <- cutree(h_clust,k=2)
groups

#using pca
pcmp <- princomp(scaled_bc3)
pred_pc <- predict(pcmp, newdata=scaled_bc3)[,1:2]

comp_dt <- cbind(as.data.table(pred_pc),cluster = as.factor(groups))
ggplot(comp_dt,aes(Comp.1,Comp.2))+
  geom_point(aes(color = cluster),size=3)
table(comp_dt$cluster, bc2$diagnosis)

#kmeans
kclust <- kmeans(scaled_bc3,centers = 2)

ggplot(comp_dt,aes(Comp.1,Comp.2))+
  geom_point(aes(color = as.factor(kclust$cluster)),size=3)

table(comp_dt$cluster, bc2$diagnosis)

kclust

#try agglomerative nesting here, result seems to be not so good, only 192 malignant were misdiagnosed. 
#bc2.use = subset(bc2,select=-diagnosis)

d = dist(bc3)
library(cluster)
d1 = daisy(bc3)
sum(abs(d - d1))

z = agnes(d)
plot(z)

table(cutree(z,2), bc2$diagnosis)


tunek <- kmeansruns(scaled_bc2,krange = 1:10,criterion = "ch")
tunek
tunekw <- kmeansruns(scaled_bc2,krange = 1:10,criterion = "asw")
tunekw$bestk #4

plot(tunek)
