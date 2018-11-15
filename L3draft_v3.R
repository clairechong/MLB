#loading library
library(readr)
library(ggplot2)
library(plyr)
library(dplyr)

#loading data
setwd("C:/Users/jpano/Documents/DATA ANALYTICS COURSE/CSDA1010/My R Stuff")

bc=read_csv("breastcancer.csv", col_name=TRUE,na=c("","NA","#NA"))
head(bc)
nrow(bc)
ncol(bc)
str(bc)
summary(bc)
colnames(bc)

#check if NAs exist
bc.na <- sapply(bc,function(x) sum(is.na(x)))
bc.na
colnames(bc.na)

#dropping unnecessary columns
bc2=bc
#bc2$'id'=NULL not necessary to remove, good to have ID since this identifies the patient
bc2$'X33'=NULL
colnames(bc2)
#bc2$'diagnosis'=NULL, included diagonsis in bc2 in order to compare clusters with labeled data later


#export data to explore in WEKA
#write.csv(bc2, file = "/Users/Wilson/Documents/School/CSDA1010/SandP/L3v4.csv", row.names = FALSE)

library (readr)
#importing file
#bc2 = read_csv("l3v4.csv")
summary(bc2)

# Get only numeric colums
list_of_numcols = sapply(bc2, is.numeric)
numcols = bc2[ , list_of_numcols]
library(reshape2)
melt_data = melt(numcols, id.vars=c("id"))
ggplot(data = melt_data, mapping = aes(x = value)) + geom_histogram(bins = 10) + facet_wrap(~variable, scales = 'free_x')



#there are outliers in the data

boxplot(bc2$area_mean, main = "area_mean with outliers")
boxplot(bc2$compactness_mean, main = "compactness_mean with outliers")
boxplot(bc2$radius_se, main = "radius_se with outliers")
boxplot(bc2$perimeter_se, main = "perimeter_se with outliers")
boxplot(bc2$texture_se, main = "texture_se with outliers")
boxplot(bc2$fractal_dimension_se, main = "fractal_dimension_se with outliers")
boxplot(bc2$symmetry_worst, main = "symmetry_worst with outliers")
boxplot(bc2$fractal_dimension_worst, main = "fractal_dimension_worst with outliers")


# capping the outliers, not sure we should do it.

bc2_cap=bc2

#compactness_mean
#before capping
summary(bc2$compactness_mean)
x <- bc2_cap$compactness_mean
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]
bc2_cap$compactness_mean = x
#after capping
summary(bc2_cap$compactness_mean)
boxplot(bc2_cap$compactness_mean, main = "compactness_mean with capped outliers")

#area_mean
#before capping
summary(bc2$area_mean)
x <- bc2_cap$area_mean
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]
bc2_cap$area_mean = x
#after capping
summary(bc2_cap$area_mean)
boxplot(bc2_cap$area_mean, main = "area_mean with capped outliers")

#radius_se
#before capping
summary(bc2$radius_se)
x <- bc2_cap$radius_se
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]
bc2_cap$radius_se = x
#after capping
summary(bc2_cap$radius_se)
boxplot(bc2_cap$radius_se, main = "radius_se with capped outliers")

#perimeter_se
#before capping
summary(bc2$perimeter_se)
x <- bc2_cap$perimeter_se
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]
bc2_cap$perimeter_se = x
#after capping
summary(bc2_cap$perimeter_se)
boxplot(bc2_cap$perimeter_se, main = "perimeter_se with capped outliers")

#fractal_dimension_se
#before capping
summary(bc2$fractal_dimension_se)
x <- bc2_cap$fractal_dimension_se
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]
bc2_cap$fractal_dimension_se = x
#after capping
summary(bc2_cap$fractal_dimension_se)
boxplot(bc2_cap$fractal_dimension_se, main = "fractal_dimension_se with capped outliers")

summary(bc2)
summary(bc2_cap)

#taking subset of bc2 by removing diagnosis
bc3 = subset(bc2, select = -diagnosis)
bc3_cap=subset(bc2_cap, select=-diagnosis)



#scale the variables
scaled_bc3 <- scale(bc3)

head(scaled_bc3)
summary(scaled_bc3)

scaled_bc3_cap <- scale(bc3_cap)


#Hierarchical Clustering - run this 2x - once after percentile capping the outliers, and once without percentile capping to see diffs
d <- dist(scaled_bc3,method = "euclidean") #distance matrix
h_clust <- hclust(d, method = "ward.D") #clustering
plot(h_clust) #dendrogram
rect.hclust(h_clust,k=2)

table(cutree(h_clust,2), bc2$diagnosis)

#initial kmeans
fitk <- kmeans(scaled_bc3, 2)
fitk

#hierarch clustering with capped outliers
d_cap <- dist(scaled_bc3_cap,method = "euclidean") #distance matrix
h_clust_cap <- hclust(d_cap, method = "ward.D") #clustering
plot(h_clust_cap) #dendrogram
rect.hclust(h_clust_cap,k=2)

table(cutree(h_clust_cap,2), bc2$diagnosis)

fitk_cap <- kmeans(scaled_bc3_cap, 2)
fitk_cap

#plotting kmeans leaving outliers untreated

library(cluster)
library(HSAUR)
library(fpc)
library(data.table)


# Kmeans cluster analysis-using bc3 without the diagnosis column (unscaled data)
clus <- kmeans(bc3, centers=2)

plotcluster(bc3, clus$cluster)

clusplot(bc3, clus$cluster, color=TRUE, shade=TRUE, 
          lines=0)
table(clus$cluster, bc2$diagnosis)


#kmeans using scaled bc3

dat <- scaled_bc3 # without known classification 
# Kmeans clustre analysis

clus1 <- kmeans(dat, centers=2)

plotcluster(dat, clus1$cluster)

clusplot(dat, clus1$cluster, color=TRUE, shade=TRUE, 
         lines=0)
table(clus1$cluster, bc2$diagnosis)


#try with capped outliers, unscaled
clus <- kmeans(bc3_cap, centers=2)

plotcluster(bc3_cap, clus$cluster)

clusplot(bc3, clus$cluster, color=TRUE, shade=TRUE, 
         lines=0)
table(clus$cluster, bc2$diagnosis)


#try with capped outliers, scaled

dat <- scaled_bc3_cap # without known classification 
# Kmeans clustre analysis

clus1 <- kmeans(dat, centers=2)



plotcluster(dat, clus1$cluster)

clusplot(dat, clus1$cluster, color=TRUE, shade=TRUE, 
         lines=0)
table(clus1$cluster, bc2$diagnosis)

#extract clusters
#groups <- cutree(h_clust,k=2)
#groups

#using pca
pcmp <- princomp(scaled_bc3)
pred_pc <- predict(pcmp, newdata=scaled_bc3)[,1:2]

comp_dt <- cbind(as.data.table(pred_pc),cluster = as.factor(groups), Labels = bc2$diagnosis)
ggplot(comp_dt,aes(Comp.1,Comp.2))+
  geom_point(aes(color = cluster),size=3)
table(comp_dt$cluster, bc2$diagnosis)

#kmeans
kclust <- kmeans(scaled_bc3,centers = 2,iter.max = 100)

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


