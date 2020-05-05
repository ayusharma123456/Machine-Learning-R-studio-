setwd("D:/BA classes/machine learning dataset of R/unsupervised learning/clustering/her clustering")
getwd()
clusterdata<-read.csv("Wholesale customers data.csv")
str(clusterdata)

cust_data_f<- scale(clusterdata)

head(cust_data_f)

#euclidean distance
dist.res=dist(cust_data_f,method = "euclidean")

#herclerical clustering
hc<- hclust(dist.res,method="complete")
hc1<- hclust(dist.res,method="single")
hc2<- hclust(dist.res,method="centroid")

plot(hc, hang = -1, cex = 0.6)
plot(hc2, hang = -1, cex = 0.6)