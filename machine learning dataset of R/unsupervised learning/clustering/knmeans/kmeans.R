setwd("D:/BA classes/machine learning dataset of R/unsupervised learning/clustering/knmeans")
getwd()
clusterdata<-read.csv("Wholesale customers data.csv")
str(clusterdata)

summary(clusterdata)

#scaling the data 
cust_data_f<- scale(clusterdata)

#plot to know the k means 
library(factoextra)
set.seed(123)

#1 elbow plot
fviz_nbclust(cust_data_f, kmeans, method = "wss")

#2 silhouette plot
fviz_nbclust(cust_data_f, kmeans, method = "silhouette")

#3 gap stastistic
set.seed(123)
gap_stat <- clusGap(cust_data_f, FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)
fviz_gap_stat(gap_stat)

###Run the kmeans algorithm to generate the clusters
k1<-kmeans(cust_data_f,center=3,nstart = 25)
str(k1)
k1
fviz_cluster(k1, data =cust_data_f )


