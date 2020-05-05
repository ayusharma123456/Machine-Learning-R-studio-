setwd("D:/BA classes/machine learning dataset of R/unsupervised learning/PCA")
getwd()
data<-read.csv("Social_Network_Ads.csv")
data = data[, 3:5]
str(data)

#splitting
set.seed(1234)
split<-sample(2,nrow(data),replace = T,prob = c(0.8,0.2))
training<-data[split == 1,]
test<-data[split == 2,]

#scatterplot and correation
library(psych)
pairs.panels(training[-3],gap=0,bg=c('red','yellow','blue')[training$Purchased],
             pch = 21)


# Applying Kernel PCA
library(kernlab)
kpca = kpca(~., data = training[-3], kernel = 'rbfdot', features = 2)
print(kpca)
summary(kpca)

#predict
pred<-predict(kpca,training)
pred <- data.frame(pred,training$Purchased)

pred1<-predict(kpca,test)
pred1 <- data.frame(pred1,test$Purchased)