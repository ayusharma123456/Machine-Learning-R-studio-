setwd("D:/BA classes/machine learning dataset of R/supervised learning/classification/random forest")
getwd()
# Random Forest Classification

# Importing the dataset
data = read.csv('Social_Network_Ads.csv')
data = data[3:5]

# Encoding the target feature as factor
data$Purchased<-as.factor(data$Purchased)

#splitting
set.seed(1234)
split<-sample(2,nrow(data),replace = T,prob = c(0.7,0.3))
training<-data[split == 1,]
test<-data[split == 2,]

#random forest model
library(randomForest)
set.seed(222)
rf <- randomForest(Purchased ~ . , data = training,ntree=300,importance=TRUE,proximity=TRUE)
print(rf)

#predicition and confusion matrix
pred<-predict(rf,training)
pred
confusionMatrix(pred,training$Purchased)

pred<-predict(rf,test)
pred
confusionMatrix(pred,test$Purchased)

#plot a error rate of rf model
plot(rf)

aucrf_test <- roc(as.numeric(test$Purchased), as.numeric(pred),
                  ci=TRUE)
plot(aucrf_test, ylim=c(0,1), print.thres=TRUE, main=paste('Random Forest
AUC:',round(aucrf_test$auc[[1]],3)),col = 'blue')

varImpPlot(rf)