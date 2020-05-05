setwd("D:/BA classes/machine learning dataset of R/supervised learning/classification/navi bayes")
getwd()
library(caret)
library(pROC)
library(e1071)

data<-read.csv("Bank.csv")
head(data)
summary(data)

data$y<-as.factor(data$y)
View(data)

split<-sample(2,nrow(data),replace = T,prob = c(0.7,0.3))
training<-data[split == 1,]
test<-data[split == 2,]

print(table(training$y))
print(table(test$y))

#NaiveBayes Model
modelnb <- naiveBayes(as.factor(y) ~. , data = training)
modelnb


#predict and confusion matrix
prednb_tr <- predict(modelnb,training)
confusionMatrix(prednb_tr,training$y)

prednb_tr <- predict(modelnb,test)
confusionMatrix(prednb_tr,test$y)

