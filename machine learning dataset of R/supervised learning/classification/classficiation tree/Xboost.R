setwd("D:/BA classes/machine learning dataset of R/supervised learning/classification/classficiation tree")
getwd()
data<-read.csv("HR dataset.csv")
data

library(readr)
library(caret)
library(dplyr)
library(xgboost)



#Removing role & salary as we are using role_code & salary_code
data = data[,-c(9,10)]
data$role_code = as.factor(data$role_code)
data$salary.code = as.factor(data$salary.code)
data$left = as.factor(data$left)


#splitting and printing the dependent variable in the table is imp fo confusion matrix
split<-sample(2,nrow(data),replace = T,prob = c(0.7,0.3))
training<-data[split == 1,]
test<-data[split == 2,]
print(table(test$left))
print(table(training$left))


model_gbm <- caret::train(left ~ .,
                          data = training,
                          method = "gbm",
                          trControl = trainControl(method = "repeatedcv", 
                                                   number = 5, 
                                                   repeats = 3, 
                                                   verboseIter = FALSE),
                          verbose = 0)

model_gbm

predtest <- predict(model_gbm,test )

confusionMatrix(predtest,test$left)
