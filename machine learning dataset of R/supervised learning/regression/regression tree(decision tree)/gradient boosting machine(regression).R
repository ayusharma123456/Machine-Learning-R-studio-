setwd("D:/BA classes/machine learning dataset of R/supervised learning/regression/regression tree(decision tree)
getwd()
data<-read.csv("mydata.csv")


##Gradient Boosting XGBoost Algorithm require require DV to be contionious variable(numerical&interger)
str(data)

#splitting
set.seed(1234)
split<-sample(2,nrow(data),replace = T,prob = c(0.7,0.3))
training<-data[split == 1,]
test<-data[split == 2,]

library(xgboost)
library(caret)
library(gbm)

# train GBM model
gbm.fit=gbm(Purchase.made ~ . ,data = training,distribution = "gaussian",n.trees = 10000,
                  shrinkage = 0.01, interaction.depth = 4,cv.folds = 10)

# print results
print(gbm.fit)

# get MSE and compute RMSE
sqrt(min(gbm.fit$cv.error))

# plot loss function as a result of n trees added to the ensemble
gbm.perf(gbm.fit, method = "cv")

vip(gbm.fit)

# predict values for test data
pred <- predict(gbm.fit,test)

# results
caret::RMSE(pred, test$Purchase.made)
