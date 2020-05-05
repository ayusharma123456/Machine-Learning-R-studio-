setwd("D:/BA classes/machine learning dataset of R/supervised learning/classification/knn")
getwd()
library(caret)
library(pROC)
library(mlbench)
# Importing the dataset
data = read.csv('Social_Network_Ads.csv')
data = data[3:5]

#factorisation
data$Purchased<-factor(data$Purchased)

#splitting
set.seed(1234)
split<-sample(2,nrow(data),replace = T,prob = c(0.7,0.3))
training<-data[split == 1,]
test<-data[split == 2,]

#KNN classification model
ct <- trainControl(method = "repeatedcv",
                        repeats = 3
                  )
set.seed(400)
fit <- train(Purchased ~ ., data = training, method = "knn", trControl = ct,
             preProcess = c("center","scale"),tuneLength = 20)

fit
plot(fit)
varImp(fit)

#predict
pred <- predict(fit,newdata = test)
pred

confusionMatrix(pred, test$Purchased)
aucrf_test <- roc(as.numeric(test$Purchased), as.numeric(pred),
                  ci=TRUE)
aucrf_test
plot(aucrf_test, ylim=c(0,1), print.thres=TRUE, main=paste('KNN MODEL
AUC:',round(aucrf_test$auc[[1]],3)),col = 'blue')


###REGRESSION###
data("BostonHousing")
data <- BostonHousing
str(data)

#splitting
set.seed(1234)
split<-sample(2,nrow(data),replace = T,prob = c(0.7,0.3))
training<-data[split == 1,]
test<-data[split == 2,]


#KNN classification model
ct <- trainControl(method = "repeatedcv",
                   repeats = 3
)
set.seed(300)
fit <- train(medv ~ ., data = training, 
             tuneGrid = expand.grid(k=1:70),
             method = "knn", trControl = ct,
             preProcess = c("center","scale"))
fit

pred<-predict(fit,newdata = test)
RMSE(pred,test$medv)
plot(pred ~ test$medv)