library(caret)
install.packages("psych")
library(glmnet)
library(mlbench)
library(psych)

data("BostonHousing")
data<-BostonHousing
str(data)

#correalatin between chas and medv
pairs.panels(data[c(-4,-14)],cex=2)
#all numerical variable are multicollinear to each other lead to overfitting


#data partion 
ind<-sample(2,nrow(data),replace = TRUE,prob = c(0.7,0.3))
train<-data[ind==1,]
test<-data[ind==2,]

#custom control parameter
control<-trainControl(method = "repeatedcv",number = 10,repeats = 5,verboseIter = TRUE)

#linear regression
lm<-train(medv~.,train,method='lm',trControl=control)
summary(lm)
plot(lm$finalModel)

###ridge model(it do shrinkage cofficient and keep all variable in the model)
ridge<-train(medv~.,train,method ='glmnet',
             tuneGrid=expand.grid(alpha=0,
              lambda = seq(0.0001,1,length=5)),trControl=control)
#plot 
plot(ridge)
plot(ridge$finalModel,xvar = "lambda",label = TRUE)
plot(ridge$finalModel,xvar = "dev",label = TRUE)
plot(varImp(ridge,scale = F))

###lasso regression(coefficient skrinkage and but do feature selection)
lasso<-train(medv~.,train,method ='glmnet',
             tuneGrid=expand.grid(alpha=1,
                                  lambda = seq(0.0001,1,length=5)),trControl=control)
plot(lasso)
plot(lasso$finalModel,xvar = "lambda",label = TRUE)
plot(lasso$finalModel,xvar = "dev",label = TRUE)
plot(varImp(lasso,scale = F))

###elastic regrssion 
en<-train(medv~.,train,method ='glmnet',
           tuneGrid=expand.grid(alpha=seq(0,1,length=10),
                                lambda = seq(0.0001,0.2,length=5)),trControl=control)
plot(en)
plot(en$finalModel,xvar = "lambda",label = TRUE)
plot(en$finalModel,xvar = "dev",label = TRUE)
plot(varImp(en,scale = F))


#compare model
model_list<-list(linear=lm,ridge=ridge,lasso=lasso,elastic=en)
res<-resamples(model_list)
summary(res)

#best model
lasso$bestTune

#save the model
saveRDS(lasso,"final_model.rds")
fm<-readRDS("final_model.rds")

perd<-predict(fm,test)

#mean square error
sqrt(mean((test$medv-perd)^2))