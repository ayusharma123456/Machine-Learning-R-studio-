setwd("D:/BA classes/machine learning dataset of R/supervised learning/classification/classficiation tree")
getwd()
data<-read.csv("HR dataset.csv")
data
library(party)
library(pROC) 
library(e1071) # for ROC curve
library(rpart)#decision tree
library(rpart.plot) #for Rpart plot

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

# DT using party and rpart algorithm
tree <- ctree(left ~ .,data = training,controls = ctree_control(mincriterion = 0.99,minsplit = 500))
tree
plot(tree)

fit = rpart(left ~ ., data = training ,method = "class", control =
              rpart.control(minsplit = 30,cp = 0.01))
rpart.plot(fit)
printcp(fit)

#predition and confusion matrix
predtr <- predict(fit,training,type = "class" )
confusionMatrix(predtr,training$left)

predtest <- predict(fit,test,type = "class" )
confusionMatrix(predtr,test$left)

auctest <- roc(as.numeric(test$left), as.numeric(predtest))
print(auctest)
plot(auctest, ylim=c(0,1), print.thres=TRUE,
     main=paste('AUC:',round(auctest$auc[[1]],3)),col = 'blue')