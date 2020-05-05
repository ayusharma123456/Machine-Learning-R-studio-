setwd("D:/BA classes/machine learning dataset of R/supervised learning/SVM/classification")
getwd()

data<-read.csv("Social_Network_Ads.csv")
data$Purchased<-as.factor(data$Purchased)
str(data)
data<-data[3:5]
View(data)


#splitting
set.seed(1234)
split<-sample(2,nrow(data),replace = T,prob = c(0.7,0.3))
training<-data[split == 1,]
test<-data[split == 2,]


trctrl<-trainControl(method = "repeatedcv" , number = 10, repeats=3)

#svm model with linear 
mod<-svm(formula = Purchased ~ .,
            data = training,
            type = 'C-classification',
            kernel = 'linear',
            trControl=trctrl,
            preProcess=c("center","scale"),
            tuneLength = 10)

# improve the model 
grid<-expand.grid(C=c(0,0.01,0.05,0.1,0.25,0.5,0.75,1,1.25,1.5,1.75,2.5))
mod2<-svm(formula = Purchased ~ .,
         data = training,
         type = 'C-classification',
         kernel = 'linear',
         trControl=trctrl,
         preProcess=c("center","scale"),
         tuneGrid=grid,
         tuneLength = 10)
#predict
pred<-predict(mod2,newdata=test)
confusionMatrix(table(pred,test$Purchased))


