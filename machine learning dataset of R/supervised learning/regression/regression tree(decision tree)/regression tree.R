setwd("D:/BA classes/machine learning dataset of R/supervised learning/regression/regression tree(decision tree)
getwd()

library(rpart)#decision tree
library(rpart.plot) #for Rpart plot
data<-read.csv("mydata.csv")
str(data)

#splitting
set.seed(1234)
split<-sample(2,nrow(data),replace = T,prob = c(0.7,0.3))
training<-data[split == 1,]
test<-data[split == 2,]

#model buliding  CART 
fit = rpart(Purchase.made ~ ., data = training)
      rpart.plot(fit)
      printcp(fit)

# Make predictions
predictions <- fit %>% predict(test)
      # Model performance
      data.frame(
      RMSE = RMSE(predictions, test$Purchase.made),
      R2 = R2(predictions, test$Purchase.made)
      )

