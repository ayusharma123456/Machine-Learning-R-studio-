setwd("D:/BA classes/machine learning dataset of R/supervised learning/classification/random forest")
getwd()
# Random Forest regression

# Importing the dataset
data = read.csv('Position_Salaries.csv')
data = data[2:3]

library(randomForest)
set.seed(1234)

rf <- randomForest(Salary ~ . , data = data,importance=TRUE,proximity=TRUE)
print(rf)

# Predicting a new result with Random Forest Regression
pred = predict(rf, data.frame(Level = 6.5))
pred

