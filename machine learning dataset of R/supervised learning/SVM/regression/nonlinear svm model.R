library(tidyverse)
library(caret)

# Load the data
data("Boston", package = "MASS")


# Split the data into training and test set
set.seed(123)
training.samples <- Boston$medv %>%
  createDataPartition(p = 0.7, list = FALSE)
train.data  <- Boston[training.samples, ]
test.data <- Boston[-training.samples, ]


#First, visualize the scatter plot of the medv vs lstat variables as follow:
ggplot(train.data, aes(lstat, medv) ) +
  geom_point() +
  stat_smooth()

# building SVR model
library(e1071)

regressor = svm(formula = medv ~ .,
                data = train.data,
                type = 'eps-regression',
                kernel = 'radial')

regressor1 = svm(formula = medv ~ .,
                data = train.data,
                type = 'eps-regression',
                kernel = 'polynomial')
print(regressor)
print(regressor1)

pred<-predict(regressor,test.data)
pred

# Model performance
data.frame(
  RMSE = RMSE(pred, test.data$medv),
  R2 = R2(pred, test.data$medv)
)



