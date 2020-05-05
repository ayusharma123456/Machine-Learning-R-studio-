setwd("D:/BA classes/machine learning dataset of R/unsupervised learning/Linear Discriminant Analysis (LDA)")
getwd()

data<-read.csv("Wine.csv")
str(data)

# Splitting the dataset into the Training set and Test set
library(caTools)
set.seed(123)
split = sample.split(data$Customer_Segment, SplitRatio = 0.8)
training_set = subset(data, split == TRUE)
test_set = subset(data, split == FALSE)

# Feature Scaling
training_set[-14] = scale(training_set[-14])
test_set[-14] = scale(test_set[-14])

# Applying PCA
library(caret)
library(e1071)
pca = preProcess(x = training_set[-14], method = 'pca', pcaComp = 2)
training_set = predict(pca, training_set)
test_set = predict(pca, test_set)
test_set = test_set[c(2, 3, 1)]

# Fitting SVM to the Training set
classifier = svm(formula = Customer_Segment ~ .,
                 data = training_set,
                 type = 'C-classification',
                 kernel = 'linear')

# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-3])

# Making the Confusion Matrix
confusionMatrix(table(test_set[, 3], y_pred))

