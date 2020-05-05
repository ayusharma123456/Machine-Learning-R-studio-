carsdata <- read.csv("http://archive.ics.uci.edu/ml/machine-learning-databases/car/car.data", 
                     header=F, stringsAsFactors=F)  # import string variables as characters.
colnames(carsdata) <- c("buying", "maint", "doors", "persons", "lug_boot", "safety", "class")
head(carsdata)

##factorization the binary value and categorial values
carsdata$buying <- factor(carsdata$buying, levels=c("low", "med", "high", "vhigh"), ordered=TRUE)
carsdata$maint <- factor(carsdata$maint, levels=c("low", "med", "high", "vhigh"), ordered=TRUE)
carsdata$doors <- factor(carsdata$doors, levels=c("2", "3", "4", "5more"), ordered=TRUE)
carsdata$persons <- factor(carsdata$persons, levels=c("2", "4", "more"), ordered=TRUE)
carsdata$lug_boot <- factor(carsdata$lug_boot, levels=c("small", "med", "big"), ordered=TRUE)
carsdata$safety <- factor(carsdata$safety, levels=c("low", "med", "high"), ordered=TRUE)
carsdata$class <- factor(carsdata$class, levels=c("unacc", "acc", "good", "vgood"), ordered=TRUE)


# Prepare Training and Test Data
set.seed(100)
trainingRows <- sample(1:nrow(carsdata), 0.7 * nrow(carsdata))
trainingData <- carsdata[trainingRows, ]
testData <- carsdata[-trainingRows, ]


### Build ordered logistic regression model
library(MASS)
polrMod <- polr(class ~ safety + lug_boot + doors + buying + maint, data=trainingData)
summary(polrMod)


### Predict
predictedClass <- predict(polrMod, testData)  # predict the classes directly
head(predictedClass)


## Confusion matrix and misclassification error
table(testData$class, predictedClass)  # confusion matrix
mean(as.character(testData$class) != as.character(predictedClass))  # misclassification error
