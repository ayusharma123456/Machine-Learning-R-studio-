setwd("D:/BA classes/machine learning dataset of R/supervised learning/classification/logistic regrssion")
getwd()

loan = read.csv("Loan.csv")
head(loan)

loan = loan[loan$Experience > 0,]

#table comparison of yes and no
loanNew = loan
t = loanNew[,"PersonalLoan"]== 1
table(t)


# Create Training Data new creating data just change the dependent var (personal loan) 

input_ones <- loanNew[which(loanNew$PersonalLoan == 1), ]  # all 1's
input_zeros <- loanNew[which(loanNew$PersonalLoan == 0), ]  # all 0's

set.seed(100)  # for repeatability of samples
input_ones_training_rows <- sample(1:nrow(input_ones), 0.7*nrow(input_ones))  # 1's for training
input_zeros_training_rows <- sample(1:nrow(input_zeros), 0.7*nrow(input_ones))  # 0's for training. Pick as many 0's as 1's
training_ones <- input_ones[input_ones_training_rows, ]  
training_zeros <- input_zeros[input_zeros_training_rows, ]
trainingData <- rbind(training_ones, training_zeros)  # row bind the 1's and 0's 

# Create Test Data
test_ones <- input_ones[-input_ones_training_rows, ]
test_zeros <- input_zeros[-input_zeros_training_rows, ]
testData <- rbind(test_ones, test_zeros)  # row bind the 1's and 0's 



#model building
Mod=glm(PersonalLoan~
          Age+
          Experience+
          Income+
          as.factor(Family)+
          CCAvg+
          as.factor(Education)+
          Mortgage+
          as.factor(SecuritiesAccount)+
          as.factor(CDAccount)+
          as.factor(Online)+
          as.factor(CreditCard)
        ,family=binomial,data=data.frame(trainingData))

(sury = summary(Mod))

#checking the multi-collinearity VIF factor is less than the value of 5
library(car)
t = vif(Mod)
sort(t, decreasing = T)

##instead of removing all these variables one by one, we use the step function, which
#automatically calculated the best equation
stpmod = step(Mod, direction = "both")
formula(stpmod)
summary(stpmod)

Mod1<-glm(formula = PersonalLoan ~ Income + as.factor(Family) + CCAvg + 
            as.factor(Education) + as.factor(SecuritiesAccount) + as.factor(CDAccount) + 
            as.factor(Online) + as.factor(CreditCard), family = binomial, 
          data = data.frame(trainingData))


#predicting the value 
predicted <- plogis(predict(Mod1, testData)) 

#Decide on optimal prediction probability cutoff for the model
library(InformationValue)
library(caret)
optCutOff <- optimalCutoff(testData$PersonalLoan, predicted)
optCutOff

#Misclassification Error and ROC curve
misClassError(testData$PersonalLoan, predicted, threshold = optCutOff)
plotROC(testData$PersonalLoan, predicted)

## Concordance Test #
Concordance(testData$PersonalLoan, predicted)

##confusion matrix ##
confusionMatrix(testData$PersonalLoan, predicted, threshold = optCutOff)
sensitivity(testData$PersonalLoan, predicted, threshold = optCutOff)
specificity(testData$PersonalLoan, predicted, threshold = optCutOff)
