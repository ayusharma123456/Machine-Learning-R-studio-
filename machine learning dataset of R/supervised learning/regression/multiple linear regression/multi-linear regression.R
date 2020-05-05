setwd("D:/BA classes/machine learning dataset of R/supervised learning/regression/multiple linear regression")
getwd()

data<-read.csv("mydata.csv")
str(data)

#splitting
set.seed(1234)
split<-sample(2,nrow(data),replace = T,prob = c(0.7,0.3))
training<-data[split == 1,]
test<-data[split == 2,]

cor(data)

mod<-lm(Purchase.made~.,data=training)
vif(mod)
step(mod)

mod1<-lm(formula = Purchase.made ~ X + Age + Signed.in.since.Days. + 
           Job.type_employed + Education_secondary + Metro_y, data = training)

summary(mod1)

# Make predictions
predictions <- mod1 %>% predict(test)
# Model performance
data.frame(
  RMSE = RMSE(predictions, test$Purchase.made),
  R2 = R2(predictions, test$Purchase.made)
)

#2 - Normality of errors
hist(residuals(mod1))
#3 - Homoscedasticity
plot(training$Purchase.made, residuals(mod1))


