setwd("D:/BA classes/machine learning dataset of R/Exploratory Data Analysis")
getwd()
data<-read.csv("EDA_data.csv")
data

summary(data)

hist(data$Landacres)

hist(data$HouseSizesqrft)

hist(data$AppraisedValue)

#scatterplot
plot(data$AppraisedValue,data$Baths)

#Two-way table
counts = table(data$Education,data$Gender)
counts

# replacing the NA values for variable with mean 
data$Rooms[is.na(data$Rooms)] <- mean(data$Rooms, na.rm =
                                                  TRUE)

#create as a dummy variables 
data$Gender<-as.numeric(data$Gender=="M")
data$Education<-as.numeric(data$Education=="Grad")

#Dummy var creation for more than 2 
library(lme4)
new = dummy(data$Location)
new_data = cbind(data,new)
View(new_data)

# Encoding the categorical variables as factors
data$Location = as.numeric(factor(data$Location,
                                      levels = c('Glen Cove
', 'Long Beach
', 'Roslyn   
'),
                                      labels = c(1, 2, 3)))


data$Gender = as.numeric(factor(data$Gender,
                                   levels = c('F', 'M'),
                                   labels = c(1, 2)))


#outliner Univariate Analysis
bx=boxplot(data$AppraisedValue)
boxplot(data$Baths)

quantile(data$AppraisedValue, seq(0,1,0.02))

#removing the outliner
bx$stats
quantile(data$AppraisedValue, .95)
data$AppraisedValue<-ifelse(data$AppraisedValue>700,850,data$AppraisedValue)

boxplot(data$AppraisedValue)

