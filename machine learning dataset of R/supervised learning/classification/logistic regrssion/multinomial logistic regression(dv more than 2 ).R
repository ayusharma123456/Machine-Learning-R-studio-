cmcData <- read.csv
("http://archive.ics.uci.edu/ml/machine-learning-databases/cmc/cmc.data", stringsAsFactors=FALSE, header=F)
head(cmcData)
colnames(cmcData) <- c("wife_age", 
                       "wife_edu", "hus_edu", "num_child", "wife_rel", 
                       "wife_work", "hus_occu", "sil", "media_exp", "cmc")
head(cmcData)
##Convert binary Numerics values to Factors
cmcData$wife_edu <- factor(cmcData$wife_edu, levels=sort(unique(cmcData$wife_edu)))
cmcData$hus_edu <- factor(cmcData$hus_edu, levels=sort(unique(cmcData$hus_edu)))
cmcData$wife_rel <- factor(cmcData$wife_rel, levels=sort(unique(cmcData$wife_rel)))
cmcData$wife_work <- factor(cmcData$wife_work, levels=sort(unique(cmcData$wife_work)))
cmcData$hus_occu <- factor(cmcData$hus_occu, levels=sort(unique(cmcData$hus_occu)))
cmcData$sil <- factor(cmcData$sil, levels=sort(unique(cmcData$sil)))
cmcData$media_exp <- factor(cmcData$media_exp, levels=sort(unique(cmcData$media_exp)))
cmcData$cmc <- factor(cmcData$cmc, levels=sort(unique(cmcData$cmc)))

set.seed(100)
trainingRows <- sample(1:nrow(cmcData), 0.7*nrow(cmcData))
training <- cmcData[trainingRows, ]
test <- cmcData[-trainingRows, ]

library(nnet)
multinomModel <- multinom(cmc ~ ., data=training) # multinom Model
summary (multinomModel) # model summary

#predicting the values
predicted_scores <- predict (multinomModel, test, "probs") # predict on new data
predicted_class <- predict (multinomModel, test)

#Confusion Matrix and Misclassification Error
table(predicted_class, test$cmc)

mean(as.character(predicted_class) != as.character(test$cmc))
##A misclassification error of 49.3% is probably too high. May be it can be
##improved by improving the model terms 