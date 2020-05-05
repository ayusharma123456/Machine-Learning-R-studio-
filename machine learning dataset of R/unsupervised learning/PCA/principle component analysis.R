setwd("D:/BA classes/machine learning dataset of R/unsupervised learning/PCA")
getwd()
data<-read.csv("Wine.csv")
str(data)

#splitting
set.seed(1234)
split<-sample(2,nrow(data),replace = T,prob = c(0.8,0.2))
training<-data[split == 1,]
test<-data[split == 2,]

#scatterplot and correation
library(psych)
library(factoextra)
pairs.panels(training[-14],gap=0,bg=c('red','yellow','blue')[training$Customer_Segment],
             pch = 21)

#principal component analysis (only numerical varibles can be used and with multicollionarity in data)
pc<-prcomp(training[-14],center = TRUE,scale. = TRUE)
print(pc)
summary(pc)
#graphical visualization of pca 
fviz_eig(pc)
fviz_pca_ind(pc,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

fviz_pca_biplot(pc, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)

pairs.panels(pc$x,gap=0,bg=c('red','yellow','blue')[training$Customer_Segment],
             pch = 21)
#predict
pred<-predict(pc,training)
pred <- data.frame(pred,training$Customer_Segment)

pred1<-predict(pc,test)
pred1 <- data.frame(pred1,test$Customer_Segment)

#data is ready in term of building any predict classfication model use pc1 pc2 as data set

