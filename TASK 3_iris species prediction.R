

# Load the Iris dataset
iris=read.csv(file.choose())
View(iris)
sum(is.na(iris))
iris$Species <- factor(iris$Species, levels = c("setosa", "versicolor","virginica"), labels = c("setosa", "versicolor","virginica"))

# Check the structure of the dataset
str(iris)
library(caTools)
set.seed(50)
sample=sample.split(iris$Species,SplitRatio = 0.7)

train_iris=subset(iris,sample==TRUE)
test_iris=subset(iris,sample==FALSE)
sum(is.na(train_iris))
sum(is.na(test_iris))
#here i do three ml supervised learning classification algorithms for predicting specis
#Load libraries 
#naive bayes
library(e1071)
inb=naiveBayes(train_iris[-5],train_iris$Species)
ipre=predict(inb,test_iris[-5])
library(caret)
confusionMatrix(ipre,test_iris$Species)
#decision tree
library(rpart)
idt=rpart(formula = Species~.,data=train_iris)
idpre=predict(idt,test_iris[-5],type="class")
library(caret)
confusionMatrix(idpre,test_iris$Species)
#randomForest
library(randomForest)
irf=randomForest(x=train_iris[-5],y=train_iris$Species,ntrees=25)
irpre=predict(irf,test_iris[-5])
library(caret)
confusionMatrix(irpre,test_iris$Species)
# all are giving 0.955 accuracy
 