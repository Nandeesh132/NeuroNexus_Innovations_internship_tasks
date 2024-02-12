
# Load the Titanic dataset
titanic_data= read.csv(file.choose())
View(titanic_data )

sum(is.na(titanic_data))
titanic_cleandata <- na.omit(titanic_data)
sum(is.na(titanic_cleandata))
View(titanic_cleandata)
View(show(is.na(titanic_data)))
# Explore the dataset
head(titanic_cleandata)
# Data Preprocessing
# Drop irrelevant columns and handle missing values
titanic_cleandata <- titanic_cleandata[, c("Survived", "Pclass", "Sex", "Age", "SibSp", "Parch", "Fare")]
titanic_cleandata$Age[is.na(titanic_cleandata$Age)] <- median(titanic_cleandata$Age, na.rm = TRUE)
titanic_cleandata$Survived <- factor(titanic_cleandata$Survived, levels = c(0, 1), labels = c("Not Survived", "Survived"))



View(titanic_data)
# Split the data into training and testing sets
library(caTools)
set.seed(50)
sampletitan=sample.split(titanic_cleandata$Survived,SplitRatio = 0.7)

train_titan=subset(titanic_cleandata,sampletitan==TRUE)
test_titan=subset(titanic_cleandata,sampletitan==FALSE)
View(test_titan)
sum(is.na(train_titan))
sum(is.na(test_titan))

sum(is.na(titanic_cleandata))

#here i used naive bayes to predict the survived accuracy
#Build and train the naive bayes
library(e1071)
inb=naiveBayes(train_titan[-1],train_titan$Survived)
ipre=predict(inb,test_titan[-1])
library(caret)
confusionMatrix(ipre,test_titan$Survived)
# here result accuracy is 0.968 and positive class is "not survived"
