library(caret)
library(randomForest)
library(fields)

setwd("~/Desktop/Titanic/")

trainSet <- read.table("train.csv", sep= "," , header = TRUE)

testSet <- read.table("test.csv", sep= "," , header = TRUE)

#Include Pclass and Sex in predictive algorithm
table(trainSet[,c("Survived","Pclass")])
table(trainSet[,c("Survived","Sex")])

table(trainSet[,c("Survived","Age")]) #children younger than 6 and younger
                                      #box plot better for continous variables
bplot.xy(trainSet$Survived, trainSet$Age)
summary(trainSet$Age) #Too many NA's

bplot.xy(trainSet$Survived, trainSet$Fare)
summary(trainSet$Fare) #No NA's

#Convert Survived to Factor
trainSet$Survived <- factor(trainSet$Survived)
#Set a random seed to get same results as tutorial
set.seed(42)
#Train the model using "random forest" algorithm

model <- train(Survived ~ Pclass + Sex + SibSp + Embarked + Parch + Fare,
               data = trainSet, method = "rf", trControl = trainControl(method = "cv", number = 5))
model

testSet$Survived <- predict(model, newdata = testSet)

testSet$Fare <- ifelse(is.na(testSet$Fare), mean(testSet$Fare, na.rm = TRUE), testSet$Fare)

submission <- testSet[,c("PassengerId", "Survived")]
write.table(submission, file = "submission.csv", col.names = TRUE, row.names = FALSE, sep = ",")
