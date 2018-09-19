1. Use the below given data set
Data Set
2. Perform the below given activities:
a. Create classification model using logistic regression model
b. verify model goodness of fit
c. Report the accuracy measures
d. Report the variable importance
e. Report the unimportant variables
f. Interpret the results
g. Visualize the results

setwd("F:/AcadGild/workings")

library(readr)
library(Hmisc)
library(dplyr)
library(MASS)
library(corrplot)
library(ggplot2)
library(car)
library(dplyr)
library(utils)
library(stats)
library(caret)
library(rpart)
library(randomForest)


#import test data set
Data<-read.csv("F:/AcadGild/workings/Example_WearableComputing_weight_lifting_exercises_biceps_curl_variations.csv", header = TRUE)
summary(Data)
str(Data)
dim(Data)
head(Data)

# list the levels for the class
sapply(Data, class)

# Delete all columns with missing values

sapply(Data, function(x) sum(is.na(x))) # missing values  

Data1 <-Data[,colSums(is.na(Data)) == 0]
str(Data1)
dim(Data1)

# deleting irrelevent(unimportant) variable to current project
Training <- Data1[,-c(1:6)]


# The data partition was split in 70% training and 30% testing.
set.seed(1111)
ind = sample(1:nrow(Training),0.7*nrow(Training),replace = F)
df_train =Training[ind,]
df_test = Training[-ind,]

#Exploratory analysis: 
dim(df_train)
str(df_train)
head(df_train)
describe(df_train)

dim(df_test)
str(df_test)
head(df_test)
describe(df_test)


# Prediction First model - Decesion Tree
FirstModel <- rpart(classe ~ ., data=df_train, method="class")


#View the Decision Tree using fancy
fancyRpartPlot(FirstModel)

#Predicting
FirstPrediction <- predict(FirstModel, df_test, type = "class")

# Using confusion Matrix to test results:
confusionMatrix(FirstPrediction, df_test$classe)

#Second Prediction Model - Random Forests
SecondModel <- randomForest(classe ~. , data=df_train, method="class")

#Predicting:
SecondPrediction <- predict(SecondModel, df_test, type = "class")

#Test results on TestingTraining data set:
confusionMatrix(SecondPrediction, df_test$classe)

#Testing the better model on original Testing Set
FinalPrediction <- predict(SecondModel, df_train, type="class")
FinalPrediction


