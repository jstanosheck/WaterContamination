library(glmnet)
library(pROC)

data <- read.csv("Florida\ Water\ Quality\ Data.csv")
summary(data)

#test train test split function
data <- traintestsplit(data, 0.2)

#set Salmonella as a factor for train and test
data$trainset$Salmonella <- as.factor(data$trainset$Salmonella)
data$testset$Salmonella <- as.factor(data$testset$Salmonella)

#set Pond to as a factor
data$trainset$Pond <- as.factor(data$trainset$Pond)
data$testset$Pond <- as.factor(data$testset$Pond)

#test outlier na value function
data <- outliermissingvalues(data$trainset, data$testset)

#standardize the data sets
data <- standardize(data$trainset, data$testset)

#Check number of instances for the Salmonella 
#############################################
table(data$Salmonella) # 0 = 223 1 = 11

#shuffle the data frame as a whole
set.seed(1234)
rows <- sample(nrow(data))
data <- data[rows, ]
head(data) #check that data was shuffled
############################################


#Get the indecies that are in the testing data set
##################################################
index <- sample(nrow(data) * 0.2, )

#Create test and train data with the index
trainset <- data[-index, ]
testset <- data[index, ]
table(trainset$Salmonella)
table(testset$Salmonella)

#set Salmonella as a factor for train and test
trainset$Salmonella <- as.factor(trainset$Salmonella)
testset$Salmonella <- as.factor(testset$Salmonella)
#set Pond to as a factor
trainset$Pond <- as.factor(trainset$Pond)
testset$Pond <- as.factor(testset$Pond)
###################################################

#Remove NA values and check for outliers 
#replace NA values with the mean of the column
for(column in 1:ncol(trainset)){
  #only runs the numeric functions if the column is not a factor
  if(!is.factor(trainset[1, column])){
    #check if any values in the data frame are NA
    if(sum(is.na(trainset)) > 0){
      #get the mean of the column
      average <- mean(trainset[, column], na.rm = T) 
      #find the instances with NA values in that column replace with average
      index <- which(is.na(trainset[, column]))
      trainset[index, column] <- average
    }
  }
  #check for outliers and replace them with the median for each row
  if(!is.factor(trainset[, column])){
    #find the min and max values for being non outliers
    iqrange <- IQR(trainset[, column], na.rm = T)
    maxvalue <- as.numeric(quantile(trainset[, column], 3/4, na.rm = T) + iqrange)
    minvalue <- as.numeric(quantile(trainset[, column], 1/4, na.rm = T) - iqrange)
    
    #all values < Q1 - IQR replaced by average
    minindex <- which(trainset[, column] < minvalue)
    trainset[minindex, column] <- median(trainset[, column])
    
    #all values > Q3 + IQR replace by average
    maxindex <- which(trainset[, column] > maxvalue)
    trainset[maxindex, column] <- median(trainset[, column])
  }
}

###################################################

#standardize the test set using train set mean and sd
###################################################
#use z-score to standardize each non-factor variable
#use mean and sd from trainset
for(column in 1:ncol(testset)){
  #only run this if is a numeric
  if(is.numeric(testset[1, column])){
    #get column mean
    average <- mean(trainset[, column])
    #get column standard deviation
    sdev <- sd(trainset[, column])
    #calculate column of z scores
    testset[, column] <- abs(testset[, column] - average) / sdev
  }
}


###################################################

#Standardize each column
###################################################
#use z-score to standardize each non-factor variable
for(column in 1:ncol(trainset)){
  #only run this if is a numeric
  if(is.numeric(trainset[, column])){
    #get column mean
    average <- mean(trainset[, column])
    #get column standard deviation
    sdev <- sd(trainset[, column])
    #calculate column of z scores
    trainset[, column] <- abs(trainset[, column] - average) / sdev
  }
}

###################################################

#check for collinearity among features
###################################################

#build linear model to predict Salmonella using all variables - Pond
collinear_model <- glm(Salmonella~.-Pond, data = trainset, family = 'binomial')

#use caret library to find VIF for each variable
#if variable has VIF higher than 5 remove from use
#remove variable with highest VIF first and recheck until all are <5
car::vif(collinear_model)#none are <2

###################################################

#use SMOTE on the train set
###################################################
smote_test <- DMwR::SMOTE(Salmonella~.-Pond, data = trainset, perc.over = 2000,
                          perc.under = 105)
table(smote_test$Salmonella) #after data augmentation
table(trainset$Salmonella) #before data augmentation

#test the model based on the smote data
test_model <- glm(Salmonella~.-Pond, data = smote_test, family = 'binomial')
summary(test_model)
predictions <- predict(test_model, newdata = testset, type = 'response')
probs <- ifelse(predictions < 0.000361, 1, 0)
table(testset$Salmonella, probs)
roc(testset$Salmonella, predictions, plot = T) #plots roc curve

#test a model based on the original train data
test_model_true <- glm(Salmonella~.-Pond, data = trainset, family = 'binomial')
summary(test_model_true)
predictions_true <- predict(test_model_true, newdata = testset, type = 'response')
probs_true <- ifelse(predictions_true > 0.5, 1, 0)
table(testset$Salmonella, probs_true)
roc(testset$Salmonella, predictions_true, plot = T) #plots roc curve
######################################################

#ADASYN testing oversampling
######################################################
#testing adasyn algorithm
adasyn_test <-smotefamily::ADAS(trainset[, -c(1,2)], trainset[, 2])
summary(adasyn_test$data)

#set Salmonella as a factor
adasyn_test$data$Salmonella <- as.factor(adasyn_test$data$Salmonella)
#test adasyn model
adasyn_model <- glm(Salmonella~., data = adasyn_test$data, family = 'binomial')
summary(adasyn_model)

predictions <- predict(adasyn_model, newdata = testset, type = 'response')
probs <- ifelse(predictions < 0.000000000000001, 1, 0)
table(testset$Salmonella, probs)
roc(testset$Salmonella, predictions, plot = T) #plots roc curve

######################################################

