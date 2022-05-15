#Source the functions for the modeling and for all pre-processing
source('modelingfunc.R')
source('Preprocessfunc.R')

#Bring in any needed libraries here
library(glmnet)
library(pROC)
library(lattice)
library(ggplot2)


#Collecting all data and doing all Pre-processing steps
########################################################

#Retrieve all of the training data
data <- read.csv("Data/FloridaWaterQualityData.csv")

#Split the training data into training and testing data
data <- traintestsplit(data, 0.2)

#set Salmonella as a factor for train and test
data$trainset$Salmonella <- as.factor(data$trainset$Salmonella)
data$testset$Salmonella <- as.factor(data$testset$Salmonella)

#set Pond to as a factor
data$trainset$Pond <- as.factor(data$trainset$Pond)
data$testset$Pond <- as.factor(data$testset$Pond)

#Test the data for any missing values or any outliers and replace both with
#the average value of the feature
data <- outliermissingvalues(data$trainset, data$testset)

#Standardize the training and testing data using a z-score analysis
data <- standardize(data$trainset, data$testset)

#Synthesize oversampling data by using SMOTE, ADASYN, and SL-SMOTE
#This will return three sets of training and testing data to use, one for each
#of the oversampling methods.
#The "K" and "C" values must be at least 4.
oversampled <- syngen(data$trainset, 'Salmonella', c(1, 2), C = 5)
########################################################


#Training and testing of the Binomial Logistic Regression
########################################################

#Use logistic_model function to determine the logistic model for each method
#this uses the self-made "logistic_model"
log_outputs <- logistic_model(oversampled, data$testset, 10, c(5, 6, 7, 8, 9), cutoff = 0.475354237)

#roc plot for each log_output
#smote
roc.info <- pROC::roc(data$testset$Salmonella, log_outputs$smote_prob, plot = T)
roc.df <- data.frame(
  tpp = roc.info$sensitivities * 100,
  fpp = roc.info$specificities * 100,
  thresholds = roc.info$thresholds)
roc.df

#make confusion matrix for SMOTE
table(log_outputs$smote_predict, data$testset$Salmonella)

#adasyn
roc.info <- pROC::roc(data$testset$Salmonella, log_outputs$adasyn_prob, plot = T)
roc.df <- data.frame(
  tpp = roc.info$sensitivities * 100,
  fpp = roc.info$specificities * 100,
  thresholds = roc.info$thresholds)
roc.df

#make confusion matrix for adasyn
table(log_outputs$adasyn_predict, data$testset$Salmonella)

#SL-SMOTE
roc.info <- pROC::roc(data$testset$Salmonella, log_outputs$slsmote_prob, plot = T)
roc.df <- data.frame(
  tpp = roc.info$sensitivities * 100,
  fpp = roc.info$specificities * 100,
  thresholds = roc.info$thresholds)
roc.df

#make confusion matrix for SL-SMOTE
table(log_outputs$slsmote_predict, data$testset$Salmonella)


#testing with original data

train_control <- caret::trainControl(method = 'cv', number = 10)

#use original data set
origin_model <- caret::train(data$trainset[, 7:11],
                             y = data$trainset[, 2],
                             method = 'glm',
                             trControl = train_control,
                             family = binomial())

#use origin model to predict on the test data
origin_prob <- predict(origin_model$finalModel, newdata = data$testset, type = 'response')
origin_predict <- ifelse(origin_prob > 0.5, 1, 0)

roc.info <- pROC::roc(data$testset$Salmonella, origin_prob, plot = T)
roc.df <- data.frame(
  tpp = roc.info$sensitivities * 100,
  fpp = roc.info$specificities * 100,
  thresholds = roc.info$thresholds)
roc.df

#make confusion matrix for SL-SMOTE
table(origin_predict, data$testset$Salmonella)
########################################################
