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
data$trainset$Pathogen <- as.factor(data$trainset$Pathogen)
data$testset$Pathogen <- as.factor(data$testset$Pathogen)


#Test the data for any missing values or any outliers and replace both with
#the average value of the feature
data <- outliermissingvalues(data$trainset, data$testset)

#Standardize the training and testing data using a z-score analysis
data <- standardize(data$trainset, data$testset)

#Synthesize oversampling data by using SMOTE, ADASYN, and SL-SMOTE
#This will return three sets of training and testing data to use, one for each
#of the oversampling methods.
#The "K" and "C" values must be at least 4.
oversampled <- syngen(data$trainset, 'Pathogen', c(1), C = 5)
########################################################


#Training and testing of the Binomial Logistic Regression
########################################################

#Use logistic_model function to determine the logistic model for each method
#this uses the self-made "logistic_model"
log_outputs <- logistic_model(oversampled, data$testset, 5, c(1, 2, 3, 4),
                              cutoff = 0.49300275)

#roc plot for each log_output
#smote
roc.info <- pROC::roc(data$testset$Pathogen, log_outputs$smote_prob, plot = TRUE, main = "Logistic Regression SMOTE ROC", 
                      print.auc = TRUE, percent = TRUE, col = "#027FB9", asp = NA, grid = TRUE)
roc.df <- data.frame(
  tpp = roc.info$sensitivities,
  fpp = roc.info$specificities,
  thresholds = roc.info$thresholds)
roc.df

#make confusion matrix for SMOTE
table(log_outputs$smote_predict, data$testset$Pathogen)

#adasyn
roc.info <- pROC::roc(data$testset$Salmonella, log_outputs$adasyn_prob, plot = TRUE, main = "Logistic Regression ADASYN ROC",
                      percent = TRUE, print.auc = TRUE, col = "black", asp = NA, grid = TRUE)
roc.df <- data.frame(
  tpp = roc.info$sensitivities,
  fpp = roc.info$specificities,
  thresholds = roc.info$thresholds)
roc.df

#make confusion matrix for adasyn
table(log_outputs$adasyn_predict, data$testset$Salmonella)

#SL-SMOTE
roc.info <- pROC::roc(data$testset$Salmonella, log_outputs$slsmote_prob, plot = TRUE, main = "Logistic Regression SL-SMOTE ROC",
                      percent = TRUE, print.auc = TRUE, col = "black", asp = NA, grid = TRUE)
roc.df <- data.frame(
  tpp = roc.info$sensitivities,
  fpp = roc.info$specificities,
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
origin_predict <- ifelse(origin_prob > 4.755040e-02, 1, 0)

roc.info <- pROC::roc(data$testset$Salmonella, origin_prob, plot = TRUE, main = "Logistic Regression Non-Oversampled ROC",
                      percent = TRUE, print.auc = TRUE, col = "black", asp = NA, grid = TRUE)
roc.df <- data.frame(
  tpp = roc.info$sensitivities,
  fpp = roc.info$specificities,
  thresholds = roc.info$thresholds)
roc.df

#make confusion matrix for SL-SMOTE
table(origin_predict, data$testset$Salmonella)
########################################################









#Testing the best solutions from all above training using the experimental data
########################################################
#This is all going to be done by taking the original data and having it be used
#as the new training data. The experimental data will be used as the testing 
#data. 

#import both data sets again
training_data <- read.csv("Data/FloridaWaterQualityData.csv")
experimental_data <- read.csv("Data/ExperimentalData.csv")

#set the 'Pathogen' column as a factor
#sets the training data to a factor
training_data$Pathogen <- as.factor(training_data$Pathogen)
#sets the experimental data as a factor
experimental_data$Pathogen <- as.factor(experimental_data$Pathogen)

#remove outliers and missing values from the training data
data <- outliermissingvalues(training_data, experimental_data)

#use the training data to standardize both training and testing data
data <- standardize(data$trainset, data$testset)

#use the syngen function to generate oversampled training set
oversampled_training <- syngen(data$trainset, "Pathogen", c(1), K=3, C=3)
#from here on, we are now done with any oversampling and just onto training 
#and testing

#Use the oversampled_training data to test the data against the true data
log_outputs <- logistic_model(oversampled_training, data$testset, 5, c(1, 2, 3, 4),
                              cutoff = 0.5)

#roc plot for each log_output
#smote
roc.info <- pROC::roc(data$testset$Salmonella, log_outputs$smote_prob, plot = TRUE, main = "Logistic Regression SMOTE ROC", 
                      print.auc = TRUE, percent = TRUE, col = "#027FB9", asp = NA, grid = TRUE)
roc.df <- data.frame(
  tpp = roc.info$sensitivities,
  fpp = roc.info$specificities,
  thresholds = roc.info$thresholds)
roc.df

#make confusion matrix for SMOTE
table(log_outputs$smote_predict, data$testset$Salmonella)

#adasyn
roc.info <- pROC::roc(data$testset$Salmonella, log_outputs$adasyn_prob, plot = TRUE, main = "Logistic Regression ADASYN ROC",
                      percent = TRUE, print.auc = TRUE, col = "black", asp = NA, grid = TRUE)
roc.df <- data.frame(
  tpp = roc.info$sensitivities,
  fpp = roc.info$specificities,
  thresholds = roc.info$thresholds)
roc.df

#make confusion matrix for adasyn
table(log_outputs$adasyn_predict, data$testset$Salmonella)

#SL-SMOTE
roc.info <- pROC::roc(data$testset$Salmonella, log_outputs$slsmote_prob, plot = TRUE, main = "Logistic Regression SL-SMOTE ROC",
                      percent = TRUE, print.auc = TRUE, col = "black", asp = NA, grid = TRUE)
roc.df <- data.frame(
  tpp = roc.info$sensitivities,
  fpp = roc.info$specificities,
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
origin_predict <- ifelse(origin_prob > 4.755040e-02, 1, 0)

roc.info <- pROC::roc(data$testset$Salmonella, origin_prob, plot = TRUE, main = "Logistic Regression Non-Oversampled ROC",
                      percent = TRUE, print.auc = TRUE, col = "black", asp = NA, grid = TRUE)
roc.df <- data.frame(
  tpp = roc.info$sensitivities,
  fpp = roc.info$specificities,
  thresholds = roc.info$thresholds)
roc.df

#make confusion matrix for SL-SMOTE
table(origin_predict, data$testset$Salmonella)
