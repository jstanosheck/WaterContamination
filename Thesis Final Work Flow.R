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
#The purpose of this is to identify the proper cutoff point for each of the 
#three different oversampling methods
########################################################
#Logistic model using non-oversampled data
train_control <- caret::trainControl(method = 'cv', number = 10)

#use original data set
origin_model <- caret::train(data$trainset[, 2:5],
                             y = data$trainset[, 1],
                             method = 'glm',
                             trControl = train_control,
                             family = binomial())

#use origin model to predict on the test data
origin_prob <- predict(origin_model$finalModel, newdata = data$testset, type = 'response')


#Logistic model using the oversampled data
log_outputs <- logistic_model(oversampled, data$testset, 5, c(1, 2, 3, 4),
                              cutoff = c(0.5, 0.5, 0.5))

#Plot all of the logistic regression training ROC curves
smote.roc <- pROC::roc(data$testset$Pathogen, log_outputs$smote_prob, plot = TRUE, main = "Training Logistic Regression ROC", 
                       print.auc = TRUE, percent = TRUE, col = "#2e3b5e", asp = NA, grid = TRUE, lwd = 2)
adasyn.roc <- pROC::roc(data$testset$Pathogen, log_outputs$adasyn_prob, plot = TRUE,
                        percent = TRUE, print.auc = TRUE, col = "#2697a7", add = TRUE, print.auc.y = 40, lwd = 2)
slsmote.roc <- pROC::roc(data$testset$Pathogen, log_outputs$slsmote_prob, plot = TRUE,
                         percent = TRUE, print.auc = TRUE, col = "#aaaaaa", add = TRUE, print.auc.y = 30, lwd = 2)
origin.roc <- pROC::roc(data$testset$Pathogen, origin_prob, plot = TRUE, add = TRUE, percent = TRUE,
                        print.auc = TRUE, col = "#a97f69", print.auc.y = 20, lwd = 2)
legend("bottom", legend = c("SMOTE", "ADASYN", "SL-SMOTE", "Original Data"),
       col = c("#2e3b5e", "#2697a7", "#aaaaaa", "#a97f69"),
       lwd = 4, cex = 1, xpd = TRUE, horiz = TRUE)

#Generate all of the roc data frames then evaluate and determine the threshold 
#that has a good balance of true positive (miximize) and false positive (min)
smote.roc.df <- data.frame(tpp = smote.roc$sensitivities, fpp = 100 - smote.roc$specificities, thresholds = smote.roc$thresholds)
smote.roc.df #either 0.504980432 or 0.551686000

adasyn.roc.df <- data.frame(tpp = adasyn.roc$sensitivities, fpp = 100 - adasyn.roc$specificities, thresholds = adasyn.roc$thresholds)
adasyn.roc.df #either 0.50359430 or 0.56008299

slsmote.roc.df <- data.frame( tpp = 100 - slsmote.roc$sensitivities, fpp = slsmote.roc$specificities, thresholds = slsmote.roc$thresholds)
slsmote.roc.df #0.15989118

origin.roc.df <- data.frame( tpp = origin.roc$sensitivities, fpp = 100 - origin.roc$specificities, thresholds = origin.roc$thresholds)
origin.roc.df #either 0.0512286011 or 0.0453628628
########################################################
#Based on the ideal threshold that is decided, insert that into the 
#logistic model function below for each of the three oversampled data sets
#and for the original data and report the confusion matrix.
#SMOTE Cutoff = 0.495433852
#ADASYN Cutoff = 0.49728109
#SLSMOTE Cutoff = 0.15989118
#Original Cutoff = 0.0442431825
########################################################
log_outputs <- logistic_model(oversampled, data$testset, 5, c(1, 2, 3, 4),
                              cutoff = c(0.495433852, 0.49728109, 0.15989118))

smote.table <- table(log_outputs$smote_predict, data$testset$Pathogen)
smote.table
#Confusion Matrix output
#  0.495433852
#       0  1
#   0  63  2 
#   1  38  5
adasyn.table <- table(log_outputs$adasyn_predict, data$testset$Pathogen)
adasyn.table
#Confusion Matrix output
#  0.49728109
#        0  1
#   0   60  2
#   1   41  5
slsmote.table <- table(log_outputs$slsmote_predict, data$testset$Pathogen)
slsmote.table
#  0.15989118
#       0  1
#   0  31  2
#   1  70  5
origin_predict <- ifelse(origin_prob > 0.0442431825, 1, 0)
table(origin_predict, data$testset$Pathogen)
#Confusion Matrix output
#  0.0442431825
#       0  1
#   0  61  2
#   1  40  5

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
combined.data <- outliermissingvalues(training_data, experimental_data)

#use the training data to standardize both training and testing data
combined.data <- standardize(combined.data$trainset, combined.data$testset)

#use the syngen function to generate oversampled training set
oversampled_training <- syngen(combined.data$trainset, "Pathogen", c(1), K=3, C=3)
#from here on, we are now done with any oversampling and just onto training 
#and testing

#Logistic model using non-oversampled data
train_control <- caret::trainControl(method = 'cv', number = 10)

#use original data set
origin_model_final <- caret::train(combined.data$trainset[, 2:5],
                             y = combined.data$trainset[, 1],
                             method = 'glm',
                             trControl = train_control,
                             family = binomial())

#use origin model to predict on the test data
origin_prob_final <- predict(origin_model_final$finalModel, newdata = combined.data$testset, type = 'response')


#Logistic model using the oversampled data
log_outputs_final <- logistic_model(oversampled_training, combined.data$testset, 5, c(1, 2, 3, 4),
                              cutoff = c(0.495433852, 0.49728109, 0.15989118))

#Plot all of the logistic regression training ROC curves
smote.final.roc <- pROC::roc(combined.data$testset$Pathogen, log_outputs_final$smote_prob, plot = TRUE,
                       main = "Logistic Regression ROC Using Experimental Test Data", 
                       print.auc = TRUE, percent = TRUE, col = "#2e3b5e", asp = NA, grid = TRUE, lwd = 2)
adasyn.final.roc <- pROC::roc(combined.data$testset$Pathogen, log_outputs_final$adasyn_prob, plot = TRUE,
                        percent = TRUE, print.auc = TRUE, col = "#2697a7", add = TRUE, print.auc.y = 40, lwd = 2)
slsmote.final.roc <- pROC::roc(combined.data$testset$Pathogen, log_outputs_final$slsmote_prob, plot = TRUE,
                         percent = TRUE, print.auc = TRUE, col = "#aaaaaa", add = TRUE, print.auc.y = 30, lwd = 2)
origin.final.roc <- pROC::roc(combined.data$testset$Pathogen, origin_prob_final, plot = TRUE, add = TRUE, percent = TRUE,
                        print.auc = TRUE, col = "#a97f69", print.auc.y = 20, lwd = 2)
legend("bottom", legend = c("SMOTE", "ADASYN", "SL-SMOTE", "Original Data"),
       col = c("#2e3b5e", "#2697a7", "#aaaaaa", "#a97f69"),
       lwd = 4, cex = 1, xpd = TRUE, horiz = TRUE)

#Confusion matrix for the experimental data with the final cutoff points
smote.table <- table(log_outputs_final$smote_predict, combined.data$testset$Pathogen)
smote.table
#Confusion Matrix output
#  0.495433852
#       0   1
#   0  13  12 
#   1  11   4
adasyn.table <- table(log_outputs_final$adasyn_predict, combined.data$testset$Pathogen)
adasyn.table
#Confusion Matrix output
#  0.49728109
#        0   1
#   0   14  12
#   1   10   4
slsmote.table <- table(log_outputs_final$slsmote_predict, combined.data$testset$Pathogen)
slsmote.table
#  0.15989118
#       0   1
#   0  24  15
#   1   0   1
origin_predict_final <- ifelse(origin_prob_final > 0.0442431825, 1, 0)
table(origin_predict_final, combined.data$testset$Pathogen)
#Confusion Matrix output
#  0.0442431825
#       0  1
#   0   6  9
#   1  18  7