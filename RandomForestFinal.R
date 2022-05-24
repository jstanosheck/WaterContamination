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

#Use the "make.names()" function to set the pathogen feature as something that 
#can be classified by the random forest function
oversampled$smote$data$class <- make.names(oversampled$smote$data$class)
oversampled$adasyn$data$class <- make.names(oversampled$adasyn$data$class)
oversampled$slsmote$data$class <- make.names(oversampled$slsmote$data$class)
data$testset$Pathogen <- make.names(data$testset$Pathogen)
data$trainset$Pathogen <- make.names(data$trainset$Pathogen)


#Train the random forest models here using the oversampling methods. 
########################################################

#This must repeat for ntre = c(300, 500, 600, 700, 1000)

#ntree = 300
#SMOTE mtry = 1
#ADASYN mtry = 2
#SL-SMOTE mtry = 1
#Original mtry = 1
########################################################
#use forest_model function on the train and test data
#original data forest model
forest_grid <- expand.grid(mtry = c(1:4))

#set train control method
train_control <- caret::trainControl(method = 'cv', number = 10,
                                     classProbs = TRUE, savePredictions = "final",
                                     summaryFunction = twoClassSummary)

#rf training on the traindata set
origin_forest <- caret::train(data$trainset[, c(2, 3, 4, 5)], y = data$trainset[, 1],
                              method = 'rf', trControl = train_control, tuneGrid = forest_grid,
                              metric = 'ROC', ntree = 300)

#predict using the test data set
origin_forest_prob <- predict(origin_forest$finalModel, newdata = data$testset, type = "prob")

forest_output <- forest_model(oversampled, data$testset, 5, c(1, 2, 3, 4),
                              mtry = c(1:4), ntree = 300, cutoff = c(0.5, 0.5, 0.5))

#Plot all of the logistic regression training ROC curves
smote.forest.roc <- pROC::roc(data$testset$Pathogen, forest_output$smote_prob[, 2], plot = TRUE,
                              main = "Random Forest ROC Using Training 300 Trees", 
                              print.auc = TRUE, percent = TRUE, col = "#2e3b5e", asp = NA, grid = TRUE, lwd = 2)
adasyn.forest.roc <- pROC::roc(data$testset$Pathogen, forest_output$adasyn_prob[, 2], plot = TRUE,
                               percent = TRUE, print.auc = TRUE, col = "#2697a7", add = TRUE, print.auc.y = 40, lwd = 2)
slsmote.forest.roc <- pROC::roc(data$testset$Pathogen, forest_output$slsmote_prob[, 2], plot = TRUE,
                                percent = TRUE, print.auc = TRUE, col = "#aaaaaa", add = TRUE, print.auc.y = 30, lwd = 2)
origin.forest.roc <- pROC::roc(data$testset$Pathogen, origin_forest_prob[, 2], plot = TRUE, add = TRUE, percent = TRUE,
                               print.auc = TRUE, col = "#a97f69", print.auc.y = 20, lwd = 2)
legend("bottomright", legend = c("SMOTE", "ADASYN", "SL-SMOTE", "Original Data"),
       col = c("#2e3b5e", "#2697a7", "#aaaaaa", "#a97f69"),
       lwd = 4, cex = 1, xpd = TRUE, horiz = FALSE)
########################################################

#Generate all of the roc data frames then evaluate and determine the threshold 
########################################################
#that has a good balance of true positive (miximize) and false positive (min)
smote.forest.roc.df <- data.frame(tpp = smote.forest.roc$sensitivities, fpp = 100 - smote.forest.roc$specificities, thresholds = smote.forest.roc$thresholds)
smote.forest.roc.df #either 0.16833333

adasyn.forest.roc.df <- data.frame(tpp = adasyn.forest.roc$sensitivities, fpp = 100 - adasyn.forest.roc$specificities, thresholds = adasyn.forest.roc$thresholds)
adasyn.forest.roc.df #either 0.136666667

slsmote.forest.roc.df <- data.frame( tpp = 100 - slsmote.forest.roc$sensitivities, fpp = slsmote.forest.roc$specificities, thresholds = slsmote.forest.roc$thresholds)
slsmote.forest.roc.df #either 0.015000000

origin.forest.roc.df <- data.frame( tpp = origin.forest.roc$sensitivities, fpp = 100 - origin.forest.roc$specificities, thresholds = origin.forest.roc$thresholds)
origin.forest.roc.df #either 0.018333333
########################################################

#This is where we use the cutoff values to make predictions about the model accuracy
########################################################
#input the cutoff values for each of the models here
forest_output <- forest_model(oversampled, data$testset, 5, c(1, 2, 3, 4),
                              mtry = c(1:4), ntree = 300, cutoff = c(0.16833333, 0.136666667, 0.015000000))

#SMOTE
#make table for smote prediction
table(forest_output$smote_predict, data$testset$Pathogen)
#Confusion Matrix output
#     0.16833333
#        0  1 
#     0 57  3
#     1 44  4
#     mtry = 1
forest_output$smote_model$bestTune

#ADASYN
#make table for adasyn prediction
table(forest_output$adasyn_predict, data$testset$Pathogen)
#Confusion Matrix output
#    0.136666667
#     0  1
#  0 55  3
#  1 46  4
#    mtry = 2
forest_output$adasyn_model$bestTune

#SL-SMOTE
#make table for slsmote prediction
table(forest_output$slsmote_predict, data$testset$Pathogen)
#Confusion Matrix output
#     0.015
#      0  1  
#   0 40  3   
#   1 61  4   
#  mtry = 1    
forest_output$slsmote_model$bestTune

#Original data to train the model
origin_forest_predict <- ifelse(origin_forest_prob[, 2] > 0.018333333, 1, 0)
#make table for original prediction
table(origin_forest_predict, data$testset$Pathogen)
#Confusion Matrix output
#    0.018333333
#     0  1 
#  0 55  2
#  1 46  5 
#  mtry = 1 
origin_forest$bestTune
########################################################

#ntree = 500
#SMOTE mtry = 1
#ADASYN mtry = 1
#SL-SMOTE mtry = 1
#Original mtry = 1
########################################################
#use forest_model function on the train and test data
#original data forest model
forest_grid <- expand.grid(mtry = c(1:4))

#set train control method
train_control <- caret::trainControl(method = 'cv', number = 10,
                                     classProbs = TRUE, savePredictions = "final",
                                     summaryFunction = twoClassSummary)

#rf training on the traindata set
origin_forest <- caret::train(data$trainset[, c(2, 3, 4, 5)], y = data$trainset[, 1],
                              method = 'rf', trControl = train_control, tuneGrid = forest_grid,
                              metric = 'ROC', ntree = 500)

#predict using the test data set
origin_forest_prob <- predict(origin_forest$finalModel, newdata = data$testset, type = "prob")

forest_output <- forest_model(oversampled, data$testset, 5, c(1, 2, 3, 4),
                              mtry = c(1:4), ntree = 500, cutoff = c(0.5, 0.5, 0.5))

#Plot all of the logistic regression training ROC curves
smote.forest.roc <- pROC::roc(data$testset$Pathogen, forest_output$smote_prob[, 2], plot = TRUE,
                              main = "Random Forest ROC Using Training 500 Trees", 
                              print.auc = TRUE, percent = TRUE, col = "#2e3b5e", asp = NA, grid = TRUE, lwd = 2)
adasyn.forest.roc <- pROC::roc(data$testset$Pathogen, forest_output$adasyn_prob[, 2], plot = TRUE,
                               percent = TRUE, print.auc = TRUE, col = "#2697a7", add = TRUE, print.auc.y = 40, lwd = 2)
slsmote.forest.roc <- pROC::roc(data$testset$Pathogen, forest_output$slsmote_prob[, 2], plot = TRUE,
                                percent = TRUE, print.auc = TRUE, col = "#aaaaaa", add = TRUE, print.auc.y = 30, lwd = 2)
origin.forest.roc <- pROC::roc(data$testset$Pathogen, origin_forest_prob[, 2], plot = TRUE, add = TRUE, percent = TRUE,
                               print.auc = TRUE, col = "#a97f69", print.auc.y = 20, lwd = 2)
legend("bottomright", legend = c("SMOTE", "ADASYN", "SL-SMOTE", "Original Data"),
       col = c("#2e3b5e", "#2697a7", "#aaaaaa", "#a97f69"),
       lwd = 4, cex = 1, xpd = TRUE, horiz = FALSE)
########################################################

#Generate all of the roc data frames then evaluate and determine the threshold 
########################################################
#that has a good balance of true positive (miximize) and false positive (min)
smote.forest.roc.df <- data.frame(tpp = smote.forest.roc$sensitivities, fpp = 100 - smote.forest.roc$specificities, thresholds = smote.forest.roc$thresholds)
smote.forest.roc.df #either 0.127

adasyn.forest.roc.df <- data.frame(tpp = adasyn.forest.roc$sensitivities, fpp = 100 - adasyn.forest.roc$specificities, thresholds = adasyn.forest.roc$thresholds)
adasyn.forest.roc.df #either 0.138

slsmote.forest.roc.df <- data.frame( tpp = 100 - slsmote.forest.roc$sensitivities, fpp = slsmote.forest.roc$specificities, thresholds = slsmote.forest.roc$thresholds)
slsmote.forest.roc.df #either 0.023

origin.forest.roc.df <- data.frame( tpp = origin.forest.roc$sensitivities, fpp = 100 - origin.forest.roc$specificities, thresholds = origin.forest.roc$thresholds)
origin.forest.roc.df #either 0.015
########################################################

#This is where we use the cutoff values to make predictions about the model accuracy
########################################################
#input the cutoff values for each of the models here
forest_output <- forest_model(oversampled, data$testset, 5, c(1, 2, 3, 4),
                              mtry = c(1:4), ntree = 500, cutoff = c(0.127, 0.137, 0.022))

#SMOTE
#make table for smote prediction
table(forest_output$smote_predict, data$testset$Pathogen)
#Confusion Matrix output
#     0.127
#        0  1 
#     0 40  3
#     1 61  4
#     mtry = 1
forest_output$smote_model$bestTune

#ADASYN
#make table for adasyn prediction
table(forest_output$adasyn_predict, data$testset$Pathogen)
#Confusion Matrix output
#    0.137
#     0  1
#  0 50  3
#  1 51  4
#    mtry = 1
forest_output$adasyn_model$bestTune

#SL-SMOTE
#make table for slsmote prediction
table(forest_output$slsmote_predict, data$testset$Pathogen)
#Confusion Matrix output
#     0.022
#      0  1  
#   0 45  4   
#   1 56  3   
#  mtry = 1    
forest_output$slsmote_model$bestTune

#Original data to train the model
origin_forest_predict <- ifelse(origin_forest_prob[, 2] > 0.015, 1, 0)
#make table for original prediction
table(origin_forest_predict, data$testset$Pathogen)
#Confusion Matrix output
#    0.015
#     0  1 
#  0 53  3
#  1 48  4 
#  mtry = 1 
origin_forest$bestTune
########################################################


#ntree = 600
#SMOTE mtry = 1
#ADASYN mtry = 1
#SL-SMOTE mtry = 1
#Original mtry = 4
########################################################
#use forest_model function on the train and test data
#original data forest model
forest_grid <- expand.grid(mtry = c(1:4))

#set train control method
train_control <- caret::trainControl(method = 'cv', number = 10,
                                     classProbs = TRUE, savePredictions = "final",
                                     summaryFunction = twoClassSummary)

#rf training on the traindata set
origin_forest <- caret::train(data$trainset[, c(2, 3, 4, 5)], y = data$trainset[, 1],
                              method = 'rf', trControl = train_control, tuneGrid = forest_grid,
                              metric = 'ROC', ntree = 600)

#predict using the test data set
origin_forest_prob <- predict(origin_forest$finalModel, newdata = data$testset, type = "prob")

forest_output <- forest_model(oversampled, data$testset, 5, c(1, 2, 3, 4),
                              mtry = c(1:4), ntree = 600, cutoff = c(0.5, 0.5, 0.5))

#Plot all of the logistic regression training ROC curves
smote.forest.roc <- pROC::roc(data$testset$Pathogen, forest_output$smote_prob[, 2], plot = TRUE,
                             main = "Random Forest ROC Using Training 600 Trees", 
                             print.auc = TRUE, percent = TRUE, col = "#2e3b5e", asp = NA, grid = TRUE, lwd = 2)
adasyn.forest.roc <- pROC::roc(data$testset$Pathogen, forest_output$adasyn_prob[, 2], plot = TRUE,
                              percent = TRUE, print.auc = TRUE, col = "#2697a7", add = TRUE, print.auc.y = 40, lwd = 2)
slsmote.forest.roc <- pROC::roc(data$testset$Pathogen, forest_output$slsmote_prob[, 2], plot = TRUE,
                               percent = TRUE, print.auc = TRUE, col = "#aaaaaa", add = TRUE, print.auc.y = 30, lwd = 2)
origin.forest.roc <- pROC::roc(data$testset$Pathogen, origin_forest_prob[, 2], plot = TRUE, add = TRUE, percent = TRUE,
                              print.auc = TRUE, col = "#a97f69", print.auc.y = 20, lwd = 2)
legend("bottomright", legend = c("SMOTE", "ADASYN", "SL-SMOTE", "Original Data"),
       col = c("#2e3b5e", "#2697a7", "#aaaaaa", "#a97f69"),
       lwd = 4, cex = 1, xpd = TRUE, horiz = FALSE)

#Generate all of the roc data frames then evaluate and determine the threshold 
########################################################
#that has a good balance of true positive (miximize) and false positive (min)
smote.forest.roc.df <- data.frame(tpp = smote.forest.roc$sensitivities, fpp = 100 - smote.forest.roc$specificities, thresholds = smote.forest.roc$thresholds)
smote.forest.roc.df #either 0.16166667 or 0.10166667

adasyn.forest.roc.df <- data.frame(tpp = adasyn.forest.roc$sensitivities, fpp = 100 - adasyn.forest.roc$specificities, thresholds = adasyn.forest.roc$thresholds)
adasyn.forest.roc.df #either 0.075833333 or 0.154166667

slsmote.forest.roc.df <- data.frame( tpp = 100 - slsmote.forest.roc$sensitivities, fpp = slsmote.forest.roc$specificities, thresholds = slsmote.forest.roc$thresholds)
slsmote.forest.roc.df #either 0.0233333333 or 0.0308333333 or 0.2775000000

origin.forest.roc.df <- data.frame( tpp = origin.forest.roc$sensitivities, fpp = 100 - origin.forest.roc$specificities, thresholds = origin.forest.roc$thresholds)
origin.forest.roc.df #either 0.0008333333 or 0.0441666667 or 0.4183333333
########################################################

#This is where we use the cutoff values to make predictions about the model accuracy
########################################################
#input the cutoff values for each of the models here
forest_output <- forest_model(oversampled, data$testset, 5, c(1, 2, 3, 4),
                              mtry = c(1:4), ntree = 600, cutoff = c(0.10166667, 0.154166667, 0.0233333333))

#SMOTE
#make table for smote prediction
table(forest_output$smote_predict, data$testset$Pathogen)
#Confusion Matrix output
#  0.16166667 
#      0  1 
#   0 51  3 
#   1 50  4
#  mtry = 1 
forest_output$smote_model$bestTune

#ADASYN
#make table for adasyn prediction
table(forest_output$adasyn_predict, data$testset$Pathogen)
#Confusion Matrix output
#  0.154166667
#      0  1
#   0 56  4 
#   1 45  3
#   mtry = 1
forest_output$adasyn_model$bestTune

#SL-SMOTE
#make table for slsmote prediction
table(forest_output$slsmote_predict, data$testset$Pathogen)
#Confusion Matrix output
#  0.0233333333
#      0  1
#   0 46  4
#   1 55  3
#  mtry = 1
forest_output$slsmote_model$bestTune

#Original data to train the model
origin_forest_predict <- ifelse(origin_forest_prob[, 2] > 0.4183333333, 1, 0)
#make table for original prediction
table(origin_forest_predict, data$testset$Pathogen)
#Confusion Matrix output
#  0.0441666667
#      0  1
#   0 73  5
#   1 28  2
#  mtry = 4 
origin_forest$bestTune
########################################################

#ntree = 700
#SMOTE mtry = 1
#ADASYN mtry = 1
#SL-SMOTE mtry = 1
#Original mtry = 1
########################################################
#use forest_model function on the train and test data
#original data forest model
forest_grid <- expand.grid(mtry = c(1:4))

#set train control method
train_control <- caret::trainControl(method = 'cv', number = 10,
                                     classProbs = TRUE, savePredictions = "final",
                                     summaryFunction = twoClassSummary)

#rf training on the traindata set
origin_forest <- caret::train(data$trainset[, c(2, 3, 4, 5)], y = data$trainset[, 1],
                              method = 'rf', trControl = train_control, tuneGrid = forest_grid,
                              metric = 'ROC', ntree = 700)

#predict using the test data set
origin_forest_prob <- predict(origin_forest$finalModel, newdata = data$testset, type = "prob")

forest_output <- forest_model(oversampled, data$testset, 5, c(1, 2, 3, 4),
                              mtry = c(1:4), ntree = 700, cutoff = c(0.5, 0.5, 0.5))

#Plot all of the logistic regression training ROC curves
smote.forest.roc <- pROC::roc(data$testset$Pathogen, forest_output$smote_prob[, 2], plot = TRUE,
                              main = "Random Forest ROC Using Training 700 Trees", 
                              print.auc = TRUE, percent = TRUE, col = "#2e3b5e", asp = NA, grid = TRUE, lwd = 2)
adasyn.forest.roc <- pROC::roc(data$testset$Pathogen, forest_output$adasyn_prob[, 2], plot = TRUE,
                               percent = TRUE, print.auc = TRUE, col = "#2697a7", add = TRUE, print.auc.y = 40, lwd = 2)
slsmote.forest.roc <- pROC::roc(data$testset$Pathogen, forest_output$slsmote_prob[, 2], plot = TRUE,
                                percent = TRUE, print.auc = TRUE, col = "#aaaaaa", add = TRUE, print.auc.y = 30, lwd = 2)
origin.forest.roc <- pROC::roc(data$testset$Pathogen, origin_forest_prob[, 2], plot = TRUE, add = TRUE, percent = TRUE,
                               print.auc = TRUE, col = "#a97f69", print.auc.y = 20, lwd = 2)
legend("bottomright", legend = c("SMOTE", "ADASYN", "SL-SMOTE", "Original Data"),
       col = c("#2e3b5e", "#2697a7", "#aaaaaa", "#a97f69"),
       lwd = 4, cex = 1, xpd = TRUE, horiz = FALSE)
########################################################

#Generate all of the roc data frames then evaluate and determine the threshold 
########################################################
#that has a good balance of true positive (miximize) and false positive (min)
smote.forest.roc.df <- data.frame(tpp = smote.forest.roc$sensitivities, fpp = 100 - smote.forest.roc$specificities, thresholds = smote.forest.roc$thresholds)
smote.forest.roc.df #either 0.075000000 or 0.172142857

adasyn.forest.roc.df <- data.frame(tpp = adasyn.forest.roc$sensitivities, fpp = 100 - adasyn.forest.roc$specificities, thresholds = adasyn.forest.roc$thresholds)
adasyn.forest.roc.df #either 0.0807142857 or 0.1778571429

slsmote.forest.roc.df <- data.frame( tpp = 100 - slsmote.forest.roc$sensitivities, fpp = slsmote.forest.roc$specificities, thresholds = slsmote.forest.roc$thresholds)
slsmote.forest.roc.df #either 0.0135714286 or 0.0264285714

origin.forest.roc.df <- data.frame( tpp = origin.forest.roc$sensitivities, fpp = 100 - origin.forest.roc$specificities, thresholds = origin.forest.roc$thresholds)
origin.forest.roc.df #either 0.0135714286 or 0.0185714286
########################################################

#This is where we use the cutoff values to make predictions about the model accuracy
########################################################
#input the cutoff values for each of the models here
forest_output <- forest_model(oversampled, data$testset, 5, c(1, 2, 3, 4),
                              mtry = c(1:4), ntree = 700, cutoff = c(0.075, 0.1778571429, 0.0135714286))

#SMOTE
#make table for smote prediction
table(forest_output$smote_predict, data$testset$Pathogen)
#Confusion Matrix output
#     0.075
#      0  1 
#   0 34  0 
#   1 67  7
#  mtry = 1
forest_output$smote_model$bestTune

#ADASYN
#make table for adasyn prediction
table(forest_output$adasyn_predict, data$testset$Pathogen)
#Confusion Matrix output
#  0.1778571429
#     0  1
#  0 59  3 
#  1 42  4
#   mtry = 1
forest_output$adasyn_model$bestTune

#SL-SMOTE
#make table for slsmote prediction
table(forest_output$slsmote_predict, data$testset$Pathogen)
#Confusion Matrix output
#  0.0135714286
#      0  1 
#   0 38  3
#   1 63  4
#  mtry = 1 
forest_output$slsmote_model$bestTune

#Original data to train the model
origin_forest_predict <- ifelse(origin_forest_prob[, 2] > 0.0135714286, 1, 0)
#make table for original prediction
table(origin_forest_predict, data$testset$Pathogen)
#Confusion Matrix output
#  0.0135714286  
#      0  1    
#   0 49  1  
#   1 52  6   
#  mtry = 1 
origin_forest$bestTune
########################################################

#ntree = 1000
#SMOTE mtry = 1
#ADASYN mtry = 1
#SL-SMOTE mtry = 1
#Original mtry = 1
########################################################
#use forest_model function on the train and test data
#original data forest model
forest_grid <- expand.grid(mtry = c(1:4))

#set train control method
train_control <- caret::trainControl(method = 'cv', number = 10,
                                     classProbs = TRUE, savePredictions = "final",
                                     summaryFunction = twoClassSummary)

#rf training on the traindata set
origin_forest <- caret::train(data$trainset[, c(2, 3, 4, 5)], y = data$trainset[, 1],
                              method = 'rf', trControl = train_control, tuneGrid = forest_grid,
                              metric = 'ROC', ntree = 1000)

#predict using the test data set
origin_forest_prob <- predict(origin_forest$finalModel, newdata = data$testset, type = "prob")

forest_output <- forest_model(oversampled, data$testset, 5, c(1, 2, 3, 4),
                              mtry = c(1:4), ntree = 1000, cutoff = c(0.5, 0.5, 0.5))

#Plot all of the logistic regression training ROC curves
smote.forest.roc <- pROC::roc(data$testset$Pathogen, forest_output$smote_prob[, 2], plot = TRUE,
                              main = "Random Forest ROC Using Training 1000 Trees", 
                              print.auc = TRUE, percent = TRUE, col = "#2e3b5e", asp = NA, grid = TRUE, lwd = 2)
adasyn.forest.roc <- pROC::roc(data$testset$Pathogen, forest_output$adasyn_prob[, 2], plot = TRUE,
                               percent = TRUE, print.auc = TRUE, col = "#2697a7", add = TRUE, print.auc.y = 40, lwd = 2)
slsmote.forest.roc <- pROC::roc(data$testset$Pathogen, forest_output$slsmote_prob[, 2], plot = TRUE,
                                percent = TRUE, print.auc = TRUE, col = "#aaaaaa", add = TRUE, print.auc.y = 30, lwd = 2)
origin.forest.roc <- pROC::roc(data$testset$Pathogen, origin_forest_prob[, 2], plot = TRUE, add = TRUE, percent = TRUE,
                               print.auc = TRUE, col = "#a97f69", print.auc.y = 20, lwd = 2)
legend("bottomright", legend = c("SMOTE", "ADASYN", "SL-SMOTE", "Original Data"),
       col = c("#2e3b5e", "#2697a7", "#aaaaaa", "#a97f69"),
       lwd = 4, cex = 1, xpd = TRUE, horiz = FALSE)
########################################################

#Generate all of the roc data frames then evaluate and determine the threshold 
########################################################
#that has a good balance of true positive (miximize) and false positive (min)
smote.forest.roc.df <- data.frame(tpp = smote.forest.roc$sensitivities, fpp = 100 - smote.forest.roc$specificities, thresholds = smote.forest.roc$thresholds)
smote.forest.roc.df #either 0.1640 or 0.0895

adasyn.forest.roc.df <- data.frame(tpp = adasyn.forest.roc$sensitivities, fpp = 100 - adasyn.forest.roc$specificities, thresholds = adasyn.forest.roc$thresholds)
adasyn.forest.roc.df #either 0.2345 or 0.1615

slsmote.forest.roc.df <- data.frame( tpp = 100 - slsmote.forest.roc$sensitivities, fpp = slsmote.forest.roc$specificities, thresholds = slsmote.forest.roc$thresholds)
slsmote.forest.roc.df #either 0.0180 or 0.0255

origin.forest.roc.df <- data.frame( tpp = origin.forest.roc$sensitivities, fpp = 100 - origin.forest.roc$specificities, thresholds = origin.forest.roc$thresholds)
origin.forest.roc.df #either 0.0235 or 0.0155
########################################################

#This is where we use the cutoff values to make predictions about the model accuracy
########################################################
#input the cutoff values for each of the models here
forest_output <- forest_model(oversampled, data$testset, 5, c(1, 2, 3, 4),
                              mtry = c(1:4), ntree = 1000, cutoff = c(0.0895, 0.1615, 0.0255))

#SMOTE
#make table for smote prediction
table(forest_output$smote_predict, data$testset$Pathogen)
#Confusion Matrix output
#     0.0895
#        0  1 
#     0 35  2
#     1 66  5
#     mtry = 1
forest_output$smote_model$bestTune

#ADASYN
#make table for adasyn prediction
table(forest_output$adasyn_predict, data$testset$Pathogen)
#Confusion Matrix output
#    0.1615
#     0  1
#  0 57  3
#  1 44  4
#    mtry = 1
forest_output$adasyn_model$bestTune

#SL-SMOTE
#make table for slsmote prediction
table(forest_output$slsmote_predict, data$testset$Pathogen)
#Confusion Matrix output
#  0.0255
#      0  1  
#   0 48  4   
#   1 53  3   
#  mtry = 1    
forest_output$slsmote_model$bestTune

#Original data to train the model
origin_forest_predict <- ifelse(origin_forest_prob[, 2] > 0.0155, 1, 0)
#make table for original prediction
table(origin_forest_predict, data$testset$Pathogen)
#Confusion Matrix output
#    0.0155
#     0  1 
#  0 47  4
#  1 54  3 
#  mtry = 1 
origin_forest$bestTune
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

#make all of the "pathogen" variables make.names()
oversampled_training$smote$data$class <- make.names(oversampled_training$smote$data$class)
oversampled_training$adasyn$data$class <- make.names(oversampled_training$adasyn$data$class)
oversampled_training$slsmote$data$class <- make.names(oversampled_training$slsmote$data$class)
combined.data$trainset$Pathogen <- make.names(combined.data$trainset$Pathogen)
combined.data$testset$Pathogen <- make.names(combined.data$testset$Pathogen)

#use forest_model function on the train and test data
#original data forest model
forest_grid <- expand.grid(mtry = c(1:4))

#set train control method
train_control <- caret::trainControl(method = 'cv', number = 10,
                                     classProbs = TRUE, savePredictions = "final",
                                     summaryFunction = twoClassSummary)
########################################################

#ntree = 300
########################################################
#rf training on the traindata set
origin_forest_final <- caret::train(combined.data$trainset[, c(2, 3, 4, 5)], y = combined.data$trainset[, 1],
                              method = 'rf', trControl = train_control, tuneGrid = forest_grid,
                              metric = 'ROC', ntree = 300)

#predict using the test data set
origin_forest_prob <- predict(origin_forest_final$finalModel, newdata = combined.data$testset, type = "prob")

forest_output <- forest_model(oversampled_training, combined.data$testset, 5, c(1, 2, 3, 4),
                              mtry = c(1:4), ntree = 300, cutoff = c(0.16833333, 0.136666667, 0.015000000))

#Plot all of the logistic regression training ROC curves
smote.forest.roc <- pROC::roc(combined.data$testset$Pathogen, forest_output$smote_prob[, 2], plot = TRUE,
                              main = "300 Trees Random Forest ROC Using Experimental Data", 
                              print.auc = TRUE, percent = TRUE, col = "#2e3b5e", asp = NA, grid = TRUE, lwd = 2)
adasyn.forest.roc <- pROC::roc(combined.data$testset$Pathogen, forest_output$adasyn_prob[, 2], plot = TRUE,
                               percent = TRUE, print.auc = TRUE, col = "#2697a7", add = TRUE, print.auc.y = 40, lwd = 2)
slsmote.forest.roc <- pROC::roc(combined.data$testset$Pathogen, forest_output$slsmote_prob[, 2], plot = TRUE,
                                percent = TRUE, print.auc = TRUE, col = "#aaaaaa", add = TRUE, print.auc.y = 30, lwd = 2)
origin.forest.roc <- pROC::roc(combined.data$testset$Pathogen, origin_forest_prob[, 2], plot = TRUE, add = TRUE, percent = TRUE,
                               print.auc = TRUE, col = "#a97f69", print.auc.y = 20, lwd = 2)
legend("bottomright", legend = c("SMOTE", "ADASYN", "SL-SMOTE", "Original Data"),
       col = c("#2e3b5e", "#2697a7", "#aaaaaa", "#a97f69"),
       lwd = 4, cex = 1, xpd = TRUE, horiz = FALSE)

#SMOTE
#make table for smote prediction
table(forest_output$smote_predict, combined.data$testset$Pathogen)
#Confusion Matrix output
#     0.16833333
#        0  1 
#     0 19 12
#     1  5  4

#ADASYN
#make table for adasyn prediction
table(forest_output$adasyn_predict, combined.data$testset$Pathogen)
#Confusion Matrix output
#    0.136666667
#     0  1
#  0 19 11
#  1  5  5

#SL-SMOTE
#make table for slsmote prediction
table(forest_output$slsmote_predict, combined.data$testset$Pathogen)
#Confusion Matrix output
#     0.015
#      0  1  
#   0 14 12   
#   1 10  4   

#Original data to train the model
origin_forest_predict <- ifelse(origin_forest_prob[, 2] > 0.018333333, 1, 0)
#make table for original prediction
table(origin_forest_predict, combined.data$testset$Pathogen)
#Confusion Matrix output
#    0.0155
#     0  1 
#  0 14 11
#  1 10  5 
########################################################

#ntree = 500
########################################################
#rf training on the traindata set
origin_forest_final <- caret::train(combined.data$trainset[, c(2, 3, 4, 5)], y = combined.data$trainset[, 1],
                                    method = 'rf', trControl = train_control, tuneGrid = forest_grid,
                                    metric = 'ROC', ntree = 500)

#predict using the test data set
origin_forest_prob <- predict(origin_forest_final$finalModel, newdata = combined.data$testset, type = "prob")

forest_output <- forest_model(oversampled_training, combined.data$testset, 5, c(1, 2, 3, 4),
                              mtry = c(1:4), ntree = 500, cutoff = c(0.127, 0.137, 0.022))

#Plot all of the logistic regression training ROC curves
smote.forest.roc <- pROC::roc(combined.data$testset$Pathogen, forest_output$smote_prob[, 2], plot = TRUE,
                              main = "500 Trees Random Forest ROC Using Experimental Data", 
                              print.auc = TRUE, percent = TRUE, col = "#2e3b5e", asp = NA, grid = TRUE, lwd = 2)
adasyn.forest.roc <- pROC::roc(combined.data$testset$Pathogen, forest_output$adasyn_prob[, 2], plot = TRUE,
                               percent = TRUE, print.auc = TRUE, col = "#2697a7", add = TRUE, print.auc.y = 40, lwd = 2)
slsmote.forest.roc <- pROC::roc(combined.data$testset$Pathogen, forest_output$slsmote_prob[, 2], plot = TRUE,
                                percent = TRUE, print.auc = TRUE, col = "#aaaaaa", add = TRUE, print.auc.y = 30, lwd = 2)
origin.forest.roc <- pROC::roc(combined.data$testset$Pathogen, origin_forest_prob[, 2], plot = TRUE, add = TRUE, percent = TRUE,
                               print.auc = TRUE, col = "#a97f69", print.auc.y = 20, lwd = 2)
legend("bottomright", legend = c("SMOTE", "ADASYN", "SL-SMOTE", "Original Data"),
       col = c("#2e3b5e", "#2697a7", "#aaaaaa", "#a97f69"),
       lwd = 4, cex = 1, xpd = TRUE, horiz = FALSE)

#SMOTE
#make table for smote prediction
table(forest_output$smote_predict, combined.data$testset$Pathogen)
#Confusion Matrix output
#     0.127
#        0  1 
#     0 19 12
#     1  5  4

#ADASYN
#make table for adasyn prediction
table(forest_output$adasyn_predict, combined.data$testset$Pathogen)
#Confusion Matrix output
#    0.137
#     0  1
#  0 19 11
#  1  5  5

#SL-SMOTE
#make table for slsmote prediction
table(forest_output$slsmote_predict, combined.data$testset$Pathogen)
#Confusion Matrix output
#     0.022
#      0  1  
#   0  4 10   
#   1 20  6   

#Original data to train the model
origin_forest_predict <- ifelse(origin_forest_prob[, 2] > 0.015, 1, 0)
#make table for original prediction
table(origin_forest_predict, combined.data$testset$Pathogen)
#Confusion Matrix output
#    0.015
#     0  1 
#  0 13 11
#  1 11  5 
########################################################

#ntree = 600
########################################################
#rf training on the traindata set
origin_forest_final <- caret::train(combined.data$trainset[, c(2, 3, 4, 5)], y = combined.data$trainset[, 1],
                                    method = 'rf', trControl = train_control, tuneGrid = forest_grid,
                                    metric = 'ROC', ntree = 600)

#predict using the test data set
origin_forest_prob <- predict(origin_forest_final$finalModel, newdata = combined.data$testset, type = "prob")

forest_output <- forest_model(oversampled_training, combined.data$testset, 5, c(1, 2, 3, 4),
                              mtry = c(1:4), ntree = 600, cutoff = c(0.10166667, 0.154166667, 0.023333333))

#Plot all of the logistic regression training ROC curves
smote.forest.roc <- pROC::roc(combined.data$testset$Pathogen, forest_output$smote_prob[, 2], plot = TRUE,
                              main = "600 Trees Random Forest ROC Using Experimental Data", 
                              print.auc = TRUE, percent = TRUE, col = "#2e3b5e", asp = NA, grid = TRUE, lwd = 2)
adasyn.forest.roc <- pROC::roc(combined.data$testset$Pathogen, forest_output$adasyn_prob[, 2], plot = TRUE,
                               percent = TRUE, print.auc = TRUE, col = "#2697a7", add = TRUE, print.auc.y = 40, lwd = 2)
slsmote.forest.roc <- pROC::roc(combined.data$testset$Pathogen, forest_output$slsmote_prob[, 2], plot = TRUE,
                                percent = TRUE, print.auc = TRUE, col = "#aaaaaa", add = TRUE, print.auc.y = 30, lwd = 2)
origin.forest.roc <- pROC::roc(combined.data$testset$Pathogen, origin_forest_prob[, 2], plot = TRUE, add = TRUE, percent = TRUE,
                               print.auc = TRUE, col = "#a97f69", print.auc.y = 20, lwd = 2)
legend("bottomright", legend = c("SMOTE", "ADASYN", "SL-SMOTE", "Original Data"),
       col = c("#2e3b5e", "#2697a7", "#aaaaaa", "#a97f69"),
       lwd = 4, cex = 1, xpd = TRUE, horiz = FALSE)

#SMOTE
#make table for smote prediction
table(forest_output$smote_predict, combined.data$testset$Pathogen)
#Confusion Matrix output
#     0.10166667
#        0  1 
#     0 17 12
#     1  7  4

#ADASYN
#make table for adasyn prediction
table(forest_output$adasyn_predict, combined.data$testset$Pathogen)
#Confusion Matrix output
#    0.154166667
#     0  1
#  0 19 12
#  1  5  4

#SL-SMOTE
#make table for slsmote prediction
table(forest_output$slsmote_predict, combined.data$testset$Pathogen)
#Confusion Matrix output
#     0.023333333
#      0  1  
#   0  9 10   
#   1 15  6   

#Original data to train the model
origin_forest_predict <- ifelse(origin_forest_prob[, 2] > 0.0441666667, 1, 0)
#make table for original prediction
table(origin_forest_predict, combined.data$testset$Pathogen)
#Confusion Matrix output
#    0.015
#     0  1 
#  0 14 13
#  1 10  3 
########################################################

#ntree = 700
########################################################
#rf training on the traindata set
origin_forest_final <- caret::train(combined.data$trainset[, c(2, 3, 4, 5)], y = combined.data$trainset[, 1],
                                    method = 'rf', trControl = train_control, tuneGrid = forest_grid,
                                    metric = 'ROC', ntree = 700)

#predict using the test data set
origin_forest_prob <- predict(origin_forest_final$finalModel, newdata = combined.data$testset, type = "prob")

forest_output <- forest_model(oversampled_training, combined.data$testset, 5, c(1, 2, 3, 4),
                              mtry = c(1:4), ntree = 700, cutoff = c(0.075, 0.1778571429, 0.0135714286))

#Plot all of the logistic regression training ROC curves
smote.forest.roc <- pROC::roc(combined.data$testset$Pathogen, forest_output$smote_prob[, 2], plot = TRUE,
                              main = "700 Trees Random Forest ROC Using Experimental Data", 
                              print.auc = TRUE, percent = TRUE, col = "#2e3b5e", asp = NA, grid = TRUE, lwd = 2)
adasyn.forest.roc <- pROC::roc(combined.data$testset$Pathogen, forest_output$adasyn_prob[, 2], plot = TRUE,
                               percent = TRUE, print.auc = TRUE, col = "#2697a7", add = TRUE, print.auc.y = 40, lwd = 2)
slsmote.forest.roc <- pROC::roc(combined.data$testset$Pathogen, forest_output$slsmote_prob[, 2], plot = TRUE,
                                percent = TRUE, print.auc = TRUE, col = "#aaaaaa", add = TRUE, print.auc.y = 30, lwd = 2)
origin.forest.roc <- pROC::roc(combined.data$testset$Pathogen, origin_forest_prob[, 2], plot = TRUE, add = TRUE, percent = TRUE,
                               print.auc = TRUE, col = "#a97f69", print.auc.y = 20, lwd = 2)
legend("bottomright", legend = c("SMOTE", "ADASYN", "SL-SMOTE", "Original Data"),
       col = c("#2e3b5e", "#2697a7", "#aaaaaa", "#a97f69"),
       lwd = 4, cex = 1, xpd = TRUE, horiz = FALSE)

#SMOTE
#make table for smote prediction
table(forest_output$smote_predict, combined.data$testset$Pathogen)
#Confusion Matrix output
#     0.075
#        0  1 
#     0 14 10
#     1 10  6

#ADASYN
#make table for adasyn prediction
table(forest_output$adasyn_predict, combined.data$testset$Pathogen)
#Confusion Matrix output
#    0.1778571429
#     0  1
#  0 19 14
#  1  5  2

#SL-SMOTE
#make table for slsmote prediction
table(forest_output$slsmote_predict, combined.data$testset$Pathogen)
#Confusion Matrix output
#     0.0135714286
#      0  1  
#   0 19 12   
#   1  5  4   

#Original data to train the model
origin_forest_predict <- ifelse(origin_forest_prob[, 2] > 0.0135714286, 1, 0)
#make table for original prediction
table(origin_forest_predict, combined.data$testset$Pathogen)
#Confusion Matrix output
#    0.0135714286
#     0  1 
#  0 14 10
#  1 10  6 
########################################################

#ntree = 1000
########################################################
#rf training on the traindata set
origin_forest_final <- caret::train(combined.data$trainset[, c(2, 3, 4, 5)], y = combined.data$trainset[, 1],
                                    method = 'rf', trControl = train_control, tuneGrid = forest_grid,
                                    metric = 'ROC', ntree = 1000)

#predict using the test data set
origin_forest_prob <- predict(origin_forest_final$finalModel, newdata = combined.data$testset, type = "prob")

forest_output <- forest_model(oversampled_training, combined.data$testset, 5, c(1, 2, 3, 4),
                              mtry = c(1:4), ntree = 1000, cutoff = c(0.0895, 0.1615, 0.0255))

#Plot all of the logistic regression training ROC curves
smote.forest.roc <- pROC::roc(combined.data$testset$Pathogen, forest_output$smote_prob[, 2], plot = TRUE,
                              main = "1000 Trees Random Forest ROC Using Experimental Data", 
                              print.auc = TRUE, percent = TRUE, col = "#2e3b5e", asp = NA, grid = TRUE, lwd = 2)
adasyn.forest.roc <- pROC::roc(combined.data$testset$Pathogen, forest_output$adasyn_prob[, 2], plot = TRUE,
                               percent = TRUE, print.auc = TRUE, col = "#2697a7", add = TRUE, print.auc.y = 40, lwd = 2)
slsmote.forest.roc <- pROC::roc(combined.data$testset$Pathogen, forest_output$slsmote_prob[, 2], plot = TRUE,
                                percent = TRUE, print.auc = TRUE, col = "#aaaaaa", add = TRUE, print.auc.y = 30, lwd = 2)
origin.forest.roc <- pROC::roc(combined.data$testset$Pathogen, origin_forest_prob[, 2], plot = TRUE, add = TRUE, percent = TRUE,
                               print.auc = TRUE, col = "#a97f69", print.auc.y = 20, lwd = 2)
legend("bottomright", legend = c("SMOTE", "ADASYN", "SL-SMOTE", "Original Data"),
       col = c("#2e3b5e", "#2697a7", "#aaaaaa", "#a97f69"),
       lwd = 4, cex = 1, xpd = TRUE, horiz = FALSE)

#SMOTE
#make table for smote prediction
table(forest_output$smote_predict, combined.data$testset$Pathogen)
#Confusion Matrix output
#     0.0895
#        0  1 
#     0 16 11
#     1  8  5

#ADASYN
#make table for adasyn prediction
table(forest_output$adasyn_predict, combined.data$testset$Pathogen)
#Confusion Matrix output
#    0.1615
#     0  1
#  0 19 13
#  1  5  3

#SL-SMOTE
#make table for slsmote prediction
table(forest_output$slsmote_predict, combined.data$testset$Pathogen)
#Confusion Matrix output
#     0.0255
#      0  1  
#   0 19 12   
#   1  5  4   

#Original data to train the model
origin_forest_predict <- ifelse(origin_forest_prob[, 2] > 0.0155, 1, 0)
#make table for original prediction
table(origin_forest_predict, combined.data$testset$Pathogen)
#Confusion Matrix output
#    0.0155
#     0  1 
#  0  8 10
#  1 16  6 
########################################################