#This is the document for BAEN 683 Peer Review Class
#The testing parameters will be implemented here for the methods presented from
#the paper therein. This can be a preliminary test for the thesis model testing.



#Import needed libraries and source from function scripts.
library(randomForest)
library(pROC)
source("Preprocessfunc.R")
source("modelingfunc.R")

#Save the original data as 'origin_data' from the Florida data set.
data <- read.csv("Data/FloridaWaterQualityData.csv")

#Split the data into training and testing data. 20% testing data
data <- traintestsplit(data, 0.2)

#Set the Salmonella for the training and testing data to a factor
data$trainset$Salmonella <- as.factor(data$trainset$Salmonella)
data$testset$Salmonella <- as.factor(data$testset$Salmonella)

#set the Pond variable for the training and testing data to a factor
data$trainset$Pond <- as.factor(data$trainset$Pond)
data$testset$Pond <- as.factor(data$testset$Pond)

#fix outliers and NA values
data <- outliermissingvalues(data$trainset, data$testset)

#Standardize the data set
data <- standardize(data$trainset, data$testset)

#Generate synthetic data for the training data.
oversampled <- syngen(data$trainset, 'Salmonella', c(1, 2), C = 3)




#Logistic model determination
################################################################################

#Use logistic_model function to determine the logistic model for each method
log_outputs <- logistic_model(oversampled, data$testset, 10, c(5, 6, 7, 8, 9), cutoff = 1.432079e-01)

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
################################################################################

#Random Forest model determination
################################################################################

#use forest_model function on the train and test data
forest_output <- forest_model(oversampled, data$testset, 10, c(5, 6, 7, 8, 9),
                              mtry = c(1:4), ntree = 600)

#SMOTE
#make table for smote prediction
table(forest_output$smote_predict, data$testset$Salmonella)
forest_output$smote_model$bestTune

#ADASYN
#make table for adasyn prediction
table(forest_output$adasyn_predict, data$testset$Salmonella)
forest_output$adasyn_model$bestTune

#SL-SMOTE
#make table for slsmote prediction
table(forest_output$slsmote_predict, data$testset$Salmonella)
forest_output$slsmote_model$bestTune


#original data forest model
forest_grid <- expand.grid(mtry = c(1:4))

#set train control method
train_control <- caret::trainControl(method = 'cv',
                                     number = 10)

#rf training on the traindata set
origin_forest <- caret::train(data$trainset[, 7:11],
                            y = data$trainset[, 2],
                            method = 'rf',
                            trControl = train_control,
                            tuneGrid = forest_grid,
                            metric = 'Accuracy',
                            ntree = 600)

#predict using the test data set
origin_forest_predict <- predict(origin_forest$finalModel, newdata = data$testset)

#make table for origin prediction
table(origin_forest_predict, data$testset$Salmonella)
origin_forest$bestTune

################################################################################

#SVM model determination
################################################################################

#set the output variables in the training data as 'positive' and 'negative'

oversampled$smote$data$class <- ifelse(oversampled$smote$data$class == 1, 'positive', 'negative')
oversampled$adasyn$data$class <- ifelse(oversampled$adasyn$data$class == 1, 'positive', 'negative')
oversampled$slsmote$data$class <- ifelse(oversampled$slsmote$data$class == 1, 'positive', 'negative')

svm_outputs <- svm_model(oversampled, data$testset[, 7:11], 10, c(5:9),
                         C = c(0.01, 0.1, 1, 10, 100, 1000, 10000),
                         sigma = c(0.000000001, 0.00000001, 0.0000001, 0.000001, 0.00001, 0.0001, 0.001, 0.01, 0.1))


#table for the smote model
table(svm_outputs$smote_predict, data$testset$Salmonella)
svm_outputs$smote_model$bestTune

#table for the adasyn model
table(svm_outputs$adasyn_predict, data$testset$Salmonella)
svm_outputs$adasyn_model$bestTune

#table for the sl-smote model
table(svm_outputs$slsmote_predict, data$testset$Salmonella)
svm_outputs$slsmote_model$bestTune


#original data
hyper_grid <- expand.grid(C = c(0.01, 0.1, 1, 10, 100, 1000, 10000),
                          sigma = c(0.000000001, 0.00000001, 0.0000001, 0.000001, 0.00001, 0.0001, 0.001, 0.01, 0.1))

#train control for cv
train_control <- caret::trainControl(method = 'cv',
                                     number = 10,
                                     classProbs = TRUE,
                                     summaryFunction = twoClassSummary,
                                     verboseIter = FALSE)

#SVM model using train
origin_svm <- caret::train(oversampled$smote$data[, c(5:9)],
                            y = oversampled$smote$data[, 10],
                            method = 'svmRadial',
                            trControl = train_control,
                            tuneGrid = hyper_grid,
                            metric = 'ROC')
#probability for smote
origin_prob <- kernlab::predict(origin_svm$finalModel, newdata = data$testset[, 7:11], type = 'prob')

#predict which is larger
origin_predict <- ifelse(origin_prob[, 2] > origin_prob[, 1], 1, 0)

table(origin_predict, data$testset$Salmonella)
origin_svm$bestTune

################################################################################
