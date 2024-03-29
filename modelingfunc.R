#This will contain the modeling functions
#binomial logistic regression

#traindata - is the output of syngen() should have $smote $adasyn $slsmote
#testdata - data frame of testing data
#target_col - integer of the column that holds the target variable
#train_cols - vector of all columns to be used in training
#cutoff - {vector} vector of length 3 for the cutoff of train test data default 0.5
logistic_model <- function(traindata,
                           testdata,
                           target_col,
                           train_cols,
                           cutoff = c(0.5, 0.5, 0.5)){
  
  #set train control method for all three models
  #10-fold cross validation
  train_control <- caret::trainControl(method = 'cv', number = 10)
  
  #use SMOTE data set from traindata to generate crossval model
   smote_model <- caret::train(traindata$smote$data[, train_cols],
                               y = traindata$smote$data[, target_col],
                               method = 'glm',
                               trControl = train_control,
                               family = binomial())
  
  #use SMOTE model to predict on the test data
  smote_prob <- predict(smote_model$finalModel, newdata = testdata, type = 'response')
  smote_predict <- ifelse(smote_prob > cutoff[1], 1, 0)
  
  #use ADASYN data set from traindata to generate crossval model
  adasyn_model <- caret::train(traindata$adasyn$data[, train_cols],
                              y = traindata$adasyn$data[, target_col],
                              method = 'glm',
                              trControl = train_control,
                              family = binomial())
  
  #use ADASYN model to predict on the test data
  adasyn_prob <- predict(adasyn_model$finalModel, newdata = testdata, type = 'response')
  adasyn_predict <- ifelse(adasyn_prob > cutoff[2], 1, 0)
  
  #use SLSMOTE data set from traindata to generate crossval model
  slsmote_model <- caret::train(traindata$slsmote$data[, train_cols],
                               y = traindata$slsmote$data[, target_col],
                               method = 'glm',
                               trControl = train_control,
                               family = binomial())
  
  #use SLSMOTE model to predict on the test data
  slsmote_prob <- predict(slsmote_model$finalModel, newdata = testdata, type = 'response')
  slsmote_predict <- ifelse(slsmote_prob > cutoff[3], 1, 0)
  
  #return overall model, and the predictions form the best model.
  return(list('smote_model' = smote_model,
              'smote_prob' = smote_prob,
              'smote_predict' = smote_predict,
              'adasyn_model' = adasyn_model,
              'adasyn_prob' = adasyn_prob,
              'adasyn_predict' = adasyn_predict,
              'slsmote_model' = slsmote_model,
              'slsmote_prob' = slsmote_prob,
              'slsmote_predict' = slsmote_predict))
}


#Support Vector machines
svm_model <- function(traindata,
                      testdata,
                      target_col, 
                      train_cols,
                      C = c(0.01, 0.1, 1),
                      sigma = c(0.001, 0.01, 0.1, 1),
                      cutoff = c(0.5, 0.5, 0.5)){
  
  #SMOTE
  
  #two hyperparameters C (cost) sigma (gamma)
  hyper_grid <- expand.grid(C = C,
                            sigma = sigma)
  
  #train control for cv
  train_control <- caret::trainControl(method = 'cv',
                                       number = 10,
                                       classProbs = TRUE,
                                       summaryFunction = twoClassSummary,
                                       verboseIter = FALSE)
  
  #SVM model using train
  smote_model <- caret::train(traindata$smote$data[, train_cols],
                            y = traindata$smote$data[, target_col],
                            method = 'svmRadial',
                            trControl = train_control,
                            tuneGrid = hyper_grid,
                            metric = 'ROC')
  #probabilityfor smote
  smote_prob <- kernlab::predict(smote_model$finalModel, newdata = testdata, type = 'prob')
  
  #predict which is larger
  smote_predict <- ifelse(smote_prob[, 2] > cutoff[1], 1, 0)


  #ADASYN
  adasyn_model <- caret::train(traindata$adasyn$data[, train_cols],
                              y = traindata$adasyn$data[, target_col],
                              method = 'svmRadial',
                              trControl = train_control,
                              tuneGrid = hyper_grid,
                              metric = 'ROC')
  #probability for smote
  adasyn_prob <- kernlab::predict(adasyn_model$finalModel, newdata = testdata, type = 'prob')
  
  #predict which is larger
  adasyn_predict <- ifelse(adasyn_prob[, 2] > cutoff[2], 1, 0)

  #SL-SMOTE
  slsmote_model <- caret::train(traindata$slsmote$data[, train_cols],
                              y = traindata$slsmote$data[, target_col],
                              method = 'svmRadial',
                              trControl = train_control,
                              tuneGrid = hyper_grid,
                              metric = 'ROC')
  #probability for smote
  slsmote_prob <- kernlab::predict(slsmote_model$finalModel, newdata = testdata, type = 'prob')
  
  #predict which is larger
  slsmote_predict <- ifelse(slsmote_prob[, 2] > cutoff[3], 1, 0)
  
  
  return(list(
    'smote_model' = smote_model,
    'smote_prob' = smote_prob,
    'smote_predict' = smote_predict,
    'adasyn_model' = adasyn_model,
    'adasyn_prob' = adasyn_prob,
    'adasyn_predict' = adasyn_predict,
    'slsmote_model' = slsmote_model,
    'slsmote_prob' = slsmote_prob,
    'slsmote_predict' = slsmote_predict
  ))
  
}




#random forest
forest_model <- function(traindata,
                         testdata,
                         target_col,
                         train_cols,
                         mtry = c(1:3),
                         ntree = 600,
                         cutoff = c(0.5, 0.5, 0.5)){
  #set the hyper parameters
  forest_grid <- expand.grid(mtry = mtry)
  
  #set train control method
  train_control <- caret::trainControl(method = 'cv',
                                       number = 10,
                                       classProbs = TRUE,
                                       savePredictions = "final",
                                       summaryFunction = twoClassSummary)
  
  #SMOTE training
  ##########
  #rf training on the traindata set
  smote_model <- caret::train(traindata$smote$data[, train_cols],
                               y = traindata$smote$data[, target_col],
                               method = 'rf',
                               trControl = train_control,
                               tuneGrid = forest_grid,
                               metric = 'ROC',
                               ntree = ntree)
  
  #predict using the test data set
  smote_prob <- predict(smote_model$finalModel, 
                           newdata = testdata,
                           type = "prob")
  smote_predict <- ifelse(smote_prob[, 2] > cutoff[1], 1, 0)
  
  #ADASYN training
  ##########
  #rf training on the traindata set
  adasyn_model <- caret::train(traindata$adasyn$data[, train_cols],
                              y = traindata$adasyn$data[, target_col],
                              method = 'rf',
                              trControl = train_control,
                              tuneGrid = forest_grid,
                              metric = 'ROC',
                              ntree = ntree)
  
  #predict using the test data set
  adasyn_prob <- predict(adasyn_model$finalModel,
                            newdata = testdata,
                            type = "prob")
  adasyn_predict <-ifelse(adasyn_prob[, 2] > cutoff[2], 1, 0)
  
  #SLSMOTE training
  ##########
  #rf training on the traindata set
  slsmote_model <- caret::train(traindata$slsmote$data[, train_cols],
                              y = traindata$slsmote$data[, target_col],
                              method = 'rf',
                              trControl = train_control,
                              tuneGrid = forest_grid,
                              metric = 'ROC',
                              ntree = ntree)
  
  #predict using the test data set
  slsmote_prob <- predict(slsmote_model$finalModel,
                             newdata = testdata,
                             type = "prob")
  slsmote_predict <- ifelse(slsmote_prob[, 2] > cutoff[3], 1, 0)
  
  return(list(
    'smote_model' = smote_model,
    'smote_prob' = smote_prob,
    'smote_predict' = smote_predict,
    'adasyn_model' = adasyn_model,
    'adasyn_prob' = adasyn_prob,
    'adasyn_predict' = adasyn_predict,
    'slsmote_model' = slsmote_model,
    'slsmote_prob' = slsmote_prob,
    'slsmote_predict' = slsmote_predict
  ))
}




