#This will contain the modeling functions
#binomial logistic regression

#traindata - is the output of syngen() should have $smote $adasyn $slsmote
#testdata - data frame of testing data
#target_col - integer of the column that holds the target variable
#train_cols - vector of all columns to be used in training
logistic_model <- function(traindata, testdata, target_col, train_cols){
  
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
  smote_predict <- predict(smote_model$finalModel, newdata = testdata, type = 'response')
  
  #use ADASYN data set from traindata to generate crossval model
  adasyn_model <- caret::train(traindata$adasyn$data[, train_cols],
                              y = traindata$adasyn$data[, target_col],
                              method = 'glm',
                              trControl = train_control,
                              family = binomial())
  
  #use ADASYN model to predict on the test data
  adasyn_predict <- predict(adasyn_model$finalModel, newdata = testdata, type = 'response')
  
  #use SLSMOTE data set from traindata to generate crossval model
  slsmote_model <- caret::train(traindata$slsmote$data[, train_cols],
                               y = traindata$slsmote$data[, target_col],
                               method = 'glm',
                               trControl = train_control,
                               family = binomial())
  
  #use SLSMOTE model to predict on the test data
  slsmote_predict <- predict(slsmote_model$finalModel, newdata = testdata, type = 'response')
  
  #return overall model, and the predictions form the best model.
  return(list('smote_model' = smote_model,
              'smote_predict' = smote_predict,
              'adasyn_model' = adasyn_model,
              'adasyn_predict' = adasyn_predict,
              'slsmote_model' = slsmote_model,
              'slsmote_predict' = slsmote_predict))
}


#Support Vector machines





#random forest
forest_model <- function(traindata, testdata, target_col, train_cols,
                         mtry = c(1:3), ntree = 600){
  #set the hyper parameters
  forest_grid <- expand.grid(mtry = mtry)
  
  #set train control method
  train_control <- caret::trainControl(method = 'cv',
                                       number = 10)
  
  #SMOTE training
  ##########
  #rf training on the traindata set
  smote_model <- caret::train(traindata$smote$data[, train_cols],
                               y = oversampled$smote$data[, target_col],
                               method = 'rf',
                               trControl = train_control,
                               tuneGrid = forest_grid,
                               metric = 'Accuracy',
                               ntree = ntree)
  
  #ADASYN training
  ##########
  #rf training on the traindata set
  
  
  #SLSMOTE training
  ##########
  #rf training on the traindata set
}




