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
   smote_model <- caret::train(traindata$smote[, train_cols],
                               y = traindata$smote[, target_col],
                               method = 'glm',
                               trControl = train_control,
                               family = binomial())
  
  #use SMOTE model to predict on the test data
  smote_predict <- predict(smote_model$finalModel, newdata = testdata, type = 'response')
  
  #use ADASYN data set from traindata to generate crossval model
  adasyn_model <- caret::train(traindata$adasyn[, train_cols],
                              y = traindata$adasyn[, target_col],
                              method = 'glm',
                              trControl = train_control,
                              family = binomial())
  
  #use ADASYN model to predict on the test data
  adasyn_predict <- predict(adasyn_model$finalModel, newdata = testdata, type = 'response')
  
  #use SLSMOTE data set from traindata to generate crossval model
  slsmote_model <- caret::train(traindata$slsmote[, train_cols],
                               y = traindata$slsmote[, target_col],
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





