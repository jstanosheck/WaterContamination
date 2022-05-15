library(glmnet)
library(pROC)

data <- read.csv("Data/FloridaWaterQualityData.csv")
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

#test the syngen function
oversampled <- syngen(data$trainset, 'Salmonella', c(1, 2), K=3, C=3)



#Check number of instances for the Salmonella 
#############################################
table(data$trainset$Salmonella) # 0 = 223 1 = 11

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
safe_level <- smotefamily::SLS(data$trainset[, -c(1, 2)],
                               data$trainset[, 2],
                               K=5, 
                               C=10)
safe_level$data$class <-as.factor(safe_level$data$class) #sets to factor vs char
summary(safe_level$data)

#check reassigned SL-Smote
r_SLS <- smotefamily::RSLS(data$trainset[, -c(1, 2)],
                               data$trainset[, 2],
                               K=5, 
                               C=10)
r_SLS$data$class <-as.factor(r_SLS$data$class) #sets to factor vs char
summary(r_SLS$data)
######################################################

#testing knn from class to replace missing values rather than median
######################################################
neighbors <- class::knn(data$trainset, data$testset, data$trainset$Conductivity, 
                        k=3)
neighbors

#try regression
regNeighbors <- FNN::knn.reg(train = data$trainset[-c(1, 2, 3), -c(1, 2, 8)],
             test = data$trainset[c(1, 2, 3), -c(1, 2, 8)],
             y = data$trainset[, 8], 
             k=3)$pred

#get the specific data points coresponding to the test
data$trainset[c(1, 2, 3), -c(1, 8)]
data$trainset[c(1, 2, 3), 8] <- regNeighbors

index1 <- c(1, 2, 3)
index2 <- c( 4, 5, 6)

#test knn.reg when using two indexes as your rows
data$trainset[c(index1, index2), ]
data$testset[-c(index1, index2), 5]




#Test the caret library with logistic regression to get 10 fold cv
######################################################
#shuffle the smote data
smote_index <- sample(nrow(oversampled$smote$data))
oversampled$smote$data <- oversampled$smote$data[smote_index, ]

train_control <- caret::trainControl(method = 'cv', number = 10)

#logistic regression model using train
log_model <- caret::train(oversampled$smote$data[, -10],
                          y = oversampled$smote$data[, 10],
                          method = 'glm',
                          trControl = train_control,
                          family = binomial())

#test the roc curve for the log model
prediction <- predict(log_model$finalModel, newdata = data$testset, type = 'response')

pROC::roc(data$testset$Salmonella, prediction, plot = T)
roc.info <- pROC::roc(data$testset$Salmonella, prediction)

#plot the roc info to find threshold values
roc.df <- data.frame(
  tpp = roc.info$sensitivities * 100,
  fpp = roc.info$specificities * 100,
  thresholds = roc.info$thresholds)
roc.df

pred <- ifelse(prediction > 0.6494, 1, 0)
table('true' = data$testset$Salmonella, 'predicted' = pred)
#########################################################

#testing for svm
#########################################################
#set the class '1' variable to 'positive' and the class '0' to 'negative'
oversampled$smote$data$class <- ifelse(oversampled$smote$data$class == 1, 'positive', 'negative')



#two hyperparameters C (cost) sigma (gamma)
hyper_grid <- expand.grid(C = c(0.01, 0.1, 1, 10),
                          sigma = c(0.0000001, 0.000001, 0.00001))

#train control for cv
train_control <- caret::trainControl(method = 'cv',
                                     number = 10,
                                     classProbs = T,
                                     summaryFunction = twoClassSummary)

#SVM model using train
svm_model <- caret::train(oversampled$smote$data[, -10],
                          y = oversampled$smote$data[, 10],
                          method = 'svmRadial',
                          trControl = train_control,
                          tuneGrid = hyper_grid,
                          metric = 'ROC')

svm_model$results

svm_probs <- kernlab::predict(svm_model$finalModel, newdata = data$testset[, -c(1, 2)], type = 'prob')
svm_predicts <- ifelse(svm_probs[, 2] > 0.027104877, 1, 0)
table("predict" = svm_predicts, "true" = data$testset$Salmonella)

#generate roc curve from the predictions
pROC::roc(data$testset$Salmonella, svm_probs[, 2], plot = T)


roc.info.svm <- pROC::roc(data$testset$Salmonella, svm_probs[, 2])

#plot the roc info to find threshold values
roc.df.svm <- data.frame(
  tpp = roc.info.svm$sensitivities * 100,
  fpp = roc.info.svm$specificities * 100,
  thresholds = roc.info.svm$thresholds)
roc.df.svm


#svm model using e1071 library
###########
e_svm_model <- e1071::svm(x = oversampled$smote$data[, -10],
    y = oversampled$smote$data[, 10],
    gamma = 1, cost = 10, type = 'C',
    probability = TRUE)

e_svm_predict <- predict(e_svm_model, newdata = data$testset[, -c(1, 2)], type = 'response')

table(e_svm_predict, data$testset$Salmonella)
#####################################


#random forest testing
#########################################################
#setting up the first random forest

#one hyper parameter mtry (number of random variables to use in tree)
forest_grid <- expand.grid(mtry = c(1:6))

#train control for cv
train_control <- caret::trainControl(method = 'cv',
                                     number = 10)

#random forest model using train
forest_model <- caret::train(oversampled$smote$data[, 4:9],
                          y = oversampled$smote$data[, 10],
                          method = 'rf',
                          trControl = train_control,
                          tuneGrid = forest_grid,
                          metric = 'Accuracy',
                          ntree = 100)

forest_predict <- predict(forest_model$finalModel, newdata = data$testset[, 6:11])
table('prediction' =forest_predict, 'true' = data$testset$Salmonella)


##########################

#use cook's distance to detect outliers
############################################

simple_model <- glm(Salmonella~Temp.water+Conductivity+pH+ORP+Trubidity,
                    data = data$trainset,
                    family = 'binomial')
cook_distance <- cooks.distance(simple_model)
cook_mean <- mean(cook_distance)

#check with indexes have a cook's distance greater than 3x mean
data$trainset$Salmonella[which(cook_distance > 3 * cook_mean)]

