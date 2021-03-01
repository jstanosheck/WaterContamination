#The contains the preprocessing functions for this project

#traintestsplit function
#ARGS (required):
#1. data (should be df), 2. test_split (should be decimal)
#k-ARGS (not required):
#1. set_seed (integer)

traintestsplit <- function(data, test_split, set_seed = 1234){
  #check that data is a df
  if(!is.data.frame(data)){
    stop("data argument must be a data.frame")
  }
  #check that test_split is between 0, 1
  if(test_split < 0 || test_split > 1){
    stop("test_split argument must be a number between 0 and 1")
  }
  #shuffle the whole data.frame use the seed 
  set.seed(set_seed)
  row_count <- nrow(data)
  rows <- sample(row_count)
  data <- data[rows, ]#resets the whole dataset 
  
  #set the seed and use the sample to make the set of indexes 
  set.seed(set_seed) #reset seed back
  index <- sample(row_count * test_split)
  
  #use the indexes to make the test and train sets
  test_set <- data[index, ]
  train_set <- data[-index, ]
  
  #return list of the trainset and the test set
  return(list('trainset' = train_set, 'testset' = test_set))
}


#outliermissingvalue function
#ARGS (required):
#1. trainset (should be df), 2. testset (should be df)
k-ARG (optional)
#1. k (integer) the number of neighbors to be used for outlier replacement default k = 3

outliermissingvalues <- function(trainset, testset, K = 3){
  #fix any missing values first in train set and test set
  #train set
  #for loop for each column
  for (column in 1:ncol(trainset)) {
    
    #check if column is not factor, if so then run
    if(!is.factor(trainset[, column])){
     
       #check if column has any missing values, ie number of missing values >0
      if(sum(is.na(trainset[, column])) > 0){
        
        #get the mean of the column
        average <- mean(trainset[, column], na.rm = T)
        
        #replace missing value with average
        index <- which(is.na(trainset[, column]))
        trainset[index, column] <- average
      }
    }
  }
  
  #test set
  #for loop fro each column
  for (column in 1:ncol(testset)) {
    
    #check if column is not factor, if so then run
    if(!is.factor(testset[, column])){
      
      #check if column has any missing values, ie number of missing values >0
      if(sum(is.na(testset[, column])) > 0){
        
        #get the mean of the column
        average <- mean(trainset[, column], na.rm = T)
        
        #replace missing value with average
        index <- which(is.na(testset[, column]))
        testset[index, column] <- average
      }
    }
  }
  
  #check for outliers in train set only fix train set outliers
  for (column in 1:ncol(trainset)){
    #only runs the numeric functions if the column is not a factor
    if(!is.factor(trainset[, column])){
      
      #find the min and max values for being non outliers
      iqrange <- IQR(trainset[, column], na.rm = T)
      maxvalue <- as.numeric(quantile(trainset[, column], 3/4, na.rm = T) + iqrange)
      minvalue <- as.numeric(quantile(trainset[, column], 1/4, na.rm = T) - iqrange)
      
      #all values < Q1 - IQR
      minindex <- which(trainset[, column] < minvalue)
      
      #all values > Q3 + IQR
      maxindex <- which(trainset[, column] > maxvalue)
      
      #replace the index values with knn.reg
      knn_rep <- FNN::knn.reg(train = trainset[-c(minindex, maxindex), -c(1, 2, column)],
                   test = trainset[c(minvalue, maxindex), -c(1, 2, column)],#remove the columns w/ non numeric values
                   y = trainset[, column], 
                   k=K)$pred #output a vector of integers
      trainset[c(minindex, maxindex), column] <- knn_rep
    }
  }
  #return the train and test sets
  return(list('trainset' = trainset, 'testset' = testset))
}

#use z-score standardization for the train and test sets
#the mean and sd of the training set was used to standardize the test set
#to have a uniform modeling method.
#returns:
#trainset (standardized df)
#testset (standardized df)
#average (vector) - the average values for each variable
#stdev (vector) - the sd of each variable
standardize <- function(trainset, testset){
  #preallocate memory
  average <- c(rep(0, ncol(trainset)))
  stdev <- c(rep(1, ncol(trainset)))
  factor_index <- c()
  
  #get mean and sd for the train set
  for (column in 1:ncol(trainset)) {
    #calculate mean and sd
    if(is.numeric(trainset[, column])){
      average[column] <- mean(trainset[, column])
      stdev[column] <- sd(trainset[, column])
    } else{
      #this won't actually do anyting to the factor variables
      average[column] <- 0
      stdev[column] <- 1
      
      #store the columns that are factors in a variable and change to numeric
      factor_index[column] <-column
      #trainset[, column] <- as.numeric(trainset[, column])
      #testset[, column] <- as.numeric(testset[, column])
    }
  }
  
  #standardize using loop train set
  for(column in 1:ncol(trainset)){
    if(is.numeric(trainset[, column])){
      #calculate column of z scores
      trainset[, column] <- abs(trainset[, column] - average[column]) / stdev[column]
    }
  }

  #standardize using loop test set
  for(column in 1:ncol(testset)){
    if(is.numeric(testset[, column])){
      #calculate column of z scores
      testset[, column] <- abs(testset[, column] - average[column]) / stdev[column]
    }
  }

  #change the formerly factor columns back to factor
  #trainset[, factor_index] <- as.factor(trainset[, factor_index])
  #testset[, factor_index] <- as.factor(testset[, factor_index])

  return(list('trainset' = trainset,
              'testset' = testset,
              'average' = average,
              'stdev' = stdev))
}

#generate synthetic data sets from the train set using SMOTE, ADASYN, and
#Safe-Level SMOTE
#ARGs
#trainset (df) - set of all data to be used 
#target (string) - name of the column to be used as the target variable
#ignore (vector) - numeric vector of the indexes that sould be ignored
syngen <- function(trainset, target, ignore, ...){
  #compatibility checks
  
  #SMOTE data synthesis
  smote <- smotefamily::SMOTE(trainset[, -ignore], trainset[, target], ...)
  smote$data$class <-as.factor(smote$data$class) #sets to factor vs char
  
  #ADASYN data synthesis
  adasyn <- smotefamily::ADAS(trainset[, -ignore], trainset[, target], ...)
  adasyn$data$class <-as.factor(adasyn$data$class) #sets to factor vs char
  
  #Safe-Level SMOTE data synthesis
  slsmote <- smotefamily::SLS(trainset[, -ignore], trainset[, target], ...)
  slsmote$data$class <-as.factor(slsmote$data$class) #sets to factor vs char
  
  #return all 3 data sets
  return(list('smote' = smote,
              'adasyn' = adasyn,
              'slsmote' = slsmote))
}



