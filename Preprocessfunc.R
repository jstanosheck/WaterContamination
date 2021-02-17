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

outliermissingvalues <- function(trainset, testset){
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
      
      #all values < Q1 - IQR replaced by median
      minindex <- which(trainset[, column] < minvalue)
      trainset[minindex, column] <- median(trainset[, column])
      
      #all values > Q3 + IQR replace by median
      maxindex <- which(trainset[, column] > maxvalue)
      trainset[maxindex, column] <- median(trainset[, column])
    }
  }
  #return the train and test sets
  return(list('trainset' = trainset, 'testset' = testset))
}

