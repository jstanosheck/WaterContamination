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