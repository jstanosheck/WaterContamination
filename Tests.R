#This file is for testing of new functions 

#test this using a hyperthyroidism dataset from UCI
#reads in the data which is stored in hyper
hyper <-read.csv('http://archive.ics.uci.edu/ml/machine-learning-databases/thyroid-disease/hypothyroid.data', header=F)
name <- read.csv('http://archive.ics.uci.edu/ml/machine-learning-databases/thyroid-disease/hypothyroid.names', header=F, sep='\t')[[1]]
name <- gsub(pattern =":|[.]",x = name, replacement="")

#assign the column names of hyper to name
colnames(hyper) <- name
head(hyper)

#change the first header to the target rather than 
colnames(hyper)[1] <- c("target")
colnames(hyper)[1]

#change the target value to 0 for negative and 1 for positive
hyper$target <- ifelse(hyper$target == 'negative', 0, 1)
hyper$target
table(hyper$target)

#check for the number of values that contain characters "?"
hyper[hyper == "?"] <- NA
