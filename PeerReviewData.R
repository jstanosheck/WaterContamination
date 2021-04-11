#This is the document for BAEN 683 Peer Review Class
#The testing parameters will be implemented here for the methods presented from
#the paper therein. This can be a preliminary test for the thesis model testing.



#Import needed libraries and source from function scripts.
library(randomForest)
source("Preprocessfunc.R")

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

