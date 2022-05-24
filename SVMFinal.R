#Source the functions for the modeling and for all pre-processing
source('modelingfunc.R')
source('Preprocessfunc.R')

#Bring in any needed libraries here
library(glmnet)
library(pROC)
library(lattice)
library(ggplot2)


#Collecting all data and doing all Pre-processing steps
########################################################

#Retrieve all of the training data
data <- read.csv("Data/FloridaWaterQualityData.csv")

#Split the training data into training and testing data
data <- traintestsplit(data, 0.2)

#set Salmonella as a factor for train and test
data$trainset$Pathogen <- as.factor(data$trainset$Pathogen)
data$testset$Pathogen <- as.factor(data$testset$Pathogen)


#Test the data for any missing values or any outliers and replace both with
#the average value of the feature
data <- outliermissingvalues(data$trainset, data$testset)

#Standardize the training and testing data using a z-score analysis
data <- standardize(data$trainset, data$testset)

#Synthesize oversampling data by using SMOTE, ADASYN, and SL-SMOTE
#This will return three sets of training and testing data to use, one for each
#of the oversampling methods.
#The "K" and "C" values must be at least 4.
oversampled <- syngen(data$trainset, 'Pathogen', c(1), C = 5)