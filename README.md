# Detection of **E. coli** and **Salmonella** in Wash Water of Fresh Produce Using Artificial Intelligence Modeling Tools

## Prupose

This repository is used to detect **E. coli** and **Salmonella** in wash water of food, specifically fresh produce. There are several different methods that are being tested here including different machine learning models and methods of data cleaning. 



## Methodology

- Test Outlier Removal
- Test over sampled vs. non-oveersampled
- Test physiochemical features
- Test random forest
- Test SVM
- Test binomial logistic regression

### Outlier removal

Compare the results between removing and not removing the outliers using the 3 standard deviation method. This will be run through the stnadardization method and the syngen method before testing on the three models.

### Oversampled vs. non-oversampled

Compare the results between oversampled and non-oversampled data. This will be combined with the outlier removal method. 

### Physiochemical features

Only the physiochemical features will be used when testing this dataset. If combining the two different data sets from Cornell and from Florida, there will be 3-4 features used. Turbidity, water temperature, pH, conductivity. 


### Lab Experiment

The experimental design of the lab experiment must test the physiochemical features that are determined to be the most important in the pre-methods.


