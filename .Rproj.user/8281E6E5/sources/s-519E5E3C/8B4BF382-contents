library (dplyr)
library(tidyverse)
library(dslabs)
library (caret)

#loading dataset and extracting data into vectors
data(heights)
y <- heights$sex
x <- heights$height

#Split dataset into training and test set
set.seed(3)
test_index <- createDataPartition(y,times=1,p=0.25, list=FALSE)
train_set <- heights[-test_index, ]
test_set <- heights[test_index, ]

#Sum/Description of the dataset
heights %>% group_by(sex) %>% summarize(mean(height),sd(height))

#Simplest ML model, guessing using a random sample
y_hat <- sample(c("male","female"),length(test_index),replace=TRUE) %>%
  factor(levels = levels(test_set$sex))
#for the sample the accuracy (or probability) is 50%

#Model predicting "male" if height is within 2 SDs from AVG "male"
y_hat <- ifelse(x > 62, "Male", "Female") %>%
  factor(levels = levels(test_set$sex))
mean(y == y_hat) #accuracy

#doing a plot of accuracy vs. different cutoffs (height of reference)
#from the training set we obtain the maximum accuracy of 83.6 with 64.5 in
cutoff <- seq(61, 70)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train_set$height < x, "Male", "Female") %>%
    factor(levels = levels(test_set$sex))
  mean(y_hat == train_set$sex)
})
accuracy
max(accuracy)
best_cut <- cutoff[which.max(accuracy)]
best_cut

#now that we know the best cutoff value we use it to get the max accuracy
best_cutoff <- 65 #cutoff[which.max(accuracy)]
y_hat <- ifelse(test_set$height > best_cutoff, "Male", "Female") %>%
  factor(levels = levels(test_set$sex))
y_hat <- factor(y_hat)
y_hat
mean(y_hat == test_set$sex)

#___________________________________________________________________
#confusion Matrix
table(predicted=y_hat, actual=test_set$sex)
#                     Actually positive | Actually negative
#Predicted positive |    True +  (TP)   |  False +  (FP)
#Predicted negative |   False -  (FN)   |   True -  (TN)    
#

test_set %>% 
  mutate(y_hat = y_hat) %>%
  group_by(sex) %>%
  summarise(accuracy = mean(y_hat == sex))

#the accuracy of men is higher due to prevalence, the number of male is higher than female
prev <- mean(y == "Male")
prev

#training data is biased, so the model is too
#IMPORTANT! it is better to look at metrics rather than overall accuracy
#when evaluating a ML algorithm

#Sensitivity: defined as the ability of an algorithm to predict a positive outcome,
#when the actual outcome is a positive - e.g. High Sensitivity is Y=1 => Y_hat=1
#TP/(TP + FN) -> it is called also TPR (True Positive Rate) or recall

#---------------------------------------------------------------------------
#Specificity: defined as the ability of an algorithm to NOT predict a positive outcome,
#when the actual outcome is NOT a positive - e.g. High Specificity is Y=0 => Y_hat=0
#TN/(TN + FP) -> TNR (True Negative Rate)
#TP/(TP + FP) -> precision or PPV positive predictive value

confusionMatrix(data=y_hat, reference =test_set$sex)

#Balanced Accuracy & F1 Score

cutoff2 <- seq(61, 70)
F_1 <- map_dbl(cutoff2, function(x){
  y_hat <- ifelse(train_set$height > x, "Male", "Female") %>%
    factor(levels = levels(test_set$sex))
  F_meas(data = y_hat, reference = factor(train_set$sex))
})
F_1

max(F_1)
best_cutoff2 <- cutoff2[which.max(F_1)]
best_cutoff2
#cutoff of 66 makes much more sense than 64, and
#balances the sensitivity and specificity of the confussion matrix
y_hat <- ifelse(test_set$height > best_cutoff2, "Male", "Female") %>%
  factor(levels = levels(test_set$sex))
confusionMatrix(data = y_hat, reference = test_set$sex)
