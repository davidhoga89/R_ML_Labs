library (dplyr)
library(dslabs)
library (caret)

#loading dataset and extracting data into vectors
data(heights)
y <- heights$sex
x <- heights$height

#Split dataset into training and test set
set.seed(2)
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
best_cutoff <- 64.5 #cutoff[which.max(accuracy)]
y_hat <- ifelse(test_set$height > best_cutoff, "Male", "Female") %>%
  factor(levels = levels(test_set$sex))
y_hat <- factor(y_hat)
mean(y_hat == test_set$sex) #accuracy

