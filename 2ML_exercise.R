library(caret)
library(tidyverse)

data(iris)
iris2 <- iris[-which(iris$Species=='setosa'),]
y <- iris2$Species

set.seed(2)
# line of code
test_index <- createDataPartition(y,times=1,p=0.5,list=FALSE)
test <- iris2[test_index,]
train <- iris2[-test_index,]

#which feature of the dataset has the greatest accuracy?
x1 <- iris2$Sepal.Length
x2 <- iris2$Sepal.Width
x3 <- iris2$Petal.Length
x4 <- iris2$Petal.Width

iris2 %>% group_by(Species) %>% summarize(mean(Petal.Length),sd(Petal.Length))

y_hat <- ifelse(x4 > 1.6, "virginica", "versicolor") %>%
  factor(levels = levels(train$Species))
mean(y == y_hat) #accuracy

#Answer from teacher
foo <- function(x){
  rangedValues <- seq(range(x)[1],range(x)[2],by=0.1)
  sapply(rangedValues,function(i){
    y_hat <- ifelse(x>i,'virginica','versicolor')
    mean(y_hat==train$Species)
  })
}
predictions <- apply(train[,-5],2,foo)
predictions
sapply(predictions,max)	
#Best Accuracy is 0.96 for Petal.Length in training set

#smart cutoff and accuracy in test set
cutoff <- seq(range(train$Petal.Length)[1], range(train$Petal.Length)[2], 0.1)
cutoff
predictions$Petal.Length
accuracy <- max(predictions$Petal.Length)
accuracy
index(predictions$Petal.Length == accuracy)

#
y_hat <- ifelse(test$Petal.Length>4.7,'virginica','versicolor')
mean(y_hat==test$Species)

#answer from professor
predictions2 <- foo(train[,3])
rangedValues2 <- seq(range(train[,3])[1],range(train[,3])[2],by=0.1)
cutoffs <-rangedValues2[which(predictions2==max(predictions2))]

y_hat <- ifelse(test[,3]>cutoffs[1],'virginica','versicolor')
mean(y_hat==test$Species)
