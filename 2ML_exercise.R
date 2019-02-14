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
cutoff2 <-rangedValues2[which(predictions2==max(predictions2))]
cutoff2
#accuracy using Petal.Length as feature
y_hat2 <- ifelse(test[,3]>cutoff2[1],'virginica','versicolor')
mean(y_hat2==test$Species)

predictions3 <- foo(train[,4])
rangedValues3 <- seq(range(train[,4])[1],range(train[,4])[2],by=0.1)
cutoff3 <-rangedValues3[which(predictions3==max(predictions3))]
cutoff3
#accuracy using Petal.Width as feature
y_hat3 <- ifelse(test[,4]>cutoff3[2],'virginica','versicolor')
mean(y_hat3==test$Species)
#accuracy using Petal.Width and Petal.Length as features
y_hat_final <- ifelse((test[,4]>cutoff3[2]) | (test[,3]>cutoff2[1]),'virginica','versicolor')
mean(y_hat_final==test$Species)

#----------------------------------------------------------------------
#alternative answer from professor
library(caret)
data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species

plot(iris,pch=21,bg=iris$Species)

set.seed(2)
test_index <- createDataPartition(y,times=1,p=0.5,list=FALSE)
test <- iris[test_index,]
train <- iris[-test_index,]

petalLengthRange <- seq(range(train[,3])[1],range(train[,3])[2],by=0.1)
petalWidthRange <- seq(range(train[,4])[1],range(train[,4])[2],by=0.1)
cutoffs <- expand.grid(petalLengthRange,petalWidthRange)

id <- sapply(seq(nrow(cutoffs)),function(i){
  y_hat <- ifelse(train[,3]>cutoffs[i,1] | train[,4]>cutoffs[i,2],'virginica','versicolor')
  mean(y_hat==train$Species)
}) %>% which.max

optimalCutoff <- cutoffs[id,] %>% as.numeric
optimalCutoff
y_hat <- ifelse(test[,3]>optimalCutoff[1] & test[,4]>optimalCutoff[2],'virginica','versicolor')
mean(y_hat==test$Species)
