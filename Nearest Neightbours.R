library(dslabs)
data("tissue_gene_expression")

dim(tissue_gene_expression$x)
tissue_gene_expression$x
d <- dist(tissue_gene_expression$x)
dim(d)
class(d)

#Compare the distances between observations 1 and 2 (both cerebellum), observations 39 and 40 (both colon), 
#and observations 73 and 74 (both endometrium).
#Distance-wise, are samples from tissues of the same type closer to each other?
ind <- c(1, 2, 39, 40, 73, 74)
as.matrix(d)[ind,ind]

image(as.matrix(d))

#__________________________________-
# K-Nearest Neightbours
library(tidyverse)
library(caret)
fit_glm <- glm(y ~ x_1 + x_2, data=mnist_27$train, family = "binomial")
p_hat_logistic <- predict(fit_glm, mnist_27$test)
y_hat_logistic <- factor(ifelse(p_hat_logistic > 0.5, 7, 2))
confusionMatrix(data = y_hat_logistic, reference = mnist_27$test$y)$overall[1]
confusionMatrix(data = y_hat_logistic, reference = mnist_27$test$y)

#using knn caret function
#1
knn_fit <- knn3(y ~ ., data = mnist_27$train, k = 5)
#2
x <- as.matrix(mnist_27$train[,2:3])
y <- mnist_27$train$y
knn_fit2 <- knn3(x,y)

y_hat_knn <- predict(knn_fit, mnist_27$test, type = "class")
confusionMatrix(data = y_hat_knn, reference = mnist_27$test$y)$overall["Accuracy"]
confusionMatrix(data = y_hat_knn, reference = mnist_27$test$y)

#finding the correct K

ks <- c(1:300)

library(purrr)
accuracy <- map_df(ks, function(k){
  fit <- knn3(y ~ ., data = mnist_27$train, k = k)
  y_hat <- predict(fit, mnist_27$train, type="class")
  train_error <- confusionMatrix(data = y_hat, reference = mnist_27$train$y)$overall[1]
  
  y_hat <- predict(fit, mnist_27$test, type="class")
  test_error <- confusionMatrix(data = y_hat, reference = mnist_27$test$y)$overall[1]
  
  list(train=train_error, test=test_error)
})
