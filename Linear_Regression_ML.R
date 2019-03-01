library(HistData)
library(dslabs)
library(caret)
library(tidyverse)

galton_heights <- GaltonFamilies %>%
  filter(childNum == 1 & gender == "male") %>%
  select(father, childHeight) %>%
  rename(son = childHeight)

y <- galton_heights$son
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
train_set <- galton_heights %>% slice(-test_index)
test_set <- galton_heights %>% slice(test_index)

avg_son <- mean(train_set$son)
R_sq <- mean((test_set$son - avg_son)^2)
R_sq
fit <- lm(son ~ father, data = train_set) #predictor function, MODEL
fit$coef
#(Intercept)      father 
#30.0487912   0.5860452

y_hat <- fit$coef[1] + fit$coef[2]*test_set$father #Math function
mean((y_hat - test_set$son)^2)

y_hat <- predict(fit, test_set)
mean((y_hat - test_set$son)^2)

#------------------------------------------------------------------------
#EXCERCISE_1
set.seed(1)
n <- 100
Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

set.seed(1)
y2 <- dat$y
n <- 100
RMSE <- replicate(n, {
  test_index2 <- createDataPartition(y2, times = 1, p = 0.5, list = FALSE)
  train_set2 <- dat %>% slice(-test_index2)
  test_set2 <- dat %>% slice(test_index2)
  fit2 <- lm(y ~ x, data = train_set2)
  y_hat2 <- predict(fit2, test_set2)
  sqrt(mean((y_hat2 - test_set2$y)^2))
})
mean(RMSE)
sd(RMSE)

#EXCERCISE_2

generator <- function(n){
  Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
  dat <- MASS::mvrnorm(n = n, c(69, 69), Sigma) %>%
    data.frame() %>% setNames(c("x", "y"))
  RMSE <- replicate(100, {
    test_index2 <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
    train_set2 <- dat %>% slice(-test_index2)
    test_set2 <- dat %>% slice(test_index2)
    fit2 <- lm(y ~ x, data = train_set2)
    y_hat2 <- predict(fit2, test_set2)
    sqrt(mean((y_hat2 - test_set2$y)^2))
  })
  c(avg = mean(RMSE), sd = sd(RMSE))
}
set.seed(1)
n<- c(100,500,1000,5000,10000)
final_ans <- sapply(n, generator)
final_ans

