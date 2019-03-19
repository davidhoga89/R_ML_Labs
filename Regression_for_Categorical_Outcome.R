library(dslabs)
library(tidyverse)
library(caret)

data("heights")
y <- heights$height
set.seed(2)
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
train_set <- heights %>% slice(-test_index)
test_set <- heights %>% slice(test_index)

plotting <- function (n) {
  train_set %>%
    filter(round(height)==n) %>%
    summarize(mean(sex=="Female"))
}

vector_x <- seq(60,75,1)
sapply(vector_x, plotting)

lm_fit <- mutate(train_set, y = as.numeric(sex== "Female")) %>% 
  lm(y ~ height, data = .) #linear model with categorical outcoume as # (female as 1, male as 0)

p_hat <- predict(lm_fit, test_set) #probabilities
y_hat <- ifelse(p_hat > 0.5, "Female", "Male") %>% factor()
confusionMatrix(y_hat, test_set$sex)

#logistic regression
glm_fit <- train_set %>%
  mutate(y = as.numeric(sex == "Female")) %>% 
  glm(y ~ height, data = ., family = "binomial")

p_hat_logit <- predict(glm_fit, newdata = test_set, type= "response")
y_hat_logit <- ifelse(p_hat_logit > 0.5, "Female", "Male") %>% factor()
confusionMatrix(y_hat_logit, test_set$sex)
