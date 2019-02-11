library(dslabs)
library(dplyr)
library(lubridate)
library(caret)

data("reported_heights")
dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
  filter(date_time >= make_date(2016, 01, 25) & date_time < make_date(2016, 02, 1)) %>%
  mutate(type = ifelse(day(date_time) == 25 & hour(date_time) == 8 & between(minute(date_time), 15, 30), "inclass","online")) %>%
  select(sex, type)

y <- factor(dat$sex, c("Female", "Male"))
x <- dat$type
#proportion of females in class and online
table(dat)
dat %>% group_by(type) %>% summarize(prop_female = mean(sex == "Female"))


set.seed(2)
test_index <- createDataPartition(y,times=1,p=0.25, list=FALSE)
train_set <- dat[-test_index, ]
test_set <- dat[test_index, ]

#accuracy to predict sex using type
y_hat <- ifelse(x == "inclass", "Female", "Male")
#y_hat
mean(y == y_hat)

#Confussion Matrix
table(predicted=y_hat, actual=y)
confusionMatrix(data = y_hat, reference = y)

#TP/(TP + FN) sensitivity
26/(26+42)
#TN/(TN + FP) specificity
69/(69+13)
