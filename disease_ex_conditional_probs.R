#                               Disease (2%) | Healthy (98%)
#Predicted Test positive |          85%      |    10%
#Predicted Test negative |          15%      |    90%   

TP <- 0.02*0.85
FP <- 0.1*0.98
TN <- 0.9*0.98
FN <- 0.15*0.02

(FN + TN) #Probability of test negative
(TP + FP) #Probability of test positive
TP/(TP + FP) #Probability of people who has the disease and test positive
FN/(FN + TN) #Probability of people who has the disease and test negative

set.seed(1)
disease <- sample(c(0,1), size=1e6, replace=TRUE, prob=c(0.98,0.02))
test <- rep(NA, 1e6)
test[disease==0] <- sample(c(0,1), size=sum(disease==0), replace=TRUE, prob=c(0.90,0.10))
test[disease==1] <- sample(c(0,1), size=sum(disease==1), replace=TRUE, prob=c(0.15, 0.85))

mean(test) #Probability of a positive test
TP/(TP + FP)/0.02 #Relative Risk of having the disease if test positive
#probability of having the disease given positive test, and normalized against disesase prevalance
