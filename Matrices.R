library(dslabs)
library(ggplot2)

mnist <- read_mnist()
class(mnist$train$images)

x<- mnist$train$images[1:1000,]
y<- mnist$train$labels[1:1000]

#Matrix notation
length(x[1,])
x_1 <- 1:5
x_2 <- 6:10
cbind(x_1, x_2)
dim(x)
nrow(x) #or dim(x)[1]
ncol(x) #or dim(x)[2]

#converting vector into matrix
dim(as.matrix(x_1)) #since a vector doesnt have a dimension, only matrices
my_vector <- 1:15
mat <- matrix(my_vector,5,3)
mat

mat_t <- matrix(my_vector,3,5, byrow = TRUE) #transpose
mat_t #Warning! Matrix function recycles values in vector

identical(mat_t,t(mat)) #function t to transpose

grid <- matrix (x[3,],28,28) #third entry (predictor) represents a 4
grid
image(1:28, 1:28, grid) #image flipped since R plots pixel 1 at the top
image(1:28, 1:28, grid[,28:1]) #flipping code

#CHALLENGES

#1. Study distribution of total pixel darkness and how it varies by digits
sums <- rowSums(x)
avg <- rowMeans(x)
#same functions but lower speed
avgs <- apply(x,1,mean) #rows
sds <- apply(x,2,sd) #cols

#2. Study the variation of each pixel and remove predictors (cols)
#   associated with pixels that dont change much and dont provide information
library(matrixStats)
sds <- colSds(x)

x[,c(351,352)] # extract cols e.g.
x[c(2,3),]# extract rows e.g.

new_x <- x[,colSds(x) > 60]
dim(new_x)

class(x[,1])
dim(x[,1])
class(x[,1,drop=FALSE])
dim(x[,1,drop=FALSE])

#3. Zero out low values that likely smudges (distribution of pixel values)
#   and select a cutoff value to define unwritten space (0s anything below that)
bin_x <- x
bin_x[bin_x < 255/2] <- 0
bin_x[bin_x > 255/2] <- 1#turning less values less than 50 into 0
#e.g. example[example >5 & example <12] <- 0

#4. Binarize the data (distribution -> cutoff for 1 or 0 when is written and not)


#5. Scale each of the predictors in each entry to have same AVG and SD
scaledx <- (x- rowMeans(x)) / rowSds(x) #forRows

scaledcolsx <- t(t(x) - colMeans(x)) #for cols needs to be transposed
x_mean_0 <- sweep(x,2,colMeans(x)) #by default is substraction

x_standarized <- sweep(x_mean_0, 2, colSds(x), FUN ="/")

#question
y2 <- rowMeans(mnist$train$images>50 & mnist$train$images<205)
qplot(as.factor(mnist$train$labels), y2, geom = "boxplot")
mean(y2)
