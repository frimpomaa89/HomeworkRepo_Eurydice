# Using a for loop, write a function to calculate the number of zeroes in a numeric vector. Before entering the loop, set up a counter variable counter <- 0. Inside the loop, add 1 to counter each time you have a zero in the vector. Finally, use return(counter) for the output.

zero_counts <- function(x) {
  counter <- 0  # setting up the counter variable
  
  for (i in x) {
    if (i == 0) {      
      counter <- counter + 1  # adding 1 to counter each time there is a zero in the vector
    }
  }
  
  return(counter)  # Return the final count of zeroes
}


# Testing the function..

d<- c(rep(-1:3, times=5))
print(x)
zero_counts(x=d)

# Using a subsetting function instead of a for loop

zero_counter <- function(x){
  return(sum(x==0))
}

#  Testing zero counter
zero_counter(x=d)

#Write a function that takes as input two integers representing the number of rows and columns in a matrix. The output is a matrix of these dimensions in which each element is the product of the row number x the column number.

#######################################
# FUNCTION: matrix_builder
# packages: none
# purpose:Building a dummy matrix 
# input: two integers, x and y, representing the number of rows and column in a matrix
# output: a matrix of specified dimensions in which each element is the product of the row number x the column number y
# -------------------------------------
matrix_builder <- function(x=NULL, y=NULL) {
  if(is.null(x)) m <- matrix(data = d, nrow = 5, ncol = 5, byrow = TRUE)
  else{
  m <- matrix(data=outer(1:x, 1:y, "*"), # found this outer function online
                nrow=x,
                ncol=y,
                byrow=FALSE)}
  return(m)
  }

# Testing matrix builder

matrix_builder(4,3)


# Simulate a dataset with 3 groups of data, each group drawn from a distribution with a different mean. The final data frame should have 1 column for group and 1 column for the response variable.

# create treatment groups
trt_group <- c(rep("Control",4),rep("Treatment1",5), rep("Treatment2",5))

# create response variable
res <- c(runif(4) + 1, runif(5) + 10, runif(5) + 6)
print(res)
print(trt_group)


df<- data.frame(group= trt_group, response=res)
print(df)


# Write a custom function that 1) reshuffles the response variable, and 2) calculates the mean of each group in the reshuffled data. Store the means in a vector of length 3.

remix_var<- function(x){
  .<- data.frame(group=x[,1], response=sample(x[,2]))
  df1<- aggregate(.[,2], list(.[,1]), FUN=mean) 
  return(df1)
}

#testing remix_var

remix_var(x=df)

# Use a for loop to repeat the function in b 100 times. Store the results in a data frame that has 1 column indicating the replicate number and 1 column for each new group mean, for a total of 4 columns


loopdf<- data.frame(RepNumber=1:100, ControlMeans= rep(NA, 100), T1Means= rep(NA, 100), T2Means= rep(NA, 100))
head(loopdf)

s<-seq(1:100)

for(i in seq_along(s)){
  . <- remix_var(x=df)
  .<- list(ControlMeans=.[1,2],
           T1Means=.[2,2],
           T2Means=.[3,2])
  loopdf[i,2:4]<- unlist(.)
}

head(loopdf)

#Use qplot() to create a histogram of the means for each reshuffled group

library(ggplot2)
qplot(x=loopdf$ControlMeans, geom = "histogram", xlab = "Mean", ylab = "Replicates", main = "Means of Control Group")

qplot(x=loopdf$T1Means, geom = "histogram", xlab = "Mean", ylab = "Replicates", main = "Means of Treatment1 Group")

qplot(x=loopdf$T2Means, geom = "histogram", xlab = "Mean", ylab = "Replicates", main = "Means of Treatment2 Group")

ggplot(loopdf, aes(x=ControlMeans)) + geom_histogram()
