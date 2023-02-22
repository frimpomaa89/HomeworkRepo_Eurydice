# Homework 5 script

a<-c(seq(from=3, to=10))
print(a)

n_dims <-sample(x=a, size=1)

print(n_dims)

b<- c(seq(from=1, to=(n_dims)^2)) #Create a vector of consecutive integers from 1 to n_dims2

print(b)

x<-c(sample(x=b))#reshuffling values
x

##create square matrix

m1<-(matrix(data=x,nrow=n_dims,ncol=n_dims))
m1
m2<-t(m1)## Transposed matrix
m2

#Calculate sum and mean of first and last row
sum(m2[1,])
mean(m2[1,])
sum(m2[n_dims,])
mean(m2[n_dims,])

#Eigen Function
m3<-eigen(m2, symmetric =TRUE, only.values=FALSE)
m3
typeof(m3)
typeof(m3$values)
typeof(m3$vectors)

##new matrix with new random vlaue
n_dims2 <-sample(x=a, size=1)
print(n_dims2)

b2<- c(seq(from=1, to=(n_dims2)^2))
b2
m4<-(matrix(data=b2,nrow=3,ncol=3))
m4

## Question 2
my_matrix<-(matrix(data=runif(16, min=1, max=20),nrow=4, ncol=4))
my_matrix 

rand<-c(runif(n=100))
rand
my_logical <- rand<0.5
print(my_logical)

alphabets<-c(letters[1:26])              
alphabets
my_letters<-c(sample(x=alphabets))
my_letters
my_list<-list(my_matrix, my_logical, my_letters)

##create a new list, which has the element[2,2] from the matrix, the second element of the logical vector, and the second element of the letters vector.

new_list<- list(my_matrix[2,2], my_logical[2], my_letters[2])

print(new_list)

typeof(new_list[[1]])
summary(new_list)


#new vector
new_vec<-c(new_list)
typeof(new_vec)
print(new_vec)

#Question 3
#Create a data frame with the two variables (= columns) and 26 cases (= rows) below

my_unis<-runif(26, min=0, max=10)
my_LETTERS<-LETTERS[1:26]
my_letters2<-sample(x=my_LETTERS)
My_frame<-data.frame(my_unis,my_letters2)

print(My_frame)

#for the first variable, use a single line of code in R to select 4 random rows and replace the numerical values in those rows with NA.

My_frame[sample(nrow(My_frame),4), 1]<-NA

My_frame[!complete.cases(My_frame[ ,c(1)]),] 


My_frame2<-data.frame(my_unis, sort(my_letters2, decreasing = FALSE))
My_frame2

mean(My_frame2[ ,1])
