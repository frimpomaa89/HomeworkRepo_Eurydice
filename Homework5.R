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

m1<-(matrix(data=x,nrow=8,ncol=8))
m1
m2<-t(m1)## Transposed matrix
m2

#Calculate sum and mean of first and last row
sum(m2[1,])
mean(m2[1,])
sum(m2[8,])
mean(m2[8,])

#Eigen Function
m3<-eigen(m2, symmetric =TRUE, only.values=FALSE)
typeof(m3)

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
new_list
typeof(new_list[[1]])
summary(new_list)
