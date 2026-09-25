
rm(list=ls())

# Comments are pre-ceeded by #

# Defining variables
a <- 4
b <- 5

x <- 1:5
y <- c(2,5,7,8)

# Initialization of vectors

z <- numeric(10)
is.numeric(z)
is.character(z)

z2 <- seq(0,4,0.5)
z2
z3 <- seq(0,4,length.out=10)
z3

z3[1:4]
z3[c(2,6)]
z3[2]

z4 <- vector(mode="numeric",length=5)
z4

z5 <- vector(mode="character",length=5)
z5

# Matrices
m <- matrix(1:20,nrow=4,ncol=5,byrow=T)
m

m[2,3]
m[2,3:4]
m[c(1,2),3:4]

m1 <- matrix(1:20,nrow=4,ncol=5)
m2 <- matrix(21:40,nrow=4,ncol=5)

m1+m2
m1*m2
m1%*%t(m2)

# Data frames
a1 <- 1:4
a2 <- c("red","white","red",NA)
a3 <- c(TRUE,TRUE,TRUE,FALSE)
mydata <- data.frame(a1,a2,a3)
names(mydata) <- c("ID","colour","passed")
mydata

mydata[,1]
mydata[3,]
mydata$colour
is.data.frame(mydata)

# Lists
l1 <- list(a=c(1,2,3,4),b=TRUE,c=1,d=c("Aa","b"))
l2 <- list(ID=c(1,2,3),colour=c("red","blue"), c=1)

l1$b
l2$colour
l2$colour[2]

l2[[2]]
l2[[2]][1]

# Print & cat
print(l2) # show me this R object
cat(a)    # show the user this nicely formatted message

x <- 13
print(paste("The value of x is", x))
cat("The value of x is", x, "\n")

for (i in 1:3) {
  i
}

for (i in 1:3) {
  print(i)
}

# Functions
source("myfun.R")


myfun1(2)

a <- myfun2(6)
a


# Some logical operations
x <- 1
y <- 2
x == y
x!=y

x <- 10:20
x[x>15]
ind <- which(x>15)
x[ind]


# for-loop & if-else
for(i in 1:10){
  print(i)
  if(i > 5){
    break
  }
}

for(i in c(1,3,10)){
  print(i)
}

for(i in c("a","A","aA")){
  print(i)
}

# Try to use vectorized operations instead of for-loops
x <- rep(0,0,10)
y <- rep(0,0,10)
for(i in 1:10){
  x[i] <- 2*i
  y[i] <- x[i] - 2
}
print(x)
print(y)

x <- 2*(1:10)
y <- x-2
print(x)
print(y)

# Fibonacci
tmp <- numeric(22)
tmp[1] <- 1
tmp[2] <- 1
for(i in 1:20){
  #print(i)
  tmp[i+2] <- tmp[i] + tmp[i+1]
  if(tmp[i+2] > 10000){
    break
  }
}


