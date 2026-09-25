# clear all variables
rm(list=ls())

################################################################
#### Problem set 3
################################################################

# (Solutions to the problems from the book are found at the end)



################################################################
### Problem 1

p <- c( .7, rep( 0.95, times=3), rep( 0.99, times=2), rep( 0.92, times=3), 0.7 )
Nsim <- 10000

# d/a)
I1 <- sample(0:1,size=Nsim,replace=TRUE,prob=c(1-p[1],p[1]))
I2 <- sample(0:1,size=Nsim,replace=TRUE,prob=c(1-p[2],p[2]))
sum(I1*I2)/Nsim # The fraction of cases when both have value 1

# d/b)
I1 <- sample(0:1,size=Nsim,replace=TRUE,prob=c(1-p[1],p[1]))
I2 <- sample(0:1,size=Nsim,replace=TRUE,prob=c(1-p[2],p[2]))
sum(pmax(I1,I2))/Nsim # The fraction of cases when at least one have value 1

# d/c)
I1 <- sample(0:1,size=Nsim,replace=TRUE,prob=c(1-p[1],p[1]))
I2 <- sample(0:1,size=Nsim,replace=TRUE,prob=c(1-p[2],p[2]))
I3 <- sample(0:1,size=Nsim,replace=TRUE,prob=c(1-p[3],p[3]))
I4 <- sample(0:1,size=Nsim,replace=TRUE,prob=c(1-p[4],p[4]))
sum(I1*I2*pmax(I3,I4))/Nsim

# d)
Imatrix <- matrix(nrow=Nsim,ncol=10)
for(i in 1:10)
  Imatrix[,i] <- sample(0:1,size=Nsim,replace=TRUE,prob=c(1-p[i],p[i]))
Nfunctioning <- rowSums(Imatrix) # Calculate number of working for each row
sum(Nfunctioning>=7)/Nsim # Probability for at least 7 working

barplot(table(Nfunctioning)/Nsim)


# Alternative code:

# Matrix of Nsim x 10 U[0,1]-values:
u.matrix <- matrix(runif(Nsim*10), ncol=10 )
functioning.matrix <- (u.matrix < matrix( rep(p,times=Nsim), byrow=T, ncol=10))
Nfunctioning2 <- rowSums(functioning.matrix)
sum(Nfunctioning2 >= 7)/Nsim




################################################################
### Problem 2

##  Function to simulate values from a general triangle distribution
## using the acceptance-rejection method:
rtriang <- function(Nsim, a, b, c){
  if(!(a<c & c<b))
     stop("Error, check a, b and c.")
  k <- 0 # counter for accepted
  x <- numeric(Nsim) # vector for accepted
  while(k<Nsim){
    u <- runif(1)
    y <- runif(1,min=a,max=b) # proposal distribution
    if(y<c){  #different pdf before and after c
      if(u<((y-a)/(c-a))){ # Then we accept
        k <- k+1
        x[k] <- y
      }
    }
    else{
      if(u<((b-y)/(b-c))){ # Then we accept
        k <- k+1
        x[k] <- y
      }
    }
  }
  return(x)
}


# a)
n <- 10000
x.tri <- rtriang(Nsim=n,a=2,b=10,c=6)
print("problem 3a")
print("conditionally on find, P(X>=8) is approximately")
print(sum(as.integer(x.tri>8.0))/n)
# now turn to unconditional probabilities
finds<- sample(x=c(0,1),size=n,prob=c(0.6,0.4),replace=T) # zero for no find, one for find
uncond.res <- finds*x.tri
print("unconditional P(X>=8) is approximately")
print(sum(as.integer(uncond.res>8.0))/n)


# b)
n <- 10000
x.1 <- rtriang( n, a=2, b=6, c=4 )
x.2 <- rtriang( n, a=3, b=11, c=7 )
x.3 <- rtriang( n, a=2, b=6, c=4 )
x.4 <- rtriang( n, a=1, b=9, c=5 )
x.5 <- rtriang( n, a=8, b=10, c=9 )
x.6 <- rtriang( n, a=5, b=9, c=7 )
x.7 <- rtriang( n, a=2, b=6, c=4 )
x.8 <- rtriang( n, a=3, b=5, c=4 )
x.9 <- rtriang( n, a=8, b=12, c=10 )
x.10 <- rtriang( n, a=3, b=7, c=5 )

# Resources assuming finding at all ten sites.
R.non.risk <- x.1+x.2+x.3+x.4+x.5+x.6+x.7+x.8+x.9+x.10
hist( R.non.risk, prob=TRUE, density=10, nclass = sqrt(n) )

# Estimated expectation, non risk-weigthed:
mean( R.non.risk )
abline( v=mean( R.non.risk ), col=2, lwd=2 )


# Resources, risk-weighted:
p <- c(0.8,0.3,0.6,0.6,0.5,0.9,0.5,0.8,0.4,0.4)
X.non.risk.matrix <- cbind(x.1,x.2,x.3,x.4,x.5,x.6,x.7,x.8,x.9,x.10)
X.risk.matrix <- X.non.risk.matrix
for(i in  1:10)
  X.risk.matrix[,i] <- X.risk.matrix[,i]*sample(x=c(0,1),size=n,prob=c(1-p[i],p[i]),replace=T)
R.risk <- rowSums(X.risk.matrix) # dim(y.non.risk.matrix);
hist( R.risk, prob=TRUE, density=10, nclass = sqrt(n) )

# Estimated expectation, non risk-weigthed:
mean( R.risk )
abline( v=mean( R.risk ), col=2, lwd=2 )


# c)
# Analytically:
# Non risk-weigthed mean:
a <- c(2,3,2,1,8,5,2,3,8,3)
b <- c(6,11,6,9,10,9,6,5,12,7)
c <- c(4,7,4,5,9,7,4,4,10,5)
sum( (a+b+c)/3 )

# Risk-weigthed mean:
sum( ((a+b+c)/3)*p)



# d)
# Standard deviation
sd(R.non.risk)
sd(R.risk)
# Standard deviation for the mean (standard error)
sd(R.non.risk)/sqrt(n)
sd(R.risk)/sqrt(n)

# Required number of simulations
4*sd(R.non.risk)^2/0.2^2
4*sd(R.risk)^2/0.2^2






################################################################
### Problem 3

# b)
library(mvtnorm)
# Specify expectation and covariance matrix
mu <- c(1,1)  # Expectation
sigma <- matrix(c(1,1.21,1.21,3),nrow=2) # Covariance matrix
sigma  # To check that the matrix is correct

Nsim <- 100000
Xdata <- rmvnorm(Nsim, mean = mu, sigma = sigma)

# Probability that the sum is more than 3
sum(Xdata[,1]+Xdata[,2]>3)/Nsim

# c)
# When there is no correlation the covariance matrix changes to
sigma <- matrix(c(1,0,0,3),nrow=2) # Covariance matrix
sigma  # To check that the matrix is correct

Nsim <- 100000
Xdata <- rmvnorm(Nsim, mean = mu, sigma = sigma)

# Probability that the sum is more than 3
sum(Xdata[,1]+Xdata[,2]>3)/Nsim







################################################################
#Solutions to the problems from the book
################################################################




################################################################
### Problem 3.3 from Rizzo

n<-10000
u <- runif(n)

x<- 2/(sqrt(1-u))
# Note: this distribution has a heavy right hand tail, therefore we truncate at x=30
hist(x[x<30],prob = TRUE,nclass=30)
curve(8.0/x^3,from=2,to=30,add=TRUE, col='red')


################################################################
### Problem 3.5 from Rizzo
probs <- c(0.1,0.2,0.2,0.2,0.3)
cprobs <- cumsum(probs)
n<- 1000
u <- runif(n)
x <- rep(0,n) # vector of integer zeros
for(i in 1:4)
    x <- x+as.integer(u>cprobs[i]) # if u>cprobs, add one to x

h <- table(x)
print("Observed frequencies")
print(h/n)
print("Theoretical frequencies")
print(probs)

# Now use sample function
x.s <- sample(x=c(0,1,2,3,4),size=n,replace=TRUE,prob=probs)
h.s <- table(x.s)
print("Observed frequencies using sample()")
print(h.s/n)

# Histograms with true probabilities added in red lines
par(mfrow=c(1,2))
hist(x,breaks=c(-0.5,0.5,1.5,2.5,3.5,4.5),density=8,prob=TRUE)
for(i in 0:4)
  lines(c(i,i),c(0,probs[i+1]),lwd=3,col="red")
hist(x.s,breaks=c(-0.5,0.5,1.5,2.5,3.5,4.5),density=8,prob=TRUE)
for(i in 0:4)
  lines(c(i,i),c(0,probs[i+1]),lwd=3,col="red")
par(mfrow=c(1,1))





################################################################
### Problem 3.7 from Rizzo

rbeta.ar <- function(n,a,b){
  # accept-reject-method using uniform proposal, ASSUMING a,b>1
  xmode <- (a-1)/(a+b-2) # see solution
  pdf.max <- (1/beta(a,b))*xmode^(a-1)*(1-xmode)^(b-1)  # maximum value of the beta-PDF
  # Since g is U(0,1), i.e. g=1, we get that f/g<= c = pdf.max
  c <- pdf.max
  x <- numeric(n)
  k <- 0 # Counter for accepted
  while(k<n){
    y <- runif(1) # draw from g
    u <- runif(1) # draw from uniform(0,1)
    alpha <- (1/beta(a,b))*y^(a-1)*(1-y)^(b-1)/c # = f/(c*g)
    if(u<alpha){
      k <- k+1
      x[k] <- y;
    }
  }
  return(x)
}

# Now try the function
a<-3.0
b<-2.0
x <- rbeta.ar(n=1000,a=a,b=b)
hist(x,probability=TRUE,breaks=20)
curve((1/beta(a,b))*x^(a-1)*(1-x)^(b-1),from=0,to=1,add=TRUE, col='red')








################################################################
### Problem 3.17 from Rizzo

# Comparing speed of the two ways of generating data
a <- 2
b <- 2
# A first speed comparison
Nsim <- 200000
set.seed(12345)
system.time(rbeta.ar(n=Nsim,a=a,b=b))
set.seed(12345)
system.time(rbeta(n=Nsim,shape1=a,shape2=b))

# Testing the for loops suggested in the problem text.
set.seed(12345)
system.time(for(i in 1:1000)
  rbeta.ar(5000,a=a,b=b))
set.seed(12345)
system.time(for(i in 1:1000)
  rbeta(n=5000,shape1=a,shape2=b))




