# Remove old variables:
rm(list=ls())

########################################################################################
### Examples of acceptance-rejection sampling

## Generating data from the density f(x)=0.75*(1-x^2) for -1<x<1
# Using the uniform distribution as proposal distribution
genfxdata <- function(Nsim){
  k <- 0 # counter for accepted
  j <- 0 # iterations
  x <- numeric(Nsim) # vector for accepted
  while(k<Nsim){
    u <- runif(1)
    j <- j+1
    y <- runif(1,min=-1,max=1) # proposal distribution
    if(u<1-y^2){ # Then we accept
      k <- k+1
      x[k] <- y
    }
  }
  print(paste("Performed ",j,"iterations to simulate",Nsim,"data."))
  return(x)
}

# Repeat the lines below for different values of Nsim
Nsim <- 10000
x <- genfxdata(Nsim=Nsim)
# Histogram of simulated data with true density on top as red line
hist(x, prob = TRUE,breaks=seq(-1,1,length.out=max(10,sqrt(Nsim))),
     main="Histogram of data and true density")
curve(0.75*(1-x^2), col = "red", lty = 2, lwd = 2, add = TRUE,xlim=c(-1,1))
# Mean of the data
mean(x)
# P(X<0.5)
sum(x<0.5)/Nsim


## Generating data from the binomial distribution
# Using the discrete uniform distribution as proposal distribution

genbindata <- function(Nsim,n,p){
  k <- 0 # counter for accepted
  j <- 0 # iterations
  x <- numeric(Nsim) # vector for accepted
  c <- (n+1)*max(dbinom(0:n,size=n, prob=p)) # max fraction between pmfs
  while(k<Nsim){
    u <- runif(1)
    j <- j+1
    y <- sample(0:n, size = 1) # uniform proposal distribution
    if(u<dbinom(y,n,p)/(c/(n+1))){ # Then we accept
      k <- k+1
      x[k] <- y
    }
  }
  print(paste("Performed ",j,"iterations to simulate",Nsim,"data."))
  return(x)
}

# Repeat the lines below for different values of Nsim
Nsim <- 10000
n <- 10
p <- 0.3
x <- genbindata(Nsim=Nsim,n=n,p=p)
table(x)
relfreq <- table(x)/Nsim
relfreq
par(mfrow=c(1,2))
barplot(relfreq,ylab="Relative frequency",col="blue")
barplot(dbinom(0:n,n,p),col="red",ylab="True pmf")
par(mfrow=c(1,1))

# Alternatively the built in rbinom function can be used 
x <- rbinom(Nsim,n,p)
table(x)
relfreq <- table(x)/Nsim
relfreq
par(mfrow=c(1,2))
barplot(relfreq,ylab="Relative frequency",col="blue")
barplot(dbinom(0:n,n,p),col="red",ylab="True pmf")
par(mfrow=c(1,1))

# Comparing speed of the two ways of generating data
Nsim <- 100000
set.seed(12345)
system.time(genbindata(Nsim=Nsim,n=n,p=p))
set.seed(12345)
system.time(rbinom(Nsim,n,p))


