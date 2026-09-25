# Remove old variables:
rm(list=ls())

########################################################################################

#### Examples of the inverse transform method

########################################################################################

## Generating data from the exponential distribution with inverse transform

# Function for generating Nsim observations from the exponential distribution
# with parameter lambda using inverse transform
genexpdistr <- function(Nsim,lambda){
  U <- runif(Nsim)
  X <- -log(1-U)/lambda
  return(X)
}

# Repeat the lines below for different values of Nsim and lambda
Nsim <- 10000
lambda <- 2
x <- genexpdistr(Nsim=Nsim,lambda=lambda)
# Histogram of simulated data with true density on top as red line
hist(x, prob = TRUE,breaks=seq(0,ceiling(max(x)*1.1),length.out=max(10,sqrt(Nsim))),
     main="Histogram of data and true exponential density")
curve(dexp(x, rate = lambda), col = "red", lty = 2, lwd = 2, add = TRUE,xlim=c(0.001,max(x)*1.1))
# Expectation
1/lambda
# Mean of the data
mean(x)
# Standard deviation of the data
sd(x)
# Compare empirical quantiles of simulated data and true quantiles
Qdata <- quantile(x,probs = seq(0.1,0.9,by=0.1))
Qtrue <- qexp(seq(0.1,0.9,by=0.1),rate=lambda)
round(rbind(Qdata,Qtrue),3)


# Alternatively the built in function rexp can be used
x <- rexp(n=Nsim,rate = lambda)
hist(x, prob = TRUE,breaks=seq(0,ceiling(max(x)*1.1),length.out=max(10,sqrt(Nsim))),
     main="Histogram of data and true exponential density")
curve(dexp(x, rate = lambda), col = "red", lty = 2, lwd = 2, add = TRUE,xlim=c(0.001,max(x)*1.1))

# Comparing speed of the two ways of generating data
Nsim <- 10000000
set.seed(12345)
system.time(genexpdistr(Nsim=Nsim,lambda=lambda))
set.seed(12345)
system.time(rexp(n=Nsim,rate = lambda))



########################################################################################

## Generating data from the number of heads example where 
## f(0)=1/8, f(1)=3/8, f(2)=3/8 and f(3)=1/8

# Function for simulating number of heads according to the distribution above
gennumofheads <- function(Nsim){
   U <- runif(Nsim)
   X <- rep(0,Nsim)
   X[(U>1/8) & (U<=4/8)] <- 1
   X[(U>4/8) & (U<=7/8)] <- 2
   X[U>7/8] <- 3
   return(X)
}

Nsim <- 10000
heads <- gennumofheads(Nsim)
table(heads)
relfreq <- table(heads)/Nsim
relfreq
barplot(relfreq,ylab="Relative frequency")

# Alternatively the built in sample function can be used 
heads <- sample(0:3,size=Nsim,replace=TRUE,prob=c(1/8,3/8,3/8,1/8))
table(heads)
relfreq <- table(heads)/Nsim
relfreq

# Comparing speed of the two ways of generating data
Nsim <- 10000000
set.seed(12345)
system.time(gennumofheads(Nsim))
set.seed(12345)
system.time(sample(0:3,size=Nsim,replace=TRUE,prob=c(1/8,3/8,3/8,1/8)))

