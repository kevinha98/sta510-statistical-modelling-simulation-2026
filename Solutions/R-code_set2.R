# clear all variables
rm(list=ls())

################################################################
#### Problem set 2
################################################################



################################################################
###--- Problem 1

## a)
x <- seq( -1, 5, 0.001 )

f <- rep( 0, times=length(x) )
lambda = 1
f[ x>=0 ] <- lambda*exp(-lambda*x[ x>=0 ])

plot( x=x, y=f, type='l', col=4, lwd=2, ylab="f(x)", main='exponential density, lambda=1' )
abline( h=0 )

## d)
pexp(10000,rate=1/10000)
1-pexp(5000,rate=1/10000)
pexp(10000,rate=1/10000)-pexp(5000,rate=1/10000)




################################################################
### Problem 2

# Notice that lambda in the dpois and ppois function in R is the
# mean (which we have denoted mu).
# e/a)
dpois(2,lambda=2)
1-ppois(2,lambda=2)

# e/b)
dpois(2,lambda=1)

# e/c)
1-ppois(19,lambda=20)

# e/d)
(1-ppois(2,lambda=2))/(1-ppois(0, lambda=2))



################################################################
###--- Problem 3
## a)
x.0 <- seq( -0.5, 1.5, 0.001 )
x <- x.0[ x.0>=0 & x.0<=1 ]
f <- rep( 0, times=length(x.0) )

par(mfrow=c(2,2)) # To have 2x2 plots on the same figure
alpha <- 1
beta <- 1
f[ x.0>=0 & x.0<=1 ] <- gamma(alpha+beta)*x^(alpha-1)*((1-x)^(beta-1)) /(gamma(alpha)*gamma(beta))
plot(x=x.0, y=f, type='l', col=4, lwd=2, xlab="x",ylab="f(x)",main="beta density, alpha=1, beta=1")
abline( h=0 )

alpha <- 2
beta <- 1
f[ x.0>=0 & x.0<=1 ] <- gamma(alpha+beta)*x^(alpha-1)*((1-x)^(beta-1)) /(gamma(alpha)*gamma(beta))
plot(x=x.0, y=f, type='l', col=4, lwd=2, xlab="x",ylab="f(x)",main="beta density, alpha=2, beta=1")
abline( h=0 )

alpha <- 1
beta <- 2
f[ x.0>=0 & x.0<=1 ] <- gamma(alpha+beta)*x^(alpha-1)*((1-x)^(beta-1)) /(gamma(alpha)*gamma(beta))
plot(x=x.0, y=f, type='l', col=4, lwd=2, xlab="x",ylab="f(x)",main="beta density, alpha=1, beta=2")
abline( h=0 )

par(mfrow=c(1,1)) # Reset to 1 plot per figure

# We can do the above more efficiently by making a function:

plot_beta_density <- function(alpha,beta){
  x.0 <- seq( -0.5, 1.5, 0.001 )
  x <- x.0[ x.0>=0 & x.0<=1 ]
  f <- rep( 0, times=length(x.0) )
  f[ x.0>=0 & x.0<=1 ] <- gamma(alpha+beta)*x^(alpha-1)*((1-x)^(beta-1)) /(gamma(alpha)*gamma(beta))
  plot(x=x.0, y=f, type='l', col=4, lwd=2, xlab="x",ylab="f(x)",
       main=paste("beta density, alpha =",alpha, "beta =",beta))
  abline( h=0 )
}

par(mfrow=c(2,2))
plot_beta_density(1,1)
plot_beta_density(2,1)
plot_beta_density(1,2)
par(mfrow=c(1,1)) 



## b)
nobs <- 10 # Repeat the code below for different values of nobs
alpha <- 1
beta <- 1
mean(rbeta(nobs,shape1 = alpha,shape2 = beta))
alpha/(alpha+beta)
alpha <- 2
beta <- 1
mean(rbeta(nobs,shape1 = alpha,shape2 = beta))
alpha/(alpha+beta)
alpha <- 1
beta <- 2
mean(rbeta(nobs,shape1 = alpha,shape2 = beta))
alpha/(alpha+beta)

# We can also do the above in a function:
beta_av_exp <- function(nobs,alpha,beta){
  av <- mean(rbeta(nobs,shape1 = alpha,shape2 = beta))
  exp <- alpha/(alpha+beta)
  print(paste("Average:",round(av,digits = 3),  # round() here rounds off to 3 digits
              "  Expectation:", round(exp,digits = 3), 
              "   Number of simulations:", nobs))
}

beta_av_exp(10,1,1)
beta_av_exp(10,2,1)
beta_av_exp(10,1,2)

for(nobs in c(10,100,1000,10000)){
  beta_av_exp(nobs,1,1)
  beta_av_exp(nobs,2,1)
  beta_av_exp(nobs,1,2)
}




################################################################
### Problem 4
# b)

nrep <- 10000 # number of replications
n <- 5 # change to other values of n given in exercise text and repeat the code below 
Xs <- vector(length=nrep)
for(i in 1:nrep){
  u <- runif(n,0,2);
  Xs[i] <- (sum(u)-n)/sqrt(n*(1/3));
}
hist(Xs,probability=T,ylim=c(0,0.45),xlim=c(-3,3),nclass=nrep/100)
curve(dnorm(x, mean=0, sd=1),from=-3,to=3,add=TRUE, col='red')


################################################################
### Problem 5

# install.packages("gss") # call only once
library(gss)
data("buffalo")
# default KDEs

par(mfrow=c(1,2))
plot(density(buffalo,kernel = "gaussian"))
plot(density(buffalo,kernel = "biweight"))

# half the default bandwidth
par(mfrow=c(1,2))
plot(density(buffalo,adjust=0.5,kernel = "gaussian"))
plot(density(buffalo,adjust=0.5,kernel = "biweight"))

# double default bandwidth
par(mfrow=c(1,2))
plot(density(buffalo,adjust=2.0,kernel = "gaussian"))
plot(density(buffalo,adjust=2.0,kernel = "biweight"))

# conclucions: the two kernels provide very similar results.
# difficult to say if the distibution is fairly Gaussian, or if there are really multiple modes

# then, some histograms
par(mfrow = c(2,2))
hist(buffalo,probability=TRUE) # default
hist(buffalo,probability=TRUE,breaks="scott") # also give same 6 bins
hist(buffalo,probability=TRUE,breaks="freedman-diaconis") # also give same 6 bins
hist(buffalo,probability=TRUE,breaks=seq(from=20,to=140,length.out=11)) # manually choosing 9 bins

# again we see that it is somewhat unclear if the data are fairly Gaussian (with default smoothing levels)
# or in fact if there are multiple modes which appear with less smoothing.




