### Illustration of the exponential distribution

# Remove old variables:
rm(list=ls())

# Generate nobs data from the exponential distribution with rate lambda
# Make a histogram of the data and plot the true density on top
# Repeat the code below many times. 
# What happens when you set nobs to a large number?


nobs <- 100
lambda <- 0.2
y <- rexp(n=nobs,rate = lambda)
hist(y, prob = TRUE,breaks=seq(0,ceiling(max(y)*1.1),length.out=max(10,sqrt(nobs))),
     main="Histogram of data and true exponential density")
curve(dexp(x, rate = lambda), col = 2, lty = 2, lwd = 2, add = TRUE,xlim=c(0.001,max(y)*1.1))

# Expectation:
1/lambda
# Mean of the data:
mean(y)
# True variance:
1/lambda^2
# Sample variance (variance of the data):
var(y)
# Sample standard deviation (standard deviation of the data):
sd(y)






###--- Additional stuff ...

x <- seq( -2,2,.01)
n <- length(x)
y <- rep(0,n)
y[ x>=-1 & x <= 1 ] <- 0.5

plot( x=x, y=y, type='l'); abline(h=0)
# dunif produces the density in x
lines( x=x, y=dunif(x,min=-1,max=1), 
       col='red', lwd=2 )
# punif produces the probability F(x)=P(X<=x)
punif( q=0.5,min=-1,max=1)
# qunif calculates the quantile q so that p=F(q);
# Here: the number q that have prob. 0.75 
# to the left.
qunif( p=0.75,min=-1,max=1)

# Similar for other distributions in R; e.g.
# the normal distribution:
x <- seq( -3,3,.01)
plot(x=x,y=dnorm(x=x),type='l',lwd=2,col=4)
grid();abline(h=0)
pnorm(q=1.645); pnorm(q=1.96)
qnorm(p=0.95)

