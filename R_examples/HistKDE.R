# Remove old variables:
rm(list=ls())


## Exponential
X <- rexp(10000)
hist(X, probability = TRUE, ylim=c(0,1))
lines(density(X), lwd=2)
xg <- seq(from=-1, to=15, by=0.01)
lines(xg, dexp(xg),col="red")

n <- 10000
X <- rexp(n)
plot(ecdf(X),main="CDF vs ECDF")
lines(xg, pexp(xg),col="red",lty=2)


## Normal mixture
X <- c(rnorm(5000,sd=0.5), rnorm(5000,mean=10,sd=0.5))
hist(X, probability = TRUE, ylim = c(0,0.4))
lines(density(X), lwd=2)
xg <- seq(from=-2, to=12, by=0.01)
lines(xg, 0.5*dnorm(xg,sd=0.5)+0.5*dnorm(xg,mean=10,sd=0.5),col="red")

n <- 5000
X <- c(rnorm(n,sd=0.5), rnorm(n,mean=10,sd=0.5))
plot(ecdf(X),main="CDF vs ECDF")
lines(xg, 0.5*pnorm(xg,sd=0.5)+0.5*pnorm(xg,mean=10,sd=0.5),col="red",lty=2)

