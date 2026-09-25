# Remove old variables:
rm(list=ls())

# P(X>2) for gamma(2,0.5)
# Excact
pgamma(2, shape=2, scale=0.5, lower.tail = FALSE)

# By simulation
nrep <- 10000
x <- rgamma(nrep, shape=2, scale=0.5)
length(x[x>2])/nrep 
sum(x>2)/nrep
# The two last lines above illustrate two ways of calculating the proportion
# of cases with x>2 in R

# Plot of gamma densities for different values of shape and scale
# Try other values of shape and scale
x <- seq(0,5,length.out = 2000) # Makes a sequence of points from 0 to 5.
plot(x,dgamma(x, shape=2, scale=0.5),xlab="x",ylab="f(x)",type="l",ylim=c(0,2))
lines(x,dgamma(x, shape=1, scale=0.5), col='blue')
lines(x,dgamma(x, shape=5, scale=0.5), col='red')

# Another way to generate these plots is via the curve()-function
plot(curve(dgamma(x, shape=2, scale=0.5),from=0,to=5),xlab="x",ylab="f(x)",type="l",ylim=c(0,2))
curve(dgamma(x, shape=1, scale=0.5),from=0,to=5,add=TRUE, col='blue')
curve(dgamma(x, shape=5, scale=0.5),from=0,to=5,add=TRUE, col='red')


