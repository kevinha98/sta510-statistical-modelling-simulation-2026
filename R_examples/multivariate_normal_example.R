# Remove old variables:
rm(list=ls())
###############################################################################

### Demonstration of the multivariate normal distribution


# In this code we need functions in a library called mvtnorm
# To install this library run:
# install.packages("mvtnorm")
# You only need to do this library installation once

# Load the multivariate normal library 
library(mvtnorm)
# You have to load the library every time you restart R 

# Specify expectation and covariance matrix
mu <- c(8,4,1.05)  # Expectation
sigma <- matrix(c(100,-24,1.5,-24,16,-0.8,1.5,-0.8,0.25),nrow=3) # Covariance matrix
sigma  # To check that the matrix is correct

Nsim <- 100000
Xdata <- rmvnorm(Nsim, mean = mu, sigma = sigma) 

# Probability that all goes in plus
sum((Xdata[,1]>0 & Xdata[,2]>0) & Xdata[,3]>0)/Nsim
# Probability that the two first goes in plus
sum((Xdata[,1]>0 & Xdata[,2]>0))/Nsim

# Notice that since X1 and X2 are dependent P(X1>0 and X2>0)
# is not the same as P(X1>0)*P(X2>0)
(sum(Xdata[,1]>0)/Nsim)*(sum(Xdata[,2]>0)/Nsim)


# Considering various investment portfolios:
Y <- Xdata %*% c(10,20,100)  # Notice %*% for matrix multiplication
hist(Y,nclass=max(10,sqrt(Nsim)))
mean(Y)
var(Y)
sd(Y)
mean(Y>0)
mean(Y>500)

Y <- Xdata %*% c(20,0,100)
hist(Y,nclass=max(10,sqrt(Nsim)))
mean(Y)
sd(Y)
mean(Y>0)
mean(Y>500)

Y <- Xdata %*% c(20,40,0)
hist(Y,nclass=max(10,sqrt(Nsim)))
mean(Y)
sd(Y)
mean(Y>0)
mean(Y>500)

Y <- Xdata %*% c(0,40,100)
hist(Y,nclass=max(10,sqrt(Nsim)))
mean(Y)
sd(Y)
mean(Y>0)
mean(Y>500)

Y <- Xdata %*% c(30,0,0)
hist(Y,nclass=max(10,sqrt(Nsim)))
mean(Y)
sd(Y)
mean(Y>0)
mean(Y>500)

Y <- Xdata %*% c(0,60,0)
hist(Y,nclass=max(10,sqrt(Nsim)))
mean(Y)
sd(Y)
mean(Y>0)
mean(Y>500)

Y <- Xdata %*% c(0,0,300)
hist(Y,nclass=max(10,sqrt(Nsim)))
mean(Y)
sd(Y)
mean(Y>0)
mean(Y>500)




