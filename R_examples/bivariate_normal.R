#### Demonstration of the bivariate normal distribution

# Remove old variables:
rm(list=ls())

# In this code we need functions in a library called mvtnorm
# To install this library run:
# install.packages("mvtnorm")
# You only need to do this library installation once

# Load the multivariate normal library 
library(mvtnorm)
# You have to load the library everytime you restart R 

# Specify expectation and covariance matrix
mu <- c(2.5,7.5)  # Expectation
sigma <- matrix(c(1,-0.6,-0.6,1),nrow=2) # Covariance matrix
# Code to generate contour plot of the density
x1 <- seq(0,5,length.out=100)
x2 <- seq(5,10,length.out=100)
z <- matrix(0,nrow=100,ncol=100)
for (i in 1:100) {
  for (j in 1:100) {
    z[i,j] <- dmvnorm(c(x1[i],x2[j]),
                      mean=mu,sigma=sigma)
  }
}
contour(x1,x2,z,xlab="x1",ylab="x2") #Make the plot
# We now simulate 1000 obs from this bivariate distributon and add to the plot
bivn <- rmvnorm(500, mean = mu, sigma = sigma) 
points(bivn)               

# Case with no correlation 
sigma <- matrix(c(1,0,0,1),nrow=2)
for (i in 1:100) {
  for (j in 1:100) {
    z[i,j] <- dmvnorm(c(x1[i],x2[j]),
                      mean=mu,sigma=sigma)
  }
}
contour(x1,x2,z,xlab="x1",ylab="x2")
bivn <- rmvnorm(500, mean = mu, sigma = sigma) 
points(bivn)               

# Strong correlation
sigma <- matrix(c(1,0.95,0.95,1),nrow=2)
for (i in 1:100) {
  for (j in 1:100) {
    z[i,j] <- dmvnorm(c(x1[i],x2[j]),
                      mean=mu,sigma=sigma)
  }
}
contour(x1,x2,z,xlab="x1",ylab="x2")
bivn <- rmvnorm(200, mean = mu, sigma = sigma) 
points(bivn)               


