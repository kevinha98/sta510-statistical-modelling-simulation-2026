# clear all variables
rm(list=ls())

##### Examples on Monte Carlo integration    ######


### Integrate cos(x) from 0 to 1

x <- runif(100000)

mean(cos(x))

# Exact: 
sin(1)


### Integrate cos(x) from a to b

cosint <- function(Nsim=10000,a,b){
  x <- runif(Nsim,a,b)
  intres <- (b-a)*mean(cos(x))
  print(paste("Integrating cos(x) from ",a,"to ",b,"."))
  print(paste("Approximate results based on", Nsim,"simulations:",round(intres,digits=6)))
  print(paste("Exact result", round(sin(b)-sin(a),digits=6)))
}

cosint(a=0,b=1)
cosint(a=0,b=2)
cosint(a=pi,b=2*pi)
cosint(a=10,b=11)


### Find the cdf of gamma(4,0.5)-distribution

# Version 1
approxpg1 <- function(xvec,Nsim=10000){
  cdf <- numeric(length(xvec)) 
  for(i in 1:length(xvec)){
    u <- runif(Nsim,0,xvec[i])
    cdf[i] <- xvec[i]*mean((8/3)*u^3*exp(-2*u))
  }
  cdf  
}
  

# Version 2
approxpg2 <- function(xvec,Nsim=10000){
  cdf <- numeric(length(xvec)) 
  u <- rgamma(Nsim,shape=4,scale=0.5)
  for(i in 1:length(xvec))
    cdf[i] <- mean(u<xvec[i])
  cdf  
}

# Test out the two versions
xvec <- seq(0.25,5,by=0.25)
cdf1 <- approxpg1(xvec)
cdf2 <- approxpg2(xvec)
exact <- pgamma(xvec,shape=4,scale=0.5)
round(rbind(xvec,cdf1,cdf2,exact),3)

plot(xvec,cdf1,type="l", col="blue")
lines(xvec,cdf2,col="red")
lines(xvec,exact)
legend("bottomright",c("Version 1","Version 2", "Exact"), 
       col=c("blue","red","black"),lty=1)


### Integrate x^5exp(-3x) from 0 to infinity
x <- rexp(10000,rate=3)
mean(x^5)/3


### Integrate function of three variables from lecture notes
n <- 100000
x <- runif(n,-1,1)
y <- runif(n,2,3)
z <- runif(n,0,1)
int <- 2*1*1*mean(x*y+y*z)
int


### MC integration on the unit sphere
### Estimate the area of a spherical cap
rm(list=ls())
set.seed(123)

n <- 100000
c <- 0.5

# Generate uniform directions on S^2:
Z <- matrix(rnorm(3*n), ncol=3)
X <- Z / sqrt(rowSums(Z^2))

# Indicator for the spherical cap x_3 > c:
H <- X[,3] > c

# Monte Carlo estimate:
Ahat <- 4*pi*mean(H)
Ahat

# Exact value:
Aexact <- 2*pi*(1-c)
Aexact

# Estimated Monte Carlo standard error:
phat <- mean(H)
MCSE <- 4*pi*sqrt(phat*(1-phat)/n)
MCSE

# Approximate 95% Monte Carlo uncertainty interval:
Ahat + c(-1,1)*qnorm(0.975)*MCSE




####################################################
### Not covered

### Integrate cos(x) from 0 to 1.5 with hit and miss

n <- 10000
y <- runif(n,0,1)
x <- runif(n,0,1.5)
z <- y <= cos(x) # hit or miss
1.5*mean(z)
# Exact
sin(1.5)


# Illustration of the above hit and miss approach
plot(x, y, col='blue', pch=20, xlab="x", ylab="y")
curve(cos(x), 0,1.5, n=100, col='red', lwd=2,add=TRUE)

plot(x[!z], y[!z], col='blue', pch=20, xlab="x", ylab="y") # those above cos(x)
points(x[z], y[z], col='green', pch=20)  # those below cos(x)
curve(cos(x), 0,1.5, n=100, col='red', lwd=2,add=TRUE)
# Estimate
1.5*mean(z)
# Exact
sin(1.5)



### Integrate cos(x) from 0 to 1.5 
### Compare SD with crude MC versus hit or miss

a <- 0
b <- 1.5
Nsim <- 10000

# Crude MC
x <- runif(Nsim,a,b)
intcMC <- (b-a)*mean(cos(x))
sdg <- sd(cos(x))
sdcMC <- sdg*(b-a)/sqrt(Nsim) 

# Hit or miss
c <- 1
y <- runif(Nsim,0,c)
x <- runif(Nsim,a,b)
z <- y <= cos(x) # hit or miss
intHM <- c*(b-a)*mean(z) # hit or miss estimate
phat <- mean(z) # estimated proportion of hit
sdHM <- c*(b-a)*sqrt(phat*(1-phat))/sqrt(Nsim)

# Exact
sin(1.5)

type <- c("Crude MC","Hit or miss","Exact")
int <- round(c(intcMC,intHM,sin(b)-sin(a)),4)
sd <- round(c(sdcMC,sdHM,0),4)
rbind(type,int,sd)



### Estimate SD of the estimators by repeating the estimation many times
c <- 1
nrep <- 200
intcMCvec <- numeric(nrep)
intHMvec <- numeric(nrep)
for(i in 1:nrep){
  x <- runif(Nsim,a,b)
  intcMCvec[i] <- (b-a)*mean(cos(x)) # crude MC result
  y <- runif(Nsim,0,c)
  z <- y <= cos(x) # hit or miss
  intHMvec[i] <- c*(b-a)*mean(z) # hit or miss result
}
# Estimated standard deviations
sd(intcMCvec)
sd(intHMvec)

# Checking the mean
mean(intcMCvec)
mean(intHMvec)



# To find c in hit or miss when it is not easy to calculate the max of the function: 
# Plot the function using the curve function in R. Pick a c a bit above the max
# point seen in the plot.
curve(2*cos(x)^2*exp(2*x^(0.5)),0,1.5)

      
