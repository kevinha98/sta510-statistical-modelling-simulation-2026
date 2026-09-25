# clear all variables
rm(list=ls())
#######################

############
# Wave data
############
wavedata <- c(3.5, 2.0, 3.3, 1.8, 1.6, 1.5, 2.0, 0.8, 2.2, 1.1, 
              1.8, 1.1, 1.9, 4.0, 2.5, 1.9, 1.5, 3.5, 1.2, 5.3)

thetaest1 <- function(data) #MLE Estimator of theta
  sqrt(mean(data^2)/2)

estimate <- round(thetaest1(wavedata),3)#The estimate of theta based on the data

# Bootstrap estimate for standard deviation and bias
n <- length(wavedata)
B <- 5000
thetaest1B <- numeric(B)
for(i in 1:B){
  thetaest1B[i] <- thetaest1(sample(wavedata,size=n,replace=TRUE))
}

hist(thetaest1B,prob=TRUE,nclass=sqrt(B)/2)  
abline(v=thetaest1(wavedata),col="blue",lwd=2)

# Standard error
sd <- round(sd(thetaest1B),3)

# Bias
bias <- round(mean(thetaest1B)-thetaest1(wavedata),3)

bcorrest <- estimate-bias 

estimator <- "Theta est 1"
rbind(estimator,estimate,sd,bias,bcorrest)

# Bootstrap confidence intervals

#Normal
error <- qnorm(0.975)*sd
lowerN <- round(estimate-error,3)
upperN <- round(estimate+error,3)

# Basic
lowerB <- round(2*estimate-quantile(thetaest1B,0.975),3)
upperB <- round(2*estimate-quantile(thetaest1B,0.025),3)

# Percentile
lowerCI <- round(quantile(thetaest1B,0.025),3)
upperCI <- round(quantile(thetaest1B,0.975),3)

intervals <- c("Normal L", "Normal U", "Basic L", "Basic U", "Percentile L", "Percentile U")
rbind(intervals, c(lowerN, upperN, lowerB, upperB, lowerCI, upperCI))


# Using the boot function
library(boot)

thetaest1func <- function(data,i)  # Need to write the estimator like this to use the boot function
  sqrt(mean(data[i]^2)/2)

boot.obj1 <- boot(data=wavedata,statistic = thetaest1func,R=5000)
boot.obj1

boot.ci(boot.obj1,type=c("norm","basic","perc","bca"))


########################################################
# Testing the Estimators for the Rayleigh distribution
########################################################

# Function to simulate data from the Rayleigh distribution
genrayleighdistr <- function(Nsim,theta){
  U <- runif(Nsim)
  X <- theta*sqrt(-log(1-U)*2)
  return(X)
}

# The two estimators
thetaest1 <- function(data)
  sqrt(mean(data^2)/2)
thetaest2 <- function(data)
  mean(data)*sqrt(2/pi)

# Test out for increasing data sizes
theta <- 1.5
for(n in c(10,100,1000,10000,100000)){
  rdata <- genrayleighdistr(n,theta)
  print(paste("n=",n,", theta1est=",round(thetaest1(rdata),5),
              ", theta2est=",round(thetaest2(rdata),5)))
}

# Compare bias, SD and MSE
Nsim <- 50000
ndata <- 2000
theta <- 1.5
thetaest1vec <- numeric(Nsim)
thetaest2vec <- numeric(Nsim)
for(i in 1:Nsim){
  rdata <- genrayleighdistr(ndata,theta)
  thetaest1vec[i] <-thetaest1(rdata)
  thetaest2vec[i] <-thetaest2(rdata)
}
estimator <- c("Theta estimator 1","Theta estimator 2")
bias <- round(c(mean(thetaest1vec)-theta,mean(thetaest2vec)-theta),4)
sd <- round(c(sd(thetaest1vec),sd(thetaest2vec)),4)
MSE <- round(c(mean((thetaest1vec-theta)^2),mean((thetaest2vec-theta)^2)),4)
rbind(estimator,bias,sd,MSE)

# Repeat the lines above for different values of theta and n.


############
# Both estimators applied to the Wave data
############

wavedata <- c(3.5, 2.0, 3.3, 1.8, 1.6, 1.5, 2.0, 0.8, 2.2, 1.1, 
              1.8, 1.1, 1.9, 4.0, 2.5, 1.9, 1.5, 3.5, 1.2, 5.3)
thetaest1(wavedata)
thetaest2(wavedata)

# Bootstrap estimate for standard deviation and bias
n <- length(wavedata)
B <- 5000
thetaest1B <- numeric(B)
thetaest2B <- numeric(B)
for(i in 1:B){
  thetaest1B[i] <- thetaest1(sample(wavedata,size=n,replace=TRUE))
  thetaest2B[i] <- thetaest2(sample(wavedata,size=n,replace=TRUE))
}
par(mfrow=c(1,2))
hist(thetaest1B,prob=TRUE,nclass=sqrt(B)/2)  
abline(v=thetaest1(wavedata),col="blue",lwd=2)
hist(thetaest2B,prob=TRUE,nclass=sqrt(B)/2)  
abline(v=thetaest2(wavedata),col="blue",lwd=2)
par(mfrow=c(1,1))
# Standard error
sd <- round(c(sd(thetaest1B),sd(thetaest2B)),3)
# Bias
bias <- round(c(mean(thetaest1B)-thetaest1(wavedata),mean(thetaest2B)-thetaest2(wavedata)),3)

estimate <- round(c(thetaest1(wavedata),thetaest2(wavedata)),3)
bcorrest <- estimate-bias 
estimator <- c("Theta est 1","Theta est 2")
rbind(estimator,estimate,sd,bias,bcorrest)


# Bootstrap confidence intervals
# Percentile
lowerCI <- round(c(quantile(thetaest1B,0.025),quantile(thetaest2B,0.025)),3)
upperCI <- round(c(quantile(thetaest1B,0.975),quantile(thetaest2B,0.975)),3)
rbind(estimator,estimate,sd,bias,bcorrest,lowerCI,upperCI)


# Using the boot function
library(boot)
#Estimator 1
thetaest1func <- function(data,i)  # Need to write the estimator like this to use the boot function
  sqrt(mean(data[i]^2)/2)
boot.obj1 <- boot(data=wavedata,statistic = thetaest1func,R=5000)
boot.obj1
boot.ci(boot.obj1,type=c("norm","basic","perc","bca"))
# Estimator 2
thetaest2func <- function(data,i)  # Need to write the estimator like this to use the boot function
  mean(data[i])*sqrt(2/pi)
boot.obj2 <- boot(data=wavedata,statistic = thetaest2func,R=5000)
boot.obj2
boot.ci(boot.obj2,type=c("norm","basic","perc","bca"))


########################################################
# Hardness of material
########################################################

hardness <- c(168,185,164,182,169,181,172,185,172,180)
mean(hardness)
sd(hardness)
n <- length(hardness)
# Standard devation of the mean
sd(hardness)/sqrt(n)

# Check with QQ-plot if the data seem to follow a normal distribution
plot(qqnorm(hardness))
qqline(hardness)

# Traditional confidence interval for expecation under normal assumption
error <- qt(0.975,df=n-1)*sd(hardness)/sqrt(n)
lowerE <- mean(hardness)-error
upperE <- mean(hardness)+error
lowerE
upperE

# Bootstrap estimate for standard deviation of the mean
B <- 5000
meanestB <- numeric(B)
for(i in 1:B)
  meanestB[i] <- mean(sample(hardness,size=n,replace = TRUE))
hist(meanestB,prob=TRUE,nclass=sqrt(B)/2)  
abline(v=mean(hardness),col="blue",lwd=2)
# Standard error
sd(meanestB)
# Bias
mean(meanestB)-mean(hardness)



### Bootstrap confidence intervals for expectation
# Standard normal
error <- qnorm(0.975)*sd(meanestB)
lowerS <- mean(hardness)-error
upperS <- mean(hardness)+error

# Basic
lowerB <- 2*mean(hardness)-quantile(meanestB,0.975)
upperB <- 2*mean(hardness)-quantile(meanestB,0.025)

# Percentile
lowerP <- quantile(meanestB,0.025)
upperP <- quantile(meanestB,0.975)

method <- c("Standard normal","Basic","Percentile","Exact")
lowerCI <- round(as.vector(c(lowerS,lowerB,lowerP,lowerE)),2)
upperCI <- round(as.vector(c(upperS,upperB,upperP,upperE)),2)
rbind(method,lowerCI,upperCI)


# The same using the using the boot function (see appendix B1)
library(boot)
meanestfunc <- function(data,i)   # Need to write the estimator like this to use the boot function
  mean(data[i])
boot.obj <- boot(data=hardness,statistic = meanestfunc,R=5000)
boot.obj
boot.ci(boot.obj,type=c("norm","basic","perc","bca"))



########################################################
#  Bootstrap uncertainty for the intrinsic Frechet mean on S^2
########################################################
library(Directional)
library(Riemann)

rm(list=ls())
set.seed(1)
set.seed(123)

# Simulate von Mises-Fisher data on S^2
n <- 100
mu <- c(1, 1, 1)
mu <- mu / sqrt(sum(mu^2))
kappa <- 8
X <- rvmf(n, mu, kappa)

# Geodesic distance on S^2
d.angle <- function(x, y){
  z <- sum(x*y)
  # Protect against numerical values slightly outside [-1,1]:
  #z <- max(-1, min(1, z))
  acos(z)
}

# Intrinsic Frechet mean
mhat <- riem.mean(wrap.sphere(X), geometry = "intrinsic")$mean
mhat <- as.vector(mhat)
mhat 

# Bootstrap
B <- 2000
mboot <- matrix(NA, B, 3)
for(b in 1:B){
  Xboot <- X[sample(1:n, n, replace=TRUE), ]
  mboot[b,] <- as.vector(
    riem.mean(
      wrap.sphere(Xboot),
      geometry = "intrinsic"
    )$mean
  )
}


# Angular distance between each bootstrap mean and observed mean:
D <- apply(mboot, 1, function(m) d.angle(m, mhat))

# 95% bootstrap uncertainty radius:
quantile(D, 0.95)*180/pi

# Visualization
clear3d()
# Original observations
sphereplot(X)
# Bootstrap Frechet means
rgl::points3d(mboot[,1],mboot[,2],mboot[,3],size = 3,col="blue")
# Estimated Frechet mean
rgl::points3d(mhat[1],mhat[2],mhat[3],size = 12,col="red")
# True mean direction used to simulate the data
rgl::points3d(mu[1],mu[2],mu[3],size = 12,col="green")
# rglwidget()  # Use WebGL output if the native rgl/OpenGL window is unavailable



# ------------------------------------------------------------
# Intrinsic Frechet mean
# Parameterize a point on S^2 by
#   phi    = colatitude
#   lambda = longitude
# ------------------------------------------------------------

frechet.mean <- function(X){
  
  # Starting value: extrinsic mean direction
  m0 <- colMeans(X)
  m0 <- m0 / sqrt(sum(m0^2))
  
  phi0 <- acos(m0[3])
  lambda0 <- atan2(m0[2], m0[1])
  
  objective <- function(par){
    
    phi <- par[1]
    lambda <- par[2]
    
    m <- c(
      sin(phi)*cos(lambda),
      sin(phi)*sin(lambda),
      cos(phi)
    )
    
    prod <- X %*% m
    
    # Numerical protection:
    prod <- pmax(-1, pmin(1, prod))
    
    mean(acos(prod)^2)
  }
  
  fit <- optim(
    c(phi0, lambda0),
    objective,
    method = "L-BFGS-B",
    lower = c(0, -pi),
    upper = c(pi, pi)
  )
  
  phi <- fit$par[1]
  lambda <- fit$par[2]
  
  c(
    sin(phi)*cos(lambda),
    sin(phi)*sin(lambda),
    cos(phi)
  )
}


# Observed Frechet mean:
mhat <- frechet.mean(X)
mhat


myriem = wrap.sphere(x)
mean.int = as.vector(riem.mean(myriem, geometry="intrinsic")$mean)
points(mean.int[1],mean.int[2], col="blue", pch=19)
#mean.ext = as.vector(riem.mean(myriem, geometry="extrinsic")$mean)
#points(mean.ext[1],mean.ext[2], col="green", pch=17)


# Generate sample in d=3
require("rgl")
x <- runif.sphere(200,3)
spheres3d(0,0,0,lit=FALSE,color="white")
spheres3d(0,0,0,radius=1.01,lit=FALSE,color="black",front="lines")
spheres3d(x[,1],x[,2],x[,3],col="red",radius=0.02)
myriem = wrap.sphere(x)
mean.int = as.vector(riem.mean(myriem, geometry="intrinsic")$mean)

# ------------------------------------------------------------
# Bootstrap
# ------------------------------------------------------------

B <- 2000

mboot <- matrix(NA, B, 3)

for(b in 1:B){
  
  ind <- sample(1:n, n, replace=TRUE)
  
  Xboot <- X[ind, ]
  
  mboot[b,] <- frechet.mean(Xboot)
}

# Angular distance between each bootstrap mean and observed mean:
D <- apply(mboot, 1, function(m) angle(m, mhat))

# 95% bootstrap uncertainty radius:
radius <- quantile(D, 0.95)

radius                         # radians
radius * 180/pi 
