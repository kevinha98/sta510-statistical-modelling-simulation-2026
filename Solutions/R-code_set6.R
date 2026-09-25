#
# R-code for exercise set 6
#
rm(list=ls())

### Problem 0

# Bootstrap estimate for standard deviation and bias of the standard deviation
hardness <- c(168,185,164,182,169,181,172,185,172,180)
n <- length(hardness)

B <- 5000
sdestB <- numeric(B)
for(i in 1:B)
  sdestB[i] <- sd(sample(hardness,size=n,replace = TRUE))
hist(sdestB,prob=TRUE,nclass=sqrt(B)/2)
abline(v=sd(hardness),col="blue",lwd=2)
# Standard error
sd(sdestB)
# Bias
bias <- mean(sdestB)-sd(hardness)
bias
# Estimate
sd(hardness)
# Bias correted estimate
sd(hardness)-bias
abline(v=sd(hardness)-bias,col="green",lwd=2)




### Bootstrap confidence intervals for standard deviation
# Standard normal
error <- qnorm(0.975)*sd(sdestB)
lowerS <- sd(hardness)-error
upperS <- sd(hardness)+error

# Basic
lowerB <- 2*sd(hardness)-quantile(sdestB,0.975)
upperB <- 2*sd(hardness)-quantile(sdestB,0.025)

# Percentile
lowerP <- quantile(sdestB,0.025)
upperP <- quantile(sdestB,0.975)


method <- c("Standard normal","Basic","Percentile")
lowerCI <- round(as.vector(c(lowerS,lowerB,lowerP)),2)
upperCI <- round(as.vector(c(upperS,upperB,upperP)),2)
rbind(method,lowerCI,upperCI)


# Exact interval for standard deviation  under normal assumption
lowerSD <- sqrt((n-1)*sd(hardness)^2/qchisq(0.975,df=n-1))
upperSD <- sqrt((n-1)*sd(hardness)^2/qchisq(0.025,df=n-1))


method <- c("Standard normal","Basic","Percentile","Exact")
lowerCI <- round(as.vector(c(lowerS,lowerB,lowerP,lowerSD)),2)
upperCI <- round(as.vector(c(upperS,upperB,upperP,upperSD)),2)
rbind(method,lowerCI,upperCI)


# The same using the using the boot function (see appendix B1)
library(boot)
sdestfunc <- function(data,i)   # Need to write the estimator like this to use the boot function
  sd(data[i])
boot.obj <- boot(data=hardness,statistic = sdestfunc,R=5000)
boot.obj
boot.ci(boot.obj,type=c("norm","basic","perc","bca"))




#
# Problem 1
#

# new or changed lines are marked with #NEW#

# general 1d Gaussian proposal random walk MH
oneD.RWMH.mod <- function(lprob, #notice log-density kernel!
                      sigma=1.0,
                      theta1=0.0,
                      n.iter=10000){
  # space for output
  output <- numeric(n.iter)
  # first iterate given
  output[1] <- theta1
  lp.old <- lprob(theta1)  #NEW# evaluate at first iterate
  # main iteration loop
  for(t in 2:n.iter){
    # proposal
    thetaStar <- output[t-1] + rnorm(1,sd=sigma)
    # accept probability, for numerical stability we compute
    lp.star <- lprob(thetaStar) #NEW# evaluate at proposal
    # the log-accept prob, and then take exp
    alpha <- exp(min(0.0,lp.star-lp.old)) #NEW# use precalculated lprobs
    # accept/reject step
    if(runif(1)<alpha && is.finite(alpha)){
      output[t] <- thetaStar
      lp.old <- lp.star #NEW# update, "this iteration will be the old in next iteration"
    } else {
      output[t] <- output[t-1]
    }
  }
  print(paste0("RWMH done, accept prob : ",mean(abs(diff(output))>1.0e-14)))
  return(output)
} 

# test the modified implementation using standard normal
# target distribution
lprob <- function(x){return(-0.5*x^2)}
# run with many iterations
out <- oneD.RWMH.mod(lprob,sigma=2.4,theta1=0.0,n.iter=500000)
hist(out,probability = TRUE,breaks=50)
xg <- seq(from=-4,to=4,length.out = 1000)
lines(xg,dnorm(xg),col="red") 
# I.e. looks like nothing was broken...


#
# Problem 2
#

# p-target
lprob.p <- function(p){
  if(p<0.0 || p>1.0) return(-1e100) 
  return(431.0*log(p) + 4.0*log(1.0-p))
}
# run RWMH and compare to reference
out <- oneD.RWMH.mod(lprob.p,sigma=0.02,theta1=0.99,n.iter=500000)
# sigma = 0.02 seems like a good choice
par(mfrow=c(2,1))
hist(out,probability = TRUE)
xg <- seq(from=0.96,to=1.0,length.out = 1000)
lines(xg,dbeta(xg,shape1=432,shape2=5),col="red")
# looks correct, 

# mu-target
lprob.mu <- function(mu){
  if(mu<0.0) return(-1.0e100)
  return(431.0*log(mu/(1.0+mu)) - 6.0*log(1.0+mu))
}

out.mu  <- oneD.RWMH.mod(lprob.mu,sigma=150.0,theta1=0.99/(1.0-0.99),n.iter=500000)
# sigma=150 seems like a reasonable choice


#inverse transformation to obtain p from mu:
out.mu.p <- out.mu/(1.0+out.mu)

# plot transformed samples
hist(out.mu.p,probability = TRUE)
lines(xg,dbeta(xg,shape1=432,shape2=5),col="red")
# also looks correct

#
# Problem 3
#

# bivariate RWMH method
twoDRWMH <- function(lprob, # log-probability density kernel
                     Sigma=diag(2), # default proposal covariance = identity matrix
                     theta1=c(0.0,0.0), # default initial configuration
                     n.iter=10000){
  # allocate output space
  out <- matrix(0.0,n.iter,2)
  out[1,] <- theta1
  
  # store old lprob
  lp.old <- lprob(theta1)
  
  # cholesky factorization of Sigma for fast sampling
  L <- t(chol(Sigma)) #lower triangular factor
  
  # accept counter
  Nacc <- 0
  # main iteration loop
  for(i in 2:n.iter){
    # proposal
    thetaStar <- out[(i-1),] + L%*%rnorm(2)
    
    # evaluate
    lp.star <- lprob(thetaStar)
    
    # accept prob
    alpha <- exp(min(0.0,lp.star-lp.old))
    
    # accept/reject
    if(runif(1)<alpha && is.finite(alpha)){
      # accept
      out[i,] <- thetaStar
      lp.old <- lp.star
      Nacc <- Nacc+1
    } else {
      out[i,] <- out[(i-1),]      
    }
  } # main iteration loop
  
  print(paste0("RWMH done, accept rate : ",Nacc/(n.iter-1)))
  return(out)
} # function

# tests of the function

P <- matrix(c(1,1,1,2),2,2)
m <- c(1.0,-1.0)
Pinv <- solve(P)

### test 1 - bivariate normal distribution
lp_gauss <- function(x){return(-0.5*t(x-m)%*%P%*%(x-m))}

out.gauss <- twoDRWMH(lp_gauss,Sigma=2.4*Pinv,
                      theta1 = m,n.iter=100000)
# estimated mean
colMeans(out.gauss) 
m

# estimated covariance
cov(out.gauss)
Pinv

# check x1 marginal (should N(m[1],Pinv[1,1])) 
par(mfrow=c(1,1))
hist(out.gauss[,1],probability = TRUE)
xg <- seq(from=min(out.gauss[,1]),to=max(out.gauss[,1]),length.out = 1000)
lines(xg,dnorm(xg,mean=m[1],sd=sqrt(Pinv[1,1])),col="red")

### test 2 - t-distribution

lp_t <- function(x){return(-3.0*log(1.0+0.25*t(x-m)%*%P%*%(x-m)))}

out.t <- twoDRWMH(lp_t,Sigma=3.0*Pinv,
                      theta1 = m,n.iter=100000)
# check mean (should be = m)
colMeans(out.t) 
m

# check covariance (should be nu/(nu-2)*Pinv = 2*Pinv)
cov(out.t)
2*Pinv


#
#  Problem 4
#

# from exercise text
y <- c(1.2, 6.3, 5.4, 3.4, 7.5, 4.8, 1.9)
lprob <- function(theta){
  mu <- theta[1]
  tau <- theta[2] 
  if(tau<0.0) return(-1e100) # log target effectively - infinity for negative tau
  s.dev <- sqrt(1.0/tau)
  loglike <- sum(dnorm(y,mean=mu,sd=s.dev,log=TRUE)) 
  mu.log.pri <- dnorm(mu,mean=0,sd=10,log=TRUE) 
  tau.log.pri <- -tau # = log(exp(-tau)) 
  return(loglike+mu.log.pri+tau.log.pri) 
}

# run RWMH sampler from previous point
Sig <- matrix(c(0.8,0.0,0.0,0.014),2,2) # obtained from initial run with identity proposal cov
out.mu.tau <- twoDRWMH(lprob=lprob,Sigma=2.0*Sig,theta1 = c(mean(y),1.0/var(y)),n.iter = 100000)
# OK tuning

# make data frame for easier presentation
out.sigma <- 1.0/sqrt(out.mu.tau[,2])
out.df <- data.frame(out.mu.tau,out.sigma)
colnames(out.df) <- c("mu","tau","s")
# print summary statistics
summary(out.df)
# print standard deviations
print("standard deviations : ")
sqrt(diag(var(out.df)))


#scatterplot
plot(out.df$mu,out.df$s,pch=20,cex=0.1)

# credible interval for s
CI <- quantile(out.df$s,probs = c(0.025,0.975))
CI
