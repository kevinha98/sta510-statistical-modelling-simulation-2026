# clear all variables
rm(list=ls())

################################################################
#### Problem set 7
################################################################


################################################################
###--- Problem 8.4 (Rizzo)
library(boot)
aircondit
timesbetween <- aircondit$hours
n <- length(timesbetween)
lambdaest <- n/sum(timesbetween)
lambdaest

B <- 2000
lambdaestB <- numeric(B)
for(i in 1:B)
  lambdaestB[i] <- n/sum(sample(timesbetween,size=n,replace = TRUE))
hist(lambdaestB,prob=TRUE)
# Standard error
sd(lambdaestB)
# Bias
mean(lambdaestB)-lambdaest

# The same using the boot function (see appendix B1)
lambdaestfunc <- function(tdata,i)
  length(tdata[i])/sum(tdata[i])
boot(data=timesbetween,statistic = lambdaestfunc,R=2000)


################################################################
###--- Problem 8.5 (Rizzo)
# note: CI for inverse of lambda
inv.lambdaestfunc <- function(tdata,i)
  mean(tdata[i])

boot.obj <- boot(data=timesbetween,statistic = inv.lambdaestfunc,R=2000)
boot.obj
boot.ci(boot.obj,type=c("norm","basic","perc","bca"))



################################################################
###--- Problem 11.3 (Rizzo)
library(coda) # mcmc utilites


# 1-d MALA method from the lectures

oneDMALA <- function(lp_grad,
                     theta1 = 0.0,
                     Delta = 0.5,
                     n.iter=10000){

  # storage
  out <- numeric(n.iter)
  out[1] <- theta1
  old.lpg <- lp_grad(theta1)

  Nacc <- 0

  for( i in 2:n.iter){
    # proposal
    thetaStar <- out[i-1] + 0.5*Delta*old.lpg$grad +
      sqrt(Delta)*rnorm(1)
    # new eval at proposal
    new.lpg <- lp_grad(thetaStar)

    # proposal density forward
    lqf <- dnorm(thetaStar,mean = out[i-1] + 0.5*Delta*old.lpg$grad,
                 sd = sqrt(Delta),log=TRUE)
    # proposal density backward
    lqb <- dnorm(out[i-1],mean = thetaStar + 0.5*Delta*new.lpg$grad,
                 sd = sqrt(Delta),log=TRUE)

    # accept probability
    alpha <- exp(min(0.0,
                     new.lpg$lp + lqb # numerator
                     -old.lpg$lp - lqf # denominator
    ))

    if(runif(1)<alpha && is.finite(alpha)){
      out[i] <- thetaStar
      old.lpg <- new.lpg
      Nacc <- Nacc + 1
    } else {
      out[i] <- out[i-1]
    }

  } # main iteration loop
  print(paste0("MALA done, accept rate :",Nacc/(n.iter-1)))
  return(out)
} # function

# target function
lpg <- function(x){
  return(list(lp=-log(1.0+x^2), #no need for normalizing constant
              grad=-2.0*x/(1.0+x^2)))
}

# the distribution is quite hard to sample due to very heavy tails,
# hence use many iterations

mala.out <- oneDMALA(lp_grad=lpg,theta1=rcauchy(1),Delta=4.0,n.iter = 100000)
# plot mcmc quantiles against theoretical deciles
par(mfrow=c(1,1))
probs <- 0.01*(11:89)
plot(qcauchy(probs),quantile(mala.out,probs))
abline(0,1)
# looks quite good, note, tails very difficult to get properly represented

# now run a few more chains to be able to run proper tests

mcmc1 <- mcmc(mala.out)
varnames(mcmc1)<- 'x'
mala.out <- oneDMALA(lp_grad=lpg,theta1=rcauchy(1),Delta=4.0,n.iter = 100000)
mcmc2 <- mcmc(mala.out)
varnames(mcmc2)<- 'x'
mala.out <- oneDMALA(lp_grad=lpg,theta1=rcauchy(1),Delta=4.0,n.iter = 100000)
mcmc3 <- mcmc(mala.out)
varnames(mcmc3)<- 'x'
mala.out <- oneDMALA(lp_grad=lpg,theta1=rcauchy(1),Delta=4.0,n.iter = 100000)
mcmc4 <- mcmc(mala.out)
varnames(mcmc4)<- 'x'

ml <- mcmc.list(mcmc1,mcmc2,mcmc3,mcmc4)

effectiveSize(ml)
# not super-good due to the heavy tails
traceplot(ml)
# look at trace plot
geweke.diag(ml)
# borderline OK
gelman.diag(ml)
# borderline OK



################################################################
###--- Problem 11.6 (Rizzo)

# write from scratch specific for standard laplace

laplaceRWMH <- function(x1=0.0,sigma=4.0,n.iter=10000){
  out <- numeric(n.iter)
  out[1] <- x1
  lpold <- -abs(x1)
  nacc <- 0
  for(i in 2:n.iter){
    xprop <- out[i-1] + sigma*rnorm(1)
    lpprop <- -abs(xprop)
    if(runif(1)<exp(min(0.0,lpprop-lpold))){
      out[i] <- xprop
      lpold <- lpprop
      nacc <- nacc+1
    } else {
      out[i] <- out[i-1]
    }
  }
  print(paste0("accept prob : ",nacc/(n.iter-1)))
  return(out)
}

out <- laplaceRWMH()
effectiveSize(out)

# too small propopsal variance
out.1 <- laplaceRWMH(sigma=1.0)
effectiveSize(out.1)

#too large proposal variance
out.20 <- laplaceRWMH(sigma=20.0)
effectiveSize(out.20)



# run more chains for diagnostics

mcmc1 <- mcmc(laplaceRWMH())
mcmc2 <- mcmc(laplaceRWMH())
mcmc3 <- mcmc(laplaceRWMH())
mcmc4 <- mcmc(laplaceRWMH())

ml <- mcmc.list(mcmc1,mcmc2,mcmc3,mcmc4)

effectiveSize(ml)
# quite OK
traceplot(ml)
# look at trace plot
geweke.diag(ml)
# OK
gelman.diag(ml)
# OK


################################################################
###--- Problem 1

# from exercise set
# simulate data
set.seed(123)
n <- 100
x <- runif(n,min=-1)
y <- rexp(n,rate=1.0/exp(0.4+0.5*x))

# log-target distribution (theta=(alpha,beta))
lp <- function(theta){
  alpha <- theta[1]
  beta <- theta[2]
  log.like <- sum(dexp(y,rate=1.0/exp(alpha+beta*x),log=TRUE))
  log.prior <- dnorm(alpha,sd=10.0,log=TRUE) + dnorm(beta,sd=10.0,log=TRUE)
  return(log.like+log.prior)
}
# end from exercise set


#
#  point 1,a)
#

# bivariate RWMH method (from exercise set 7)
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

# initial run to get a reasonable estimate of the covariance
sim <- twoDRWMH(lprob = lp)
init.theta <- colMeans(sim)
init.cov <- cov(sim)

# main run
#
sim <- twoDRWMH(lprob = lp,Sigma = 4.0*init.cov, theta1=init.theta)
colnames(sim) <- c("alpha","beta")
mcmc1 <- mcmc(sim)

#
# point 1.b)
#

# check trace plots
par(mfrow=c(2,1))
traceplot(mcmc1)
# removing burn in does not seem neccessary, as we already started
# the last MCMC run close to the posterior mean

# check diagnostics and ESS
effectiveSize(mcmc1)
geweke.diag(mcmc1)
# all look OK, with ESS>1000

#
# 1.c)
#

# predictive mean distribution
Eystar <- exp(mcmc1[,1]+0.5*mcmc1[,2])
par(mfrow=c(1,1))
hist(Eystar)
mean(Eystar)
sd(Eystar)


#
# 1.d)
#
# predictive distribution of new observation
ystar <- Eystar*rexp(length(Eystar))
hist(ystar)
mean(ystar)
sd(ystar)
# sought probability
mean(ystar>5.0)

#
# 1.e)
#
xgrid <- seq(from=-1,to=1,by=0.05)
sta <- matrix(0.0,nrow=length(xgrid),3)

for(i in 1:length(xgrid)){
  sta[i,] <- quantile(exp(mcmc1[,1]+xgrid[i]*mcmc1[,2]),probs = c(0.05,0.5,0.95))
}
par(mfrow=c(1,1))
pdf("pred_mean.pdf")
plot(xgrid,sta[,2],type="l",lwd=2,ylim=c(0,4),
     main="predictive mean",ylab="mean",xlab="x") # median
lines(xgrid,sta[,1],col="red")
lines(xgrid,sta[,3],col="green")
legend("topleft",legend=c("median","q05","q95"),
       col=c("black","red","green"),lty=c(1,1,1),lwd=c(2,1,1))
dev.off()


#
# 1.f)
#
xgrid <- seq(from=-1,to=1,by=0.05)
sta <- matrix(0.0,nrow=length(xgrid),3)
eps <- rexp(length(mcmc1[,1]))


for(i in 1:length(xgrid)){
  sta[i,] <- quantile(eps*exp(mcmc1[,1]+xgrid[i]*mcmc1[,2]),probs = c(0.05,0.5,0.95))
}
par(mfrow=c(1,1))
pdf("pred_obs.pdf")
plot(xgrid,sta[,2],type="l",lwd=2,ylim=c(0,9),
     main="predictive new observation",ylab="new observation",xlab="x") # median
lines(xgrid,sta[,1],col="red")
lines(xgrid,sta[,3],col="green")
legend("topleft",legend=c("median","q05","q95"),
       col=c("black","red","green"),lty=c(1,1,1),lwd=c(2,1,1))
dev.off()





