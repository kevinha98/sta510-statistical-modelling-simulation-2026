#
# Illustration of the independence MH sampler
#

# uniform(-1,1) target, normal(0,sd=sigma) proposal 
target_prob <- function(theta){return(dunif(theta,min=-1))}

n.iter <- 10000
sigma <- 0.7 # change to illustrate pathological effects
res <- numeric(n.iter)
# allocate memory
res[1] <- 0.0
# old importance weight
wt.old <- target_prob(res[1])/dnorm(res[1],sd=sigma)
Nacc <- 0
for(i in 2:n.iter){
  # proposal (note, independent of past)
  thetaStar <- rnorm(1,sd=sigma)
  # new importance weight
  wt.star <- target_prob(thetaStar)/dnorm(thetaStar,sd=sigma)
  
  # accept probability
  alpha <- min(1.0,wt.star/wt.old)
  # accept/reject
  if(runif(1)<alpha){
    res[i] <- thetaStar
    wt.old <- wt.star
    Nacc <- Nacc+1
  } else {
    res[i] <- res[i-1]
  }
}
print(paste0("accept rate : ",Nacc/n.iter))
plot(res,pch=20,cex=0.1,xlab="MCMC iteration #")

#
# Langevin diffusion limit targeting a N(mu,sigma^2)
# corresponding to \nabla log g(theta) = -(theta-mu)/(sigma^2)
T <- 10 # length of interval
nstep <- 200 # vary this one
Delta <- T/(nstep)
ntraj <- 50000

mu <- 1.0
sigma <- 0.5
ssq <- sigma^2

theta <- matrix(0.0,ntraj,nstep+1)
theta[,1] <- rnorm(ntraj,mean=mu,sd=10*sigma) # initial distribution
# simulate process
for(i in 2:(nstep+1)){
  theta[,i] <- theta[,i-1] + 0.5*Delta*(-(theta[,i-1]-mu)/ssq) +
    sqrt(Delta)*rnorm(ntraj)
}
par(mfrow=c(1,2))
plot((0:nstep)*Delta,theta[1,],
     type="l",
     ylim=c(mu-4*sigma,mu+4*sigma),
     xlab="time",
     ylab="trajectories")
for(ii in 2:50){
  lines((0:nstep)*Delta,theta[ii,],col=ii)
}

hist(theta[,nstep+1],probability = TRUE,
     breaks=51,ylim=c(0,dnorm(mu,mu,sigma)),
     xlab = "last theta",main="")

xg <- seq(from=mu-5*sigma,to=mu+5*sigma,length.out = 1000)
lines(xg,dnorm(xg,mean=mu,sd=sigma),col="red")
sd(theta[,nstep+1])

#
# illustration of the MALA for general 1-D target distribution
#
rm(list=ls())

#Normal test case, returns both lprob and gradient
mu <- 1.0
sigma <- 0.5
lp_gauss <- function(theta){
  return(list(
    lp=-(theta-mu)^2/(2*sigma^2), # log-target (i.e. log g)
    grad=-(theta-mu)/(sigma^2) # log-g gradient
  ))
}


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

par(mfrow=c(2,1))
out.mala <- oneDMALA(lp_gauss,theta1=mu,Delta=0.7,n.iter = 50000)
ts.plot(out.mala)
hist(out.mala,probability = TRUE,breaks=30)
xg <- seq(from=mu-5*sigma,to=mu+5*sigma,length.out = 1000)
lines(xg,dnorm(xg,mean=mu,sd=sigma),col="red")

# try a chi^2-distribution with nu df

nu <- 7.0
lp_chisq <- function(theta){
  if(theta<=0.0) return(list(lp=-1e100,grad=0.0))
  return(list(
    lp = (0.5*nu-1)*log(theta) - 0.5*theta, # log-density kernel
    grad = (0.5*nu-1)/theta - 0.5 # gradient of log-density
  ))
}
out.mala.chi <- oneDMALA(lp_chisq,theta1=nu,Delta=30.0,n.iter = 50000)
ts.plot(out.mala.chi)
hist(out.mala.chi,probability = TRUE,breaks=30)
xg <- seq(from=0,to=30,length.out = 1000)
lines(xg,dchisq(xg,df=nu),col="red")


#
# Burn-in / warmup
#

# in general, we do not know very well where to start the 
# MCMC simulation, hence the first part of the simulation
# is typically spent "locating" the high density region


out.mala.chi <- oneDMALA(lp_chisq,theta1=100*nu,Delta=20.0,
                         n.iter = 5000)
ts.plot(out.mala.chi[1:1000])
hist(out.mala.chi,probability = TRUE,breaks=30)
xg <- seq(from=0,to=30,length.out = 1000)
lines(xg,dchisq(xg,df=nu),col="red")

# throw away the first 200 samples as burn-in
# "the transient regime"
out.mala.chi.pbi <- out.mala.chi[201:length(out.mala.chi)]
ts.plot(out.mala.chi.pbi)
hist(out.mala.chi.pbi,probability = TRUE,breaks=30)
xg <- seq(from=0,to=30,length.out = 1000)
lines(xg,dchisq(xg,df=nu),col="red")
# now, the remaining chain looks stable
# only the stationary regime remains

#
# Effective sample size
# 

# from exercise set 5
ESS <- function(x){ return(as.numeric(coda::effectiveSize(x))) }
ESS(out.mala.chi.pbi)
ESS(out.mala.chi.pbi)/length(out.mala.chi.pbi) #not too bad


ESS(oneDMALA(lp_gauss,theta1=mu,Delta=1.0*0.7,n.iter = 5000))




# lets see what happens if we do not have stationarity:
# Random walk
X <- cumsum(c(0,rnorm(9999)))
ts.plot(X)
ESS(X)
ESS(X)/length(X)



#
# ESS of transformed output
#
# wish to calculate second moment E(X^2) of chi^2 variable with
# nu=7 df (should be 2*nu+nu^2 = 63)

# estimate
mean(out.mala.chi.pbi^2)

ess <- ESS(out.mala.chi.pbi^2) # ESS of estimate
sdev <- sd(out.mala.chi.pbi^2) 
sdev/sqrt(ess) # standard deviation of estimate

# error in estimate
mean(out.mala.chi.pbi^2) - (2*nu+nu^2)

#
# probability of X<nu = pchisq(q=nu,df=nu) = 0.5711201
#

# estimate 
mean(out.mala.chi.pbi<nu)

# for illustration
par(mfrow=c(1,1))
ts.plot((out.mala.chi.pbi<nu)[1:100])

ess <- ESS(as.numeric(out.mala.chi.pbi<nu))
sdev <- sd(as.numeric(out.mala.chi.pbi<nu))
sdev/sqrt(ess) # standard deviation of estimate

# error in estimate
pchisq(q=nu,df=nu)-mean(out.mala.chi.pbi<nu)



#
# convergence tests 
#
library(coda)
#
# Geweke (single chain test)
#
# make an mcmc object used by the coda package
chain.mcmc <- mcmc(data=out.mala.chi.pbi)
varnames(chain.mcmc) <- "theta"
# returns z-score (to be compared two-sided with standard normal)
geweke.diag(chain.mcmc)
# check if we should discard more burn-in
geweke.plot(chain.mcmc) 

# same exercise with random walk
rw.mcmc <- mcmc(cumsum(c(0,rnorm(9999))))
varnames(rw.mcmc) <- "RW"
geweke.diag(rw.mcmc)
geweke.plot(rw.mcmc)

#
# Gelman-Rubin (ANOVA type test)
#
# requires multiple independent chains
chain <- oneDMALA(lp_chisq,theta1=100*nu,Delta=20.0,
                         n.iter = 5000)
mcmc1 <- mcmc(chain)
varnames(mcmc1) <- "theta"

chain <- oneDMALA(lp_chisq,theta1=100*nu,Delta=20.0,
                  n.iter = 5000)
mcmc2 <- mcmc(chain)
varnames(mcmc2) <- "theta"

chain <- oneDMALA(lp_chisq,theta1=100*nu,Delta=20.0,
                  n.iter = 5000)
mcmc3 <- mcmc(chain)
varnames(mcmc3) <- "theta"

chain <- oneDMALA(lp_chisq,theta1=100*nu,Delta=20.0,
                  n.iter = 5000)
mcmc4 <- mcmc(chain)
varnames(mcmc4) <- "theta"

#make mcmc.list object
ml <- mcmc.list(mcmc1,mcmc2,mcmc3,mcmc4)
summary(ml)

gelman.diag(ml)
gelman.plot(ml)


#
# the same with random walk
#
rw.ml <- mcmc.list(mcmc(cumsum(c(0,rnorm(9999)))),
                   mcmc(cumsum(c(0,rnorm(9999)))),
                   mcmc(cumsum(c(0,rnorm(9999)))),
                   mcmc(cumsum(c(0,rnorm(9999)))))

gelman.diag(rw.ml)
gelman.plot(rw.ml)


