#
# This R-script illustrates various MCMC methods
# 
#

# general 1d Gaussian proposal random walk MH
oneD.RWMH <- function(lprob, #notice log-density kernel!
                     sigma=1.0,
                     theta1=0.0,
                     n.iter=10000){
  # space for output
  output <- numeric(n.iter)
  # first iterate given
  output[1] <- theta1
  
  # main iteration loop
  for(t in 2:n.iter){
    # proposal
    thetaStar <- output[t-1] + rnorm(1,sd=sigma)
    # accept probability, for numerical stability we compute
    # the log-accept prob, and then take exp
    alpha <- exp(min(0.0,lprob(thetaStar)-lprob(output[t-1])))
    # accept/reject step
    if(runif(1)<alpha && is.finite(alpha)){
      output[t] <- thetaStar
    } else {
      output[t] <- output[t-1]
    }
  }
  return(output)
} 

# standard Gaussian target

lp_std_norm <- function(x){return(dnorm(x,log=TRUE))}

# try algorithm (change initial condition and sigma for illustration)
out <- oneD.RWMH(lp_std_norm,theta1 = 0.0,sigma=2.4)
par(mfrow=c(2,1))
plot(1:length(out),out,pch=20,cex=0.1,xlab="MCMC iteration #")
hist(out)
mean(out)
sd(out)

# uniform(-1,1) target

lp_unif <- function(x){return(dunif(x,min=-1,log=TRUE))}
# notice
lp_unif(-2.0)

# try algorithm 

out <- oneD.RWMH(lp_unif,theta1 = 0.0,sigma=2.4)
par(mfrow=c(2,1))
plot(1:length(out),out,pch=20,cex=0.1,xlab="MCMC iteration #")
hist(out)
mean(out) # should be 0
sd(out) # should be sqrt(1/3) = 0.5773503

###


# standard Gaussian target with a gap
# illustrates that we do not need a proper density
# for the algorithm to work.


lp_std_norm_gap <- function(x){return(dnorm(x,log=TRUE) - 1e100*(x<0.5 & x>-0.5))}

# try MCMC algorithm 
out <- oneD.RWMH(lp_std_norm_gap,theta1 = 1.0,sigma=2.4,n.iter = 50000)
par(mfrow=c(2,1))
plot(1:length(out),out,pch=20,cex=0.1,xlab="MCMC iteration #")
hist(out,probability = TRUE,ylim=c(0,0.6))

# since the problem is 1d, we can cheat using numerical integration
# to get some 
gfun <- function(x){return(exp(lp_std_norm_gap(x)))}
C <- integrate(gfun,lower=-10,upper=10)$value

pi.fun <- function(x){return(exp(lp_std_norm_gap(x))/C)}
# add to histogram
xg <- seq(from =-4,to=4,length.out = 10000)
lines(xg,pi.fun(xg))

# compute exact moments
e.mean <- integrate(function(x){x*pi.fun(x)},lower = -10, upper = 10)
mean(out) 

e.sd <- sqrt(integrate(function(x){x^2*pi.fun(x)},lower = -10, upper = 10)$value)
sd(out)

###



#
#
#  Example from Bayes slides
#

# fix observations and constants
y <- 1.0
sigma <- 2.0

# theoretical posterior mean and SD
post.mean <- 100*y/(100+sigma^2)
post.sd <- sqrt(100*sigma^2/(100+sigma^2))

# log-target density KERNEL
lp_temp <- function(theta){
  return(dnorm(y,mean=theta,sd=sigma,log=TRUE) # log-likelihood
         +dnorm(theta,mean=0.0,sd=10.0,log=TRUE) # log-prior
         )
}

# run MCMC simulations
out <- oneD.RWMH(lprob=lp_temp,theta1 = 0.0,sigma=2.4*post.sd,n.iter = 50000)

# plot output
par(mfrow=c(2,1))
plot(1:length(out),out,pch=20,cex=0.1,xlab="MCMC iteration #")
hist(out,probability = TRUE)

# check against theoretical posterior

xg <- seq(from=min(out),to=max(out),length.out = 1000)
lines(xg,dnorm(xg,mean=post.mean,sd=post.sd),col="red")

###


# probability of slippery hill:
# from mcmc output
mean(out<0.0)
# from theoretical calculations
pnorm(0.0,mean=post.mean,sd=post.sd)

# typical range ("credible interval")
# from mcmc output
quantile(out,probs = c(0.025,0.975))
# from theoretical calculations
qnorm(p=c(0.025,0.975),mean=post.mean,sd=post.sd)


#
#  tuning
#
#

# consider a normal distribution with mean 0=and sd=10

lprob <- function(x){return(-x^2/(200))} # notice, normalization constant not needed


# wish to compare the estimation of E(X) for different values
# of the proposal scale 10

n.iter <- 1000 # length of chain
n.rep <- 100 # number of replications
sigma <- 2.4*10

res <- numeric(n.rep) # E(X) estimated from the different chains
Ealpha <- numeric(n.rep) # accept probabilities
for(i in 1:n.rep){
  x1 <- rnorm(1,mean=0,sd=10) # intial x from target distribution
  out <- oneD.RWMH(lprob,sigma=sigma,theta1=x1,n.iter = n.iter)
  res[i] <- mean(out)
  Ealpha[i] <- mean(abs(diff(out))>1.0e-14) # Probability of moving=E(alpha)
}

par(mfrow=c(1,1))
ts.plot(out)

print(paste0("sigma : ",sigma))
print(paste0("MC SD of E(X) : ",sd(res)))
print(paste0("SD of E(X) under independent sampling : ", sqrt(100/n.iter)))
print(paste0("mean accept prob : ",mean(Ealpha)))
