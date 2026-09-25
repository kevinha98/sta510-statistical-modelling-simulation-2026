rm(list=ls())
#
# Gibbsampler for bivariate normal target
#
rho <- 0.95
n.iter <- 10000 # number of Gibbs iterations
theta <- c(3,-2) # variable used as state throughout
res <- matrix(0.0,2*n.iter+1,2) # results, note stores twice per iteration
res[1,] <- theta
k <- 2 # store counter

for(i in 1:n.iter){
  # update first block
  theta[1] <- rnorm(1,mean=rho*theta[2],sd=sqrt(1-rho^2))
  # store state
  res[k,] <- theta
  k <- k+1
  
  # update second block
  theta[2] <- rnorm(1,mean=rho*theta[1],sd=sqrt(1-rho^2))
  # store state
  res[k,] <- theta
  k <- k+1
}

plot(res[,1],res[,2],type="l")
cov(res)
colMeans(res)



#
# Gibbs sampler for normal mean/precision model
#
set.seed(123)
n <- 100
y <- rnorm(n=n,mean=1.0,sd=0.5)



n.iter <- 10000
mu <- 0.8
tau <- 4.0
res <- matrix(0.0,2*n.iter+1,2) #(mu,tau)
res[1,] <- c(mu,tau)
k <- 2

sumy <- sum(y)
for(i in 1:n.iter){
  # update mu conditional on tau and data
  mu <- rnorm(1,mean = tau*sumy/(n*tau+0.01),sd=sqrt(1.0/(n*tau+0.01)))
  # store state
  res[k,] <- c(mu,tau)
  k <- k+1
  
  # update tau conditional on mu and data
  Ssq <- sum((y-mu)^2)
  tau <- rgamma(1,shape=0.5*n+1,rate=0.5*Ssq+1.0)
  # store state
  res[k,] <- c(mu,tau)
  k <- k+1
  
}
# joint distribution
par(mfrow=c(1,2))
plot(res[,1],res[,2],type="l",xlab="mu",ylab="tau")

# marginal posterior of mu
hist(res[,1],probability = TRUE)

# chain for sigma = 1/sqrt(tau)
par(mfrow=c(1,2))
plot(1.0/sqrt(res[,2]),type="l")
hist(1.0/sqrt(res[,2]))

sd(y)








#
# Metropolized Gibbs sampler for an (zero mean) AR(1)
# process
#

# simulate some data
set.seed(1)
T <- 1000
phi <- 0.95
sigma <- 1.0
y <- numeric(length = T)
y[1] <- rnorm(1,sd=sigma/sqrt(1.0-phi^2))
for(t in 2:T) y[t] <- phi*y[t-1] + sigma*rnorm(1)
par(mfrow=c(1,1))
ts.plot(y)



# Metropolized Gibbs sampler for recovering phi and tau=1/sigma^2
n.iter <- 10000
res <- matrix(0.0,nrow=n.iter + 1,2)

phi <- 0.9
tau <- 1.0
res[1,] <- c(phi,tau)
k <- 2

sumYsq <- sum(y[1:(T-1)]^2)
phiHat <- sum(y[2:T]*y[1:(T-1)])/sumYsq

nacc <- 0

for( i in 1:n.iter){
  # update tau
  d <- 1.0 + 0.5*y[1]^2*(1.0-phi^2) + 
    0.5*sum((y[2:T]-phi*y[1:(T-1)])^2)
  tau <- rgamma(1,shape=1.0+0.5*T,rate=d)
  
  
  # Metropolized update of phi
  
  # proposal standard deviation
  propSD <- 1.0/sqrt(tau*sumYsq) # modulate to obtain poorer proposal
  
  # actual proposal
  pPhi <- rnorm(1,mean=phiHat,sd=propSD)
  
  # reject any proposals inconsistent with prior (dont even bother calculating likelihood)
  if(abs(pPhi)<1.0){
    # importance weight at proposal
    lwtProp <- (0.5*log(1.0-pPhi^2)-
      0.5*tau*y[1]^2*(1.0-pPhi^2)-
      0.5*tau*sum((y[2:T]-pPhi*y[1:(T-1)])^2)-
      dnorm(pPhi,mean=phiHat,sd=propSD,log = TRUE))
    
    # importance weight at old phi
    lwtOld <- (0.5*log(1.0-phi^2) - 
      0.5*tau*y[1]^2*(1.0-phi^2) -
      0.5*tau*sum((y[2:T]-phi*y[1:(T-1)])^2) -
      dnorm(phi,mean=phiHat,sd=propSD,log = TRUE))
    
    # accept/reject-step
    if(runif(1)<exp(min(0.0,lwtProp-lwtOld))){
      phi <- pPhi
      nacc <- nacc+1
    }
  } # check of abs(pPhi)<1
  
  # store state
  res[k,] <- c(phi,tau)
  k <- k+1
  
} # main gibbs iteration loop
print(paste0("phi update accept prob : ",nacc/n.iter))

par(mfrow=c(1,1))
plot(res[,1],res[,2],type="l")

# classical estimators
1/var(y[2:T]-0.9*y[1:(T-1)])
phiHat

# trace plots
par(mfrow=c(1,2))
ts.plot(res[,1])
ts.plot(res[,2])

coda::effectiveSize(res[,1])
coda::effectiveSize(res[,2])



#
# future outcome 
#
Tpred <- 100
predPaths <- matrix(0.0,n.iter,Tpred)
for(i in 1:n.iter){
  # get parameters from gibbs output
  phi <- res[i+1,1]
  sigma <- 1.0/sqrt(res[i+1,2])
  
  # simulate one future scenario
  predPaths[i,1] <- phi*y[T] + sigma*rnorm(1)
  for(t in 2:Tpred) predPaths[i,t] <- phi*predPaths[i,t-1] + sigma*rnorm(1)
}

# get statistics from the simulated paths
sta <- matrix(0.0,Tpred,5)
for(t in 1:Tpred) sta[t,] <- quantile(predPaths[,t],probs = c(0.025,0.2,0.5,0.8,0.975))

#graphical representation

# last part of the observed data
par(mfrow=c(1,1))
plot((T-20):T,y[(T-20):T],xlim=c(T-20,T+Tpred),ylim=c(-6,6),col="red")
for(i in 1:5) lines((T+1):(T+Tpred),sta[,i],col=i)
legend("topleft",legend=c("obs","q02.5","q20","q50","q80","q97.5"),
       lty=c(NA,1,1,1,1,1),
       col=c(2,1,2,3,4,5),
       pch = c(1,NA,NA,NA,NA,NA))


