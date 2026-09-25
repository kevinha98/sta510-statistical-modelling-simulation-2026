if(F){
# page 13
# Illustration of importance sampling with
# too thin tails. Here g(x) is a t-density with nu
# degrees of freedom is integrated (so the
# true value of the integral is 1). The
# proposal is Gaussian with standard deviation
# n.std (so that the second derivatives at the
# modes matches)
#
nu <- 40
nsim <- 100000

n.std <- sqrt(nu/(nu+1.0)) # ensure same second derivative at mode

x.sim <- rnorm(n=nsim,mean=0,sd=n.std)
x.grid <- seq(from=-2.5*n.std,to=2.5*n.std,length.out = 1000)

par(mfrow=c(3,1))
plot(x.grid,
     dt(x.grid,df=nu)/dnorm(x.grid,mean=0,sd=n.std),
     type="l",
     ylab="importance weight",
     xlab="x")


wts <- dt(x.sim,df=nu)/dnorm(x.sim,mean=0,sd=n.std)
c.est <- cumsum(wts)/(1:nsim)

plot(1:nsim,wts,type="l",
     ylab="importance weights")

plot(1:nsim,c.est,type="l",
     ylab="integral estimator")
lines(c(0,nsim),c(1,1),type="l",col="red")

}


#
# page 14
# target log-function log(g(x))
log.g <- function(x){
  T <- length(x)
  lp <- dnorm(x[1],mean=0,sd=0.15/sqrt(1-0.99^2),log = TRUE)
  lp <- lp + sum(dnorm(x[2:T],mean=0.99*x[1:(T-1)],sd=0.15,log = TRUE))
  lp <- lp + sum(dnorm(0.1*(1:T)/T,mean=0.0,sd=0.6*exp(0.5*x),log=TRUE))
  return(lp)
}

#plot 2d case
nn <- 60
x1.g <- seq(from=-6,to=6,length.out = nn)
x2.g <- x1.g
lg <- matrix(0.0,nrow=nn,ncol=nn)
x1 <- lg
x2 <- lg
for(i in 1:nn){
  for(j in 1:nn){
    lg[i,j] <- log.g(c(x1.g[i],x2.g[j]))
    x1[i,j] <- x1.g[i]
    x2[i,j] <- x2.g[j]
  }
}
par(mfrow=c(1,1))
contour(x1.g,x2.g,lg,levels=-5*(1:10))
# find mode
x.opt <- optim(par=c(0.0,0.0),fn=log.g,control=list(fnscale=-1.0),hessian = TRUE)
#indicate mode on plot
points(x.opt$par[1],x.opt$par[2],col="red")

readline()


# illustration

d <- 2
# find mode
x.opt <- optim(par=rep(0.0,d),fn=log.g,
               control=list(fnscale=-1.0),
               hessian = TRUE)

# parameters of multivariate normal proposal
mu <- x.opt$par
Sigma <- solve(-x.opt$hessian) 


# actual monte carlo simulations
n.sim <- 10000
lwts <- numeric(n.sim)
library("mvtnorm")
for(i in 1:n.sim){
  if(i %% 1000 == 0) message(paste0("iteration # ",i))
  x <- rmvnorm(1,mean=mu,sigma=Sigma)
  lwts[i] <- log.g(x) - dmvnorm(x=x,mean=mu,sigma=Sigma,log=TRUE) # LOG-weight!
}
wts <- exp(lwts)

par(mfrow=c(2,1))
# plot weights
plot(1:n.sim,wts,type="l")
# plot trace of estimates
plot(1:n.sim,cumsum(wts)/(1:n.sim),type="l",ylab="estimate")

