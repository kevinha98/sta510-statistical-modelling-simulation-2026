rm(list=ls())

set.seed(1)
gam <- 0.1
phi <- 0.9
sigma <- 0.1

stationary.mean <- gam/(1.0-phi)
stationary.sd <- sigma/sqrt(1.0-phi^2)
print(paste0("stationary mean and SD: ",stationary.mean,"  ",stationary.sd))

T <- 1000 # length of realization
N <- 10000 # number of realizations

a1 <- -1.0 # initial configuration (same in all realizations)
a2 <- rnorm(n=N,mean=3,sd=2) # some random initial configuration
#a <- rnorm(n=N,mean=stationary.mean,sd=stationary.sd) # stationary distribution

# Simulate AR1 process
sims <- matrix(0.0,nrow = N, ncol=T)
sims[,1] <- a1
sims2 <- sims
sims2[,1] <- a2
for( n in 2:T){
  sims[,n] <- gam + phi*sims[,n-1] + rnorm(N,mean=0.0,sd=sigma)
  sims2[,n] <- gam + phi*sims2[,n-1] + rnorm(N,mean=0.0,sd=sigma)
}

# Simulate stationary distribution
statDist <- matrix(rnorm(N*T,mean=stationary.mean,sd=stationary.sd),N,T)

# Plot 1st simulation of AR1 process versus independent simulations from
# stationary distribution
par(mfrow=c(2,1))
plot(1:T,sims[1,],type="l",xlab = "", ylim = c(min(sims[1,]),max(sims[1,]))) # first trajectory
abline(h=1,col="red",lty=2)
plot(1:T,statDist[1,],type="l",xlab = "time n", ylim = c(min(sims[1,]),max(sims[1,]))) # first trajectory
abline(h=1,col="red",lty=2)

# Plot simulations of AR1 process and histogram
par(mfrow=c(2,1))
plot(1:T,sims[1,],type="l",xlab = "", ylim = c(min(sims),max(sims)),
     main="Initial value a = -1")
for(i in 2:min(N,50)){ # take only first 50 realizations to save computing time
  lines(1:T,sims[i,],col=i)
}
plot(1:T,sims2[1,],type="l",xlab = "", ylim = c(min(sims2),max(sims2)),
     main="Initial value a ~ N(3,4)")
for(i in 2:min(N,50)){ # take only first 50 realizations to save computing time
  lines(1:T,sims2[i,],col=i)
}

# Histogram of X_T for all N simulations (i.e. of the last value in each process)
par(mfrow=c(2,1))
msd <- max(sd(sims[,T]),sd(statDist[,T]))
hist(sims[,T],probability = TRUE,breaks=21,
     xlim=c(mean(sims[,T])-3*msd,mean(sims[,T])+3*msd),
     main = "Hist. of last state in each AR1 process")
if(abs(phi)<1.0){ # only stationary case
  xgrid <- seq(from=stationary.mean-4*stationary.sd,
             to=stationary.mean+4*stationary.sd,
             length.out = 1000)
  lines(xgrid,dnorm(xgrid,mean=stationary.mean,stationary.sd))
}

hist(statDist[,T],probability = TRUE,breaks=21,
     xlim=c(mean(sims[,T])-3*msd,mean(sims[,T])+3*msd),
     main = "Hist. of stationary distr.")



# Impact of the definition of the AR1 process on the variance of the averages
meanEstAR <- rowMeans(sims)
meanEstIId <- rowMeans(statDist)
msd <- max(sd(meanEstAR),sd(meanEstIId))
hist(meanEstAR,xlim=c(mean(meanEstAR)-3*msd,mean(meanEstAR)+3*msd),breaks=21)
hist(meanEstIId,xlim=c(mean(meanEstAR)-3*msd,mean(meanEstAR)+3*msd),breaks=21)

print(paste0("Standard deviation based on AR(1) samples: ",sd(meanEstAR)))
print(paste0("Standard deviation based on iid samples: ",sd(meanEstIId)))
