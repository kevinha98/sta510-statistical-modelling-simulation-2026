# clear all variables
rm(list=ls())

################################################################
#### Problem set 4 
################################################################

# (Solutions to the problems from the book are found at the end)

################################################################
### Problem 1

# a)

# Function to simulate HPP
simHPP <- function(lambda,stoptime){
  narrivals <- rpois(1,lambda*stoptime)
  timesto <- sort(runif(narrivals,min=0,max=stoptime))
  return(timesto)
}

# Test
timesto <- simHPP(lambda=0.2,stoptime=365)
Nevents <- length(timesto)
plot(timesto,1:Nevents,type="s",xlab = "arrival time", 
     ylab = "Event number",lwd=1.5,ylim=c(0,Nevents))
points(timesto,rep(0,Nevents),pch=21,bg="red")

# b) 
Nsim <- 10000 
N10vector <- vector(length=Nsim)
for(i in 1:Nsim)
  N10vector[i] <- length(simHPP(lambda=2,stoptime=10))
mean(N10vector>25)

# Or more directly whithout generating all the times to events:
N10vector <- rpois(n=Nsim,20)
mean(N10vector>25)

# Exact
1-ppois(25,2*10)






################################################################
#Solutions to the problems from the book 
################################################################



################################################################
### Problem 3.1 from Rizzo


# The quantile function
qtwo.par.exp <- function(q,lambda,eta)
    eta-log(1-q)/lambda

# The random number function
rtwo.par.exp <- function(n,lambda,eta)
    qtwo.par.exp(runif(n),lambda,eta)


n <- 10000 # number of simulations
lambda <- 2.0 # scale parameter
eta <- 0.5 # location parameter
x <- rtwo.par.exp(n,lambda,eta)

# The quantiles we want to compute
ps <- seq(from=0.01,to=0.99,by=0.01)
# Theoretical quantiles for probabilities in ps
theo.q <- qtwo.par.exp(ps,lambda,eta)
# The corresponding empirical quantiles using built-in function quantile
empi.q <- quantile(x=x,probs=ps)
# Compare the numbers
round(rbind(theo.q,empi.q),3)


# Plot the empirical aganist theoretical quantiles, should be approximately
# on the x=y-line (try googling qq-plot for explanation)
plot(x=empi.q,y=theo.q)
# add x=y-line
abline(0,1 ,col="red")

# Plot of probabilities versus quantiles:
plot(empi.q,ps,type="l",xlab="quantile",ylab="probability",lwd=2.5)
lines(theo.q,ps,col="red",lwd=2.5)







################################################################
### Problem 3.11 from Rizzo

# The function below, also used in examples in the lectures,
# simulate a general mixture of normal populations
gennormmix <- function(Nsim,pvec,muvec,sdvec){
  if(min(c(length(pvec),length(muvec),length(sdvec))) != 
     max(c(length(pvec),length(muvec),length(sdvec))))
    stop("Length of pvec,muvec and sdvec must be equal") # Check for valid input
  if(sum(pvec)!=1)
    stop("pvec must sum to 1") # Check for valid input
  m <- length(pvec) # Number of distributions
  # Determine which distribution to sample from in each simulation
  whichdist <- sample(1:m,size=Nsim,replace = TRUE,prob = pvec)
  # Simulate the mixture
  Y <- rnorm(Nsim,mean = muvec[whichdist],sd = sdvec[whichdist])
  Y
}

# Generate the mixture in the exercise:
Nsim <- 10000
p1=0.25
pvec <- c(p1,1-p1)
muvec <- c(0,3)
sdvec <- c(1,1)
Y <- gennormmix(Nsim,pvec=pvec,muvec=muvec,sdvec=sdvec)
hist(Y,prob = TRUE,nclass=max(10,sqrt(Nsim)),main="Histogram of normal mixture")
curve(pvec[1]*dnorm(x, mean=0, sd=1)+pvec[2]*dnorm(x, mean=3, sd=1),from=-4,to=7,add=TRUE, col='red',lwd=1.6)
# Repeat the above lines for different values of p1


################################################################
### Problem 4.1 from Rizzo


## Version 1, without loops but have to generate a long vector to be certain
## that Sn reaches 0 or 20
Nsim <- 10000
Sn <- 10+cumsum(sample(c(-1,1),size=Nsim,replace=TRUE))
tstop <- min(which(Sn==0),which(Sn==20))  # Find the position of the first hit of 0 or 20
plot(1:tstop,Sn[1:tstop],type="b",ylim=c(0,20),
     xlab="n",ylab="Sn")
# Repeate the above lines many times


## Version 2 with a while loop
score <- 10+sample(c(-1,1),size=1) # Step 1
Sn <- score  # Step 1
# Next steps - stop when score reaches 0 or 20
while(score>0 & score<20){
  score <- score+sample(c(-1,1),size=1)
  Sn <- c(Sn,score)
}
tstop <- length(Sn)
plot(1:tstop,Sn[1:tstop],type="b",ylim=c(0,20),
     xlab="n",ylab="Sn")
# Repeate the above lines many times



