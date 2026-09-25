

my.kde <- function(x,X,h){ # allow for a vector of x-s
  fx <- numeric(length(x)) # allocate space
  for(i in 1:length(x)){
    #eqn in previous slide
    fx[i] <- mean(dnorm(x[i],mean=X,sd=h)) 
  }
  return(list(x=x,fx=fx,h=h))
}

# Exampe for illustraion
X <- rnorm(5) # tiny sample for illustration
x.grid <- seq(from=-3.0,to=3.0,by=0.01)
kde.out <-  my.kde(x=x.grid,X=X,h=0.05)
plot(kde.out$x,kde.out$fx,type="l",ylim=c(0,0.5))
points(X,0*X,pch=3,col="red")

lines(x.grid,dnorm(x.grid),col="blue")


## Example 2
X <- rnorm(5000); 
kde.out <- my.kde(x=x.grid,X=X,h=0.05)
plot(kde.out$x,kde.out$fx,type="l")
lines(x.grid,dnorm(x.grid),col="blue")

# More smoothing
kde.out <- my.kde(x=x.grid,X=X,h=0.2)
lines(kde.out$x,kde.out$fx,type="l",col="green")

# Even more smoothing
kde.out <- my.kde(x=x.grid,X=X,h=1)
lines(kde.out$x,kde.out$fx,type="l",ylim=c(0,0.4),col="red")


# Optimal bandwidth
my.kde <- function(x,X,h=1.06*sd(X)*length(X)^(-0.2)){
  fx <- numeric(length(x)) # allocate space
  for(i in 1:length(x)){
    fx[i] <- mean(dnorm(x[i],mean=X,sd=h)) 
  }
  return(list(x=x,fx=fx,h=h))
}

kde.out <- my.kde(x=x.grid,X=X)
plot(kde.out$x,kde.out$fx,type="l",
     main=paste0("bandwidth = ",kde.out$h))
lines(x.grid,dnorm(x.grid),col="blue")




# Impact of bandwith selction for a mixture

n <- 50
X1 <- rnorm(n,mean=20,sd=4)
X2 <- rnorm(n,mean=30,sd=2)
X3 <- rnorm(n,mean=40,sd=3)
X4 <- rnorm(n,mean=50,sd=2)
X <- c(X1,X2,X3,X4)

set.seed(1)

plotHistKDE <- function(X,bw="nrd0",main=""){
  hist(X, probability = TRUE, ylim = c(0,0.1),xlim=c(0,60),main=main)
  xg <- seq(from=0,to=60,by=0.01)
  lines(xg, 0.25*dnorm(xg,mean=20,sd=4)+0.25*dnorm(xg,mean=30,sd=2)+
          0.25*dnorm(xg,mean=40,sd=3)+0.25*dnorm(xg,mean=50,sd=2),col="blue")
  lines(density(X,bw=bw), lwd=2, col="red")
}

set.seed(1)
par(mfrow=c(2,2))
plotHistKDE(X,bw="nrd0",main="Silverman's rule of thumb")
plotHistKDE(X,bw="ucv",main="Unbiased cross-validation")
plotHistKDE(X,bw="bcv",main="Biased cross-validation")
plotHistKDE(X,bw="SJ",main="Sheather & Jones")
par(mfrow=c(1,1))



## Another mixture (Exp + Norm)

X1 <- rexp(5000)
X2 <- rnorm(5000,mean=5,sd=1)
X <- c(X1,X2)  

hist(X, probability = TRUE, xlim=c(-1,10), ylim=c(0,0.5))
xg <- seq(from=-1, to=10, by=0.01)
lines(xg, 0.5*dexp(xg)+0.5*dnorm(xg,mean=5,sd=1),col="red")

lines(density(X), lwd=1) # Silverman's rule of thumb
lines(density(X,bw="ucv"), lwd=1,col="blue") # Unbiased cross-validation
lines(density(X,bw="bcv"), lwd=1,col="green") # Biased cross-validation
lines(density(X,bw="SJ"), lwd=2,col="yellow") # Sheather & Jones






###################################################
## Adaptive weight smoothing
library(aws)
demo(aws_ex1) # 1D local constant smoothing
demo(aws_ex2) # 2D local constant smoothing
demo(aws_ex3)


