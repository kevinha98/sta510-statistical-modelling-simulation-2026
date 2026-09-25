# illustration of Accept-reject algorithm for
# target density f(x) = 0.75*(1-x^2), -1<x<1
# using a uniform(-1,1) proposal
rm(list=ls())

# a function of the density
f <- function(x){return(0.75*(1.0-x^2))}
C <- 1.5 # found in slides (avoid redefining c())

# for plotting purposes
x.grid <- seq(from=-1.0,to=1.0,length.out = 1000)
# plot the target density f(x)
plot(x.grid,f(x.grid),type="l",lwd=2,xlab="x",ylab="")
# plot C*g(x)
lines(x.grid,C*dunif(x.grid,min=-1,max=1),col=2,lwd=2)
# we should have f(x) <= C*g(x) for all possible x
# This looks good for now

# single draw
gen.f.AR.draw <- function(){
  proposals <- 0
  while(TRUE){ # this is scary, should be safeguarded
    x <- runif(1,min=-1,max=1) # proposal from uniform(-1,1)
    a <- f(x)/(C*dunif(x,min=-1,max=1)) # accept prob
    proposals <- proposals + 1 
    if(runif(1) < a) return(c(x,proposals))
  }
}

# many draws
rf.AR <- function(n=1000){
  ret <- vector(mode="numeric",length=n)
  nprop <- vector(mode="numeric",length=n)
  for(i in 1:n){
    tmp <- gen.f.AR.draw()
    ret[i] <- tmp[1]
    nprop[i] <- tmp[2]
  }
  print(paste0(
    "number of proposal per accepted RV : ",
    mean(nprop)),quote=F)
  return(ret)
}

# draw some random numbers and add to figure
x <- rf.AR(n=100000)
hist(x,probability = T,add=T)


