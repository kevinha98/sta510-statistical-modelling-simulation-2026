
#Inverse transform sampling
gen.poisson.IT.draw <- function(lambda=2){
  x <- 0
  p <- exp(-lambda)
  s <- p
  u <- runif(1)
  while(u > s){
    x <- x+1
    p <- p*lambda/x
    s <- s + p
  }
  return(x)
}

gen.poisson.IT.draw()


# many draws
rv.Poisson <- function(n=1000,lambda=2){
  ret <- vector(mode="numeric",length=n)
  for(i in 1:n){
    ret[i] <- gen.poisson.IT.draw(lambda=lambda)
  }
  return(ret)
}

# plot the poisson density
lambda = 5
x.grid <- 0:(2*lambda+10)
plot(x.grid,dpois(x.grid, lambda),type="p",lwd=2,xlab="x",ylab="",ylim=c(0,max(dpois(x.grid, lambda))+0.1))


# draw some random numbers and add to figure
n <- 100000
x <- rv.Poisson(n,lambda)
points(table(x)/n,col="red")


n <- 100000
print(system.time(x1 <- rv.Poisson(n,lambda)))
print(system.time(x2 <- rpois(n,lambda)))


