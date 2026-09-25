#### Acceptance-rejection sampling for target density
#    von Mises distribution

rm(list=ls())


set.seed(123)

n <- 5000
mu <- pi/2
kappa <- 3

theta <- numeric(n)
nprop <- numeric(n)
for(i in 1:n){
  proposals <- 0
  repeat{
    y <- runif(1, 0, 2*pi) # Proposal
    a <- exp(kappa*(cos(y-mu)-1)) # Acceptance probability
    proposals <- proposals + 1 
    
    if(runif(1) < a){
      theta[i] <- y
      nprop[i] <- proposals
      break
    }
  }
}

print(paste0("number of proposal per accepted RV : ", mean(nprop)),quote=F)


# Compare simulated data with the target density:
hist(theta,
     probability=TRUE,
     breaks=30,
     xlim=c(0,2*pi),
     xlab=expression(theta),
     main="AR sampling from a von Mises distribution")

x <- seq(0,2*pi,length.out=500)
f <- exp(kappa*cos(x-mu)) / (2*pi*besselI(kappa,0))
lines(x,f,lwd=2,col="red")



## Rose-histogram for circular data
library(circular)

theta.c <- circular(
  theta,
  units = "radians",
  template = "geographics",
  modulo = "2pi"
)

nbins <- 24
binwidth <- 2*pi/nbins
prop <- 1.5

# Rose diagram
p <- rose.diag(
  theta.c,
  bins = nbins,
  col = "steelblue",
  prop = prop,
  main="AR sampling from a von Mises distribution"
)

# Angular grid
x <- seq(0, 2*pi, length.out = 500)

# True von Mises density
f <- exp(kappa*cos(x-mu)) /  (2*pi*besselI(kappa, 0))

# Convert density to the same radial scale as rose.diag()
r <- prop * sqrt(binwidth * f)

# Circular representation of x
xc <- circular(
  x,
  units = "radians",
  template = "geographics",
  modulo = "2pi"
)

# Add true density curve
lines.circular(
  xc,
  r,
  plot.info = p,
  offset = 0, 
  join = TRUE,
  col = "red",
  lwd = 2
)












theta <- circular(
  theta,
  units = "radians",
  template = "geographics",
  modulo = "2pi"
)

rose.diag(
  theta,
  bins = 24,
  col = "steelblue",
  prop = 1.5
)

lines.circular(x,f,lwd=2,col="red")

## 3. Circular KDE
#bw <- bw.nrd.circular(theta) # Assume Mises Fisher (one mode)
bw <- bw.nrd.circular(theta,kappa.est = "trigmoments")
kd <- density.circular(theta, bw = bw)
plot(
  kd,
  main = "Circular kernel density estimate", 
  xlim = c(-1.2,1.2), ylim = c(-1.2,1.2)
)
