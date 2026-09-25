# clear all variables
rm(list=ls())

################################################################
#### Problem set 5
################################################################

# (Solutions to the problems from the book are found at the end)

###
################################################################
################################################################
### Problem 1
n.replic <- 10000

theta.hat <- numeric(5)
se <- numeric(5)
g <- function(y)
  exp(-1/y -log(1+y^2))*(y>0)*(y<=1)
curve(g(x),0,1)

###--- f0, U[0,1]
x <- runif( n.replic )
y <- g(x) 
theta.hat[1] <- mean(y)
se[1] <- sd(y)

###--- f1, Exp(1)
x <- rexp(n=n.replic, rate=1)
y <- g(x)/exp(-x) 
theta.hat[2] <- mean(y)
se[2] <- sd(y)
round(rbind(theta.hat, se), 4)

###--- f2, Standard Chauchy

x <- rcauchy(n=n.replic, location=0, scale=1)  # Alternatively: x <- tan(pi*(runif(n.replic)-0.5)) 
y <- g(x)*pi*(1+x^2) 
theta.hat[3] <- mean(y, na.rm=TRUE)
se[3] <- sd( y, na.rm=TRUE )
round(rbind( theta.hat, se ), 4)
###--- This produced some NaN's. The functions mean and sd does not work then.
###--- Therefore the na.rm=TRUE is included to disgard these NaN's.
###--- Some further checks:
# mean(y[!is.na(y)])
# which(is.na(y))
# y[4678]; x[4678]; g( x[4678] )
# This (in the g-function) is the problem:  exp(-1/x[4678])
#                                           produces NaN's when x is too close to 0


###--- f3, trucated Exp(1)
x <- -log( 1-runif(n=n.replic)*(1-exp(-1))) #; hist(x)
y <- g(x)*(1-exp(-1))/exp(-x)
theta.hat[4] <- mean(y)
se[4] <- sd(y)
round( rbind(theta.hat, se ), 4)

###--- f4, trucated Cauchy 
x <- tan( runif(n=n.replic)*pi/4 ) #; hist( x )
y <- g(x)*pi*(1+x^2)/4
theta.hat[5] <- mean(y)
se[5] <- sd(y)
round(rbind(theta.hat, se), 4)

res <- round(rbind(theta.hat, se), 4)
colnames(res) <- c("f0","f1","f2","f3","f4")
res

###--- Plot of y=g/importance function:

x <- seq(0,.99,.01)
r.0 <- g(x)
r.1 <- g(x)/exp(-x) 
r.2 <- g(x)*pi*(1+x^2)
r.3 <- g(x)*(1-exp(-1))/exp(-x)
r.4 <- g(x)*pi*(1+x^2)/4

matplot( x, matrix( c(r.0,r.1,r.2,r.3,r.4), nrow=length(r.0)) , type='l', ylab='',lwd=2 )
legend("topleft", c('f0','f1','f2','f3','f4'),
       col=1:5, lty=1:5, lwd=c(2), pch=c(-1), bty='n')


################################################################
###--- Problem 6.7 (Rizzo)

###--- Simple MC estimate:
n.replic <- 10000
u <- runif(n.replic)
g <- exp(u)
(theta.hat.1 <- mean(g))
# Corresp. standard error:
(se.1 <- sd(g)/sqrt(n.replic))
v.1 <- se.1^2
round( cbind( theta.hat.1, se.1, v.1 ) , 8 )

###--- Antithetic variable MC estimate:
m <- n.replic/2
u <- runif(m)
g.2 <- exp(u) + exp(1-u)
(theta.hat.2 <- mean(g.2)/2)
# Corresp. standard error:
(se.2 <- sd(g.2)/sqrt(4*m))
v.2 <- se.2^2
round( cbind( theta.hat.2, se.2, v.2 ) , 8 )

###--- Variance reduction:
100*(v.1-v.2)/v.1





###############################################################################
################################################################
###--- Problem 6.10 (Rizzo)
###--- Simple MC estimate:
n.replic <- 10000

u <- runif(n.replic)
g <- exp(-u)/(1+u^2)
(theta.hat.1 <- mean(g))
# Corresp. standard error:
(se.1 <- sd(g)/sqrt(n.replic))

###--- Antithetic variable MC estimate:
m <- n.replic/2
u <- runif(m)
v <- 1-u
g.2 <- exp(-u)/(1+u^2) + exp(-v)/(1+v^2)
(theta.hat.2 <- mean(g.2)/2)
# Corresp. standard error:
(se.2 <- sd(g.2)/sqrt(4*m))


###--- Variance reduction:
100*(se.1^2-se.2^2)/se.1^2


