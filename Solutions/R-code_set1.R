# clear all variables
rm(list=ls())

################################################################
#### Problem set 1
################################################################


################################################################
###--- Problem 1
##b)


# We can directly use the code from the lecture with k=5 and a large Nsim
simheads <- function(Nsim,k){
  nheads <- vector(length=Nsim) # Define a vector to store the number of heads in each repetition
  for(i in 1:Nsim)              # Generate the number of heads Nsim times
    nheads[i] <- sum(sample(0:1,size=k,replace=TRUE)) 
  return(nheads)                # Return the resulting vector
}
k <- 5
Nsim <- 100000
nheadsim <- simheads(Nsim=Nsim,k=k)
relfreq <- table(nheadsim)/Nsim   # Calculate relative frequency for each outcome
relfreq # This give the approximate probability for all outcomes, including 3
barplot(relfreq,ylab="Relative frequency")


##d)
# The only change needed in the code is to specify the probability for
# 0 and 1 with the prob option in sample()
simheads2 <- function(Nsim,k){
  nheads <- vector(length=Nsim) # Define a vector to store the number of heads in each repetition
  for(i in 1:Nsim)              # Generate the number of heads Nsim times
    nheads[i] <- sum(sample(0:1,size=k,replace=TRUE,prob=c(0.4,0.6)))  # See explanation above
  return(nheads)                # Return the resulting vector
}
k <- 5
Nsim <- 100000
nheadsim <- simheads2(Nsim=Nsim,k=k)
relfreq <- table(nheadsim)/Nsim   # Calculate relative frequency for each outcome
relfreq # This give the approximate probability for all outcomes, including 3
barplot(relfreq,ylab="Relative frequency")



################################################################
###--- Problem 2

##c)

x <- c(1,2,3,4)
c <- 10
f <- x/c 
sum(f)
barplot( height=f, names.arg=x )


x <- c(0,1,2,3)
c <- 1/sum((x+1)^2)
f <- c*((x+1)^2) 
sum(f)
barplot( height=f, names.arg=x )


################################################################
###--- Problem 3
x <- c(1,2,3,4)
c <- 10
f <- x/c
E <- sum( x*f ) 
E
Eg <- sum( (x^3)*f ) 
Eg


x <- c(0,1,2,3)
c <- 1/sum((x+1)^2)
f <- c*((x+1)^2) 
sum(f)
E <- sum( x*f ) 
E
Eg <- sum( (x^3)*f ) 
Eg


################################################################
###--- Problem 4

## c)
x <- seq( 0, 1, 0.001 )

f <- 4*x*(1-x^2)
plot( x=x, y=f, type='l', col=4, lwd=2, ylab="f(x)",main='pdf' )

F <- 2*x^2-x^4
plot( x=x, y=F, type='l', col=4, lwd=2, ylab="F(x)", main='cdf' )

f <- function(x) 
  4*x*(1-x^2)

integrate( f=f, 0,0.5)
integrate( f=f, 0,0.3)
integrate( f=f, 0.7,1.0)




################################################################
###--- Problem 5

##b)

x <- seq( -1, 1, 0.001 )

f <- 0.75*(1-x^2)
plot( x=x, y=f, type='l', col=4, lwd=2, ylab="f(x)",main='pdf' )

f <- function(x) 
  0.75*(1-x^2)

integrate( f=f, -1,0.5)



################################################################
###--- Problem 6

x <- c(23.89,23.39,22.20,24.35,24.10,23.39,23.96,24.15,23.69,22.57)
y <- c(23.99,23.39,22.65,24.25,23.99,23.09,23.66,24.25,23.36,22.57)
plot(x,y,col="blue",pch=16,cex=1.3)
cor(x,y)


################################################################
###--- Problem 7 

# P(X>=3) by direct calculation (choose() calculate the binomial coefficient):
1- ( 1*(0.2^0)*0.8^(10) + 10*(0.2^1)*0.8^(9) + choose(10,2)*(0.2^2)*0.8^(8) )
# P(X>=3) by using the pbinom function:
1 - pbinom( q=2, size=10, prob=0.2 )






################################################################
###--- Problem 8

pnorm(175, mean=180,sd=6.5)
1-pnorm(190, mean=180,sd=6.5)
pnorm(180, mean=180,sd=6.5)-pnorm(170, mean=180,sd=6.5)
qnorm(0.01, mean=180,sd=6.5, lower.tail = FALSE)

################################################################
################################################################






























