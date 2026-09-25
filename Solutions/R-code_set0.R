#problem 1
x <- runif(1000)

#problem 2
hist(x)
readline() #note this line is included to make R stop
           #until you hit enter in the command window

#problem 3
print("number of elements in x greater than 0.5")
print(sum(as.integer(x>0.5))) #should be roughly half of the 1000 uniforms

#problem 4
xx<-seq(from=0.0,to=2*pi,by=0.001)
plot(x=xx,y=sin(xx),type="l",xlab="x",ylab="sin(x)")

#problem 5
quadpoly.roots<-function(a,b,c){
tmp1 <- 0.5*sqrt(b^2-4*a*c)/c
tmp2 <- -0.5*b/c
ret <- c(tmp2+tmp1,tmp2-tmp1)
return(ret)
}

#example
print("roots of x^2-1 are")
print(quadpoly.roots(-1,0,1))

#problem 6
normal.mean.CI <- function(x){
xbar <- mean(x)
s <- sd(x)
n <- length(x)
tmp1 <- 1.96*s/sqrt(n)
ret <- c(xbar-tmp1,xbar+tmp1)
return(ret)
}

#example
x <- 1.0+rnorm(10000) # elements in x are N(1,1)
print("normal 95% confidence interval for mean")
print(normal.mean.CI(x))

#problem 7
print("problem 7, Fibonacci numbers < 100000")

old <- 0 #F_0
current <- 1 #F_1
print(old)
for( i in 1:10000){ # this could also be done using a while-loop
  print(current)
  new <- current + old
  if(new>=100000) break
  old <- current
  current <- new
}




