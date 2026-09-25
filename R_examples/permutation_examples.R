# clear all variables
rm(list=ls())
#######################

########################################################
# Wave height
########################################################
wavedata1 <- c(3.5, 2.0, 3.3, 1.8, 1.6, 1.5, 2.0, 0.8, 2.2, 1.1, 
              1.8, 1.1, 1.9, 4.0, 2.5, 1.9, 1.5, 3.5, 1.2, 5.3)
wavedata2 <- c(1.0, 1.4, 0.9, 1.7, 1.0, 0.5, 1.3, 0.8, 0.7, 2.9, 
               1.0, 0.8, 1.5, 1.9, 2.4, 1.5, 0.7, 0.4, 3.7, 2.2)
# Check means
mean(wavedata1)
mean(wavedata2)
sd(wavedata1)
sd(wavedata2)
# Check theta estimates
thetaest <- function(data)
  sqrt(mean(data^2)/2)
thetaest(wavedata1)
thetaest(wavedata2)

### Test for difference in mean wave height using permutation
n1 <- length(wavedata1)
n2 <- length(wavedata2)
m <- n1+n2
alldata <- c(wavedata1,wavedata2)
P <- 10000
meandiff0 <- mean(wavedata1)-mean(wavedata2)  # mean difference original data
meandiffP <- numeric(P)
for(i in 1:P){
  permdata <- sample(alldata,size=m,replace=FALSE)  # Permute the data in random order
  meandiffP[i]<- mean(permdata[1:n1])-mean(permdata[(n1+1):m]) # Compute mean diff for permuted sample
}
hist(meandiffP,prob=TRUE,nclass=sqrt(P)/2)  
abline(v=meandiff0,col="blue",lwd=2)
# Proportion of times the mean difference for the permuted data 
# is further away from 0 than the mean difference of the original data
p <- mean(abs(meandiffP)>=abs(meandiff0)) 
p  # The p-value for two-sided test obtained by permutation

# One sided test for expectation1>expectation2
p <- mean(meandiffP>=meandiff0) 
p  # The p-value for one-sided test obtained by permutation

### Test for difference in theta
thetadiff0 <- thetaest(wavedata1)-thetaest(wavedata2)  # mean difference original data
thetadiffP <- numeric(P)
for(i in 1:P){
  permdata <- sample(alldata,size=m,replace=FALSE)  # Permute the data in random order
  thetadiffP[i]<- thetaest(permdata[1:n1])-thetaest(permdata[(n1+1):m]) # Compute mean diff for permuted sample
}
hist(thetadiffP,prob=TRUE,nclass=sqrt(P)/2)  
abline(v=thetadiff0,col="blue",lwd=2)

# Proportion of times the mean difference for the permuted data 
# is further away from 0 than the mean difference of the original data
p <- mean(abs(thetadiffP)>abs(thetadiff0)) 
p  # The p-value for two-sided test obtained by permutation

# One sided test for expectation1>expectation2
p <- mean(thetadiffP>=thetadiff0) 
p  # The p-value for one-sided test obtained by permutation


#############################################################
# Trend test
#############################################################

# Function for a permutation test for trend in a sequence of data
trendtest <- function(P=10000,xdata,plot=FALSE){
  nobs <- length(xdata)
  sdata <- cumsum(xdata)
  ntest <- nobs-1
  testobs0 <- mean(sdata[1:ntest])-sdata[nobs]/2 # Test observator for original data
  testobsP <- numeric(P)
  for(i in 1:P){
    permX <- sample(xdata,size=nobs,replace=FALSE)
    permS <- cumsum(permX)
    testobsP[i] <- mean(permS[1:ntest])-permS[nobs]/2 # Test observator permuted data
  }
  if(plot){
    hist(testobsP,prob=TRUE,nclass=sqrt(P)/2)  
    abline(v=testobs0,col="blue",lwd=2)
  }
  p <- mean(abs(testobsP)>=abs(testobs0)) # Two-sided p-value
  return(p)  
}

# Steering unit data
suftimes <- c(2,4,7,17,32,48,81,98,121,140,177,236,343,356)
# Plot of data
plot(suftimes,1:length(suftimes),type="s",xlab = "time", 
     ylab = "Event number",lwd=1.5,ylim=c(0,length(suftimes)))
points(suftimes,rep(0,length(suftimes)),pch=21,bg="red")
# Test for trend
timesbetween <- diff(c(0,suftimes))
trendtest(xdata=timesbetween,plot=TRUE)
trendtest(P=100000,xdata=timesbetween,plot=TRUE)


########################################################


rm(list=ls())

########################################################
# Compressive strength of concrete
########################################################

concreteA <- c(483.2, 466.3, 554.2, 505.5, 469.6,443.0)
concreteB <- c(450.8, 448.7, 478.1, 505.7, 441.2)
mean(concreteA)
mean(concreteB)
sp <- sqrt((5*var(concreteA)+4*var(concreteB))/9)
sp
tobs <- (mean(concreteA)-mean(concreteB))/(sp*sqrt(1/6+1/5))
tobs
t.test(concreteA,concreteB, var.equal=TRUE, paired=FALSE)
t.test(concreteA,concreteB, var.equal=FALSE, paired=FALSE)

nA <- length(concreteA)
nB <- length(concreteB)
m <- nA+nB
alldata <- c(concreteA,concreteB)
P <- 10000
meandiff0 <- mean(concreteA)-mean(concreteB)  # mean difference original data
meandiffP <- numeric(P)
for(i in 1:P){
  permdata <- sample(alldata,size=m,replace=FALSE)  # Permute the data in random order
  meandiffP[i]<- mean(permdata[1:nA])-mean(permdata[(nA+1):m]) # Compute mean diff for permuted sample
}
hist(meandiffP,prob=TRUE,nclass=sqrt(P)/2)  
abline(v=meandiff0,col="blue",lwd=2)
# Proportion of times the mean difference for the permuted data 
# is further away from 0 than the mean difference of the original data
p <- mean(abs(meandiffP)>=abs(meandiff0)) 
p  # The p-value for two-sided test obtained by permutation

# One sided test for expectationA>expectationB
p <- mean(meandiffP>=meandiff0) 
p  # The p-value for one-sided test obtained by permutation
# One sided test for expectationA<expectationB
p <- mean(meandiffP<=meandiff0) 
p  # The p-value for one-sided test obtained by permutation

##############



