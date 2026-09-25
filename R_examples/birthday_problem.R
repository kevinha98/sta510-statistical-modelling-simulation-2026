### The birthday problem. 

# Remove old variables:
rm(list=ls())

## Function for generate n random birthdays among the numbers 1 to 365.
# To take into account leap days, replace 365 by 365.25
genbirthdays <- function(n)
   ceiling(runif(n,0,365)) 
# ceiling() rounds up to the nearest integer.
# runif(n,a,b) simulate n numers from the Uniform(a,b) distribution 

# Generate a set of birthdays and make a histogram to display them
# Run these next two lines many times. Also try different values of n
birthdays <- genbirthdays(n=60)
hist(birthdays,nclass=365,density=100,angle=0, col="blue")


# Function for simulating the probability of at least k persons sharing 
# the same birthday in a group of n persons.
simbirthprobabilities <- function(nsim=10000,k=2,n=23){
  nk <- 0   # Counter for number of times there is at least k common birthdays
  for(i in 1:nsim){ # Repeat this nsim times
    birthdays <- genbirthdays(n) # Generate birthday
    daydist <- table(birthdays)  # Calculate the frequency distribution of the birthdays
    if(sum(daydist>=k)>0)        # Check if there is at least one occurrency of at least k common birthdays
      nk <- nk+1                 # Increase the counter by 1 if there is an occurrency  
  }
  psamebirthday <- nk/nsim       # Calculate proportion of times there is at least k common birthdays  
  print(paste("The probability of at least",k,"persons in a groups of",n,
              "persons having the same birthday is", psamebirthday))
  print(paste("The probability is approximated based on ",nsim,"simulations."))
}

# Run the birthday probability function with different settings
simbirthprobabilities(k=3,n=60)
simbirthprobabilities(k=2,n=23)
simbirthprobabilities(nsim=100000,k=2,n=23)
simbirthprobabilities(k=2,n=30)
simbirthprobabilities(nsim=10000,k=2,n=23)
simbirthprobabilities(k=3,n=50)




# For k=2 these probabilities can also easily be calculated exactly with the following function 
# (but for k>2 exact calculations are very difficult while simulation is still easy)
exactbirthprobabilities <- function(n){
  psamebirthday <- 1-(choose(365,n)*factorial(n))/365^n
  print(paste("The probability of at least 2 persons in a groups of",n,
              "persons having the same birthday is", round(psamebirthday,digits = 4)))
  print(paste("Exact calculation."))
}

# Use the exact calculation to verify the approximate probabilities found by simulations for 
# the k=2 cases above.
exactbirthprobabilities(n=60)
exactbirthprobabilities(n=23)
exactbirthprobabilities(n=30)



# Calculate exact probabilities for two or more 
# people with the same birthday in a group of n people
exactBirthdayProb <- function(n=23){
  for(i in 1:n){ 
     p <- 1 - exp(-n*(n-1)/730)
    # p <- 1-(choose(365,n)*factorial(n))/365^n
  }
  return(p)
}

exactBirthdayProb(23)

numPeop <- 1:80
res <- sapply(numPeop, FUN=exactBirthdayProb)
plot(numPeop,res,xlab="number of people",ylab="probability of a pair")
abline(h=0.5,v=23,col="red")

plot(numPeop,res,xlab="number of people",ylab="probability of a pair",type="l")



# Given a group of 23 people. The probability that two random selected people 
# of the group have the birth on a given day, e.g. May 1st is
dbinom(2,23,1/365)


