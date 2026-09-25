### Illustration of number of heads in k throws of a fair dice

# Remove old variables:
rm(list=ls())

# Simulate the number of heads in k throws of a dice repeated Nsim times
# We let 0 correspond to tail and 1 correspond to head,
# The sample()-command below randomly draw a 0 or 1 with equal probability k times, 
# and then sum() will summarize the number of heads
simheads <- function(Nsim,k){
  nheads <- vector(length=Nsim) # Define a vector to store the number of heads in each repetition
  for(i in 1:Nsim)              # Generate the number of heads Nsim times
    nheads[i] <- sum(sample(0:1,size=k,replace=TRUE))  # See explanation above
  return(nheads)                # Return the resulting vector
}
  

simheads(Nsim=10,k=6)
simheads(Nsim=100,k=3)
simheads(Nsim=100,k=5)

# Summarize the results by histograms and numbers
# Repeat the lines below many times for different numbers of Nsim and k
# What happens when Nsim gets large?
k <- 3
Nsim <- 1000
nheadsim <- simheads(Nsim=Nsim,k=k)
relfreq <- table(nheadsim)/Nsim   # Calculate relative frequency for each outcome
relfreq
barplot(relfreq,ylab="Relative frequency")
mean(nheadsim)
var(nheadsim)




