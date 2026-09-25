# Remove old variables:
rm(list=ls())

# Approximate calculation of probability and expectation
# by simulating from the negative binomial distribution

r <- 6
psuccess <- 0.91
nsim <- 100
x <- r+rnbinom(n=nsim, size=r, prob=psuccess) # Generate nsim obs from the neg. bin. distribution
relfreq <- table(x)/nsim   # Calculate relative frequency for each outcome
barplot(relfreq,ylab="Relative frequency")
relfreq
# Probability of at least 8
1-sum(relfreq[1:2])
# Expectation
r/psuccess
# Mean of data
mean(x)
# Repeat the above lines for different values of nsim and compare to the exact answer

# Exact calculation using the built in function for calculating neg. bin. probabilities
pnbinom(q=1,size=r,prob=psuccess,lower.tail = FALSE)
