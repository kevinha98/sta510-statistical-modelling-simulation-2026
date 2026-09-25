## Markov chain and the Markov chain Monte Carlo method

rm(list=ls())

### General algorithm to sample Markov chains with a given transition matrix

markov <- function(mat,start,n) { ## markov(matrix, starting state, # of steps)
  state <- start
  k = dim(mat)[[1]]
  for (i in 2:(n+1)) 
    state <- sample(1:k,1,prob=mat[state,])
  return(state)
}

### The transition matrix of a random walk on the pentagon graph
RWonP<- matrix(c(0,1/2,0,0,1/2,1/2,0,1/2,0,0,0,1/2,0,1/2,0,0,0,1/2,0,1/2,1/2,0,0,1/2,0),nrow=5,ncol=5)#,byrow=T)
RWonP

###To show that RW on the pentagon is a regular Markov chain it is enough to show that RWop^5 >0
RWonP%*%RWonP%*%RWonP%*%RWonP%*%RWonP #Note that we use %*% for matrix multiplication and not *

### We check that the uniform measue on (1,2,3,4,5) is stationary wrt RWoP
unif<-c(1/5,1/5,1/5,1/5,1/5)
unif
unif%*%RWonP
unif==unif%*%RWonP

###Simulating the RWoP with the random walk starting at 1.
start=1
simlist<- replicate(10000,markov(RWonP,start,200))
table(simlist)/10000

###Estimating probabilities by using the LLN for Markov chains

markovLLN <- function(mat,start,n) { ## markov(matrix, starting state, # of steps)
  state=seq(n+1)
  state[1] <- start
  k = dim(mat)[[1]]
  for (i in 2:(n+1)) 
    state[i] <- sample(1:k,1,prob=mat[state[i-1],])
  return(state)
}

SimRWonP = markovLLN(RWonP,start,100000)
table(SimRWonP)/100000

SimRWonP = SimRWonP[-c(1:10000)]
table(SimRWonP)/90000

### The MCMC method

mcmc <- function(mat, start, n, PI) { ## markov(matrix, starting state, # of steps, tarket distribution)
  state <- start
  for (i in 2:(n+1)) {
    proposal <- sample(seq(5),1,TRUE,prob=mat[state,])
    accept <- (PI[proposal] *mat[proposal,state])/(PI[state]*mat[state,proposal])
    if (runif(1) < accept) {state <- proposal  }
    }
    return(state) }

### Example 1
PI <- c(0.1, 0.1, 0.3, 0.3, 0.2) #Tarket distribution

trials <- 5000
simlist <- replicate(trials,mcmc(RWonP,1,200,PI))
table(simlist)/trials

mcmc2 <- function(mat, start, n, PI) { ## markov(matrix, starting state, # of steps, tarket distribution)
  state <- numeric(n+1)
  state[1] <- start
  for (i in 2:(n+1)) {
    old=state[i-1]
    proposal <- sample(seq(5),1,TRUE,prob=mat[old,])
    accept <- (PI[proposal] *mat[proposal,old])/(PI[old]*mat[old,proposal])
    if (runif(1) < accept) {state[i] <- proposal  }
    else {state[i]=old}
  }
  return(state) }

SimDist = mcmc2(RWonP,1,10000,PI)
table(SimDist)/10000

### Example 2 - power law distribution

### In order to use the previous algorithm we need to specify an infinite matrix. This is clearly not feasible. However, a more direct construct is possible:

trials <- 1000000
simlist <- numeric(trials)
simlist[1] <- 2 #starting from 2
for (i in 2:trials)
  {  	if (simlist[i-1] ==1) #checks whether we are in state 1 for which the random walk jumps to 2
    {  p <- (1/2)^(3)
      new <- sample(c(1,2),1,prob=c(1-p,p)) #samples the new state
      simlist[i] <- new} 
      else { leftright <- sample(c(-1,1),1) #sample one the neighbouring points
          if  (leftright == -1)   #In this case we always accept
            { simlist[i] <- simlist[i-1] - 1} 
          else { p <- (simlist[i-1]/(simlist[i-1]+1))^(2) #probability of acceptance
                  simlist[i] <- sample(c(simlist[i-1],1+simlist[i-1]),1,prob=c(1-p,p))
} } } 
dat <- simlist[1000:trials]  # discard first 1000
tab <- table(dat)/length(dat)	#calculates the distribution
tab[1:9] #Outputs the first 9 values

exact<-seq(9)
for (i in 1:9){exact[i]=(6/(pi^2))*(i^(-2))}
exact  


### Example 3 - The hard-core model on the m by m grid with 0 boundary condition

HardCore = function(n, m){
state=matrix(seq((m+2)^2),nrow=(m+2))*0 #embedding the matrix in a larger matrix with 0's at the boundary
for (i in 1:n){
  x=sample(seq(m),2,replace=TRUE)+c(1,1) #picking the coordinates of a vertex in the interior of the graph uniformly at random
  coin=runif(1) #sample a coin flip
    if( coin<=0.5 && (state[x[1]-1,x[2]]+state[x[1]+1,x[2]]+state[x[1],x[2]-1]+state[x[1],x[2]+1])==0 )
    {state[x[1],x[2]]<-1}
    else {state[x[1],x[2]]=0}
}
return(state)
}


### Simulating for m=2
m=2

start=matrix(seq((m+2)^2),nrow=(m+2))*0
OneTrial=HardCore(100000,m)
OneTrial

HardCoreLLN = function(n, m){
  state=list()
    state[[1]]=matrix(0,nrow=m+2,ncol=m+2) #embedding the matrix in a larger matrix with 0's at the boundary
  #state<- list(n+1,state)
  for (i in 2:(n+1)){
    x=sample(seq(m),2,replace=TRUE)+c(1,1) #picking the coordinates of a vertex in the interior of the graph uniformly at random
    coin=runif(1) #sample a coin flip
    state[[i]]=state[[i-1]]
    if( coin<=0.5 && (state[[i]][x[1]-1,x[2]]+state[[i]][x[1]+1,x[2]]+state[[i]][x[1],x[2]-1]+state[[i]][x[1],x[2]+1])==0 )
    {   state[[i]][x[1],x[2]]=1 }
    else {state[[i]][x[1],x[2]]=0}
  }
  return(state)
}

n=100000
RunHC2= HardCoreLLN(n,2)

TimesEqualState<-seq(n)
for(i in 2:n+1){#checking for each simulating whether the outcome matrix equaled the all 0 matrix
  TimesEqualState[i] <-sum(all(RunHC2[[i]]==start))
}

NumbConfig0 = 1/mean(TimesEqualState==1) #Estimating the total number of feasible configurations
NumbConfig0

Prob_OneTrial = mean(TimesEqualState==1)
Prob_OneTrial

### Simulating for m=3
m=3
start=matrix(seq((m+2)^2),nrow=(m+2))*0

OneTrial=HardCore(100000,m)
OneTrial

n=100000
RunHC3= HardCoreLLN(n,3)

TimesEqualState<-seq(n)
for(i in 2:n+1){#checking for each simulating whether the outcome matrix equaled the all 0 matrix
  TimesEqualState[i] <-sum(all(RunHC3[[i]]==start))
}

NumbConfig0 = 1/mean(TimesEqualState==1) #Estimating the total number of feasible configurations
NumbConfig0

Prob_OneTrial = mean(TimesEqualState==1)
Prob_OneTrial
