## Math 185, HW 6 ##
## Spencer Ochs ##


dat = ToothGrowth
attach(dat)
dose = as.factor(dose)

# extract blocks from dat
unique(dose)
lev <- levels(dose)


## Problem 1 ##

kruskal2.test <- function(stacked.df=stack(ToothGrowth), B=2000, blocks=dose, 
                          treatment=supp, observations=len){
  # H_0: for each block j = 1,2,3,... Y_i,j,r are iid
  # H_1: This is not the case

  df <- unstack(stacked.df)
  lev <- levels(blocks)
  obs.stat <- numeric(length(lev))
  for(i in 1:length(lev)){
    dat.block <- stacked.df[blocks == lev[i], 1:2]
    ind <- (blocks == lev[i])
    obs.stat[i] <- kruskal.test(observations[ind], treatment[ind])$stat
  }
  two.way.obs.stat <- sum(obs.stat)

  # simulate the one-way Kruskal-Wallis statistic under the null
  n <- table(blocks,treatment)
  sim.stat <- numeric(length(lev))
  two.way.sim.stat <- 1:B
  
  for(b in 1:B){
    
    for(i in 1:length(lev)){
      dat.i <- df[blocks == lev[i], 1:2]
      Y.sim.i <- runif(nrow(dat.i))
      sim.stat[i] <- kruskal.test(Y.sim.i ~ dat.i[,2])$stat
    }
    
    two.way.sim.stat[b] <- sum(sim.stat)
  }
  
  p_value <- 1-sum(abs(two.way.sim.stat) > abs(two.way.obs.stat))/(B+1)
  return(p_value)
}

kruskal2.test()# for ToothGrowth dataframe give p-value = 0.0004997501 


## Problem 2 ##
perm2.aov <- function(stacked.df=stack(ToothGrowth), B=2000, observations=len,
                      treatment=supp, blocks=dose){
  
  #unstack()
  
  dat.aov = aov(observations ~ treatment)
  SST.obs <- summary(dat.aov)[[1]][1,2]
  SST.sim <- numeric(B)
  # Simulated/permuted values
  for(b in 1:B){
    lev <- unique(blocks)
    permute.obs <- numeric(length(observations))
    for(i in 1:length(lev)){
      ind <- (blocks == lev[i])
      permute.obs[ind] <- sample(observations[ind])
    }
    SST.sim[b] <- summary(aov(permute.obs ~ treatment))[[1]][1,2]
  }
  
  p_value <- sum(abs(SST.sim) > abs(SST.obs))/(B+1)
  return(p_value)
}

perm2.aov() # gives p-value = 0.0004997501 when dataframe is ToothGrowth
