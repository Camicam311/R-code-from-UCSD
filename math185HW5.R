# Math 185 Homework 5

# Spencer Ochs 


## problem 1 ##

# have 4-sample data, not balanced
singers <- read.table("Singers.txt", fill=TRUE, header=TRUE)

# side by side boxplot
boxplot(singers, col=c("#ffffff","#cccccc","#777777","#333333"), ylab='Singer Height')

# one-way ANOVA (with equal variances)
# Null Hypothesis: all the means are equal (ie the average height of each group is the same)
# Alternative: not all means are equal (some groups taller than others)
oneway.test(values ~ ind, data = stack(singers), var.equal=TRUE)

singer.aov <- aov(values ~ ind, data = stack(singers))
summary(singer.aov)

# want to know which singers have different heights, use Tukey to correct for multiple tests
# Hypothesis test of means between each group.
Diff = TukeyHSD(singer.aov)
plot(Diff, cex.axis=0.8)

# Kruskal-Wallis test
# Null Hypothesis: each singer groups heights come from the same distribution, and all heights
# measured are iid.
# Alternative: This is not the case.
kruskal.test(singers)

# Permutation test
# Same hypothesis testing as Kruskal-Wallis test
# see next problem for implementation.


## Problem 2 ##
perm.aov <- function(one.way.df=singers, B=2000){
  # takes one-way data frame
  # permutation test, test whether each cell Y_i,j is iid.
  
  # 1) Compute observed SS_T (sum of squares treatment)
  number.obs <- length(as.vector(na.omit(stack(one.way.df))$values)) # # of observations
  grand.mean <- sum(as.vector(na.omit(stack(one.way.df))$values))/number.obs  
  SS_T.obs <- 0 # observed treatment sum of squares
  k <- ncol(one.way.df) # gives number of blocks
  height.df <- nrow(one.way.df) # gives height of biggest group in data frame
  n <- 1:k # vector for storing size of groups
  for (j in 1:k){
    n[j] <- nrow(na.omit(one.way.df[j])) # gives number of values in a group j
    mean.col.j <- as.numeric(colMeans(na.omit(one.way.df[j])))
    SS_T.obs <- SS_T.obs + n[j]*(mean.col.j - grand.mean)^2 # between group sum of squares
  }
  
  # 2) Simulate SS_T's after permutation.
  stacked.dat <- stack(one.way.df) # concatenated sample
  values <- as.vector(na.omit(stacked.dat$values)) # get values (omitting any NA's)
  
  SS_T.sim <- rep(0,B) # vector for storing simulated values
  new.blocks <- matrix(data=NA, nrow=nrow(one.way.df), ncol=ncol(one.way.df)) # permuted table
  
  for(b in 1:B){
    # 2a) permute values
    Ind <- sample(length(values))
    # 2b) reallocate values to original # of rows
    for (j in 1:k){
      # create column vector with values up to orignal length of block, and NA's the rest.
      new.blocks[,j] <- c(values[Ind[1:n[j]]], rep(NA,height.df-n[j]))
    }
    # 2c) calculate SS_T_sim[b]
    for (j in 1:k){
      mean.col.j <- mean(na.omit(new.blocks[,j]))
      SS_T.sim[b] <- SS_T.sim[b] + n[j]*(mean.col.j - grand.mean)^2 # 
    }
  }
  # 3) Compute p-value  
  p_value <- 1-sum(SS_T.sim<SS_T.obs)/(B+1)
  # print(p_value)
  return(p_value)
}

# running perm.aov(B=100000) gives p-value of 9.999e-06
# I got a observed SS_T of 940.8504, and none of the simulated came close to that value.
# the max simulatd SS_T I got was 521.6363.
# Thus we conclude that this value for the singers data table is very (very) unlikely to 
# happen by chance under the null that each Y_i,j is iid, and we reject the null.


## Problem 3 ##

# a) Graphical method from 'Naive Analysis of Variance' by W. John Braun

# Apply graphical method to singers dataset

# code for plotting 'Naive Analysis of Variance' by W. John Braun
naiveANOVA <- function(dataset, k=150){
  color <- c("black", "red", "blue", "green4", "purple", "green", "brown")
  dataset <- dataset[order(dataset[,1]),]
  means <- sapply(split(dataset[,2],dataset[,1]),mean)
  n <- sapply(split(dataset[,2], dataset[,1]), length)
  sim.data <- dataset[,2]-rep(means, n) + mean(dataset[,2])
  sim.means <- NULL
  for (i in 1:k) {
    sim.sample <- sample(sim.data, size=n, replace=TRUE)
    sim.means <- c(sim.means, mean(sim.sample))
  }
  hist(sim.means, xlim=range(c(sim.means, means)),
       xlab="simulated treatment averages", main="", cex.lab=1.4)
  points(means, rep(0, length(means)), cex=2.5, col=color[1:length(means)],
         pch=14+1:length(means))
  legend("topleft", legend= c(paste("average", unique(dataset[,1]))),
         pch=14+1:length(means), col=1:length(means))
}

# Need to put singers into two column data frame as described in paper
colnames(singers) <- c(1,2,3,4)
two.col.df <- na.omit(stack(singers))
x <- two.col.df$ind
y <- two.col.df$values
two.col.df <- as.data.frame(cbind(x,y))

# plot singer data with naive ANOVA
naiveANOVA(two.col.df) 


# b) Nonparametric bootstrap for calibrating F-test
# involves sampling from the empirical distributions

nonparametric.bootstrap.F.test <- function(one.way.df=singers, B=2000){
  
  number.obs <- length(as.vector(na.omit(stack(one.way.df))$values)) # # of observations
  grand.mean <- sum(as.vector(na.omit(stack(one.way.df))$values))/number.obs 
  col.mean <- as.vector(sapply(na.omit(one.way.df),mean)) # column of means for each group
  col.var <-  as.vector(sapply(na.omit(singers),var))
  SS_T.obs <- 0 # observed treatment sum of squares
  SSE.obs <- 0
  k <- ncol(one.way.df) # gives number of blocks
  height.df <- nrow(one.way.df) # gives height of biggest group in data frame
  n <- 1:k # vector for storing size of groups
  for (j in 1:k){
    n[j] <- nrow(na.omit(one.way.df[j])) # gives number of values in a group j
    #mean.col.j <- as.numeric(colMeans(na.omit(one.way.df[j])))
    #var.col.j <- as.numeric()
    SS_T.obs <- SS_T.obs + n[j]*(col.mean[j] - grand.mean)^2 # between group sum of squares
    SSE.obs <- SSE.obs + (n[j]-1)*(col.var[j])^2
  }
  
  F.obs <- (SS_T.obs/(k-1))/(SSE.obs/(number.obs-k)) # 5.04435 for singers

  # Simulate values of F to compare against F.obs
  F.sim <- 1:B
  for (b in 1:B){
    # sample from the empirical distribution for each group in turn
    sample.groups <- matrix(nrow=height.df,ncol=k)
    for (j in 1:k){
      sample.groups[,j] <- c(sample(sapply(one.way.df[j], as.numeric),n[j], replace=TRUE),
                             rep(NA,height.df-n[j])) # sample enough to fill up column
    }
    # Compute F.sim for the newly populated matrix
    SS_T.sim <- 0; SSE.sim <- 0
    grand.mean <- sum(sample.groups, na.rm=TRUE)/number.obs 

    for (j in 1:k){
      col.mean.sim <- mean(sample.groups[,j], na.rm=TRUE)
      col.var.sim <- var(sample.groups[,j], na.rm=TRUE)
      SS_T.sim <- SS_T.sim + n[j]*(col.mean.sim - grand.mean)^2 # between group sum of squares
      SSE.sim <- SSE.sim + (n[j]-1)*(col.var.sim)^2
    }
    
    F.sim[b] <- (SS_T.sim/(k-1))/(SSE.sim/(number.obs-k))
  }
  p_value <- 1-sum(F.sim>F.obs)/(B+1)
   
  return(p_value)
}
# gives a p-value of 0.0004997501 for B=10000 on singers dataset, as we would expect after 
# seeing the graphical anova and how far apart the groups are.
