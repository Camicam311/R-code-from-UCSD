# Math 185 Homework 3

# Spencer Ochs


## problem 1 ##
load("quake_times.rda") # 0.5 corresponds to noon
qt <- quake_times

## a) ##
# Check whether data is uniformly distributed in [0,1] using Kolmogorov-Smirnov test.

test <- ks.test(quake_times, "punif", min=0, max=1) # two-sided and exact
# p-value = 0.01761 Thus reject hypothesis that quake_times are uniformly distributed

## b) ##
# Anderson-Darling Test

# Compute Anderson Darling statistic from the data
# let F_0(x) = unif(0,1)

# Get test statistic from quake_times
n <- length(quake_times)
sorted_quake_times <- sort(quake_times) # Order the observations
W_n_vector <- 1:n
for (i in 1:n){
  W_n_vector[i] <- abs(sorted_quake_times[i] - i/n)/sqrt(sorted_quake_times[i] 
                                                         * (1 - sorted_quake_times[i]))
}
W_data <- max(W_n_vector) 
# test stat from data is W_n = 0.08286188 

# Get test statistics from simulated uniform samples for comparison
B <- 10000; W_b <- 1:n; W_sim <- 1:B

for (b in 1:B){
  # Simulate values from uniform distribution
  X_b <- runif(n=n, min=0, max=1)
  X_b <- sort(X_b) # make X_b the ordered observations
  # Compute test statistics from sample
  for (i in 1:n){
    W_b[i] <- abs(X_b[i] - i/n)/sqrt(X_b[i] * (1 - X_b[i]))
  }
  # Get Max value of W over i
  W_sim[b] <- max(W_b)
}

R3 <- sum(W_sim <= W_data) 
1 - R3/(B+1) # p-value = 0.1494253 for 2000 simulations, = 0.149785 for 10000 simulations


## Problem 2 ##
dat = read.table("Natality, 2007-2010.txt", skip=1, sep="\t", nrows=12)
births <- dat[,4] # observed counts
birth.weights <- dat[,2] # labels

# Estimate mean and std deviation of births under normal assumption
est.mean <- mean(births) # 333282.2
est.sd <- sd(births) # 514017.6

# Create vector of expected counts
exp.counts <- 1:length(birth.weights)
for(i in 1:10){
  exp.counts[i] <- sum(births) * (pnorm(500*i,mean=est.mean,sd=est.sd) 
                                  - pnorm(500*(i-1),mean=est.mean,sd=est.sd))
}
exp.counts[11] <- sum(births) * (pnorm(8165,mean=est.mean,sd=est.sd) 
                                 - pnorm(5000,mean=est.mean,sd=est.sd))
# ignoring those births where weight "not stated"
exp.counts[12] <- NA

# Test if normal with chisq.test
chisq.test(x=births[1:11], y=exp.counts[1:11]) # p-value = 0.2322
# With a p-value of 0.2322 can't say that the normal model fits the data very well
# looking at the data we can see that it is skewed to the left, perhaps a transformation
# might improve the model fit. 


## Problem 3 ##
load("185-hw3.rda") # gives vector pbm3
# Want to test whether population mean is 1
## a) ## 
# apply t-test
t.test(pbm3, mu=1) # p-value = 0.7135, cannot reject null that mean = 1

# check normality with Q-Q plot
qqnorm(pbm3, pch=16)
qqline(pbm3)
# plot looks normal, therefore we can feel comfotable using t-test, and that mean = 1

# draw several q-q plots of simulated normal samples
par(mfrow = c(3,4), mai=rep(0.1, 4), xaxt='n', yaxt='n')
pos = sample(1:12, 1)
for (i in 1:12){
  if (i == pos){ 
    qqnorm(pbm3, pch=16, main=" "); qqline(pbm3)
  } else {
    x = rnorm(19); qqnorm(x, pch=16, main=" "); qqline(x)
  }
}

dev.off()

## b) ## 
# Calibrate t-test by parametric bootstrap (Gamma dist)
install.packages("MASS")
library(MASS)
# Estimate the parameters of pbm3 assuming gamma distribution
fit <- fitdistr(pbm3,"gamma")
estimated.shape <- unname(unlist(fit)['estimate.shape']) # = 1.996129
estimated.rate <- unname(unlist(fit)['estimate.rate'])   # = 2.071182

# Parametric bootstrap: estimate parameters shape and rate, and resample from 
# Gamma(estimated.shape, estimated.rate)
B <- 10000
T_data <- (mean(pbm3)-1)/(sd(pbm3)/sqrt(n)) # under the null mean is 1, # -2.133811

R<- 0
for (b in 1:B){
  sample <- rgamma(n=length(pbm3), shape=estimated.shape, rate=estimated.rate)
  # Compute test statistic
  T_sim <- (mean(sample)-mean(pbm3))/(sd(sample)/sqrt(n)) 
  if(T_sim <= T_data) R <- R+1 # since know T_data = -2.133811
}
R/(B+1) # p-value is 0.3765623, ie 38% of values fall below T_data = -2.133811


## c) ##
# Calibrate the t-test using the nonparametric bootstrap
R2 <- 0
n <- length(pbm3)
# Studentized Bootstrap
for (b in 1:B){
  # Sample from pbm3 with replacement
  X_b <- sample(pbm3, size=length(pbm3), replace=TRUE)
  # Compute test statistics for the sample
  T_sim <- (mean(X_b)-mean(pbm3))/(sd(X_b)/sqrt(n)) 
  
  # Check against T from pbm3
  if(T_sim <= T_data) R2 <- R2+1  
}
R2/(B+1) # p-value is 0.03659634
