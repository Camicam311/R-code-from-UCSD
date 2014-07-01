# Math 185 Homework 3

# Spencer Ochs


## problem 1 ##
install.packages('UsingR')
father.son <- read.table("http://stat-www.berkeley.edu/users/juliab/141C/pearson.dat",
                         sep=" ")[,-1]
# V2 = fathers height, V3 = sons height. Both in inches


# paired data (father height, son height) so becomes a univariate numerical data problem.
# can make univariate by taking the difference:
father.son.univariate <- father.son[,'V2'] - father.son[,'V3']

# First we want to test that the mean height of the fathers is equal to the mean height of
# the sons, or:
# Null Hypothesis: mean height of father equals mean hight of son
# Alternative: mean heights are not equal

# Assuming the data is normally distributed  we can use  the (two-sided) student t-test

# But first, need to check if univariate data is normal
qqnorm(father.son.univariate)
qqline(father.son.univariate)
# looks normally distributed

# checking normality of univariate data with pearsons test of normality gives:
install.packages("nortest")
library(nortest) 
pearson.test(father.son.univariate) # which gives p-value = 0.06132
# Lilliefores Test (and Kolmogorov-Smirnov test)
lillie.test(father.son.univariate) # p-value = 0.2767

# Know that t-test is robust to deviations from normality, and we have larger sample
# size, so combined with the QQ plot we decide to use the t-test 

# (two sided) t-test to check H_0 : u_x - u_y = 0 vs. H_1 : u_x - u_y != 0 we get:
t.test(father.son.univariate, mu=0)
# p-value < 2.2e-16 So we reject the null that father's heights are equals to son's heights.

# Sign test: (Doesn't assume normality, resistant to outliers, "quick")
# H_0 : median = 0 vs. H_1 : median != 0
# since this data is normally distributed, the mean is the same as the median and so this 
# translates into the previous t-test, but were we to run it, would get:
n = length(father.son.univariate) # sample size
dat.sign = sign(father.son.univariate)
nplus = sum(dat.sign==1)
n0 = sum(dat.sign==0)
Z = (nplus +n0/2 -n/2)/sqrt(n/4)
pnorm(Z) # Here the normal approximation is valid since n > 9 (rule of thumb)
pbinom(nplus, n-n0, 0.5) # exact p-values = 1.357282e-19
# So we reject the hypothesis that the median of the distribution of the difference of the 
# heights is at 0.

# Wilcoxon Signed Rank test: testing for symmetry of the distribution of heights about
# the mean u = 0
# H_0: father's height - son's height are symmetric about the mean = 0,
# H_1: not symmetric about 0
wilcox.test(father.son.univariate) # p-value < 2.2e-16
# thus reject the null that the difference between heights is symmetric about 0.

# From these tests have determined that the difference between fathers and son's heights in
# this data set is likely normally distributed, but we can say with high confidence that 
# the mean/median is not at zero, the heights of fathers and sons are more often not the same.


## Problem 2 ##

# copy and paste data from webpage "http://lib.stat.cmu.edu/DASL/Datafiles/DRPScores.html"
dr.scores <- "Treatment Response
Treated  24
Treated	43
Treated	58
Treated	71
Treated	43
Treated	49
Treated	61
Treated	44
Treated	67
Treated	49
Treated	53
Treated	56
Treated	59
Treated	52
Treated	62
Treated	54
Treated	57
Treated	33
Treated	46
Treated	43
Treated	57
Control	42
Control	43
Control	55
Control	26
Control	62
Control	37
Control	33
Control	41
Control	19
Control	54
Control	20
Control	85
Control	46
Control	10
Control	17
Control	60
Control	53
Control	42
Control	37
Control	42
Control	55
Control	28
Control	48
"
dr.scores.table <- read.table(textConnection(dr.scores), header=TRUE)

treated <- NULL; control <- NULL # vectors for different student scores
for(i in 1:44){
  if(dr.scores.table[i,1] == "Treated"){
    treated <- append(treated, dr.scores.table[i,2])
  } 
  else control <- append(control, dr.scores.table[i,2])
}
# Now have two vectors 'treated' and 'control'

# Two sample, (unpaired) bivariate numerical data, since two independent(?) classes
# 44 students total, 21 in treatment, 23 in control.

# Two sample t-test (variances not assumed equal):
# Null Hypothesis: mean of control is equal to mean of treatment
# Alternative: mean of control is less than mean of treatment

# Checking assumptions:
# check normality of each sample with a separate QQ plot (don't assume equal variances)
par(mfrow=c(1,2))
qqnorm(control, pch=21, bg="blue")
qqline(control)
qqnorm(treated, pch=21, bg="green")
qqline(treated)
# both look normal enough to use t-test
# Check variances
boxplot(control); boxplot(treated)
hist(control); hist(treated)
var.test(control,treated) # p-value = 0.05067
# variances don't look equal, thus use t-test without assuming equal variances:
t.test(control,treated, alternative="less") # p-value = 0.01319, reject the null

# Two sample median test:
# One sided test to see if median of treatment is greater than median of control
# H_0: median of treatment = median of control. vs. 
# H_1: median of treatment > median of control
source("median.test.R")
median.test(control, treated, alternative="less")
# p-value = 0.0722, fail to reject the null hypothesis

# Wilcoxon Rank Sum Test
# no assumption of normality and resistant to outliers
# Null: the population medians of the two samples is equal
# Alternative: the median of the treatment group is greater than the control group
wilcox.test(control, treated, alternative="less", exact=FALSE)
# Note that wilcoxon rank sum test cannot compute exact p-value with ties, however the 
# result given was p-value = 0.006339 which uses a normal approximation.
# so we reject the null that the two medians are equal.

# Kolmogorov Smirnov Two Sample Test:
# Null: treatment and control come from the same (continuous) distributions
# Alternative: the CDF of control is larger than the CDF of treated, meaning that the
# distibution of treated is shifted to the right of control. 
ks.test(control, treated, alternative="greater", exact=FALSE)
# Note: ks.test cannot compute exact p-value with ties, however if we approximate the 
# p-value we get: p-value = 0.007834 which means we believe that the distibution of 
# treatment is to the right of the distribution of control, which is what we expect from
# looking at the plots of the data.

# Permutation Test:
# Hypothsis testing same as above for Kolmogorov Smirnov
one.perm.diff = function(x, y) {
  m = length(x)
  z = c(x, y)
  Ind = sample(length(z))
  mean(z[Ind[1:m]])-mean(z[Ind[-(1:m)]])
}

dummy = replicate(1000, one.perm.diff(control, treated))
p_val = sum(dummy <= mean(control)-mean(treated))/1000
hist(dummy, breaks=50, main='Histogram of 2,000 permutated differences')
abline(v=mean(control)-mean(treated), col=2)
p_val # p-value = 0.015 so would reject the null, meaning pdf of treatment is right of the 
# pdf of control

# From these tests can conclude that the DRP scores were improved by the treatment at a 
# statistically significant level. 


## Problem 3 ##

permdiff <- function(x,y,B){
  # x and y are samples, B = number of simulations returns a p-value for comparing the
  # population means of x and y using a difference permutation test based on B permutations
  #
  # Compute the test statistic from the observed data
  m <- length(x)
  z <- c(x,y)
  D_obs <- mean(control) - mean(treated)
  #
  # Compute test statistics from simulated data
  D_sim <- numeric(B) # vector for test statistics for B simulations
  for(b in 1:B){
    Ind = sample(length(z)) # permute the indices
    D_sim[b] <- mean(z[Ind[1:m]])-mean(z[Ind[-(1:m)]])
  }
  p_value <- sum(D_sim <= D_obs)/(B+1)
  #
  return(list("p-value for CDF of x < CDF of y" = p_value))
}

permdiff(control,treated,20000) # gives p-value = 0.01534923 for B = 20000
