## Math 181B HW 4 ##

## Spencer Ochs ##

install.packages("nortest")
library(nortest) # 

## Problem 1 ##

# create table of data
ozone_free <- c(41.0,25.9,13.1,-16.9,15.4,22.4,29.4,26.0,38.0,21.9,27.3,1.4,27.4,17.7,21.4,26.6)
ozone_rich <- c(10.1,7.3,-9.9,17.9,6.6,39.9,-14.7,-9,6.1,14.3,6.8,-12.9,12.1,-15.9,44.1,20.4)
dat <- cbind(ozone_free,ozone_rich)
names(dat) <- c("Ozone-free", "Ozone-rich")

## a) ##
# Does ozone affect the weights?
# Null Hypothesis: mean weight of ozone-free is equal to mean weight of ozone-rich
# Alternative: mean weights are not the same.

# want to test if mean weights are the same, first check for normality of each sample, then
# check the variance of each.

# check normality of each sample with a separate QQ plot (don't assume equal variances)
par(mfrow=c(1,2))
qqnorm(ozone_free, pch=21, bg="blue")
qqline(ozone_free)
qqnorm(ozone_rich, pch=21, bg="green")
qqline(ozone_rich)
# neither sample looks very normal
# checking with pearsons test of normality:
pearson.test(ozone_free) # gives p-value = 0.2873, so cannot reject the null that normal
pearson.test(ozone_rich) # gives p-value = 0.2087, likewise cannot say not normal.
# So after seeing results of pearson's test of normality decide to use two sample t-test

# Check variances
boxplot(ozone_free); boxplot(ozone_rich)
hist(ozone_free); hist(ozone_rich)
var.test(ozone_free,ozone_rich) # p-value = 0.3283
# variances don't look equal, even though var.test returned high p-value choose not to use 
# result (since concerned with multiple inferences and visually can tell variances not same), 
# thus use t-test without assuming equal variances:
t.test(ozone_free,ozone_rich) # p-value = 0.02418, reject the null at 5% significance

## b) ##
# 98% confidence interval:
t.test(ozone_free,ozone_rich, conf.level= 0.98)
# 98 percent confidence interval: (-0.4713083, 27.3213083)

## c) ##
# want to use t.test to find out how big sample size should be given the other parameters
# Specifying mean_x - mean_y = d = 5 and power is 0.9, we know that as sample size 
# increases the power also increases, so the n given (number of observations per group) will
# be the minimum n needed.
install.packages("pwr")
library(pwr)
pwr.t.test(n=NULL, d=5, sig.level=0.05, power=0.9, type="two.sample", alternative="two.sided")
# gives n = 2.328877 where n is the number of samples in each group, thus n >= 3 gives 
# power = 0.9 when difference between the means is 5.


## Problem 2 (9.3.8 from Larsen & Marx) ##

# two sample t-test:
# to decide which version of the two sample t-test to use, we must know what the variances
# are for each sample. (Since problem requires using t-test going to assume normality).
schools.in.AfricanAmerican.neighborhoods <- c(36,28,41,32,46,39,2432,45)
schools.in.White.neighborhoods <- c(21,14,11,30,29,6,18,25,23)

boxplot(schools.in.AfricanAmerican.neighborhoods); boxplot(schools.in.White.neighborhoods)
hist(schools.in.AfricanAmerican.neighborhoods); hist(schools.in.White.neighborhoods)
var.test(schools.in.AfricanAmerican.neighborhoods,schools.in.White.neighborhoods) 
# Variances do not look the same, and we get a p-value of 5.995e-15, so we would definately
# use the Behrens-Fisher approximation for the t-test.


## Problem 3 (9.4.8) ##

# Null Hypothesis: proportion of divorce rates in successful breeders is the same as the 
# divorce rate in unsuccessful breeders.
# Alternative: This is not the case.
# 
# Want to make a test of proportions, divorced succesful breeders = 175 out of 609. Divorced
# unsuccesful breeders = 100 out of 160.
prop.test(c(175,100), c(609,160)) # p-value = 4.611e-15 which is statistically significant,
# thus reject null hypothesis.


## Problem 4 (9.5.10) ##
# Construct an 80% CI for p_M - p_W in the nightmare freq data from case study 9.4.2

m <- 160 # number of men
x <- 55 # men with nightmares often
w <- 192 # number of women
y <- 60 # women with nightmares often
alpha <- 0.2 # gives 80% Confidence Interval
z_lower <- qnorm(alpha/2); z_upper <- qnorm(1 - alpha/2) 

CI_lower <- x/m - y/w + z_lower*sqrt((x/m)*(1-x/m)/m + (y/w)*(1-y/w)/w) # from Thm 9.5.3
CI_upper <- x/m - y/w + z_upper*sqrt((x/m)*(1-x/m)/m + (y/w)*(1-y/w)/w)
CI <- c(CI_lower,CI_upper)
CI # gives (-0.03319675, 0.09569675)
