## Math 181B HW 2 ##
## 4/26/13        ##
## Spencer Ochs   ##

install.packages("nortest")
library(nortest) # used in problems 3 and 5

## Problem 1 part e) ##

# create table from count data
col1 <- c(34,6)
col2 <- c(19,16)
table1 <- cbind(col1,col2)
rownames(table1) <- c("Patient survived", "Patient did not survive")
colnames(table1) <- c("With disinfectan", "Without disinfectant")
table1
# Test for association using chi square test of independence
chisq.test(table1)
# get p-value = 0.007803 so can reject at 5% and 1% significance level.


## Problem 2 ##
dat <- read.table("GSS.txt")
## a) ##
.Table <- xtabs(~ dat$education + dat$premarital.sex)

## b) ##
chisq.test(.Table)
# The expected frequncies are all >5, so we can trust the test
# X-squared = 512.354 means that the computed value from the chi square test of independence
# is quite large, and with only 4 = (3 - 1)(3 - 1) degrees of freedom we get the result:
# p-value < 2.2e-16 which means it is very unlikely to see a test statistic this large under
# the null.

## c) ##
# verify calculation by R:
tab <- addmargins(.Table) # get sums from table, calculate matrix of expected counts
expected.count <- (tab['Sum',1] * tab[1,'Sum'])/tab['Sum','Sum'] # 4238.497
# calculate test statistic from observed and expected, etc.

# based on the p-value we conclude that within the population who takes general social 
# surveys there is an association between education and opinions on premarital sex, with 
# significance level of <1%


## Problem 3 (10.4.6) ##
avg.SAT.score <- c(911,1011,939,935,895,969,898,892,849,879,844,881,969,1024,876,1080,1044,
                   997,1011,883,908,901,1009,1057,1013,1017,986,1025,913,924,893,1003,888,
                   860,1056,966,1019,927,879,882,838,1031,1023,886,1067,899,893,922,921,
                   1044,980)
# Test if SAT scores normally distributed
pearson.test(avg.SAT.score) # p-value = 0.009509 -> conclude not normally distributed


## Problem 4 (10.5.2) ##
Manufacturing <- c(168, 42)
Other <- c(73,26)
table4 <- cbind(Manufacturing,Other)
rownames(table4) <- c("Extremely or somewhat", "Not very")

# Want to test for association. Null hypothesis is that the importance of a "high-quality" 
# work force is viewed the same by all different types of business. 
chisq.test(table4) # test for independence gives p-value = 0.2745 so data does not suggest
# that the importance of a high-quality work force is viewed differently by diff' companies.

# Also by checking
addmargins(table4)
# we can tell by inspection that expected counts are >5


## Problem 5 ##
# chisq.test and outliers

## a) ##
sample1 <- rnorm(n=100,mean=0,sd=1)

## b) ##
# sample + 10 outliers to the right of 3, and 10 outliers to the left of -3
sample2 <- c(rnorm(n=100,mean=0,sd=1), rexp(10, rate=.1)+3, -rexp(10,rate=.1)-3)
sample3 <- c(rnorm(n=100,mean=0,sd=1), rexp(6, rate=.05)+3, -rexp(6,rate=.05)-3)


## c) ##
# test for composite hypothesis of normality
pearson.test(sample1) # p-value = 0.8703
pearson.test(sample2) # p-value < 2.2e-16
pearson.test(sample3) # p-value < 2.2e-16

# Conclude that adding 20 outliers to 100 normal data has a huge influence on the pearson 
# test; as evidenced by the disparity in p-values.

## d) ##
sample1.t <- rt(n=100,df=4) # from plots looks like large outlier is >5
sample2.t <- c(rt(n=100,df=4), rexp(6, rate=.05)+5, -rexp(6,rate=.05)-5)
# repeating step c) assuming we want to test for normality again
pearson.test(sample1.t) # p-value = 0.0001735
pearson.test(sample2.t) # p-value < 2.2e-16
# once again see difference made by adding outliers


