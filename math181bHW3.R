## Math 181B HW 3 ##

## Spencer Ochs ##

install.packages("nortest")
library(nortest) 

## Problem 4 ##
grader1 <- c(3,0,5,2,5,5,5,4,4,5)
grader2 <- c(2,1,4,1,4,3,3,2,3,5)

## a) ##
# Want to test if the mean of grader 1 is equal to the mean of grader 2
# H_0: mean.grader1 = mean.grader2; vs. H_1: mean.grader1 != mean.grader2
# Would also want to test if variances are equal before running t-test

## b) ##
t.test(grader1,grader2,var.equal=FALSE)
# t = 1.478 is the value of the test statistic t at those values of graders 1 and 2
# df = 16.999 since variances are not assumed equal the degrees of freedom are approximated
# p-value = 0.1577 The probability of seeing this value of t or anything more extreme under
#    the null is about 16% so in this case we would not reject H_0
# 95 percent confidence interval: -0.4274951  2.4274951 This is the interval that 95% of the
#    time when running this test will 'capture' the true mean. In this case, this suggests
#    that the true mean, hence the true value for t, lies somewhere within this interval.
# sample estimates: mean of x 3.8, mean of y 2.8, These are the sample means calculated from
#    grader1 and grader2 using the formula for the MLE for a normal distribution's mean.

## c) ##
t.test(grader1,grader2,var.equal=TRUE)
# t = 1.478 same value of t above
# df = 18, degrees of freedom are now an integer since assumed the variances are equal, and 
#     since df are bigger than other t-test, we know the tails are smaller which means the
#     power increases for this test relative to when we assumed variances were not equal.
# p-value = 0.1567, assuming the variances are true gives a slightly smaller p-value, this is
#    because we have greater degrees of freedom, and since t=1.478 occurs in the tail, less
#    probability of seeing a value this extreme under the null.
# 95 percent confidence interval: -0.4214687  2.4214687, the confidence interval is smaller 
#    to reflect the smaller values of t when it has larger degrees of freedom, since smaller
#    tails will mean more of the pdf is closer to the mean. 
# sample estimates: mean of x 3.8, mean of y 2.8, the data did not change, only our 
#    assumptions about the structure of the data, and the calculation of the mean does not
#    rely on the variance.

## d) ##
# by plotting the boxplots of the two samples
boxplot(grader1, grader2)
hist(grader1)
# we can tell that grader1 does not look normal, further using qq plot and pearson test
qqnorm(grader1); qqline(grader1)
pearson.test(grader1) # p-value = 0.01545 -> reject assumption that grader1 is normal
# we can be confident that grader1 is not normally distributed.
# So in this case we would not trust the output of a t-test simply because of this lack of
# normality of one of the samples, and especially the small sample size.

# However, we also know that the graders are grading the same exams so we can consider the 
# data as paired in which case, we can use the paired t-test.
t.test(grader1,grader2,paired=TRUE) 
# which gives p-value = 0.008468 and CI: (0.325555, 1.674445) which are both smaller than 
# the unpaired t-tests, and would allow us to reject the null hypothesis at 5% significance.

# Note: By checking the difference of the paired data, grader1-grader2, 
qqnorm(grader1-grader2); qqline(grader1-grader2)
pearson.test(grader1-grader2)
# we see that we still don't have normality on the paired samples, so in this case still not
# completely comfortable using a t-test, paired or otherwise. But if we were to assume that 
# the t-test was robust to the deviation from normality of grader1-grader2, then we would 
# prefer to use the paired t-test.

# Note: Also assuming that the implementation of the paired t-test in R combines the samples
# so that there is only one variance.


## Problem 5 ##

## a) &  b) ##
x <- rt(100, df=4)
y <- rt(100, df=4)

par(mfrow=c(1,2))
hist(x, col="blue", main='x', xlab='')
hist(y, col="green", main='y', xlab='')
dev.off()

## c) ##
t.test(x,y, var.equal=TRUE) # tests H_0: u_x = u_y
t.test(x,y, var.equal=FALSE)
# in both t-tests the p-values are the same at 0.1306 and the 95% CI is the same up to 4 
# significant digits at (-0.0837, 0.6430) with the t-test assuming equal variances having a 
# slightly smaller CI, as we would expect, since it has larger degrees of freedom. 

# What we do not expect to see however, is such a low p-value since the data is generated 
# from the same distribution. However this is likely due to niether being actually normally 
# distributed (df are only 4 meaning much larger tails then normal) so the t-test's 
# assumptions would be violated, if only slightly, and we might have extreme values in the
# tails pulling the mean away from 0.
