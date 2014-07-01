### Math 185 Take Home Final ###

### Spencer Ochs ###


## Problem 1 ##

uniform_test <- function(n=200, k=4, B=2000){
  # Generate samples from the uniform distribution
  pearsonGOF_stats <- 1:B
  for(b in 1:B){ # simulate the pearsonGOF stat under the null (each category 1:k chosin w/ 1/k prob)
    # create sample of count data from the uniform dist in k different bins
    # force table to have k levels, count table for number of times 1:k appears in data
    samp <- table(factor(sample.int(k, size=200, replace=TRUE, prob=rep(1/k, k)), levels=1:k))
    # test that count data is uniformly distributed, expected count = 200/k
    pearsonGOF_stats[b] <- chisq.test(samp, p = rep(1/k, k))$stat[[1]]
  }
  # Since the null hypothesis is true pearsonGOF_stats will have chi-squared dist with k-1 df
  # Check that the B values of pearsonGOF are distributed in the shape of a chi squared w/ df = k-1
  hist(pearsonGOF_stats) # plot values, compare to chi-squared dist's for that degree of freedom
  return(ks.test(pearsonGOF_stats, "pchisq", df=k-1))
}

## a) k = 4
uniform_test(k=4, B=2000)
# gives p-value of 0.2203 so we cannot reject the null that the GOF stats are chi-squared distributed
# also the plot generated looks like what we would expect from seeing pictures of the chi-squared
# distribution with 3 degrees of freedom. And since the expected counts are 200/4 = 50 the rule of thumb
# that exp counts must be > 5 holds.

## b) k = 40
uniform_test(k=40, B=2000)
# expected count is 200/40 = 5, so observed counts may fall below 5 in each bin but the rule of thumb is
# not violated and when we run the test many times we get lower p-values then when k=4 but most do not
# reject the null, and the histograms of the data look like the chi-squared distribution with 39 df

## c) k = 400
uniform_test(k=400, B=2000)
# expected count = 200/400 = .5 so bins are too small (rule of thumb is > 5) so GOF test is compromised
# and in fact get warning messages that "Chi-squared approximation may be incorrect". Running the test 
# many times we often get p-values that would have us reject the null that the GOF test stats are 
# chi-squared distributed.
# We know that when n is large and k is fixed then the  Pearson GOF statistics has, approximately, a 
# chi2 distribution with k-1 degrees of freedom under the null hypothesis, and this has been shown when
# k is small enough that the expected counts are > 5. However when k is large we find that the GOF stats
# no longer are chi2 distributed. So can conclude if rule of thumb is violated and expected counts are 
# too small then the GOF stats will not be chi-squared distributed as expected.


## Problem 2 ##
install.packages("MASS")
require(MASS)
attach(steam)
perm.cor <- function(df = steam, B = 2000){
  # takes as input bivariate paired numerical data in form of data frame with 2 columns
  # returns p-value obtained by simulation and permutation Pearson's correlation test.
  X_obs <- df[,1]
  Y_obs <- df[,2]
  corr_obs <- cor.test(X_obs, Y_obs, method="pearson")$estimate
  
  # Permute Y values then calculate the correlation
  corr_sample <- 1:B
  for(b in 1:B){
    Y_permuted <- sample(Y_obs)
    corr_sample[b] <- cor.test(X_obs, Y_permuted, method="pearson")$estimate
  }
  p_value <- 1 - sum(abs(corr_sample) <= abs(corr_obs))/(B+1) # two sided
  return(p_value)
}
perm.cor(B=100000) # gives p-value of 1.99998e-05 which is very close to value given by cor.test


## Problem 3 ##

lowbwt <- read.table(file= "lowbwt.txt", header=TRUE)[,c(3:11)] # ignore ID and LOW

## a) Association between race and smoking habit
#
# Note: both Race and Smoke are categorical variables, therefore can use test of independence
# H_0: race of mother and smoking habit are independent
# H_1: race and smoking habit are dependent
Smoke <- as.factor(lowbwt$SMOKE)
Race <- as.factor(lowbwt$RACE)

lowbwt_table <- table(Race,Smoke)
# plots
mosaicplot(lowbwt_table)
barplot(lowbwt_table, legend=TRUE, args.legend=list(x = "topright"), xlab="Smoking during pregnancy", ylab= "Race")
# tests
chisq.test(lowbwt_table) # p-value = 1.865e-05 so reject null and conclude that race and smoking associated


## b) Correlation between mother's and baby's weights
#
# Note: Paired Bivariate Numerical data
install.packages("nortest")
require(nortest)

mother_wt <- lowbwt$LWT # weight of mother in pounds at last menstrual period
baby_wt <- lowbwt$BWT # Birth Weight in Grams of baby

# Correlation t-test
# assuming measurements are i.i.d. bivariate normal
qqnorm(mother_wt); qqline(mother_wt) # does not look normal
pearson.test(mother_wt) # p-value = 3.079e-09, so cannot say that mother_wt is normal 
qqnorm(baby_wt); qqline(baby_wt) # looks normal
# we cannot have bivariate normal since mother's weight not normal, but if we were to say that the test
# was robust to this level of non-normality, we could test:
# H_0: correlation between mother's and baby's weight is 0
# H_1: correlation does not equal 0
cor.test(mother_wt, baby_wt) # p-value = 0.01048, and cor = 0.1857887 indicating weak linear relationship

# Permutation test
# H_0: mother's weight and baby's weight are independent
# H_1: weights are monotonically associated (using Kendall's distribution free test)
cor_obs <- cor(mother_wt, baby_wt, method="kendall")
cor_sim <- 1:2000
for(b in 1:2000){
  baby_wt_permuted <- sample(baby_wt)
  cor_sim[b] <- cor(mother_wt, baby_wt_permuted, method="kendall")
}
# p-value
1 - sum(abs(cor_sim) <= abs(cor_obs))/(2000+1) # gives 0.00149925 so reject null that independent

# Spearman's Rank Correlation
# H_0: mother's weight and baby's weight are independent
# H_1: mother's weight and baby's weight are monotonically associated
cor.test(mother_wt, baby_wt, method="spearman")
# since have ties cannot compute exact p-value, but p-value given is 0.0005708 so reject null
# and since rho is 0.2483077 we have baby's weight is monotonically increasing for mother's weight

# Kendall's Tau Correlation
# H_0: mother's weight and baby's weight are independent
# H_1: mother's weight and baby's weight are monotonically associated
cor.test(mother_wt, baby_wt, method="kendall")
# p-value given is 0.0006343 so reject null that weights are independent
# and since tau is 0.169429 we have baby's weight is monotonically increasing for mother's weight


## c) Whether smoking habit during pregnancy affects the baby's birth weight
# baby's birth weight dependent variable, smoking habit independent

## (i) ignore all other variables 
# have multivariate numerical data, Question: does smoking habit affect baby's birth weight?
# interested in testing:
# H_0: mean baby weight of nonsmoker equals mean baby weight of smoker
# H_1: means are not equal (two sided?)

baby_wt <- lowbwt$BWT # Birth Weight in Grams of baby
smoke <- lowbwt$SMOKE # smoking habit of mother during pregnancy 

# ANOVA table
dat.aov = aov(baby_wt ~ smoke)
summary(dat.aov)
# The p-value is very small (0.00916). 

# diagnostic plots for ANOVA
par(mfrow=c(1,2))
plot(dat.aov, which = 1) # residuals vs means plot to check for homoscedasticity
# variances look equal
plot(dat.aov, which = 2) # qqplot of the residuals to check for normality
# data looks normal
# assume independence since clinical trial
# thus we trust the p-value given above, and reject the null that the mean of the birth weights are same

# Hruskal-Wallis test 
# H_0: the observations are i.i.d. v.s. H_1: this is not the case
kruskal.test(baby_wt, smoke)
## Again, we have a small p-value  = 0.00708 so observations not iid

# Tukey's HSD doesn't apply since only have two means to compare so would get same answer as anova

# Permutation Test
# H_0: the observations are i.i.d. v.s. H_1: this is not the case
perm.aov = function(values, ind, B=2000){
  dat.aov = aov(values ~ ind)
  f_ratio_obs = summary(dat.aov)[[1]][1,4]
  f_ratio_sim = numeric(B)
  for(b in 1:B){
    values_b = sample(values)
    dat.aov_b = aov(values_b ~ ind)
    f_ratio_sim[b] = summary(dat.aov_b)[[1]][1,4]
  }
  p_val = 1 - sum(f_ratio_sim <= f_ratio_obs)/(B+1)
  return(p_val)
}
perm.aov(baby_wt, smoke, 2000) # p-value = 0.008995502 so reject the null that obs's are i.i.d.
# perm.aov function from HW solution 5 at http://math.ucsd.edu/~eariasca/math185.html

# Baby's weight on smoking habit as categorical (factor) using linear model
Smoke <- as.factor(smoke)
boxplot(baby_wt ~ Smoke, col=c("#ffffff","#cccccc"), ylab='Birth Weight', xlab='Smoking habit')
fit <- lm(baby_wt ~ Smoke) 
summary(fit) # both intercept and slope are significant and p-value: 0.009156
anova(fit) # adding smoke gives significantly better fit to the data
# From above know that there is decreasing slope such that birth weights of smokers are less than the 
# birth weights of non-smokers (during pregnancy).


## (ii) Taking into account all the variables
# build regression model explaining BWT as a function of the other variables
# AGE, LWT are numerical, RACE, SMOKE, PTL, HT, UI, FTV are categorical
attach(lowbwt)
race <- as.factor(RACE)
smoke <- as.factor(SMOKE)
PTL <- as.factor(PTL) # history of premature labor
HT <- as.factor(HT) # history of hypertension
UI <- as.factor(UI) # presence of uterine irritability
FTV <- as.factor(FTV) # number of physician visits during first trimester 

plot(lowbwt) # hard to say if correlation between any variables just from plots
require(ellipse)
plotcorr(cor(lowbwt)) # correlation between smoke and race, possible correlation between LWT and HT, as
# well as between PTL and UI, though nothing too extreme. Check var inflation factors below to be sure.
fit_all <- lm(BWT ~ ., data=lowbwt) # explain BWT as a function of all other variables
# here we have fit a linear model but if we wanted to refine it we could remove variables which are 
# correlated with other independent variables, and which do not improve the fit of the model.
summary(fit_all) # AGE, PTL, and FTV all look as if they can be removed from the model
# from this summary can see that LWT is related to increased baby weight, while RACE, SMOKE, HT, and
# UI are all strongly related to decreased baby weight.
anova(fit_all)
# looks as though FTV can be removed without affecting the model
# checking for multicolinearity via variance inflation factors (VIF)
require(car)
vif(fit_all) # none of the variables have VIF_j > 10 so don't worry about correlation between predictors
# Diagnostic plots of full model
par(mfrow=c(2,2))
plot(fit_all, which = c(1,2,4))
# fitted values vs residuals look good - errors homoskedastic, qqplot is good fit - residuals are 
# normally distributed, no observation has Cook distance greater than 1
plot(hatvalues(fit_all), type='h', main="Hat values") # detect outliers in predictor
abline(h=2*8/nrow(lowbwt))
# many outliers in predictor (~25), ignore for now since too many to remove without cause
# checking outliers in response via externally studentized residuals
plot(abs(rstudent(fit_all)), type='h', col="blue", ylab="Externally Studentized Residuals (in absolute value)", main="Externally Studentized Residuals (in absolute value)")
abline(h = qt(.95, nrow(lowbwt)-8-2), lty=2) # threshold for suspects
# again large number of outliers in response (~18) so uncomfortable removing them without cause

# We now go about simplifying/reducing the model, if we were to attempt to fit a model using only the
# methods we have learned in class, we would check each predictor by fitting a new model with it removed
# and comparing the previous model with the new smaller model by anova.
# removing the least significant variable from summary(fit_all), choose AGE
fit_no_AGE <- update(fit_all, .~.-AGE) # create new model with AGE removed
summary(fit_no_AGE) # adjusted R-squared has increased
anova(fit_no_AGE, fit_all) # Pr(>F) = 0.9713 so removing age had no significant effect on the model.
# we could then continue this way removing predictors until we have reached the minimum adequate model,
# and removing any more variables would affect the adjusted R-squared (the proportion of variance 
# explained by the linear model), and all variables included are significant.
# To automate this process we use backwards stepwise regression, which performs a greedy search for the
# model with the lowest Akaike Information Coefficient (ie the goodness of fit of the model minus the 
# complexity, or adjusted R-squared) which, while it is not guarunteed to find the "best" model, is 
# equivalent to the step by step process I would perform with my limited knowledge of model selection.
fit_sbr <- step(fit_all, direction="backward")
# this gives the model: BWT ~ LWT + RACE + SMOKE + HT + UI
summary(fit_sbr) # shows that all the variables are significant, and adjusted R-squared is 0.2 which is
# better than other models have inspected. 
anova(fit_sbr) # shows each additional variable added to the model is significant
# Checking the model with diagnostic plots we get:
par(mfrow=c(2,2))
plot(fit_sbr, which = c(1,2,4)) # diagnostics look "decent"
plot(hatvalues(fit_sbr), type='h', main="Hat values")
abline(h=2*5/nrow(lowbwt))
# residuals look homoskedastic and normal, and no observation is a Cook's distance > 1
# There are 23 outliers in predictor but again decide to ignore them since so many.
plot(abs(rstudent(fit_sbr)), type='h', col="blue", ylab="Externally Studentized Residuals (in absolute value)", main="Externally Studentized Residuals (in absolute value)")
abline(h = qt(.95, nrow(lowbwt)-5-2), lty=2) # threshold for suspects
# again have large number (~20) outliers in response, with that many would be strange to say that they
# should be removed because they are not typical of the overall data. So ignore/keep them.
# So we can say that the linear model assumptions aren't violated, and outliers are likely not adversely
# affecting the fit.

# from here we can add interaction terms to the model to give different slopes at different values of the
# categorical values, but I prefer a simpler model to avoid overfitting since this regression would be
# useful for prediction (and overfitting could compromise that).
# However if did choose to use interaction terms, I could imagine a model selection process where we 
# start with a full model including all interaction terms then run backwards step regression to prune 
# away those interactions which aren't significant to the model.

## Formally test whether SMOKE is in the model
# 1) testing whether SMOKE should be in the full model:
fit_noSMOKE <- lm(BWT~AGE+LWT+RACE+PTL+HT+UI+FTV, data=lowbwt) # full model with SMOKE removed
anova(fit_noSMOKE, fit_all) # compare full model with SMOKE to model with all variables except SMOKE
# Pr(>F) = 0.001085 so SMOKE variable should be included in the model, so even though RACE is already in
# the model (which it is correlated with) adding the smoke variable still improves the model enough to 
# include it. 
# 2) testing whether smoke should be in the reduced/simplified model:
fit_sbr_noSMOKE <- lm(BWT ~ LWT + RACE + HT + UI)
anova(fit_sbr_noSMOKE, fit_sbr) # rejects at p = 0.0005927 in favor of simplified model with SMOKE
# so we can say that SMOKE should be included in the full model as well as in the minimum model produced
# by stepwise backward regression.
# This implies that a mother's smoking habit during pregnancy has a significant linear association to 
# what the baby's birth weight will be. Perhaps follow up studies would then determine if there is a 
# causation between smoking and lower birth weight.


## Problem 4
require(alr3)
data(hooker)
dat = hooker
attach(dat, warn.conflicts = FALSE)

lm.boot.ci <- function(bi_pair = hooker, conf_level = 0.95, B = 2000){
  # using bi_pair data frame, explain the second variable as a function of the first variable
  alpha <- 1 - conf_level  
  # calculate estimates for slope and intercept based on bivariate paired numerical data bi_pair
  fit <- lm(bi_pair[,2] ~ bi_pair[,1])
  observed_intercept <- coef(fit)[1]
  observed_slope <- coef(fit)[2]
  
  sample_intercepts <- numeric(B)
  sample_slopes <- numeric(B)
  
  for(b in 1:B){
    # bootstrap X and Y pairs, sample pairs of values
    new_sample <- matrix(nrow=nrow(bi_pair), ncol=2)
    for(i in 1:nrow(bi_pair)){ # extra loop slows down, but couldn't find sample function for pairs
      r <- sample(nrow(bi_pair), size=1) # random indices
      new_sample[i,1] <- bi_pair[r,][[1]] # assign random pair of values to new sample
      new_sample[i,2] <- bi_pair[r,][[2]]
    }
    X_b <- new_sample[,1]
    Y_b <- new_sample[,2]
    # calculate estimates for slope and intercept based on bth bootstrap sample
    fit_sample <- lm(Y_b ~ X_b)
    sample_intercepts[b] <- coef(fit_sample)[1]
    sample_slopes[b] <- coef(fit_sample)[2]
  }
  # get percentile values of bootstrap estimates, and calculate CI's
  intercept_CI_lower <- 2*observed_intercept - quantile(sample_intercepts, 1-alpha/2)
  intercept_CI_upper <- 2*observed_intercept - quantile(sample_intercepts, alpha/2)
  slope_CI_lower <- 2*observed_slope - quantile(sample_slopes, 1-alpha/2)
  slope_CI_upper <- 2*observed_slope - quantile(sample_slopes, alpha/2)
  
  
  return(data.frame(list(intercept_CI_lower=intercept_CI_lower, intercept_CI_upper=intercept_CI_upper),
                    list(slope_CI_lower=slope_CI_lower, slope_CI_upper=slope_CI_upper)))
}

# running function with confidence level 95% on hooker dataset, gives
lm.boot.ci(B=10000)
#              pivot_bs_CI_lower    pivot_bs_CI_upper
# intercept    -70.07107            -61.2387 
# slope         0.4237586            0.4703969

# stardard confidence intervals based on t-distribution
fit_hooker <- lm(Pressure ~ Temp)
confint(fit_hooker)
#                   2.5 %      97.5 %
#  (Intercept) -67.3357219 -61.4897805
#  Temp          0.4250565   0.4555074 

# Checking normality of data
require(nortest)
qqnorm(Pressure); qqline(Pressure); pearson.test(Pressure)
qqnorm(Temp); qqline(Temp); pearson.test(Temp)
# both look questionable, and pearson test of normality rejects at < .005 for normality of Pressure
# so we would say the default t-test method is not reliable since it requires data to be approx normal
# whereas with bootstrap we know that converges to true probability as B increases, so at B = 10000 we 
# can be rather confident in its calculated values, so I would use the bootstrap CI's even though they
# are rather close in values to the std confidence intervals based on the t-distribution.
