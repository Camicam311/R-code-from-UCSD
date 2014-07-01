
## Math 181B HW 6 ##

## Spencer Ochs ##

## Problem 1 ##

dat <- read.table("http://www.math.ucsd.edu/~jbradic/math181b/indicators.txt",header=TRUE)

## a)
plot(dat$PriceChange, dat$LoanPaymentsOverdue)
fit <- lm(dat$LoanPaymentsOverdue ~ dat$PriceChange)
abline(fit, col='red', lwd = 2)
summary(fit) # slope is -0.15412 and Pr(>|t|) = 2.71e-11 so we know this is significant
cor(dat$PriceChange, dat$LoanPaymentsOverdue) # -0.2629791
# Together with the plot and the values in the summary we conclude there is evidence of 
# negative linear association between the two variables.

## b)

e <- rstandard(fit)
lpo.pred <- predict(fit)
plot(lpo.pred,e, bg="purple")
abline(h = 0, lty = 2, col = 'red') 
# looking at the plot we have outliers near 3, 4, and 4.7 so those are problematic
outlier <- (e > 2)
dat$MetroArea[outlier] # gives areas: Camden, Elizabeth, and Indianapolis
# which I'm guessing is Philadelphia, New Jersey, and Indiana respectively.
# So Philadelphia is the biggest outlier (the most problematic).

## c)
attach(dat)
loan.fit <- lm(PriceChange ~ LoanPaymentsOverdue)
newloan <- data.frame(LoanPaymentsOverdue=4)
predict(loan.fit, newloan, interval="predict")
#  fit       lwr      upr
# -2.617566 -10.20611 4.970978
detach(dat)


## Problem 2 ##

bond <- read.table("http://www.math.ucsd.edu/~jbradic/math181b/bonds.txt", header=TRUE)

plot(bond$CouponRate, bond$BidPrice)
fit.b <- lm(BidPrice ~ CouponRate, data=bond)
abline(fit.b, col='red', lwd = 2)

residuals = resid(fit.b)
plot(bond$CouponRate,residuals)
abline(h = 0, lty = 2)
# shows clear trend of line being off

# must remove first three points and try again
good <- (bond$CouponRate > 5)
new.lm <- lm(BidPrice ~ CouponRate, data=bond, subset=good)
plot(bond$CouponRate[good], bond$BidPrice[good])
abline(fit.b, col='sienna3', lwd=2)  #  old line
abline(new.lm, col='green', lwd=2)  #  new line

e.new = rstandard(new.lm)
VL.predicted.new = predict(new.lm)
plot(VL.predicted.new, e.new, bg = 19, pch = 21)
abline(h=0, lwd=2, col='royalblue', lty=2)
# in this case now have possible quadratic curve to data, but this is best linear model
