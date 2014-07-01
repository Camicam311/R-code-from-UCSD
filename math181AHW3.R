#Homework 3 #14
#Weibull distribution

#random sample from Weibull density function w/ \alpha = 1, \beta = 2
# => "shape parameter" a = 1, "scale parameter" b = 1

x <- rweibull(100, shape = 1, scale = 1) #100 random variates

hist(x)
mean(x) #value given is 0.7108096
median(x) #value given is 0.4284179
var(x) #value given is 0.7138365

#library(MASS)
library(stats4)

fit <- fitdistr(x,"weibull")

#function for mle to use
negLL <- function(alpha, beta) {
  -sum(dweibull(x, a, b, log = TRUE))
}
#fit <- mle(negLL, start = list(0.1,0.1), nobs = 100)

#MLE of Weibull

#Likelihood function of weibull from ?rweibull
L <- function(a,b) {
  prod((a/b)*(x/b)^(a -1)*exp(-(x/b)^a))
}

#Negative Log-Likelihood of weibull
neg_LL <- function(a,b) -log(L(a,b))

#Compute MLE of negative Log-Likelihood
fit <- mle(neg_LL, start = list(a = 1.5, b = 1.5)) # a = 0.9105222, b = 0.6779325

#plot of the likelihood as functions of alpha and beta
require(grDevices)
at <- bt <- seq(1,3,len=100)
#create z matrix for contour plot
z <- mat.or.vec(100,100)
# fill out z matrix, rows: at, col: bt
count_i <- count_j <- 0
for (i in at) {
  count_i <- count_i + 1
  for (j in bt) {
    count_j <- count_j + 1
    z[count_i,count_j] <- L(i,j)
  }
}
