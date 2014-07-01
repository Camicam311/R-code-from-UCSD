# Statistics HW #4

#(1) simulate Binomial
binomVec <- rbinom(n=10, s=1, p=0.4)
print(binomVec)
#Confidence Interval
k <- sum(binomVec)
# 95% confidence interval gives \alpha = 0.05, z_(\alpha/2) = z_(.025) = 1.96
lowerLim <- k/10 - (1.96)*((k/10)*(1-k/10)/10)^(1/2)

upperLim <- k/10 + (1.96)*((k/10)*(1-k/10)/10)^(1/2)

#check if confidence interval contains true p = 0.4
print(c(lowerLim,upperLim, lowerLim <= 0.4 && upperLim >= 0.4))

# (2) Simulate (1) 100 times
frequency <- 0
for(i in 1:100){ #100 trials
  #new trial
  binomVec <- rbinom(n=100, s=1, p=0.4)
  binomVec
  k <- sum(binomVec)
  lowerLim <- k/100 - (1.96)*((k/100)*(1-k/100)/100)^(1/2)
  upperLim <- k/100 + (1.96)*((k/100)*(1-k/100)/100)^(1/2)
  #check if p in CI
  if(lowerLim <= 0.4 && upperLim >= 0.4){
    frequency <- frequency+1
  }
}
#empirical coverage prabability
print(frequency/100)

# (3) repeat (2) for 200 trials
frequency <- 0
for(i in 1:200){ #100 trials
  #new trial
  binomVec <- rbinom(n=200, s=1, p=0.4)
  k <- sum(binomVec)
  lowerLim <- k/200 - (1.96)*((k/200)*(1-k/200)/200)^(1/2)
  upperLim <- k/200 + (1.96)*((k/200)*(1-k/200)/200)^(1/2)
  #check if p in CI
  if(lowerLim <= 0.4 && upperLim >= 0.4){
    frequency <- frequency+1
  }
}
#empirical coverage prabability
print(frequency/200)

# As the number of trials increases the error bars catch a higher percentage of the empiricle coverage.
