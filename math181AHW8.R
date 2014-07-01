Math 181A HW week 8

#plot power curve for 6.4.4

x <- seq(55,65,by=.1)

power_test <- function(mean1){

 beta <- pnorm(61.96-mean1) - pnorm(58.04-mean1)

 return(1 - beta)

}

y <- lapply(x,power_test)

plot(x,y, type="l", xlab= "mean for alternative hypothesis", ylab= "power")

#simulate random sample from N(60,16)

s <- rnorm(16,mean=60, sd=16)

summary_stat <- sum(s)/16 

summary_stat

# Note: critical region is (58.04,61.96)

# part 2

count <- 0

for(i in 1:100){

 s1 <- rnorm(16, mean=60, sd=4)

 summary_stat_1 <- sum(s1)/16 

 if(summary_stat_1 < 58.04 || summary_stat_1 > 61.96){count <- count+1}

}

count/100

#reflects alpha, number of times random sample from N(60,16) will be outside critical region by chance

# part 3

count <- 0

for(i in 1:100){

 s2 <- rnorm(16,mean=70, sd=4)

 summary_stat_2 <- sum(s2)/16 

 if(summary_stat_2 < 58.04 || summary_stat_2 > 61.96){count <- count+1}

}

count/100

# (1 - alpha) often very near 1, which is what we would expect from looking at power curve
