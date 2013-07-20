w <- rbinom(10000, 5, .1) # 10000 times, draw a sample of five wizards and count how
                          # many eat no chocolate (if 90% eat chocolate)
m <- rbinom(10000, 5, .1)

qplot(m-w, xlab="muggle abstainers - wizard abstainers")

pop <- c(rep(0,times = 4954),rep(1, times = 51)) # create a population of 5005 with 
                                                 # 51 marked

result <- numeric(10000)

for (i in 1:10000){                         
  x <- sample(pop, 5, replace = FALSE)          # choose 5 muggles without replacement
  result[i] <- sum(x)                           # count how many abstain from chocolate
}

qplot(result, xlab = "muggle abstainers")       # graph

hist(result, plot = FALSE)                # setting plot to false gives a numeric summary

result2 <- numeric(10000)

for (i in 1:10000){                         
  x <- sample(pop, 5, replace = TRUE)          # choose 5 muggles with replacement
  result2[i] <- sum(x)                           # count how many abstain from chocolate
}

qplot(result2, xlab = "muggle abstainers")       # graph

hist(result2, plot = FALSE)                # setting plot to false gives a numeric summary