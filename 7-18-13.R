# Notes for blog post on power of study designs, assuming population follows
# binomial distribution with low probability of success

N <- 2000 

p01 <- numeric(N)
p05 <- numeric(N)    # store power for p=.05
p1 <- numeric(N)    # store power for p=.1

for (i in 1:N){
  n <- i
  
  q01 <- qbinom(.99, n, .0005)
  q05 <- qbinom(.95, n, .0005)    # conclude effect if this many successes are detected
  q1 <- qbinom(.9, n, .0005)      # with higher or lower confidence
  
  p01[i] <- 1 - pbinom(q01, n, .002)        # probability of concluding an effect if leafleting 
  p05[i] <- 1 - pbinom(q05, n, .002)    # does convert 1/500 people (again with high/low conf.)
  p1[i] <- 1 - pbinom(q1, n, .002)
}

samplesize <- seq(1, 2000, by = 1)

estimatedpower <- data.frame(samplesize, p01, p05, p1)

ep <- melt(estimatedpower, id = "samplesize", var = "significance")

qplot(samplesize, value, data = ep, geom = "path", colour = significance, main = "power by sample size for effect: 1/500 success")

f1 <- numeric(N)
f2 <- numeric(N)
f3 <- numeric(N)
f4 <- numeric(N)
f5 <- numeric(N)

for (i in 1:N){
  n <- i
  
  f1[i] <- 1 - pbinom(0, n, .002)
  f2[i] <- 1 - pbinom(1, n, .002)
  f3[i] <- 1 - pbinom(2, n, .002)
  f4[i] <- 1 - pbinom(3, n, .002)
  f5[i] <- 1 - pbinom(4, n, .002)
}

simpleprob <- data.frame(samplesize, f1, f2, f3, f4, f5)

sp <- melt(simpleprob, id = "samplesize", var = "numberOfSuccesses")

qplot(samplesize, value, data = sp, geom = "path", colour = numberOfSuccesses, main = "probability of at least n successes")

p <- ggplot(sp, aes(samplesize, value))


p <- p + xlab("sample size") + ylab("probability") 
p <- p + ylim(0,1)
p <- p + opts(title ="Each trial has 1/500 chance of success.")

p1 <- p + layer(geom = "line", mapping = aes(linetype = numberOfSuccesses))
p1 <- p1 + scale_linetype("Minimum Successes", 
                        breaks = c("f1", "f2", "f3", "f4", "f5"),
                        labels = c("1", "2", "3", "4", "5"))
p1

p2 <- p + layer(geom = "line", mapping = aes(x = samplesize, y = value, 
                         colour = significance), data = ep)
p2 <- p2 + scale_colour_brewer("Significance Level", 
            palette = "Set1",
            breaks = c("p01", "p05", "p1"),
            labels = c("p < .01", "p < .05", "p < .1"))                 

p2

p3 <- p1 + layer(geom = "line", mapping = aes(x = samplesize, y = value, 
                                              colour = significance), data = ep)
p3 <- p3 + scale_colour_brewer("Significance Level", 
                               palette = "Set1",
                               breaks = c("p01", "p05", "p1"),
                               labels = c("p < .01", "p < .05", "p < .1"))  
p3

