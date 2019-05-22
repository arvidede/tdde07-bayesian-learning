alpha <- 2
beta <- 2
s <- 14
f <- 6
n <- 20
nDraws <- c(10, 100, 1000)

# 1a
# What are the true values of the posterior mean and standard deviation? 
# Look at the Wikipedia page for the beta distribution or in the table of 
# distributions on the course webpage. Compute these based on the posterior parameters. 
# Then compute the same values based on the posterior samples 
# (using mean() and sd() in R) and see if you get the same results.
# To show the convergence, make a plot that shows the posterior mean and standard deviation on the y-axis, 
# computed based on posterior samples and number of samples on the x-axis. 
# You can then also include the true values in this plot.

xGrid <- seq(0.001, 0.999, by=0.001)
posterior = dbeta(xGrid, alpha + s, beta + (n-s))
par(mfrow=c(1,3))
for (draws in nDraws) {
  random = rbeta(draws, alpha+s, beta+(n-s))
  mean <- mean(random)
  stdv <- sd(random)
  trueMean <- (alpha + s)/(alpha + s + beta + f)
  trueStdv <- (sqrt((alpha + s)*(beta + f)/((alpha + s + beta + f)^2 * (alpha + s + beta + f + 1))))
  hist(random, xlim = c(0,1), freq = FALSE, breaks=10)
  lines(xGrid, posterior, type='l', col ='red')
  
}
dev.off() 

# 1b

nDraws <- 10000
random = rbeta(nDraws, alpha+s, beta+(n-s))
val <- random * (random < 0.4)
print(sum(val>0)/length(val))
real <- pbeta(.4, alpha + s, beta + (n-s))

# 1c
nDraws <- 10000
random = rbeta(nDraws, alpha+s, beta+(n-s))
phi <- log(random/(1 - random))
plot(density(random), lwd=3, type='l', col='red')

