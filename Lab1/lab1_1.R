alpha <- 2
beta <- 2
s <- 14
f <- 6
n <- 20
f <- n-s
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
trueMean <- (alpha + s)/(alpha + beta + s + f)
trueStdv <- sqrt(((alpha + s) * ( beta + f)) / ((alpha + beta + n)^2 * (alpha + beta + n + 1)))
par(mfrow=c(1,3))
for (draws in nDraws) {
  random = rbeta(draws, alpha+s, beta+(n-s))
  hist(random, xlim = c(0,1), freq = FALSE, breaks=10)
  lines(xGrid, posterior, type='l', col ='red')
}
dev.off() 

mean <- c()
stdv <- c()
for (i in 1:1000) {
  random = rbeta(i, alpha+s, beta+(n-s))
  mean <- c(mean, mean(random))
  stdv <- c(stdv, sd(random))
}

# plot mean
plot(mean, type='l', col='green', xlab = 'n draws')
abline(h = trueMean)
legend('topright', legend = 'True mean', col = 'black', lwd = 1)

# Plot standard diviation 
plot(stdv, type='l', col='black', xlab='n draws')
abline(h = trueStdv, col='red')
legend('topright', legend = 'True stdv', col = 'red', lwd = 1)



# 1b
nDraws <- 10000
random = rbeta(nDraws, alpha+s, beta+(n-s))
print(sum(random < 0.4)/length(random))
real <- pbeta(.4, alpha + s, beta + (n-s))

# 1c
##you compute random draws of phi, but then you plot the density of the 
##original parameter theta. Plot the density of phi instead. 
##Also the mathematical statement should say theta instead of phi on the right hand side.

nDraws <- 10000
random = rbeta(nDraws, alpha+s, beta+(n-s))
phi <- log(random/(1 - random))
plot(density(phi), lwd=1, type='l', col='red', main = expression('Density of' ~ phi))
