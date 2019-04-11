alpha <- 2
beta <- 2
s <- 14
n <- 20
nDraws <- c(10, 100, 1000)

# 1a

xGrid <- seq(0.001, 0.999, by=0.001)
posterior = dbeta(xGrid, alpha + s, beta + (n-s))
par(mfrow=c(1,3))
for (draws in nDraws) {
  random = rbeta(draws, alpha+s, beta+(n-s))
  hist(random, xlim = c(0,1), freq = FALSE, breaks=10)
  lines(xGrid, posterior, type='l', col ='red')
}
dev.off() 

# 1b

nDraws <- 10000
random = rbeta(nDraws, alpha+s, beta+(n-s))
val <- random * (random < 0.4)
print(sum(val>0)/length(val))

# 1c
nDraws <- 10000
random = rbeta(nDraws, alpha+s, beta+(n-s))
phi <- log(random/(1 - random))
plot(density(random), lwd=3, type='l', col='red')

