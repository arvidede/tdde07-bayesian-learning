library('rstan')

########## A ##########
mu <- 10
sigma_sq <- 2
T <- 200 
x <- mu

par(mfrow=c(3,1))

AR <- function(mu, prev_x, phi, sigma_sq) {
  error <- rnorm(1, 0, sqrt(sigma_sq))
  return (mu + phi * (prev_x - mu) + error)
}

phis <- c(-1,0,1)

for(phi in phis) {
  result = c(x)
  for (i in 2:T) {
    x <- AR(mu, x, phi, sigma_sq)
    result <- c(result, x)
  }
  plot(result, type = 'line', main = bquote(phi ~ ' =' ~ .(phi)))
}


########## B ##########

mu <- 10
sigma_sq <- 2
T <- 200 
x <- mu

phis <- c(0.3, 0.95)

X = c()
for(phi in phis) {
  result <- c(x)
  for (i in 2:T) {
    x <- AR(mu, x, phi, sigma_sq)
    result <- c(result, x)
  }
  X <- rbind(X, result)
}

model <- stan_model('StanNormalModel.stan')

fitX <- sampling(model, data = list(T=200, x=X[1,]), iter = 3000, warmup = 500)
fitY <- sampling(model, data = list(T=200, x=X[2,]), iter = 3000, warmup = 500)

# Print the fitted model
print(fitX,digits_summary=3) # Extract posterior samples
print(fitY,digits_summary=3) # Extract posterior samples

postDrawsX <- extract(fitX)
postDrawsY <- extract(fitX)

par(mfrow=c(1,2))
plot(postDrawsX$mu, postDrawsX$phi)
lines(mean(postDrawsX$mu), mean(postDrawsX$phi), type='p', col="red")
# legend('topright', legend=expression(mean(postDrawsX$mu) ~ ',' ~ mean(postDrawsX$phi)))
plot(postDrawsY$mu, postDrawsY$phi)
lines(mean(postDrawsY$mu), mean(postDrawsY$phi), type='p', col="red")
# legend('topright', legend=expression(().mean(postDrawsY$mu) ~ ',' ~ ().mean(postDrawsY$phi)))

########## C ##########
library(ggplot2)
data <- read.table('campy.txt', header=TRUE)

N <- length(data$c) 

model <- stan_model('StanPoissonModel.stan')
fit <- sampling(model, data = list(N=N, c=data$c), iter = 2000, warmup = 1000)

# Print the fitted model
print(fit,digits_summary=3) # Extract posterior samples
postDraws <- extract(fit)

x <- postDraws$x

xMean <- c()
thetaUp <- c()
thetaLow <- c()

for (i in 1:length(x[1,])) {
  xMean <- c(xMean, mean(x[,i]))
  thetaUp <- c(thetaUp, exp(quantile(x[,i], probs = 0.975)))
  thetaLow <- c(thetaLow, exp(quantile(x[,i], probs = 0.025)))
}

# draws <- c()
# for(i in 1:140) {
#   draws <- c(draws, rpois(1, exp(xMean[i])))
# }

draws <- rpois(140, exp(xMean))

plot(data$c, type='line')
lines(draws, col='red')
lines(thetaUp, col='blue')
lines(thetaLow, col='green')

# geom_ribbon(mapping = aes(seq(1,140,1), ymin = thetaLow, ymax = thetaUp), alpha = 0.25)

# Do traceplots of the first chain
par(mfrow = c(1,1))
# plot(postDraws$mu[1:(1000)],type="l",ylab="mu",main="Traceplot")
# Do automatic traceplots of all chains
# traceplot(fit)
# Bivariate posterior plots
pairs(fit)
# plot(fit)

########## D ##########


data <- read.table('campy.txt', header=TRUE)

N <- length(data$c) 

model <- stan_model('StanPoissonModelD.stan')
fit <- sampling(model, data = list(N=N, c=data$c), iter = 2000, warmup = 1000)

# Print the fitted model
print(fit,digits_summary=3) # Extract posterior samples
postDraws <- extract(fit)

x <- postDraws$x

xMean <- c()
thetaUp <- c()
thetaLow <- c()

for (i in 1:length(x[1,])) {
  xMean <- c(xMean, mean(exp(x[,i])))
  thetaUp <- c(thetaUp, exp(quantile(x[,i], probs = 0.975)))
  thetaLow <- c(thetaLow, exp(quantile(x[,i], probs = 0.025)))
}

# draws <- rpois(140, exp(xMean))

plot(data$c, type='line')
lines(xMean, col='red')
lines(thetaUp, col='blue')
lines(thetaLow, col='green')

dev.off()
