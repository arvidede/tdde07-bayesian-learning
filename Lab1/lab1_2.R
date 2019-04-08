library(invgamma)

income <- c(14,25,45,25,30,33,19,50,34,67)
m <- 3.5
n <- length(income)
n_draws = 10000

t <- 0

for (v in income) {
  t <- t + (log(v) - m) ^ 2
}

tSquared <- t / n

# simulate postarior draws

X_draw <- rchisq(n_draws, n)
sigma_sq = n*tSquared / X_draw

#theoretical

theoretical <- function(theta, v, s) {
  return ( ((v/2) ^ (v/2)) / gamma(v/2)  * (s ^ v) * (theta ^ (-(v/2+1))) * exp((-v * s ^ 2) / (2 * theta) ))
}

range <- seq(0,10,by=0.01)
mx <- 0 * range

for (i in 1:length(range)) {
  mx[i] <- theoretical(range[i], length(income), sqrt(tSquared))
}

# plot
hist(sigma_sq, freq=FALSE) 
lines(range, mx, lwd=3, type='l', col='red', xlab = 'sigma')

