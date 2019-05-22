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

phis <- c(0.3, 0.95)
arData <-c()

for(phi in phis) {
  x <- mu
  X = c(x)
  for (i in 2:T) {
    x <- AR(mu, x, phi, sigma_sq)
    X <- c(X, x)
  }
  arData <- rbind(arData, X)
}

StanModel = '
data {
  int<lower=0> T; // Number of observations 
}
parameters {
  real mu; 
  real<lower=0> sigma2; 
  real phi;
 }
model {
  mu ~ normal(10,2);
  phi ~ normal(0,1);
  sigma2 ~ scaled_inv_chi_square(1,2); // Scaled-inv-chi2 with nu 
}'

fit = stan(model_code = StanModel, data = list(T=200), iter = 2000, warmup = 1000)

# Print the fitted model
print(fit,digits_summary=3) # Extract posterior samples
postDraws <- extract(fit)
# Do traceplots of the first chain
par(mfrow = c(1,1))
plot(postDraws$mu[1:(1000)],type="l",ylab="mu",main="Traceplot")
# Do automatic traceplots of all chains
traceplot(fit)
# Bivariate posterior plots
pairs(fit)



########## C ##########

data <- read.table('campy.txt', header=TRUE)

mu <- mean(data$c)
sigma_sq <- 2
phi <- 0.5 #fairly correlated
N <- length(data$c) 
x <- mu
x_t <- c(x)

for (i in 2:N) {
  x <- AR(mu, x, phi, sigma_sq)
  x_t <- c(x_t, x)
}

x_t <- exp(x_t) 

StanModel = '
data {
  int<lower=0> N; // Number of observations
  int c[N];
}
parameters {
  real<lower=0> x[N]; // Data points
}
model {
  for(n in 1:N) 
      c[n] ~ poisson(x[n]); // Poisson
}'

fit = stan(model_code = StanModel, data = list(N=N, c=data$c), iter = 2000, warmup = 1000)

# Print the fitted model
print(fit,digits_summary=3) # Extract posterior samples
postDraws <- extract(fit)
# Do traceplots of the first chain
par(mfrow = c(1,1))
# plot(postDraws$mu[1:(1000)],type="l",ylab="mu",main="Traceplot")
# Do automatic traceplots of all chains
# traceplot(fit)
# Bivariate posterior plots
pairs(fit)
# plot(fit)



########## D ##########
