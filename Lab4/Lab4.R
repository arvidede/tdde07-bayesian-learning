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
  mu ~ normal(10,4); // Normal with mean 0, st.dev. 100
  phi ~ normal(10,2);
  sigma2 ~ scaled_inv_chi_square(1,2); // Scaled-inv-chi2 with nu 
}'

fit = stan(model_code = StanModel, data = list(T=200), iter = 1000, warmup = 0)
