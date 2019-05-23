data {
  int<lower=0> T; // Number of observations 
  real y[T];
}

parameters {
  real mu; 
  real<lower=0> sigma2; 
  real phi;
}
 
model {
  //priors
  mu ~ normal(10,2);
  phi ~ uniform(-1,1);
  sigma2 ~ scaled_inv_chi_square(1,2); // Scaled-inv-chi2 with nu 
  
  //likelihood
  for (t in 2:T)
    y[t] ~ normal(mu + phi * y[t-1], sqrt(sigma2));
}
