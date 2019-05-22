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
}
