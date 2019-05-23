data {
  int<lower=0> N; // Number of observations
  int c[N];
}

parameters {
  real<lower=0> x[N]; // Data points
}

transformed parameters {
  real lambda[N]; 
  lambda[N] = exp(x[N]);
}

model {
  //likelihood
  for(n in 1:N) 
      c[n] ~ poisson(lambda[n]); // Poisson
      
  //prior
}
