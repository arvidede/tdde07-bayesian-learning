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
}
