data {
  int<lower=0> N; // n observations 
  real y[N]; // observed 
  real y_last[N]; // last observed
}
parameters {
  real<lower=0, upper = 10> sigma;
  real beta;
  real alpha; 
}
transformed parameters {
  vector[N] mu; 
  for( i in 1:N)
  mu[i] <- alpha + beta*y_last[i];
}
model {
  //priors
  beta ~ normal(0, 10);
  alpha ~ normal(0, 10);
  //likelihood 
  y ~ normal(mu, sigma);
}