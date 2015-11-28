data {
  int<lower=0> N; // n observations 
  real y[N]; // observed 
  real y_last[N]; // last observed
  real clim1[N]; // climate observed
}
parameters {
  real<lower=0, upper = 10> sigma;
  real beta;
  real alpha; 
  real theta;
}
transformed parameters {
  vector[N] mu; 
  for( i in 1:N)
  mu[i] <- alpha + beta*y_last[i] + theta*clim1[i];
}
model {
  //priors
  beta ~ normal(1, 10);
  alpha ~ normal(0, 10);
  theta ~ normal(0, 10);
  //likelihood 
  y ~ normal(mu, sigma);
}