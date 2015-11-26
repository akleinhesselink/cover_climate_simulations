data {
  int<lower=0> N; // n observations 
  real y[N]; // observed 
  real y_last[N]; // last observed
  int<lower=0> nplot; // n plots 
  int<lower=0> plot[N]; // unique plot 
}
parameters {
  real beta;
  real alpha; 
  vector[nplot] B;
  real<lower=0, upper = 1000> sigma;
  real<lower=0, upper = 1000> nu; 
}
transformed parameters {
  vector[N] mu;
  
  for( i in 1:N)
  mu[i] <- alpha + B[plot[i]]*y_last[i]; 
}
model {
  //priors
  beta ~ normal(0, 10);
  alpha ~ normal(0, 10);
  B ~ normal( beta , nu );
  
  //likelihood 
  y ~ normal(mu, sigma);
}