data {
  int<lower=0> N; // n observations 
  real y[N]; // observed 
  real y_last[N]; // last observed
  real clim1[N]; //observed climate
  real clim2[N]; 
  int<lower=0> nplot; // n plots 
  int<lower=0> plot[N]; // unique plot 
}
parameters {
  real beta;
  real alpha; 
  vector[nplot] B;
  real<lower=0, upper = 10> sigma;
  real<lower=0, upper = 10> nu; 
  real theta1;
  real theta2;
}
transformed parameters {
  vector[N] mu;
  
  for( i in 1:N)
  mu[i] <- alpha + B[plot[i]]*y_last[i] + theta1*clim1[i] + theta2*clim2[i]; 
}
model {
  //priors
  beta ~ normal(1, 10);
  alpha ~ normal(0, 10);
  theta1 ~ normal(0, 10);
  theta2 ~ normal(0, 10);
  B ~ normal( beta , nu );
  //likelihood 
  y ~ normal(mu, sigma);
}