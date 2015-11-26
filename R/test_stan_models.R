library(rstan)

source('temp/sim_data.R')

str(dat_list)

simple_data <- list( y = dat_list$y, y_last = dat_list$y_last, N = dat_list$N)

simple_fit <- stan( file = 'R/sim_data.stan', data = simple_data, 
                    model_name = 'simple_gompertz', 
      iter = 1000, warmup = 100, chains = 4, thin = 3,cores = 4  )

plot( simple_fit , pars = c('sigma', 'beta', 'alpha')) 

traceplot(simple_fit)

# hierarchical ---------------------------
str(dat_list)

h_fit <- stan( file = 'R/sim_data_hier.stan', data = dat_list, 
                    model_name = 'hier_gompertz', 
                    iter = 1000, warmup = 100, chains = 4, thin = 3, cores = 4 )

plot( h_fit, pars = c('sigma', 'beta', 'alpha') ) 
