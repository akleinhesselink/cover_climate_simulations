library(rstan)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

source('temp/sim_data.R')

simple_data <- list( y = dat_list$y, y_last = dat_list$y_last, N = dat_list$N)

simple_fit <- stan( file = 'R/sim_data.stan', data = simple_data, 
                    model_name = 'simple_gompertz', 
      iter = 5000, warmup = 1000, chains = 4, thin = 3,cores = 4  )

plot( simple_fit , pars = c('beta')) 

traceplot(simple_fit, pars = c('sigma', 'beta', 'alpha'))

print(simple_fit, pars = c('alpha', 'beta'))

pairs(simple_fit, pars = c('alpha', 'beta'))

# clim effects --------------------------- 

dat_list_clim <- list( y = dat_list$y, y_last = dat_list$y_last, N = dat_list$N, clim1 = dat_list$clim1)
str(dat_list_clim)

simple_fit_clim <- stan( file = 'R/sim_data_climfx.stan', data = dat_list_clim, 
                    model_name = 'simple_gompertz_with_clim', 
                    iter = 5000, warmup = 100, chains = 4, thin = 3,cores = 4  )

plot( simple_fit_clim , pars = c('theta')) 

traceplot(simple_fit_clim, pars = c('beta', 'alpha'))

print(simple_fit_clim, pars = c('alpha', 'beta', 'theta'))

pairs( simple_fit_clim, pars = c('alpha', 'beta','theta'))

# hierarchical ---------------------------
str(dat_list)

dat_list_h <- list( y = dat_list$y, y_last = dat_list$y_last, plot = dat_list$plot, nplot = dat_list$nplot, N = dat_list$N, clim1 = dat_list$clim1, clim2 = dat_list$clim2)

str( dat_list_h ) 

h_fit <- stan( file = 'R/sim_data_hier.stan', data = dat_list, 
                    model_name = 'hier_gompertz', 
                    iter = 5000, warmup = 1000, chains = 4, thin = 3, cores = 4 )

plot( h_fit, pars = c('theta1', 'theta2') ) 
traceplot(h_fit, pars = c('beta', 'alpha'))
traceplot(h_fit, pars = c('theta1', 'theta2'))

pairs( h_fit, pars = c('alpha', 'beta', 'theta1', 'theta2'))

summary(h_fit, pars = c('alpha', 'beta', 'theta1', 'theta2'))

# hierarchical dd with hierarchical variance model --------------------
str(dat_list)

dat_list_h <- list( y = dat_list$y, y_last = dat_list$y_last, plot = dat_list$plot, nplot = dat_list$nplot, N = dat_list$N, clim1 = dat_list$clim1, clim2 = dat_list$clim2)

str( dat_list_h ) 

h_fit <- stan( file = 'R/sim_data_hier.stan', data = dat_list, 
               model_name = 'hier_gompertz', 
               iter = 5000, warmup = 1000, chains = 4, thin = 3, cores = 4 )

plot( h_fit, pars = c('theta1', 'theta2') ) 
traceplot(h_fit, pars = c('beta', 'alpha'))
traceplot(h_fit, pars = c('theta1', 'theta2'))

pairs( h_fit, pars = c('alpha', 'beta', 'theta1', 'theta2'))

summary(h_fit, pars = c('alpha', 'beta', 'theta1', 'theta2'))
