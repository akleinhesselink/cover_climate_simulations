rm(list = ls() ) 
#### Gompertz Population Dynamics single species 
library(lme4)
library(ggplot2)
library(mvtnorm)
library(dplyr) # need for "lag" function 
source('R/functions.R')

# functions -----------------------------------------------------------------

logGompertz = function( X, A, B, E, C, U) { 
  ##### X is log species abundance
  ##### U is a vector of climate covariates 
  ##### C is a vector of climate effects 
  ##### A is intrinsic growth rate
  ##### B is density dependence 
  ##### E is error 
  
  X = as.numeric(X)
  C = as.numeric(C)
  U = as.numeric(U)
  X2 = A + B%*%X + C%*%U + E 
  return(X2)

} 


initialize_population <- function(pop_init, time){ 
  
  pop = rep(NA, time)
  pop[1:nlags] = pop_init
  N = log(pop)
  
  return(N)
  
}


sim_climate <- function( time, var_clim, nlags ) { 
  
  U <- data.frame( V1 = rnorm( time + 1, 0, var_clim ) )

  for( i in 1:nlags) { 
    U[, i+1] <- lag(U[,1], i) 
  }
  
  return( U ) 
} 

sim_time_series <- function(N, sigma_p, time, A, B, C, U, nlags){ 
  
  for( i in (nlags + 1):time) { 
    E = rnorm(1, 0, sigma_p)
    N[i] = logGompertz(X = N[i-1], 
                       A = A, 
                       B = B, 
                       C = C,
                       U = U[i, ],
                       E = E)
  }
  return( N ) 
}

make_pop_df <- function(time, N, U, sigma_o ) { 
  OE <- rnorm( length(N), 0, sigma_o ) 
  
  pop_df = data.frame(
    year = 1:time, 
    U = U[-nrow(U), ],
    N = N, 
    OE = OE 
  )
  return( pop_df )
}

run_simulation <- function( burnTime = burnTime, time = time, pop_init = pop_init, A = A, B = B, C = C, sigma_p = sigma_p, sigma_o = sigma_o, var_clim= var_clim, nlags =1 ) { 

  ##### population data  
  N <- initialize_population( time, pop_init )
  U <- sim_climate ( time, var_clim, nlags )
  
  ##### Simulate data 
  N <- sim_time_series(N = N, sigma_p = sigma_p, time = time, A = A, B = B, C = C, U = U, nlags = nlags)
  
  pop_DF <- make_pop_df(time = time, N = N, U = U, sigma_o = sigma_o)
  
  pop_DF <- pop_DF[ -c(1:burnTime), ]
  return( pop_DF)
}


model_formula <- function( nlags ) { 
  
  f <- list()
  f[[1]] <-  as.formula( paste('N ~ N_lag +', paste0('U.V', 1:(nlags+1), collapse = ' + '))) 
  f[[2]] <-  as.formula( paste('N_obs ~ N_obs_lag +', paste0('U.V', 1:(nlags+1), collapse = ' + ')))

  return(f)
}


model_effects <- function( df, nlags ){ 
  
  f <-  model_formula(nlags = nlags )

  m1 <- coef(summary(lm(data = df, formula = f[[1]]))) # without observation error 
  
  m1_OE <- coef(summary(lm(data = df, formula = f[[2]]))) # with observation error 
  
  npars <- nrow(m1)  
  pars <- row.names(m1)
  m1 <-  data.frame( Obs_E = FALSE, pars = pars, m1  )
  m1_OE <- data.frame( Obs_E = TRUE, pars = pars, m1_OE)
  return( rbind( m1, m1_OE))
} 

add_observation_error <- function(df, sigma_o_factor) { 
  
  df$sigma_o_factor <- sigma_o_factor  
  df$N_obs <- df$N + df$OE*df$sigma_o_factor

  return(df)
} 
  
lag_population <- function(df ) { 
  
  df$N_lag <- lag( df$N, 1) 
  df$N_obs_lag <- lag(df$N_obs, 1)
  
  return(df)
}

make_par_labs <- function( x ) { 
  l <- levels(x)
  l[ 1:2 ] <- c('a', 'b')
  lag_labels <- paste('lag', 0:(length(l)-3 ))
  l[ -c(1:2) ] <- lag_labels
  return(l)
}


##########################################################
############## SET PARAMETERS ############################ 

obsTime = 10000
burnTime = 100

time = obsTime + burnTime
pop_init = 100 

A = 1 # gompertz intercept
B = 0.5 # gompertz slope 

### These are the critical factors: ########
C = c(-0.5, 0, 0)  #### climate effects [ lag 0 , lag 1, lag 2, ... etc. ]

nlags <- length(C) - 1 
############################################

var_clim <- 1   ## annual climate variation 

sigma_p <- 0.8  ## "proccess" error 
sigma_o <- 1    ## base observation error                               
sigma_o_factor <- seq(0, 4, length.out = 20) ## observation error above is multiplied by this increasing factor 

################# RUN SIMULATIONS ###################################

# run one simulation  
# add simulated observation error with each level of observation error 
# a seperate dataframe in a list 

df <- run_simulation(burnTime = burnTime, time = time, pop_init = pop_init, A = A, B = B, C = C, sigma_p = sigma_p, sigma_o = sigma_o, var_clim = var_clim, nlags = nlags)

test_df_list <- lapply( X = sigma_o_factor, function( x ) { add_observation_error( df, sigma_o_factor = x) } )

test_df_list <- lapply( X = test_df_list, lag_population)

head( test_df_list[[4]] )



results_list <- lapply ( test_df_list , function(x, nlags ) model_effects ( x, nlags = nlags), nlags = nlags)  

results_list[[1]]

set_values = c(A, B, C)

for( i in 1:length(sigma_o_factor)) { 
  results_list[[i]]$set_values <- set_values 
  results_list[[i]]$sigma_o_factor <- sigma_o_factor[i] 
}

results <- do.call( rbind, results_list) 
results$bias = results$Estimate - results$set_values

######## change labels just for plotting ###########
results$`with observation error` <- results$Obs_E
results$`observation error factor` <- results$sigma_o_factor
results$parameter <- results$pars
results$parameter_labels <- factor( results$parameter, labels = make_par_labs ( results$parameter) )

############## main results figure ###################################

result_plot <- ggplot(results, aes( x = `observation error factor`, y = Estimate, color = `with observation error`, ymax = Estimate + Std..Error, ymin = Estimate - Std..Error   ) ) + 
  geom_hline( aes( yintercept = set_values)) + 
  geom_point() + 
  geom_errorbar() + 
  facet_wrap(~ parameter_labels, nrow = nlags + 1) 

bias_plot <- ggplot(results, aes( x = `observation error factor`, y = bias, color = `with observation error`, ymax = bias + Std..Error, ymin = bias - Std..Error)) + 
  geom_errorbar() + 
  geom_point() + 
  facet_wrap( ~ parameter_labels, nrow = nlags + 1)

result_plot 

bias_plot 

png( 'figs/gompertz_lag_effects_parameter_estimates.png', width = 6, height = 6, units = 'in', res = 300)
print(result_plot) 
dev.off()

png( 'figs/gompertz_lag_effects_bias.png', width = 6, height = 6, units = 'in', res = 300 ) 
print(bias_plot)
dev.off()

  
######### plot example time series -------------------------------------- 

all_test_df <- do.call( rbind, test_df_list ) 

all_test_df$observation_error <- paste0('obs. error factor = ', round( all_test_df$sigma_o_factor, 2) )

example_ts <-reshape( all_test_df, varying = c('N' , 'N_obs'), v.names = 'N', timevar = 'type', times = c('actual', 'observed'), direction = 'long') 

ggplot( subset(example_ts, year < 150), aes( x = year, y = N , color = type ) ) + 
  geom_point() + 
  geom_line() + 
  facet_wrap(~ observation_error, nrow = 5) 

# plot example climate time series -----------------------------------

U <- all_test_df[ , grep(pattern = 'U', names( all_test_df))]

U <- unique(U )
U$year <- unique( all_test_df$year) 

U <- reshape( U, varying = list(1:(length(U)-1)), v.names = 'climate', timevar = 'lag', times = paste0('lag_', 0:(length(U)-2)), direction = 'long')
ggplot( subset( U, year < 150 ), aes( x = year, y = climate, color = lag)) + geom_point() + geom_line()

