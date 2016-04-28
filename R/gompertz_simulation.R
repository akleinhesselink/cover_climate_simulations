rm(list = ls() ) 
#### Gompertz Population Dynamics single species 
library(lme4)
library(ggplot2)
library(mvtnorm)
source('R/functions.R')

# functions -----------------------------------------------------------------

logGompertz = function( X, A, B, E, C1, C2, U1, U2) { 
  #### X is species current log abundance 
  #### U1 is lag climate 
  #### U2 is current climate 
  #### X2 is species log abundance in next time step 
  #### A is the intrinsic growth rate 
  #### B determines density dependence 
  #### C[1,2] is the effect of U[1,2] on pop growth
  
  X = as.vector(X, mode= 'numeric')
  U1 = as.vector(U1)
  U2 = as.vector(U2)
  X2 = A + B%*%X + C2%*%U2 + C1%*%U1 + E 
  return(X2)
}


make_pop_df <- function(pop_series ) { 
  pop_df = data.frame(
    year = as.numeric(row.names(pop_series)), 
    pop_series, 
    popLag = c(NA, pop_series$population[ -length(pop_series$population)]), 
    popLag_obs = c(NA, pop_series$population_obs[ -length(pop_series$population_obs)])
  )
  return( pop_df )
}

initialize_population <- function(pop, pop_init, ... ){ 
  pop[1] = pop_init
  N = log(pop)
  return(N)
}


sim_climate <- function( time, var_clim, ... ){ 
  clim = rnorm(time, 0, var_clim) 
  clim_lag = c(NA, clim[ -length(clim) ] )
  climate <- cbind(clim = c(clim, NA), clim_lag = c(clim_lag, NA))
  return(climate)
}


sim_time_series <- function(popDF, E1, time, X, A, B, C1, C2, C3, C4, M1 ){ 
  
  for( i in 2:time) { 
    E = rnorm(1, 0, E1)
    popDF$population[i] = logGompertz(X = popDF$population[i-1], 
                                      A = A, 
                                      B = B, 
                                      C2 = C2, 
                                      C1 = C1, 
                                      U2 = popDF$clim[i], 
                                      U1 = popDF$clim_lag[i], 
                                      E = E)
  }
  OE <- rnorm(nrow( popDF), 0, E_obs)
  popDF$population_obs <- popDF$population + OE
  return( popDF)
}


run_simulation <- function( burnTime, time, pop_init, A, B, C1, C2, E, E_obs, ... ) { 
  pop = rep(NA, time + 1)
  
  ##### population data  
  N <- initialize_population( pop, pop_init )
  climate <- sim_climate ( time, var_clim )
  
  ##### data frame for analysis 
  empty = data.frame( population = N, climate) 
  
  ##### Simulate data 
  pop_series <- sim_time_series(empty, E = E, time = time, A = A, B = B, C2 = C2, C1 = C1)

  pop_DF <- make_pop_df( pop_series = pop_series )
  
  OE <- rnorm(nrow(pop_DF), 0, E_obs )
  
  pop_DF <- pop_DF[ -c(1:burnTime), ]
  return(pop_DF)
}


model_effects <- function( df ){ 
  m1 <- coef(summary(lm(data = df, population ~ popLag + clim_lag + clim)))
  m1_OE <- coef(summary(lm(data = df, population_obs ~ popLag_obs + clim_lag + clim)))
  npars <- nrow(m1)  
  pars <- row.names(m1)
  m1 <-  data.frame( OE = FALSE, pars = pars, m1  )
  m1_OE <- data.frame( OE = TRUE, pars = pars, m1_OE)
  return( rbind( m1, m1_OE))
} 


#### parameters 
obsTime = 1000
burnTime = 100

time = obsTime + burnTime
pop_init = 100 

A = 0.8
B = 0.5

C2 = -1 # climate effect current year  
C1 = 0 # climate lag effect

var_clim <- 1

E <- 0.4 # proccess error 
E_obs <- seq(0.01, 1, length.out = 10) # observation error 

# run several simulations at various observation error levels 
# keep results in a list and then plot 

test_df_list  <- lapply(E_obs , FUN = function(x ) { run_simulation( burnTime, time, pop_init, A, B, C1, C2, E, E_obs = x) } )  

results_list <- lapply ( test_df_list , model_effects ) 

set_values = c(A, B, C1, C2)

for( i in 1:length(E_obs)) { 
  results_list[[i]]$set_values <- set_values 
  results_list[[i]]$E_obs <- E_obs[i] 
}

results <- do.call( rbind, results_list) 
results$`with observation error` <- results$OE
results$`observation error` <- results$E_obs
results$parameter <- results$pars

example_ts <- test_df_list[[length(E_obs)]][ 1:50 , ]
example_ts <- example_ts[ , c('year', 'population', 'population_obs' )]

example_ts <- reshape( example_ts, varying = c('population' , 'population_obs'), v.names = 'pop_size', timevar = 'type', times = c('actual', 'observed'), direction = 'long')


head( sim_climate(time = time, var_clim = var_clim ) ) 

ggplot( example_ts, aes( x = year, y = pop_size , color = type ) ) + 
  geom_point() + 
  geom_line() + 
  ggtitle(paste0( 'Actual vs. observed pop size with observation error =', round( E_obs[ length(E_obs) ] ) , 2) ) 


ggplot( subset( results, pars %in% c('clim', 'clim_lag') ), aes( x = `observation error`, y = Estimate, shape = parameter, color = `with observation error`, ymax = Estimate + Std..Error, ymin = Estimate - Std..Error   ) ) + 
  geom_hline( aes( yintercept = set_values)) + 
  geom_point() + 
  geom_errorbar() + 
  ylab( 'Effect estimate') + 
  scale_color_discrete( ) + 
  facet_wrap(~ parameter )


  