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


sim_time_series <- function(popDF, E, time, X, A, B, C1, C2, C3, C4, M1, E_obs ){ 
  
  for( i in 2:time) { 
    Z = rnorm(1, 0, E)
    popDF$population[i] = logGompertz(X = popDF$population[i-1], 
                                      A = A, 
                                      B = B, 
                                      C2 = C2, 
                                      C1 = C1, 
                                      U2 = popDF$clim[i], 
                                      U1 = popDF$clim_lag[i], 
                                      E = Z)
  }
  OE <- rnorm(nrow( popDF), 0, E_obs)
  popDF$population_obs <- popDF$population + OE
  return( popDF)
}


run_simulation <- function( burnTime = burnTime, time = time, pop_init = pop_init, A = A, B = B, C1 = C1, C2 = C2, E = E, E_obs = E_obs, ... ) { 
  pop = rep(NA, time + 1)
  
  ##### population data  
  N <- initialize_population( pop, pop_init )
  climate <- sim_climate ( time, var_clim )
  
  ##### data frame for analysis 
  empty = data.frame( population = N, climate) 
  
  ##### Simulate data 
  pop_series <- sim_time_series(empty, E = E, time = time, A = A, B = B, C2 = C2, C1 = C1, E_obs = E_obs)

  pop_DF <- make_pop_df( pop_series = pop_series )
  
  pop_DF <- pop_DF[ -c(1:burnTime), ]
  return(pop_DF)
}


model_effects <- function( df ){ 

  m1 <- coef(summary(lm(data = df, population ~ popLag + clim + clim_lag))) # without observation error 
  
  m1_OE <- coef(summary(lm(data = df, population_obs ~ popLag_obs + clim + clim_lag))) # with observation error 
  
  npars <- nrow(m1)  
  pars <- row.names(m1)
  m1 <-  data.frame( OE = FALSE, pars = pars, m1  )
  m1_OE <- data.frame( OE = TRUE, pars = pars, m1_OE)
  return( rbind( m1, m1_OE))
} 


#### parameters ----------------------------------
obsTime = 10000
burnTime = 100

time = obsTime + burnTime
pop_init = 100 

A = 0.8 # gompertz intercept
B = 0.5 # gompertz slope 

#################################################
### These are the critical factors: 

C1 =  0 ############## climate lag effect
C2 = -1 ############### climate effect current year  


#################################################

var_clim <- 1 ### annual climate variation 

E <- 0.4 ################ "proccess" error 
OE_list <- seq(0, 1, length.out = 10) ######## observation error 

# run several simulations at various observation error levels 
# keep results in a list and then plot 

test_df_list  <- lapply(X = OE_list , FUN = function(x ) { run_simulation( burnTime, time, pop_init, A, B, C1 = C1, C2 = C2, E, E_obs = x) } )  

results_list <- lapply ( test_df_list , model_effects )  

results_list[[1]]

set_values = c(A, B, C2, C1)

for( i in 1:length(OE_list)) { 
  results_list[[i]]$set_values <- set_values 
  results_list[[i]]$E_obs <- OE_list[i] 
}

results <- do.call( rbind, results_list) 
results$`with observation error` <- results$OE
results$`observation error` <- results$E_obs
results$parameter <- results$pars
results$parameter_labels <- factor( results$parameter , labels = c('clim','clim_lag',  'Gompertz "a"', 'Gompertz "b"'))

result_plot <- ggplot(results, aes( x = `observation error`, y = Estimate, color = `with observation error`, ymax = Estimate + Std..Error, ymin = Estimate - Std..Error   ) ) + 
  geom_hline( aes( yintercept = set_values)) + 
  geom_point() + 
  geom_errorbar() + 
  scale_color_discrete( ) + 
  facet_wrap(~ parameter_labels, nrow = 2 ) + 
  ylim( c(-1.5, 1.5))

result_plot

png( 'figs/gompertz_lag_effects.png', width = 6, height = 6, units = 'in', res = 300)

print(result_plot) 

dev.off()
  
######### plot example time series -------------------------------------- 

for( i in 1:length(OE_list)) { 
  test_df_list[[i]]$`observation_error` <- paste0 ( 'Obs. error = ', round ( OE_list[i], 2 ) ) 
} 

all_test_df <- do.call( rbind, test_df_list ) 

example_ts <-reshape( all_test_df, varying = c('population' , 'population_obs'), v.names = 'pop_size', timevar = 'type', times = c('actual', 'observed'), direction = 'long') 

ggplot( subset(example_ts, year < 150), aes( x = year, y = pop_size , color = type ) ) + 
  geom_point() + 
  geom_line() + 
  facet_wrap(~ observation_error, nrow = 5) 

