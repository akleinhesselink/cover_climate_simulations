

logGompertz = function( X, A, B, ER, C2, C1, U2, U1) { 
  #### X is species current log abundance 
  #### U[1,2] is external environment [at first and second year of transition] 
  #### X2 is species log abundance in next time step 
  #### A is the intrinsic growth rate 
  #### B determines density dependence 
  #### ER is an observation error 
  #### C[1,2] is the effect of U[1,2] on pop growth
  
  X = as.vector(X, mode= 'numeric')
  U1 = as.vector(U1)
  U2 = as.vector(U2)
  X2 = A + B%*%X + C2%*%U2 + C1%*%U1 + ER   
  return(X2)
}


sim_time_series <- function(popDF, EV, time, ... ){ 
  
  for( i in 2:time) { 
    E = rnorm(1, 0, EV)
    popDF$population[i] = logGompertz(X = popDF$population[i-1], 
                                      A = A, 
                                      B = B, 
                                      C2 = C2, 
                                      C1 = C1,  
                                      U2 = popDF$clim2[i], 
                                      U1 = popDF$clim1[i], 
                                      ER = E)
  }
  return( popDF)
}

make_pop_df <- function(pop_series) { 
  pop_df = data.frame(year = as.numeric(row.names(pop_series)), pop_series, popLag = c(NA, pop_series$population[ -length(pop_series$population)]))
  return( pop_df )
}

initialize_population <- function(pop, pop_init ){ 
  pop[1] = pop_init
  N = log(pop)
  return(N)
}

# simulate climate 
sim_climate <- function( time, mean_clim, var_clim){ 
  clim2 = rnorm(time, mean_clim, var_clim) 
  clim1 = c(NA, clim2[ -length(clim2) ] )
  climate <- cbind(clim2 = c(clim2, NA), clim1 = c(clim1, NA))
  return(climate)
}


run_simulation <- function( parms ) { 

  with(parms, { 
    
    pop = rep(NA, time + 1)

    ##### population data  
    
    N <- initialize_population( pop, pop_init)
    
    
    climate <- sim_climate ( time, mean_clim, var_clim)
    
    ##### data frame for analysis 
    empty = data.frame( population = N, climate) 
    
    ##### Simulate data 
    pop_series <- sim_time_series(empty, EV, time, A, B, C2, C1 )
    
    pop_DF <- make_pop_df( pop_series = pop_series)
    
    pop_DF$site = site 
    
    pop_DF <- pop_DF[ -c(1:burnTime), ]
    return(pop_DF)
  })
}

