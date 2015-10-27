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


sim_time_series <- function(popDF, EV, time ){ 
  
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
