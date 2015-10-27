##### density dependent pop model with environment 


logistic_growth = function( N, E, ... ){
  #### discrete logistic growth 
  #### N is current population size 
  #### E is current environmental state 
  #### output is population size at next time step
  
  K_step = conditions( K, E )
  N_new = N + r*N*(1 - N/K_step)
  
  if(N_new < 1 ){ #### extinction 
    N_new <- 0 
  }
  
  return(N_new)
}

conditions = function( K, E, ... ){ 
  #### determines short-term climate effect
  #### K is baseline carrying capacity 
  #### E is environmental state
  #### return K_step, short-term carrying capacity 
  
  K_step = K + (K/4)*E
  return(K_step)  
}

t = 1000
E = rnorm( t, 0, 1) ### environment 
E = E/max(abs(E))  #### normalize to -1 to 1 

N = rep(NA, t)
K = 100 
r = 0.9

N[1] = K 

for(i in 2:length(N)){ 
  print( conditions(K=K, E=E[i]))
  N[ i ] = logistic_growth(N[i-1], E = E[i], K = K, r = r)
}

plot( 1:t, N, type = 'l', xlim = c(100, 200))

popData = data.frame( N, E)
popData$Nlast = c(NA, N[-length(N)]) 
popData$Elast = c(NA, E[-length(E)])

head(popData)

m1 = lm( log(N) ~ 0 + log(Nlast) + E + Elast, popData)
summary(m1)


