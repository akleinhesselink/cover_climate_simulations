##### density dependent pop model with environment 

t = 1000
E = rnorm( t, 0, 1) ### environment 
N = rep(NA, t)

logistic_growth = function( N, E, ... ){ 
  
  K_temp = conditions( K, E )
  N_new = N + r*N*(K_temp-N)/K_temp
  
  return(N_new)
}

conditions = function( K, E, ... ){ 
  K_temp = K - (K/20)*E
  return(K_temp)  
}

K_temp = conditions(K = 200, E = E)
plot(1:t, K_temp, type = 'l')

plot(E, K_temp)

K = 100 
r = 0.9

N[1] = K 

for(i in 2:length(N)){ 
  N[ i ] = logistic_growth(N[i-1], E = E[i], K = K, r = r)
}

plot( 1:t, N, type = 'l', ylim = c(0, 150))
popData = data.frame( N, E)
popData$Nlast = c(NA, N[-length(N)]) 
popData
popData$Elast = c(NA, E[-length(E)])

m1 = lm( log(N) ~ log(Nlast) + E + Elast, popData)

summary(m1)
