#### for annual plant model 

popGrowth = function( N, climate, ... ) { 

  #### N is last seasons seed production 
  #### climate is current climate conditions 
  #### return next seasons seed bank 
  
  fecundity = maxF - climate*(maxF/2)  
  competition = g*N*gamma
  N_new = (g*fecundity*N)/competition - s*(1 - g)*N

}

t = 1000
N = rep(NA, t)
N[1] = 100
g = 1 #### 100% germination 
s = 0 #### 0% survival 
maxF = 100
climate = rnorm(t, 0, 1) 
climate = abs(climate/max(abs(climate))) ### normalize 0 to 1 

gamma = 0.5 #### intraspecific competition coefficient 

for ( i in 2:t ){ 
  N[i] = popGrowth(N[i-1], climate[i], maxF = maxF, gamma = gamma, g = g, s = s)
}

plot(N, type ='l', xlim = c(100, 200))

popDF = data.frame( N, Nlag = c(NA, N[ - length(N)]), climate, climateLag = c(NA, climate[ -length(climate) ] ))
head(popDF)

m1 = lm( log(N) ~ 0 + log(Nlag) + climate, data = popDF)
summary(m1)

m2 = lm( log(N) ~ 0 + log(Nlag) + climate + climateLag, data = popDF )
summary(m2)

plot(climate ~ climateLag, popDF)
plot( N  ~ Nlag, popDF)
