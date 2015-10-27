rm(list = ls() ) 

#### Gompertz Population Dynamics single species 
logGompertz = function( X, A, B, ER, C2, C1, U2, U1) { 
  #### X is species current log abundance 
  #### U[j] is external environment factor j 
  #### X2 is species log abundance in next time step 
  #### A is the intrinsic growth rate 
  #### B determines density dependence 
  #### ER is an observation error 
  #### C is the effect of U on population growth
  
  X = as.vector(X, mode= 'numeric')
  U1 = as.vector(U1)
  U2 = as.vector(U2)
  X2 = A + B%*%X + C2%*%U2 + C1%*%U1 + ER   
  return(X2)
}

#### parameters 
obsTime = 10
burnTime = 100
time = obsTime + burnTime

clim2 = rnorm(time, 2, 1)
clim1 = c(NA, clim2[ -length(clim2) ] )

pop = rep(NA, time + 1)
B = 0.9
A = 0
C2 = 0.5 # climate effect second year of transition 
C1 = 2 # climate effect first year of transition  

EV = 0.1

##### population data  
N = pop
pop[1] = 100
N[1] = log(pop[1])

##### data frame for analysis 
popDF = data.frame( clim2 = c(clim2, NA), population = N, clim1 = c(clim1, NA)) 


##### Simulate data 

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

popDF = data.frame( popDF, popLag = c(NA, popDF$population[ -length(popDF$population)]))
head( popDF ) 

##### Plot time series 
par(mfrow = c(1,1))
plot(popDF$clim2, type = 'l', ylim = c(0, max(popDF$population, na.rm=TRUE)), xlim = c(100,200))
points(popDF$population, type = 'l', col= 'red')
points(popDF$clim1, type = 'l', col = 'blue')

##### simple linear model 
##### estimate parameters 
popDF = popDF[ -c(1:burnTime), ] #### drop burn in time time steps to remove transient effects

m1 = lm( population ~ 0 + popLag + clim2 + clim1, data = popDF)
summary(m1)

m2 = lm( population ~ 0 + popLag + clim1, data = popDF)
summary(m2)


