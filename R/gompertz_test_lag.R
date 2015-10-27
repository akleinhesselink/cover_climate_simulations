rm(list = ls() ) 

#### Gompertz Population Dynamics single species 
logGompertz = function( X, A, B, ER, C, C2, U, U2) { 
  #### X is species current log abundance 
  #### U is external environment 
  #### X2 is species log abundance in next time step 
  #### A is the intrinsic growth rate 
  #### B determines density dependence 
  #### ER is an observation error 
  #### C is the effect of U on population growth
  
  X = as.vector(X, mode= 'numeric')
  U = as.vector(U)
  X2 = A + B%*%X + C%*%U + C2%*%U2 + ER   
  return(X2)
}

#### parameters 
time = 10000
clim = rnorm(time, 2, 1)
climLag = c(NA, clim[ -length(clim) ] )

pop = rep(NA, time + 1)
B = 0.9
A = 0
C = 2 ### climate effect 
C2 = 1 #### climate lag effect 

EV = 0.1

##### population data  
N = pop
pop[1] = 100
N[1] = log(pop[1])

##### data frame for analysis 
popDF = data.frame( climate = c(clim, NA), population = N, climLag = c(climLag, NA)) 


##### Simulate data 
for( i in 2:time) { 
  E = rnorm(1, 0, EV)
  popDF$population[i] = logGompertz(X = popDF$population[i-1], 
                                    A = A, 
                                    B = B, 
                                    C = C, 
                                    C2 = C2,  
                                    U = popDF$climate[i], 
                                    U2 = popDF$climLag[i], 
                                    ER = E)
}

popDF = data.frame( popDF, popLag = c(NA, popDF$population[ -length(popDF$population)]))
head( popDF ) 

##### Plot time series 
par(mfrow = c(1,1))
plot(popDF$climate, type = 'l', ylim = c(0, max(popDF$population, na.rm=TRUE)), xlim = c(100,200))
points(popDF$population, type = 'l', col= 'red')
points(popDF$climLag, type = 'l', col = 'blue')

##### simple linear model 
##### estimate parameters 
popDF = popDF[ -c(1:100), ] #### drop first 100 time steps to remove transient effects

m1 = lm( population ~ 0 + popLag + climate + climLag, data = popDF)
summary(m1)

m2 = lm( population ~ 0 + popLag + climLag, data = popDF)
summary(m2)


