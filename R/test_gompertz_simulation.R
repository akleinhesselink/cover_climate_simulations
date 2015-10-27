rm(list = ls() ) 

source('R/functions.R')

#### Gompertz Population Dynamics single species 

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

EV = 1

##### population data  
N = pop
pop[1] = 100
N[1] = log(pop[1])

##### data frame for analysis 
popDF = data.frame( clim2 = c(clim2, NA), population = N, clim1 = c(clim1, NA)) 


##### Simulate data 

popDF <- sim_time_series(popDF, EV, time )

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

