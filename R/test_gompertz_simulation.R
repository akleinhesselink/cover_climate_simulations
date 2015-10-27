rm(list = ls() ) 

source('R/functions.R')

#### Gompertz Population Dynamics single species 

#### parameters 
obsTime = 10
burnTime = 100
time = obsTime + burnTime

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

# simulate climate 
clim2 = rnorm(time, 2, 1) 
clim1 = c(NA, clim2[ -length(clim2) ] )
climate <- cbind(clim2 = c(clim2, NA), clim1 = c(clim1, NA))

##### data frame for analysis 
empty = data.frame( population = N, climate) 

##### Simulate data 
pop_series <- sim_time_series(empty, EV, time )

popDF <- make_pop_df( pop_series = pop_series)
##### Plot time series 
ggplot(subset(popDF, year > burnTime), aes(x = year, y = population, group = 1 ) ) + geom_point() + geom_line() + scale_x_continuous(breaks = c(burnTime:time))


par(mfrow = c(1,1))
plot(popDF$clim2, type = 'l', ylim = c(0, max(popDF$population, na.rm=TRUE)), xlim = c(100,time))
points(popDF$population, type = 'l', col= 'red')
points(popDF$clim1, type = 'l', col = 'blue')



##### simple linear model 
##### estimate parameters 
popDF = popDF[ -c(1:burnTime), ] #### drop burn in time time steps to remove transient effects

m1 = lm( population ~ 0 + popLag + clim2 + clim1, data = popDF)
summary(m1)

m2 = lm( population ~ 0 + popLag + clim1, data = popDF)
summary(m2)

