rm(list = ls() ) 
#### Gompertz Population Dynamics single species 

source('R/functions.R')

#### parameters 
obsTime = 10
burnTime = 100
time = obsTime + burnTime
pop_init = 100 
B = 0.2
A = 0
C2 = 0.5 # climate effect second year of transition 
C1 = 2 # climate effect first year of transition  
mean_clim <- 2 
var_clim <- 1
EV = 1

####

nsites = max( sapply(list(burnTime, time, pop_init), function (x) {  length ( x )  } ) ) 

parms_df <- data.frame( site = factor(1:nsites), burnTime = burnTime, time = time, pop_init = pop_init, B = B, A = A, C2 = C2, C1 = C1, EV = EV )

parms_list <- split(parms_df, f = 1:nrow(parms_df))

#### 
results <- run_simulation( parms_list[[1]] )
results

##### Plot time series 
par(mfrow = c(1,1))
plot(results$clim2, type = 'l', ylim = c(0, max(results$population, na.rm=TRUE)))
points(results$population, type = 'l', col= 'red')
points(results$clim1, type = 'l', col = 'blue')

##### simple linear model 
##### estimate parameters 


m1 = lm( population ~  popLag + clim2 + clim1, data = results)
summary(m1)

