rm(list = ls() ) 
library(rjags)

#### Gompertz Population Dynamics single species 
logGompertz = function( X, A, B, ER, C, U) { 
  #### X is species current abundance 
  #### U is external environment 
  #### X2 is species abundance in next time step 
  #### A is the intrinsic growth rate 
  #### B determines density dependence 
  #### ER is an observation error 
  #### C is the effect of U on population growth
  
  X = as.vector(X, mode= 'numeric')
  U = as.vector(U)
  X2 = A + B%*%X + C%*%U + ER   
  return(X2)
}

#### parameters 
time = 10000
clim = rnorm(time, 2, 1)
climLag = c(NA, clim[ -length(clim) ] )

pop = rep(NA, time + 1)
B = 0.5
A = 0
C = 0.2
EV = 0.1

##### population data  
N = pop
pop[1] = 1000
N[1] = log(pop[1])

##### data frame for analysis 
popDF = data.frame( climate = c(clim, NA), population = N) 

##### Simulate data 
for( i in 1:time) { 
  E = rnorm(1, 0, EV)
  popDF$population[i+1] = logGompertz(X = popDF$population[i], A = A, B = B, C = C, U = popDF$climate[i], ER = E)
}

popDF = data.frame( popDF, climLag = c(climLag, NA), popLag = c(NA, popDF$population[ -length(popDF$population)]), climLag2 = c(NA, NA, popDF$climate[ 1:(time-1)]))

##### Plot time series 
stopifnot( time > 200 )

par(mfrow = c(1,1))
plot(popDF$climate, type = 'l', ylim = c(0, max(popDF$population)), xlim = c(100,200))
points(popDF$population, type = 'l', col= 'red')
points(popDF$climLag, type = 'l', col = 'blue')
points(popDF$climLag2, type = 'l', col = 'orange')

##### simple linear model 
##### estimate parameters 
popDF = popDF[ -c(1:100), ] #### drop first 100 time steps to remove transient effects

simpMod = lm( population ~ 0 + popLag + climLag, data = popDF)
summary(simpMod)

simpMod.Lag = lm(population ~ 0 + popLag + climLag + climLag2, data = popDF)
summary(simpMod.Lag)




##### RJAGS model 
### data list 
popData = list( y = Nnow, popLast = Nlast, clim = climLast, N = length(Nnow))

#### jags model 
m1 = jags.model( 'simpleGompertz.txt', data = popData, n.chains= 2)

## run the model, burnin then samples
update(m1,n.iter=1000)

out <-coda.samples(model=m1,variable.names=c("B", "C", "tau"), n.iter=500000, thin = 20)

plot(out)
summary(out)
effectiveSize(out)

