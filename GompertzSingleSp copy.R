rm(list = ls() ) 
library(rjags)
setwd('~/Documents/Kleinhesselink/MAR_models/')
#### Gompertz Population Dynamics single species 

logGompertz = function( X, A, B, ER, C, U) { 
  X = as.vector(X, mode= 'numeric')
  U = as.vector(U)
  X2 = A + B%*%X + C%*%U + ER   
  return(X2)
}

#### parameters 
time = 10
clim = rnorm(time, 2, 1)
pop = rep(NA, time)
B = 0.95
A = 0
C = 0.2
EV = 0.1

##### Data list 
N = pop
pop[1] = 1000
N[1] = log(pop[1])

##### Simulate data 
for( i in 2:time) { 
  E = rnorm(1, 0, EV)
  N[i] = logGompertz(X = N[i-1], A = A, B = B, C = C, U = clim[i-1], ER = E)
}

par(mfrow = c(1,1))
plot(clim, type = 'l', ylim = c(0, max(N)))
points(N, type = 'l', col= 'red')

###### population 
Nlast = N[ -length(N)]
Nnow = N[ - 1 ]

#######
climLast = clim[ -length(clim) ]

##### simple linear model 
##### estimate parameters 
simpMod = lm( Nnow ~ 0 + Nlast + climLast)
summary(simpMod)


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

