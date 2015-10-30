rm(list = ls() ) 

library(ggplot2)
library(lme4)

source('R/functions.R')

#### parameters 

obsTime = runif( 100, min = 2, max = 3 ) 
burnTime = 100 
time = obsTime + burnTime
pop_init = 100  
B = 0.8
A = 0.4
C2 = -0.01 # climate effect second year of transition 
C1 = 0.02 # climate effect first year of transition  
EV = 0.1
mean_clim <- 2 
var_clim <- 1

nsites = max( sapply(list(burnTime, time, pop_init), function (x) {  length ( x )  } ) ) 

parms_df <- data.frame( site = factor(1:nsites), burnTime = burnTime, time = time, pop_init = pop_init, B = B, A = A, C2 = C2, C1 = C1, EV = EV )

parms_list <- split(parms_df, f = 1:nrow(parms_df))

# run simulation 
results <- lapply( parms_list,  run_simulation ) 

results_df <- do.call( rbind, results ) 

##### Plot time series 
ggplot(results_df, aes(x = as.numeric(year), y = population, color = site ) ) + 
  geom_point() + 
  geom_line() + 
  scale_x_continuous( breaks = c(unique(results_df$year))) 

ggplot( results_df, aes(x = popLag, y = population, color = site) ) + 
  geom_point() + geom_line()

head( results_df ) 

m1 <- lmer(data = results_df, population ~ popLag + clim2 + clim1 + (1|site ))  

summary(m1)
