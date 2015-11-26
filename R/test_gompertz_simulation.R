rm(list = ls() ) 
#### Gompertz Population Dynamics single species 
library(lme4)
library(ggplot2)
source('R/functions.R')

#### parameters 

nsites = 8
obsTime = rep(10, nsites)
burnTime = 100
time = obsTime + burnTime
pop_init = 100 
B = rnorm(n = 1, mean = 0.8, sd = 0.1)
A = 0.2
C2 = 0.1 # climate effect second year of transition 
C1 = 0.1 # climate effect first year of transition  
mean_clim <- 0 
var_clim <- 1
EV = 1

0.05*2

####

nsites = max( sapply(list(burnTime, time, pop_init), function (x) {  length ( x )  } ) ) 

parms_df <- data.frame( site = factor(1:nsites), burnTime = burnTime, time = time, pop_init = pop_init, B = B, A = A, C2 = C2, C1 = C1, EV = EV )
parms_df

parms_list <- split(parms_df, f = 1:nrow(parms_df))
parms_list
rm(B)
#### 
run_simulation(parms_list[[1]])

results <- lapply(parms_list, FUN = run_simulation) 

results <- do.call(rbind, results)
results <- results[ !is.na(results$population), ]


dat_list = list(  
                     y = results$population, 
                     y_last = results$popLag, 
                     plot = as.numeric(results$site),
                     nplot = nlevels(factor(results$site)), 
                     N = nrow(results)
                     )

dump(list = "dat_list", file = 'temp/sim_data.R' )

##### Plot time series 

library(ggplot2)
ggplot( data = results, aes( x = as.integer(year), y = population, color = site)) + 
  facet_wrap(~ site, ncol = 1  ) +
  scale_x_continuous() + 
  geom_point() + 
  geom_path() 

##### simple linear model 
##### estimate parameters 

m1 = lmer( population ~  popLag + (popLag|site), data = results)
summary(m1)

