rm(list = ls() ) 
#### Gompertz Population Dynamics single species 
library(lme4)
library(ggplot2)
source('R/functions.R')

#### parameters 

ndatasets = 8
nsites = 600
obsTime = runif( n = nsites, min = 3, max = 15)
burnTime = 100
time = obsTime + burnTime
pop_init = 100 
B = rnorm(n = nsites, 0.89, sd =  0.005)
A = runif(n = nsites, min = 0.2, max = 0.2)
C2 = -0.1 # climate effect second year of transition 
C1 = 0.2 # climate effect first year of transition  
mean_clim <- 0 
var_clim <- 0.2
EV <- 0.2

m_B <- mean( B)
m_A <- mean( A)

hist( EV)

####

nsites = max( sapply(list(burnTime, time, pop_init), function (x) {  length ( x )  } ) ) 

parms_df <- data.frame( site = factor(1:nsites), burnTime = burnTime, time = time, pop_init = pop_init, B = B, A = A, C2 = C2, C1 = C1, EV = EV )
parms_df

parms_list <- split(parms_df, f = 1:nrow(parms_df))
parms_list
rm( B, A)
#### 
do.call( run_simulation, args = as.list ( parms_list[[1]]))

results <- lapply(parms_list, FUN = function(x) { do.call( run_simulation, args = as.list(x)) } ) 

results <- do.call(rbind, results)
results <- results[ !is.na(results$population), ]


dat_list = list(  
                     y = results$population, 
                     y_last = results$popLag, 
                     plot = as.numeric(results$site),
                     nplot = nlevels(factor(results$site)), 
                     N = nrow(results), 
                     clim1 = results$clim1,
                     clim2 = results$clim2
                     )

dump(list = "dat_list", file = 'temp/sim_data.R' )

##### Plot time series 

library(ggplot2)
log_plot <- ggplot( data = results, aes( x = as.integer(year), y = population, color = site)) + 
  facet_wrap(~ site, ncol = 2  ) +
  scale_x_continuous() + 
  geom_point() + 
  geom_path() 

plot <- log_plot

plot$data$population <- exp(log_plot$data$population)
plot$data$site

plot %+% subset( plot$data, site %in% c(1:10) ) 


##### simple linear model 
##### estimate parameters 

m1 = lmer( population ~  popLag + (popLag|site) + clim1 + clim2, data = results)
summary(m1)

m2 = lmer( population ~ popLag + (popLag-1|site) + clim1 + clim2, data = results)
summary(m2)

m_A
m_B
C1
C2
