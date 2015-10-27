rm(list = ls() ) 

source('R/functions.R')

#### parameters 

obsTime = runif( 4, min = 2, max = 10 ) 
burnTime = 100 
time = obsTime + burnTime
pop_init = 100  
B = 0.9
A = 0
C2 = 0.5 # climate effect second year of transition 
C1 = 2 # climate effect first year of transition  
EV = 1

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
  facet_grid(site ~ .) +
  scale_x_continuous( breaks = c(unique(results_df$year))) 
  
