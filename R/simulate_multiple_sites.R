rm(list = ls() ) 

source('R/functions.R')

# simulate 2 sites 

#### parameters 
obsTime = list(10,10)
burnTime = 100
time = lapply( obsTime, FUN = function( x, burnTime) { x + burnTime }, burnTime )

pop = lapply(time, function (t) { rep(NA, t + 1) } )

pop_init = 100  

B = 0.9
A = 0
C2 = 0.5 # climate effect second year of transition 
C1 = 2 # climate effect first year of transition  
EV = 1

##### population data  
pop <- lapply( pop , function (x, pop_init ) { x[ 1 ] = pop_init; return (x) }, pop_init )
N <- lapply( pop, log )

# simulate climate 
clim2 = lapply( time, function ( t ) { rnorm(t, 2, 1) })
clim1 <- lapply( clim2, FUN = function(x) { c(NA, x [ - length(x )] ) }  )

length(clim2[[1]])
length(N[[1]])

climate <- mapply(FUN = function(x, y) { data.frame(clim2 = c(x, NA) , clim1 = c(y, NA) ) }, x = clim2, y = clim1, SIMPLIFY = FALSE)

##### data frame for analysis 
empty = mapply( FUN = function(x, y ) { data.frame (cbind( population = x,  y)) }, x = N, y = climate , SIMPLIFY = FALSE) 
head( empty[[1]] ) 

##### Simulate data 
pop_series <- mapply(FUN = sim_time_series, empty, EV, time , SIMPLIFY = FALSE)

popDF <- lapply( X = pop_series, make_pop_df) 


##### Plot time series 
ggplot(subset(popDF, year > burnTime), aes(x = year, y = population, group = 1 ) ) + geom_point() + geom_line() + scale_x_continuous(breaks = c(burnTime:time))

