
##### Test random effects approach to time series subplot agregation 
rm(list = ls())
library(ggplot2)
library(lme4)
#### simulate cover data at a site
#### there are 8 replicate subplots at the site
#### subplot cover varies over the course of 10 years
#### simulate multiple sites along a gradient 
#### need to build in density dependence or it doesn't make sense. 

###### ----------------------------------------------------------- 

findPGR = function( clim ){ 
  pgr = -0.5 + 0.15*clim - 0.005*clim^2 
  return(pgr)
}

#### climate to pgr curve
curve(-0.5 + 0.15*x - 0.005*x^2, from = 0.01, to = 30, 100)

##### simulate data 
subplot = factor(1:8)
year = 1:10

df =  expand.grid (subplot =  subplot,  year = year )
subplotMeans = c(8, 1, 2, 3, 20, 40, 60, 50)
site1 = data.frame(year = year, clim = rnorm(length(year), 5 , 2))
site1$clim[ site1$clim < 0] <- 0.1
site1

site1$pgr = findPGR(site1$clim)
site1

TruePGR = data.frame()

df$cover = NA
df$cover[ df$year == 1  ] = subplotMeans #### set initial cover values 
head(df, 10)

for (i in 2:max(year)){ 
  #### loop through each subplot and year and calc cover 
  #### each subplot's cover determined by last years cover 
  #### in that subplot and the yearly growth rate
  pgr = site1$pgr[i]
  newCover = exp(pgr)*df$cover[df$year == i - 1 ]   
  df$cover[ which(df$year == i) ] = rnorm(length(subplot), newCover, 2) #### This could represent measurement error 
}

df$cover[ df$cover < 0 ] <- 0 #### change negative values to 0
df

#### visualize simulated data 
ggplot( data = df, aes( x = as.numeric(year), y = cover , color = subplot ) ) + geom_point() + geom_line()
