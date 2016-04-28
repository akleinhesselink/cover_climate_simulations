rm(list = ls() ) 
#### Gompertz Population Dynamics single species 
library(lme4)
library(ggplot2)
library(mvtnorm)
source('R/functions.R')

model <- readRDS('~/Documents/sagebrush_response_to_climate/analysis/lmer_clim_model.rds')
summary(model)
A_pars <- coef(summary(model))[1, ]
B_pars <- coef(summary(model))[2, ]

m_cors <- cov2cor(vcov(model))
AB_cor <- m_cors['x', '(Intercept)']

#### parameters 
nloc = 20
nsites = 100
obsTime = runif( n = nsites, min = 1000, max = 1001)

burnTime = 100
time = obsTime + burnTime
pop_init = 100 

R <-  matrix( c(1, AB_cor, AB_cor, 1), byrow = TRUE, nrow = 2)
sdv <- c(A_pars[2], B_pars[2]) 
D <- diag(sdv)
S = D%*%R%*%D

AB_mat <- rmvnorm(n = nsites, mean = c(A_pars[1], B_pars[1]), sigma = S)

A = AB_mat[,1]
B = AB_mat[,2] 

C2 = 1 # climate effect second year of transition 
C1 = 0 # climate effect first year of transition  
C3 = 0 # climate interaction first year
C4 = 0 # climate interaction second year 
M1 <- seq( min( model@frame$tmax_growing_avg_log) , max(model@frame$tmax_growing_avg_log), length.out = nsites)

var_clim <- 1
EV <- seq( 0.01, 2, length.out = 20)

m_B <- mean( B)
m_A <- mean( A)

####

nsites = max( sapply(list(burnTime, time, pop_init), function (x) {  length ( x )  } ) ) 
results_list = list()

for ( i in 1:length(EV)) { 
  parms_df <- data.frame( site = factor(1:nsites), burnTime = burnTime, time = time, pop_init = pop_init, B = B, A = A, C2 = C2, C1 = C1, C3 = C3, C4 = C4, EV = EV[i], M1 = M1 )
  
  parms_list <- split(parms_df, f = 1:nrow(parms_df))
  
  #### 
  results <- lapply(parms_list, FUN = function(x) { do.call( run_simulation, args = as.list(x)) } ) 
  
  results <- do.call(rbind, results)
  results <- results[ !is.na(results$population), ]
  results_list[[i]] <- results
}

rm( B, A)

# dat_list = list(  
#                      y = results$population, 
#                      y_last = results$popLag, 
#                      plot = as.numeric(results$site),
#                      nplot = nlevels(factor(results$site)), 
#                      N = nrow(results), 
#                      clim1 = results$clim1,
#                      clim2 = results$clim2,
#                      M1 = results$M1
#                      )
# 
# dump(list = "dat_list", file = 'temp/sim_data.R' )

##### Plot time series 

library(ggplot2)
log_plot <- ggplot( data = results_list[[1]], aes( x = as.integer(year), y = population, color = site)) + 
  facet_wrap(~ site, ncol = 2  ) +
  scale_x_continuous() + 
  geom_point() + 
  geom_path() 

plot <- log_plot

plot$data$population <- exp(log_plot$data$population)
plot$data$site

plot %+% subset( plot$data, site %in% c(1:10) ) 

results_list[[1]]

##### simple linear model 
##### estimate parameters 
for (i in 1:length(results_list)) { 
  m1 = lmer( population ~  popLag + (popLag|site) + clim1 + clim2 + clim1:M1 + clim2:M1, data = results_list[[i]])
  model_list[[i]] <- m1
}

coeffs <- lapply( model_list, function(x) data.frame( coef(summary(x))))

coeffs <- lapply( coeffs, function(x ) data.frame( fixef = row.names(x), x))

coeffs <- do.call(rbind, coeffs )
coeffs$EV <- sort( rep(EV, length(fixef(model_list[[1]]))) ) 

head( coeffs)

coeffs$true_value = c(m_A, m_B, C1, C2, C3, C4 )


ggplot( data = subset( coeffs, fixef %in% c('clim1', 'clim1:M1', 'clim2', 'clim2:M1')), aes( x = EV, y = Estimate, color = fixef )) + 
  geom_point() +  
  geom_line() + 
  geom_hline(aes(yintercept = true_value )) + 
  facet_wrap(~ fixef)


summary(model_list[[6]])

m_A
m_B
C1
C2
C3
C4
