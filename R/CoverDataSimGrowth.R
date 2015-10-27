##### Test random effects approach to time series subplot agregation 
rm(list = ls())
library(ggplot2)
library(lme4)
#### simulate cover data at a site
#### there are 8 replicate subplots at the site
#### subplot cover varies over the course of 10 years
#### Test random effects and no random effects models to recover year effects in 2 cases: 
####  1. When all subplots are sampled each year 
####  2. When data from some subplots are missing in some years

###### ----------------------------------------------------------- 


##### simulate data 
subplot = factor(1:8)
year = 1:10

df =  expand.grid (subplot =  subplot,  year = year )
subplotMeans = c(8, 1, 2, 3, 20, 40, 60, 50)
TruePGR = data.frame(year = year, tPGR = rnorm(length(year), 0, 0.08))

df$cover = NA
df$cover[ df$year == 1  ] = subplotMeans #### set initial cover values 
head(df, 10)

for (i in 2:max(year)){ 
  #### loop through each subplot and year and calc cover 
  #### each subplot's cover determined by last years cover 
  #### in that subplot and the yearly growth rate
  newCover = exp(TruePGR$tPGR[TruePGR$year == i])*df$cover[df$year == i - 1 ]   
  df$cover[ which(df$year == i) ] = rnorm(length(subplot), newCover, 2) #### This could represent measurement error 
}

df$cover[ df$cover < 0 ] <- 0 #### change negative values to 0


#### visualize simulated data 
ggplot( data = df, aes( x = as.numeric(year), y = cover , color = subplot ) ) + geom_point() + geom_line()


########## ------------------------------------------------------------

#### Test 1:  all data 
m1 = lm( cover ~ as.factor(year), data = df)   
m2 = lmer( cover ~ as.factor(year) + (1|subplot), data = df)

predict(m1)
predict(m2, re.form = NA) ####  re.form = NA means no random effects
ranef(m2)
fixef(m2)

genMeanCoverTable = function( dat, mod1, mod2 ){ 
  #### Aggregate and compare yearly cover estimates 
  dat$predictedLM = predict(mod1, dat)
  
  dat$predictedMER = predict(mod2, dat, re.form = NA) 
  #### re.form is the trick here! It predicts without random effects

  yearlyMean = aggregate(cover ~ year, dat, FUN = 'mean') 
  meanCoverMER = aggregate(predictedMER ~ year, dat, FUN = 'mean')
  meanCoverLM = aggregate(predictedLM ~ year, dat, FUN = 'mean')
  
  meanCover = cbind(yearlyMean, LMCover = meanCoverLM$predictedLM, MERCover = meanCoverMER$predictedMER)
  return(meanCover)
} 

genMeanPGRTable = function ( meanCover){ 
  
  #### calculate PGR for each mean estimate 
  years = unique(meanCover$year)
  meanPGR = data.frame(year = years, PGR1 = NA, PGRLM = NA, PGRMER = NA) 
  
  for( i in 2:nrow(meanCover)  )  { 
    meanPGR[ i, 'PGR1'] = log( meanCover[ i, 2]/ meanCover[i - 1, 2])
    meanPGR[ i, 'PGRLM'] = log( meanCover[i, 3]/ meanCover[i - 1, 3])
    meanPGR[ i, 'PGRMER'] = log (meanCover[i, 4]/ meanCover[i - 1, 4])
  }
  
  return(meanPGR)

} 

#### Run the functions above 
coverTable = genMeanCoverTable(dat = df, m1, m2)
coverTable  #### cover estimates are the same 

PGRTable = genMeanPGRTable( coverTable)
PGRLong = stack(PGRTable, select = c(PGRLM, PGRMER))
PGRLong$year = unique(PGRTable$year)
PGRLong$Complete = 'Complete'

##### PGR estimates are the same 
ggplot(PGRLong, aes(x = as.numeric(year), y = values, color = ind)) + 
  geom_point() + 
  geom_line() + 
  geom_line( data = TruePGR, aes(x = as.numeric(year), y = tPGR, color = factor('True PGR')))

######--------------------------------------------------------------

##### Test 2:  Missing Data 
##### remove subset of data 

n = 20 #### select n random rows to skip 
missed = sample( 1:80 , n)  

dfPartial = df[ - missed , ]

dfPartial[ order(dfPartial$subplot, dfPartial$year), ] 
ggplot( dfPartial, aes(x = as.numeric(year), y = cover, color = subplot) ) + 
  geom_point() + 
  geom_line() 

m3 = lm( cover ~ as.factor(year), data = dfPartial)
m4 = lmer(cover ~ as.factor(year) + (1|subplot), data = dfPartial)

partialCovTable = genMeanCoverTable(dat = dfPartial, mod1= m3, mod2 = m4)

partialCovTable  ##### cover estimates are not the same 

##### make a dataframe for plotting 
partialCovTable$trueCover = coverTable$cover
partialCovTableLong = stack(partialCovTable, select = c(LMCover, MERCover, trueCover))
partialCovTableLong$year = year

##### Look at PGR 
ggplot(partialCovTableLong, aes(x = as.numeric(year), y = values, color = ind)) + geom_point() + geom_line()

##### Test althernate method calculate only PGR using only
##### plots that are present in year(t) and year(t+1) 
censoredPGR = NA  ### vector of PGR's
for (i in 2:max(year)){
  
  lastyearplots = dfPartial[ dfPartial$year == i - 1, 'subplot'] 
  lastyear = mean(dfPartial[ dfPartial$year == i - 1, 'cover'])
  thisyear = mean (as.numeric(dfPartial [ dfPartial$year == i & dfPartial$subplot %in% lastyearplots, 'cover' ]))
  censoredPGR[i] = log(thisyear/lastyear)   
}    

partialPGRTable = genMeanPGRTable(partialCovTable)
partialPGRTable$censoredPGR = censoredPGR

##### Next steps make a data frame for plotting 
partialPGRLong = stack(partialPGRTable, select = c(PGRLM, PGRMER, censoredPGR))
partialPGRLong$year = unique(partialPGRTable$year)
partialPGRLong$Complete = 'Incomplete'
partialPGRLong

tPGR_for_fig = rbind(TruePGR, TruePGR) 
tPGR_for_fig$ind = 'truePGR'
tPGR_for_fig$Complete = c(rep('Complete', 10), rep('Incomplete', 10)) 
tPGR_for_fig$year = tPGR_for_fig$year
names(tPGR_for_fig)[2] <- 'values'

allPGR_estimates = rbind(partialPGRLong, PGRLong, tPGR_for_fig ) #### plotting dataframe 

ggplot(allPGR_estimates,  aes(x = as.numeric(year), y = values, color = ind)) + 
  geom_point() + 
  geom_line() +
  facet_grid(. ~ Complete ) 
#### the random effects model cover estimates do a better job capturing the PGR with missing data


#### another data frame for plotting one-to-one line 
partialPGRCorTable = stack( partialPGRTable, select = c(PGRLM, PGRMER, censoredPGR) ) 
partialPGRCorTable$tPGR = TruePGR$tPGR
partialPGRCorTable

ggplot(partialPGRCorTable, aes( x = tPGR, y = values, color = ind, shape = ind)) + 
  geom_point( size = 3) + geom_abline( intercept = 0, slope = 1)

####  Correlations 
cor.test(partialPGRTable$PGRMER, TruePGR$tPGR) ##### correlation depends on data points missing 
cor.test(partialPGRTable$PGRLM, TruePGR$tPGR)
cor.test(partialPGRTable$censoredPGR, TruePGR$tPGR)

