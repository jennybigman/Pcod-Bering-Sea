# Fit Ricker spawner/recruit model with environmental covariate
# 
#  The 'recruits' column in the recruits time series is not yet multiplied 
#  by 1000 and the SSB column (SSB_mil_ton) is in millions of tons. 
#  
#  

library(dplyr)

rec<-read.csv(here("./data/SAFE_Pcod_EBS_2021_Recruits.csv"))
ssb<-read.csv(here("./data/SpawningStockBiomass_Thompson_etal_2021.csv"))

mydat<-merge(rec,ssb)
colnames(mydat)[1]<-"year"

mydat<- mydat %>%
  mutate(lrecr = log(recruits*1000),
         ssb = SSB_mil_ton*1000000) %>%
  mutate(lnRS = log(recruits*1000 / ssb)) %>%
  mutate(habindex = rnorm(length(mydat$year))) ## I just used random variable here for example!!


# first approach for modeling is helpful for comparing to models without
# ricker density dependence (using the log(ssb) offset so that only log recruits is on left side)
mod1<-lm(lrecr~ssb,offset=log(ssb),data=mydat) #Ricker
mod2<-lm(lrecr~ssb+habindex,offset=log(ssb),data=mydat) #Ricker with covariate 



# but can also fit like this:

mod1b<-lm(lnRS~ssb,data=mydat)
mod2b<-lm(lnRS~ssb+habindex,data=mydat)

# inspect slopes to see these give equivalent estimates
# note that the test for significant ssb slope is not appropriate since spawners 
# are on both sides of the equation, but we don't need that anyhow
summary(mod2)
summary(mod2b)

#could use AIC to compare models with and without habindex:
AIC(mod1b,mod2b) 

#or anova
anova(mod1b,mod2b)


#Regarding comparing simple linear regression and correlation:
#
cor.test(mydat$lnRS,mydat$habindex)
mod3<-lm(lnRS~habindex,data=mydat)
# the p-value for habindex slope in regression should be identical to
# p-value for correlation assuming 2-sided test.


