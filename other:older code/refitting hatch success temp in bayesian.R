# older code - refitting hatch success - temp R

# refitting hatch success curve using brms

	library(brms)

	# load in data
	eggs <- read.csv("../data and code from Laurel & Rogers 2020 CJFAS/laurelandrogerscode/Pcod and WP hatch probabilities.csv")
	
	eggs2 <- eggs %>% 
		dplyr::select(-contains("X")) %>%
		filter(Species == "Pcod")
	
	
	eggs$Ntot <- eggs$Hatched + eggs$Dead
	eggs$Phatch <- eggs$Hatched/(eggs$Hatched + eggs$Dead)
	eggs <-eggs[eggs$Species=="Pcod",] #Pcod
	eggs$pMax<-eggs$Phatch/max(eggs$Phatch)
	
	# gaussian
	res<- nls( Phatch ~ k*exp(-1/2*(Temp_act-mu)^2/sigma^2), start=c(mu=5,sigma=2,k=1) , data = eggs,weights=Ntot)
	v <- summary(res)$parameters[,"Estimate"]

	# cauchy
	res2 <- nls( Phatch ~ k/(1+((Temp_act-mu)/sigma)^2), start=c(mu=5,sigma=2,k=1) , data = eggs, weights=Ntot)
	v2 <- summary(res2)$parameters[,"Estimate"]

	cauchy <- function(x) v2[3]/(1+((x-v2[1])/v2[2])^2)

	################################################
fit_loss <- brm(
  bf(cum ~ ult * (1 - exp(-(dev/theta)^omega)),
     ult ~ 1 + (1|AY), omega ~ 1, theta ~ 1,
     nl = TRUE),
  data = loss, family = gaussian(),
  prior = c(
    prior(normal(5000, 1000), nlpar = "ult"),
    prior(normal(1, 2), nlpar = "omega"),
    prior(normal(45, 10), nlpar = "theta")
  ),
  control = list(adapt_delta = 0.9)
)
	################################################

 test_fit <- brm(bf(Phatch ~ k*exp(-1/2*(Temp_act-mu)^2/sigma^2), 
 									 k + mu + sigma ~ 1,
 									 nl = TRUE),
 								data = eggs2, family = gaussian())
	
	summary(test_fit)
 									 
 	################################################
							
	
	hillprior <- c(
  set_prior("normal(0.2, 0.1)", nlpar = "a", lb=0, ub=1),
  set_prior("normal(2, 1)", nlpar = "b", lb=1),
  set_prior("normal(7, 3)", nlpar = "c", lb=0), 
  set_prior("normal(0.05, 0.2)", class="sigma"))
	
hill_bayes_fit_formula <- bf(parentFraction ~ 1 - ( ( (1-a) * Time^b) / 
                                                      ( 10^c + (Time)^b ) ),
     # Nonlinear variables
     a + b + c ~ 1,
     # Nonlinear fit
     nl = TRUE)
hill_bayes_fit <- brm(
  hill_bayes_fit_formula,
  family=gaussian(), 
  data = pfdat,
  prior = hillprior )

	################################################

	priors <- c(
  	set_prior("normal(0, 10)", nlpar = "a"),
  	set_prior("normal(0, 10)", nlpar = "b"))

	fit_formula_gaussian <- bf(
		Phatch ~ (1/(a * sqrt(2*3.14)) * exp(-1/2 * (Temp_act - b)^2/a^2)),
     # Nonlinear variables
     a + b ~ 1, # a = sigma, # b = mu
     # Nonlinear fit
     nl = TRUE)
	
	test_fit_gaussian <- brm(
  	fit_formula_gaussian,
  	family=gaussian(), 
  	prior = priors,
  	data = eggs2)
	
	summary(test_fit_gaussian)

	
	fit_formula_cauchy <- bf(
		Phatch ~ (1/(3.14 * a))/(1+((Temp_act-b)/a)^2),
     # Nonlinear variables
     a + b ~ 1, # a = sigma, # b = mu
     # Nonlinear fit
     nl = TRUE)

	test_fit_cauchy <- brm(
  	fit_formula_gaussian,
  	family=gaussian(), 
  	prior = priors,
  	data = eggs2)
	
	summary(test_fit_cauchy)
	
	
	###################################
	
	priors <- c(
  	set_prior("normal(0, 10)", nlpar = "a"),
  	set_prior("normal(0, 10)", nlpar = "b"),
  	set_prior("normal(0,1)", class = sigma))

	fit_formula_gaussian <- bf(
		Phatch ~ a*exp(-1/2*(Temp_act-b)^2/sigma^2)
     # Nonlinear variables
     a + b ~ 1, # a = sigma, # b = mu
     # Nonlinear fit
     nl = TRUE)
	
	test_fit_gaussian <- brm(
  	fit_formula_gaussian,
  	family=gaussian(), 
  	prior = priors,
  	data = eggs2)
	
	summary(test_fit_gaussian)

	
	fit_formula_cauchy <- bf(
		Phatch ~ (1/(3.14 * a))/(1+((Temp_act-b)/a)^2),
     # Nonlinear variables
     a + b ~ 1, # a = sigma, # b = mu
     # Nonlinear fit
     nl = TRUE)

	test_fit_cauchy <- brm(
  	fit_formula_gaussian,
  	family=gaussian(), 
  	prior = priors,
  	data = eggs2)
	
	summary(test_fit_gaussian)

######################################################
	# Bootstrapping NLS
	
	# existing NLS
	library(tidyverse)
	library(MASS)

	# load in data
	eggs <- read.csv("../data and code from Laurel & Rogers 2020 CJFAS/laurelandrogerscode/Pcod and WP hatch probabilities.csv")
	
	eggs2 <- eggs %>% 
		dplyr::select(-contains("X")) %>%
		filter(Species == "Pcod")
	
	
	eggs$Ntot <- eggs$Hatched + eggs$Dead
	eggs$Phatch <- eggs$Hatched/(eggs$Hatched + eggs$Dead)
	eggs <-eggs[eggs$Species=="Pcod",] #Pcod
	eggs$pMax<-eggs$Phatch/max(eggs$Phatch)
	
	# gaussian
	gaus_mod <- nls( Phatch ~ k*exp(-1/2*(Temp_act-mu)^2/sigma^2), start=c(mu=5,sigma=2,k=1) , data = eggs,weights=Ntot)
	v <- summary(res)$parameters[,"Estimate"]

	# cauchy
	cauchy_mod <- nls( Phatch ~ k/(1+((Temp_act-mu)/sigma)^2), start=c(mu=5,sigma=2,k=1) , data = eggs, weights=Ntot)
	v2 <- summary(res2)$parameters[,"Estimate"]

	cauchy <- function(x) v2[3]/(1+((x-v2[1])/v2[2])^2)
	
	
	# bootstrap cauchy first
	
	# equation: Phatch = k/(1 + ((x - mu)/sigma^2)
	
	# need to bootstrap k, mu, sigma
	egg_dat <- eggs %>%
		rename(temp = Temp_act) %>%
		dplyr::select(Phatch, temp, Ntot)
	
		cauchy_mod <- nls(Phatch ~ k/(1 + ((temp - mu)/sigma)^2), start = c(mu = 5, sigma = 2, k = 1), 
											data = egg_dat, weights=Ntot)

	summary(cauchy_mod)
	
	# extract coefficients and covariances matrix
	cauchy_coefs <- coef(cauchy_mod)	
	cauchy_vcov <- vcov(cauchy_mod)
		
	# no of iterations
	iter <- 500
	
	# bootstrap
	output <- mapply(mvrnorm, n = iter, mu = cauchy_coefs, Sigma = cauchy_vcov)
	
	
	######## 
	
	# try it with gaussian
	
	gaus_mod <- nls( Phatch ~ k*exp(-1/2*(Temp_act-mu)^2/sigma^2), start=c(mu=5,sigma=2,k=1) , data = eggs,weights=Ntot)

	gaus_coefs <- coef(gaus_mod)	
	gaus_vcov <- vcov(gaus_mod)
		
	# no of iterations
	iter <- 500
	
	# bootstrap
	output <- mapply(mvrnorm, n = iter, mu = gaus_coefs, Sigma = gaus_vcov)
	
	########### 

	# try it with the Boot() function
	
	cauchy_boot <- Boot(cauchy_mod, R = 500)
	
	cauchy_boot_output <- as.data.frame(cauchy_boot$t)
	
	# subsample 10
	cauchy_boot_sample <- cauchy_boot_output %>%
		sample_n(10)
	
	
	