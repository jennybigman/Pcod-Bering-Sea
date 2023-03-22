# bootstrapping 

	library(tidyverse)
	library(here)
	library(data.table)
	library(car)
	library(gt)
  library(gtsummary)
	library(modelsummary)
	library(here)

	# load in data 
	eggs <- read.csv("../data and code from Laurel & Rogers 2020 CJFAS/laurelandrogerscode/Pcod and WP hatch probabilities.csv")

	eggs$Ntot <- eggs$Hatched + eggs$Dead
	eggs$Phatch <- eggs$Hatched/(eggs$Hatched + eggs$Dead)
	eggs <-eggs[eggs$Species=="Pcod",] #Pcod
	eggs$pMax<-eggs$Phatch/max(eggs$Phatch)
		
	egg_dat <- eggs %>% 
		dplyr::select(-contains("X")) %>%
		filter(Species == "Pcod")  %>%
		rename(temp = Temp_act) %>%
		dplyr::select(Phatch, temp, Ntot)


	ROMS_hindcast_dat <- fread(file = "./data/ROMS_dat_hind_trim.csv")

	ROMS_dat_hind_trim <- ROMS_hindcast_dat %>%
		dplyr::select(temp, year, latitude, longitude) %>%
		mutate(scenario = "hindcast",
					 simulation = "hindcast")
	
	ROMS_projected_dat <- fread(file = "./data/ROMS_projected_dat.csv") %>%
		mutate(latitude = Lat,
					 longitude = Lon, 
					 projection = scenario,
					 simulation = toupper(simulation),
					 projection = toupper(projection)) %>%
		dplyr::select(bc_temp_sd, year, latitude, longitude, scenario, simulation) %>%
		rename(temp = bc_temp_sd)
	
	ROMS_temp <- bind_rows(ROMS_dat_hind_trim, ROMS_projected_dat)


	#### Bootstrap existing NLS ####
	
	## Cauchy ####
	
	cauchy_mod <- nls(Phatch ~ k/(1 + ((temp - mu)/sigma)^2), start = c(mu = 5, sigma = 2, k = 1), 
										data = egg_dat, weights=Ntot)

	set.seed(8)
	cauchy_boot <- Boot(cauchy_mod, R = 500, method = "case")
	
	cauchy_boot_output <- as.data.frame(cauchy_boot$t) %>% na.omit()
	

	# subsample 10
	set.seed(8)
	
	cauchy_boot_sample <- cauchy_boot_output %>%
		sample_n(10) %>%
		mutate(iter = 1:10)
	

	# generate new time series of sp hab suit for bootstrapped coefs
	row_nums <- 1:10
	
	# function to create new dfs with bootstrapped coeffs
	dat_combine_func <- function(x){
 
 		k_cauchy <- rep(cauchy_boot_sample$k[x], nrow(ROMS_temp))
		mu_cauchy <- rep(cauchy_boot_sample$mu[x], nrow(ROMS_temp))
		sigma_cauchy <- rep(cauchy_boot_sample$sigma[x], nrow(ROMS_temp))
		
		coefs <- tibble(k_cauchy, mu_cauchy, sigma_cauchy)
		
		df <- bind_cols(ROMS_temp, coefs) 
		
	}
	
	dat_list <- lapply(row_nums, dat_combine_func)
	
	# calc hatch success for each df
	hs_func_cauchy <- function(df){
 	
		df <- df %>% 
		 rowwise() %>% 
		 mutate(hs_cauch = (k_cauchy / (1 + (((temp - mu_cauchy)/sigma_cauchy))^2)))
		
		max_hs <- max(df$hs_cauch)
		
		df <- df %>%
			mutate(sphabsuit_cauch = hs_cauch/max_hs)
	}
 
	dat_list <- lapply(dat_list, hs_func_cauchy)
	
	#  add a column to differentiate the 10 combos of bootstrapped coefs
	samp_ID_func <- function(df, x){
 	
		df <- bind_cols(df, x)
		
		}
 
	dat_list2 <- mapply(samp_ID_func, dat_list, x = 1:10, SIMPLIFY=F)
	
	ROMS_cauchy_sens <- bind_rows(dat_list2)
	
#	new_data <- ROMS_cauchy_sens %>% filter_all(any_vars(is.na(.))) 


	ROMS_cauchy_sens <- ROMS_cauchy_sens %>%
		rename(iter = "...12") %>%
		dplyr::filter(scenario != "historical")
	
	write_csv(ROMS_cauchy_sens, file = here("./data/ROMS_cauchy_sens.csv"))
	
	ROMS_cauchy_sens <- fread(file = here("./data/ROMS_cauchy_sens.csv"))
	
	
	ROMS_cauchy_sens_hind <- ROMS_cauchy_sens %>%
		dplyr::filter(scenario == "hindcast") 

	scens <- c("ssp126", "ssp585")
	
	ROMS_cauchy_sens_proj <- ROMS_cauchy_sens %>%
		dplyr::filter(scenario %in% scens) 
	
	# any trend?
	
	# hindcast
	ROMS_cauchy_sens_hind$iter_c <- as.character(ROMS_cauchy_sens_hind$iter)
	
	lm_func_hind <- function(x){
		
		new_dat <- ROMS_cauchy_sens_hind %>% filter(iter_c == x)
		
		fit <- lm(sphabsuit_cauch ~ year, data = new_dat)
		
		fits <- coef(fit)[2]
		lwr <- confint(fit)[2, 1]
		upr <- confint(fit)[2, 2]
		iter_c <- x
		
		fit_out <- tibble(fits, lwr, upr, iter_c)
		
	}
	
	iters <- unique(ROMS_cauchy_sens_hind$iter_c)
	
	lm_list <- lapply(iters, lm_func_hind)
	
	lm_hind_out <- bind_rows(lm_list)
	
	lm_hind_out_cauch <- lm_hind_out %>%
		rename(mean = fits, 
					 lower = lwr, 
					 upper = upr, 
					 iter = iter_c) %>%
		mutate(scenario = "hindcast")
	
	# projection
	
	ROMS_cauchy_sens_proj$iter_c <- as.character(ROMS_cauchy_sens_proj$iter)

	lm_func_proj <- function(x, y){
		
		new_dat <- ROMS_cauchy_sens_proj %>% filter(iter_c == x & scenario == y)
		
		fit <- lm(sphabsuit_cauch ~ year, data = new_dat)
		
		fits <- coef(fit)[2]
		lwr <- confint(fit)[2, 1]
		upr <- confint(fit)[2, 2]
		iter_c <- x
		scenario <- y
		
		fit_out <- tibble(fits, lwr, upr, iter_c, scenario)
		
	}
	
	iters <- unique(ROMS_cauchy_sens_hind$iter_c)
	
	scens <- rep(unique(ROMS_cauchy_sens_proj$scenario), each = 10)
	
	
	lm_list_proj <- mapply(lm_func_proj, 
												 x = iters, y = scens, 
												 SIMPLIFY = FALSE)
	
	lm_proj_out <- bind_rows(lm_list_proj)
	
	lm_proj_out_cauchy <- lm_proj_out %>%
		rename(mean = fits, 
					 lower = lwr, 
					 upper = upr, 
					 iter = iter_c) 

	lm_cauchy_outs <- bind_rows(lm_hind_out_cauch, lm_proj_out_cauchy)

	#### make table ####
	
	#cauch_lm_fits <- gt(lm_cauchy_outs)
 #
	#gtsave(cauch_lm_fits, 
	#			 file = here("./scripts/manuscript figure scripts/used in ms/pngs of figs/cauch_lm_fits.docx"))

	
	# plots ####
	
	# time series
	ROMS_cauchy_sens_hind_sum <- ROMS_cauchy_sens_hind %>%
		group_by(year, iter) %>%
		summarise(mean_hbsuit = mean(sphabsuit_cauch)) %>%
		mutate(scenario = "hindcast") %>%
		mutate(iter = as.factor(iter))
	
	ROMS_cauchy_sens_proj_sum <- ROMS_cauchy_sens_proj %>%
		group_by(year, iter, scenario) %>%
		summarise(mean_hbsuit = mean(sphabsuit_cauch)) %>%
		mutate(iter = as.factor(iter))
	
	ROMS_sens_cauchy_sum <- bind_rows(ROMS_cauchy_sens_hind_sum, ROMS_cauchy_sens_proj_sum)

	# add time series from manuscript
	yearly_temp_hind <- ROMS_hindcast_dat %>%
		group_by(year) %>%
    summarise(mean_sp_hab_suit = mean(sp_hab_suit)) %>%
		mutate(scenario = "hindcast")
	
	years_proj <- 2020:2099
	
	yearly_temp_proj <- ROMS_projected_dat %>% 
		filter(year %in% years_proj) %>%
		group_by(simulation, projection, year) %>%
   	summarise(mean_sp_hab_suit = mean(sp_hab_suit_var)) 
	
	yearly_temp_proj$scenario <- NA
		
	yearly_temp_proj$scenario[yearly_temp_proj$projection == "SSP126"] <- "ssp126"
	yearly_temp_proj$scenario[yearly_temp_proj$projection == "SSP585"] <- "ssp585"

	sens_cauchy_plot <-
		ggplot() +
		geom_line(data = ROMS_sens_cauchy_sum,
							aes(year, mean_hbsuit, 
									group = iter, 
									color = iter)) +
		geom_line(data = yearly_temp_hind,
					  	aes(year, mean_sp_hab_suit), 
								color = "black") +
		geom_line(data = yearly_temp_proj,
						 aes(year, mean_sp_hab_suit), 
								color = "black") +
		facet_wrap(~ scenario, scales = "free") +
		xlab("Year") + 
		ylab("Thermal spawning\nhabitat suitability") +
   	theme_bw()
   		
	 ggsave("./scripts/manuscript figure scripts/used in ms/pngs of figs/sens_cauchy_plot_SI.tiff",
			 sens_cauchy_plot)


	## Gaussian equation: Phatch ~ k * e^(-1/2 * (temp - mu)^2/sigma^2) ####
	
	gaus_mod <- nls(Phatch ~ k * exp(-1/2 * (temp - mu)^2/sigma^2), 
									 start = c(mu = 5, sigma = 2, k = 1), data = egg_dat, weights=Ntot)

	set.seed(8)
	gaus_boot <- Boot(gaus_mod, R = 500, method = "case")
	
	gaus_boot_boot_output <- as.data.frame(gaus_boot$t) %>% na.omit()
	
	# subsample 10
	set.seed(8)
	gaus_boot_boot_sample <- gaus_boot_boot_output %>%
		sample_n(10)
	
	# generate new time series of sp hab suit for bootstrapped coefs
	row_nums <- 1:10
	
	# function to create new dfs with bootstrapped coeffs
	dat_combine_func <- function(x){
 
 		k_gaus <- rep(gaus_boot_boot_sample$k[x], nrow(ROMS_temp))
		mu_gaus <- rep(gaus_boot_boot_sample$mu[x], nrow(ROMS_temp))
		sigma_gaus <- rep(gaus_boot_boot_sample$sigma[x], nrow(ROMS_temp))
		
		coefs <- tibble(k_gaus, mu_gaus, sigma_gaus)
		
		df <- bind_cols(ROMS_temp, coefs) 
		
	}
	
	dat_list3 <- lapply(row_nums, dat_combine_func)
	
	# calc hatch success for each df
	hs_func_gaus <- function(df){
 	
		df <- df %>% 
		 rowwise() %>% 
		 mutate(hs_gaus = (k_gaus * exp(-1/2 * (temp - mu_gaus)^2/sigma_gaus^2)))
		
		max_hs <- max(df$hs_gaus)
		
		df <- df %>%
			mutate(sphabsuit_gaus = hs_gaus/max_hs)
	}
 
	dat_list4 <- lapply(dat_list3, hs_func_gaus)
	
	#  add a column to differentiate the 10 combos of bootstrapped coefs
	samp_ID_func <- function(df, x){
 	
		df <- bind_cols(df, x)
		
		}
 
	dat_list5 <- mapply(samp_ID_func, dat_list4, x = 1:10, SIMPLIFY=F)
	
	ROMS_gaus_sens <- bind_rows(dat_list5)

	
	write_csv(ROMS_gaus_sens, file = here("./data/ROMS_gaus_sens.csv"))


	ROMS_gaus_sens <- fread(file = here("./data/ROMS_gaus_sens.csv"))
	
	ROMS_gaus_sens_hind <- ROMS_gaus_sens %>%
		dplyr::filter(scenario == "hindcast") %>%
		rename(iter = "...12") 

	scens <- c("ssp126", "ssp585")
	
	ROMS_gaus_sens_proj <- ROMS_gaus_sens %>%
		dplyr::filter(scenario %in% scens) %>%
		rename(iter = "...12")
	
	# any trend?
	
	# hindcast
	ROMS_gaus_sens_hind$iter_c <- as.character(ROMS_gaus_sens_hind$iter)

	lm_func_hind <- function(x){
		
		new_dat <- ROMS_gaus_sens_hind %>% filter(iter_c == x)
		
		fit <- lm(sphabsuit_gaus ~ year, data = new_dat)
		
		fits <- coef(fit)[2]
		lwr <- confint(fit)[2, 1]
		upr <- confint(fit)[2, 2]
		iter_c <- x
		
		fit_out <- tibble(fits, lwr, upr, iter_c)
		
	}
	
	iters <- unique(ROMS_gaus_sens_hind$iter_c)
	
	lm_list <- lapply(iters, lm_func_hind)
	
	lm_hind_out_gaus <- bind_rows(lm_list)
	
	lm_hind_out_gaus <- lm_hind_out_gaus %>%
		rename(mean = fits, 
					 lower = lwr, 
					 upper = upr, 
					 iter = iter_c) %>%
		mutate(scenario = "hindcast")
	
	# projection
	
	ROMS_gaus_sens_proj$iter_c <- as.character(ROMS_gaus_sens_proj$iter)

	lm_func_proj <- function(x, y){
		
		new_dat <- ROMS_gaus_sens_proj %>% filter(iter_c == x & scenario == y)
		
		fit <- lm(sphabsuit_gaus ~ year, data = new_dat)
		
		fits <- coef(fit)[2]
		lwr <- confint(fit)[2, 1]
		upr <- confint(fit)[2, 2]
		iter_c <- x
		scenario <- y
		
		fit_out <- tibble(fits, lwr, upr, iter_c, scenario)
		
	}
	
	iters <- unique(ROMS_gaus_sens_hind$iter_c)
	
	scens <- rep(unique(ROMS_gaus_sens_proj$scenario), each = 10)
	
	
	lm_list_proj <- mapply(lm_func_proj, 
												 x = iters, y = scens, 
												 SIMPLIFY = FALSE)
	
	lm_proj_out_gaus <- bind_rows(lm_list_proj) 
	
	lm_proj_out_gaus <- lm_proj_out_gaus %>%
		rename(mean = fits, 
					 lower = lwr, 
					 upper = upr, 
					 iter = iter_c) 

	lm_gaus_outs <- bind_rows(lm_hind_out_gaus, lm_proj_out_gaus)

	#### make table ####
	
	gaus_lm_fits <- gt(lm_gaus_outs)
 
	gtsave(gaus_lm_fits, 
				 file = here("./scripts/manuscript figure scripts/used in ms/pngs of figs/gaus_lm_fits.docx"))

	
		# plot ####
	ROMS_gaus_sens_hind_sum <- ROMS_gaus_sens_hind %>%
		group_by(year, iter) %>%
		summarise(mean_hbsuit = mean(sphabsuit_gaus)) %>%
		mutate(scenario = "historical") %>%
		mutate(iter = as.factor(iter))
	
	ROMS_gaus_sens_proj_sum <- ROMS_gaus_sens_proj %>%
		group_by(year, iter, scenario) %>%
		summarise(mean_hbsuit = mean(sphabsuit_gaus)) %>%
		mutate(iter = as.factor(iter))
	
	ROMS_sens_gaus <- bind_rows(ROMS_gaus_sens_hind_sum, ROMS_gaus_sens_proj_sum)

	sens_gaus_plot <-
		ggplot() +
		geom_line(data = ROMS_sens_gaus,
							aes(year, mean_hbsuit, 
									group = iter, 
									color = iter)) +
		facet_wrap(~ scenario, scales = "free") +
		xlab("Year") + 
   	theme_bw()
   		
	 ggsave("./scripts/manuscript figure scripts/used in ms/pngs of figs/sens_gaus_plot_SI.tiff",
			 sens_gaus_plot)
