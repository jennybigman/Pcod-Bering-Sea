# bootstrapping 

	library(tidyverse)
	library(here)
	library(data.table)
	library(car)
	library(gt)
  library(gtsummary)
	library(modelsummary)
	library(here)
	library(patchwork)


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
	
	ROMS_projected_dat <- fread(file = "./data/ROMS_projected_dat.csv")
	
	ROMS_projected_dat_trim <- ROMS_projected_dat	%>%
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

	ROMS_cauchy_sens <- ROMS_cauchy_sens %>%
		rename(iter = "...12") %>%
		dplyr::filter(scenario != "historical")
	
	write_csv(ROMS_cauchy_sens, file = here("./data/ROMS_cauchy_sens.csv"))
	
	ROMS_cauchy_sens <- fread(file = here("./data/ROMS_cauchy_sens.csv"))
	
	# linear trends
	
	# linear models of hindcast ####
	
	cauch_hind_yr <- ROMS_cauchy_sens_hind %>%
		group_by(year, iter) %>%
    summarise(mean_sphabsuit_cauch = mean(sphabsuit_cauch))
	
	cauch_hind_yr_fits <- cauch_hind_yr %>%
    group_by(iter) %>%
    do(broom::tidy(lm(mean_sphabsuit_cauch ~ year, data = .), conf.int = .95)) %>%
    ungroup %>% rename(model = iter)
	
	cauch_hind_yr_fits <- cauch_hind_yr_fits %>%
		dplyr::filter(term == "year")

	in_paper <- tibble(
		model = 0,
		term = "year",
		estimate = 0.0006616,
		std.error = 0.0004994, 
		statistic = NA,
		p.value = 0.191,
		conf.low = -0.0003408616,
		conf.high = 0.001664144)
	
	cauch_hind_yr_fits <- bind_rows(in_paper, cauch_hind_yr_fits)
	
	cauch_hind_yr_fits$model[cauch_hind_yr_fits$model == "0"] <- "in paper"

	
	# make plot
	coef_plot_cauch_hind <- 
		secret_weapon(cauch_hind_yr_fits, var = "year") +
	  xlab("Coefficient estimate") + 
		ylab("Iteration") +
		theme_bw() +
		scale_colour_grey() +
		scale_x_continuous(
			breaks = c(0, 0.001),
			labels = c(0, 0.001)
		) +
		theme(legend.position = "none") +
		ggtitle("a)")
	
	
	# projection ####
	
	# low emission scenario
	cauch_proj_yr_ssp126 <- ROMS_cauchy_sens_proj %>%
		filter(scenario == "ssp126") %>%
		group_by(year, iter) %>%
    summarise(mean_sphabsuit_cauch = mean(sphabsuit_cauch))
	
	cauch_proj_yr_ssp126_fits <- cauch_proj_yr_ssp126 %>%
    group_by(iter) %>%
    do(broom::tidy(lm(mean_sphabsuit_cauch ~ year, data = .), conf.int = .95)) %>%
    ungroup %>% rename(model = iter)
	
	cauch_proj_yr_ssp126_fits <- cauch_proj_yr_ssp126_fits %>%
		dplyr::filter(term == "year")

	in_paper <- tibble(
		model = 0,
		term = "year",
		estimate = 0.0007879,
		std.error = 0.0002051,
		statistic = NA,
		p.value = 0.000157,
		conf.low =  0.0003838231,
		conf.high = 0.001191981)
	
	cauch_proj_yr_ssp126_fits <- bind_rows(in_paper, cauch_proj_yr_ssp126_fits)
	
	cauch_proj_yr_ssp126_fits$model[cauch_proj_yr_ssp126_fits$model == "0"] <- "in paper"

	
	# make plot
	coef_plot_cauch_proj_ssp126 <- 
		secret_weapon(cauch_proj_yr_ssp126_fits, var = "year") +
	  xlab("Coefficient estimate") + 
		ylab("Iteration") +
		scale_colour_grey() +
		ggtitle("b)") +
		scale_x_continuous(
			breaks = c(0.0006, 0.001),
			labels = c("0.0006", 0.001)
		) +
		theme_bw() +
		theme(legend.position = "none",
					axis.text.y = element_blank(),
					axis.title.y = element_blank(),
					axis.ticks.y = element_blank())
	
 # high emission scenario
	cauch_proj_yr_ssp585 <- ROMS_cauchy_sens_proj %>%
		filter(scenario == "ssp585") %>%
		group_by(year, iter) %>%
    summarise(mean_sphabsuit_cauch = mean(sphabsuit_cauch))
	
	cauch_proj_yr_ssp585_fits <- cauch_proj_yr_ssp585 %>%
    group_by(iter) %>%
    do(broom::tidy(lm(mean_sphabsuit_cauch ~ year, data = .), conf.int = .95)) %>%
    ungroup %>% rename(model = iter)
	
	cauch_proj_yr_ssp585_fits <- cauch_proj_yr_ssp585_fits %>%
		dplyr::filter(term == "year")
	

	in_paper <- tibble(
		model = 0,
		term = "year",
		estimate =  0.0029634,
		std.error = 0.0001989,
		statistic = NA,
		p.value = NA,
		conf.low =   0.002571493,
		conf.high = 0.003355326)
	
	cauch_proj_yr_ssp585_fits <- bind_rows(in_paper, cauch_proj_yr_ssp585_fits)
	
	cauch_proj_yr_ssp585_fits$model[cauch_proj_yr_ssp585_fits$model == "0"] <- "in paper"

	# make plot
	coef_plot_cauch_proj_ssp585 <- 
		secret_weapon(cauch_proj_yr_ssp585_fits, var = "year") +
	  xlab("Coefficient estimate") + 
		ylab("Iteration") +
		scale_color_grey() +
		scale_x_continuous(
			breaks = c(0.0025, 0.003),
			labels = c(0.0025, 0.003)
		) +
		ggtitle("c)") +
		theme_bw() +
		theme(legend.position = "none",
					axis.text.y = element_blank(),
					axis.title.y = element_blank(),
					axis.ticks.y = element_blank())
	
	# plot together 
	
 plot1 <- coef_plot_cauch_hind 

 plot2 <- coef_plot_cauch_proj_ssp126

 plot3 <- coef_plot_cauch_proj_ssp585 


 coef_plots_cauch <- plot1 + plot2 + plot3 


 ggsave("./scripts/manuscript figure scripts/used in ms/pngs of figs/coef_plots_cauch.tiff",
			 coef_plots_cauch, dpi = 500)
	
	# plot time series 
	ROMS_cauchy_sens_hind <- ROMS_cauchy_sens %>%
		dplyr::filter(scenario == "hindcast") 

	scens <- c("ssp126", "ssp585")
	
	ROMS_cauchy_sens_proj <- ROMS_cauchy_sens %>%
		dplyr::filter(scenario %in% scens) 
	

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
	yearly_habsuit_hind <- ROMS_hindcast_dat %>%
		group_by(year) %>%
    summarise(mean_sp_hab_suit = mean(sp_hab_suit)) %>%
		mutate(scenario = "hindcast")
	
	years_proj <- 2020:2099
	
	yearly_habsuit_proj <- ROMS_projected_dat %>% 
		filter(year %in% years_proj) %>%
		group_by(projection, year) %>%
   	summarise(mean_sp_hab_suit = mean(sp_hab_suit_var)) 
	
	yearly_habsuit_proj$scenario <- NA
		
	yearly_habsuit_proj$scenario[yearly_habsuit_proj$projection == "ssp126"] <- "ssp126"
	yearly_habsuit_proj$scenario[yearly_habsuit_proj$projection == "ssp585"] <- "ssp585"

	sens_cauchy_plot <-
		ggplot() +
		geom_line(data = ROMS_sens_cauchy_sum,
							aes(year, mean_hbsuit, 
									group = iter, 
									color = iter)) +
		geom_line(data = yearly_habsuit_hind,
					  	aes(year, mean_sp_hab_suit), 
								color = "black") +
		geom_line(data = yearly_habsuit_proj,
						 aes(year, mean_sp_hab_suit), 
								color = "black") +
		facet_wrap(~ scenario, scales = "free") +
		xlab("Year") + 
		ylab("Thermal spawning\nhabitat suitability") +
   	theme_bw()
   		
	 ggsave("./scripts/manuscript figure scripts/used in ms/pngs of figs/sens_cauchy_plot_SI.tiff",
			 sens_cauchy_plot, dpi = 500)



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
	
	ROMS_gaus_sens <- ROMS_gaus_sens %>%
		rename(iter = "...12")
	
	ROMS_gaus_sens_hind <- ROMS_gaus_sens %>%
		dplyr::filter(scenario == "hindcast")

	scens <- c("ssp126", "ssp585")
	
	ROMS_gaus_sens_proj <- ROMS_gaus_sens %>%
		dplyr::filter(scenario %in% scens) 
	
	# linear trends
	
	# linear models of hindcast ####
	
	gaus_hind_yr <- ROMS_gaus_sens_hind %>%
		group_by(year, iter) %>%
    summarise(mean_sphabsuit_gaus = mean(sphabsuit_gaus))
	
	gaus_hind_yr_fits <- gaus_hind_yr %>%
    group_by(iter) %>%
    do(broom::tidy(lm(mean_sphabsuit_gaus ~ year, data = .), conf.int = .95)) %>%
    ungroup %>% rename(model = iter)
	
	gaus_hind_yr_fits <- gaus_hind_yr_fits %>%
		dplyr::filter(term == "year")

	in_paper <- tibble(
		model = 0,
		term = "year",
		estimate = 0.0006616,
		std.error = 0.0004994, 
		statistic = NA,
		p.value = 0.191,
		conf.low = -0.0003408616,
		conf.high = 0.001664144)
	
	gaus_hind_yr_fits <- bind_rows(in_paper, gaus_hind_yr_fits)
	
	gaus_hind_yr_fits$model[gaus_hind_yr_fits$model == "0"] <- "in paper"

	
	# make plot
	coef_plot_gaus_hind <- 
		secret_weapon(gaus_hind_yr_fits, var = "year") +
	  xlab("Coefficient estimate") + 
		ylab("Iteration") +
		theme_bw() +
		scale_colour_grey() +
		scale_x_continuous(
			breaks = c(0, 0.001),
			labels = c(0, 0.001)
		) +
		theme(legend.position = "none") +
		ggtitle("a)")
	
	
	# projection ####
	
	# low emission scenario
	gaus_proj_yr_ssp126 <- ROMS_gaus_sens_proj %>%
		filter(scenario == "ssp126") %>%
		group_by(year, iter) %>%
    summarise(mean_sphabsuit_gaus = mean(sphabsuit_gaus))
	
	gaus_proj_yr_ssp126_fits <- gaus_proj_yr_ssp126 %>%
    group_by(iter) %>%
    do(broom::tidy(lm(mean_sphabsuit_gaus ~ year, data = .), conf.int = .95)) %>%
    ungroup %>% rename(model = iter)
	
	gaus_proj_yr_ssp126_fits <- gaus_proj_yr_ssp126_fits %>%
		dplyr::filter(term == "year")

	in_paper <- tibble(
		model = 0,
		term = "year",
		estimate = 0.0007879,
		std.error = 0.0002051,
		statistic = NA,
		p.value = 0.000157,
		conf.low =  0.0003838231,
		conf.high = 0.001191981)
	
	gaus_proj_yr_ssp126_fits <- bind_rows(in_paper, gaus_proj_yr_ssp126_fits)
	
	gaus_proj_yr_ssp126_fits$model[gaus_proj_yr_ssp126_fits$model == "0"] <- "in paper"

	
	# make plot
	coef_plot_gaus_proj_ssp126 <- 
		secret_weapon(gaus_proj_yr_ssp126_fits, var = "year") +
	  xlab("Coefficient estimate") + 
		ylab("Iteration") +
		scale_colour_grey() +
		ggtitle("b)") +
		scale_x_continuous(
			breaks = c(0.0006, 0.001),
			labels = c("0.0006", 0.001)
		) +
		theme_bw() +
		theme(legend.position = "none",
					axis.text.y = element_blank(),
					axis.title.y = element_blank(),
					axis.ticks.y = element_blank())
	
 # high emission scenario
	gaus_proj_yr_ssp585 <- ROMS_gaus_sens_proj %>%
		filter(scenario == "ssp585") %>%
		group_by(year, iter) %>%
    summarise(mean_sphabsuit_gaus = mean(sphabsuit_gaus))
	
	gaus_proj_yr_ssp585_fits <- gaus_proj_yr_ssp585 %>%
    group_by(iter) %>%
    do(broom::tidy(lm(mean_sphabsuit_gaus ~ year, data = .), conf.int = .95)) %>%
    ungroup %>% rename(model = iter)
	
	gaus_proj_yr_ssp585_fits <- gaus_proj_yr_ssp585_fits %>%
		dplyr::filter(term == "year")
	

	in_paper <- tibble(
		model = 0,
		term = "year",
		estimate =  0.0029634,
		std.error = 0.0001989,
		statistic = NA,
		p.value = NA,
		conf.low =   0.002571493,
		conf.high = 0.003355326)
	
	gaus_proj_yr_ssp585_fits <- bind_rows(in_paper, gaus_proj_yr_ssp585_fits)
	
	gaus_proj_yr_ssp585_fits$model[gaus_proj_yr_ssp585_fits$model == "0"] <- "in paper"

	# make plot
	coef_plot_gaus_proj_ssp585 <- 
		secret_weapon(gaus_proj_yr_ssp585_fits, var = "year") +
	  xlab("Coefficient estimate") + 
		ylab("Iteration") +
		scale_color_grey() +
		scale_x_continuous(
			breaks = c(0.003, 0.004),
			labels = c(0.003, 0.004)
		) +
		ggtitle("c)") +
		theme_bw() +
		theme(legend.position = "none",
					axis.text.y = element_blank(),
					axis.title.y = element_blank(),
					axis.ticks.y = element_blank())
	
	# plot together 
	
 plot1 <- coef_plot_gaus_hind 

 plot2 <- coef_plot_gaus_proj_ssp126

 plot3 <- coef_plot_gaus_proj_ssp585 


 coef_plots_gaus <- plot1 + plot2 + plot3 


 ggsave("./scripts/manuscript figure scripts/used in ms/pngs of figs/coef_plots_gaus.tiff",
			 coef_plots_gaus, dpi = 500)
	
	# plot time series 

	# time series
	ROMS_gaus_sens_hind_sum <- ROMS_gaus_sens_hind %>%
		group_by(year, iter) %>%
		summarise(mean_hbsuit = mean(sphabsuit_gaus)) %>%
		mutate(scenario = "hindcast") %>%
		mutate(iter = as.factor(iter))
	
	ROMS_gaus_sens_proj_sum <- ROMS_gaus_sens_proj %>%
		group_by(year, iter, scenario) %>%
		summarise(mean_hbsuit = mean(sphabsuit_gaus)) %>%
		mutate(iter = as.factor(iter))
	
	ROMS_sens_gaus_sum <- bind_rows(ROMS_gaus_sens_hind_sum, ROMS_gaus_sens_proj_sum)

	# add time series from manuscript
	yearly_habsuit_hind <- ROMS_hindcast_dat %>%
		group_by(year) %>%
    summarise(mean_sp_hab_suit = mean(sp_hab_suit)) %>%
		mutate(scenario = "hindcast")
	
	years_proj <- 2020:2099
	
	yearly_habsuit_proj <- ROMS_projected_dat %>% 
		filter(year %in% years_proj) %>%
		group_by(projection, year) %>%
   	summarise(mean_sp_hab_suit = mean(sp_hab_suit_var)) 
	
	yearly_habsuit_proj$scenario <- NA
		
	yearly_habsuit_proj$scenario[yearly_habsuit_proj$projection == "ssp126"] <- "ssp126"
	yearly_habsuit_proj$scenario[yearly_habsuit_proj$projection == "ssp585"] <- "ssp585"

	sens_gaus_plot <-
		ggplot() +
		geom_line(data = ROMS_sens_gaus_sum,
							aes(year, mean_hbsuit, 
									group = iter, 
									color = iter)) +
		geom_line(data = yearly_habsuit_hind,
					  	aes(year, mean_sp_hab_suit), 
								color = "black") +
		geom_line(data = yearly_habsuit_proj,
						 aes(year, mean_sp_hab_suit), 
								color = "black") +
		facet_wrap(~ scenario, scales = "free") +
		xlab("Year") + 
		ylab("Thermal spawning\nhabitat suitability") +
   	theme_bw()
   		
	 ggsave("./scripts/manuscript figure scripts/used in ms/pngs of figs/sens_gaus_plot_SI.tiff",
			 sens_gaus_plot, dpi = 500)

