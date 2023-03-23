# test for 0 slope

	#### temperature ####
	
	# hindcast
	
	yearly_temp_hind <- ROMS_hindcast_dat %>%
		group_by(year) %>%
    summarise(avg_temp = mean(temp))

	hind_temp_lm <- lm(avg_temp ~ year, data = yearly_temp_hind)
	confint(hind_temp_lm)
	summary(hind_temp_lm)
	
	range(yearly_temp_hind$avg_temp)
	
	max(yearly_temp_hind$avg_temp) - min(yearly_temp_hind$avg_temp)
	
	# projection
	years_proj <- 2020:2099

	yearly_temp_proj <- ROMS_projected_dat %>% 
		filter(year %in% years_proj) %>%
		group_by(simulation, projection, year) %>%
   	summarise(avg_temp = mean(bc_temp_sd)) 

	proj_temp_lm <- lme4::lmer(avg_temp ~ year * projection + (1 | simulation), data = yearly_temp_proj)
	confint(proj_temp_lm)
	summary(proj_temp_lm)
	
	# mid century temp
	temp_proj_sum <- ROMS_projected_dat %>%
	 group_by(month, year, simulation, projection) %>%
	 summarize(mean_temp = mean(bc_temp_sd))

	temp_proj_sum_ssp126 <- temp_proj_sum %>%
		filter(projection == "SSP126")

	temp_proj_sum_ssp585 <- temp_proj_sum %>%
		filter(projection == "SSP585")

	temp2050_low <- temp_proj_sum_ssp126 %>%
		filter(year == 2050) %>%
		summarize(mean_temp = mean(mean_temp))
	
	temp2050_low <- max(temp2050_low$mean_temp)
	
	temp2050_high <- temp_proj_sum_ssp585 %>%
		filter(year == 2050) %>%
		summarize(mean_temp = mean(mean_temp))
	
	temp2050_high <- max(temp2050_high$mean_temp)
	
	
	temp2099_low <- temp_proj_sum_ssp126 %>%
		filter(year == 2099) %>%
		summarize(mean_temp = mean(mean_temp))
	
	temp2099_low <- max(temp2099_low$mean_temp)
	
	temp2099_high <- temp_proj_sum_ssp585 %>%
		filter(year == 2099) %>%
		summarize(mean_temp = mean(mean_temp))
	
	temp2099_high <- max(temp2099_high$mean_temp)


  current_temp <- ROMS_hindcast_dat %>%
	 group_by(year) %>%
	 summarise(mean_temp = mean(temp)) %>%
   filter(year == 2020)
  
  past_temp <- ROMS_hindcast_dat %>%
	 group_by(year) %>%
	 summarise(mean_temp = mean(temp)) %>%
   filter(year == 1970)
  
  temp2050_low - current_temp$mean_temp
  temp2050_low - past_temp$mean_temp
  
  temp2050_high - current_temp$mean_temp
  temp2050_high - past_temp$mean_temp
  
  temp2099_low - current_temp$mean_temp
  temp2099_low - past_temp$mean_temp

  temp2099_high - current_temp$mean_temp
  temp2099_high - past_temp$mean_temp
 
  
	#### spawning habitat suitability ####
  yearly_hab_dat_hind <- ROMS_hindcast_dat %>%
		group_by(year) %>%
    summarise(mean_hab_suit = mean(sp_hab_suit))
	
	years_proj <- 2020:2099

	yearly_hab_dat_proj <- ROMS_projected_dat %>% 
		filter(year %in% years_proj) %>%
		group_by(simulation, projection, year) %>%
   	summarise(mean_hab_suit = mean(sp_hab_suit_var))

	max(yearly_hab_dat_hind$mean_hab_suit) - min(yearly_hab_dat_hind$mean_hab_suit)
  
  range(yearly_hab_dat_hind$mean_hab_suit)
  
  sphabsuit1970 <- yearly_hab_dat_hind %>%
  	filter(year == 1970)
  
  sphabsuit2020 <- yearly_hab_dat_hind %>%
  	filter(year == 2020)
  
  sphabsuit2020$mean_hab_suit - sphabsuit1970$mean_hab_suit
  
	# hindcast
	hind_habsuit_lm <- lm(mean_hab_suit ~ year, data = yearly_hab_dat_hind)
	confint(hind_habsuit_lm)
	summary(hind_habsuit_lm)

	# projection
	proj_habsuit_lm <- lme4::lmer(mean_hab_suit ~ year * projection + (1 | simulation), 
																data = yearly_hab_dat_proj)
	confint(proj_habsuit_lm)
	summary(proj_habsuit_lm)

	
	# change to sp hab suit
	sphabsuit_proj_sum <- ROMS_projected_dat %>%
	 group_by(month, year, simulation, projection) %>%
	 summarize(mean_shs = mean(sp_hab_suit_var))

	sphabsuit_proj_sum_ssp126 <- sphabsuit_proj_sum %>%
		filter(projection == "SSP126")

	sphabsuit_proj_sum_ssp585 <- sphabsuit_proj_sum %>%
		filter(projection == "SSP585")

	shs2050_low <- sphabsuit_proj_sum_ssp126 %>%
		filter(year == 2050) %>%
		summarize(mean_shs = mean(mean_shs))
	
	shs2050_low <- max(shs2050_low$mean_shs)
	
	shs2050_high <- sphabsuit_proj_sum_ssp585 %>%
		filter(year == 2050) %>%
		summarize(mean_shs = mean(mean_shs))
	
	shs2050_high <- max(shs2050_high$mean_shs)
	
	
	shs2099_low <- sphabsuit_proj_sum_ssp126 %>%
		filter(year == 2099) %>%
		summarize(mean_shs = mean(mean_shs))
	
	shs2099_low <- max(shs2099_low$mean_shs)
	
	shs2099_high <- sphabsuit_proj_sum_ssp585 %>%
		filter(year == 2099) %>%
		summarize(mean_shs = mean(mean_shs))
	
	shs2099_high <- max(shs2099_high$mean_shs)


  current_shs <- ROMS_hindcast_dat %>%
	 group_by(year) %>%
	 summarise(mean_shs = mean(sp_hab_suit)) %>%
   filter(year == 2020)
  
  past_shs <- ROMS_hindcast_dat %>%
	 group_by(year) %>%
	 summarise(mean_shs = mean(sp_hab_suit)) %>%
   filter(year == 1970)
  
  shs2050_low - current_shs$mean_shs
  shs2050_low - past_shs$mean_shs
  
  shs2050_high - current_shs$mean_shs
  shs2050_high - past_shs$mean_shs
  
  shs2099_low - current_shs$mean_shs
  shs2099_low - past_shs$mean_shs

  shs2099_high - current_shs$mean_shs
  shs2099_high - past_shs$mean_shs
  
  # % increase
  ((shs2050_low - past_shs$mean_shs)/past_shs$mean_shs) * 100
 
  ((shs2050_high - past_shs$mean_shs)/past_shs$mean_shs) * 100
	
	
	#### area ####
	c_area_hind_dat <- ROMS_hindcast_dat %>%
		filter(sp_hab_suit >= 0.9) 
	
	c_area_hind_dat_sum <- c_area_hind_dat %>%
		group_by(latitude, longitude, year) %>%
		distinct(across(c(latitude, longitude)), .keep_all = TRUE)

	c_area_hind_dat_sum_yr <- c_area_hind_dat_sum %>%
		group_by(year) %>%
		summarize(area = sum(area_km2)) %>% 
		mutate(sp_hab_threshold = "core",
					 area_scl = rescale(area))
	
	# potential habitat = sum of area where sps >= 0.5
	
	p_area_hind_dat <- ROMS_hindcast_dat %>%
		filter(sp_hab_suit >= 0.5) 
	
	p_area_hind_dat_sum <- p_area_hind_dat %>%
		group_by(latitude, longitude, year) %>%
		distinct(across(c(latitude, longitude)), .keep_all = TRUE)

	p_area_hind_dat_sum_yr <- p_area_hind_dat_sum %>%
		group_by(year) %>%
		summarize(area = sum(area_km2)) %>% ## avg per cell across a given time period
		mutate(sp_hab_threshold = "potential",
					 area_scl = rescale(area))


	# hindcast core
	core_area_hind_lm <- lm(area_scl ~ year, data = c_area_hind_dat_sum_yr)
	confint(core_area_hind_lm)
	summary(core_area_hind_lm)

	# hindcast potential
	pot_area_hind_lm <- lm(area_scl ~ year, data = p_area_hind_dat_sum_yr)
	confint(pot_area_hind_lm)
	summary(pot_area_hind_lm)

	# projection core
	
	years_proj <- 2021:2099
	
	ROMS_projected_dat_proj <- ROMS_projected_dat %>%
		filter(year %in% years_proj)
	
	# with bias-corrected temperature using variance ratio
	
	c_area_proj_dat <- ROMS_projected_dat_proj %>%
		filter(sp_hab_suit_var >= 0.9) 

	c_area_proj_dat_sum <- c_area_proj_dat %>%
		group_by(simulation, projection, latitude, longitude, year) %>%
		distinct(across(c(latitude, longitude)), .keep_all = TRUE)
	
	c_area_proj_dat_sum_yr <- c_area_proj_dat_sum %>%
		group_by(simulation, projection, year) %>%
		summarize(area = sum(area_km2)) %>% ## avg per cell across a given time period
		mutate(sp_hab_threshold = "core",
					 area_scl = rescale(area))


	# potential habitat = sum of area where sps >= 0.5
	p_area_proj_dat <- ROMS_projected_dat_proj %>%
		filter(sp_hab_suit_var >= 0.5) 

	p_area_proj_dat_sum <- p_area_proj_dat %>%
		group_by(simulation, projection, latitude, longitude, year) %>%
		distinct(across(c(latitude, longitude)), .keep_all = TRUE)
	
	p_area_proj_dat_sum_yr <- p_area_proj_dat_sum %>%
		group_by(simulation, projection, year) %>%
		summarize(area = sum(area_km2)) %>% ## avg per cell across a given time period
		mutate(sp_hab_threshold = "potential",
					 area_scl = rescale(area))
	
	# models
	core_area_proj_lm <- lme4::lmer(area_scl ~ year * projection + (1 | simulation), 
																 data = c_area_proj_dat_sum_yr)
	confint(core_area_proj_lm)
	summary(core_area_proj_lm)

	# projection potential
	pot_area_proj_lm <- lme4::lmer(area_scl ~ year * projection + (1 | simulation), 
																 data = p_area_proj_dat_sum_yr)
	confint(pot_area_proj_lm)
	summary(pot_area_proj_lm)
	
	
	### area increase
	#### hindcasts ####
	
	c_area_hind_dat <- ROMS_hindcast_dat %>%
		filter(sp_hab_suit >= 0.9) 
	
	c_area_hind_dat_sum <- c_area_hind_dat %>%
		group_by(latitude, longitude, year) %>%
		distinct(across(c(latitude, longitude)), .keep_all = TRUE)

	c_area_hind_dat_sum_yr <- c_area_hind_dat_sum %>%
		group_by(year) %>%
		summarize(area = sum(area_km2)) %>% 
		mutate(sp_hab_threshold = "core")
	
	range(c_area_hind_dat_sum_yr$area)
	max(c_area_hind_dat_sum_yr$area) - min(c_area_hind_dat_sum_yr$area)

  c_area_hind <- c_area_hind_dat_sum_yr %>% filter(year == 1970)
  c_area_hind <- c_area_hind$area
 
	# potential habitat = sum of area where sps >= 0.5
	
	p_area_hind_dat <- ROMS_hindcast_dat %>%
		filter(sp_hab_suit >= 0.5) 
	
	p_area_hind_dat_sum <- p_area_hind_dat %>%
		group_by(latitude, longitude, year) %>%
		distinct(across(c(latitude, longitude)), .keep_all = TRUE)

	p_area_hind_dat_sum_yr <- p_area_hind_dat_sum %>%
		group_by(year) %>%
		summarize(area = sum(area_km2)) %>% ## avg per cell across a given time period
		mutate(sp_hab_threshold = "potential")
	
	p_area_hind <- p_area_hind_dat_sum_yr %>% filter(year == 1970)
	p_area_hind <- p_area_hind$area
 
 # core projected area
	years_proj <- 2021:2099
	
	ROMS_projected_dat_proj <- ROMS_projected_dat %>%
		filter(year %in% years_proj)
	
	# with bias-corrected temperature using variance ratio
	
	c_area_proj_dat <- ROMS_projected_dat_proj %>%
		filter(sp_hab_suit_var >= 0.9) 

	c_area_proj_dat_sum <- c_area_proj_dat %>%
		group_by(projection, latitude, longitude, year) %>%
		distinct(across(c(latitude, longitude)), .keep_all = TRUE)
	
	c_area_proj_dat_sum_yr <- c_area_proj_dat_sum %>%
		group_by(projection, year) %>%
		summarize(area = sum(area_km2)) %>% ## avg per cell across a given time period
		mutate(sp_hab_threshold = "core")
	
 # 2050
	c_area_2050 <- c_area_proj_dat_sum %>%
		group_by(projection, year) %>%
		summarize(area = sum(area_km2)) %>% ## avg per cell across a given time period
		mutate(sp_hab_threshold = "core") %>%
		filter(year == 2050)

	c_area_2050_low <- c_area_2050 %>% filter(projection == "ssp126")
	c_area_2050_low <- c_area_2050_low$area
	
	c_area_2050_low - c_area_hind
	
	c_area_2050_high <- c_area_2050 %>% filter(projection == "ssp585")
	c_area_2050_high <- c_area_2050_high$area
	
	c_area_2050_high - c_area_hind
	
	# 2099 
		
	c_area_2099 <- c_area_proj_dat_sum %>%
		group_by(projection, year) %>%
		summarize(area = sum(area_km2)) %>% ## avg per cell across a given time period
		mutate(sp_hab_threshold = "core") %>%
		filter(year == 2099)
	
	c_area_2099_low <- c_area_2099 %>% filter(projection == "ssp126")
	c_area_2099_low <- c_area_2099_low$area
	
	c_area_2099_low - c_area_2050_low
	
	c_area_2099_high <- c_area_2099 %>% filter(projection == "ssp585")
	c_area_2099_high <- c_area_2099_high$area
	
	c_area_2099_high - c_area_2050_high


	# potential projected area

	p_area_proj_dat <- ROMS_projected_dat_proj %>%
		filter(sp_hab_suit_var >= 0.5) 

	p_area_proj_dat_sum <- p_area_proj_dat %>%
		group_by(projection, latitude, longitude, year) %>%
		distinct(across(c(latitude, longitude)), .keep_all = TRUE)
	
	p_area_proj_dat_sum_yr <- p_area_proj_dat_sum %>%
		group_by(projection, year) %>%
		summarize(area = sum(area_km2)) %>% ## avg per cell across a given time period
		mutate(sp_hab_threshold = "potential")
	
	range(p_area_hind_dat_sum_yr$area)
	max(p_area_hind_dat_sum_yr$area) - min(p_area_hind_dat_sum_yr$area)
	
 # 2050
	p_area_2050 <- p_area_proj_dat_sum %>%
		group_by(projection, year) %>%
		summarize(area = sum(area_km2)) %>% ## avg per cell across a given time period
		mutate(sp_hab_threshold = "potential") %>%
		filter(year == 2050)

	p_area_2050_low <- p_area_2050 %>% filter(projection == "ssp126")
	p_area_2050_low <- p_area_2050_low$area
	
	p_area_2050_low - p_area_hind
	
	p_area_2050_high <- p_area_2050 %>% filter(projection == "ssp585")
	p_area_2050_high <- p_area_2050_high$area
	
	p_area_2050_high - p_area_hind
	
	# 2099 
		
	p_area_2099 <- p_area_proj_dat_sum %>%
		group_by(projection, year) %>%
		summarize(area = sum(area_km2)) %>% ## avg per cell across a given time period
		mutate(sp_hab_threshold = "potential") %>%
		filter(year == 2099)
	
	p_area_2099_low <- p_area_2099 %>% filter(projection == "ssp126")
	p_area_2099_low <- p_area_2099_low$area
	
	p_area_2099_low - p_area_2050_low
	
	p_area_2099_high <- p_area_2099 %>% filter(projection == "ssp585")
	p_area_2099_high <- p_area_2099_high$area
	
	p_area_2099_high - p_area_2050_high
	
	
	
	#### mean latitude ####
	
		hind_mean_lat_yr <- function(x){
		
		new_dat <- ROMS_hindcast_dat %>%
			filter(., sp_hab_suit >= x)
		
		new_dat_sum <- new_dat %>%
			group_by(year) %>%
			summarise(hist_mean_lat = mean(latitude)) 

		new_dat_sum		
	}
	
	sp_hab_thresholds <- c(0.5, 0.9)
	
	hind_mean_lats_yr <- lapply(sp_hab_thresholds, hind_mean_lat_yr)
	
	hind_mean_lats_yr_0.5 <- hind_mean_lats_yr[[1]] %>%
		mutate(sp_hab_threshold = 0.5)

	hind_mean_lats_yr_0.9 <- hind_mean_lats_yr[[2]]	%>%
		mutate(sp_hab_threshold = 0.9)
	

	# hindcast core
	hind_meanlat_core_lm <- lm(hist_mean_lat ~ year, data = hind_mean_lats_yr_0.9)
	confint(hind_meanlat_core_lm)
	summary(hind_meanlat_core_lm)
	
	# hindcast potential
	hind_meanlat_pot_lm <- lm(hist_mean_lat ~ year, data = hind_mean_lats_yr_0.5)
	confint(hind_meanlat_pot_lm)
	summary(hind_meanlat_pot_lm)

	# projection core
	mean_lat_proj_core <- mean_lat_proj %>%
		filter(sp_hab_threshold == 0.9)
	
	proj_meanlat_core_lm <- lme4::lmer(mean_lat ~ year * projection + (1 | simulation), 
																data = mean_lat_proj_core)
	confint(proj_meanlat_core_lm)
	summary(proj_meanlat_core_lm)
	
	# projection potential
	mean_lat_proj_pot <- mean_lat_proj %>%
		filter(sp_hab_threshold == 0.5)
	
	proj_meanlat_pot_lm <- lme4::lmer(mean_lat ~ year * projection + (1 | simulation), 
																data = mean_lat_proj_pot)
	confint(proj_meanlat_pot_lm)
	summary(proj_meanlat_pot_lm)

	
	#### try again
	
		proj_mean_lat_yr <- function(x){
		
		new_dat <- ROMS_projected_dat %>%
			filter(., sp_hab_suit_var >= x)
		
		new_dat_sum <- new_dat %>%
			group_by(simulation, projection, year) %>%
			summarise(proj_mean_lat = mean(latitude)) 

		new_dat_sum		
	}
	
	sp_hab_thresholds <- c(0.5, 0.9)
	
	proj_mean_lat_yr <- lapply(sp_hab_thresholds, proj_mean_lat_yr)
	
	proj_mean_lats_yr_0.5 <- proj_mean_lat_yr[[1]] %>%
		mutate(sp_hab_threshold = 0.5)

	proj_mean_lats_yr_0.9 <- proj_mean_lat_yr[[2]]	%>%
		mutate(sp_hab_threshold = 0.9)
	
	proj_mean_lat_yr <- bind_rows(proj_mean_lats_yr_0.5, proj_mean_lats_yr_0.9) 
	

	years_proj <- c(2020:2099)
	proj_mean_lat_yr <- proj_mean_lat_yr %>%
		filter(year %in% years_proj)
	
	mean_lat_proj <- proj_mean_lat_yr %>%
		group_by(year, projection, simulation, sp_hab_threshold) %>%
		summarize(mean_lat = mean(proj_mean_lat))

	mean_lat_proj_core <- mean_lat_proj %>%
		filter(sp_hab_threshold == 0.9)
	
	proj_meanlat_core_lm <- lme4::lmer(mean_lat ~ year * projection + (1 | simulation), 
																data = mean_lat_proj_core)
	confint(proj_meanlat_core_lm)
	summary(proj_meanlat_core_lm)
	
	# projection potential
	mean_lat_proj_pot <- mean_lat_proj %>%
		filter(sp_hab_threshold == 0.5)
	
	proj_meanlat_pot_lm <- lme4::lmer(mean_lat ~ year * projection + (1 | simulation), 
																data = mean_lat_proj_pot)
	confint(proj_meanlat_pot_lm)
	summary(proj_meanlat_pot_lm)
