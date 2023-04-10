### area time series ####
	
	## hindcast ##
	
	# core habitat = sum of area where sps >= 0.9

	c_area_hind_dat <- ROMS_hindcast_dat %>%
		filter(sp_hab_suit >= 0.9) 
	
	c_area_hind_dat_sum <- c_area_hind_dat %>%
		group_by(latitude, longitude, year) %>%
		distinct(across(c(latitude, longitude)), .keep_all = TRUE)

	c_area_hind_dat_sum_yr <- c_area_hind_dat_sum %>%
		group_by(year) %>%
		summarize(area = sum(area_km2)) %>% 
		mutate(sp_hab_threshold = "core")
	
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

	# join together 
	
	hind_area_yr <- bind_rows(c_area_hind_dat_sum_yr, p_area_hind_dat_sum_yr)

	## projections ##
	
	# remove historical years for plotting for presentations
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
		mutate(sp_hab_threshold = "core")

	# potential habitat = sum of area where sps >= 0.5
	
	p_area_proj_dat <- ROMS_projected_dat_proj %>%
		filter(sp_hab_suit_var >= 0.5) 

	p_area_proj_dat_sum <- p_area_proj_dat %>%
		group_by(simulation, projection, latitude, longitude, year) %>%
		distinct(across(c(latitude, longitude)), .keep_all = TRUE)
	
	p_area_proj_dat_sum_yr <- p_area_proj_dat_sum %>%
		group_by(simulation, projection, year) %>%
		summarize(area = sum(area_km2)) %>% ## avg per cell across a given time period
		mutate(sp_hab_threshold = "potential")
	
	# join together
	
	proj_area_yr <- bind_rows(c_area_proj_dat_sum_yr, p_area_proj_dat_sum_yr)
	
	## rolling means of area ##
	
	# low emission
	hind_area_yr_core <- hind_area_yr %>%
		filter(sp_hab_threshold == "core")
	
	hind_area_yr_pot <- hind_area_yr %>%
		filter(sp_hab_threshold == "potential") 
	
	proj_area_yr_sum <- proj_area_yr %>%
		group_by(year, projection, sp_hab_threshold) %>%
		summarise(area = mean(area))
	
	proj_area_yr_sum_core <- proj_area_yr_sum %>%
		filter(sp_hab_threshold == "core")
	
	proj_area_yr_sum_pot <- proj_area_yr_sum %>%
		filter(sp_hab_threshold == "potential")

	proj_area_yr_sum_core_low <- proj_area_yr_sum_core %>%
		filter(projection == "ssp126") 
	
	proj_area_yr_sum_pot_low <- proj_area_yr_sum_pot %>%
		filter(projection == "ssp126")
	
	proj_area_yr_sum_core_low_rm <- bind_rows(proj_area_yr_sum_core_low, hind_area_yr_core) %>%
		mutate(scen = "low")
	
	proj_area_yr_sum_pot_low_rm <- bind_rows(proj_area_yr_sum_pot_low, hind_area_yr_pot) %>%
		mutate(scen = "low")
	
	means_proj_core_low <- NA
	means_proj_pot_low <- NA

  for(i in 5:130){
  	win <- (i - 4):(i + 4)
  	means_proj_core_low[i] <- mean(proj_area_yr_sum_core_low_rm$area[win])
  }
	
	for(i in 5:130){
  	win <- (i - 4):(i + 4)
  	means_proj_pot_low[i] <- mean(proj_area_yr_sum_pot_low_rm$area[win])
  }
	
	
	proj_area_yr_sum_core_high <- proj_area_yr_sum_core %>%
		filter(projection == "ssp585") 
	
	proj_area_yr_sum_pot_high <- proj_area_yr_sum_pot %>%
		filter(projection == "ssp585")
	
	proj_area_yr_sum_core_high_rm <- bind_rows(proj_area_yr_sum_core_high, hind_area_yr_core) %>%
		mutate(scen = "high")
	
	proj_area_yr_sum_pot_high_rm <- bind_rows(proj_area_yr_sum_pot_high, hind_area_yr_pot) %>%
		mutate(scen = "high")

	means_proj_core_high <- NA
	means_proj_pot_high <- NA

  
  for(i in 5:130){
  	win <- (i - 4):(i + 4)
  	means_proj_core_high[i] <- mean(proj_area_yr_sum_core_high_rm$area[win])
  }
	
	for(i in 5:130){
  	win <- (i - 5):(i + 5)
  	means_proj_pot_high[i] <- mean(proj_area_yr_sum_pot_high_rm$area[win])
  }
	
	years_proj <- c(1970:2099) 
	
	core <- rep("core", 130)
	potential <- rep("potential", 130)
	low <- rep("low emission\n(ssp126)", 130)
	high <- rep("high emission\n(ssp585)", 130)


	core_low <- as.data.frame(cbind(means_proj_core_low, core, low, years_proj)) %>% 
		rename(area = means_proj_core_low, 
					 sp_hab_threshold = core,
					 projection = low,
					 year = years_proj)
	
	core_high <- as.data.frame(cbind(means_proj_core_high, core, high, years_proj)) %>%
		rename(area = means_proj_core_high, 
					 sp_hab_threshold = core,
					 projection = high,
					 year = years_proj)

	pot_low <- as.data.frame(cbind(means_proj_pot_low, potential, low, years_proj)) %>% 
		rename(area = means_proj_pot_low, 
					 sp_hab_threshold = potential,
					 projection = low,
					 year = years_proj)
	
	pot_high <- as.data.frame(cbind(means_proj_pot_high, potential, high, years_proj)) %>%
		rename(area = means_proj_pot_high, 
					 sp_hab_threshold = potential,
					 projection = high,
					 year = years_proj)

	
	rolling_area_proj <- rbind(core_low, core_high,
														 pot_low, pot_high)

	rolling_area_proj$area <- as.numeric(rolling_area_proj$area)
	rolling_area_proj$year <- as.numeric(rolling_area_proj$year)

	
	# for plotting by scenario
		
	proj_area_yr <- proj_area_yr %>% filter(., projection != "historical")
		
	proj_area_yr$scen <- NA
		
	proj_area_yr$scen[proj_area_yr$projection == "ssp126"] <- "low emission\n(ssp126)"
	proj_area_yr$scen[proj_area_yr$projection == "ssp585"] <- "high emission\n(ssp585)"
	
	rolling_area_proj$scen <- NA
		
	rolling_area_proj$scen <- rolling_area_proj$projection 
	rolling_area_proj$scen <- rolling_area_proj$projection 

	proj_area_yr <- tidyr::unite(proj_area_yr,"sim_proj",
															 simulation, projection, remove = F)

	colors <- c("#6dc3a9", "#ff7373", # cesm low, cesm high
						  "#4e8d9c", "#ff4040", # gfdl low, gfdl high
						  "#97c3e5", "#ffb733") # miroc low, miroc high

	sim_proj <- unique(proj_area_yr$sim_proj)
	
	names(colors) <- unique(proj_area_yr$sim_proj)
	
	# order factors for plotting
	proj_area_yr$scen_f = factor(proj_area_yr$scen, 
													levels=c('low emission\n(ssp126)', 'high emission\n(ssp585)'))
	
	rolling_area_proj$scen_f = factor(rolling_area_proj$scen, 
																	levels=c('low emission\n(ssp126)', 'high emission\n(ssp585)'))
	
	#### area plots ####
	
	area_plot <-    
   	ggplot(data = proj_area_yr, aes(year, area)) +
		geom_line(data = rolling_area_proj, 
   						aes(x = year, y = area), 
   						color = light_black) +
	 	geom_line(aes(year, area), alpha = 0.3,
            data = hind_area_yr %>% filter(sp_hab_threshold == "potential"), color = "black") +
		geom_line(data = proj_area_yr, 
							aes(year, area, color = sim_proj, group = sim_proj), alpha = 0.5) +
		facet_grid(sp_hab_threshold ~ scen_f) +
	  geom_line(aes(year, area, colour = sp_hab_threshold), alpha = 0.3,
            data = hind_area_yr %>% filter(sp_hab_threshold == "core"), color = "black") +
		xlab("Year") +
		scale_color_manual(name = "sim_proj", values = colors) +
	  scale_y_continuous(
	  	name =	"Area (x 10<sup>5</sup> km<sup>2</sup>)",
	  	breaks = c(100000, 300000, 500000),
	  	labels = c(1,3,5),
	  	limits = c(19000, 567000)) +
   	xlim(1970, 2110) +
		geom_segment(x = 2020, y = 19000, xend = 2020, yend = 480000,
								 color = "lightgrey", size = 0.5) +
		time_series_plot_theme() +
		theme(axis.text.x = element_blank())
	
