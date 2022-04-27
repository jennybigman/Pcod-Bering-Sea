## FIGURE -- all time series 

	# time series plot theme function  
	time_series_plot_theme <- function(plot_tag_x = 0.04, 
																		 plot_tag_y = 0.9, 
																		 plot_title_size = 20, 
																		 axis_text_size = 16){
			theme(
  				legend.position = "none",
  				panel.background = element_rect(fill = NA),
  				plot.title = element_text(size = plot_title_size, face = "bold", hjust = 0.5),
					axis.text = element_text(size = axis_text_size),
  			  axis.ticks.y = element_line(),
					axis.ticks.x = element_blank(),
  			  axis.line = element_blank(),
  			  axis.title =  element_blank(),
  			  panel.grid.major = element_blank(),
  			  panel.grid.minor = element_blank(),
  			  panel.border = element_rect(fill = NA, color = "grey50"),
					plot.tag.position = c(plot_tag_x, plot_tag_y))
	}

	light_black <- "#306364"
	
	
	#### temperature time series ####

	# temp data
	
	yearly_temp_hind <- ROMS_hindcast_dat %>%
		group_by(year) %>%
    summarise(avg_temp = mean(temp)) 
	
	yearly_temp_hind$avg_temp <- as.numeric(yearly_temp_hind$avg_temp)
	
	years_proj <- 2020:2099
	
	yearly_temp_proj <- ROMS_projected_dat %>% 
		filter(year %in% years_proj) %>%
		group_by(simulation, projection, year) %>%
   	summarise(avg_temp = mean(bc_temp_sd)) 
	
	yearly_temp_proj$scen <- NA
		
	yearly_temp_proj$scen[yearly_temp_proj$projection == "ssp126"] <- "low emission (ssp126)"
	yearly_temp_proj$scen[yearly_temp_proj$projection == "ssp585"] <- "high emission (ssp585)"
	
	yearly_temp_proj <- tidyr::unite(yearly_temp_proj,"sim_proj",
															 simulation, projection, remove = F)

	colors <- c("#6dc3a9", "#ff7373", # cesm low, cesm high
						  "#4e8d9c", "#ff4040", # gfdl low, gfdl high
						  "#97c3e5", "#ffb733") # miroc low, miroc high

	sim_proj <- unique(yearly_temp_proj$sim_proj)
	
	names(colors) <- unique(yearly_temp_proj$sim_proj)
	
	# order facets
	yearly_temp_proj$scen_f = factor(yearly_temp_proj$scen, 
																	 levels=c('low emission (ssp126)', 
																						'high emission (ssp585)'))

	# rolling means of temp
	
	# hind
	means_hind <- NA
  
  for(i in 6:51){
  	win <- (i - 5):(i + 5)
  	means_hind[i] <- mean(yearly_temp_hind$avg_temp[win])
  }
  
	years_hind <- c(1970:2020) # does this need to be 1980?
	
	rolling_mean_temp_hind <- as.data.frame(cbind(years_hind, means_hind)) 
	
	# proj
	yearly_temp_proj_sum <- yearly_temp_proj %>%
		group_by(year, scen_f) %>%
		summarise(mean_temp = mean(avg_temp))
	
	yearly_temp_proj_sum_low <- yearly_temp_proj_sum %>%
		filter(scen_f == "low emission (ssp126)")
	
	means_proj_low <- NA
  
  for(i in 6:80){
  	win <- (i - 5):(i + 5)
  	means_proj_low[i] <- mean(yearly_temp_proj_sum_low$mean_temp[win])
  }
	
	yearly_temp_proj_sum_high <- yearly_temp_proj_sum %>%
		filter(scen_f == "high emission (ssp585)")
	
	means_proj_high <- NA
  
  for(i in 6:80){
  	win <- (i - 5):(i + 5)
  	means_proj_high[i] <- mean(yearly_temp_proj_sum_high$mean_temp[win])
  }
	
	years_proj <- c(2020:2099) # does this need to be 1980?
	
	rolling_mean_temp_proj <- as.data.frame(cbind(years_proj, means_proj_low, means_proj_high)) 

	#### temp plots ####
	
	# low emissions #
	
	low_temp_proj <- yearly_temp_proj %>%
		filter(., scen == "low emission (ssp126)")
	
	low_temp_time <-    
   	ggplot() +
		geom_line(data = rolling_mean_temp_hind, 
   						aes(x = years_hind, y = means_hind), 
   						color = light_black) +
		geom_line(data = rolling_mean_temp_proj, 
   						aes(x = years_proj, y = means_proj_low), 
   						color = light_black) +
   	geom_line(data = yearly_temp_hind, 
   						aes(x = year, y = avg_temp), 
   						color = "grey50", alpha = 0.3) +
		geom_line(data = low_temp_proj,
							aes(year, avg_temp, 
									group = sim_proj, 
									color = sim_proj), alpha = 0.3) +
		xlab("Year") + 
		scale_color_manual(name = "sim_proj", values = colors) +
	  scale_y_continuous(
	  	name = "Temperature (˚C)",
	  	breaks = c(-1, 1, 3),
	  	limits = c(-1, 4)
	  ) +
		labs(tag = "(a)") +
		geom_vline(aes(xintercept = 2020, color = "#f2f2f2"), alpha = 0.5) +
		scale_x_continuous(
	  	name = "Year",
	  	breaks = c(1980, 2030, 2080),
	  		  	limits = c(1970, 2110)) +
		ggtitle("low emission (ssp126)") +
  	time_series_plot_theme() +
		theme(axis.text.x = element_blank())
	
		low_temp_time_lab <- low_temp_time +
			annotate(geom = "text", x = 2107, y = 1.9,
           label = "cesm",
           color = "#6dc3a9", size = 4.5) +
				annotate(geom = "text", x = 2107, y = 0.9,
           label = "gfdl",
           color = "#4e8d9c", size = 4.5) +
				annotate(geom = "text", x = 2107, y = 0.25,
           label = "miroc",
           color = "#97c3e5", size = 4.5)  +
				annotate(geom = "text", x = 1995, y = 1,
           label = "hindcast",
           color = "black", alpha = 0.5, size = 4.5)
		
	# high emissions #		
	high_temp_dat <- yearly_temp_proj %>%
		filter(., scen == "high emission (ssp585)")
	
	high_temp_time <-    
   	ggplot() +
		geom_line(data = rolling_mean_temp_hind, 
   						aes(x = years_hind, y = means_hind), 
   						color = light_black) +
		geom_line(data = rolling_mean_temp_proj, 
   						aes(x = years_proj, y = means_proj_high), 
   						color = light_black) +
   	geom_line(data = yearly_temp_hind, 
   						aes(x = year, y = avg_temp), 
   						color = "black", alpha = 0.3) +
		geom_line(data = high_temp_dat,
							aes(year, avg_temp, 
									group = sim_proj, 
									color = sim_proj), alpha = 0.3) +
		xlab("Year") +
		geom_vline(aes(xintercept = 2020, color = "#f2f2f2"), alpha = 0.5) +
		scale_color_manual(name = "sim_proj", values = colors) +
	  scale_y_continuous(
	  	position = "right",
	  	name = "Temperature (˚C)",
	  	breaks = c(-1, 1, 3),
	  	limits = c(-1, 4)
	  ) +
		scale_x_continuous(
	  	name = "Year",
	  	breaks = c(1980, 2030, 2080),
	  		  	limits = c(1970, 2110)) +
		ggtitle("high emission (ssp585)") +
		labs(tag = "(b)") +
   	time_series_plot_theme() +
		theme(axis.text.y = element_blank(),
					axis.ticks.y = element_blank(),
					axis.text.x = element_blank())
		
		high_temp_time_lab <- high_temp_time +
			annotate(geom = "text", x = 2107, y = 3.2,
           label = "cesm",
           color = "#ffabab", size = 4.5) +
				annotate(geom = "text", x = 2107, y = 2.2,
           label = "gfdl",
           color = "#ff4040", size = 4.5) +
				annotate(geom = "text", x = 2107, y = 3.5,
           label = "miroc",
           color = "#ffb733", size = 4.5) +
				annotate(geom = "text", x = 1994, y = 1,
           label = "hindcast",
           color = "black", alpha = 0.5, size = 4.5)
		

	#### spawning habitat suitability index time series ####
	
	yearly_hab_dat_hind <- ROMS_hindcast_dat %>%
		group_by(year) %>%
    summarise(mean_hab_suit = mean(sp_hab_suit))
	
	years_proj <- 2020:2099

	yearly_hab_dat_proj <- ROMS_projected_dat %>% 
		filter(year %in% years_proj) %>%
		group_by(simulation, projection, year) %>%
   	summarise(mean_hab_suit = mean(sp_hab_suit_var))

	yearly_hab_dat_proj$scen <- NA
		
	yearly_hab_dat_proj$scen[yearly_hab_dat_proj$projection == "ssp126"] <- "low emission (ssp126)"
	yearly_hab_dat_proj$scen[yearly_hab_dat_proj$projection == "ssp585"] <- "high emission (ssp585)"
	
	yearly_hab_dat_proj <- tidyr::unite(yearly_hab_dat_proj,"sim_proj",
															 simulation, projection, remove = F)

	colors <- c("#6dc3a9", "#ffabab", # cesm low, cesm high
						  "#4e8d9c", "#ff4040", # gfdl low, gfdl high
						  "#97c3e5", "#ffb733") # miroc low, miroc high

	sim_proj <- unique(yearly_hab_dat_proj$sim_proj)
	
	names(colors) <- unique(yearly_hab_dat_proj$sim_proj)
	
	# order facets
	yearly_hab_dat_proj$scen_f = factor(yearly_hab_dat_proj$scen, levels=c('low emission (ssp126)', 
																																																																						 'high emission (ssp585)'))
	## rolling means of spawning habitat suitability ##
	
	# hind
	means_hind <- NA
  
  for(i in 6:51){
  	win <- (i - 5):(i + 5)
  	means_hind[i] <- mean(yearly_hab_dat_hind$mean_hab_suit[win])
  }
  
	years_hind <- c(1970:2020) 
	
	rolling_mean_habsuit_hind <- as.data.frame(cbind(years_hind, means_hind)) 
	
	# proj
	yearly_hab_dat_proj_sum <- yearly_hab_dat_proj %>%
		group_by(year, scen_f) %>%
		summarise(mean_hab_suit = mean(mean_hab_suit))
	
	yearly_hab_dat_proj_sum_low <- yearly_hab_dat_proj_sum %>%
		filter(scen_f == "low emission (ssp126)")
	
	means_proj_low <- NA
  
  for(i in 6:80){
  	win <- (i - 5):(i + 5)
  	means_proj_low[i] <- mean(yearly_hab_dat_proj_sum_low$mean_hab_suit[win])
  }
	
	yearly_hab_dat_proj_sum_high <- yearly_hab_dat_proj_sum %>%
		filter(scen_f == "high emission (ssp585)")
	
	means_proj_high <- NA
  
  for(i in 6:80){
  	win <- (i - 5):(i + 5)
  	means_proj_high[i] <- mean(yearly_hab_dat_proj_sum_high$mean_hab_suit[win])
  }
	
	years_proj <- c(2020:2099) 
	
	rolling_mean_habsuit_proj <- as.data.frame(cbind(years_proj, means_proj_low, means_proj_high)) 
	
	#### hab suit plots ####
	
	# low emissions #
	low_dat <- yearly_hab_dat_proj %>%
		filter(., scen == "low emission (ssp126)")
	
	low_habsuit_time <-    
   	ggplot() +
		geom_line(data = rolling_mean_habsuit_hind, 
   						aes(x = years_hind, y = means_hind), 
   						color = light_black) +
		geom_line(data = rolling_mean_habsuit_proj, 
   						aes(x = years_proj, y = means_proj_low), 
   						color = light_black) +
   	geom_line(data = yearly_hab_dat_hind, 
   						aes(x = year, y = mean_hab_suit), 
   						color = "black", alpha = 0.3) +
		geom_line(data = low_dat,
							aes(year, mean_hab_suit, 
									group = sim_proj, 
									color = sim_proj), alpha = 0.3) +
		xlab("Year") + 
		geom_vline(aes(xintercept = 2020, color = "#f2f2f2"), alpha = 0.5) +
		scale_color_manual(name = "sim_proj", values = colors) +
	  scale_y_continuous(
	  	name = "Cross-shelf annual spawning\nhabitat suitability index",
	  	breaks = c(0.20, 0.40, 0.60),
	  	labels = c(0.20, 0.40, 0.60),
	  	limits = c(0.2, 0.65)
	  ) +
		scale_x_continuous(
	  	name = "Year",
	  	breaks = c(1980, 2030, 2080),
	  		  	limits = c(1970, 2110)) +
		labs(tag = "(c)") +
		time_series_plot_theme(plot_tag_x = 0.125, plot_tag_y = 0.9) +
		theme(axis.text.x = element_blank())
	
	low_habsuit_time_lab <- low_habsuit_time +
			annotate(geom = "text", x = 2107, y = 0.45,
           label = "cesm",
           color = "#6dc3a9", size = 4) +
				annotate(geom = "text", x = 2107, y = 0.30,
           label = "gfdl",
           color = "#4e8d9c", size = 4) +
				annotate(geom = "text", x = 2107, y = 0.37,
           label = "miroc",
           color = "#97c3e5", size = 4)  +
				annotate(geom = "text", x = 1995, y = 0.39,
           label = "hindcast",
           color = "black", alpha = 0.5, size = 4)

	high_dat <- yearly_hab_dat_proj %>%
		filter(., scen == "high emission (ssp585)")
	
	high_habsuit_time <-    
   	ggplot() +
		geom_line(data = rolling_mean_habsuit_hind, 
   						aes(x = years_hind, y = means_hind), 
   						color = light_black) +
		geom_line(data = rolling_mean_habsuit_proj, 
   						aes(x = years_proj, y = means_proj_high), 
   						color = light_black) +
   	geom_line(data = yearly_hab_dat_hind, 
   						aes(x = year, y = mean_hab_suit), 
   						color = "black", alpha = 0.3) +
		geom_line(data = high_dat,
							aes(year, mean_hab_suit, 
									group = sim_proj, 
									color = sim_proj), alpha = 0.3) +
		xlab("Year") + 
		geom_vline(aes(xintercept = 2020, color = "#f2f2f2"), alpha = 0.5) +
		scale_color_manual(name = "sim_proj", values = colors) +
	  scale_y_continuous(
	  	name = "Cross-shelf annual spawning\nhabitat suitability index",
	  	breaks = c(0.20, 0.40, 0.60),
	  	labels = c(0.20, 0.40, 0.60),
	  	limits = c(0.2, 0.65)
	  ) +
		scale_x_continuous(
	  	name = "Year",
	  	breaks = c(1980, 2030, 2080),
	  	limits = c(1970, 2110)) +
		labs(tag = "(d)") +
		time_series_plot_theme(plot_tag_x = 0.03, plot_tag_y = 0.9) +
		theme(axis.title.y = element_blank(),
					axis.text.y = element_blank(),
					axis.ticks.y = element_blank(),
				  axis.text.x = element_blank())
	
	high_habsuit_time_lab <- high_habsuit_time +
			annotate(geom = "text", x = 2107, y = 0.46,
           label = "cesm",
           color = "#ffabab", size = 4) +
				annotate(geom = "text", x = 2107, y = 0.5,
           label = "gfdl",
           color = "#ff4040", size = 4) +
				annotate(geom = "text", x = 2107, y = 0.62,
           label = "miroc",
           color = "#ffb733", size = 4) +
				annotate(geom = "text", x = 1994, y = 0.39,
           label = "hindcast",
           color = "black", alpha = 0.5, size = 4)
	
	#### area time series ####
	
	# calculate area 
	
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
	years_proj <- 2020:2099
	
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
	
	# hind
	hind_area_yr_core <- hind_area_yr %>%
		filter(., sp_hab_threshold == "core")
	
	means_hind_core <- NA
  
  for(i in 6:51){
  	win <- (i - 5):(i + 5)
  	means_hind_core[i] <- mean(hind_area_yr_core$area[win])
  }
  
	hind_area_yr_pot <- hind_area_yr %>%
		filter(., sp_hab_threshold == "potential")
	
	means_hind_pot <- NA
  
  for(i in 6:51){
  	win <- (i - 5):(i + 5)
  	means_hind_pot[i] <- mean(hind_area_yr_pot$area[win])
  }
	
	years_hind <- c(1970:2020) 
	
	core <- rep("core", 51)
	potential <- rep("potential", 51)
	
	means_core <- as.data.frame(cbind(means_hind_core, core, years_hind)) %>%
		rename(area = means_hind_core, 
					 sp_hab_threshold = core,
					 year = years_hind)
	
	means_pot <- as.data.frame(cbind(means_hind_pot, potential, years_hind)) %>%
		rename(area = means_hind_pot, 
					 sp_hab_threshold = potential,
					  year = years_hind)
	
	rolling_area_hind <- rbind(means_core, means_pot)
	
	rolling_area_hind$area <- as.numeric(rolling_area_hind$area)
	rolling_area_hind$year <- as.numeric(rolling_area_hind$year)

	# proj
	proj_area_yr_sum <- proj_area_yr %>%
		group_by(year, projection, sp_hab_threshold) %>%
		summarise(mean_area = mean(area))
	
	proj_area_yr_sum_core <- proj_area_yr_sum %>%
		filter(sp_hab_threshold == "core")
	
	proj_area_yr_sum_pot <- proj_area_yr_sum %>%
		filter(sp_hab_threshold == "potential")

	proj_area_yr_sum_core_low <- proj_area_yr_sum_core %>%
		filter(projection == "ssp126") 
	
	proj_area_yr_sum_pot_low <- proj_area_yr_sum_pot %>%
		filter(projection == "ssp126")

	
	means_proj_core_low <- NA
	means_proj_pot_low <- NA

  for(i in 6:80){
  	win <- (i - 5):(i + 5)
  	means_proj_core_low[i] <- mean(proj_area_yr_sum_core_low$mean_area[win])
  }
	
	for(i in 6:80){
  	win <- (i - 5):(i + 5)
  	means_proj_pot_low[i] <- mean(proj_area_yr_sum_pot_low$mean_area[win])
  }
	
	
	proj_area_yr_sum_core_high <- proj_area_yr_sum_core %>%
		filter(projection == "ssp585") 
	
	proj_area_yr_sum_pot_high <- proj_area_yr_sum_pot %>%
		filter(projection == "ssp585")

	means_proj_core_high <- NA
	means_proj_pot_high <- NA

  
  for(i in 6:80){
  	win <- (i - 5):(i + 5)
  	means_proj_core_high[i] <- mean(proj_area_yr_sum_core_high$mean_area[win])
  }
	
	for(i in 6:80){
  	win <- (i - 5):(i + 5)
  	means_proj_pot_high[i] <- mean(proj_area_yr_sum_pot_high$mean_area[win])
  }
	
	years_proj <- c(2020:2099) 
	
	core <- rep("core", 80)
	potential <- rep("potential", 80)
	low <- rep("low emission\n(ssp126)", 80)
	high <- rep("high emission\n(ssp585)", 80)


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
   	ggplot() +
		geom_line(data = rolling_area_hind, 
   						aes(x = year, y = area), 
   						color = light_black) +
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
		geom_vline(xintercept = 2020, color = "#f2f2f2", alpha = 0.5) +
    time_series_plot_theme() +
		theme(strip.background = element_blank(),
  				strip.text = element_blank(),
					panel.spacing = unit(0, "lines"),
					axis.text.x = element_blank(),
					axis.title.x = element_blank(),
					axis.ticks.x = element_blank())
	
	area_plot_labs <- 
			ggdraw(area_plot) +
			draw_label("cesm", x = 0.475, y = 0.65, color = "#6dc3a9", size = 12) +
			draw_label("gfdl", x = 0.475, y = 0.55, color = "#4e8d9c", size = 12) +
			draw_label("miroc", x = 0.475, y = 0.60, color = "#97c3e5", size = 12) +
			draw_label("cesm", x = 0.93, y = 0.62, color = "#ffabab", size = 12) +
			draw_label("gfdl", x = 0.93, y = 0.65, color = "#ff4040", size = 12) +
			draw_label("miroc", x = 0.93, y = 0.73, color = "#ffb733", size = 12) +
			draw_label("(a)", x = 0.07, y = 0.84, color = "black", size = 12) + # fix this
			draw_label("(b)", x = 0.525, y = 0.84, color = "black", size = 12) + # fix this
			draw_label("(c)", x = 0.07, y = 0.45, color = "black", size = 12) + # fix this
			draw_label("(d)", x = 0.525, y = 0.45, color = "black", size = 12)# fix this

	#### mean latitude time series ####
	
	# calculating the mean latitude of spawning habitat suitability

	## hindcasts ##
	
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
	
	hind_mean_lat_yr <- bind_rows(hind_mean_lats_yr_0.5, hind_mean_lats_yr_0.9) 
	
	# mean latitude
	mean_lat_hind <- hind_mean_lat_yr %>%
		group_by(year, sp_hab_threshold) %>%
		summarize(mean_lat = mean(hist_mean_lat))
	
	## projections ##
	
	# bias-corrected temp with variance ratio
		
	proj_mean_lat_yr <- function(x){
		
		new_dat <- ROMS_projected_dat %>%
			filter(., sp_hab_suit_var >= x)
		
		new_dat_sum <- new_dat %>%
			group_by(simulation, projection, year) %>%
			summarise(proj_mean_lat = mean(Lat)) 

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

	

	## rolling means ##
	
	# hind
	means_hind_core <- NA
  
  for(i in 6:51){
  	win <- (i - 5):(i + 5)
  	means_hind_core[i] <- mean(hind_mean_lats_yr_0.9$hist_mean_lat[win])
  }
  
	means_hind_pot <- NA
  
  for(i in 6:51){
  	win <- (i - 5):(i + 5)
  	means_hind_pot[i] <- mean(hind_mean_lats_yr_0.5$hist_mean_lat[win])
  }
	
	years_hind <- c(1970:2020) 
	
	core <- rep("core", 51)
	potential <- rep("potential", 51)
	
	means_core <- as.data.frame(cbind(means_hind_core, core, years_hind)) %>%
		rename(mean_lat = means_hind_core, 
					 sp_hab_threshold = core,
					 year = years_hind)
	
	means_pot <- as.data.frame(cbind(means_hind_pot, potential, years_hind)) %>%
		rename(mean_lat = means_hind_pot, 
					 sp_hab_threshold = potential,
					  year = years_hind)
	
	rolling_mean_lat_hind <- rbind(means_core, means_pot)
	
	rolling_mean_lat_hind$mean_lat <- as.numeric(rolling_mean_lat_hind$mean_lat)
	rolling_mean_lat_hind$year <- as.numeric(rolling_mean_lat_hind$year)

	# proj
	proj_mlat_yr_sum <- proj_mean_lat_yr %>%
		group_by(year, projection, sp_hab_threshold) %>%
		summarise(mean_lat = mean(proj_mean_lat))
	
	proj_mlat_yr_sum_core <- proj_mlat_yr_sum %>%
		filter(sp_hab_threshold == 0.9)
	
	proj_mlat_yr_sum_pot <- proj_mlat_yr_sum %>%
		filter(sp_hab_threshold == 0.5)

	proj_mlat_yr_sum_core_low <- proj_mlat_yr_sum_core %>%
		filter(projection == "ssp126") 
	
	proj_mlat_yr_sum_pot_low <- proj_mlat_yr_sum_pot %>%
		filter(projection == "ssp126")

	means_proj_core_low <- NA
	means_proj_pot_low <- NA

  
  for(i in 6:80){
  	win <- (i - 5):(i + 5)
  	means_proj_core_low[i] <- mean(proj_mlat_yr_sum_core_low$mean_lat[win])
  }
	
	for(i in 6:80){
  	win <- (i - 5):(i + 5)
  	means_proj_pot_low[i] <- mean(proj_mlat_yr_sum_pot_low$mean_lat[win])
  }
	
	
	proj_mlat_yr_sum_core_high <- proj_mlat_yr_sum_core %>%
		filter(projection == "ssp585") 
	
	proj_mlat_yr_sum_pot_high <- proj_mlat_yr_sum_pot %>%
		filter(projection == "ssp585")

	means_proj_core_high <- NA
	means_proj_pot_high <- NA

  
  for(i in 6:80){
  	win <- (i - 5):(i + 5)
  	means_proj_core_high[i] <- mean(proj_mlat_yr_sum_core_high$mean_lat[win])
  }
	
	for(i in 6:80){
  	win <- (i - 5):(i + 5)
  	means_proj_pot_high[i] <- mean(proj_mlat_yr_sum_pot_high$mean_lat[win])
  }
	
	core <- rep("core", 80)
	potential <- rep("potential", 80)
	low <- rep("low emission\n(ssp126)", 80)
	high <- rep("high emission\n(ssp585)", 80)


	core_low <- as.data.frame(cbind(means_proj_core_low, core, low, years_proj)) %>% 
		rename(mean_lat = means_proj_core_low, 
					 sp_hab_threshold = core,
					 scen = low,
					 year = years_proj)
	
	core_high <- as.data.frame(cbind(means_proj_core_high, core, high, years_proj)) %>%
		rename(mean_lat = means_proj_core_high, 
					 sp_hab_threshold = core,
					 scen = high,
					 year = years_proj)

	pot_low <- as.data.frame(cbind(means_proj_pot_low, potential, low, years_proj)) %>% 
		rename(mean_lat = means_proj_pot_low, 
					 sp_hab_threshold = potential,
					 scen = low,
					 year = years_proj)
	
	pot_high <- as.data.frame(cbind(means_proj_pot_high, potential, high, years_proj)) %>%
		rename(mean_lat = means_proj_pot_high, 
					 sp_hab_threshold = potential,
					 scen = high,
					 year = years_proj)

	
	rolling_mean_lat_proj <- rbind(core_low, core_high,
														 pot_low, pot_high)

	rolling_mean_lat_proj$mean_lat <- as.numeric(rolling_mean_lat_proj$mean_lat)
	rolling_mean_lat_proj$year <- as.numeric(rolling_mean_lat_proj$year)

	# for plotting by scenario
	proj_mean_lat_yr$scen <- NA
		
	proj_mean_lat_yr$scen[proj_mean_lat_yr$projection == "ssp126"] <- "low emission\n(ssp126)"
	proj_mean_lat_yr$scen[proj_mean_lat_yr$projection == "ssp585"] <- "high emission\n(ssp585)"
	
	proj_mean_lat_yr <- tidyr::unite(proj_mean_lat_yr,"sim_proj",
															 simulation, projection, remove = F)

	colors <- c("#6dc3a9", "#ff7373", # cesm low, cesm high
						  "#4e8d9c", "#ff4040", # gfdl low, gfdl high
						  "#97c3e5", "#ffb733") # miroc low, miroc high

	sim_proj <- unique(proj_mean_lat_yr$sim_proj)
	
	names(colors) <- unique(proj_mean_lat_yr$sim_proj)
	
	# order factors for plotting
	
	hind_mean_lat_yr <- hind_mean_lat_yr %>%
		mutate(thresh = case_when(
			sp_hab_threshold == 0.9 ~ "core",
			sp_hab_threshold == 0.5 ~ "potential"))
		
	rolling_mean_lat_hind <- rolling_mean_lat_hind %>%
		mutate(thresh = sp_hab_threshold)
	
	rolling_mean_lat_proj <- rolling_mean_lat_proj %>%
		mutate(thresh = sp_hab_threshold)
	
	proj_mean_lat_yr$scen_f = factor(proj_mean_lat_yr$scen, 
															 levels=c('low emission\n(ssp126)', 'high emission\n(ssp585)'))
	rolling_mean_lat_proj$scen_f = factor(rolling_mean_lat_proj$scen, 
																		levels=c('low emission\n(ssp126)', 'high emission\n(ssp585)'))
	
	#### mean latitude plot ####
	
	mean_latitude_plot <-    
   	ggplot() +
		geom_line(data = rolling_mean_lat_hind, 
   						aes(x = year, y = mean_lat), 
   						color = light_black) +
		geom_line(data = rolling_mean_lat_proj, 
   						aes(x = year, y = mean_lat), 
   						color = light_black) +
	 	geom_line(aes(year, hist_mean_lat),
            data = hind_mean_lat_yr %>% filter(sp_hab_threshold == 0.5), color = "black", alpha = 0.3) +
		geom_line(data = proj_mean_lat_yr, 
							aes(year, proj_mean_lat, color = sim_proj, group = sim_proj), alpha = 0.3) +
		facet_grid(thresh ~ scen_f) +
	  geom_line(aes(year, hist_mean_lat, colour = sp_hab_threshold),
            data = hind_mean_lat_yr %>% filter(sp_hab_threshold == 0.9), color = "black",  alpha = 0.3) +
		xlab("Year") +
		scale_color_manual(name = "sim_proj", values = colors) +
	  scale_y_continuous(
	  	name = "Mean latitude (˚N)",
	  	breaks = c(56, 57, 58, 59),
	  	labels = c(56, 57, 58, 59),
	  	limits = c(55.2, 59.7)
	  ) +
   	xlim(1970, 2110) +
		geom_vline(data = hind_mean_lat_yr, aes(xintercept = 2020, color = "#f2f2f2"), alpha = 0.5) +
    time_series_plot_theme() +
  	theme(strip.background = element_blank(),
  				strip.text = element_blank(),
  				panel.spacing = unit(0, "lines"),
  				axis.title.x = element_text(size = 18),
  				axis.text.x = element_text(size = 16))
	
	mean_latitude_plot_labs <- 
		ggdraw(mean_latitude_plot) +
		draw_label("cesm", x = 0.485, y = 0.74, color = "#6dc3a9", size = 12) +
		draw_label("gfdl", x = 0.485, y = 0.67, color = "#4e8d9c", size = 12) +
		draw_label("miroc", x = 0.485, y = 0.63, color = "#97c3e5", size = 12) +
		draw_label("cesm", x = 0.93, y = 0.82, color = "#ffabab", size = 12) +
		draw_label("gfdl", x = 0.93, y = 0.71, color = "#ff4040", size = 12) +
		draw_label("miroc", x = 0.93, y = 0.77, color = "#ffb733", size = 12) +
		draw_label("(a)", x = 0.08, y = 0.84, color = "black", size = 12) +
		draw_label("(b)", x = 0.53, y = 0.84, color = "black", size = 12) +
		draw_label("(c)", x = 0.08, y = 0.465, color = "black", size = 12) +
		draw_label("(d)", x = 0.53, y = 0.465, color = "black", size = 12) 


	#### plot together ####
	
	## time series temp
		plot1 <- low_temp_time_lab + theme(plot.margin = unit(c(0.5, 0, 0, 1), "in"))
		plot2 <- high_temp_time_lab + theme(plot.margin = unit(c(0.5, 1, 0, 0), "in"))

		time_series_temp <- plot1 + plot2 + plot_layout(ncol = 2)

	## time series hab suit
		plot3 <- low_habsuit_time_lab + theme(plot.margin = unit(c(0, 0, 0, 0.5), "in"))
		plot4 <- high_habsuit_time_lab + theme(plot.margin = unit(c(0, 1, 0, -0.05), "in"))
	
		time_series_habsuit <- plot3 + plot4 + plot_layout(ncol = 2)

  ## time series area
		area_ts <- area_plot_labs + theme(plot.margin = unit(c(0, 1, 0, 0.5), "in"))

	## time series mean lat
		meanlat_ts <- mean_latitude_plot_labs + theme(plot.margin = unit(c(0, 1, 0, 0.5), "in"))
	
	# plot together
	time_series_plots_top <- time_series_temp/time_series_habsuit
		
	time_series_plots_bot <- area_ts/meanlat_ts 
	#+
	#	plot_layout(widths = c(1, 1, 1.2, 1.2), heights = c(1,1,1,1))
	
	time_series_plots <- time_series_plots_top/time_series_plots_bot 
	#+
	 #plot_layout(ncol = 1, widths = c(1, 1.5), heights = c(1, 1.5))
	
	ggsave("./output/plots/time_series_plots.png",
			 time_series_plots,
			 width = 15, height = 20, units = "in")
	
	

	
	