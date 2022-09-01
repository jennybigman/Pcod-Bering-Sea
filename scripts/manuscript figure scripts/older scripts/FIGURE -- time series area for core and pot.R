# figure 3 -- time series of area of spawning habitat suitability

	# calculate area 

	#### by year #### each grid cell counted only once
	
	#### hindcast ####
	
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

	
	#### projections ####
	
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
	
	#### rolling means of area ####
	
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
	
	years_hind <- c(1970:2020) # does this need to be 1980?
	
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
	
	rolling_mean_hind <- rbind(means_core, means_pot)
	
	rolling_mean_hind$area <- as.numeric(rolling_mean_hind$area)
	rolling_mean_hind$year <- as.numeric(rolling_mean_hind$year)

	# proj
	proj_area_yr_sum <- proj_area_yr %>%
		group_by(year, scen, sp_hab_threshold) %>%
		summarise(mean_area = mean(area))
	
	proj_area_yr_sum_core <- proj_area_yr_sum %>%
		filter(sp_hab_threshold == "core")
	
	proj_area_yr_sum_pot <- proj_area_yr_sum %>%
		filter(sp_hab_threshold == "potential")

	proj_area_yr_sum_core_low <- proj_area_yr_sum_core %>%
		filter(scen_f == "low emission\n(ssp126)") 
	
	proj_area_yr_sum_pot_low <- proj_area_yr_sum_pot %>%
		filter(scen_f == "low emission\n(ssp126)")

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
		filter(scen_f == "high emission\n(ssp585)") 
	
	proj_area_yr_sum_pot_high <- proj_area_yr_sum_pot %>%
		filter(scen_f == "high emission\n(ssp585)")

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
					 scen_f = low,
					 year = years_proj)
	
	core_high <- as.data.frame(cbind(means_proj_core_high, core, high, years_proj)) %>%
		rename(area = means_proj_core_high, 
					 sp_hab_threshold = core,
					 scen_f = high,
					 year = years_proj)

	pot_low <- as.data.frame(cbind(means_proj_pot_low, potential, low, years_proj)) %>% 
		rename(area = means_proj_pot_low, 
					 sp_hab_threshold = potential,
					 scen_f = low,
					 year = years_proj)
	
	pot_high <- as.data.frame(cbind(means_proj_pot_high, potential, high, years_proj)) %>%
		rename(area = means_proj_pot_high, 
					 sp_hab_threshold = potential,
					 scen_f = high,
					 year = years_proj)

	
	rolling_mean_proj <- rbind(core_low, core_high,
														 pot_low, pot_high)

	rolling_mean_proj$area <- as.numeric(rolling_mean_proj$area)
	rolling_mean_proj$year <- as.numeric(rolling_mean_proj$year)

	
	# for plotting by scenario
		
	proj_area_yr <- proj_area_yr %>% filter(., projection != "historical")
		
	proj_area_yr$scen <- NA
		
	proj_area_yr$scen[proj_area_yr$projection == "ssp126"] <- "low emission\n(ssp126)"
	proj_area_yr$scen[proj_area_yr$projection == "ssp585"] <- "high emission\n(ssp585)"
	
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
	rolling_mean_proj$scen_f = factor(rolling_mean_proj$scen, 
																		levels=c('low emission\n(ssp126)', 'high emission\n(ssp585)'))
	
	#### plot ####
	
	area_plot <-    
   	ggplot(data = hind_area_yr) +
		geom_line(data = rolling_mean_hind, 
   						aes(x = year, y = area), 
   						color = "#e5e5e5") +
		geom_line(data = rolling_mean_proj, 
   						aes(x = year, y = area), 
   						color = "#e5e5e5") +
	 	geom_line(aes(year, area, alpha = 0.5),
            data = . %>% filter(sp_hab_threshold == "potential"), color = "black") +
		geom_line(data = proj_area_yr, 
							aes(year, area, color = sim_proj, group = sim_proj, alpha = 0.5)) +
		facet_grid(sp_hab_threshold ~ scen_f) +
	  geom_line(aes(year, area, colour = sp_hab_threshold, alpha = 0.5),
            data = . %>% filter(sp_hab_threshold == "core"), color = "black") +
		xlab("Year") +
		scale_color_manual(name = "sim_proj", values = colors) +
	  scale_y_continuous(
	  	name =	"Area (x 10<sup>5</sup> km<sup>2</sup>)",
	  	breaks = c(100000, 200000, 300000, 400000, 500000),
	  	labels = c(1, 2, 3, 4,5),
	  	limits = c(19000, 567000)) +
   	xlim(1970, 2110) +
		geom_vline(aes(xintercept = 2020, color = "#f2f2f2"), alpha = 0.5) +
    theme_bw() +
  	theme(legend.position = "none") +
  	theme(
			strip.background = element_blank(),
  		strip.text = element_text(size = 18, face = "bold"),
			axis.text = element_text(size = 16, colour = "grey50"),
  	  axis.ticks = element_line(colour = "grey50"),
  	  axis.line = element_line(colour = "grey50"),
			axis.title.x = element_text(size = 16, color = "grey50"),
  	  axis.title.y = element_markdown(size=16, color = "grey50"),
  	  panel.grid.major = element_blank(),
  	  panel.grid.minor = element_blank(),
  	  panel.border = element_rect(fill = NA, color = "grey50"))
	
	area_plot_labs <- 
			ggdraw(area_plot) +
			draw_label("cesm", x = 0.475, y = 0.65, color = "#6dc3a9", size = 12) +
			draw_label("gfdl", x = 0.475, y = 0.55, color = "#4e8d9c", size = 12) +
			draw_label("miroc", x = 0.475, y = 0.60, color = "#97c3e5", size = 12) +
			draw_label("cesm", x = 0.93, y = 0.62, color = "#ffabab", size = 12) +
			draw_label("gfdl", x = 0.93, y = 0.65, color = "#ff4040", size = 12) +
			draw_label("miroc", x = 0.93, y = 0.73, color = "#ffb733", size = 12) +
			draw_label("(a)", x = 0.07, y = 0.84, color = "black", size = 12) +
			draw_label("(b)", x = 0.525, y = 0.84, color = "black", size = 12) +
			draw_label("(c)", x = 0.07, y = 0.45, color = "black", size = 12) +
			draw_label("(d)", x = 0.525, y = 0.45, color = "black", size = 12) 

	ggsave("./output/plots/area_plot_labs.png",
			 area_plot_labs,
			 width = 10, height = 5, units = "in")
	