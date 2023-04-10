# figure -- consistency year chunks

	# format breaks for longitude for these plots
  breaks_x <- c(-170, -160)
	labels_x <- c("-170˚", "-160˚") 
	limits_x <- c(-1400000, -150000)
	
	# historical

  # remove 2020 because decade goes from 2010-2019
  hist_yrs <- 1970:1989

	ROMS_hist20 <- ROMS_hindcast_dat %>% filter(., year %in% hist_yrs)

	# number of years
	no_yrs_hist <- length(unique(ROMS_hist20$year))

	# annual average
	ROMS_hist20_yr_sum <- ROMS_hist20 %>%
		group_by(latitude, longitude, year) %>%
		summarise(mean_sphabsuit = mean(sp_hab_suit))

 
	# 0.9
	ROMS_hist20_yr_sum_09 <- ROMS_hist20_yr_sum %>%
		group_by(latitude, longitude) %>%
		dplyr::summarize(no_yrs = length(which(mean_sphabsuit >= 0.9))) %>%
		mutate(year_tot = no_yrs_hist,
					 pct_yrs = (no_yrs/year_tot) * 100)

	ROMS_hist20_yr_sum_09_sf <- ROMS_hist20_yr_sum_09 %>%
			mutate(long_not_360 = longitude - 360) %>%
	  	st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326)
	
	# 0.5
	ROMS_hist20_yr_sum_05 <- ROMS_hist20_yr_sum %>%
		group_by(latitude, longitude) %>%
		dplyr::summarize(no_yrs = length(which(mean_sphabsuit >= 0.5))) %>%
		mutate(year_tot = no_yrs_hist,
					 pct_yrs = (no_yrs/year_tot) * 100)

	ROMS_hist20_yr_sum_05_sf <- ROMS_hist20_yr_sum_05 %>%
			mutate(long_not_360 = longitude - 360) %>%
	  	st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326)
 
  
	# current period

  # remove 2020 because decade goes from 2010-2019
  current_yrs <- 2001:2020

	ROMS_current <- ROMS_hindcast_dat %>% filter(., year %in% current_yrs)

	# number of years
	no_yrs_current <- length(unique(ROMS_current$year))

	# annual average
	ROMS_current_yr_sum <- ROMS_current %>%
		group_by(latitude, longitude, year) %>%
		summarise(mean_sphabsuit = mean(sp_hab_suit))

	# 0.9
	ROMS_current_yr_sum_09 <- ROMS_current_yr_sum %>%
		group_by(latitude, longitude) %>%
		dplyr::summarize(no_yrs = length(which(mean_sphabsuit >= 0.9))) %>%
		mutate(year_tot = no_yrs_current,
					 pct_yrs = (no_yrs/year_tot) * 100)

	ROMS_current_yr_sum_09_sf <- ROMS_current_yr_sum_09 %>%
			mutate(long_not_360 = longitude - 360) %>%
	  	st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326)
  
	# 0.5
	ROMS_current_yr_sum_05 <- ROMS_current_yr_sum %>%
		group_by(latitude, longitude) %>%
		dplyr::summarize(no_yrs = length(which(mean_sphabsuit >= 0.5))) %>%
		mutate(year_tot = no_yrs_current,
					 pct_yrs = (no_yrs/year_tot) * 100)

	ROMS_current_yr_sum_05_sf <- ROMS_current_yr_sum_05 %>%
			mutate(long_not_360 = longitude - 360) %>%
	  	st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326)

	# projection
	
	last_yrs <- 2080:2099

	ROMS_proj <- ROMS_projected_dat %>% filter(., year %in% last_yrs)
	
	# number of years
	no_yrs_proj <- length(unique(ROMS_projected_dat_dec$year))

	ROMS_proj_yr_sum <- ROMS_proj %>%
		group_by(simulation, projection, year, latitude, longitude) %>%
		summarise(mean_sphabsuit = mean(sp_hab_suit_var))

	# 0.9
	ROMS_proj_yr_sum_09 <- ROMS_proj_yr_sum %>%
		group_by(simulation, projection,latitude, longitude) %>%
		dplyr::summarize(no_yrs = length(which(mean_sphabsuit >= 0.9))) %>%
		mutate(year_tot = no_yrs_proj,
					 pct_yrs = (no_yrs/year_tot) * 100)

	ROMS_proj_yr_sum_09_sf <- ROMS_proj_yr_sum_09 %>%
			mutate(long_not_360 = longitude - 360) %>%
	  	st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326)
	

	# 0.5
	ROMS_proj_yr_sum_05 <- ROMS_proj_yr_sum %>%
		group_by(simulation, projection, latitude, longitude) %>%
		dplyr::summarize(no_yrs = length(which(mean_sphabsuit >= 0.5))) %>%
		mutate(year_tot = no_yrs_proj,
					 pct_yrs = (no_yrs/year_tot) * 100)

	ROMS_proj_yr_sum_05_sf <- ROMS_proj_yr_sum_05 %>%
			mutate(long_not_360 = longitude - 360) %>%
	  	st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326)
  

		# add in scenario as factor
	ROMS_proj_yr_sum_05_sf$scen <- NA
	
  ROMS_proj_yr_sum_05_sf$scen[ROMS_proj_yr_sum_05_sf$projection == "SSP126"] <- "low\nemission\n(SSP126)"
	ROMS_proj_yr_sum_05_sf$scen[ROMS_proj_yr_sum_05_sf$projection == "SSP585"] <- "high\nemission\n(SSP585)"
	
	ROMS_proj_yr_sum_05_sf$scen_f = factor(ROMS_proj_yr_sum_05_sf$scen, 
																				 levels=c('low\nemission\n(SSP126)', 
																								  'high\nemission\n(SSP585)'))

	ROMS_proj_yr_sum_09_sf$scen <- NA
	
  ROMS_proj_yr_sum_09_sf$scen[ROMS_proj_yr_sum_09_sf$projection == "SSP126"] <- "low\nemission\n(SSP126)"
	ROMS_proj_yr_sum_09_sf$scen[ROMS_proj_yr_sum_09_sf$projection == "SSP585"] <- "high\nemission\n(SSP585)"
	
	ROMS_proj_yr_sum_09_sf$scen_f = factor(ROMS_proj_yr_sum_09_sf$scen, 
																				 levels=c('low\nemission\n(SSP126)', 
																									'high\nemission\n(SSP585)'))

	#### core plots ####

  # historical
  hist20_consistency09 <-	
		ggplot() +
		geom_sf(data = ROMS_hist20_yr_sum_09_sf, aes(color = pct_yrs))  +
		geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
		coord_sf(crs = 3338) +
		scale_color_viridis_c(limits = c(0, 100)) +
 		scale_x_continuous(
 			breaks = breaks_x,
 			labels = labels_x,
 			name = "Longitude",
 			limits = c(-1400000, -150000)
 		) +
 		scale_y_continuous(
 			breaks = c(55, 60),
 			limits = c(470000, 1900000),
 			name = "Latitude",
 		) +
    labs(colour = "%\nyears") +
		theme_bw() +
	 	theme(
			panel.border = element_rect(color = "#666666"),
 			axis.text = element_text(size = 8,  color = "#666666"),	
  		axis.title = element_text(size = 10,  color = "#666666"),
 			axis.ticks = element_line(color = "#666666"),
 			plot.margin = unit(c(0.25, 0, 0, 0), "in"))

  # current
	current20_consistency09 <-	
		ggplot() +
		geom_sf(data = ROMS_current_yr_sum_09_sf, aes(color = pct_yrs))  +
		geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
		coord_sf(crs = 3338) +
		scale_color_viridis_c(limits = c(0, 100)) +
 		scale_x_continuous(
 			breaks = breaks_x,
 			labels = labels_x,
 			name = "Longitude",
 			limits = c(-1400000, -150000)
 		) +
 		scale_y_continuous(
 			breaks = c(55, 60),
 			limits = c(470000, 1900000),
 			name = "Latitude",
 		) +
    labs(colour = "%\nyears") +
		theme_bw() +
	 	theme(
			panel.border = element_rect(color = "#666666"),
 			axis.text.x = element_text(size = 8,  color = "#666666"),	
  		axis.title.x = element_text(size = 10,  color = "#666666"),
 			axis.ticks.y = element_blank(),
 			axis.title.y = element_blank(),
 			axis.text.y = element_blank(),
 			plot.margin = unit(c(0.25, -0.05, 0, 0), "in"))

	
	last20_consistency09 <-	
		ggplot() +
		geom_sf(data = ROMS_proj_yr_sum_09_sf, aes(color = pct_yrs))  +
		geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
		facet_grid(scen_f ~ simulation) +
		coord_sf(crs = 3338) +
		scale_color_viridis_c(limits = c(0, 100)) +
 		scale_x_continuous(
 			breaks = breaks_x,
 			labels = labels_x,
 			name = "Longitude",
 			limits = limits_x
 		) +
 		scale_y_continuous(
 			breaks = c(55, 60),
 			limits = c(470000, 1900000),
 			name = "Latitude",
 		) +
    labs(colour = "%\nyears") +
		theme_bw() +
 		theme(
 			panel.spacing = unit(4, "pt"),
 			panel.border = element_rect(color = "#666666"),
 			strip.text.x = element_text(size = 10, face = "bold",  color = "#808080"),
 			strip.text.y = element_blank(),
 			strip.background = element_blank(),
 			axis.text.x = element_text(size = 8,  color = "#666666"),	
  		axis.title.x = element_text(size = 10,  color = "#666666"),
 			axis.ticks.y = element_blank(),
 			axis.title.y = element_blank(),
 			axis.text.y = element_blank(),
 			axis.ticks.x = element_line( color = "#666666"),
 			plot.margin = unit(c(0.25,-0.05,-0.05, -0.1), "in"))
	
	#### potential plots ####
	
	hist20_consistency05 <-	
		ggplot() +
		geom_sf(data = ROMS_hist20_yr_sum_05_sf, aes(color = pct_yrs))  +
		geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
		coord_sf(crs = 3338) +
		scale_color_viridis_c(limits = c(0, 100)) +
 		scale_x_continuous(
 			breaks = breaks_x,
 			labels = labels_x,
 			name = "Longitude",
 			limits = c(-1400000, -150000)
 		) +
 		scale_y_continuous(
 			breaks = c(55, 60),
 			limits = c(470000, 1900000),
 			name = "Latitude",
 		) +
    labs(colour = "%\nyears") +
		theme_bw() +
	 	theme(
			panel.border = element_rect(color = "#666666"),
 			axis.text.x = element_text(size = 8,  color = "#666666"),	
  		axis.title.x = element_text(size = 10,  color = "#666666"),
 			axis.ticks.x = element_line(color = "#666666"),
			axis.text.y = element_blank(),
  		axis.title.y = element_blank(),
 			axis.ticks.y = element_blank(),
 			plot.margin = unit(c(0.25,-0.1, 0, 0), "in"))

	current20_consistency05 <-	
		ggplot() +
		geom_sf(data = ROMS_current_yr_sum_05_sf, aes(color = pct_yrs))  +
		geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
		coord_sf(crs = 3338) +
		scale_color_viridis_c(limits = c(0, 100)) +
 		scale_x_continuous(
 			breaks = breaks_x,
 			labels = labels_x,
 			name = "Longitude",
 			limits = c(-1400000, -150000)
 		) +
 		scale_y_continuous(
 			breaks = c(55, 60),
 			limits = c(470000, 1900000),
 			name = "Latitude",
 		) +
    labs(colour = "%\nyears") +
		theme_bw() +
	 	theme(
			panel.border = element_rect(color = "#666666"),
 			axis.text.x = element_text(size = 8,  color = "#666666"),	
  		axis.title.x = element_text(size = 10,  color = "#666666"),
 			axis.ticks.x = element_line(color = "#666666"),
			axis.text.y = element_blank(),
  		axis.title.y = element_blank(),
 			axis.ticks.y = element_blank(),
 			plot.margin = unit(c(0.25,-0.1, 0, 0), "in"))

	
	last20_consistency05 <-	
		ggplot() +
		geom_sf(data = ROMS_proj_yr_sum_05_sf, aes(color = pct_yrs))  +
		geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
		facet_grid(scen_f ~ simulation) +
		coord_sf(crs = 3338) +
		scale_color_viridis_c(limits = c(0, 100)) +
 		scale_x_continuous(
 			breaks = breaks_x,
 			labels = labels_x,
 			name = "Longitude",
 			limits = limits_x
 		) +
 		scale_y_continuous(
 			breaks = c(55, 60),
 			limits = c(470000, 1900000),
 			name = "Latitude",
 		) +
    labs(colour = "%\nyears") +
		theme_bw() +
 			theme(
 			panel.spacing = unit(2, "pt"),
 			panel.border = element_rect(color = "#666666"),
 			strip.text.y = element_text(size = 10, face = "bold",  color = "#808080", angle = 0),
 			strip.text.x = element_text(size = 10, face = "bold",  color = "#808080", angle = 0),
 			strip.background = element_blank(),
 			axis.text.x = element_text(size = 8,  color = "#666666"),	
  		axis.title.x = element_text(size = 10,  color = "#666666"),
 			axis.ticks.y = element_blank(),
 			axis.title.y = element_blank(),
 			axis.text.y = element_blank(),
 			axis.ticks.x = element_line( color = "#666666"),
 			plot.margin = unit(c(0.25,-0.05,-0.05, -0.1), "in"))
	

	#### plot together  ####
  consis_maps <-   
   	hist20_consistency09 + 
  	current20_consistency09 +
  	last20_consistency09 +
  	hist20_consistency05 + 
		current20_consistency05 + 
  	last20_consistency05 +
  	plot_spacer() +
 		plot_layout(guides = 'collect',
 								ncol = 7, 
 								widths = c(1.3, 1.3, 4, 1.3, 1.3, 4, 0))
 				
  ggsave("./output/plots/consis_maps.png",
		consis_maps,
		height = 5,
		width = 20)
 								
  
   consis_maps_form <- consis_maps + 
  	annotate("text", label = "Historical\n(1970 - 1999)", 
  			x = 1, y = 1, size = 10, fontface = 2)  +
		annotate("text", label = "Current\n(2001 - 2020)", 
  			x = -14.78, y = 0.68, size = 3, fontface = 2)  +
		annotate("text", label = "Projected (2080 - 2099)",	
						 x = -4, y = 0.87, size = 3, fontface = 2) +
  	annotate("text", label = "Historical\n(1970 - 1999)", 
  			x = -14.78, y = 0.68, size = 3, fontface = 2)  +
  	annotate("text", label = "Current\n(2001 - 2020)", 
  			x = -7.5, y = 0.68, size = 3, fontface = 2)  +
		annotate("text", label = "Projected (2080 - 2099)",	
						 x = -11, y = 0.87, size = 3, fontface = 2) +
  	annotate("text", label = "Potential habitat",	
						 x = -7, y = 1.03, size = 5, fontface = 2) +
  	annotate("text", label = "Core habitat",	
						 x = -15, y = 1.03, size = 5, fontface = 2) +
  	annotate("segment", x = -8.45, xend = -8.45, y = 0.13, yend = 1.05)

   
 # consis_maps_form <- consis_maps + 
 # 	annotate("text", label = "Historical\n(1970 - 1999)", 
 # 			x = -14.78, y = 0.68, size = 3, fontface = 2)  +
#		annotate("text", label = "Current\n(2001 - 2020)", 
 # 			x = -14.78, y = 0.68, size = 3, fontface = 2)  +
#		annotate("text", label = "Projected (2080 - 2099)",	
#						 x = -4, y = 0.87, size = 3, fontface = 2) +
 # 	annotate("text", label = "Historical\n(1970 - 1999)", 
 # 			x = -14.78, y = 0.68, size = 3, fontface = 2)  +
 # 	annotate("text", label = "Current\n(2001 - 2020)", 
 # 			x = -7.5, y = 0.68, size = 3, fontface = 2)  +
#		annotate("text", label = "Projected (2080 - 2099)",	
#						 x = -11, y = 0.87, size = 3, fontface = 2) +
 # 	annotate("text", label = "Potential habitat",	
#						 x = -7, y = 1.03, size = 5, fontface = 2) +
 # 	annotate("text", label = "Core habitat",	
#						 x = -15, y = 1.03, size = 5, fontface = 2) +
 # 	annotate("segment", x = -8.45, xend = -8.45, y = 0.13, yend = 1.05)
#
  ggsave("./output/plots/consis_maps_form.png",
		consis_maps_form,
			 height = 5,
			 width = 20)
 
  
  
  ### experiment with layout
  
   consis_maps_core <-   
   	hist20_consistency09 + 
		current20_consistency09 + 
  	last20_consistency09 +
   	plot_layout(guides ="collect",
   							ncol = 3, widths = c(1.3, 1.3, 4))
   	
   consis_maps_pot <-
  	hist20_consistency05 + 
		current20_consistency05 + 
  	last20_consistency05 +
   	plot_layout(guides ="collect",
   							ncol = 3, widths = c(1.3, 1.3, 4))
   
   consis_maps_combined <- cowplot::plot_grid(
		 consis_maps_core, consis_maps_pot,
		 labels = "AUTO", ncol = 1)

  ggsave("./output/plots/consis_maps_combined.png",
		consis_maps_combined)
 	
  ## again
  
  consis_maps_combined <-   
   	(hist20_consistency09 + plot_layout(widths = c(0.25)) +
  	current20_consistency09 + plot_layout(widths = c(0.25)) +
  	last20_consistency09 + plot_layout(widths = c(1))) /
  	(hist20_consistency05 + 
		current20_consistency05 + 
  	last20_consistency05) +
 		plot_layout(guides = 'collect',
 								widths = c(1.3, 1.3, 4, 1.3, 1.3, 4))

   ggsave("./output/plots/consis_maps_combined.png",
		consis_maps_combined)
 