# figure -- consistency year chunks

  
  # remove 2020 because decade goes from 2010-2019
  first_yrs <- 2001:2020

	ROMS_hindcast_dat_dec <- ROMS_hindcast_dat %>% filter(., year %in% first_yrs)

	# number of years
	no_yrs <- length(unique(ROMS_hindcast_dat_dec$year))

	# annual average
	ROMS_hindcast_dat_dec_yr_sum <- ROMS_hindcast_dat_dec %>%
		group_by(latitude, longitude, year) %>%
		summarise(mean_sphabsuit = mean(sp_hab_suit))

	# 0.5
	ROMS_hindcast_dat_dec_yr_sum_05 <- ROMS_hindcast_dat_dec_yr_sum %>%
		group_by(latitude, longitude) %>%
		dplyr::summarize(no_yrs = length(which(mean_sphabsuit >= 0.5))) %>%
		mutate(year_tot = 20,
					 pct_yrs = (no_yrs/year_tot) * 100)

	ROMS_hindcast_dat_dec_yr_sum_05_sf <- ROMS_hindcast_dat_dec_yr_sum_05 %>%
			mutate(long_not_360 = longitude - 360) %>%
	  	st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326)
  
	# 0.9
	ROMS_hindcast_dat_dec_yr_sum_09 <- ROMS_hindcast_dat_dec_yr_sum %>%
		group_by(latitude, longitude) %>%
		dplyr::summarize(no_yrs = length(which(mean_sphabsuit >= 0.9))) %>%
		mutate(year_tot = 20,
					 pct_yrs = (no_yrs/year_tot) * 100)

	ROMS_hindcast_dat_dec_yr_sum_09_sf <- ROMS_hindcast_dat_dec_yr_sum_09 %>%
			mutate(long_not_360 = longitude - 360) %>%
	  	st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326)
  
	# projection
	
	last_yrs <- 2080:2099

	ROMS_projected_dat_dec <- ROMS_projected_dat %>% filter(., year %in% last_yrs)
	
	# number of years
	no_yrs <- length(unique(ROMS_projected_dat_dec$year))

	ROMS_projected_dat_yr_sum <- ROMS_projected_dat_dec %>%
		group_by(simulation, projection, year, latitude, longitude) %>%
		summarise(mean_sphabsuit = mean(sp_hab_suit_var))

	# 0.5
	ROMS_projected_dat_yr_sum_05 <- ROMS_projected_dat_yr_sum %>%
		group_by(simulation, projection, latitude, longitude) %>%
		dplyr::summarize(no_yrs = length(which(mean_sphabsuit >= 0.5))) %>%
		mutate(year_tot = 20,
					 pct_yrs = (no_yrs/year_tot) * 100)

	ROMS_projected_dat_yr_sum_05_sf <- ROMS_projected_dat_yr_sum_05 %>%
			mutate(long_not_360 = longitude - 360) %>%
	  	st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326)
  
	# 0.9
	ROMS_projected_dat_yr_sum_09 <- ROMS_projected_dat_yr_sum %>%
		group_by(simulation, projection,latitude, longitude) %>%
		dplyr::summarize(no_yrs = length(which(mean_sphabsuit >= 0.9))) %>%
		mutate(year_tot = 20,
					 pct_yrs = (no_yrs/year_tot) * 100)

	ROMS_projected_dat_yr_sum_09_sf <- ROMS_projected_dat_yr_sum_09 %>%
			mutate(long_not_360 = longitude - 360) %>%
	  	st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326)
	
	# add in scenario as factor
	ROMS_projected_dat_yr_sum_05_sf$scen <- NA
	
  ROMS_projected_dat_yr_sum_05_sf$scen[ROMS_projected_dat_yr_sum_05_sf$projection == "ssp126"] <- "low\nemission\n(ssp126)"
	ROMS_projected_dat_yr_sum_05_sf$scen[ROMS_projected_dat_yr_sum_05_sf$projection == "ssp585"] <- "high\nemission\n(ssp585)"
	
	ROMS_projected_dat_yr_sum_05_sf$scen_f = factor(ROMS_projected_dat_yr_sum_05_sf$scen, 
																									levels=c('low\nemission\n(ssp126)', 
																											     'high\nemission\n(ssp585)'))

	ROMS_projected_dat_yr_sum_09_sf$scen <- NA
	
  ROMS_projected_dat_yr_sum_09_sf$scen[ROMS_projected_dat_yr_sum_09_sf$projection == "ssp126"] <- "low\nemission\n(ssp126)"
	ROMS_projected_dat_yr_sum_09_sf$scen[ROMS_projected_dat_yr_sum_09_sf$projection == "ssp585"] <- "high\nemission\n(ssp585)"
	
	ROMS_projected_dat_yr_sum_09_sf$scen_f = factor(ROMS_projected_dat_yr_sum_09_sf$scen, 
																									levels=c('low\nemission\n(ssp126)', 
																											     'high\nemission\n(ssp585)'))

	
	current20_consistency05 <-	
		ggplot() +
		geom_sf(data = ROMS_hindcast_dat_dec_yr_sum_05_sf, aes(color = pct_yrs))  +
		geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
		coord_sf(crs = 3338) +
		scale_color_viridis_c() +
 		scale_x_continuous(
 			breaks = c(-175, -170, -165, -160),
 			labels = c("-175˚", "-170˚", "-165˚", "-160˚"),
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
 			legend.position = "none",
 			axis.text = element_text(size = 8,  color = "#666666"),	
  		axis.title = element_text(size = 10,  color = "#666666"),
 			axis.ticks = element_line(color = "#666666"),
 			plot.margin = unit(c(0.05,-0.1,-0, 0), "in"))
	
	current20_consistency09 <-	
		ggplot() +
		geom_sf(data = ROMS_hindcast_dat_dec_yr_sum_09_sf, aes(color = pct_yrs))  +
		geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
		coord_sf(crs = 3338) +
		scale_color_viridis_c() +
 		scale_x_continuous(
 			breaks = c(-175, -170, -165, -160),
 			labels = c("-175˚", "-170˚", "-165˚", "-160˚"),
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
 			legend.position = "none",
 			axis.text = element_text(size = 8,  color = "#666666"),	
  		axis.title = element_text(size = 10,  color = "#666666"),
 			axis.ticks = element_line(color = "#666666"),
 			plot.margin = unit(c(0.05,-0.1,-0, 0), "in"))
   
	# projections
	
	last20_consistency05 <-	
		ggplot() +
		geom_sf(data = ROMS_projected_dat_yr_sum_05_sf, aes(color = pct_yrs))  +
		geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
		facet_grid(scen_f ~ simulation) +
		coord_sf(crs = 3338) +
		scale_color_viridis_c() +
 		scale_x_continuous(
 			breaks = c(-175, -170, -165, -160),
 			labels = c("-175˚", "-170˚", "-165˚", "-160˚"),
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
 			panel.spacing = unit(0.1, "in"),
 			panel.border = element_rect(color = "#666666"),
 			legend.position = "none",
 			strip.text.x = element_text(size = 10, face = "bold",  color = "#808080"),
 			strip.text.y = element_text(size = 10, face = "bold", angle = 0,  color = "#666666"),
 			strip.background = element_blank(),
 			axis.text.x = element_text(size = 8,  color = "#666666"),	
  		axis.title.x = element_text(size = 10,  color = "#666666"),
 			axis.ticks.y = element_blank(),
 			axis.title.y = element_blank(),
 			axis.text.y = element_blank(),
 			axis.ticks.x = element_line( color = "#666666"),
 			plot.margin = unit(c(0.25,-0.05,-0.05, -0.1), "in"))

	
	
	last20_consistency09 <-	
		ggplot() +
		geom_sf(data = ROMS_projected_dat_yr_sum_09_sf, aes(color = pct_yrs))  +
		geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
		facet_grid(scen_f ~ simulation) +
		coord_sf(crs = 3338) +
		scale_color_viridis_c() +
 		scale_x_continuous(
 			breaks = c(-175, -170, -165, -160),
 			labels = c("-175˚", "-170˚", "-165˚", "-160˚"),
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
 			panel.spacing = unit(0.1, "in"),
 			panel.border = element_rect(color = "#666666"),
 			legend.position = "none",
 			strip.text.x = element_text(size = 10, face = "bold",  color = "#808080"),
 			strip.text.y = element_text(size = 10, face = "bold", angle = 0,  color = "#666666"),
 			strip.background = element_blank(),
 			axis.text.x = element_text(size = 8,  color = "#666666"),	
  		axis.title.x = element_text(size = 10,  color = "#666666"),
 			axis.ticks.y = element_blank(),
 			axis.title.y = element_blank(),
 			axis.text.y = element_blank(),
 			axis.ticks.x = element_line( color = "#666666"),
 			plot.margin = unit(c(0.25,-0.05,-0.05, -0.1), "in"))

	# extract legend
	
	legend_plot <-	
		ggplot() +
		geom_sf(data = ROMS_hindcast_dat_dec_yr_sum_05_sf, aes(color = pct_yrs))  +
		geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
		coord_sf(crs = 3338) +
		scale_color_viridis_c() +
 		scale_x_continuous(
 			breaks = c(-175, -170, -165, -160),
 			labels = c("-175˚", "-170˚", "-165˚", "-160˚"),
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
	 		axis.text = element_text(size = 8),	
  		axis.title = element_text(size = 10),
 			legend.title.align = 0.5,
 			legend.title = element_text( color = "#666666", size = 9),
 			legend.text = element_text(color = "#666666", size = 7),
 			plot.margin = unit(c(-0.05,-0.05,-0.05, -0.05), "in"),
 			panel.border = element_rect(color = "#666666"))
 	
  legend_plot <- cowplot::get_legend(legend_plot) 
  legend_plot <- 	ggpubr::as_ggplot(legend_plot) 

  # plot together
  
  #05 Potential habitat 
 
	consistency_maps05 <- current20_consistency05 + last20_consistency05 + 
		legend_plot + plot_layout(ncol = 3, widths = c(1.3,4,1))

  consistency_maps05_form <- consistency_maps05 + 
  	annotate("text", label = (paste(" (a) Potential habitat\n(spawning habitat\nsuitability", 
			 symbol, "0.5)", sep =" ")), x = -5.8, y = 1, size = 5, fontface = 2, hjust = 0) +
		annotate("text", label = "Current\n(2001 - 2020)", 
  			x = -5.2, y = 0.83, size = 4, fontface = 2) +
		annotate("text", label = "Projected (2080 - 2099)",	
						 x = -2.5, y = 1.1, size = 4, fontface = 2)
		
  ggsave("./output/plots/consistency_maps05_form.png",
			 consistency_maps05_form,
			 height = 5,
			 width = 10)
  
  # core habitat
  consistency_maps09 <- current20_hist09 + last20_hist09 + 
		legend_plot + plot_layout(ncol = 3, widths = c(1.3,4,1))

  consistency_maps09_form <- consistency_maps09 + 
  	annotate("text", label = (paste("Core habitat\n(spawning habitat\nsuitability", 
			 symbol, "0.9)", sep =" ")), x = -5.82, y = 0.7, size = 2, fontface = 2, hjust = 0,
			 color = "#00345C") +
		annotate("text", label = "Current\n(2001 - 2020)", 
  			x = -5.2, y = 0.83, size = 5, fontface = 2) +
		annotate("text", label = "Projected (2080 - 2099)",	
						 x = -2.5, y = 1.1, size = 5, fontface = 2)
		
  ggsave("./output/plots/consistency_maps09_form.png",
			 consistency_maps09_form,
			 height = 5,
			 width = 10)