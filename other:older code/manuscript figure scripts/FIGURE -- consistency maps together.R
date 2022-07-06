# figure -- consistency year chunks

	# format breaks for longitude for these plots
  breaks_x <- c(-170, -160)
	labels_x <- c("-170˚", "-160˚") 
	limits_x <- c(-1400000, -150000)
	
	# historical

  # remove 2020 because decade goes from 2010-2019
  hist_yrs <- 1970:1999

	ROMS_hindcast_dat_dec_hist <- ROMS_hindcast_dat %>% filter(., year %in% hist_yrs)

	# number of years
	no_yrs <- length(unique(ROMS_hindcast_dat_dec_hist$year))

	# annual average
	ROMS_hindcast_dat_dec_hist_yr_sum <- ROMS_hindcast_dat_dec_hist %>%
		group_by(latitude, longitude, year) %>%
		summarise(mean_sphabsuit = mean(sp_hab_suit))

	# 0.5
	ROMS_hindcast_dat_dec_hist_yr_sum_05 <- ROMS_hindcast_dat_dec_hist_yr_sum %>%
		group_by(latitude, longitude) %>%
		dplyr::summarize(no_yrs = length(which(mean_sphabsuit >= 0.5))) %>%
		mutate(year_tot = 20,
					 pct_yrs = (no_yrs/year_tot) * 100)

	ROMS_hindcast_dat_dec_hist_yr_sum_05_sf <- ROMS_hindcast_dat_dec_hist_yr_sum_05 %>%
			mutate(long_not_360 = longitude - 360) %>%
	  	st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326)
  
	# 0.9
	ROMS_hindcast_dat_dec_hist_yr_sum_09 <- ROMS_hindcast_dat_dec_hist_yr_sum %>%
		group_by(latitude, longitude) %>%
		dplyr::summarize(no_yrs = length(which(mean_sphabsuit >= 0.9))) %>%
		mutate(year_tot = 20,
					 pct_yrs = (no_yrs/year_tot) * 100)

	ROMS_hindcast_dat_dec_hist_yr_sum_09_sf <- ROMS_hindcast_dat_dec_hist_yr_sum_09 %>%
			mutate(long_not_360 = longitude - 360) %>%
	  	st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326)
  
	# current period

  # remove 2020 because decade goes from 2010-2019
  current_yrs <- 2001:2020

	ROMS_hindcast_dat_dec_current <- ROMS_hindcast_dat %>% filter(., year %in% current_yrs)

	# number of years
	no_yrs <- length(unique(ROMS_hindcast_dat_dec_current$year))

	# annual average
	ROMS_hindcast_dat_dec_current_yr_sum <- ROMS_hindcast_dat_dec_current %>%
		group_by(latitude, longitude, year) %>%
		summarise(mean_sphabsuit = mean(sp_hab_suit))

	# 0.5
	ROMS_hindcast_dat_dec_current_yr_sum_05 <- ROMS_hindcast_dat_dec_current_yr_sum %>%
		group_by(latitude, longitude) %>%
		dplyr::summarize(no_yrs = length(which(mean_sphabsuit >= 0.5))) %>%
		mutate(year_tot = 20,
					 pct_yrs = (no_yrs/year_tot) * 100)

	ROMS_hindcast_dat_dec_current_yr_sum_05_sf <- ROMS_hindcast_dat_dec_current_yr_sum_05 %>%
			mutate(long_not_360 = longitude - 360) %>%
	  	st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326)
  
	# 0.9
	ROMS_hindcast_dat_dec_current_yr_sum_09 <- ROMS_hindcast_dat_dec_current_yr_sum %>%
		group_by(latitude, longitude) %>%
		dplyr::summarize(no_yrs = length(which(mean_sphabsuit >= 0.9))) %>%
		mutate(year_tot = 20,
					 pct_yrs = (no_yrs/year_tot) * 100)

	ROMS_hindcast_dat_dec_current_yr_sum_09_sf <- ROMS_hindcast_dat_dec_current_yr_sum_09 %>%
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

	#### potential plots ####
	
	hist20_consistency05 <-	
		ggplot() +
		geom_sf(data = ROMS_hindcast_dat_dec_hist_yr_sum_05_sf, aes(color = pct_yrs))  +
		geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
		coord_sf(crs = 3338) +
		scale_color_viridis_c() +
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
	 		legend.position = "none",
			panel.border = element_rect(color = "#666666"),
 			axis.text.x = element_text(size = 8,  color = "#666666"),	
  		axis.title.x = element_text(size = 10,  color = "#666666"),
 			axis.ticks.x = element_line(color = "#666666"),
			axis.text.y = element_blank(),
  		axis.title.y = element_blank(),
 			axis.ticks.y = element_blank(),
 			plot.margin = unit(c(0.05,-0.1, 0, 0), "in"))

	current20_consistency05 <-	
		ggplot() +
		geom_sf(data = ROMS_hindcast_dat_dec_current_yr_sum_05_sf, aes(color = pct_yrs))  +
		geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
		coord_sf(crs = 3338) +
		scale_color_viridis_c() +
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
	 		legend.position = "none",
			panel.border = element_rect(color = "#666666"),
 			axis.text.x = element_text(size = 8,  color = "#666666"),	
  		axis.title.x = element_text(size = 10,  color = "#666666"),
 			axis.ticks.x = element_line(color = "#666666"),
			axis.text.y = element_blank(),
  		axis.title.y = element_blank(),
 			axis.ticks.y = element_blank(),
 			plot.margin = unit(c(0.05,-0.1, 0, 0), "in"))

	
	last20_consistency05 <-	
		ggplot() +
		geom_sf(data = ROMS_projected_dat_yr_sum_05_sf, aes(color = pct_yrs))  +
		geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
		facet_grid(scen_f ~ simulation) +
		coord_sf(crs = 3338) +
		scale_color_viridis_c() +
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
 			legend.position = "none",
 			panel.spacing = unit(2, "pt"),
 			panel.border = element_rect(color = "#666666"),
 			strip.text = element_text(size = 10, face = "bold",  color = "#808080"),
 			strip.background = element_blank(),
 			axis.text.x = element_text(size = 8,  color = "#666666"),	
  		axis.title.x = element_text(size = 10,  color = "#666666"),
 			axis.ticks.y = element_blank(),
 			axis.title.y = element_blank(),
 			axis.text.y = element_blank(),
 			axis.ticks.x = element_line( color = "#666666"),
 			plot.margin = unit(c(0.25,-0.05,-0.05, -0.1), "in"))
	
	# legend
	
	legend <- 	
		ggplot() +
		geom_sf(data = ROMS_hindcast_dat_dec_yr_sum_05_sf, aes(color = pct_yrs))  +
		geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
		coord_sf(crs = 3338) +
		scale_color_viridis_c() +
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
	 		legend.title.align = 0.5,
			panel.border = element_rect(color = "#666666"),
 			axis.text = element_text(size = 8,  color = "#666666"),	
  		axis.title = element_text(size = 10,  color = "#666666"),
 			axis.ticks = element_line(color = "#666666"),
 			plot.margin = unit(c(0.05,-0.1, 0, 0), "in"))
	
	legend_plot <- cowplot::get_legend(legend) 
  legend_plot <- 	ggpubr::as_ggplot(legend_plot) 

  #### core plots ####

  # historical
  hist20_consistency09 <-	
		ggplot() +
		geom_sf(data = ROMS_hindcast_dat_dec_hist_yr_sum_09_sf, aes(color = pct_yrs))  +
		geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
		coord_sf(crs = 3338) +
		scale_color_viridis_c() +
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
	 		legend.position = "none",
			panel.border = element_rect(color = "#666666"),
 			axis.text = element_text(size = 8,  color = "#666666"),	
  		axis.title = element_text(size = 10,  color = "#666666"),
 			axis.ticks = element_line(color = "#666666"),
 			plot.margin = unit(c(0.05, 0, 0, 0), "in"))

  # current
	current20_consistency09 <-	
		ggplot() +
		geom_sf(data = ROMS_hindcast_dat_dec_yr_sum_09_sf, aes(color = pct_yrs))  +
		geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
		coord_sf(crs = 3338) +
		scale_color_viridis_c() +
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
	 		legend.position = "none",
			panel.border = element_rect(color = "#666666"),
 			axis.text.x = element_text(size = 8,  color = "#666666"),	
  		axis.title.x = element_text(size = 10,  color = "#666666"),
 			axis.ticks.y = element_blank(),
 			axis.title.y = element_blank(),
 			axis.text.y = element_blank(),
 			plot.margin = unit(c(0.05, -0.05, 0, 0), "in"))

	
	last20_consistency09 <-	
		ggplot() +
		geom_sf(data = ROMS_projected_dat_yr_sum_09_sf, aes(color = pct_yrs))  +
		geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
		facet_grid(scen_f ~ simulation) +
		coord_sf(crs = 3338) +
		scale_color_viridis_c() +
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
 			legend.position = "none",
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
	
	# legend
	
	legend <- 	
		ggplot() +
		geom_sf(data = ROMS_hindcast_dat_dec_yr_sum_09_sf, aes(color = pct_yrs))  +
		geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
		coord_sf(crs = 3338) +
		scale_color_viridis_c(breaks = c(0,25,50,75,100)) +
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
	 		legend.title.align = 0.5,
			panel.border = element_rect(color = "#666666"),
 			axis.text = element_text(size = 8,  color = "#666666"),	
  		axis.title = element_text(size = 10,  color = "#666666"),
 			axis.ticks = element_line(color = "#666666"),
 			plot.margin = unit(c(0.05,-0.1, 0, 0), "in"))
	
	legend_plot <- cowplot::get_legend(legend) 
  legend_plot <- 	ggpubr::as_ggplot(legend_plot) 



	#### plot together  ####
  consis_maps <-   
   	hist20_consistency09 + 
  	current20_consistency09 +
  	last20_consistency09 +
  	hist20_consistency05 + 
		current20_consistency05 + 
  	last20_consistency05 +
  	plot_spacer() +
  	legend_plot +
  	plot_spacer() +
 		plot_layout(ncol = 9, widths = c(1.3, 1.3, 4, 1.3, 1.3, 4, 0, 0.25, 0))
 				
  ggsave("./output/plots/consis_maps.png",
		consis_maps,
		height = 5,
		width = 20)
 								
  consis_maps_form <- consis_maps + 
  	annotate("text", label = "Historical\n(1970 - 1999)", 
  			x = -14.78, y = 0.68, size = 3, fontface = 2)  +
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

  ggsave("./output/plots/consis_maps_form.png",
		consis_maps_form,
			 height = 5,
			 width = 20)
 
  ### experiment with layout
  
   consis_maps_core <-   
   	hist20_consistency09 + 
		current20_consistency09 + 
  	last20_consistency09 +
   	plot_layout(ncol = 3, widths = c(1.3, 1.3, 4))
   	
   consis_maps_pot <-
  	hist20_consistency05 + 
		current20_consistency05 + 
  	last20_consistency05 +
  	legend_plot 
  	
  	
 		plot_layout(ncol = 7, widths = c(1.3, 1.3, 4, 1.3, 1.3, 4, 0.75))
 				
  ggsave("./output/plots/consis_maps.png",
		consis_maps)
 	