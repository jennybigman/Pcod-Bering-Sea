# figure -- consistency year chunks
	
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
	
  ROMS_projected_dat_yr_sum_05_sf$scen[ROMS_projected_dat_yr_sum_05_sf$projection == "SSP126"] <- "low\nemission\n(SSP126)"
	ROMS_projected_dat_yr_sum_05_sf$scen[ROMS_projected_dat_yr_sum_05_sf$projection == "SSP585"] <- "high\nemission\n(SSP585)"
	
	ROMS_projected_dat_yr_sum_05_sf$scen_f = factor(ROMS_projected_dat_yr_sum_05_sf$scen, 
																									levels=c('low\nemission\n(SSP126)', 
																											     'high\nemission\n(SSP585)'))

	ROMS_projected_dat_yr_sum_09_sf$scen <- NA
	
  ROMS_projected_dat_yr_sum_09_sf$scen[ROMS_projected_dat_yr_sum_09_sf$projection == "SSP126"] <- "low\nemission\n(SSP126)"
	ROMS_projected_dat_yr_sum_09_sf$scen[ROMS_projected_dat_yr_sum_09_sf$projection == "SSP585"] <- "high\nemission\n(SSP585)"
	
	ROMS_projected_dat_yr_sum_09_sf$scen_f = factor(ROMS_projected_dat_yr_sum_09_sf$scen, 
																									levels=c('low\nemission\n(SSP126)', 
																											     'high\nemission\n(SSP585)'))

	# potential habitat
	
	hist20_consistency05 <-	
		ggplot() +
		geom_sf(data = ROMS_hindcast_dat_dec_hist_yr_sum_05_sf, aes(color = pct_yrs))  +
		geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
		coord_sf(crs = 3338) +
		scale_color_viridis_c() +
 		scale_x_continuous(
 			breaks = c( -170, -160),
 			labels = c("-170˚", "-160˚"),
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
 			plot.margin = unit(c(0.1, 0, 0.1, 0.1), "in"))

	current20_consistency05 <-	
		ggplot() +
		geom_sf(data = ROMS_hindcast_dat_dec_current_yr_sum_05_sf, aes(color = pct_yrs))  +
		geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
		coord_sf(crs = 3338) +
		scale_color_viridis_c() +
 		scale_x_continuous(
 			breaks = c(-170, -160),
 			labels = c("-170˚", "-160˚"),
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
 			plot.margin = unit(c(0.1, 0, 0.1, -0.05), "in"))

	
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
	


	#### plot together ####
	
	#05 Potential habitat 

  consis05_maps <- 
  	hist20_consistency05 + 
  	current20_consistency05 + 
  	last20_consistency05 + 
 		plot_layout(ncol = 4, widths = c(1.3, 1.3, 4 ,1), guides = "collect") 
	
  consis05_maps_form <- consis05_maps +
  	annotate("text", label = "Historical\n(1970 - 1999)", 
  			x = -6.6, y = 0.8, size = 4, fontface = 2)  +
		annotate("text", label = "Current\n(2001 - 2020)", 
  			x = -5.3, y = 0.8, size = 4, fontface = 2)  +
		annotate("text", label = "Projected (2080 - 2099)",	
						 x = -2.6, y = 1.05, size = 4, fontface = 2)
 
  ggsave("./output/plots/consis05_maps_form.png",
			 consis05_maps_form,
			 height = 5,
			 width = 10)


	# core habitat

  # historical
  hist20_consistency09 <-	
		ggplot() +
		geom_sf(data = ROMS_hindcast_dat_dec_hist_yr_sum_09_sf, aes(color = pct_yrs))  +
		geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
		coord_sf(crs = 3338) +
		scale_color_viridis_c() +
 		scale_x_continuous(
 			breaks = c(-170, -160),
 			labels = c( "-170˚","-160˚"),
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
 			plot.margin = unit(c(0.05,-0.1, 0, 0), "in"))

  # current
	current20_consistency09 <-	
		ggplot() +
		geom_sf(data = ROMS_hindcast_dat_dec_current_yr_sum_09_sf, aes(color = pct_yrs))  +
		geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
		coord_sf(crs = 3338) +
		scale_color_viridis_c() +
 		scale_x_continuous(
 			breaks = c(-170, -160),
 			labels = c("-170˚", "-160˚"),
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
 			plot.margin = unit(c(0.05,-0.1, 0, 0), "in"))

	
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
 			panel.spacing = unit(2, "pt"),
 			panel.border = element_rect(color = "#666666"),
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
	
	# legend

	legend <- 	
		ggplot() +
		geom_sf(data = ROMS_hindcast_dat_dec_current_yr_sum_09_sf, aes(color = pct_yrs))  +
		geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
		coord_sf(crs = 3338) +
		scale_color_viridis_c(breaks = c(0,25,50,75,100)) +
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
	 		legend.title.align = 0.5,
			panel.border = element_rect(color = "#666666"),
 			axis.text = element_text(size = 8,  color = "#666666"),	
  		axis.title = element_text(size = 10,  color = "#666666"),
 			axis.ticks = element_line(color = "#666666"),
 			plot.margin = unit(c(0.05,-0.1, 0, 0), "in"))
	
	legend_plot <- cowplot::get_legend(legend) 
  legend_plot <- 	ggpubr::as_ggplot(legend_plot) 


	#### plot together ####
	
 consis09_maps <- 
  	hist20_consistency09 + 
  	current20_consistency09 + 
  	last20_consistency09 + 
 		plot_layout(ncol = 4, widths = c(1.3, 1.3, 4 ,1), guides = "collect") 
	
  consis09_maps_form <- consis09_maps +
  	annotate("text", label = "Historical\n(1970 - 1999)", 
  			x = -6.6, y = 0.8, size = 5, fontface = 2)  +
		annotate("text", label = "Current\n(2001 - 2020)", 
  			x = -5.3, y = 0.8, size = 4, fontface = 2)  +
		annotate("text", label = "Projected (2080 - 2099)",	
						 x = -2.6, y = 1.05, size = 4, fontface = 2)
 
  ggsave("./output/plots/consis09_maps_form.png",
			 consis09_maps_form,
			 height = 5,
			 width = 10)
  
  #### plot both core and potential together
  
  # on top - doesn't work
  
  consistency_sum_maps <- consis09_maps_form/consis05_maps_form
  
  ggsave("./output/plots/consistency_sum_maps.png",
			 consistency_sum_maps,
			 height = 5,
			 width = 10)
  
  # using grid arrange
  test <- grid.arrange(consis09_maps_form, consis05_maps_form, nrow = 2)

  ggsave("./output/plots/test.png",
			 test,
			 height = 5,
			 width = 10)
  
  # using grobs
  core_plot <- as_grob(consis09_maps_form)
  pot_plot <- as_grob(consis05_maps_form)
  
  test <- plot_grid(core_plot, pot_plot, nrow =  2)
  
  ggsave("./output/plots/test.png",
			 test,
			 height = 5,
			 width = 10)
 
  # cow plot
  test <- plot_grid(consis09_maps_form, consis05_maps_form, nrow = 2)
  
  ggsave("./output/plots/test.png",
			 test,
			 height = 5,
			 width = 10)
 
  # on top 
  
  layout <- "
  	##BBBB
  	AABBBB
  	AABBBB
  	##BBBB
  	"
  plot1 <- current20_consistency05
  plot2 <- last20_consistency05
  
  consistency_sum_maps <- (plot1 / plot2) + plot_layout(design = layout, guides = "collect")
  
  ggsave("./output/plots/consistency_sum_maps.png",
		consistency_sum_maps)
  
  # side by side - works
  
  layout <- "
  	##BBBB
  	AABBBB
  	AABBBB
  	##BBBB
  	"
  plot1 <- current20_consistency05
  plot2 <- last20_consistency05
  
  consistency_sum_maps <- plot1 + plot2 + plot_layout(design = layout, guides = "collect")
  
  ggsave("./output/plots/consistency_sum_maps.png",
		consistency_sum_maps)
  
  
  ## side by side - works
  
  consis_maps <- 
   	hist20_consistency09 + current20_consistency09 + last20_consistency09 + legend_plot +
   	hist20_consistency05 + current20_consistency05 + last20_consistency05 +
 		plot_layout(ncol = 4, nrow = 2, widths = c(0.5, 0.5, 5, 0.5, 0.5, 5, 1))
 				
  ggsave("./output/plots/consis_maps.png",
		consis_maps,
		height = 5,
		width = 10)
 				
 	# adding plot spacer
  
   consis_maps <- 
   	hist20_consistency09 + current20_consistency09 + plot_spacer() + last20_consistency09 + legend_plot +
   	hist20_consistency05 + current20_consistency05 + plot_spacer() + last20_consistency05 +
 		plot_layout(ncol = 5, nrow = 2, widths = c(0.5, 0.5, -0.5, 5, 0.5, 0.5, -0.5,5, 1))
 				
  ggsave("./output/plots/consis_maps.png",
		consis_maps,
		height = 5,
		width = 10)
 				
 	