	# consistency

	#### hindcasts ####

	# mapping years for which spawning habitat suitability >= 0.5
	
	# annual average
	ROMS_hindcast_dat_dec_yr_sum <- ROMS_hindcast_dat_dec %>%
		group_by(latitude, longitude, year) %>%
		summarise(mean_sphabsuit = mean(sp_hab_suit))

	# 0.5
	ROMS_hindcast_dat_dec_yr_sum_05 <- ROMS_hindcast_dat_dec_yr_sum %>%
		group_by(latitude, longitude) %>%
		summarize(no_yrs = length(which(mean_sphabsuit >= 0.5))) %>%
		mutate(year_tot = 50,
					 pct_yrs = (no_yrs/year_tot) * 100)

	ROMS_hindcast_dat_dec_yr_sum_05_sf <- ROMS_hindcast_dat_dec_yr_sum_05 %>%
			mutate(long_not_360 = longitude - 360) %>%
	  	st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326)

	# 0.9
	ROMS_hindcast_dat_dec_yr_sum_09 <- ROMS_hindcast_dat_dec_yr_sum %>%
		group_by(latitude, longitude) %>%
		summarize(no_yrs = length(which(mean_sphabsuit >= 0.9))) %>%
		mutate(year_tot = 50,
					 pct_yrs = (no_yrs/year_tot) * 100)

	ROMS_hindcast_dat_dec_yr_sum_09_sf <- ROMS_hindcast_dat_dec_yr_sum_09 %>%
			mutate(long_not_360 = longitude - 360) %>%
	  	st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326)
	
	# plots for X years for summary
	
	early_plot_core <- 
	  		ggplot() +
						geom_sf(data = ROMS_hindcast_dat_dec_yr_sum_09_sf, 
										aes(color = pct_yrs))  +
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
						ggtitle("Core: 1970 - 2019") +
						labs(subtitle = "") +
						theme_bw() +
							theme(
 							axis.text = element_text(size = 12),	
  						axis.title = element_text(size = 14),
  						legend.position = "none",
 							plot.title = element_text(size = 15.5, hjust = 0.5)
 						)
 						
		ggsave(here("./output/plots/early_plot_core.png"),
			 early_plot_core,
			 width = 5, height = 5, units = "in")

	
		early_plot_potential <- 
	  		ggplot() +
						geom_sf(data = ROMS_hindcast_dat_dec_yr_sum_05_sf, 
										aes(color = pct_yrs))  +
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
						ggtitle("Potential: 1970 - 2019") +
						labs(subtitle = "") +
						theme_bw() +
						theme(
 							axis.text = element_text(size = 12),	
  						axis.title = element_text(size = 14),
  						legend.position = "none",
 							plot.title = element_text(size = 15.5, hjust = 0.5)
 						)
		
							
		ggsave(here("./output/plots/early_plot_potential.png"),
			 early_plot_potential,
			 width = 5, height = 5, units = "in")
		
	#### projections ####
	
	# annual average
	ROMS_projected_dat_yr_sum <- ROMS_projected_dat %>%
		group_by(projection, year, latitude, longitude) %>%
		summarise(mean_sphabsuit = mean(sp_hab_suit_var))
	
	# code below is missing last 5 years because historical is 1980 - 2014 

	years_map <- 2050:2099

	ROMS_projected_dat_yr_sum <- ROMS_projected_dat_yr_sum %>%
		filter(., year %in% years_map) 
	
 # 05 

	ROMS_projected_dat_yr_sum_05 <- ROMS_projected_dat_yr_sum %>%
		group_by(projection, latitude, longitude) %>%
		summarize(no_yrs = length(which(mean_sphabsuit >= 0.5))) %>%
		mutate(year_tot = 50,
					 pct_yrs = (no_yrs/year_tot) * 100)
	
	ROMS_projected_dat_yr_sum_05$scen <- NA
	
	ROMS_projected_dat_yr_sum_05$scen[ROMS_projected_dat_yr_sum_05$projection == "ssp126"] <- "low emission (ssp126)"
	ROMS_projected_dat_yr_sum_05$scen[ROMS_projected_dat_yr_sum_05$projection == "ssp585"] <- "high emission (ssp585)"
	
	ROMS_projected_dat_yr_sum_05_sf <- ROMS_projected_dat_yr_sum_05 %>%
			mutate(long_not_360 = longitude - 360) %>%
	  	st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326)
	
	ROMS_projected_dat_yr_sum_05_sf$scen_f = factor(ROMS_projected_dat_yr_sum_05_sf$scen,
																									levels=c('low emission (ssp126)', 'high emission (ssp585)'))


	#09
	
	ROMS_projected_dat_yr_sum_09 <- ROMS_projected_dat_yr_sum %>%
		group_by(projection, latitude, longitude) %>%
		summarize(no_yrs = length(which(mean_sphabsuit >= 0.9))) %>%
		mutate(year_tot = 50,
					 pct_yrs = (no_yrs/year_tot) * 100)
	
	ROMS_projected_dat_yr_sum_09$scen <- NA
	
	ROMS_projected_dat_yr_sum_09$scen[ROMS_projected_dat_yr_sum_09$projection == "ssp126"] <- "low emission (ssp126)"
	ROMS_projected_dat_yr_sum_09$scen[ROMS_projected_dat_yr_sum_09$projection == "ssp585"] <- "high emission (ssp585)"
	
	ROMS_projected_dat_yr_sum_09_sf <- ROMS_projected_dat_yr_sum_09 %>%
			mutate(long_not_360 = longitude - 360) %>%
	  	st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326)
	
	ROMS_projected_dat_yr_sum_09_sf$scen_f = factor(ROMS_projected_dat_yr_sum_09_sf$scen,
																									levels=c('low emission (ssp126)', 'high emission (ssp585)'))


	# plots
	
	late_plot_core <- 
	  		ggplot() +
						geom_sf(data = ROMS_projected_dat_yr_sum_09_sf, 
										aes(color = pct_yrs))  +
						geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
						coord_sf(crs = 3338) +
						scale_color_viridis_c() +
						facet_grid(. ~ scen_f) +
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
						ggtitle("2050 - 2099") +
						theme_bw() +
					theme(
	 						strip.background = element_blank(),
	 						strip.text = element_text(size = 12),
	 						plot.title = element_text(color = "black", size = 16, hjust = 0.5),
  						axis.text.x=element_text(size= 12, colour = "black"),
  						axis.title.x= element_text(size=14, color = "black"),
  						axis.line.x = element_line(color = "black"),
  						axis.ticks.x = element_line(colour = "black"),
	 						axis.text.y=element_blank(),
  						axis.title.y= element_blank(),
  						axis.line.y = element_blank(),
  						axis.ticks.y = element_blank(),
  						legend.title = element_text(color = "black"),
  						legend.text = element_text(color = "black"))
	
							
		ggsave(here("./output/plots/late_plot_core.png"),
			 late_plot_core,
			 width = 10, height = 5, units = "in")
	

	late_plot_potential <- 
	  		ggplot() +
						geom_sf(data = ROMS_projected_dat_yr_sum_05_sf, 
										aes(color = pct_yrs))  +
						geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
						coord_sf(crs = 3338) +
						scale_color_viridis_c() +
						facet_grid(. ~ scen_f) +
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
						ggtitle("2050 - 2099") +
						theme_bw() +
	 					theme(
	 						strip.background = element_blank(),
	 						strip.text = element_text(size = 12),
	 						plot.title = element_text(color = "black", size = 16, hjust = 0.5),
  						axis.text.x=element_text(size= 12, colour = "black"),
  						axis.title.x= element_text(size=14, color = "black"),
  						axis.line.x = element_line(color = "black"),
  						axis.ticks.x = element_line(colour = "black"),
	 						axis.text.y=element_blank(),
  						axis.title.y= element_blank(),
  						axis.line.y = element_blank(),
  						axis.ticks.y = element_blank(),
  						legend.title = element_text(color = "black"),
  						legend.text = element_text(color = "black"),
  						panel.background = element_rect(fill = "white"),
  						plot.background = element_rect(fill = "white", color = "white"))
	
								
		ggsave(here("./output/plots/late_plot_potential.png"),
			 late_plot_potential,
			 width = 10, height = 5, units = "in")
	


	
	# plot together
	
	plot1 <- early_plot_core + theme(plot.margin = unit(c(0.2, -5, 0, 0.2), "in"))
	
	plot2 <- late_plot_core + theme(plot.margin = unit(c(0.2, 0.2, 0, -0.05), "in"))
	
	plot_top <- plot1 + plot2 + plot_layout(widths = c(1, 2))
	
	plot3 <- early_plot_potential + theme(plot.margin = unit(c(0.2, -5, 0, 0.2), "in"))
	
	plot4 <- late_plot_potential + theme(plot.margin = unit(c(0.2, 0.2, 0, -0.05), "in"))
	
	plot_bot <- plot3 + plot4 + plot_layout(widths = c(1, 2))
	
	plot_total <- plot_top/plot_bot
	
	ggsave(here("./output/plots/consistency_summary.png"),
			 plot_total,
			 width = 10, height = 7, units = "in")

  
	
		
		