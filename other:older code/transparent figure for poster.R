# figures for poster

 # theme 

	time_series_plot_theme <- function(t = 0,
																		 r = 0,
																		 b = 0,
																		 l = 0){
			theme(
				strip.background = element_blank(),
				strip.text = element_blank(),
				panel.spacing = unit(0, "lines"),
  			legend.position = "none",
  			panel.border = element_rect(fill = NA, color = "black"),
			  panel.grid.major = element_blank(),
  			panel.grid.minor = element_blank(),
				panel.background = element_rect(fill = "transparent", color = NA),
				plot.background = element_rect(fill = "transparent", color = NA),
  			axis.text = element_text(size = 16),
  			axis.ticks.y = element_line(),
				axis.ticks.x = element_blank(),
  			axis.line = element_blank(),
  			axis.title =  element_blank(),
				plot.margin = margin(t,r,b,l, "cm"))
	}


	light_black <- "#306364"
	
	
	# test plot
	#test_plot <-
	#	ggplot(yearly_temp_hind) +
	#	geom_line(aes(x = year, y = avg_temp)) +
	#	time_series_plot_theme()
	#	
	#	
	#	theme(
  #			panel.border = element_rect(fill = NA, color = "grey50"),
	#		  panel.grid.major = element_blank(),
  #			panel.grid.minor = element_blank(),
	#			panel.background = element_rect(fill = "transparent", color = NA),
	#			plot.background = element_rect(fill = "transparent", color = NA))
	#
	#ggsave(test_plot, file = here("./output/plots/test_transparent.png"))
	
	# temperature ####
	
	colors <- c("#579C80", "#ff7373", # cesm low, cesm high
						  "#3e707c", "#ff4040", # gfdl low, gfdl high
						  "#6988a0", "#ffb733") # miroc low, miroc high

	sim_proj <- unique(yearly_temp_proj$sim_proj)
	
	names(colors) <- unique(yearly_temp_proj$sim_proj)
	
	# order facets
	yearly_temp_proj$scen_f = factor(yearly_temp_proj$scen, 
																	 levels=c('low emission (SSP126)', 
																						'high emission (SSP585)'))

	temp_plot <- 
	 	ggplot(yearly_temp_proj, aes(year, avg_temp)) +
		geom_line(data = rolling_mean_temp_hind, 
   						aes(x = years_hind, y = means_hind), 
   						color = light_black, size = 1) +
		geom_line(data = rolling_mean_temp_proj, 
   						aes(x = years_proj, y = means_proj), 
   						color = light_black, size = 1) +
   	geom_line(data = yearly_temp_hind, 
   						aes(x = year, y = avg_temp), 
   						color = "black", alpha = 0.3) +
		geom_line(data = yearly_temp_proj,
							aes(year, avg_temp, 
									group = sim_proj, 
									color = sim_proj), alpha = 0.3) +
		facet_wrap(~ scen_f) +
		xlab("Year") + 
		geom_segment(x = 2020, y = -1, xend = 2020, yend = 4.2,
								 color = "grey", size = 0.5) +
		scale_color_manual(name = "sim_proj", values = colors) +
	  scale_y_continuous(
	  	name = "Temperature (˚C)",
	  	breaks = c(0, 2, 4),
	  	labels = c(0, 2, 4),
	  	limits = c(-0.8, 4.5)
	  ) +
		scale_x_continuous(
	  	name = "Year",
	  	breaks = c(1980, 2030, 2080),
	  		  	limits = c(1970, 2110)) +
		time_series_plot_theme() +
		theme(axis.text.x = element_blank())
	

	ggsave(temp_plot, file = here("./output/plots/temp_plot.png"),
				 width = 10, height = 3, units = "in", dpi = 300, limitsize = FALSE)


	# habitat suitability plot ####
	
	colors <- c("#579C80", "#ff7373", # cesm low, cesm high
						  "#3e707c", "#ff4040", # gfdl low, gfdl high
						  "#6988a0", "#ffb733") # miroc low, miroc high

	sim_proj <- unique(yearly_hab_dat_proj$sim_proj)
	
	names(colors) <- unique(yearly_hab_dat_proj$sim_proj)
	
	# order facets
	yearly_hab_dat_proj$scen_f = factor(yearly_hab_dat_proj$scen, 
																	 levels=c('low emission (SSP126)', 
																						'high emission (SSP585)'))

	habsuit_plot <- 
	 	ggplot(data = yearly_hab_dat_proj, aes(x = year, y = mean_hab_suit)) +
		geom_line(data = rolling_mean_habsuit_hind, 
   						aes(x = years_hind, y = means_hind), 
   						color = light_black, size = 1) +
		geom_line(data = rolling_mean_habsuit_proj, 
   						aes(x = years_proj, y = means_proj), 
   						color = light_black, size = 1) +
   	geom_line(data = yearly_hab_dat_hind, 
   						aes(x = year, y = mean_hab_suit), 
   						color = "black", alpha = 0.3) +
		geom_line(data = yearly_hab_dat_proj,
							aes(year, mean_hab_suit, 
									group = sim_proj, 
									color = sim_proj), alpha = 0.3) +
		facet_wrap(~ scen_f) +
		xlab("Year") + 
		geom_segment(x = 2020, y = 0.1, xend = 2020, yend = 0.6,
				color = "grey", size = 0.5) +		
		scale_color_manual(name = "sim_proj", values = colors) +
	  scale_y_continuous(
	  	name = "Spawning habitat\nsuitability index",
	  	breaks = c(0.20, 0.40, 0.60),
	  	labels = c(0.20, 0.40, 0.60),
	  	limits = c(0.2, 0.65)
	  ) +
		scale_x_continuous(
	  	name = "Year",
	  	breaks = c(1980, 2030, 2080),
	  		  	limits = c(1970, 2110)) +
		time_series_plot_theme() +
		theme(axis.text.x = element_blank())

		ggsave(habsuit_plot, file = here("./output/plots/habsuit_plot.png"),
				 width = 10, height = 3, units = "in", dpi = 300, limitsize = FALSE)


		
	# area plots ####
	
	colors <- c("#579C80", "#ff7373", # cesm low, cesm high
					  "#3e707c", "#ff4040", # gfdl low, gfdl high
					  "#6988a0", "#ffb733") # miroc low, miroc high

	sim_proj <- unique(proj_area_yr$sim_proj)
	
	names(colors) <- unique(proj_area_yr$sim_proj)
	
	# order factors for plotting
	proj_area_yr$scen_f = factor(proj_area_yr$scen, 
													levels=c('low emission\n(SSP126)', 'high emission\n(SSP585)'))
	
	rolling_area_proj$scen_f = factor(rolling_area_proj$scen, 
																	levels=c('low emission\n(SSP126)', 'high emission\n(SSP585)'))
	
	area_plot <-    
   	ggplot(data = proj_area_yr, aes(year, area)) +
		geom_line(data = rolling_area_hind, 
   						aes(x = year, y = area), 
   						color = light_black, size = 1) +
		geom_line(data = rolling_area_proj, 
   						aes(x = year, y = area), 
   						color = light_black, size = 1) +
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
								 color = "grey", size = 0.5) +
		time_series_plot_theme() +
		theme(axis.text.x = element_blank())
	
		
		ggsave(area_plot, file = here("./output/plots/area_plot.png"),
			 width = 10, height = 6, units = "in", dpi = 300, limitsize = FALSE)

	# mean latitude plot ####
	colors <- c("#579C80", "#ff7373", # cesm low, cesm high
				  "#3e707c", "#ff4040", # gfdl low, gfdl high
				  "#6988a0", "#ffb733") # miroc low, miroc high

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
															 levels=c('low emission\n(SSP126)', 'high emission\n(SSP585)'))
	rolling_mean_lat_proj$scen_f = factor(rolling_mean_lat_proj$scen, 
																		levels=c('low emission\n(SSP126)', 'high emission\n(SSP585)'))
	
	mean_latitude_plot <-    
   	ggplot(proj_mean_lat_yr, 
							aes(year, proj_mean_lat)) +
		geom_line(data = rolling_mean_lat_hind, 
   						aes(x = year, y = mean_lat), 
   						color = light_black, size = 1) +
		geom_line(data = rolling_mean_lat_proj, 
   						aes(x = year, y = mean_lat), 
   						color = light_black, size = 1) +
	 	geom_line(aes(year, hist_mean_lat),
            data = hind_mean_lat_yr %>% filter(sp_hab_threshold == 0.5), color = "black", alpha = 0.3) +
		geom_line(data = proj_mean_lat_yr, 
							aes(year, proj_mean_lat, color = sim_proj, group = sim_proj), alpha = 0.3) +
		facet_grid(factor(thresh) ~ factor(scen_f)) +
	  geom_line(aes(year, hist_mean_lat, colour = sp_hab_threshold),
            data = hind_mean_lat_yr %>% filter(sp_hab_threshold == 0.9), color = "black",  alpha = 0.3) +
		xlab("Year") +
		scale_color_manual(name = "sim_proj", values = colors) +
	  scale_y_continuous(
	  	name = "Mean latitude (˚N)",
	  	breaks = c(56, 57, 58, 59),
	  	labels = c(56, 57, 58, 59),
	  	limits = c(55.4, 59.7)
	  ) +
   	xlim(1970, 2110) +
		geom_segment(x = 2020, y = 55, xend = 2020, yend = 59.3,
								 color = "grey", size = 0.5) +
		time_series_plot_theme() +
  	theme(axis.title.x = element_text(size = 18),
  				axis.text.x = element_text(size = 16))

	ggsave(mean_latitude_plot, file = here("./output/plots/mean_latitude_plot.png"),
			 width = 10, height = 6, units = "in", dpi = 300, limitsize = FALSE)

	