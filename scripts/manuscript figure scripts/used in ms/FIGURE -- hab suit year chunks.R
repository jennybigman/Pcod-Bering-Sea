# figure -- spawning habitat suitability year chunks

	#### with temps bias corrected at monthly, regional level ####

	# format breaks for longitude for these plots
  breaks_x <- c(-170, -160)
	labels_x <- c("-170˚", "-160˚") 
	limits_x <- c(-1400000, -150000)

	# historical period
  hist_yrs <- 1970:1999

	ROMS_hindcast_dat_hist <- ROMS_hindcast_dat %>% filter(., year %in% hist_yrs)

	yearly_hab_dat_hind_hist <- ROMS_hindcast_dat_hist %>%
		group_by(latitude, longitude, long_not_360) %>%
    summarise(mean_hab_suit = mean(sp_hab_suit)) %>%
		st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326, remove = FALSE)

	
  # current period
  current_yrs <- 2001:2020

	ROMS_hindcast_dat_current <- ROMS_hindcast_dat %>% filter(., year %in% current_yrs)

	yearly_hab_dat_hind_current <- ROMS_hindcast_dat_current %>%
		group_by(latitude, longitude, long_not_360) %>%
    summarise(mean_hab_suit = mean(sp_hab_suit)) %>%
		st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326, remove = FALSE)

	
	# last period
	last_yrs <- 2080:2099
	
	ROMS_projected_dat_last <- ROMS_projected_dat %>% filter(., year %in% last_yrs)

	yearly_hab_dat_proj_last <- ROMS_projected_dat_last %>% 
		mutate(long_not_360 = case_when(
					 longitude >= 180 ~ longitude - 360,
					 longitude < 180 ~ longitude)) %>%
		group_by(latitude, longitude, long_not_360, simulation, projection) %>%
   	summarise(mean_hab_suit = mean(sp_hab_suit_var))  %>%
		st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326, remove = FALSE)

	
	# add in scenario as factor
	yearly_hab_dat_proj_last$scen <- NA
	
  yearly_hab_dat_proj_last$scen[yearly_hab_dat_proj_last$projection == "SSP126"] <- "low\nemission\n(SSP126)"
	yearly_hab_dat_proj_last$scen[yearly_hab_dat_proj_last$projection == "SSP585"] <- "high\nemission\n(SSP585)"
	
	yearly_hab_dat_proj_last$scen_f = factor(yearly_hab_dat_proj_last$scen, 
																					levels=c('low\nemission\n(SSP126)', 
																									 'high\nemission\n(SSP585)'))

	yearly_hab_dat_proj_last$scen <- NA
	
  yearly_hab_dat_proj_last$scen[yearly_hab_dat_proj_last$projection == "SSP126"] <- "low\nemission\n(SSP126)"
	yearly_hab_dat_proj_last$scen[yearly_hab_dat_proj_last$projection == "SSP585"] <- "high\nemission\n(SSP585)"
	
	
	yearly_hab_dat_proj_last$scen_f = factor(yearly_hab_dat_proj_last$scen, 
																						 levels=c('low\nemission\n(SSP126)', 
																											'high\nemission\n(SSP585)'))
	
	yearly_hab_dat_proj_last$scen_f = factor(yearly_hab_dat_proj_last$scen, 
																						 levels=c('low\nemission\n(SSP126)', 
																											'high\nemission\n(SSP585)'))


	#### plots ####
	
	#historical
	
	historical20 <-	
		ggplot() +
		geom_sf(data = yearly_hab_dat_hind_hist, aes(color = mean_hab_suit))  +
		geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
		coord_sf(crs = 3338) +
		scale_color_gradientn(
			colors = c("#B3E5FC", "#B3E5FC", 
								 "#01579B", "#01579B",
								 "#00345C", "#00345C"),
			values = c(0, 0.499, 0.5, 0.899, 0.9, 1),
			breaks = c(0.1, 0.5, 0.9),
			labels = format(c(0.1, 0.5, 0.9)),
			limits = c(0, 1)) +
 		scale_x_continuous(
 			breaks = breaks_x,
 			labels = labels_x,
 			name = "Longitude",
 			limits = limits_x
 		) +
 		scale_y_continuous(
 			breaks = breaks_y,
 			limits = limits_y,
 			name = "Latitude",
 		) +
    labs(colour = "Spawning habitat suitability") +
		theme_bw() +
	 		theme(
 			panel.border = element_rect(color = "#666666"),
 			legend.position = "none",
 			axis.text = element_text(size = 8,  color = "#666666"),	
  		axis.title = element_text(size = 10,  color = "#666666"),
 			axis.ticks = element_line(color = "#666666"),
 			plot.margin = unit(c(0.05,-0.1, 0, 0), "in"))
	
	# current
	current20 <-	
		ggplot() +
		geom_sf(data = yearly_hab_dat_hind_current, aes(color = mean_hab_suit))  +
		geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
		coord_sf(crs = 3338) +
		scale_color_gradientn(
			colors = c("#B3E5FC", "#B3E5FC", 
								 "#01579B", "#01579B",
								 "#00345C", "#00345C"),
			values = c(0, 0.499, 0.5, 0.899, 0.9, 1),
			breaks = c(0.1, 0.5, 0.9),
			labels = format(c(0.1, 0.5, 0.9)),
			limits = c(0, 1)) +
 		scale_x_continuous(
 			breaks = breaks_x,
 			labels = labels_x,
 			name = "Longitude",
 			limits = limits_x
 		) +
 		scale_y_continuous(
 			breaks = breaks_y,
 			limits = limits_y,
 			name = "Latitude",
 		) +
    labs(colour = "Spawning habitat suitability") +
		theme_bw() +
	 		theme(
 			panel.border = element_rect(color = "#666666"),
 			legend.position = "none",
 			axis.text = element_text(size = 8,  color = "#666666"),	
  		axis.title = element_text(size = 10,  color = "#666666"),
 			axis.ticks = element_line(color = "#666666"),
 			plot.margin = unit(c(0.05,-0.1, 0, 0), "in"))
	

	# projections
	
	last20 <-	
		ggplot() +
		geom_sf(data = yearly_hab_dat_proj_last, aes(color = mean_hab_suit))  +
		geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
		coord_sf(crs = 3338) +
		facet_grid(scen_f ~ simulation) +
		scale_color_gradientn(
			colors = c("#B3E5FC", "#B3E5FC", 
								 "#01579B", "#01579B",
								 "#00345C", "#00345C"),
			values = c(0, 0.499, 0.5, 0.899, 0.9, 1),
			breaks = c(0.1, 0.5, 0.9),
			labels = format(c(0.1, 0.5, 0.9)),
			limits = c(0, 1)) +
 		scale_x_continuous(
 			breaks = breaks_x,
 			labels = labels_x,
 			name = "Longitude",
 			limits = limits_x
 		) +
 		scale_y_continuous(
 			breaks = breaks_y,
 			limits = limits_y,
 			name = "Latitude",
 		) +
    labs(colour = "Spawning habitat suitability") +
		theme_bw() +
 		theme(
 			panel.spacing = unit(0.04, "in"),
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
		geom_sf(data = yearly_hab_dat_hind_current, aes(color = mean_hab_suit))  +
		geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
		coord_sf(crs = 3338) +
		scale_color_gradientn(
			colors = c("#B3E5FC", "#B3E5FC", 
								 "#01579B", "#01579B",
								 "#00345C", "#00345C"),
			values = c(0, 0.499, 0.5, 0.899, 0.9, 1),
			breaks = c(0.1, 0.5, 0.9),
			labels = format(c(0.1, 0.5, 0.9)),
			limits = c(0, 1)) +
 		scale_x_continuous(
 			breaks = breaks_x,
 			labels = labels_x,
 			name = "Longitude",
 			limits = limits_x
 		) +
 		scale_y_continuous(
 			breaks = breaks_y,
 			limits = limits_y,
 			name = "Latitude",
 		) +
    labs(colour = "Spawning\nhabitat\nsuitability") +
		theme_bw() +
	 		theme(
	 		legend.title.align = 0.5,
	 		legend.box.just = "center",
	 		legend.direction = "vertical",
 			panel.border = element_rect(color = "#666666"),
 			axis.text = element_text(size = 8,  color = "#666666"),	
  		axis.title = element_text(size = 10,  color = "#666666"),
 			axis.ticks = element_line(color = "#666666"),
 			plot.margin = unit(c(0,0,0,0), "in"))
	
  legend_plot <- cowplot::get_legend(legend_plot) 
  legend_plot <- 	ggpubr::as_ggplot(legend_plot) 

  # plot together
  

  hab_suit_maps <- current20 + last20 + legend_plot +
 		plot_layout(ncol = 3, widths = c(1.3,4,1))
    
  hab_suit_maps_form <- hab_suit_maps + 
		annotate("text", label = "Current\n(2001 - 2020)", 
  			x = -5.2, y = 0.83, size = 4, fontface = 2)  +
		annotate("text", label = "Projected (2080 - 2099)",	
						 x = -2.5, y = 1.1, size = 4, fontface = 2)
 
  ggsave("./output/plots/hab_suit_maps_form.png",
			 hab_suit_maps_form,
			 height = 5,
			 width = 10)
  
  
  ## add in two historical panels

  # no y-axis for current years

  current20_noaxis <-	
		ggplot() +
		geom_sf(data = yearly_hab_dat_hind_current, aes(color = mean_hab_suit))  +
		geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
		coord_sf(crs = 3338) +
		scale_color_gradientn(
			colors = c("#B3E5FC", "#B3E5FC", 
								 "#01579B", "#01579B",
								 "#00345C", "#00345C"),
			values = c(0, 0.499, 0.5, 0.899, 0.9, 1),
			breaks = c(0.1, 0.5, 0.9),
			labels = format(c(0.1, 0.5, 0.9)),
			limits = c(0, 1)) +
 		scale_x_continuous(
 			breaks = breaks_x,
 			labels = labels_x,
 			name = "Longitude",
 			limits = limits_x
 		) +
 		scale_y_continuous(
 			breaks = breaks_y,
 			limits = limits_y,
 			name = "Latitude",
 		) +
    labs(colour = "Spawning habitat suitability") +
		theme_bw() +
	 		theme(
 			panel.border = element_rect(color = "#666666"),
 			legend.position = "none",
 			axis.text.x = element_text(size = 8,  color = "#666666"),	
  		axis.title.x = element_text(size = 10,  color = "#666666"),
 			axis.ticks.x = element_line(color = "#666666"),
 			axis.text.y = element_blank(),
  		axis.title.y =  element_blank(),
 			axis.ticks.y =  element_blank(),
 			plot.margin = unit(c(0.05, 0, 0, 0), "in"))
	

  hab_suit_maps <- historical20 + current20_noaxis + last20 + 
  	plot_spacer() +
  	legend_plot +
 		plot_layout(ncol = 5, widths = c(1.3, 1.3, 4, 0, 1))
   
  hab_suit_maps_form <- hab_suit_maps + 
  	annotate("text", label = "Historical\n(1970 - 1999)", 
  			x = -6.8, y = 0.8, size = 4, fontface = 2)  +
		annotate("text", label = "Current\n(2001 - 2020)", 
  			x = -5.5, y = 0.8, size = 4, fontface = 2)  +
		annotate("text", label = "Projected (2080 - 2099)",	
						 x = -2.8, y = 1.05, size = 4, fontface = 2)
 
  ggsave("./output/plots/hab_suit_maps_form.png",
			 hab_suit_maps_form,
			 height = 5,
			 width = 10)
  
  	
	 ggsave("./scripts/manuscript figure scripts/used in ms/pngs of figs/Figure4.png",
			 hab_suit_maps_form,
			 height = 5,
			 width = 10)

  
  #### with temps bias corrected at weekly, grid cell level ####

	# format breaks for longitude for these plots
  breaks_x <- c(-170, -160)
	labels_x <- c("-170˚", "-160˚") 
	limits_x <- c(-1400000, -150000)
	
	# last period
	last_yrs <- 2080:2099
	
	ROMS_projected_dat_last <- proj_mo_dfs %>% filter(., year %in% last_yrs)

	yearly_hab_dat_proj_last <- ROMS_projected_dat_last %>% 
		mutate(long_not_360 = case_when(
					 longitude >= 180 ~ longitude - 360,
					 longitude < 180 ~ longitude)) %>%
		group_by(latitude, longitude, long_not_360, simulation, scenario) %>%
   	summarise(mean_hab_suit = mean(sp_hab_suit))  %>%
		st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326, remove = FALSE)

	
	# add in scenario as factor
	yearly_hab_dat_proj_last$scen <- NA
	
  yearly_hab_dat_proj_last$scen[yearly_hab_dat_proj_last$scenario == "ssp126"] <- "low\nemission\n(SSP126)"
	yearly_hab_dat_proj_last$scen[yearly_hab_dat_proj_last$scenario == "ssp585"] <- "high\nemission\n(SSP585)"
	
	yearly_hab_dat_proj_last$scen_f = factor(yearly_hab_dat_proj_last$scen, 
																					levels=c('low\nemission\n(SSP126)', 
																									 'high\nemission\n(SSP585)'))

	yearly_hab_dat_proj_last$scen <- NA
	
  yearly_hab_dat_proj_last$scen[yearly_hab_dat_proj_last$scenario == "ssp126"] <- "low\nemission\n(SSP126)"
	yearly_hab_dat_proj_last$scen[yearly_hab_dat_proj_last$scenario == "ssp585"] <- "high\nemission\n(SSP585)"
	
	
	yearly_hab_dat_proj_last$scen_f = factor(yearly_hab_dat_proj_last$scen, 
																						 levels=c('low\nemission\n(SSP126)', 
																											'high\nemission\n(SSP585)'))
	
	yearly_hab_dat_proj_last$scen_f = factor(yearly_hab_dat_proj_last$scen, 
																						 levels=c('low\nemission\n(SSP126)', 
																											'high\nemission\n(SSP585)'))


	#### plots ####
	

	# projections
	
	last20_hbsuit_wkgc <-	
		ggplot() +
		geom_sf(data = yearly_hab_dat_proj_last, aes(color = mean_hab_suit))  +
		geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
		coord_sf(crs = 3338) +
		facet_grid(scen_f ~ simulation) +
		scale_color_gradientn(
			colors = c("#B3E5FC", "#B3E5FC", 
								 "#01579B", "#01579B",
								 "#00345C", "#00345C"),
			values = c(0, 0.499, 0.5, 0.899, 0.9, 1),
			breaks = c(0.1, 0.5, 0.9),
			labels = format(c(0.1, 0.5, 0.9)),
			limits = c(0, 1)) +
 		scale_x_continuous(
 			breaks = breaks_x,
 			labels = labels_x,
 			name = "Longitude",
 			limits = limits_x
 		) +
 		scale_y_continuous(
 			breaks = breaks_y,
 			limits = limits_y,
 			name = "Latitude",
 		) +
    labs(colour = "Spawning habitat suitability") +
		theme_bw() +
 		theme(
 			panel.spacing = unit(0.04, "in"),
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
  
  
  ggsave(here("./scripts/with weekly grid cell level bias correct temps/last20_hbsuit_wkgc.png"),
			 last20_hbsuit_wkgc,
			 height = 5,
			 width = 10)
 
 