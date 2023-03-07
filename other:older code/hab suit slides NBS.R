# animate habitat suitability plots

	## yearly ###

	years_hind <- c(1980, 2015)
	
	ROMS_hindcast_dat_trim <- ROMS_hindcast_dat %>%
		filter(year %in% years_hind)

	ROMS_hindcast_dat_sum <- ROMS_hindcast_dat_trim %>%
		group_by(year, latitude, long_not_360) %>%
		summarise(mean_sphabsuit = mean(sp_hab_suit))
	
	ROMS_hindcast_dat_sum_sf <- ROMS_hindcast_dat_sum	%>%
  	st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326, remove = FALSE)
	
	hab_suit_yr_plot_func_hind <- function(x){
		
		    new_dat <- ROMS_hindcast_dat_sum_sf %>% filter(., year == x)
    
    	  plot <- 
    	  	ggplot() +
					geom_sf(data = new_dat, aes(color = mean_sphabsuit))  +
					geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
					coord_sf(crs = 3338) +
					scale_color_gradientn(colors = c("#B3E5FC","#B3E5FC",
																					 "#3378af","#3378af",
																					 "#00345C","#00345C"),
																values = c(0, 0.499, 0.5, 0.899, 0.9, 1),
																breaks = c(0.1, 0.5, 0.9),
																labels = c(0.1, 0.5, 0.9),
																limits = c(0, 1)) +
 					scale_x_continuous(
 						breaks = c(-170, -160),
 						labels = c( "-170˚", "-160˚"),
 						name = "Longitude",
 						limits = c(-1400000, -150000)
 					) +
 					scale_y_continuous(
 						breaks = c(55, 60),
 						limits = c(470000, 1900000),
 						name = "Latitude",
 					) +
    	  	labs(colour = "Spawning\nhabitat\nsuitability") +
					theme_bw() +
    	  	ggtitle(paste0(x)) + 
  		theme(
 						strip.text = element_text(size = 14, face = "bold"),
 						strip.background = element_blank(),
 						axis.text = element_text(size = 12),	
  					axis.title = element_text(size = 14),
  					legend.title.align=0.5)
    	  	
    	  plot
    	  
	}
	
	hind_plot_list <- lapply(years_hind, hab_suit_yr_plot_func_hind)
  
	habsuit_hind_1980_plot <- hind_plot_list[[1]]
	habsuit_hind_2015_plot <- hind_plot_list[[2]]
	
	ggsave(here("./output/plots/habsuit_hind_1980_plot_SSC.png"),
			habsuit_hind_1980_plot,
			width = 5, height = 5, units = "in")
	
	ggsave(here("./output/plots/habsuit_hind_2105_plot_SSC.png"),
		habsuit_hind_2015_plot,
		width = 5, height = 5, units = "in")
 	
 	
	# projections ####

	ROMS_projected_dat_trim <- ROMS_projected_dat %>% 
		filter(year == 2099) %>%
			mutate(long_not_360 = case_when(
						 longitude >= 180 ~ longitude - 360,
						 longitude < 180 ~ longitude)) 
	
	ROMS_projected_dat_trim_sum <- ROMS_projected_dat_trim %>%
		group_by(projection, year, latitude, long_not_360) %>%
		summarise(mean_sphabsuit = mean(sp_hab_suit_var))
	
	ROMS_projected_dat_trim_sum_sf <- ROMS_projected_dat_trim_sum	%>%
  	st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326, remove = FALSE)
	
	ROMS_projected_dat_trim_sum_sf$scen <- NA
	
	ROMS_projected_dat_trim_sum_sf$scen[ROMS_projected_dat_trim_sum_sf$projection == "SSP126"] <- "low"
	ROMS_projected_dat_trim_sum_sf$scen[ROMS_projected_dat_trim_sum_sf$projection == "SSP585"] <- "high"
	

    	  plot <- 
    	  	ggplot() +
					geom_sf(data = ROMS_projected_dat_trim_sum_sf, aes(color = mean_sphabsuit))  +
					geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
					coord_sf(crs = 3338) +
    	  	facet_wrap(~projection) +
					scale_color_gradientn(colors = c("#B3E5FC","#B3E5FC",
																					 "#3378af","#3378af",
																					 "#00345C","#00345C"),
																values = c(0, 0.499, 0.5, 0.899, 0.9, 1),
																breaks = c(0.1, 0.5, 0.9),
																labels = c(0.1, 0.5, 0.9),
																limits = c(0, 1)) +
 					scale_x_continuous(
 						breaks = c(-170,-160),
 						labels = c( "-170˚", "-160˚"),
 						name = "Longitude",
 						limits = c(-1400000, -150000)
 					) +
 					scale_y_continuous(
 						breaks = c(55, 60),
 						limits = c(470000, 1900000),
 						name = "Latitude",
 					) +
    	  	labs(colour = "Spawning\nhabitat\nsuitability") +
 					theme_bw() +
	 					theme(
 						strip.text = element_text(size = 20, face = "bold"),
 						strip.background = element_blank(),
 						axis.text = element_text(size = 18),	
  					axis.title = element_text(size = 20),
  					legend.title.align=0.5,
 						legend.title = element_text(size = 18),
 						legend.text = element_text(size = 16))
    	  	
  	
    	  	
    	  plot
    	  

	proj_plot_list <- lapply(years_proj, hab_suit_yr_plot_func)
  
	habsuit_proj_2050_plot <- proj_plot_list[[1]]
	habsuit_proj_2095_plot <- proj_plot_list[[2]]
	
	ggsave(here("./output/plots/habsuit_proj_2050_plot.png"),
			habsuit_proj_2050_plot,
			width = 12, height = 10, units = "in")
	
	ggsave(here("./output/plots/habsuit_proj_2095_plot.png"),
		habsuit_proj_2095_plot,
		width = 12, height = 10, units = "in")
 	
	
    	  plot <- 
    	  	ggplot() +
					geom_sf(data = new_dat, aes(color = mean_sphabsuit))  +
					geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
					coord_sf(crs = 3338) +
    	  	facet_grid(projection) +
					scale_color_gradientn(colors = c("#B3E5FC","#B3E5FC",
																					 "#3378af","#3378af",
																					 "#00345C","#00345C"),
																values = c(0, 0.499, 0.5, 0.899, 0.9, 1),
																breaks = c(0.1, 0.5, 0.9),
																labels = c(0.1, 0.5, 0.9),
																limits = c(0, 1)) +
 					scale_x_continuous(
 						breaks = c(-170,-160),
 						labels = c( "-170˚", "-160˚"),
 						name = "Longitude",
 						limits = c(-1400000, -150000)
 					) +
 					scale_y_continuous(
 						breaks = c(55, 60),
 						limits = c(470000, 1900000),
 						name = "Latitude",
 					) +
    	  	labs(colour = "Spawning\nhabitat\nsuitability") +
 					theme_bw() +
	 					theme(
 						strip.text = element_text(size = 20, face = "bold"),
 						strip.background = element_blank(),
 						axis.text = element_text(size = 18),	
  					axis.title = element_text(size = 20),
  					legend.title.align=0.5,
 						legend.title = element_text(size = 18),
 						legend.text = element_text(size = 16))
    	  	
  	
    	 
	ggsave(here("./output/plots/habsuit_proj_2099_plot.png"),
			plot,
			width = 10, height = 10, units = "in")
	