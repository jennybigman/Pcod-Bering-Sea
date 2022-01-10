#### projections ####

# mapping years for which spawning habitat suitability >= 0.5

# annual average
ROMS_projected_dat_yr_sum <- ROMS_projected_dat %>%
	group_by(simulation, projection, year, latitude, longitude) %>%
	summarise(mean_sphabsuit = mean(sp_hab_suit_var))

# code below is missing last 5 years because historical is 1980 - 2014 

years_map <- 1985:2094

ROMS_projected_dat_yr_sum <- ROMS_projected_dat_yr_sum %>%
	filter(., year %in% years_map) %>%
	mutate(decade = case_when(
		between(year, 1985, 1994) ~ "1985 - 1994",
		between(year, 1995, 2004) ~ "1995 - 2004",
		between(year, 2005, 2014) ~ "2005 - 2014",
		between(year, 2015, 2024) ~ "2015 - 2024",
		between(year, 2025, 2034) ~ "2025 - 2034",
		between(year, 2035, 2044) ~ "2035 - 2044",
		between(year, 2045, 2054) ~ "2045 - 2054",
		between(year, 2055, 2064) ~ "2055 - 2064",
		between(year, 2065, 2074) ~ "2065 - 2074",
		between(year, 2075, 2084) ~ "2075 - 2084",
		between(year, 2085, 2094) ~ "2085 - 2094",
		))


 # 05 

ROMS_projected_dat_yr_sum_05 <- ROMS_projected_dat_yr_sum %>%
	group_by(simulation, projection, latitude, longitude, decade) %>%
	summarize(no_yrs = length(which(mean_sphabsuit >= 0.5))) %>%
	mutate(year_tot = 10,
				 pct_yrs = (no_yrs/year_tot) * 100)

ROMS_projected_dat_yr_sum_05_sf <- ROMS_projected_dat_yr_sum_05 %>%
		mutate(long_not_360 = longitude - 360) %>%
  	st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326)
  

sims_hist_cons05 <- ROMS_projected_dat_yr_sum_05_sf %>%
	filter(projection == "historical")

sims_ssp126_cons05 <- ROMS_projected_dat_yr_sum_05_sf %>%
	filter(projection == "ssp126")

sims_ssp585_cons05 <- ROMS_projected_dat_yr_sum_05_sf %>%
	filter(projection == "ssp585")


# 09

ROMS_projected_dat_yr_sum_09 <- ROMS_projected_dat_yr_sum %>%
	group_by(simulation, projection, latitude, longitude, decade) %>%
	summarize(no_yrs = length(which(mean_sphabsuit >= 0.9))) %>%
	mutate(year_tot = 10,
				 pct_yrs = (no_yrs/year_tot) * 100)

ROMS_projected_dat_yr_sum_09_sf <- ROMS_projected_dat_yr_sum_09 %>%
		mutate(long_not_360 = longitude - 360) %>%
  	st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326)
  

sims_hist_cons09 <- ROMS_projected_dat_yr_sum_09_sf %>%
	filter(projection == "historical")

sims_ssp126_cons09 <- ROMS_projected_dat_yr_sum_09_sf %>%
	filter(projection == "ssp126")

sims_ssp585_cons09 <- ROMS_projected_dat_yr_sum_09_sf %>%
	filter(projection == "ssp585")

# plots

# historical 

hist_plot_cons_05 <- 
  		ggplot() +
					geom_sf(data = sims_hist_cons05, 
									aes(color = pct_yrs))  +
					geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
					coord_sf(crs = 3338) +
  		    facet_grid(simulation ~ decade ) +
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
					ggtitle(label = paste("Percent of years for which\nspawning habitat suitability", 
																symbol, "0.5", sep =" ")) +
					theme_bw() +
 					theme(
 						strip.text = element_text(size = 14, face = "bold"),
 						strip.background = element_blank(),
 						axis.text = element_text(size = 12),	
  					axis.title = element_text(size = 14),
  					legend.title = element_text(hjust = 0.5, size = 12),
 						plot.title = element_text(size = 16, face = "bold")
 						)
  	

  	ggsave("./output/plots/hist_plot_cons_05.png",
		hist_plot_cons_05,
		width = 13, height = 7, units = "in")
  
  	#09
  	
  hist_plot_cons_09 <- 
  		ggplot() +
					geom_sf(data = sims_hist_cons09, 
									aes(color = pct_yrs))  +
					geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
					coord_sf(crs = 3338) +
  		    facet_grid(simulation ~ decade ) +
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
					ggtitle(label = paste("Percent of years for which\nspawning habitat suitability", 
																symbol, "0.9", sep =" ")) +
					theme_bw() +
 					theme(
 						strip.text = element_text(size = 14, face = "bold"),
 						strip.background = element_blank(),
 						axis.text = element_text(size = 12),	
  					axis.title = element_text(size = 14),
  					legend.title = element_text(hjust = 0.5, size = 12),
 						plot.title = element_text(size = 16, face = "bold")
 						)
  	

  	ggsave("./output/plots/hist_plot_cons_09.png",
		hist_plot_cons_09,
		width = 13, height = 7, units = "in")
 
  	# ssp126
  	
ssp126_plot_cons_05 <- 
  		ggplot() +
					geom_sf(data = sims_ssp126_cons05, 
									aes(color = pct_yrs))  +
					geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
					coord_sf(crs = 3338) +
  		    facet_grid(simulation ~ decade ) +
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
					ggtitle(label = paste("Percent of years for which\nspawning habitat suitability", 
																symbol, "0.5", sep =" ")) +
					theme_bw() +
 					theme(
 						strip.text = element_text(size = 14, face = "bold"),
 						strip.background = element_blank(),
 						axis.text = element_text(size = 12),	
  					axis.title = element_text(size = 14),
  					legend.title = element_text(hjust = 0.5, size = 12),
 						plot.title = element_text(size = 16, face = "bold")
 						)
  	

  	ggsave("./output/plots/ssp126_plot_cons_05.png",
		ssp126_plot_cons_05,
		width = 13, height = 7, units = "in")
  
  	#09
  	
  ssp126_plot_cons_09 <- 
  		ggplot() +
					geom_sf(data = sims_ssp126_cons09, 
									aes(color = pct_yrs))  +
					geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
					coord_sf(crs = 3338) +
  		    facet_grid(simulation ~ decade ) +
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
					ggtitle(label = paste("Percent of years for which\nspawning habitat suitability", 
																symbol, "0.9", sep =" ")) +
					theme_bw() +
 					theme(
 						strip.text = element_text(size = 14, face = "bold"),
 						strip.background = element_blank(),
 						axis.text = element_text(size = 12),	
  					axis.title = element_text(size = 14),
  					legend.title = element_text(hjust = 0.5, size = 12),
 						plot.title = element_text(size = 16, face = "bold")
 						)
  	

  	ggsave("./output/plots/ssp126_plot_cons_09.png",
		ssp126_plot_cons_09,
		width = 13, height = 7, units = "in")
  	
  	# ssp585
  	
ssp585_plot_cons_05 <- 
  		ggplot() +
					geom_sf(data = sims_ssp585_cons05, 
									aes(color = pct_yrs))  +
					geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
					coord_sf(crs = 3338) +
  		    facet_grid(simulation ~ decade ) +
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
					ggtitle(label = paste("Percent of years for which\nspawning habitat suitability", 
																symbol, "0.5", sep =" ")) +
					theme_bw() +
 					theme(
 						strip.text = element_text(size = 14, face = "bold"),
 						strip.background = element_blank(),
 						axis.text = element_text(size = 12),	
  					axis.title = element_text(size = 14),
  					legend.title = element_text(hjust = 0.5, size = 12),
 						plot.title = element_text(size = 16, face = "bold")
 						)
  	

  	ggsave("./output/plots/ssp585_plot_cons_05.png",
		ssp585_plot_cons_05,
		width = 13, height = 7, units = "in")
  
  	#09
  	
  ssp585_plot_cons_09 <- 
  		ggplot() +
					geom_sf(data = sims_ssp585_cons09, 
									aes(color = pct_yrs))  +
					geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
					coord_sf(crs = 3338) +
  		    facet_grid(simulation ~ decade ) +
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
					ggtitle(label = paste("Percent of years for which\nspawning habitat suitability", 
																symbol, "0.9", sep =" ")) +
					theme_bw() +
 					theme(
 						strip.text = element_text(size = 14, face = "bold"),
 						strip.background = element_blank(),
 						axis.text = element_text(size = 12),	
  					axis.title = element_text(size = 14),
  					legend.title = element_text(hjust = 0.5, size = 12),
 						plot.title = element_text(size = 16, face = "bold")
 						)
  	

  	ggsave("./output/plots/ssp585_plot_cons_09.png",
		ssp585_plot_cons_09,
		width = 13, height = 7, units = "in")
 
 