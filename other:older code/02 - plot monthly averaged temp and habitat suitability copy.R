	# map monthly-averaged temperature and spawning habitat suitability 

	#### hindcasts ####
	
	#### temperature ####
	vars <- c("latitude", "longitude", "Xi", "Eta")

	ROMS_hindcast_temp_dat_sf <- ROMS_hindcast_temp_dat %>%
  	mutate_at(vars, as.numeric) %>%
		mutate(long_not_360 = case_when(
					 longitude >= 180 ~ longitude - 360,
					 longitude < 180 ~ longitude))  %>%
  	st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326, remove = FALSE)
	
	# plots by year
	
	temp_yr_plot_func <- function(x){
		
		    new_dat <- ROMS_hindcast_temp_dat_sf %>% filter(., year == 1990)
    
    	  plot <- 
    	  	ggplot() +
					geom_sf(data = new_dat, aes(color = temp))  +
					geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
					coord_sf(crs = 3338) +
					scale_color_viridis_c() +
 					scale_x_continuous(
 						breaks = breaks_x,
 						labels = breaks_x,
 						name = "Longitude",
 						limits = limits_x
 					) +
 					scale_y_continuous(
 						breaks = breaks_y,
 						limits = limits_y,
 						name = "Latitude",
 					) +
    	  	labs(colour = "Temperature (˚C)") +
					theme_bw() +
    	  	#ggtitle(paste0("Year:", x)) + 
 					theme(
 						strip.text = element_text(size = 14, face = "bold"),
 						strip.background = element_blank(),
 						axis.text = element_text(size = 12),	
  					axis.title = element_text(size = 14),
  					legend.title.align=0.5)
    	  
    	  plot
    	  
	}
	
	years <- c(1970:2020)

	yr_plot_list <- lapply(years, temp_yr_plot_func)
  
  name_func_year <- function(x){
  	year_name <- paste0(x, "_temp2")
  }
   
  names_year <- sapply(years, name_func_year)
  
	name_func_file <- function(x){
  	file_path <- paste0("/Users/jenniferbigman/Dropbox/NOAA AFSC Postdoc/Pcod Project/Pcod Bering Sea Habitat Suitability/Pcod-Bering-Sea/output/plots/monthly plots/bottom temperature/", x)
  }
   
  file_names <- sapply(names_year, name_func_file)
				
	plot_list <- mapply(ggsave_func, x = yr_plot_list, y = file_names)

	# plots by month

		temp_monthly_plot_func <- function(x){
		
		    new_dat <- ROMS_hindcast_dat_sf %>% filter(., year == x)
    
    	  plot <- 
    	  	ggplot() +
					geom_sf(data = new_dat, aes(color = temp))  +
					geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
					coord_sf(crs = 3338) +
					scale_color_viridis_c() +
    	  	facet_wrap(~ month_name, ncol = 2, nrow = 2) +
 					scale_x_continuous(
 						breaks = breaks_x,
 						labels = breaks_x,
 						name = "Longitude",
 						limits = limits_x
 					) +
 					scale_y_continuous(
 						breaks = breaks_y,
 						limits = limits_y,
 						name = "Latitude",
 					) +
    	  	labs(colour = "Temperature (˚C)") +
					theme_bw() +
    	  	ggtitle(paste0("Year:", x)) + 
 					theme(
 						strip.text = element_text(size = 14, face = "bold"),
 						strip.background = element_blank(),
 						axis.text = element_text(size = 12),	
  					axis.title = element_text(size = 14),
  					legend.title.align=0.5)
    	  
    	  plot
    	  
	}
	
	years <- c(1970:2020)

	monthly_plot_list <- lapply(years, temp_monthly_plot_func)
  
  mo_name_func_year <- function(x){
  	year_month <- paste0(x, "_temp")
  }
   
  month_names_year <- sapply(years, mo_name_func_year)
  
	mo_name_func_file <- function(x){
  	year_month <- paste0("/Users/jenniferbigman/Dropbox/NOAA AFSC Postdoc/Pcod Project/Pcod Bering Sea Habitat Suitability/Pcod-Bering-Sea/output/plots/monthly plots/bottom temperature/", x)
  }
   
  month_names <- sapply(month_names_year, mo_name_func_file)
				
	plot_list <- mapply(ggsave_func, x = monthly_plot_list, y = month_names)


  #### map with a color gradient for spawning habitat suitability  ####
  
		hab_suit_monthly_plot_func <- function(x){
		
		    new_dat <- ROMS_hindcast_dat_sf %>% filter(., year == x)
    
    	  plot <- 
    	  	ggplot() +
					geom_sf(data = new_dat, aes(color = sp_hab_suit))  +
					geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
					coord_sf(crs = 3338) +
					scale_color_viridis_c(breaks = c(0, 0.50, 1.0), limits = c(0,1)) +
    	  	facet_wrap(~ month_name, ncol = 2, nrow = 2) +
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
    	  	labs(colour = "Spawning\nhabitat\nsuitability") +
					theme_bw() +
 					theme(
 						strip.text = element_text(size = 14, face = "bold"),
 						strip.background = element_blank(),
 						axis.text = element_text(size = 12),	
  					axis.title = element_text(size = 14),
  					legend.title.align=0.5)
    	  
    	  plot
    	  
	}
	
	years <- c(1970:2020)

	monthly_plot_list <- lapply(years, hab_suit_monthly_plot_func)
  
  mo_name_func_year <- function(x){
  	year_month <- paste0(x, "_habitat_suitability_gradient_check")
  }
   
  month_names_year <- sapply(years, mo_name_func_year)
  
	mo_name_func_file <- function(x){
  	year_month <- paste0("/Users/jenniferbigman/Dropbox/NOAA AFSC Postdoc/Pcod Project/Pcod Bering Sea Habitat Suitability/Pcod-Bering-Sea/output/plots/monthly plots/habitat suitability/habitat suitability gradient/", x)
  }
   
  month_names <- sapply(month_names_year, mo_name_func_file)

	plot_list <- mapply(ggsave_func, x = monthly_plot_list, y = month_names)


  #### map spawning habitat suitability where > 0.9 colored dark blue ####

	hab_suit_monthly_plot_func <- function(x){
		
		    new_dat <- ROMS_hindcast_dat_sf %>% filter(., year == x)
    
    	  plot <- 
    	  	ggplot() +
					geom_sf(data = new_dat, aes(color = sp_hab_suit))  +
					geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
					coord_sf(crs = 3338) +
					scale_color_gradientn(colors = c("#B3E5FC", "#B3E5FC", 
																					 "#01579B", "#01579B"),
																values = c(0, 0.899, 0.9, 1),
																breaks = c(0.1, 0.5, 0.9),
																labels = format(c(0.1, 0.5, 0.9)),
																limits = c(0, 1))  +
    	  	facet_wrap(~ month_name, ncol = 2, nrow = 2) +
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
    	  	labs(colour = "Spawning\nhabitat\nsuitability") +
					theme_bw() +
    	  	ggtitle(paste0("Year:", x)) + 
 					theme(
 						strip.text = element_text(size = 14, face = "bold"),
 						strip.background = element_blank(),
 						axis.text = element_text(size = 12),	
  					axis.title = element_text(size = 14),
  					legend.title.align=0.5)
    	  	
    	  plot
    	  
	}
	
	years <- c(1970:2020)

	monthly_plot_list <- lapply(years, hab_suit_monthly_plot_func)
  

  mo_name_func_year <- function(x){
  	year_month <- paste0(x, "_habitat_suitability_90")
  }
   
  month_names_year <- sapply(years, mo_name_func_year)
  
	mo_name_func_file <- function(x){
  	year_month <- paste0("/Users/jenniferbigman/Dropbox/NOAA AFSC Postdoc/Pcod Project/Pcod Bering Sea Habitat Suitability/Pcod-Bering-Sea/output/plots/monthly plots/habitat suitability/habitat suitability 90/", x)
  }
   
  month_names <- sapply(month_names_year, mo_name_func_file)
				
	plot_list <- mapply(ggsave_func, x = monthly_plot_list, y = month_names)

 
	#### map spawning habitat suitability where > 0.1 < 0.9 and > 0.9 colored different blues ####

		hab_suit_monthly_plot_func <- function(x){
		
		    new_dat <- ROMS_hindcast_dat_sf %>% filter(., year == x)
    
    	  plot <- 
    	  	ggplot() +
					geom_sf(data = new_dat, aes(color = sp_hab_suit))  +
					geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
					coord_sf(crs = 3338) +
					scale_color_gradientn(colors = c("#B3E5FC","#B3E5FC",
																					 "#01579B","#01579B",
																					 "#00345C","#00345C"),
																values = c(0, 0.099, 0.1, 0.899, 0.9, 1),
																breaks = c(0.1, 0.5, 0.9),
																labels = c(0.1, 0.5, 0.9),
																limits = c(0, 1)) +
    	  	facet_wrap(~ month_name, ncol = 2, nrow = 2) +
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
    	  	labs(colour = "Spawning\nhabitat\nsuitability") +
					theme_bw() +
 					theme(
 						strip.text = element_text(size = 14, face = "bold"),
 						strip.background = element_blank(),
 						axis.text = element_text(size = 12),	
  					axis.title = element_text(size = 14),
  					legend.title.align=0.5)
    	  
    	  plot
    	  
	}
	
	years <- c(1970:2020)

	monthly_plot_list <- lapply(years, hab_suit_monthly_plot_func)
  
	mo_name_func_year <- function(x){
  	year_month <- paste0(x, "_habitat_suitability_10")
  }
   
  month_names_year <- sapply(years, mo_name_func_year)
  
	mo_name_func_file <- function(x){
  	year_month <- paste0("/Users/jenniferbigman/Dropbox/NOAA AFSC Postdoc/Pcod Project/Pcod Bering Sea Habitat Suitability/Pcod-Bering-Sea/output/plots/monthly plots/habitat suitability/habitat suitability 10/", x)
  }
   
  month_names <- sapply(month_names_year, mo_name_func_file)
  
	plot_list <- mapply(ggsave_func, x = monthly_plot_list, y = month_names)

	
	#### map spawning habitat suitability where > 0.5 < 0.9 and > 0.9 colored different blues ####
	
		hab_suit_monthly_plot_func <- function(x){
		
		    new_dat <- ROMS_hindcast_dat_sf %>% filter(., year == x)
    
    	  plot <- 
    	  	ggplot() +
					geom_sf(data = new_dat, aes(color = sp_hab_suit))  +
					geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
					coord_sf(crs = 3338) +
					scale_color_gradientn(colors = c("#B3E5FC", "#B3E5FC", 
																					 "#01579B", "#01579B",
																					 "#00345C", "#00345C"),
																values = c(0, 0.499, 0.5, 0.899, 0.9, 1),
																breaks = c(0.1, 0.5, 0.9),
																labels = format(c(0.1, 0.5, 0.9)),
																limits = c(0, 1)) +
    	  	facet_wrap(~ month_name, ncol = 2, nrow = 2) +
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
    	  	labs(colour = "Spawning\nhabitat\nsuitability") +
					theme_bw() +
 					theme(
 						strip.text = element_text(size = 14, face = "bold"),
 						strip.background = element_blank(),
 						axis.text = element_text(size = 12),	
  					axis.title = element_text(size = 14),
  					legend.title.align=0.5)
    	  
    	  plot
    	  
	}
	
	years <- c(1970:2020)

	monthly_plot_list <- lapply(years, hab_suit_monthly_plot_func)
  
  mo_name_func_year <- function(x){
  	year_month <- paste0(x, "_habitat_suitability_5090")
  }
   
  month_names_year <- sapply(years, mo_name_func_year)
  
	mo_name_func_file <- function(x){
  	year_month <- paste0("/Users/jenniferbigman/Dropbox/NOAA AFSC Postdoc/Pcod Project/Pcod Bering Sea Habitat Suitability/Pcod-Bering-Sea/output/plots/monthly plots/habitat suitability/habitat suitability 50_90/", x)
  }
   
  month_names <- sapply(month_names_year, mo_name_func_file)
				
	plot_list <- mapply(ggsave_func, x = monthly_plot_list, y = month_names)

	
	#### projections ####
	
	## all plots/maps below with bias-corrected temp with variance ratio

	# temp ####
	
	temp_var_plot_func <- function(x){
		
		new_dat <- ROMS_projected_dat_sf %>% filter(., year == x)
    
    	plot <- 
    	  	ggplot() +
					geom_sf(data = new_dat, aes(color = bc_temp_sd))  +
					geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
					coord_sf(crs = 3338) +
					scale_color_viridis_c() +
    	  	facet_grid(projection ~ simulation) +
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
    			ggtitle(x) +
    	  	labs(colour = "projected temp") +
					theme_bw() +
 					theme(
 						strip.text = element_text(size = 14, face = "bold"),
 						strip.background = element_blank(),
 						axis.text = element_text(size = 12),	
  					axis.title = element_text(size = 14),
  					legend.title.align=0.5)
    	  
    	  plot
    	  
	}
	
	years <- c(1980:2099)

	plot_list <- lapply(years, temp_var_plot_func)
  
  name_func_year <- function(x){
  	paste0(x, "_proj_temp")
  }
   
  names_year <- sapply(years, name_func_year)
  
	func_file_path <- function(x){
  	paste0("/Users/jenniferbigman/Dropbox/NOAA AFSC Postdoc/Pcod Project/Pcod Bering Sea Habitat Suitability/Pcod-Bering-Sea/output/plots/yearly plots/projected temps/faceted plots/", x)
  }
   
  plot_list_full <- sapply(names_year, func_file_path)
				
	mapply(ggsave_func, x = plot_list, y = plot_list_full)
	

	#### habitat suitability ####

	## var ratio ####
	
	hab_suit_plot_func <- function(x){
		
		    new_dat <- ROMS_projected_dat_sf %>% filter(., year == x)
    
    	  plot <- 
    	  	ggplot() +
					geom_sf(data = new_dat, aes(color = sp_hab_suit_var))  +
					geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
					coord_sf(crs = 3338) +
					scale_color_gradientn(colors = c("#B3E5FC", "#B3E5FC", 
																					 "#01579B", "#01579B",
																					 "#00345C", "#00345C"),
																values = c(0, 0.499, 0.5, 0.899, 0.9, 1),
																breaks = c(0.1, 0.5, 0.9),
																labels = format(c(0.1, 0.5, 0.9)),
																limits = c(0, 1)) +
    	  	facet_grid(projection ~ simulation) +
 					scale_x_continuous(
 						breaks = c(-170, -160),
 						labels = c("-170˚","-160˚"),
 						name = "Longitude",
 						limits = c(-1400000, -150000)
 					) +
 					scale_y_continuous(
 						breaks = c(55, 60),
 						limits = c(470000, 1900000),
 						name = "Latitude",
 					) +
    	  	ggtitle(x) +
    	  	labs(colour = "Spawning\nhabitat\nsuitability") +
					theme_bw() +
 					theme(
 						strip.text = element_text(size = 14, face = "bold"),
 						strip.background = element_blank(),
 						axis.text = element_text(size = 12),	
  					axis.title = element_text(size = 14),
  					legend.title.align=0.5)
    	  
    	  plot
    	  
	}
	
  years <- c(1980:2099)

	plot_list <- lapply(years, hab_suit_plot_func)
  
  name_func_year <- function(x){
  	paste0(x, "_proj_habitat_suitability")
  }
   
  names_year <- sapply(years, name_func_year)
  
	func_file_path <- function(x){
  	paste0("/Users/jenniferbigman/Dropbox/NOAA AFSC Postdoc/Pcod Project/Pcod Bering Sea Habitat Suitability/Pcod-Bering-Sea/output/plots/yearly plots/projected temps/faceted plots/", x)
  }
   
  plot_list_full <- sapply(names_year, func_file_path)
				
	mapply(ggsave_func, x = plot_list, y = plot_list_full)
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	# create a list of separate dfs by simulation
	sims <- list("cesm", "gfdl", "miroc")
	
	sep_dfs_func <- function(x){
		new_dat <- ROMS_projected_dat %>% filter(., simulation == x)
		new_dat
	}
	
	sep_dfs <- lapply(sims, sep_dfs_func)
	
	# convert each df into an sf object
	sf_func <- function(df){
		new_df <- df %>%
			mutate(longitude = Lon,
  					 latitude = Lat,
  					 long_not_360 = case_when(
						 longitude >= 180 ~ longitude - 360,
						 longitude < 180 ~ longitude)) %>%
  	st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326, remove = FALSE)
	
	new_df
		
	}
	
	sep_dfs_sf <- lapply(sep_dfs, sf_func)
	
	# save dfs for mapping -- need to write a loop so that don't have to separate each simulation
	cesm_df <- sep_dfs_sf[[1]]
	gfdl_df <- sep_dfs_sf[[2]]
	miroc_df <- sep_dfs_sf[[3]]
	
	## all plots/maps below with bias-corrected temp with variance ratio

	#### cesm ####
	
	# temp ####
	
	temp_var_monthly_plot_func <- function(x){
		
		new_dat <- cesm_df %>% filter(., year == x)
    
    	plot <- 
    	  	ggplot() +
					geom_sf(data = new_dat, aes(color = bc_temp_sd))  +
					geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
					coord_sf(crs = 3338) +
					scale_color_viridis_c() +
    	  	facet_wrap(~ month_name, ncol = 3, nrow = 3) +
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
    	  	labs(colour = "projected temp no var") +
					theme_bw() +
 					theme(
 						strip.text = element_text(size = 14, face = "bold"),
 						strip.background = element_blank(),
 						axis.text = element_text(size = 12),	
  					axis.title = element_text(size = 14),
  					legend.title.align=0.5)
    	  
    	  plot
    	  
	}
	
	years <- c(1980:2099)

	monthly_plot_list <- lapply(years, temp_var_monthly_plot_func)
  
  mo_name_func_year <- function(x){
  	year_month <- paste0(x, "_proj_temp_var_cesm")
  }
   
  month_names_year <- sapply(years, mo_name_func_year)
  
	mo_name_func_file <- function(x){
  	year_month <- paste0("/Users/jenniferbigman/Dropbox/NOAA AFSC Postdoc/Pcod Project/Pcod Bering Sea Habitat Suitability/Pcod-Bering-Sea/output/plots/monthly plots/habitat suitability/projections/", x)
  }
   
  month_names <- sapply(month_names_year, mo_name_func_file)
				
	plot_list <- mapply(ggsave_func, x = monthly_plot_list, y = month_names)
	

	#### habitat suitability ####

	## var ratio ####
	
	hab_suit_var_monthly_plot_func <- function(x){
		
		    new_dat <- cesm_df %>% filter(., year == x)
    
    	  plot <- 
    	  	ggplot() +
					geom_sf(data = new_dat, aes(color = sp_hab_suit_var))  +
					geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
					coord_sf(crs = 3338) +
					scale_color_gradientn(colors = c("#B3E5FC", "#B3E5FC", 
																					 "#01579B", "#01579B",
																					 "#00345C", "#00345C"),
																values = c(0, 0.499, 0.5, 0.899, 0.9, 1),
																breaks = c(0.1, 0.5, 0.9),
																labels = format(c(0.1, 0.5, 0.9)),
																limits = c(0, 1)) +
    	  	facet_wrap(~ month_name, ncol = 3, nrow = 3) +
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
    	  	labs(colour = "Spawning\nhabitat\nsuitability var") +
					theme_bw() +
 					theme(
 						strip.text = element_text(size = 14, face = "bold"),
 						strip.background = element_blank(),
 						axis.text = element_text(size = 12),	
  					axis.title = element_text(size = 14),
  					legend.title.align=0.5)
    	  
    	  plot
    	  
	}
	
	years <- c(1980:2099)

	monthly_plot_list <- lapply(years, hab_suit_var_monthly_plot_func)
  
  mo_name_func_year <- function(x){
  	year_month <- paste0(x, "_proj_habitat_suitability_var_5090_cesm")
  }
   
  month_names_year <- sapply(years, mo_name_func_year)
  
	mo_name_func_file <- function(x){
  	year_month <- paste0("/Users/jenniferbigman/Dropbox/NOAA AFSC Postdoc/Pcod Project/Pcod Bering Sea Habitat Suitability/Pcod-Bering-Sea/output/plots/monthly plots/habitat suitability/projections/", x)
  }
   
  month_names <- sapply(month_names_year, mo_name_func_file)
				
	plot_list <- mapply(ggsave_func, x = monthly_plot_list, y = month_names)

	
	
	#### gfdl ####

	# temp with variance ratio
	
	temp_var_monthly_plot_func <- function(x){
		
		new_dat <- gfdl_df %>% filter(., year == x)
    
    	plot <- 
    	  	ggplot() +
					geom_sf(data = new_dat, aes(color = bc_temp_sd))  +
					geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
					coord_sf(crs = 3338) +
					scale_color_viridis_c() +
    	  	facet_wrap(~ month_name, ncol = 3, nrow = 3) +
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
    	  	labs(colour = "projected temp no var") +
					theme_bw() +
 					theme(
 						strip.text = element_text(size = 14, face = "bold"),
 						strip.background = element_blank(),
 						axis.text = element_text(size = 12),	
  					axis.title = element_text(size = 14),
  					legend.title.align=0.5)
    	  
    	  plot
    	  
	}
	
	years <- c(1980:2099)

	monthly_plot_list <- lapply(years, temp_var_monthly_plot_func)
  
  mo_name_func_year <- function(x){
  	year_month <- paste0(x, "_proj_temp_var_gfdl")
  }
   
  month_names_year <- sapply(years, mo_name_func_year)
  
	mo_name_func_file <- function(x){
  	year_month <- paste0("/Users/jenniferbigman/Dropbox/NOAA AFSC Postdoc/Pcod Project/Pcod Bering Sea Habitat Suitability/Pcod-Bering-Sea/output/plots/projections/temperature maps/gfdl/temp with var/", x)
  }
   
  month_names <- sapply(month_names_year, mo_name_func_file)
				
	plot_list <- mapply(ggsave_func, x = monthly_plot_list, y = month_names)
	
	
	#### hab suit ####
	
	### with  var ###
	
		hab_suit_var_monthly_plot_func <- function(x){
		
		    new_dat <- gfdl_df %>% filter(., year == x)
    
    	  plot <- 
    	  	ggplot() +
					geom_sf(data = new_dat, aes(color = sp_hab_suit_var))  +
					geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
					coord_sf(crs = 3338) +
					scale_color_gradientn(colors = c("#B3E5FC", "#B3E5FC", 
																					 "#01579B", "#01579B",
																					 "#00345C", "#00345C"),
																values = c(0, 0.499, 0.5, 0.899, 0.9, 1),
																breaks = c(0.1, 0.5, 0.9),
																labels = format(c(0.1, 0.5, 0.9)),
																limits = c(0, 1)) +
    	  	facet_wrap(~ month_name, ncol = 3, nrow = 3) +
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
    	  	labs(colour = "Spawning\nhabitat\nsuitability") +
					theme_bw() +
 					theme(
 						strip.text = element_text(size = 14, face = "bold"),
 						strip.background = element_blank(),
 						axis.text = element_text(size = 12),	
  					axis.title = element_text(size = 14),
  					legend.title.align=0.5)
    	  
    	  plot
    	  
	}
	
	years <- c(1980:2099)

	monthly_plot_list <- lapply(years, hab_suit_var_monthly_plot_func)
  
  mo_name_func_year <- function(x){
  	year_month <- paste0(x, "_proj_habitat_suitability_var_5090_gfdl")
  }
   
  month_names_year <- sapply(years, mo_name_func_year)
  
	mo_name_func_file <- function(x){
  	year_month <- paste0("/Users/jenniferbigman/Dropbox/NOAA AFSC Postdoc/Pcod Project/Pcod Bering Sea Habitat Suitability/Pcod-Bering-Sea/output/plots/projections/habitat suitability maps/gfdl/with var/", x)
  }
   
  month_names <- sapply(month_names_year, mo_name_func_file)
				
	plot_list <- mapply(ggsave_func, x = monthly_plot_list, y = month_names)

	
	#### miroc ####
	
	# temp with variance ratio
	
	temp_var_monthly_plot_func <- function(x){
		
		new_dat <- miroc_df %>% filter(., year == x)
    
    	plot <- 
    	  	ggplot() +
					geom_sf(data = new_dat, aes(color = bc_temp_sd))  +
					geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
					coord_sf(crs = 3338) +
					scale_color_viridis_c() +
    	  	facet_wrap(~ month_name, ncol = 3, nrow = 3) +
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
    	  	labs(colour = "projected temp no var") +
					theme_bw() +
 					theme(
 						strip.text = element_text(size = 14, face = "bold"),
 						strip.background = element_blank(),
 						axis.text = element_text(size = 12),	
  					axis.title = element_text(size = 14),
  					legend.title.align=0.5)
    	  
    	  plot
    	  
	}
	
	years <- c(1980:2099)

	monthly_plot_list <- lapply(years, temp_var_monthly_plot_func)
  
  mo_name_func_year <- function(x){
  	year_month <- paste0(x, "_proj_temp_var_miroc")
  }
   
  month_names_year <- sapply(years, mo_name_func_year)
  
	mo_name_func_file <- function(x){
  	year_month <- paste0("/Users/jenniferbigman/Dropbox/NOAA AFSC Postdoc/Pcod Project/Pcod Bering Sea Habitat Suitability/Pcod-Bering-Sea/output/plots/projections/temperature maps/miroc/temp with var/", x)
  }
   
  month_names <- sapply(month_names_year, mo_name_func_file)
				
	plot_list <- mapply(ggsave_func, x = monthly_plot_list, y = month_names)
	
	
	#### hab suit ####
	
	### with var ###
	
		hab_suit_var_monthly_plot_func <- function(x){
		
		    new_dat <- miroc_df %>% filter(., year == x)
    
    	  plot <- 
    	  	ggplot() +
					geom_sf(data = new_dat, aes(color = sp_hab_suit_var))  +
					geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
					coord_sf(crs = 3338) +
					scale_color_gradientn(colors = c("#B3E5FC", "#B3E5FC", 
																					 "#01579B", "#01579B",
																					 "#00345C", "#00345C"),
																values = c(0, 0.499, 0.5, 0.899, 0.9, 1),
																breaks = c(0.1, 0.5, 0.9),
																labels = format(c(0.1, 0.5, 0.9)),
																limits = c(0, 1)) +
    	  	facet_wrap(~ month_name, ncol = 3, nrow = 3) +
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
    	  	labs(colour = "Spawning\nhabitat\nsuitability") +
					theme_bw() +
 					theme(
 						strip.text = element_text(size = 14, face = "bold"),
 						strip.background = element_blank(),
 						axis.text = element_text(size = 12),	
  					axis.title = element_text(size = 14),
  					legend.title.align=0.5)
    	  
    	  plot
    	  
	}
	
	years <- c(1980:2099)

	monthly_plot_list <- lapply(years, hab_suit_var_monthly_plot_func)
  
  mo_name_func_year <- function(x){
  	year_month <- paste0(x, "_proj_habitat_suitability_var_5090_miroc")
  }
   
  month_names_year <- sapply(years, mo_name_func_year)
  
	mo_name_func_file <- function(x){
  	year_month <- paste0("/Users/jenniferbigman/Dropbox/NOAA AFSC Postdoc/Pcod Project/Pcod Bering Sea Habitat Suitability/Pcod-Bering-Sea/output/plots/projections/habitat suitability maps/miroc/with var/", x)
  }
   
  month_names <- sapply(month_names_year, mo_name_func_file)
				
	plot_list <- mapply(ggsave_func, x = monthly_plot_list, y = month_names)

	
