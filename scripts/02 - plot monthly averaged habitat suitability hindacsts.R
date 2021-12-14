	# map monthly-averaged spawning habitat suitability 

  #### map with a color gradient for spawning habitat suitability  ####
  
		hab_suit_monthly_plot_func <- function(x){
		
		    new_dat <- ROMS_hindcast_dat_sf %>% filter(., year == x)
    
    	  plot <- 
    	  	ggplot() +
					geom_sf(data = new_dat, aes(color = sp_hab_suit))  +
					geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
					coord_sf(crs = 3338) +
					scale_color_viridis_c(breaks = c(0, 0.50, 1.0), limits = c(0,1)) +
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
	
	years <- c(1970:2020)

	monthly_plot_list <- lapply(years, hab_suit_monthly_plot_func)
  
  mo_name_func_year <- function(x){
  	year_month <- paste0(x, "_habitat_suitability_gradient_check")
  }
   
  month_names_year <- sapply(years, mo_name_func_year)
  
	mo_name_func_file <- function(x){
  	year_month <- paste0("/Users/jenniferbigman/Google Drive/NOAA AFSC Postdoc/Pcod Bering Sea Habitat Suitability/Pcod-Bering-Sea/output/plots/monthly plots/habitat suitability/habitat suitability gradient/", x)
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
	
	years <- c(1970:2020)

	monthly_plot_list <- lapply(years, hab_suit_monthly_plot_func)
  

  mo_name_func_year <- function(x){
  	year_month <- paste0(x, "_habitat_suitability_90")
  }
   
  month_names_year <- sapply(years, mo_name_func_year)
  
	mo_name_func_file <- function(x){
  	year_month <- paste0("/Users/jenniferbigman/Google Drive/NOAA AFSC Postdoc/Pcod Bering Sea Habitat Suitability/Pcod-Bering-Sea/output/plots/monthly plots/habitat suitability/habitat suitability 90/", x)
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
	
	years <- c(1970:2020)

	monthly_plot_list <- lapply(years, hab_suit_monthly_plot_func)
  
	mo_name_func_year <- function(x){
  	year_month <- paste0(x, "_habitat_suitability_10")
  }
   
  month_names_year <- sapply(years, mo_name_func_year)
  
	mo_name_func_file <- function(x){
  	year_month <- paste0("/Users/jenniferbigman/Google Drive/NOAA AFSC Postdoc/Pcod Bering Sea Habitat Suitability/Pcod-Bering-Sea/output/plots/monthly plots/habitat suitability/habitat suitability 10/", x)
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
	
	years <- c(1970:2020)

	monthly_plot_list <- lapply(years, hab_suit_monthly_plot_func)
  
  mo_name_func_year <- function(x){
  	year_month <- paste0(x, "_habitat_suitability_5090")
  }
   
  month_names_year <- sapply(years, mo_name_func_year)
  
	mo_name_func_file <- function(x){
  	year_month <- paste0("/Users/jenniferbigman/Google Drive/NOAA AFSC Postdoc/Pcod Bering Sea Habitat Suitability/Pcod-Bering-Sea/output/plots/monthly plots/habitat suitability/habitat suitability 50_90/", x)
  }
   
  month_names <- sapply(month_names_year, mo_name_func_file)
				
	plot_list <- mapply(ggsave_func, x = monthly_plot_list, y = month_names)

	
	

		