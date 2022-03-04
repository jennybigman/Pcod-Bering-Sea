	# map monthly-averaged temperature and spawning habitat suitability 

	#### projections ####
	
	## all plots/maps below with bias-corrected temp with variance ratio

	# temp ####
	
	temp_var_plot_func <- function(x){
		
		new_dat <- ROMS_projected_dat_drp_sf %>% filter(., year == x)
    
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
  	paste0(x, "_proj_temp_drp")
  }
   
  names_year <- sapply(years, name_func_year)
  
	func_file_path <- function(x){
  	paste0("/Users/jenniferbigman/My Drive/NOAA AFSC Postdoc/Pcod Bering Sea Habitat Suitability/Pcod-Bering-Sea/scripts/bias corrections with alt years/output/temp/", x)
  }
   
  plot_list_full <- sapply(names_year, func_file_path)
				
	mapply(ggsave_func, x = plot_list, y = plot_list_full)
	

	#### habitat suitability ####

	## var ratio ####
	
	hab_suit_plot_func <- function(x){
		
		    new_dat <- ROMS_projected_dat_drp_sf %>% filter(., year == x)
    
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
  	paste0(x, "_proj_habitat_suitability_drp")
  }
   
  names_year <- sapply(years, name_func_year)
  
	func_file_path <- function(x){
  	paste0("/Users/jenniferbigman/My Drive/NOAA AFSC Postdoc/Pcod Bering Sea Habitat Suitability/Pcod-Bering-Sea/scripts/bias corrections with alt years/output/hab suit/", x)
  }
   
  plot_list_full <- sapply(names_year, func_file_path)
				
	mapply(ggsave_func, x = plot_list, y = plot_list_full)
	