#### projections ####
	
	sims <- list("cesm", "gfdl", "miroc")
	
	sep_dfs_func <- function(x){
		new_dat <- ROMS_projected_dat %>% filter(., simulation == x)
		new_dat
	}
	
	sep_dfs <- lapply(sims, sep_dfs_func)
	
	#### map spawning habitat suitability where > 0.5 < 0.9 and > 0.9 colored different blues ####
	
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
	
	
	cesm_df <- sep_dfs_sf[[1]]
	gfdl_df <- sep_dfs_sf[[2]]
	miroc_df <- sep_dfs_sf[[3]]
	
	
	
		hab_suit_monthly_plot_func <- function(x){
		
		    new_dat <- cesm_df %>% filter(., year == 2050)
    
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
	
	years <- c(1980:2099)

	monthly_plot_list <- lapply(years, hab_suit_monthly_plot_func)
  
  mo_name_func_year <- function(x){
  	year_month <- paste0(x, "_proj_habitat_suitability_5090_cesm")
  }
   
  month_names_year <- sapply(years, mo_name_func_year)
  
	mo_name_func_file <- function(x){
  	year_month <- paste0("/Users/jenniferbigman/My Drive/NOAA AFSC Postdoc/Pcod Bering Sea Habitat Suitability/Pcod-Bering-Sea/output/plots/monthly plots/habitat suitability/projections/", x)
  }
   
  month_names <- sapply(month_names_year, mo_name_func_file)
				
	plot_list <- mapply(ggsave_func, x = monthly_plot_list, y = month_names)

	
	
	### with a color gradient
	
		hab_suit_monthly_plot_func <- function(x){
		
		    new_dat <- ROMS_projected_dat_sf %>% filter(., year == x)
    
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
	
	years <- c(1980:2099)

	monthly_plot_list <- lapply(years, hab_suit_monthly_plot_func)
  
  mo_name_func_year <- function(x){
  	year_month <- paste0(x, "_proj_habitat_suitability_gradient")
  }
   
  month_names_year <- sapply(years, mo_name_func_year)
  
	mo_name_func_file <- function(x){
  	year_month <- paste0("/Users/jenniferbigman/My Drive/NOAA AFSC Postdoc/Pcod Bering Sea Habitat Suitability/Pcod-Bering-Sea/output/plots/monthly plots/habitat suitability/projections/", x)
  }
   
  month_names <- sapply(month_names_year, mo_name_func_file)
				
	plot_list <- mapply(ggsave_func, x = monthly_plot_list, y = month_names)
