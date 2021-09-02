# function to plot monthly bottom temps and hatch success by year

	fread("./Pcod-Bering-Sea/data/sm_temp_df_poly_depth.csv")
	
  # standardize hatch success (calculating spawning habitat suitability)
  sm_temp_df_poly_depth <- sm_temp_df_poly_depth %>%
  	mutate(sp_hab_suit = hatch_success_cauchy/max(hatch_success_cauchy))
  
  #convert to shapefile
  sm_temp_df_poly_depth_sf <- sm_temp_df_poly_depth %>%
  	st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326)

	## plots
  
  ## color gradient
  
	hab_suit_monthly_plot_func <- function(x){
		
		    new_dat <- sm_temp_df_poly_depth_sf %>% filter(., year == x)
    
    	  plot <- 
    	  	ggplot() +
					geom_sf(data = new_dat, aes(color = sp_hab_suit))  +
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
  
  setwd("~/Google Drive/NOAA AFSC Postdoc/Pcod Bering Sea Habitat Suitability/Pcod-Bering-Sea/output/plots/monthly plots/habitat suitability")
        
  mo_name_func <- function(x){
  	year_month <- paste0(x, "_habitat_suitability")
  }
   
  month_names <- sapply(years, mo_name_func)

  ggsave_func <- function(x,y){
  	ggsave(plot = x,
    file = paste(y,".png",sep=""),
    width = 10, height = 10, units = "in")
	}
				
	mapply(ggsave_func, x = monthly_plot_list, y = month_names)


  ## spawning habitat suitability > 0.9 colored dark blue

	hab_suit_monthly_plot_func <- function(x){
		
		    new_dat <- sm_temp_df_poly_depth_sf %>% filter(., year == x)
    
    	  plot <- 
    	  	ggplot() +
					geom_sf(data = new_dat, aes(color = sp_hab_suit))  +
					geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
					coord_sf(crs = 3338) +
					scale_color_gradientn(colors = c("#B3E5FC", "#B3E5FC", 
																					 "#01579B", "#01579B"),
																values = c(0, 0.89, 0.9, 1),
																breaks = c(0.1, 0.5, 0.9)) +
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
  
  setwd("~/Google Drive/NOAA AFSC Postdoc/Pcod Bering Sea Habitat Suitability/Pcod-Bering-Sea/output/plots/monthly plots/habitat suitability")
        
  mo_name_func <- function(x){
  	year_month <- paste0(x, "_habitat_suitability_90")
  }
   
  month_names <- sapply(years, mo_name_func)

  ggsave_func <- function(x,y){
  	ggsave(plot = x,
    file = paste(y,".png",sep=""),
    width = 10, height = 10, units = "in")
	}
				
	mapply(ggsave_func, x = monthly_plot_list, y = month_names)

  ## spawning habitat suitability = 0.1 colored dark blue

		hab_suit_monthly_plot_func <- function(x){
		
		    new_dat <- sm_temp_df_poly_depth_sf %>% filter(., year == x)
    
    	  plot <- 
    	  	ggplot() +
					geom_sf(data = dat1970, aes(color = sp_hab_suit))  +
					geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
					coord_sf(crs = 3338) +
					scale_color_gradientn(colors = c("#B3E5FC", "#01579B",
																					 "#B3E5FC","#B3E5FC"),
																values = c(0, 0.1, 0.2, 1),
																breaks = c(0.1, 0.5, 0.9)) +
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
  
  setwd("~/Google Drive/NOAA AFSC Postdoc/Pcod Bering Sea Habitat Suitability/Pcod-Bering-Sea/output/plots/monthly plots/habitat suitability")
        
  mo_name_func <- function(x){
  	year_month <- paste0(x, "_habitat_suitability_90")
  }
   
  month_names <- sapply(years, mo_name_func)

  ggsave_func <- function(x,y){
  	ggsave(plot = x,
    file = paste(y,".png",sep=""),
    width = 10, height = 10, units = "in")
	}
				
	mapply(ggsave_func, x = monthly_plot_list, y = month_names)
