# animate habitat suitability plots

	#### load map ####
	world_map_data <- ne_countries(scale = "medium", returnclass = "sf") 
	
	#### plotting function ####
  ggsave_func <- function(x,y){
  	ggsave(plot = x,
    file = paste(y,".png",sep=""),
    width = 10, height = 10, units = "in")
  }
  

	## yearly ###

	years_hind <- seq(from = 1970, to = 2020, by = 5)
	
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
 						labels = c("-170˚", "-160˚"),
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
    	  	ggtitle(paste0("Year: ", x)) + 
  		theme(
					legend.background = element_rect(fill = "black"),
  				axis.text=element_text(size= 14, colour = "white"),
  				axis.title= element_text(size=16, color = "white"),
  				axis.line = element_line(color = "white"),
  				axis.ticks = element_line(colour = "white"),
  				legend.title = element_text(color = "white"),
  				legend.text = element_text(color = "white"),
  				panel.background = element_rect(fill = "black"),
  				plot.background = element_rect(fill = "black", color = "black"),
					legend.title.align = 0.5,
    	  	plot.title = element_text(size = 20, color = "white"))
    	  	
    	  plot
    	  
	}
	
	monthly_plot_list <- lapply(years_hind, hab_suit_yr_plot_func_hind)
  
  mo_name_func_year <- function(x){
  	year_month <- paste0(x, "_hind_habitat_suitability_90")
  }
   
  month_names_year <- sapply(years_hind, mo_name_func_year)
  
	mo_name_func_file <- function(x){
  	year_month <- paste0("/Users/jenniferbigman/Dropbox/NOAA AFSC Postdoc/Pcod Project/Pcod Bering Sea Habitat Suitability/Pcod-Bering-Sea/output/plots/animation/sequence/hindcast/", x)
  }
   
  month_names <- sapply(month_names_year, mo_name_func_file)
				
  	
	#### plotting function ####
  ggsave_func <- function(x,y){
  	ggsave(plot = x,
    file = paste(y,".png",sep=""),
    width = 5, height = 5, units = "in")
  }
  

	plot_list <- mapply(ggsave_func, x = monthly_plot_list, y = month_names)


	
	# make a gif

	# hindcasts
	
	library(magick)
			
	dir_out <- ("~/Dropbox/NOAA AFSC Postdoc/Pcod Project/Pcod Bering Sea Habitat Suitability/Pcod-Bering-Sea/output/plots/animation/sequence/hindcast")

	imgs <- list.files(dir_out, full.names = TRUE)
	imgs <- imgs[1:11]
	img_list <- lapply(imgs, image_read)

	## join the images together
	img_joined <- image_join(img_list)
			
	## animate at 2 frames per second
	img_animated <- image_animate(img_joined, fps = 1)
			
	## view animated image
	img_animated
			
	## save to disk
	image_write(image = img_animated,
			        path = "./output/plots/animation/sequence/hindcast/habitat_suit_hind.gif")			

	
	
	
	
	# projections ####
	years_proj <- seq(from = 2025, to = 2095, by = 5)
	
	ROMS_projected_dat_trim <- ROMS_projected_dat %>% 
		filter(year %in% years_proj) %>%
			mutate(long_not_360 = case_when(
						 longitude >= 180 ~ longitude - 360,
						 longitude < 180 ~ longitude)) 
	
	ROMS_projected_dat_trim_sum <- ROMS_projected_dat_trim %>%
		group_by(simulation, projection, year, latitude, long_not_360) %>%
		summarise(mean_sphabsuit = mean(sp_hab_suit_var))
	
	ROMS_projected_dat_trim_sum_sf <- ROMS_projected_dat_trim_sum	%>%
  	st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326, remove = FALSE)
	
	ROMS_projected_dat_trim_sum_sf$scen <- NA
	
	ROMS_projected_dat_trim_sum_sf$scen[ROMS_projected_dat_trim_sum_sf$projection == "ssp126"] <- "low"
	ROMS_projected_dat_trim_sum_sf$scen[ROMS_projected_dat_trim_sum_sf$projection == "ssp585"] <- "high"
	
		hab_suit_yr_plot_func <- function(x){
		
		    new_dat <- ROMS_projected_dat_trim_sum_sf %>% filter(., year == x)
    
    	  plot <- 
    	  	ggplot() +
					geom_sf(data = new_dat, aes(color = mean_sphabsuit))  +
					geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
					coord_sf(crs = 3338) +
    	  	facet_grid(scen ~ simulation) +
					scale_color_gradientn(colors = c("#B3E5FC","#B3E5FC",
																					 "#3378af","#3378af",
																					 "#00345C","#00345C"),
																values = c(0, 0.499, 0.5, 0.899, 0.9, 1),
																breaks = c(0.1, 0.5, 0.9),
																labels = c(0.1, 0.5, 0.9),
																limits = c(0, 1)) +
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
    	  	labs(colour = "Spawning\nhabitat\nsuitability") +
    	  	ggtitle(paste0("Year: ", x)) + 
 					theme_bw() +
	 				theme(
	 					legend.background = element_rect(fill = "black"),
						legend.title = element_text(color = "white"),
  					legend.text = element_text(color = "white"),
	 					plot.title = element_text(size = 20, color = "white"),
						legend.title.align = 0.5,
	 					strip.background = element_blank(),
					strip.text = element_text(size = 16, color = "white"),
  				axis.text=element_text(size= 14, colour = "white"),
  				axis.title= element_text(size=16, color = "white"),
  				axis.line = element_line(color = "white"),
  				axis.ticks = element_line(colour = "white"),
  				panel.background = element_rect(fill = "black", color = "black"),
  				plot.background = element_rect(fill = "black", color = "black"))
  	
    	  	
    	  plot
    	  
	}
	
	monthly_plot_list <- lapply(years_proj, hab_suit_yr_plot_func)
  

  mo_name_func_year <- function(x){
  	year_month <- paste0(x, "_proj_habitat_suitability_90")
  }
   
  month_names_year <- sapply(years_proj, mo_name_func_year)
  
	mo_name_func_file <- function(x){
  	year_month <- paste0("/Users/jenniferbigman/Dropbox/NOAA AFSC Postdoc/Pcod Project/Pcod Bering Sea Habitat Suitability/Pcod-Bering-Sea/output/plots/animation/sequence/projection/", x)
  }
   
  month_names <- sapply(month_names_year, mo_name_func_file)
				
  ggsave_func2 <- function(x,y){
  	ggsave(plot = x,
    file = paste(y,".png",sep=""),
    width = 10, height = 6, units = "in")
  }
  

	plot_list <- mapply(ggsave_func2, x = monthly_plot_list, y = month_names)
	
	# projections
	
	library(magick)
			
	dir_out <- ("~/Dropbox/NOAA AFSC Postdoc/Pcod Project/Pcod Bering Sea Habitat Suitability/Pcod-Bering-Sea/output/plots/animation/sequence/projection")

	imgs <- list.files(dir_out, full.names = TRUE)
	imgs <- imgs[1:15]
	img_list <- lapply(imgs, image_read)

	## join the images together
	img_joined <- image_join(img_list)
			
	## animate at 1 frames per second
	img_animated <- image_animate(img_joined, fps = 1)
			
	## view animated image
	img_animated
			
	## save to disk
	image_write(image = img_animated,
			        path = "./output/plots/animation/sequence/projection/habitat_suit_proj.gif")			
	
	

	#### temperature ####
	
	## yearly ###

	years_hind <- seq(from = 1970, to = 2020, by = 5)
	
	ROMS_hindcast_dat_trim <- ROMS_hindcast_dat %>%
		filter(year %in% years_hind)

	ROMS_hindcast_dat_sum <- ROMS_hindcast_dat_trim %>%
		group_by(year, latitude, long_not_360) %>%
		summarise(mean_temp = mean(temp))
	
	ROMS_hindcast_dat_sum_sf <- ROMS_hindcast_dat_sum	%>%
  	st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326, remove = FALSE)
	
	temp_yr_plot_func_hind <- function(x){
		
		    new_dat <- ROMS_hindcast_dat_sum_sf %>% filter(., year == x)
    
    	  plot <- 
    	  	ggplot() +
					geom_sf(data = new_dat, aes(color = mean_temp))  +
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
    	  	labs(colour = "Temperature ˚C") +
					theme_bw() +
    	  	#ggtitle(paste0("Year: ", x)) + 
  		theme(
					legend.background = element_rect(fill = "black"),
  				axis.text=element_text(size= 14, colour = "white"),
  				axis.title= element_text(size=16, color = "white"),
  				axis.line = element_line(color = "white"),
  				axis.ticks = element_line(colour = "white"),
  				legend.title = element_text(color = "white"),
  				legend.text = element_text(color = "white"),
  				panel.background = element_rect(fill = "black"),
  				plot.background = element_rect(fill = "black", color = "black"),
					legend.title.align = 0.5,
    	  	plot.title = element_text(size = 20, color = "white"))
    	  	
    	  plot
    	  
	}
	
	monthly_plot_list <- lapply(years_hind, temp_yr_plot_func_hind)
  
  mo_name_func_year <- function(x){
  	year_month <- paste0(x, "_mean_temp")
  }
   
  month_names_year <- sapply(years_hind, mo_name_func_year)
  
	mo_name_func_file <- function(x){
  	year_month <- paste0("/Users/jenniferbigman/Dropbox/NOAA AFSC Postdoc/Pcod Project/Pcod Bering Sea Habitat Suitability/Pcod-Bering-Sea/output/plots/yearly plots/hindcast/", x)
  }
   
  month_names <- sapply(month_names_year, mo_name_func_file)
				
  	
	#### plotting function ####
  ggsave_func <- function(x,y){
  	ggsave(plot = x,
    file = paste(y,".png",sep=""),
    width = 5, height = 5, units = "in")
  }
  

	plot_list <- mapply(ggsave_func, x = monthly_plot_list, y = month_names)


	
	
	# projections ####
	years_proj <- seq(from = 2019, to = 2099, by = 10)
	
	ROMS_projected_dat_trim <- ROMS_projected_dat %>% 
		filter(year %in% years_proj) %>%
			mutate(long_not_360 = case_when(
						 longitude >= 180 ~ longitude - 360,
						 longitude < 180 ~ longitude)) 
	
	ROMS_projected_dat_trim_sum <- ROMS_projected_dat_trim %>%
		group_by(simulation, projection, year, latitude, long_not_360) %>%
		summarise(mean_temp = mean(bc_temp_sd))
	
	ROMS_projected_dat_trim_sum_sf <- ROMS_projected_dat_trim_sum	%>%
  	st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326, remove = FALSE)
	
	ROMS_projected_dat_trim_sum_sf$scen <- NA
	
	ROMS_projected_dat_trim_sum_sf$scen[ROMS_projected_dat_trim_sum_sf$projection == "ssp126"] <- "low"
	ROMS_projected_dat_trim_sum_sf$scen[ROMS_projected_dat_trim_sum_sf$projection == "ssp585"] <- "high"
	
	temp_yr_plot_func <- function(x){
		
		    new_dat <- ROMS_projected_dat_trim_sum_sf %>% filter(., year == x)
    
    	  plot <- 
    	  	ggplot() +
					geom_sf(data = new_dat, aes(color = mean_temp))  +
					geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
					coord_sf(crs = 3338) +
    	  	facet_grid(scen ~ simulation) +
					scale_color_viridis_c() +
 					scale_x_continuous(
 						breaks = c(-170, -160),
 						labels = c("-170˚", "-160˚"),
 						name = "Longitude",
 						limits = c(-1400000, -150000)
 					) +
 					scale_y_continuous(
 						breaks = c(55, 60),
 						limits = c(470000, 1900000),
 						name = "Latitude",
 					) +
    	  	labs(colour = "Temperature ˚C") +
    	  #	ggtitle(paste0("Year: ", x)) + 
 					theme_bw() +
	 				theme(
				  #legend.position = "none",
					plot.title = element_text(hjust = 0.5),
					plot.tag.position = c(0.2, 0.87),
					axis.text = element_text(size = 12, colour = "grey50"),
  	  		axis.ticks.x = element_line(colour = "grey50"),
  	  		axis.line = element_blank(),
  	  		axis.title.x = element_text(size=14, color = "grey50"),
  	  		panel.border = element_rect(fill = NA, color = "grey50"),
					plot.margin = margin(0, 0, 0, 0, "cm"),
	 				strip.background = element_blank(),
					strip.text = element_text(size = 18, color = "black"))
  	
    	  	
    	  plot
    	  
	}
	
	monthly_plot_list <- lapply(years_proj, temp_yr_plot_func)
  

  mo_name_func_year <- function(x){
  	year_month <- paste0(x, "_proj_temp")
  }
   
  month_names_year <- sapply(years_proj, mo_name_func_year)
  
	mo_name_func_file <- function(x){
  	year_month <- paste0("/Users/jenniferbigman/Dropbox/NOAA AFSC Postdoc/Pcod Project/Pcod Bering Sea Habitat Suitability/Pcod-Bering-Sea/output/plots/yearly plots/projected temps/", x)
  }
   
  month_names <- sapply(month_names_year, mo_name_func_file)
				
  ggsave_func2 <- function(x,y){
  	ggsave(plot = x,
    file = paste(y,".png",sep=""),
    width = 10, height = 6, units = "in")
  }
  

	plot_list <- mapply(ggsave_func2, x = monthly_plot_list, y = month_names)
	
