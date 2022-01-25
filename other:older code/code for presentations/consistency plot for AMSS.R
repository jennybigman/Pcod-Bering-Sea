# 08 - other ways of looking at spawning habitat suitability

# for AMSS

#### consistency per decade ####

#### hindcasts ####

	# mapping years for which spawning habitat suitability >= 0.5

	# remove 2020 because decade goes from 2010-2019
	ROMS_hindcast_dat_dec <- ROMS_hindcast_dat %>% filter(., year != 2020)

	# number of years
	no_yrs <- length(unique(ROMS_hindcast_dat_dec$year))

	# annual average
	ROMS_hindcast_dat_dec_yr_sum <- ROMS_hindcast_dat_dec %>%
		group_by(latitude, longitude, year) %>%
		summarise(mean_sphabsuit = mean(sp_hab_suit))

	ROMS_hindcast_dat_dec_yr_sum <- ROMS_hindcast_dat_dec_yr_sum %>%
		mutate(decade = case_when(
			between(year, 1970, 1979) ~ "1970s",
			between(year, 1980, 1989) ~ "1980s",
			between(year, 1990, 1999) ~ "1990s",
			between(year, 2000, 2009) ~ "2000s",
			between(year, 2010, 2019) ~ "2010s"))

	# 0.5
	ROMS_hindcast_dat_dec_yr_sum_05 <- ROMS_hindcast_dat_dec_yr_sum %>%
		group_by(latitude, longitude, decade) %>%
		summarize(no_yrs = length(which(mean_sphabsuit >= 0.5))) %>%
		mutate(year_tot = 10,
					 pct_yrs = (no_yrs/year_tot) * 100)

	ROMS_hindcast_dat_dec_yr_sum_05_sf <- ROMS_hindcast_dat_dec_yr_sum_05 %>%
			mutate(long_not_360 = longitude - 360) %>%
	  	st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326)
  
consistency_hind_func05 <- function(x){
	
	new_dat <- ROMS_hindcast_dat_dec_yr_sum_05_sf %>% filter(decade == x)
	
	plot <- 
	  		ggplot() +
						geom_sf(data = new_dat, 
										aes(color = pct_yrs))  +
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
	    	  	labs(colour = "%\nyears") +
						ggtitle(label = paste("Percent of years for which spawning habitat suitability", 
																	symbol, "0.5", sep =" ")) +
						ggtitle(paste0("Potential habitat\nDecade: ", x)) +
						theme_bw() +
	 				theme(
	 				plot.title = element_text(color = "white", size = 12),
	 				legend.background = element_rect(fill = "black"),
  				axis.text=element_text(size= 8, colour = "white"),
  				axis.title= element_text(size=10, color = "white"),
  				axis.line = element_line(color = "white"),
  				axis.ticks = element_line(colour = "white"),
  				legend.title = element_text(color = "white"),
  				legend.text = element_text(color = "white"),
  				panel.background = element_rect(fill = "black"),
					panel.grid = element_blank(),
  				plot.background = element_rect(fill = "black", color = "black"))
}
  	
  decades_hind <- unique(ROMS_hindcast_dat_dec_yr_sum_05_sf$decade)

	dec_hind_plot_list05 <- lapply(decades_hind, consistency_hind_func05)
  
  name_func_dec05 <- function(x){
  	year_month <- paste0(x, "_hind_decade_05")
  }
   
  names_hind_dec05 <- sapply(decades_hind, name_func_dec05)
  
	file_path_hind_dec05 <- function(x){
  	path <- paste0("/Users/jenniferbigman/My Drive/NOAA AFSC Postdoc/Pcod Bering Sea Habitat Suitability/Pcod-Bering-Sea/output/plots/animation/consistency/hindcast/05/", x)
  }
   
  names_hind_dec05_full <- sapply(names_hind_dec05, file_path_hind_dec05)
	
   ggsave_func <- function(x,y){
  	ggsave(plot = x,
    file = paste(y,".png",sep=""),
    width = 5, height = 5, units = "in")
  }
	plot_list <- mapply(ggsave_func, x = dec_hind_plot_list05, y = names_hind_dec05_full)
	
	# animate
	
	library(magick)
			
	dir_out <- "/Users/jenniferbigman/My Drive/NOAA AFSC Postdoc/Pcod Bering Sea Habitat Suitability/Pcod-Bering-Sea/output/plots/animation/consistency/hindcast/05"

	imgs <- list.files(dir_out, full.names = TRUE)
	img_list <- lapply(imgs, image_read)

	## join the images together
	img_joined <- image_join(img_list)
			
	## animate at 2 frames per second
	img_animated <- image_animate(img_joined, fps = 2)
			
	## view animated image
	img_animated
			
	## save to disk
	image_write(image = img_animated,
			        path = "./output/plots/animation/consistency/hindcast/05/consistency_hind05.gif")			
	
	
	# 0.9
	ROMS_hindcast_dat_dec_yr_sum_09 <- ROMS_hindcast_dat_dec_yr_sum %>%
		group_by(latitude, longitude, decade) %>%
		summarize(no_yrs = length(which(mean_sphabsuit >= 0.9))) %>%
		mutate(year_tot = 10,
					 pct_yrs = (no_yrs/year_tot) * 100)

	ROMS_hindcast_dat_dec_yr_sum_09_sf <- ROMS_hindcast_dat_dec_yr_sum_09 %>%
			mutate(long_not_360 = longitude - 360) %>%
	  	st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326)
  
consistency_hind_func09 <- function(x){
	
	new_dat <- ROMS_hindcast_dat_dec_yr_sum_09_sf %>% filter(decade == x)
	
	plot <- 
	  		ggplot() +
						geom_sf(data = new_dat, 
										aes(color = pct_yrs))  +
						geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
						coord_sf(crs = 3338) +
						scale_color_viridis_c(limits = c(0, 100)) +
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
						ggtitle(label = paste("Percent of years for which spawning habitat suitability", 
																	symbol, "0.9", sep =" ")) +
						ggtitle(paste0("Core habitat\nDecade: ", x)) +
						theme_bw() +
	 				theme(
	 				plot.title = element_text(color = "white", size = 18),
	 				legend.background = element_rect(fill = "black"),
  				axis.text=element_text(size= 12, colour = "white"),
  				axis.title= element_text(size=16, color = "white"),
  				axis.line = element_line(color = "white"),
  				axis.ticks = element_line(colour = "white"),
  				legend.title = element_text(color = "white"),
  				legend.text = element_text(color = "white"),
  				panel.background = element_rect(fill = "black"),
					panel.grid = element_blank(),
  				plot.background = element_rect(fill = "black", color = "black"))
}
  	
  decades_hind <- unique(ROMS_hindcast_dat_dec_yr_sum_09_sf$decade)

	dec_hind_plot_list09 <- lapply(decades_hind, consistency_hind_func09)
  
  name_func_dec09 <- function(x){
  	year_month <- paste0(x, "_hind_decade_09")
  }
   
  names_hind_dec09 <- sapply(decades_hind, name_func_dec09)
  
	file_path_hind_dec09 <- function(x){
  	path <- paste0("/Users/jenniferbigman/My Drive/NOAA AFSC Postdoc/Pcod Bering Sea Habitat Suitability/Pcod-Bering-Sea/output/plots/animation/consistency/hindcast/09/", x)
  }
   
  names_hind_dec09_full <- sapply(names_hind_dec09, file_path_hind_dec09)
				
	plot_list <- mapply(ggsave_func, x = dec_hind_plot_list09, y = names_hind_dec09_full)
	
	# animate
	
	library(magick)
			
	dir_out <- "/Users/jenniferbigman/My Drive/NOAA AFSC Postdoc/Pcod Bering Sea Habitat Suitability/Pcod-Bering-Sea/output/plots/animation/consistency/hindcast/09"

	imgs <- list.files(dir_out, full.names = TRUE)
	img_list <- lapply(imgs, image_read)

	## join the images together
	img_joined <- image_join(img_list)
			
	## animate at 2 frames per second
	img_animated <- image_animate(img_joined, fps = 2)
			
	## view animated image
	img_animated
			
	## save to disk
	image_write(image = img_animated,
			        path = "./output/plots/animation/consistency/hindcast/09/consistency_hind09.gif")			
	

	
	
	#### projections ####

	# mapping years for which spawning habitat suitability >= 0.5

	# annual average
	ROMS_projected_dat_yr_sum <- ROMS_projected_dat %>%
		group_by(simulation, projection, year, latitude, longitude) %>%
		summarise(mean_sphabsuit = mean(sp_hab_suit_var))
	
	# code below is missing last 5 years because historical is 1980 - 2014 

	years_map <- 2020:2099

	ROMS_projected_dat_yr_sum <- ROMS_projected_dat_yr_sum %>%
		filter(., year %in% years_map) %>%
		mutate(decade = case_when(
			between(year, 2020, 2029) ~ "2020 - 2029",
			between(year, 2030, 2039) ~ "2030 - 2039",
			between(year, 2040, 2049) ~ "2040 - 2049",
			between(year, 2050, 2059) ~ "2050 - 2059",
			between(year, 2060, 2069) ~ "2060 - 2069",
			between(year, 2070, 2079) ~ "2070 - 2079",
			between(year, 2080, 2089) ~ "2080 - 2089",
			between(year, 2090, 2099) ~ "2090 - 2099",
			))


 # 05 

	ROMS_projected_dat_yr_sum_05 <- ROMS_projected_dat_yr_sum %>%
		group_by(simulation, projection, latitude, longitude, decade) %>%
		summarize(no_yrs = length(which(mean_sphabsuit >= 0.5))) %>%
		mutate(year_tot = 10,
					 pct_yrs = (no_yrs/year_tot) * 100)
	
	ROMS_projected_dat_yr_sum_05$scen <- NA
	
	ROMS_projected_dat_yr_sum_05$scen[ROMS_projected_dat_yr_sum_05$projection == "ssp126"] <- "low"
	ROMS_projected_dat_yr_sum_05$scen[ROMS_projected_dat_yr_sum_05$projection == "ssp585"] <- "high"
	

	ROMS_projected_dat_yr_sum_05_sf <- ROMS_projected_dat_yr_sum_05 %>%
			mutate(long_not_360 = longitude - 360) %>%
	  	st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326)
  
	consistency_proj_func05 <- function(x){
	
	new_dat <- ROMS_projected_dat_yr_sum_05_sf %>% filter(decade == x)
	
	plot <- 
	  		ggplot() +
						geom_sf(data = new_dat, 
										aes(color = pct_yrs))  +
						geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
						coord_sf(crs = 3338) +
						facet_grid(scen ~ simulation) +
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
						ggtitle(label = paste("Percent of years for which spawning habitat suitability", 
																	symbol, "0.5", sep =" ")) +
						ggtitle(paste0("Potential habitat\nDecade: ", x)) +
 					theme_bw() +
	 				theme(
	 					legend.background = element_rect(fill = "black"),
						legend.title = element_text(color = "white"),
  					legend.text = element_text(color = "white"),
	 					plot.title = element_text(size = 18, color = "white"),
						legend.title.align = 0.5,
	 					strip.background = element_blank(),
					strip.text = element_text(size = 16, color = "white"),
  				axis.text=element_text(size= 14, colour = "white"),
  				axis.title= element_text(size=14, color = "white"),
  				axis.line = element_line(color = "white"),
  				axis.ticks = element_line(colour = "white"),
  				panel.background = element_rect(fill = "black", color = "black"),
  				plot.background = element_rect(fill = "black", color = "black"))
}
  	
  decades_proj <- unique(ROMS_projected_dat_yr_sum_05_sf$decade)

	dec_proj_plot_list05 <- lapply(decades_proj, consistency_proj_func05)
  
  name_func_dec05 <- function(x){
  	year_month <- paste0(x, "_proj_decade_05")
  }
   
  names_proj_dec05 <- sapply(decades_proj, name_func_dec05)
  
	file_path_proj_dec05 <- function(x){
  	path <- paste0("/Users/jenniferbigman/My Drive/NOAA AFSC Postdoc/Pcod Bering Sea Habitat Suitability/Pcod-Bering-Sea/output/plots/animation/consistency/projection/05/", x)
  }
   
  names_proj_dec05_full <- sapply(names_proj_dec05, file_path_proj_dec05)
	
    ggsave_func2 <- function(x,y){
  	ggsave(plot = x,
    file = paste(y,".png",sep=""),
    width = 10, height = 6, units = "in")
  }
  
	plot_list <- mapply(ggsave_func2, x = dec_proj_plot_list05, y = names_proj_dec05_full)

	
	# animate
	
	library(magick)
			
	dir_out <- "/Users/jenniferbigman/My Drive/NOAA AFSC Postdoc/Pcod Bering Sea Habitat Suitability/Pcod-Bering-Sea/output/plots/animation/consistency/projection/05"

	imgs <- list.files(dir_out, full.names = TRUE)
	img_list <- lapply(imgs, image_read)

	## join the images together
	img_joined <- image_join(img_list)
			
	## animate at 2 frames per second
	img_animated <- image_animate(img_joined, fps = 2)
			
	## view animated image
	img_animated
			
	## save to disk
	image_write(image = img_animated,
			        path = "./output/plots/animation/consistency/projection/05/consistency_proj05.gif")			
	

	 # 09 

	ROMS_projected_dat_yr_sum_09 <- ROMS_projected_dat_yr_sum %>%
		group_by(simulation, projection, latitude, longitude, decade) %>%
		summarize(no_yrs = length(which(mean_sphabsuit >= 0.9))) %>%
		mutate(year_tot = 10,
					 pct_yrs = (no_yrs/year_tot) * 100)
	
	ROMS_projected_dat_yr_sum_09$scen <- NA
	
	ROMS_projected_dat_yr_sum_09$scen[ROMS_projected_dat_yr_sum_09$projection == "ssp126"] <- "low"
	ROMS_projected_dat_yr_sum_09$scen[ROMS_projected_dat_yr_sum_09$projection == "ssp585"] <- "high"
	

	ROMS_projected_dat_yr_sum_09_sf <- ROMS_projected_dat_yr_sum_09 %>%
			mutate(long_not_360 = longitude - 360) %>%
	  	st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326)
  
	consistency_proj_func09 <- function(x){
	
	new_dat <- ROMS_projected_dat_yr_sum_09_sf %>% filter(decade == x)
	
	plot <- 
	  		ggplot() +
						geom_sf(data = new_dat, 
										aes(color = pct_yrs))  +
						geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
						coord_sf(crs = 3338) +
						facet_grid(scen ~ simulation) +
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
						ggtitle(label = paste("Percent of years for which spawning habitat suitability", 
																	symbol, "0.9", sep =" ")) +
						ggtitle(paste0("Core habitat\nDecade: ", x)) +
 					theme_bw() +
	 				theme(
	 					legend.background = element_rect(fill = "black"),
						legend.title = element_text(color = "white"),
  					legend.text = element_text(color = "white"),
	 					plot.title = element_text(size = 18, color = "white"),
						legend.title.align = 0.5,
	 					strip.background = element_blank(),
					strip.text = element_text(size = 16, color = "white"),
  				axis.text=element_text(size= 14, colour = "white"),
  				axis.title= element_text(size=14, color = "white"),
  				axis.line = element_line(color = "white"),
  				axis.ticks = element_line(colour = "white"),
  				panel.background = element_rect(fill = "black", color = "black"),
  				plot.background = element_rect(fill = "black", color = "black"))
}
  	
  decades_proj <- unique(ROMS_projected_dat_yr_sum_09_sf$decade)

	dec_proj_plot_list09 <- lapply(decades_proj, consistency_proj_func09)
  
  name_func_dec09 <- function(x){
  	year_month <- paste0(x, "_proj_decade_09")
  }
   
  names_proj_dec09 <- sapply(decades_proj, name_func_dec09)
  
	file_path_proj_dec09 <- function(x){
  	path <- paste0("/Users/jenniferbigman/My Drive/NOAA AFSC Postdoc/Pcod Bering Sea Habitat Suitability/Pcod-Bering-Sea/output/plots/animation/consistency/projection/09/", x)
  }
   
  names_proj_dec09_full <- sapply(names_proj_dec09, file_path_proj_dec09)
	
    ggsave_func2 <- function(x,y){
  	ggsave(plot = x,
    file = paste(y,".png",sep=""),
    width = 10, height = 6, units = "in")
  }
  
	plot_list <- mapply(ggsave_func2, x = dec_proj_plot_list09, y = names_proj_dec09_full)

	
	# animate
	
	library(magick)
			
	dir_out <- "/Users/jenniferbigman/My Drive/NOAA AFSC Postdoc/Pcod Bering Sea Habitat Suitability/Pcod-Bering-Sea/output/plots/animation/consistency/projection/09"

	imgs <- list.files(dir_out, full.names = TRUE)
	img_list <- lapply(imgs, image_read)

	## join the images together
	img_joined <- image_join(img_list)
			
	## animate at 2 frames per second
	img_animated <- image_animate(img_joined, fps = 2)
			
	## view animated image
	img_animated
			
	## save to disk
	image_write(image = img_animated,
			        path = "./output/plots/animation/consistency/projection/09/consistency_proj09.gif")			
	

	
	
	
	
	
	
	# plots not for animation 
	
	# 05
	
	# hindcast
	consistency_hind_05_plot <- 
	  		ggplot() +
						geom_sf(data = ROMS_hindcast_dat_dec_yr_sum_05_sf, 
										aes(color = pct_yrs))  +
						geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
						coord_sf(crs = 3338) +
						facet_grid(~ decade) +
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
						ggtitle(label = paste("Percent of years for which spawning habitat suitability", 
																	symbol, "0.5", sep =" ")) +
						theme_bw() +
	 							theme(strip.background = element_blank(),
					strip.text = element_text(size = 14, color = "white"),
					legend.background = element_rect(fill = "black"),
  				axis.text=element_text(size= 12, colour = "white"),
  				axis.title= element_text(size=16, color = "white"),
  				axis.line = element_line(color = "white"),
  				axis.ticks = element_line(colour = "white"),
  				legend.title = element_text(color = "white", size = 18),
  				legend.text = element_text(color = "white", size = 16),
  				panel.background = element_rect(fill = "black"),
  				plot.background = element_rect(fill = "black", color = "black"),
					plot.title = element_text(size = 18, color = "white"),
					legend.title.align = 0.5)
  	
	
	  	ggsave(here("./output/plots/consistency_hind_05_plot.png"),
			 consistency_hind_05_plot,
			 width = 9, height = 4, units = "in")

	# projection

	consistency_proj_05_plot <- 
	  		ggplot() +
						geom_sf(data = ROMS_projected_dat_yr_sum_05_sf, 
										aes(color = pct_yrs))  +
						geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
						coord_sf(crs = 3338) +
						facet_grid(decade ~ simulation ~ projection) +
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
						ggtitle(label = paste("Percent of years for which spawning habitat suitability", 
																	symbol, "0.5", sep =" ")) +
						theme_bw() +
	 					theme(
	 						strip.text = element_text(size = 14, face = "bold"),
	 						strip.background = element_blank(),
	 						axis.text = element_text(size = 12),	
	  					axis.title = element_text(size = 14),
	  					legend.title = element_text(hjust = 0.5, size = 12),
	 						plot.title = element_text(size = 18, face = "bold")
	 						)
	
	
	  	ggsave(here("./output/plots/consistency_hind_05_proj.png"),
			 consistency_proj_05_plot,
			 width = 9, height = 8, units = "in")


	# 09
	
	# hindcast
	consistency_hind_09_plot <- 
	  		ggplot() +
						geom_sf(data = ROMS_hindcast_dat_dec_yr_sum_09_sf, 
										aes(color = pct_yrs))  +
						geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
						coord_sf(crs = 3338) +
						facet_grid(~decade) +
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
						ggtitle(label = paste("Percent of years for which spawning habitat suitability", 
																	symbol, "0.9", sep =" ")) +
						theme_bw() +
	 							theme(strip.background = element_blank(),
					strip.text = element_text(size = 14, color = "white"),
					legend.background = element_rect(fill = "black"),
  				axis.text=element_text(size= 12, colour = "white"),
  				axis.title= element_text(size=16, color = "white"),
  				axis.line = element_line(color = "white"),
  				axis.ticks = element_line(colour = "white"),
  				legend.title = element_text(color = "white", size = 18),
  				legend.text = element_text(color = "white", size = 16),
  				panel.background = element_rect(fill = "black"),
  				plot.background = element_rect(fill = "black", color = "black"),
					plot.title = element_text(size = 18, color = "white"),
					legend.title.align = 0.5)
  	
	
	
	  	ggsave(here("./output/plots/consistency_hind_09_plot.png"),
			 consistency_hind_09_plot,
			 width = 9, height = 4, units = "in")

	# projection

	consistency_proj_09_plot <- 
	  		ggplot() +
						geom_sf(data = ROMS_projected_dat_yr_sum_09_sf, 
										aes(color = pct_yrs))  +
						geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
						coord_sf(crs = 3338) +
						facet_grid(decade ~ simulation ~ projection) +
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
						ggtitle(label = paste("Percent of years for which spawning habitat suitability", 
																	symbol, "0.9", sep =" ")) +
								theme_bw() +
	 							theme(strip.background = element_blank(),
					strip.text = element_text(size = 14, color = "white"),
					legend.background = element_rect(fill = "black"),
  				axis.text=element_text(size= 12, colour = "white"),
  				axis.title= element_text(size=16, color = "white"),
  				axis.line = element_line(color = "white"),
  				axis.ticks = element_line(colour = "white"),
  				legend.title = element_text(color = "white", size = 18),
  				legend.text = element_text(color = "white", size = 16),
  				panel.background = element_rect(fill = "black"),
  				plot.background = element_rect(fill = "black", color = "black"),
					plot.title = element_text(size = 18, color = "white"),
					legend.title.align = 0.5)
  	
	

	 
  	ggsave(here("./output/plots/consistency_proj_09_plot.png"),
			 consistency_proj_09_plot,
			 width = 9, height = 8, units = "in")
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
  	#### alt years ####
  	# annual average
	ROMS_projected_dat_yr_sum_alt <- ROMS_projected_dat %>%
		group_by(simulation, projection, year, latitude, longitude) %>%
		summarise(mean_sphabsuit = mean(sp_hab_suit_var))

  ROMS_projected_dat_yr_sum_alt <- ROMS_projected_dat_yr_sum_alt %>%
		mutate(decade = case_when(
			between(year, 1980, 1984) ~ "1980 - 1984",
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
			between(year, 2095, 2099) ~ "2095 - 2099"
			))
  	
  # 05 

	ROMS_projected_dat_yr_sum_05_alt <- ROMS_projected_dat_yr_sum_alt %>%
		group_by(simulation, projection, latitude, longitude, decade) %>%
		summarize(no_yrs = length(which(mean_sphabsuit >= 0.5))) %>%
		mutate(year_tot = 10,
					 pct_yrs = (no_yrs/year_tot) * 100)

	ROMS_projected_dat_yr_sum_05_alt_sf <- ROMS_projected_dat_yr_sum_05_alt %>%
			mutate(long_not_360 = longitude - 360) %>%
	  	st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326)
  
	sims_hist_cons05_alt <- ROMS_projected_dat_yr_sum_05_alt_sf %>%
		filter(projection == "historical")
	
	sims_ssp126_cons05_alt <- ROMS_projected_dat_yr_sum_05_alt_sf %>%
		filter(projection == "ssp126")
	
	sims_ssp585_cons05_alt <- ROMS_projected_dat_yr_sum_05_alt_sf %>%
		filter(projection == "ssp585")


	# 09

	ROMS_projected_dat_yr_sum_09_alt <- ROMS_projected_dat_yr_sum_alt %>%
		group_by(simulation, projection, latitude, longitude, decade) %>%
		summarize(no_yrs = length(which(mean_sphabsuit >= 0.9))) %>%
		mutate(year_tot = 10,
					 pct_yrs = (no_yrs/year_tot) * 100)
	
	ROMS_projected_dat_yr_sum_09_alt_sf <- ROMS_projected_dat_yr_sum_09_alt %>%
			mutate(long_not_360 = longitude - 360) %>%
	  	st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326)

	sims_hist_cons09_alt <- ROMS_projected_dat_yr_sum_09_alt_sf %>%
		filter(projection == "historical")
	
	sims_ssp126_cons09_alt <- ROMS_projected_dat_yr_sum_09_alt_sf %>%
		filter(projection == "ssp126")
	
	sims_ssp585_cons09_alt <- ROMS_projected_dat_yr_sum_09_alt_sf %>%
		filter(projection == "ssp585")

	# plots

	# historical 
	
	#05
	hist_plot_cons_05_alt <- 
  		ggplot() +
					geom_sf(data = sims_hist_cons05_alt, 
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
  	
  	ggsave("./output/plots/hist_plot_cons_05_alt.png",
		hist_plot_cons_05_alt,
		width = 13, height = 7, units = "in")
  
 	#09
  hist_plot_cons_09_alt <- 
  		ggplot() +
					geom_sf(data = sims_hist_cons09_alt, 
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
  	
  	ggsave("./output/plots/hist_plot_cons_09_alt.png",
		hist_plot_cons_09_alt,
		width = 13, height = 7, units = "in")
 
  	# ssp126
  	
	ssp126_plot_cons_05_alt <- 
  		ggplot() +
					geom_sf(data = sims_ssp126_cons05_alt, 
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
  	
  	ggsave("./output/plots/ssp126_plot_cons_05_alt.png",
		ssp126_plot_cons_05_alt,
		width = 13, height = 7, units = "in")
  
  #09
  ssp126_plot_cons_09_alt <- 
  		ggplot() +
					geom_sf(data = sims_ssp126_cons09_alt, 
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
  	
  	ggsave("./output/plots/ssp126_plot_cons_09_alt.png",
		ssp126_plot_cons_09_alt,
		width = 13, height = 7, units = "in")
  	
  	# ssp585
  	
	ssp585_plot_cons_05_alt <- 
  		ggplot() +
					geom_sf(data = sims_ssp585_cons05_alt, 
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
  	
  	ggsave("./output/plots/ssp585_plot_cons_05_alt.png",
		ssp585_plot_cons_05_alt,
		width = 13, height = 7, units = "in")
  
  #09
  ssp585_plot_cons_09_alt <- 
  		ggplot() +
					geom_sf(data = sims_ssp585_cons09_alt, 
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
  	
  	ggsave("./output/plots/ssp585_plot_cons_09_alt.png",
		ssp585_plot_cons_09_alt,
		width = 13, height = 7, units = "in")
  	
 
 
###### SKIP FOR NOW #########################################################################
	
## monthly average
  	
ROMS_projected_dat_month_sum <- ROMS_projected_dat %>%
	group_by(simulation, projection, year, latitude, longitude, month_name) %>%
	summarise(mean_sphabsuit = mean(sp_hab_suit))

ROMS_projected_dat_month_sum <- ROMS_projected_dat_month_sum %>%
	mutate(decade = case_when(
		between(year, 1980, 1989) ~ "1980s",
		between(year, 1990, 1999) ~ "1990s",
		between(year, 2000, 2009) ~ "2000s",
		between(year, 2010, 2019) ~ "2010s",
		between(year, 2020, 2029) ~ "2020s",
		between(year, 2030, 2039) ~ "2030s",
		between(year, 2040, 2049) ~ "2040s",
		between(year, 2050, 2059) ~ "2050s",
		between(year, 2060, 2069) ~ "2060s",
		between(year, 2070, 2079) ~ "2070s",
		between(year, 2080, 2089) ~ "2080s",
		between(year, 2090, 2099) ~ "2090s"
		))

# 0.5
ROMS_projected_dat_month_sum_05 <- ROMS_projected_dat_month_sum %>%
	group_by(simulation, projection, latitude, longitude, month_name, decade) %>%
	summarize(no_yrs = length(which(mean_sphabsuit >= 0.5))) %>%
	mutate(year_tot = 10,
				 pct_yrs = (no_yrs/year_tot) * 100)

ROMS_projected_dat_month_sum_05_sf <- ROMS_projected_dat_month_sum_05 %>%
		mutate(long_not_360 = longitude - 360) %>%
  	st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326)
  

plot <- 
  		ggplot() +
					geom_sf(data = ROMS_hindcast_dat_dec_month_sum_05_sf,
									aes(color = pct_yrs))  +
					geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
					coord_sf(crs = 3338) +
  		    facet_grid(decade ~ month_name) +
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
					ggtitle(label = paste("Percent of years for which spawning habitat suitability", 
																symbol, "0.5", sep =" ")) +
					theme_bw() +
 					theme(
 						strip.text = element_text(size = 14, face = "bold"),
 						strip.background = element_blank(),
 						axis.text = element_text(size = 12),	
  					axis.title = element_text(size = 14),
  					legend.title = element_text(hjust = 0.5, size = 12),
 						plot.title = element_text(size = 18, face = "bold")
 						)
  	

  	ggsave("./output/plots/percent_years_months05.png",
		plot,
		width = 15, height = 10, units = "in")
	

 # 0.9
ROMS_projected_dat_month_sum_09 <- ROMS_projected_dat_month_sum %>%
	group_by(simulation, projection, latitude, longitude, month_name, decade) %>%
	summarize(no_yrs = length(which(mean_sphabsuit >= 0.9))) %>%
	mutate(year_tot = 10,
				 pct_yrs = (no_yrs/year_tot) * 100)

ROMS_projected_dat_month_sum_09_sf <- ROMS_projected_dat_month_sum_09 %>%
		mutate(long_not_360 = longitude - 360) %>%
  	st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326)
  

plot <- 
  		ggplot() +
					geom_sf(data = ROMS_hindcast_dat_dec_month_sum_09_sf,
									aes(color = pct_yrs))  +
					geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
					coord_sf(crs = 3338) +
  		    facet_grid(decade ~ month_name) +
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
					ggtitle(label = paste("Percent of years for which spawning habitat suitability", 
																symbol, "0.9", sep =" ")) +
					theme_bw() +
 					theme(
 						strip.text = element_text(size = 14, face = "bold"),
 						strip.background = element_blank(),
 						axis.text = element_text(size = 12),	
  					axis.title = element_text(size = 14),
  					legend.title = element_text(hjust = 0.5, size = 12),
 						plot.title = element_text(size = 18, face = "bold")
 						)
  	

  	ggsave("./output/plots/percent_years_months09.png",
		plot,
		width = 15, height = 10, units = "in")
	
###### SKIP FOR NOW #########################################################################

  	
 #### mapping % change ####
  
 #### hindcasts ####	

	ROMS_hindcast_dat_current <- ROMS_hindcast_dat %>%
		filter(., year != 2020) 

	# summarize data into year bins

	# create a list of year bins
	year_bins  <- list('1970_1979' = c(1970:1979),
                     '1980_1989' = c(1980:1989),
                     '1990_1999' = c(1990:1999),
                     '2000_2009' = c(2000:2009),
                     '2010_2019' = c(2010:2019)) 

	# create a list of dfs summarized by year bins
	sum_yr_bin <- function(x){
 
		df <- ROMS_hindcast_dat_current %>%
		filter(year %in% x) %>%
		group_by(longitude, latitude) %>%
		summarise(mean_hs = mean(hatch_success_cauchy),
							mean_temp = mean(temp),
							mean_sphabsuit = mean(sp_hab_suit)) 
		
		df
	
	}

	df_list <- lapply(year_bins, sum_yr_bin)

	# add a column of the year bin
	df_list_tp <- mapply(cbind, df_list, "time_period"= names(year_bins), SIMPLIFY = FALSE)

	# bind all rows of all dfs together and add a column for longitude not on 360 scale
	ROMS_hindcast_dat_current_sum <- bind_rows(df_list_tp) %>% 
  	dplyr::select(latitude, longitude, mean_sphabsuit, time_period)
 
  year_list  <- list('1970_1979',
                     '1980_1989',
                     '1990_1999',
                     '2000_2009',
                     '2010_2019')
 

	reshape_func <- function(x){
		
		new_dat <- ROMS_hindcast_dat_current_sum %>% filter(., time_period == x)

  	new_dat <- new_dat %>% 
  		rename(!!paste0("mean_sphabsuit_", x) := mean_sphabsuit) %>%
  		dplyr::select(- time_period)
  			
    new_dat
    
  }
  
  df_list <- lapply(year_list, reshape_func)
  
  sp_yr_sum <- bind_cols(df_list)
  
  sp_yr_sum <- sp_yr_sum %>%
  	rename(latitude = latitude...1,
  				 longitude = longitude...2)
  
  sp_yr_sum <- dplyr::select(sp_yr_sum, - contains(c("latitude..", "longitude..")))
  
  sp_yr_sum2 <- sp_yr_sum %>%
  	rowwise() %>%
  	mutate( pct_change1 =
  				 	(mean_sphabsuit_1980_1989 - mean_sphabsuit_1970_1979),
  				  pct_change2 = 
  				 	(mean_sphabsuit_1990_1999 - mean_sphabsuit_1980_1989),
  				  pct_change3 = 
  				 	(mean_sphabsuit_2000_2009 - mean_sphabsuit_1990_1999),
					  pct_change4 = 
  				 	(mean_sphabsuit_2010_2019 - mean_sphabsuit_2000_2009)
					 )
 
  facet_labeller_top <- function(variable, value) {
  c(
    "1970s-1980s", 
    "1980s-1990s",
    "1990s-2000s",
    "2000s-2010s"
  )
}
  
  sp_yr_sum <- sp_yr_sum %>%
  	dplyr::select(- contains("mean_sphabsuit"))
 
  sp_yr_sum_long <- sp_yr_sum %>%
  	gather(pct_change, value, -c("latitude", "longitude")) 
  
  sp_yr_sum_long_sf <- sp_yr_sum_long %>%
  	mutate(long_not_360 = longitude - 360) %>%
  	st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326)
  
  sp_yr_sum_long_sf <- sp_yr_sum_long_sf %>%
  	rename(time_period = pct_change,
  				 pct_change = value)
  
  sp_yr_sum_long_sf$time_period <- str_replace(sp_yr_sum_long_sf$time_period,
  																						 "pct_change_", "")

  	
  	plot <- 
  		ggplot() +
					geom_sf(data = sp_yr_sum_long_sf, aes(color = pct_change))  +
					geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
					coord_sf(crs = 3338) +
  		    facet_wrap(~ time_period, nrow = 1, labeller = labeller(
							 	time_period = as_labeller(facet_labeller_top))) +
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
    	  	labs(colour = "% change in\nspawning habitat\nsuitability") +
					theme_bw() +
 					theme(
 						strip.background = element_blank(),
 						strip.text = element_text(size = 12),
 						axis.text = element_text(size = 12),	
  					axis.title = element_text(size = 14),
  					legend.title.align=0.5)
  	

  	ggsave("./output/plots/percent change_hind.png",
		plot,
		width = 13, height = 7, units = "in")
	
  
  #### projections ####
  	
	# create a list of year bins

  year_bins_proj <- list('dec_1985_1989' = c(1985:1994),
                    		 'dec_1990_1994' = c(1995:2004),
                    		 'dec_1995_1999' = c(2000:2014),
                    		 'dec_2000_2004' = c(2010:2024),
  											 'dec_2005_2009' = c(2015:2034),
  											 'dec_2010_2014' = c(2020:2044),
  											 'dec_2015_2019' = c(2030:2054),
  											 'dec_2020_2024' = c(2040:2064),
  											 'dec_2025_2029' = c(2050:2074),
  											 'dec_2030_2034' = c(2060:2084),
  											 'dec_2035_2039' = c(2070:2094),
  											 'dec_2040_2044' = c(2070:2094),
  											 'dec_2045_2049' = c(2070:2094),
  											 'dec_2050_2054' = c(2070:2094),
  											 'dec_2055_2059' = c(2070:2094),
  											 'dec_2060_2064' = c(2070:2094),
  											 'dec_2065_2069' = c(2070:2094),
  											 'dec_2070_2074' = c(2070:2094),
  											 'dec_2075_2079' = c(2070:2094),
  											 'dec_2080_2084' = c(2070:2094),
  											 'dec_2085_2089' = c(2070:2094),
  											 'dec_2090_2094' = c(2070:2094),
  											 'dec_2095_2099' = c(2070:2094))

	# create a list of dfs summarized by year bins
	sum_yr_bin <- function(x){
 
		df <- ROMS_projected_dat %>%
		filter(year %in% x) %>%
		group_by(simulation, projection, longitude, latitude) %>%
		summarise(mean_hs = mean(hatch_success_cauchy_var),
							mean_temp = mean(bc_temp_sd),
							mean_sphabsuit = mean(sp_hab_suit_var)) 
		
		df
	
	}

	df_list <- lapply(year_bins_proj, sum_yr_bin)
	
	# add a column of the year bin
	df_list_tp <- mapply(cbind, df_list, "time_period"= names(year_bins_proj), SIMPLIFY = FALSE)


	# bind all rows of all dfs together and add a column for longitude not on 360 scale
	ROMS_projected_dat_yrs_sum <- bind_rows(df_list_tp) 
 
  year_list_proj <- list('dec_1985_1989',
                    		 'dec_1990_1994',
                    		 'dec_1995_1999',
                    		 'dec_2000_2004',
  											 'dec_2005_2009',
  											 'dec_2010_2014',
  											 'dec_2015_2019',
  											 'dec_2020_2024',
  											 'dec_2025_2029',
  											 'dec_2030_2034',
  											 'dec_2035_2039',
  											 'dec_2040_2044',
  											 'dec_2045_2049',
  											 'dec_2050_2054',
  											 'dec_2055_2059',
  											 'dec_2060_2064',
  											 'dec_2065_2069',
  											 'dec_2070_2074',
  											 'dec_2075_2079',
  											 'dec_2080_2084',
  											 'dec_2085_2089',
  											 'dec_2090_2094',
  											 'dec_2095_2099')


	reshape_func <- function(x){
		
		new_dat <- ROMS_projected_dat_yrs_sum %>% filter(., time_period == x)

  	new_dat <- new_dat %>% 
  		rename(!!paste0("mean_sphabsuit_", x) := mean_sphabsuit) %>%
  		dplyr::select(- time_period)
  			
    new_dat
    
  }
  
  df_list_names <- lapply(year_list_proj, reshape_func)
  
  sims_historical <- df_list_names[c(1:6)]
  
  sims_historical_dfs <- merge(sims_historical, by = c("simulation", "projection", 
  																										 "longitude", "latitude",
  																										 "mean_hs", "mean_temp"))
  
  sims_historical_dfs <- sims_historical_dfs %>%
  	rename(simulation = simulation...1,
  				 projection = projection...2,
  				 latitude = latitude...4,
  				 longitude = longitude...3) %>%
  	dplyr::select(- contains("simulation...")) %>%
  	dplyr::select(- contains("projection...")) %>%
  	dplyr::select(- contains("latitude...")) %>%
  	dplyr::select(- contains("longitude...")) %>%
  	dplyr::select(- contains("mean_hs..."))


  sims_historical_dfs <- sims_historical_dfs %>%
  	rowwise() %>%
  	mutate( pct_change_1985_2004=
  				 	(mean_sphabsuit_1995_2004 - mean_sphabsuit_1985_1994),
  				  pct_change_1995_2014 = 
  				 	(mean_sphabsuit_2005_2014 - mean_sphabsuit_1995_2004)
					 )
  
   label = c("1985 to 1995",'1995 to 2005')
 
  sims_historical_dfs <- sims_historical_dfs %>%
  	dplyr::select(- contains("mean_sphabsuit"))
 
  sims_historical_dfs_long <- sims_historical_dfs %>%
  	gather(pct_change, value, -c("latitude", "longitude", "simulation", "projection")) 
  
  sims_historical_dfs_long_sf <- sims_historical_dfs_long %>%
  	mutate(long_not_360 = longitude - 360) %>%
  	st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326)
  
  sims_historical_dfs_long_sf <- sims_historical_dfs_long_sf %>%
  	rename(time_period = pct_change,
  				 pct_change = value)
  
  sims_historical_dfs_long_sf$time_period <- str_replace(sims_historical_dfs_long_sf$time_period,
  																						 "pct_change_", "")

  	
  	plot <- 
  		ggplot() +
					geom_sf(data = sims_historical_dfs_long_sf, aes(color = pct_change))  +
					geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
					coord_sf(crs = 3338) +
  		    facet_grid(simulation ~ time_period) +
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
    	  	labs(colour = "% change in\nspawning habitat\nsuitability") +
					theme_bw() +
 					theme(
 						strip.background = element_blank(),
 						strip.text = element_text(size = 14),
 						axis.text = element_text(size = 12),	
  					axis.title = element_text(size = 14),
  					legend.title.align=0.5)
  	

  	ggsave("./output/plots/percent change_proj.png",
		plot,
		width = 20, height = 10, units = "in")
	
  
  ### scenarios ###
  	
  sims_scens <- df_list_names[c(4:11)]

  sims_ssp126_dfs <- sims_ssp126

  sims_historical_dfs <- sims_historical_dfs %>%
  	rename(simulation = simulation...1,
  				 projection = projection...2,
  				 latitude = latitude...3,
  				 longitude = longitude...4) %>%
  	dplyr::select(- contains("simulation...")) %>%
  	dplyr::select(- contains("projection...")) %>%
  	dplyr::select(- contains("latitude...")) %>%
  	dplyr::select(- contains("longitude..."))

  sims_historical_dfs <- merge(sims_historical_dfs, df_hist3) 

  sims_historical_dfs <- sims_historical_dfs %>%
  	rowwise() %>%
  	mutate( pct_change1 =
  				 	(mean_sphabsuit_1995_2004 - mean_sphabsuit_1985_1994),
  				  pct_change2 = 
  				 	(mean_sphabsuit_2005_2014 - mean_sphabsuit_1995_2004)
					 )
  
   label = c("1985 to 1995",'1995 to 2005')
 
  sims_historical_dfs <- sims_historical_dfs %>%
  	dplyr::select(- contains("mean_sphabsuit"))
 
  sims_historical_dfs_long <- sims_historical_dfs %>%
  	gather(pct_change, value, -c("latitude", "longitude")) 
  
  sims_historical_dfs_long_sf <- sims_historical_dfs_long %>%
  	mutate(long_not_360 = longitude - 360) %>%
  	st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326)
  
  sims_historical_dfs_long_sf <- sims_historical_dfs_long_sf %>%
  	rename(time_period = pct_change,
  				 pct_change = value)
  
  sims_historical_dfs_long_sf$time_period <- str_replace(sims_historical_dfs_long_sf$time_period,
  																						 "pct_change_", "")

  	
  	plot <- 
  		ggplot() +
					geom_sf(data = sims_historical_dfs_long_sf, aes(color = pct_change))  +
					geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
					coord_sf(crs = 3338) +
  		    facet_wrap(~ time_period, nrow = 1) +
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
    	  	labs(colour = "% change in\nspawning habitat\nsuitability") +
					theme_bw() +
 					theme(
 						strip.background = element_blank(),
 						strip.text = element_blank(),
 						axis.text = element_text(size = 12),	
  					axis.title = element_text(size = 14),
  					legend.title.align=0.5)
  	

  	ggsave("./output/plots/percent change_proj.png",
		plot,
		width = 20, height = 10, units = "in")
	
  
  	
  	
  	
  	
  	
  	
  	
  	
  	#### specific year bins -- recent vs historic ####
  	
	# summarize data into year bins
  years_keep <- c(2000:2020)
  	
  sm_temp_hind_df_sub <- sm_temp_hind_df %>% filter(., year %in% years_keep)
  

	# create a list of year bins
	year_bins  <- list('2000_2014' = c(2000:2014),
                     '2015_2020' = c(2015:2020)) 

	# create a list of dfs summarized by year bins
	sum_yr_bin <- function(x){
 
		df <- sm_temp_hind_df_sub %>%
		filter(year %in% x) %>%
		group_by(longitude, latitude) %>%
		summarise(mean_hs = mean(hatch_success_cauchy),
							mean_temp = mean(temp),
							mean_sphabsuit = mean(sp_hab_suit)) 
		
		df
	
	}

	df_list <- lapply(year_bins, sum_yr_bin)

	# add a column of the year bin
	df_list_tp <- mapply(cbind, df_list, "time_period"= names(year_bins), SIMPLIFY = FALSE)

	# bind all rows of all dfs together and add a column for longitude not on 360 scale
	sm_temp_hind_df_yr_sum <- bind_rows(df_list_tp) %>% 
  	dplyr::select(latitude, longitude, mean_sphabsuit, time_period)
 
  year_list  <- list("2000_2014",
                     "2015_2020")

  	reshape_func <- function(x){
		
		new_dat <- sm_temp_hind_df_yr_sum %>% filter(., time_period == x)

  	new_dat <- new_dat %>% 
  		rename(!!paste0("mean_sphabsuit_", x) := mean_sphabsuit) %>%
  		dplyr::select(- time_period)
  			
    new_dat
    
  }
  
  df_list <- lapply(year_list, reshape_func)
  
  sp_yr_sum <- bind_cols(df_list)
  
  sp_yr_sum <- sp_yr_sum[- c(4:5)]
  
  sp_yr_sum <- sp_yr_sum %>%
  	rename(latitude = latitude...1,
  				 longitude = longitude...2)
  
  sp_yr_sum <- sp_yr_sum %>%
  	rowwise() %>%
  	mutate(pct_change = 
  				 	(mean_sphabsuit_2015_2020 - mean_sphabsuit_2000_2014) * 100)
  
  sp_yr_sum_sf <- sp_yr_sum %>%
  	mutate(long_not_360 = longitude - 360) %>%
  	st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326)
 
  	
  	plot <- 
  		ggplot() +
					geom_sf(data = sp_yr_sum_sf, aes(color = pct_change))  +
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
    	  	labs(colour = "% change") +
					theme_bw() +
  				ggtitle(label = "Absolute percent change of spawning habitat\nsuitability from 2000-2014 to 2015-2020") +
 					theme(
 						axis.text = element_text(size = 12),	
  					axis.title = element_text(size = 14),
  					legend.title.align=0.5)
  	

  	ggsave("./output/plots/percent_change_historic_today.png",
		plot,
		width = 5, height = 5, units = "in")
	
  