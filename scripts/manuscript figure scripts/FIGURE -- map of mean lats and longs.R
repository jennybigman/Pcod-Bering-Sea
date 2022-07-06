# figure -- map of mean latitude and longitude over time

	#### hindcast ####
  
 	coords_hind_func <- function(x){
		
		new_dat <- ROMS_hindcast_dat %>%
			filter(., sp_hab_suit >= x)
		
		new_dat_sum <- new_dat %>%
			group_by(year) %>%
			summarise(latitude = mean(latitude),
								longitude = mean(longitude)) %>%
			mutate(sp_hab_threshold = x)

		new_dat_sum		
	}
	
	sp_hab_thresholds <- c(0.5, 0.9)
	
	coords_hind <- lapply(sp_hab_thresholds, coords_hind_func)
	
	coords_hind <- bind_rows(coords_hind) 
	
	coords_hind_sf <- coords_hind %>%
		mutate(long_not_360 = case_when(
				longitude >= 180 ~ longitude - 360,
				longitude < 180 ~ longitude)) %>%
  	st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326, remove = FALSE)

	
	## projection
	
	coords_proj_func <- function(x){
		
		new_dat <- ROMS_projected_dat %>%
			filter(., sp_hab_suit_var >= x)
		
		new_dat_sum <- new_dat %>%
			group_by(projection, year) %>%
			summarise(proj_mean_lat = mean(latitude),
								proj_mean_long = mean(long_not_360))  %>%
			mutate(sp_hab_threshold = x)

		new_dat_sum		
	}
	
	sp_hab_thresholds <- c(0.5, 0.9)
	
	coords_proj <- lapply(sp_hab_thresholds, coords_proj_func)
	
	coords_proj <- bind_rows(coords_proj) 
	
	years_proj <- 2020:2099
	
	coords_proj <- coords_proj %>%
		filter(year %in% years_proj)
	
	coords_proj_sf <- coords_proj %>%
  	st_as_sf(coords = c("proj_mean_long", "proj_mean_lat"), crs = 4326, remove = FALSE)
		
	## set up for plotting
	
			
	coords_proj_sf$scen <- NA
	
	coords_proj_sf$scen[coords_proj_sf$projection == "ssp126"] <- "low emission\n(ssp126)"
	coords_proj_sf$scen[coords_proj_sf$projection == "ssp585"] <- "high emission\n(ssp585)"

	coords_proj_sf$scen_f = factor(coords_proj_sf$scen, 
																		levels=c('low emission\n(ssp126)', 'high emission\n(ssp585)'))
	


	coords_proj_sf$thresh <- NA
	coords_hind_sf$thresh <- NA

	coords_proj_sf$thresh[coords_proj_sf$sp_hab_threshold == 0.5] <- "potential"
	coords_proj_sf$thresh[coords_proj_sf$sp_hab_threshold == 0.9] <- "core"
	
	coords_hind_sf$thresh[coords_hind_sf$sp_hab_threshold == 0.5] <- "potential"
	coords_hind_sf$thresh[coords_hind_sf$sp_hab_threshold == 0.9] <- "core"
	
	
	coords_proj_sf$scen_f = factor(coords_proj_sf$scen, 
																		levels=c('low emission\n(ssp126)', 'high emission\n(ssp585)'))

		
		map_mean_lats <- 
		 	ggplot() +
			geom_point(data = coords_hind_sf, aes(color = year, 
  			geometry = geometry), alpha = 0.5, 
    		stat = "sf_coordinates") +
			geom_point(data = coords_proj_sf, aes(color = year, 
  			geometry = geometry), alpha = 0.5, 
    		stat = "sf_coordinates") +
			coord_sf(crs = 3338) +
 			scale_x_continuous(
 				breaks = c(-167, -169),
 				labels = c("-167˚", "-169˚"),
 				name = "Longitude",
 				limits = c(-1030000, -750000)) +
 			scale_y_continuous(
 				breaks = c(56, 57, 58),
 				limits = c(700000, 1090000),
 				name = "Latitude") +
		facet_grid(thresh ~ scen_f) +
		scale_color_viridis_c(
			breaks = c(1970, 2030, 2090)
		) +
	  theme_bw() +
 		theme(panel.spacing = unit(0.25, "lines"),
			legend.title.align = 0.5,
 			panel.border = element_rect(color = "#666666"),
 			strip.text.x = element_text(size = 12, face = "bold",  color = "black"),
 			strip.text.y = element_text(size = 12, face = "bold",  color = "black"),
 			strip.background = element_blank(),
 			axis.text = element_text(size = 10,  color = "#666666"),	
  		axis.title = element_text(size = 12,  color = "#666666"),
 			axis.ticks = element_line( color = "#666666"))

		ggsave("./output/plots/mean_lats_map.png",
			 map_mean_lats,
			 width = 5, height = 7, units = "in")