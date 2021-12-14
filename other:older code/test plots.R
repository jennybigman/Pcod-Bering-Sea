### test plots ####

	#### CESM hist temp  ####

	cesm_hist_sum <- cesm_hist_dfs %>%
			group_by(Lat, Lon) %>%
			summarize(mean_temp = mean(temp)) %>%
			mutate(latitude = Lat,
						 longitude = Lon, 
						 long_not_360 = case_when(
						 longitude >= 180 ~ longitude - 360,
						 longitude < 180 ~ longitude)) %>%
  		st_as_sf(coords = c("long_not_360", "Lat"), crs = 4326)
	
		
	ggplot() +
			geom_sf(data = cesm_hist_sum, aes(color = mean_temp))  +
			geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
			coord_sf(crs = 3338) +
			scale_x_continuous(
				breaks = c(-175, -170, -165, -160),
				labels = c("-175˚", "-170˚", "-165˚", "-160˚"),
				name = "Longitude",
				limits = c(-1400000, -150000)
			) +
			scale_y_continuous(
				breaks = c(55, 60),
				limits = c(470000, 1900000),
				name = "Latitude"
			)
			
	#### CESM ssp126 temp  ####

	cesm_ssp126_sum <- cesm_ssp126_dfs %>%
			group_by(Lat, Lon) %>%
			summarize(mean_temp = mean(temp)) %>%
			mutate(latitude = Lat,
						 longitude = Lon, 
						 long_not_360 = case_when(
						 longitude >= 180 ~ longitude - 360,
						 longitude < 180 ~ longitude)) %>%
  		st_as_sf(coords = c("long_not_360", "Lat"), crs = 4326)
	
		
	ggplot() +
		geom_sf(data = cesm_ssp126_sum, aes(color = mean_temp))  +
		geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
		coord_sf(crs = 3338) +
 		scale_x_continuous(
 			breaks = c(-175, -170, -165, -160),
 			labels = c("-175˚", "-170˚", "-165˚", "-160˚"),
 			name = "Longitude",
 			limits = c(-1400000, -150000)
 		) +
 		scale_y_continuous(
 			breaks = c(55, 60),
 			limits = c(470000, 1900000),
 			name = "Latitude"
 		)
			
	
		#### CESM ssp585 temp  ####

	cesm_ssp585_sum <- cesm_ssp585_dfs %>%
			group_by(Lat, Lon) %>%
			summarize(mean_temp = mean(temp)) %>%
			mutate(latitude = Lat,
						 longitude = Lon, 
						 long_not_360 = case_when(
						 longitude >= 180 ~ longitude - 360,
						 longitude < 180 ~ longitude)) %>%
  		st_as_sf(coords = c("long_not_360", "Lat"), crs = 4326)
	
	ggplot() +
		geom_sf(data = cesm_ssp126_sum, aes(color = mean_temp))  +
		geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
		coord_sf(crs = 3338) +
 		scale_x_continuous(
 			breaks = c(-175, -170, -165, -160),
 			labels = c("-175˚", "-170˚", "-165˚", "-160˚"),
 			name = "Longitude",
 			limits = c(-1400000, -150000)
 		) +
 		scale_y_continuous(
 			breaks = c(55, 60),
 			limits = c(470000, 1900000),
 			name = "Latitude"
 		)
	
	## all good, now check trimmed dfs
	
	cesm_dat_trim_hist <- cesm_dat_trim %>% filter(., projection == "historical")
		
	cesm_dat_trim_hist_sum <- cesm_dat_trim_hist %>%
			group_by(Lat, Lon) %>%
			summarize(mean_temp = mean(temp)) %>%
			mutate(latitude = Lat,
						 longitude = Lon, 
						 long_not_360 = case_when(
						 longitude >= 180 ~ longitude - 360,
						 longitude < 180 ~ longitude)) %>%
  		st_as_sf(coords = c("long_not_360", "Lat"), crs = 4326)
	
		
	ggplot() +
			geom_sf(data = cesm_dat_trim_hist_sum, aes(color = mean_temp))  +
			geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
			coord_sf(crs = 3338) +
			scale_x_continuous(
				breaks = c(-175, -170, -165, -160),
				labels = c("-175˚", "-170˚", "-165˚", "-160˚"),
				name = "Longitude",
				limits = c(-1400000, -150000)
			) +
			scale_y_continuous(
				breaks = c(55, 60),
				limits = c(470000, 1900000),
				name = "Latitude"
			)
			
	
	### issue with overlap, try saved proj