
	# summarize by lat/lon and convert to sf object
	proj_temp_dat_all$latitude <- proj_temp_dat_all$Lat
	proj_temp_dat_all$longitude <- proj_temp_dat_all$Lon

  cesm_dfs_trim_sum <- cesm_dfs_trim %>%
		group_by(Lat, Lon) %>%
		summarize(mean_temp = mean(temp)) %>%
		mutate(long_not_360 = case_when(
							Lon >= 180 ~ Lon - 360,
							Lon < 180 ~ Lon)) %>%
  	st_as_sf(coords = c("long_not_360", "Lat"), crs = 4326)
  
  # make a summary object of the hindcast data for intersecting the lat/lons
  ROMS_hindcast_dat_sum <- ROMS_hindcast_dat %>%
		group_by(latitude, longitude) %>%
 		summarise(mean_temp = mean(temp)) %>%
		mutate(long_not_360 = case_when(
				   longitude >= 180 ~ longitude - 360,
				   longitude < 180 ~ longitude)) %>%
  	st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326)
	
	dat_ints <- st_intersection(cesm_dfs_trim_sum, ROMS_hindcast_dat_sum)
  
	cesm_dat_trim <- cesm_dfs_trim %>% 
		filter(., Lon %in% dat_ints$longitude) %>%
		filter(., Lat %in% dat_ints$latitude)

	fwrite(cesm_dat_trim, "./data/cesm_dat_trim.csv")

