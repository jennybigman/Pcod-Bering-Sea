	# coords of study region
	longslats <- ROMS_dat_hind_trim %>% 
		dplyr::select(longitude, latitude) %>%
		distinct(across(everything())) 
	
	depths <- get.depth(NOAA_bathy, longslats, locator = FALSE)

	
	##############################################################
	bsregions_df2 <- bsregions_df %>%
		rename(long_not_360 = long,
					 latitude = lat) %>%
		mutate(longitude2 = case_when(
			long_not_360 >= -189 ~ long_not_360 + 360,
			long_not_360 < 180 ~ long_not_360))
	
	max(bsregions_df2$longitude2)
	min(bsregions_df2$longitude2)
	max(bsregions_df2$latitude)
	min(bsregions_df2$latitude)

	NOAA_bathy <- getNOAA.bathy(lon1 = -179.99999999, lon2 = -144, lat1 = 52, 
															lat2 = 66, resolution = 1, antimeridian = TRUE,
														  keep = TRUE) # coords match full ROMS coords


	longslats <- bsregions_df2 %>%
		dplyr::select(longitude2, latitude) %>%
		distinct(across(everything())) %>%
		rename(longitude = longitude2)
	
	depths <- get.depth(NOAA_bathy, longslats, locator = FALSE)
	
	
	b <- fortify.bathy(NOAA_bathy)
	