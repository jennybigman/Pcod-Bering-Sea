# NOAA bathy

###############	 	
	# restrict to depths < 250 m ####
	###############
	
	BS_bathy <- getNOAA.bathy(lon1 = 179.95, lon2 = -158.01, lat1 = 53, 
														lat2 = 65.75, resolution = 1, antimeridian = TRUE,
														keep = TRUE)

	#plot(BS_bathy, image = TRUE, land = TRUE, axes = FALSE, lwd = 0.1,
	#		 bpal = list(c(0, max(BS_bathy), grey(0.7), grey(0.9), grey(0.95)),
	#		 						c(min(BS_bathy), 0, "darkblue", "lightblue")))
	#plot(BS_bathy, n = 1, lwd = 0.5, add = TRUE)
	
	longslats <- ROMS_dat_hind_poly %>% 
		dplyr::select(longitude, latitude) %>%
		distinct(across(everything())) %>%
		filter(., between(longitude, 179, 202))
	
	depths <- get.depth(BS_bathy, longslats, locator = FALSE)
	
	depths <- depths %>%
		rename(latitude = lat,
					 longitude = lon)
	
	ROMS_dat_hind_poly_depth <- merge(ROMS_dat_hind_poly, 
																		depths, 
																		by = c("longitude", "latitude")) %>%
															filter(., between(depth, -250, 0))
	
