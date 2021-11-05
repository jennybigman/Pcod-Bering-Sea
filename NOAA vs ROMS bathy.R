# compare NOAA bathy to ROMS bathy

	library(here)
	library(marmap)
  library(ncdf4)
  library(thredds)   
  library(reshape2)
  library(data.table)
<<<<<<< HEAD

	#### NOAA bathy ####

	NOAA_bathy <- getNOAA.bathy(lon1 = 156.4388, lon2 = -144.8651, lat1 = 45.00381, 
															lat2 = 69.69127, resolution = 1, antimeridian = TRUE,
														  keep = TRUE) # coords match full ROMS coords
	
	# trim bathy to only those coords of study region
	
	# coords of study region
	longslats <- ROMS_dat_hind_trim %>% 
		dplyr::select(longitude, latitude) %>%
		distinct(across(everything())) 
	
	depths <- get.depth(NOAA_bathy, longslats, locator = FALSE)
	
	NOAA_bathy_depths <- depths %>%
		rename(latitude = lat,
					 longitude = lon)

  #### NOAA bathy plot ####
  
  # convert to sf
  NOAA_bathy_depths$lats <- NOAA_bathy_depths$latitude
  
  NOAA_bathy_depths_sf <- NOAA_bathy_depths %>% 
  	mutate(long_not_360 = case_when(
  			longitude >= 180 ~ longitude - 360,
				longitude < 180 ~ longitude)) %>%
  	st_as_sf(coords = c("long_not_360", "lats"), crs = 4326)
  
  NOAA_bathy_plot <- 
  	ggplot() +
  	geom_point(data = NOAA_bathy_depths, aes(x = longitude, y = latitude, color = depth))
  
  ggsave("./output/plots/NOAA_bathy_plot.png",
		NOAA_bathy_plot,
		width = 10, height = 7, units = "in")
  
  NOAA_bathy_plot_sf <- 
  	ggplot() +
  	geom_sf(data = NOAA_bathy_depths_sf, aes(color = depth))  +
  	coord_sf(crs = 3338) 
  
  ggsave("./output/plots/NOAA_bathy_plot_sf.png",
		NOAA_bathy_plot_sf,
		width = 10, height = 7, units = "in")
=======
	library(scales)

>>>>>>> f545ca04b272d73d72bec7347995d58a36e6b703

	#### ROMS bathy ####
  		
	# download ROMS bathy

	url_base <- "https://data.pmel.noaa.gov/aclim/thredds/"
  opendap  <- "dodsC/extended_grid/Bering10K_extended_grid.nc"
  
  nc <- nc_open(paste(url_base,opendap,sep=""))
  
  ## to see data 
  
  # Open ncdf4 connection with dataset
  #nc_handle <- nc_open(paste(url_base,opendap,sep="")) 

  # Show metadata from this dataset
	#nc_handle

  # Close the connection
  #nc_close(nc_handle)
    
    
  # create objects for known lats and longs and xi and eta axes
  lats <- ncvar_get(nc,"lat_rho")
  lons <- ncvar_get(nc,"lon_rho")
  
  xi_axis  <- seq(1,182) # Hardcoded axis length, ROMS coordinates
  eta_axis <- seq(1,258) # Hardcoded axis length, ROMS coordinates

  # download array
  final_bathy_array <- ncvar_get(nc, "h") # long name is 'final bathymetry at RHO-points'
  
  # turn into dataframe from array
  
  ROMS_fbathy_df <- reshape2::melt(final_bathy_array, 
                  	varnames=c("Xi", "Eta"), 
                  	value.name="depth")

  # add lat/long cols
  ROMS_fbathy_df$longitude <- lons[cbind(ROMS_fbathy_df$Xi,ROMS_fbathy_df$Eta)]
  ROMS_fbathy_df$latitude <- lats[cbind(ROMS_fbathy_df$Xi,ROMS_fbathy_df$Eta)]
	
  # if want to also do 'working bathymetry'
  # working_bathy_array <- ncvar_get(nc, "hraw") # long name is 'working bathymetry at RHO-points'
  
  # turn into dataframe from array
  # ROMS_wbathy_df <- reshape2::melt(working_bathy_array, 
  #                 	varnames=c("Xi", "Eta"), 
  #                 	value.name="depth")

  # add lat/long cols
  # ROMS_wbathy_df$longitude <- lons[cbind(ROMS_wbathy_df$Xi,ROMS_wbathy_df$Eta)]
  # ROMS_wbathy_df$latitude <- lats[cbind(ROMS_wbathy_df$Xi,ROMS_wbathy_df$Eta)]

	# save df
  #fwrite(ROMS_wbathy_df, "./data/ROMS_working_bathy.csv")
  
  # close the connection
  nc_close(nc)
  
  # trim ROMS bathy to study region

  # load Ortiz Bering Sea regions and convert to sf object
	bsregions <- readOGR("./Mapping Code - Bigman/Ortiz Regions", layer="BSIERP_regions_2012")
	bsregions <- spTransform(bsregions, CRS("+init=epsg:4326"))
	bsregions_sf <- st_as_sf(bsregions)
	bsregions_no_15 <-bsregions[bsregions@data$DOMAIN != 15, ] # remove domain 15 

	bsregions_no_15_sf <- bsregions_no_15 %>% 	  
		st_as_sf(coords = c("long", "lat"), crs = 4326) 
	poly_bsregions_no_15_sf <- st_union(bsregions_no_15_sf)
	
	# remove domain 15 to fix it
	bsregions15 <-bsregions[bsregions@data$DOMAIN == 15, ] 

	# fix domain 15
	bsregions15@data$id = rownames(bsregions15@data)
	bsregions_points15 <- fortify(bsregions15, region="id")
	bsregions15_df <- left_join(bsregions_points15, bsregions15@data, by = "id") %>%
		filter(., long < 0)
	bsregions_sf_15 <- bsregions15_df %>% st_as_sf(coords = c("long", "lat"), crs = 4326)
	
	polygon_bsregion15 <- 
  	bsregions15_df %>%
	  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
	  summarise(geometry = st_combine(geometry)) %>%
	  st_cast("POLYGON") 
	
	# add back into polygin
	full_poly <-  st_union(polygon_bsregion15, poly_bsregions_no_15_sf)
	
	# find intersection of points in df and polygon
	ROMS_fbathy_df_sf <- ROMS_fbathy_df %>% 
<<<<<<< HEAD
  	mutate(long_not_360 = case_when(
  			longitude >= 180 ~ longitude - 360,
				longitude < 180 ~ longitude)) %>%
  	st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326)
	
	ROMS_fbathy_df_sf <- st_intersection(ROMS_fbathy_df_sf, full_poly) 
	
	#### plot ROMS bathy ####
	
	ROMS_fbathy_df_sf$depth2 <- ROMS_fbathy_df_sf$depth * -1
	
  ROMS_fbathy_sf_plot <-
  	ggplot() +
  	geom_sf(data = ROMS_fbathy_df_sf, aes(color = depth2)) +
  	coord_sf(crs = 3338) 
  
  	ggsave("./output/plots/ROMS_fbathy_sf_plot.png",
			 ROMS_fbathy_sf_plot,
			 width = 10, height = 7, units = "in")

	#### length of dfs ####
  nrow(NOAA_bathy_depths_sf)	
  nrow(ROMS_fbathy_df_sf)
  
  ## still a discrepancy of ~ 800 ! 
  	
=======
  	mutate(lats = latitude,
  		long_not_360 = case_when(
  			longitude >= 180 ~ longitude - 360,
				longitude < 180 ~ longitude)) %>%
  	st_as_sf(coords = c("long_not_360", "lats"), crs = 4326)
	
	ROMS_fbathy_df_sf <- st_intersection(ROMS_fbathy_df_sf, full_poly) 
	
	# remove depths > 250
	ROMS_fbathy_df_sf_shallow <- ROMS_fbathy_df_sf %>%
  		filter(., between(depth, 0, 250))
	
	#### plot ROMS bathy ####
	
  ROMS_fbathy_sf_plot <-
  	ggplot() +
  	geom_sf(data = ROMS_fbathy_df_sf, aes(color = depth)) +
  	coord_sf(crs = 3338)
	
	 	ggsave("./output/plots/ROMS_fbathy_sf_plot.png",
			 ROMS_fbathy_sf_plot,
			 width = 10, height = 7, units = "in")

	 ROMS_fbathy_sf_plot_shallow <-
  	ggplot() +
  	geom_sf(data = ROMS_fbathy_df_sf_shallow, aes(color = depth)) +
  	coord_sf(crs = 3338) 
 
  	ggsave("./output/plots/ROMS_fbathy_sf_plot_shallow.png",
			 ROMS_fbathy_sf_plot_shallow,
			 width = 10, height = 7, units = "in")

	#### NOAA bathy ####

	NOAA_bathy <- getNOAA.bathy(lon1 = 156.4388, lon2 = -144.8651, lat1 = 45.00381, 
															lat2 = 69.69127, resolution = 1, antimeridian = TRUE,
														  keep = TRUE) # coords match full ROMS coords
	
	# trim bathy to only those coords of study region
	
	# coords of study region
  ROMS_fbathy_df <- ROMS_fbathy_df %>% 
  	mutate(lats = latitude,
  				 long_not_360 = case_when(
  						longitude >= 180 ~ longitude - 360,
							longitude < 180 ~ longitude))
  
	longslats <- ROMS_dat_hind_trim %>% 
		dplyr::select(longitude, latitude) %>%
		distinct(across(everything())) 
	
#	longlats <- ROMS_fbathy_df_sf %>% 
#		dplyr::select(latitude, longitude) %>%
#		st_drop_geometry() %>%
#			mutate(longitude = case_when(
#  						longitude >= 180 ~ longitude - 360,
#							longitude < 180 ~ longitude))
	
	depths <- get.depth(NOAA_bathy, longslats, locator = FALSE)
	
	NOAA_bathy_depths <- depths %>%
		rename(latitude = lat,
					 longitude = lon) %>%
		mutate(depth_pos = depth * -1)

  #### NOAA bathy plot ####
  
  # convert to sf
  NOAA_bathy_depths$lats <- NOAA_bathy_depths$latitude
  
  NOAA_bathy_depths_sf <- NOAA_bathy_depths %>% 
  	mutate(long_not_360 = case_when(
  			longitude >= 180 ~ longitude - 360,
				longitude < 180 ~ longitude)) %>%
  	st_as_sf(coords = c("long_not_360", "lats"), crs = 4326)
  
  ## remove any points with a depth > 250 m
  
  NOAA_bathy_depths_sf_shallow <- NOAA_bathy_depths_sf %>%
  		filter(., between(depth_pos, 0, 250))
  
  ## plot
  
  NOAA_bathy_plot <- 
  	ggplot() +
  	geom_point(data = NOAA_bathy_depths, aes(x = longitude, y = latitude, 
  																					 color = depth_pos))
  
  ggsave("./output/plots/NOAA_bathy_plot.png",
		NOAA_bathy_plot,
		width = 10, height = 7, units = "in")
  
  NOAA_bathy_plot_sf <- 
  	ggplot() +
  	geom_sf(data = NOAA_bathy_depths_sf, aes(color = depth_pos))  +
  	coord_sf(crs = 3338) +
  		scale_x_continuous(
 				breaks = c(-175, -170, -165, -160),
 				labels = c("-175˚", "-170˚", "-165˚", "-160˚"),
 				name = "Longitude",
 				limits = c(-1400000, -150000))
 
  ggsave("./output/plots/NOAA_bathy_plot_sf.png",
		NOAA_bathy_plot_sf,
		width = 10, height = 7, units = "in")
  
  # weird data point at 196 (elevation?)
  ggplot() +
  	geom_sf(data = NOAA_bathy_depths_sf, aes(color = depth)) +
  	geom_point(aes(x = -166.5214, y = 53.90532))
  
  # plot shallow
  
   NOAA_bathy_plot_shallow <- 
  	ggplot() +
  	geom_sf(data = NOAA_bathy_depths_sf_shallow, aes(color = depth_pos))  +
  	coord_sf(crs = 3338) 
  
  ggsave("./output/plots/NOAA_bathy_plot_shallow.png",
		NOAA_bathy_plot_shallow,
		width = 10, height = 7, units = "in")
  
  ## plot difference
  
  names(NOAA_bathy_depths_sf)
  names(ROMS_fbathy_df_sf)
  
  NOAA_bathy_depths2 <- NOAA_bathy_depths_sf %>%
  	mutate(NOAA_lat = latitude,
  				 NOAA_long = longitude,
  				 NOAA_depth = depth_pos) %>%
  	st_drop_geometry() 
  
  ROMS_fbathy_df2 <- ROMS_fbathy_df_sf %>%
  	dplyr::select(-Xi, -Eta) %>%
  	mutate(ROMS_lat = latitude,
  				 ROMS_long = longitude,
  				 ROMS_depth = depth)  %>%
  	st_drop_geometry()
  
  bathys <- merge(NOAA_bathy_depths2, ROMS_fbathy_df2,
  								by = c("latitude", "longitude"))
  
  bathys <- bathys %>%
  	mutate(depth_difference = NOAA_depth - ROMS_depth,
  				 long_not_360 = case_when(
  				 		longitude >= 180 ~ longitude - 360,
							longitude < 180 ~ longitude)) %>%
  	st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326)
  

  bathy_diff_plot <- ### fix color scale
  	ggplot() +
  	geom_sf(data = bathys, aes(color = depth_difference))  +
  	coord_sf(crs = 3338) +
  	scale_x_continuous(
 				breaks = c(-175, -170, -165, -160),
 				labels = c("-175˚", "-170˚", "-165˚", "-160˚"),
 				name = "Longitude",
 				limits = c(-1400000, -150000)) +
  	scale_color_gradient2(
  		low = muted("red"),
  		mid = "white",
  		high = muted("dodgerblue"),
  		midpoint = 0
  	) +
  	ggtitle("NOAA - ROMS bathymetry\n(blue = NOAA deeper, red = ROMS deeper)")
  
  ggsave("./output/plots/bathy_diff_plot.png",
		bathy_diff_plot,
		width = 10, height = 7, units = "in")
  
  # bathy difference 0  to 250 m
  
  bathys_shallow <- bathys %>%
  		filter(between(NOAA_depth, 0, 250)) %>%
  		filter(between(ROMS_depth, 0, 250)) %>%
  	mutate(depth_difference = NOAA_depth - ROMS_depth)
  
  bathy_diff_shallow_plot <- ### fix color scale
  	ggplot() +
  	geom_sf(data = bathys_shallow, aes(color = depth_difference))  +
  	coord_sf(crs = 3338) +
  	scale_x_continuous(
 				breaks = c(-175, -170, -165, -160),
 				labels = c("-175˚", "-170˚", "-165˚", "-160˚"),
 				name = "Longitude",
 				limits = c(-1400000, -150000)) +
  	scale_color_gradient2(
  		low = muted("red"),
  		mid = "white",
  		high = muted("dodgerblue"),
  		midpoint = 0
  	) +
  	ggtitle("NOAA - ROMS bathymetry\n(blue = NOAA deeper, red = ROMS deeper)")
  
  
  ggsave("./output/plots/bathy_diff_shallow_plot.png",
		bathy_diff_shallow_plot,
		width = 10, height = 7, units = "in")
  
>>>>>>> f545ca04b272d73d72bec7347995d58a36e6b703
