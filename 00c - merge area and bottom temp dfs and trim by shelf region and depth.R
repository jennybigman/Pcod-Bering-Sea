# merge area and bottom temp dfs and trim by shelf region and depth

	library(here)
	library(data.table)

	#### merge dfs ####
	
	# load dfs
	temp_df <- fread("./data/ROMS_all_temp.csv")
	area_df <- fread( "./data/ROMS_area_grid_cells.csv")

	# merge dfs
	ROMS_dat_hind <- merge(temp_df, area_df, by = c("latitude", "longitude"),
												 all = TRUE)
	
	ROMS_dat_hind <- na.omit(ROMS_dat_hind)

	#### trim df by shelf and depth ####
	
	## trim by shelf
	
	# add column of long not on 360 scale
	ROMS_dat_hind <- ROMS_dat_hind %>%
			mutate(long_not_360 = case_when(
				longitude >= 180 ~ longitude - 360,
				longitude < 180 ~ longitude
			))
	
	# create smaller dataframe for intersection function
	ROMS_dat_hind_sum <- ROMS_dat_hind %>%
		group_by(latitude, longitude) %>%
		summarise(mean_temp = mean(temp))

	# add column of long not on 360 scale
	ROMS_dat_hind_sum <- ROMS_dat_hind_sum %>%
			mutate(long_not_360 = case_when(
				longitude >= 180 ~ longitude - 360,
				longitude < 180 ~ longitude
	))
	
	# convert to shapefile for intersection function
	ROMS_dat_hind_sum_sf <- st_as_sf(ROMS_dat_hind_sum,
		coords = c("long_not_360", "latitude"), crs = 4326)

	###############	 	
	# remove points not on Bering Sea Shelf ####
	###############
	
	# load Ortiz Bering Sea regions and convert to sf object
	bsregions <- readOGR("./Mapping Code - Bigman/Ortiz Regions", layer="BSIERP_regions_2012")
	bsregions <- spTransform(bsregions, CRS("+init=epsg:4326"))
	bsregions_sf <- st_as_sf(bsregions)
	bsregions_no_15 <-bsregions[bsregions@data$DOMAIN != 15, ] # remove domain 15 

	#plot(bsregions_no_15)
	
	bsregions_no_15_sf <- bsregions_no_15 %>% 	  
		st_as_sf(coords = c("long", "lat"), crs = 4326) 
	poly_bsregions_no_15_sf <- st_union(bsregions_no_15_sf)
	
	#plot(poly_bsregions_no_15_sf)

	# remove domain 15 to fix it
	bsregions15 <-bsregions[bsregions@data$DOMAIN == 15, ] 
	#plot(bsregions15)

	# fix domain 15
	bsregions15@data$id = rownames(bsregions15@data)
	bsregions_points15 <- fortify(bsregions15, region="id")
	bsregions15_df <- left_join(bsregions_points15, bsregions15@data, by = "id") %>%
		filter(., long < 0)
	bsregions_sf_15 <- bsregions15_df %>% st_as_sf(coords = c("long", "lat"), crs = 4326)
	
	ggplot() +
		geom_polygon(data = bsregions15_df, aes(x = long, y = lat, group = group),
 								 fill = "darkgrey", color = NA) 

	polygon_bsregion15 <- 
  	bsregions15_df %>%
	  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
	  summarise(geometry = st_combine(geometry)) %>%
	  st_cast("POLYGON") 

	plot(polygon_bsregion15)

	# add back into polygin
	full_poly <-  st_union(polygon_bsregion15, poly_bsregions_no_15_sf)
	
	plot(full_poly$geometry)

	# find intersection of points in df and polygon
	int_pts_sum <- st_intersection(ROMS_dat_hind_sum_sf, full_poly) 
	
	int_pts_sum_albers <- st_transform(int_pts_sum, crs = 3338) # in albers for plotting
	
	# plot
	world_map_data <- ne_countries(scale = "medium", returnclass = "sf") 

 	combined_poly_plot <-
 		ggplot() +
		geom_sf(data = int_pts_sum_albers, aes(color = mean_temp))  +
		geom_sf(data = world_map_data, fill = "darkgrey", lwd = 0) +
		coord_sf(crs = 3338) +
		scale_color_viridis_c() +
 		scale_x_continuous(
 			breaks = c(-175, -170, -165, -160),
 			labels = c("-175˚", "-170˚", "-165˚", "-160˚"),
 			limits = c(-1400000, -150000)
 		) +
 		scale_y_continuous(
 			breaks = c(55, 60),
 			limits = c(470000, 1900000)
 		) +
		theme_bw() +
 		theme(
 			axis.text = element_text(size = 12),	
  		axis.title = element_text(size = 14),
  		legend.title.align=0.5)

	#ggsave("./Pcod-Bering-Sea/output/plots/combined_poly_plot.png",
	#	combined_poly_plot,
	#	width = 10, height = 10, units = "in")
	
	
 	## filter full dataframe by lats/longs in polygon
	
	# decompose geometry back into lat/long
	int_pts_sum_ll <- int_pts_sum %>%
			mutate(longitude_not_360 = sf::st_coordinates(.)[, 1],
					   latitude =  sf::st_coordinates(.)[, 2])
	
	ROMS_dat_hind_poly <- ROMS_dat_hind %>% 
		filter(., long_not_360 %in% int_pts_sum_ll$longitude_not_360) %>%
		filter(., latitude %in% int_pts_sum_ll$latitude)
	 
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
	

	# restrict dataset to only those months of spawning (January to June)
	sp_months <- c(1:6)
	
	ROMS_dat_hind_poly_depth$date <- as.Date(ROMS_dat_hind_poly_depth$DateTime) # date in Date format
	ROMS_dat_hind_poly_depth$month <- month(ROMS_dat_hind_poly_depth$date) # month of year
	ROMS_dat_hind_poly_depth$week <- week(ROMS_dat_hind_poly_depth$date) # week of year
	ROMS_dat_hind_poly_depth$year <- year(ROMS_dat_hind_poly_depth$date)
	
	ROMS_dat_hind_trim <- ROMS_dat_hind_poly_depth %>%
		filter(month %in% sp_months)

  # add name of month for plotting
	ROMS_dat_hind_trim$month_name <- NA
     
  ROMS_dat_hind_trim$month_name[ROMS_dat_hind_trim$month == 1] <- "January"
  ROMS_dat_hind_trim$month_name[ROMS_dat_hind_trim$month == 2] <- "February"
	ROMS_dat_hind_trim$month_name[ROMS_dat_hind_trim$month == 3] <- "March"
	ROMS_dat_hind_trim$month_name[ROMS_dat_hind_trim$month == 4] <- "April"
	ROMS_dat_hind_trim$month_name[ROMS_dat_hind_trim$month == 5] <- "May"
	ROMS_dat_hind_trim$month_name[ROMS_dat_hind_trim$month == 6] <- "June"
	
  
	# save
	fwrite(ROMS_dat_hind_trim, "./data/ROMS_dat_hind_trim.csv")
