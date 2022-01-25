 # trim temp_df

	# by months of known spawning: Jan - April ####
	temp_df$date <- as.Date(temp_df$DateTime) # date in Date format
	temp_df$month <- month(temp_df$date) # month of year
	temp_df$week <- week(temp_df$date) # week of year
	temp_df$year <- year(temp_df$date)
	
	sp_months <- c(1:4)

	temp_df_spmo <- temp_df %>%
		filter(month %in% sp_months)

  # add name of month for plotting
	temp_df_spmo$month_name <- NA
     
  temp_df_spmo$month_name[temp_df_spmo$month == 1] <- "January"
  temp_df_spmo$month_name[temp_df_spmo$month == 2] <- "February"
	temp_df_spmo$month_name[temp_df_spmo$month == 3] <- "March"
	temp_df_spmo$month_name[temp_df_spmo$month == 4] <- "April"

	# by region: remove any grid cells not within Ortiz regions ####
	
	# add column of long not on 360 scale
	temp_df_spmo <- temp_df_spmo %>%
			mutate(long_not_360 = case_when(
				longitude >= 180 ~ longitude - 360,
				longitude < 180 ~ longitude
			))
	
	# create smaller dataframe for intersection function
	temp_df_spmo_sum <- temp_df_spmo %>%
		group_by(latitude, longitude) %>%
		summarise(mean_temp = mean(temp))
	
	# add column of long not on 360 scale
	temp_df_spmo_sum <- temp_df_spmo_sum %>%
			mutate(long_not_360 = case_when(
				longitude >= 180 ~ longitude - 360,
				longitude < 180 ~ longitude
	))
	
	# convert to shapefile for intersection function
	temp_df_spmo_sum_sf <- temp_df_spmo_sum %>%
		st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326, remove = FALSE)

	###############	 	
	# remove points not on Bering Sea Shelf ####
	###############
	
	# load Ortiz Bering Sea regions and convert to sf object
	bsregions <- readOGR("./other:older code/Mapping Code - Bigman/Ortiz Regions", layer="BSIERP_regions_2012")
	bsregions <- spTransform(bsregions, CRS("+init=epsg:4326"))
	bsregions_sf <- st_as_sf(bsregions)
	bsregions_no_15 <-bsregions[bsregions@data$DOMAIN != 15, ] # remove domain 15 

	plot(bsregions_no_15)
	
	bsregions_no_15_sf <- bsregions_no_15 %>% 	  
		st_as_sf(coords = c("long", "lat"), crs = 4326) 
	poly_bsregions_no_15_sf <- st_union(bsregions_no_15_sf)
	
	plot(poly_bsregions_no_15_sf)

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
		group_by(DOMAIN) %>%
	  summarise(geometry = st_combine(geometry)) %>%
	  st_cast("POLYGON") 

	plot(polygon_bsregion15)

	# add back into polygon
	full_poly <-  st_union(polygon_bsregion15, poly_bsregions_no_15_sf)
	
	plot(full_poly$geometry)

	# find intersection of points in df and polygon
	int_pts_sum <- st_intersection(temp_df_spmo_sum_sf, full_poly) 
	
	int_pts_sum_albers <- st_transform(int_pts_sum, crs = 3338) # in albers for plotting
	
	# plot
	world_map_data <- ne_countries(scale = "medium", returnclass = "sf") 

 #	combined_poly_plot <-
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

#	ggsave(here("./output/plots/combined_poly_plot_ROMSdepth.png"),
#		combined_poly_plot,
#		width = 10, height = 10, units = "in")
	
	
 	## filter full dataframe by lats/longs in polygon
	
	temp_df_trim <- temp_df_spmo %>% 
		filter(., long_not_360 %in% int_pts_sum$long_not_360) %>%
		filter(., latitude %in% int_pts_sum$latitude)
	
	temp_df_trim <- temp_df_trim %>% 
		dplyr::select(-Xi, -Eta)
	
	# test to see how many grid cells
	temp_df_trim_sum <- temp_df_trim %>%
		group_by(latitude, longitude) %>%
		summarise(mean_temp = mean(temp))
	
	temp_df_trim_sum <- temp_df_trim_sum %>%
		mutate(long_not_360 = case_when(
				longitude >= 180 ~ longitude - 360,
				longitude < 180 ~ longitude
	))
	 
	# save
	fwrite(temp_df_trim, "./data/temp_df_trim.csv")

	# by depth ####
	depth_df <- fread("./data/ROMS_depth_df.csv") %>% na.omit()

	depth_df <- depth_df %>%
		dplyr::select(-Xi, -Eta)
	
	depth_df <- depth_df %>%
				mutate(long_not_360 = case_when(
				longitude >= 180 ~ longitude - 360,
				longitude < 180 ~ longitude
	))
		
		depth_df <- depth_df %>%
			filter(., longitude %in% int_pts_sum$longitude) %>%
			filter(., latitude %in% int_pts_sum$latitude)
	
	
	temp_df_trim_depth <- merge(temp_df_trim, depth_df, 
															by = c("latitude", "long_not_360", "longitude"))
	
	
	# these aren't working
	temp_df_trim_depth <- temp_df_trim_depth %>%
		filter(., between(depth, 0, 250))
	
	# sum for plot
	
	temp_df_trim_depth_sum <- temp_df_trim_depth %>%
		group_by(latitude, long_not_360) %>%
		summarise(mean_temp = mean(temp))
	
	temp_df_trim_depth_sum_sf <- temp_df_trim_depth_sum %>%
	  	st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326, remove = FALSE)

	
	# map to check 
	 test_plot <- 
    	  	ggplot() +
					geom_sf(data = temp_df_trim_depth_sum_sf, aes(color = mean_temp))  +
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
    	  	labs(colour = "Temperature (˚C)") +
					theme_bw() +
 					theme(
 						strip.text = element_text(size = 14, face = "bold"),
 						strip.background = element_blank(),
 						axis.text = element_text(size = 12),	
  					axis.title = element_text(size = 14),
  					legend.title.align=0.5)
    	  
	 # add in area ####
	area_df <- fread( "./data/ROMS_area_grid_cells.csv") %>% na.omit()

	area_df <- area_df %>% 
	 	dplyr::select(-Xi, -Eta) %>%
	 	mutate(long_not_360 = case_when(
					 longitude >= 180 ~ longitude - 360,
					 longitude < 180 ~ longitude)) 
	 
	 area_df_trim <- area_df %>%
		filter(., long_not_360 %in% int_pts_sum$long_not_360) %>%
		filter(., latitude %in% int_pts_sum$latitude)

	 temp_hind_dat <- merge(temp_df_trim_depth, area_df_trim,
	 											 by = c("latitude", "longitude","long_not_360"))
	 
	 # test map
	 temp_hind_dat_sum <- temp_hind_dat %>%
	 	group_by(latitude, longitude, long_not_360, area_km2) %>%
	 	summarize(mean_temp = mean(temp))
	 
	 	temp_hind_dat_sum_sf  <- temp_hind_dat_sum %>%
	  	st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326, remove = FALSE)

	
	# map to check 
	 test_plot <- 
    	  	ggplot() +
					geom_sf(data = temp_hind_dat_sum_sf, aes(color = area_km2))  +
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
    	  	labs(colour = "area km2") +
					theme_bw() +
 					theme(
 						strip.text = element_text(size = 14, face = "bold"),
 						strip.background = element_blank(),
 						axis.text = element_text(size = 12),	
  					axis.title = element_text(size = 14),
  					legend.title.align=0.5)
	 
	   	  	ggplot() +
					geom_sf(data = temp_hind_dat_sum_sf, aes(color = mean_temp))  +
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
    	  	labs(colour = "Temp") +
					theme_bw() +
 					theme(
 						strip.text = element_text(size = 14, face = "bold"),
 						strip.background = element_blank(),
 						axis.text = element_text(size = 12),	
  					axis.title = element_text(size = 14),
  					legend.title.align=0.5)
	 
    	  
	# hatch success ####
	 
	hatch_success_cauchy_func <- function(x, k = 0.453, mu = 4.192, sigma = 2.125 ){
  			 		(k / (1 + (((x - mu)/sigma))^2)) 
	}

	temp_hind_dat <- temp_hind_dat %>%
		mutate(hatch_success_cauchy = sapply(temp, hatch_success_cauchy_func))
	
	# standardize hatch success (calculating spawning habitat suitability)
  temp_hind_dat <- temp_hind_dat %>%
  	mutate(sp_hab_suit = hatch_success_cauchy/max(hatch_success_cauchy))
  
  glimpse(temp_hind_dat)
  
  