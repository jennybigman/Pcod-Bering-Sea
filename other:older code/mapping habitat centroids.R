### mapping centroids


	# yearly centroids where habitat suitability >= 0.5 ####
		
	yr_centroid05_df$lat <- yr_centroid05_df$lat_centroid
	yr_centroid05_df$long <- yr_centroid05_df$long_centroid
		
	yr_centroid05_df <- yr_centroid05_df %>%
		mutate(long_not_360 = case_when(
				long >= 180 ~ long - 360,
				long < 180 ~ long
		))
	
	yr_centroid05_df_sf <- st_as_sf(yr_centroid05_df,
		coords = c("long_not_360", "lat"), crs = 4326)
		
	map_yrcentroids_05 <- 
		 	ggplot(yr_centroid05_df_sf) +
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
 				name = "Latitude",
 			) +
  		geom_point(aes(geometry = geometry),
    		stat = "sf_coordinates")
	
	ggsave("./output/plots/map_yrcentroids_05.png",
			 map_yrcentroids_05,
			 width = 10, height = 7, units = "in")
	
	
	# yearly centroids where habitat suitability >= 0.9 ####
	
	yr_centroid09_df$lat <- yr_centroid09_df$lat_centroid
	yr_centroid09_df$long <- yr_centroid09_df$long_centroid
		
	yr_centroid09_df <- yr_centroid09_df %>%
		mutate(long_not_360 = case_when(
				long >= 180 ~ long - 360,
				long < 180 ~ long
		))
	
	yr_centroid09_df_sf <- st_as_sf(yr_centroid09_df,
		coords = c("long_not_360", "lat"), crs = 4326)
		
	map_yrcentroids_09 <- 
		 	ggplot(yr_centroid09_df_sf) +
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
 				name = "Latitude",
 			) +
  		geom_point(aes(geometry = geometry),
    		stat = "sf_coordinates")
	
	ggsave("./output/plots/map_yrcentroids_09.png",
			 map_yrcentroids_09,
			 width = 10, height = 7, units = "in")
	
	# yearly centroids of all habitat suitability ####
	
	yr_centroids_all_df$lat <- yr_centroids_all_df$lat_centroid
	yr_centroids_all_df$long <- yr_centroids_all_df$long_centroid
		
	yr_centroids_all_df <- yr_centroids_all_df %>%
		mutate(long_not_360 = case_when(
				long >= 180 ~ long - 360,
				long < 180 ~ long
		))
	
	yr_centroids_all_df_sf <- st_as_sf(yr_centroids_all_df,
		coords = c("long_not_360", "lat"), crs = 4326)
		
	map_yrcentroids_all <- 
		 	ggplot(yr_centroids_all_df_sf) +
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
 				name = "Latitude",
 			) +
  		geom_point(aes(geometry = geometry),
    		stat = "sf_coordinates")
	
	ggsave("./output/plots/map_yrcentroids_all.png",
			 map_yrcentroids_all,
			 width = 10, height = 7, units = "in")


	
	#### for data Jan to June
	
	# yearly centroids where habitat suitability >= 0.5 ####
		
	yr_centroid05_df_JM$lat <- yr_centroid05_df_JM$lat_centroid
	yr_centroid05_df_JM$long <- yr_centroid05_df_JM$long_centroid
		
	yr_centroid05_df_JM <- yr_centroid05_df_JM %>%
		mutate(long_not_360 = case_when(
				long >= 180 ~ long - 360,
				long < 180 ~ long
		))
	
	yr_centroid05_df_sf_JM <- st_as_sf(yr_centroid05_df_JM,
		coords = c("long_not_360", "lat"), crs = 4326)
		
	map_yrcentroids_05_JM <- 
		 	ggplot(yr_centroid05_df_sf_JM) +
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
 				name = "Latitude",
 			) +
  		geom_point(aes(geometry = geometry),
    		stat = "sf_coordinates")
	
	ggsave("./output/plots/map_yrcentroids_05_JM.png",
			 map_yrcentroids_05_JM,
			 width = 10, height = 7, units = "in")
	
	
	# yearly centroids where habitat suitability >= 0.9 ####
	
	yr_centroid09_df_JM$lat <- yr_centroid09_df_JM$lat_centroid
	yr_centroid09_df_JM$long <- yr_centroid09_df_JM$long_centroid
		
	yr_centroid09_df_JM <- yr_centroid09_df_JM %>%
		mutate(long_not_360 = case_when(
				long >= 180 ~ long - 360,
				long < 180 ~ long
		))
	
	yr_centroid09_df_sf_JM <- st_as_sf(yr_centroid09_df_JM,
		coords = c("long_not_360", "lat"), crs = 4326)
		
	map_yrcentroids_09_JM <- 
		 	ggplot(yr_centroid09_df_sf_JM) +
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
 				name = "Latitude",
 			) +
  		geom_point(aes(geometry = geometry),
    		stat = "sf_coordinates")
	
	ggsave("./output/plots/map_yrcentroids_09_JM.png",
			 map_yrcentroids_09_JM,
			 width = 10, height = 7, units = "in")
	
	# yearly centroids of all habitat suitability ####
	
	yr_centroids_all_df_JM$lat <- yr_centroids_all_df_JM$lat_centroid
	yr_centroids_all_df_JM$long <- yr_centroids_all_df_JM$long_centroid
		
	yr_centroids_all_df_JM <- yr_centroids_all_df_JM %>%
		mutate(long_not_360 = case_when(
				long >= 180 ~ long - 360,
				long < 180 ~ long
		))
	
	yr_centroids_all_df_sf_JM <- st_as_sf(yr_centroids_all_df_JM,
		coords = c("long_not_360", "lat"), crs = 4326)
		
	map_yrcentroids_all_JM <- 
		 	ggplot(yr_centroids_all_df_sf_JM) +
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
 				name = "Latitude",
 			) +
  		geom_point(aes(geometry = geometry),
    		stat = "sf_coordinates")
	
	ggsave("./output/plots/map_yrcentroids_all_JM.png",
			 map_yrcentroids_all_JM,
			 width = 10, height = 7, units = "in")

