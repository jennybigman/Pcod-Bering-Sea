# trim area_df 

# add column of long not on 360 scale
	area_df <- area_df %>%
			mutate(long_not_360 = case_when(
				longitude >= 180 ~ longitude - 360,
				longitude < 180 ~ longitude
			))
	
	# create smaller dataframe for intersection function
	area_df_sum <- area_df %>%
		group_by(latitude, longitude) %>%
		summarise(mean_area = mean(area_km2))

	# add column of long not on 360 scale
	area_df_sum <- area_df_sum %>%
			mutate(long_not_360 = case_when(
				longitude >= 180 ~ longitude - 360,
				longitude < 180 ~ longitude
	))
	
	area_df_sum$latitude2 <- area_df_sum$latitude
	area_df_sum$longitude2 <- area_df_sum$longitude
	
	# convert to shapefile for intersection function
	area_sum_sf <- st_as_sf(area_df_sum,
		coords = c("long_not_360", "latitude"), crs = 4326)

	### Ortiz regions
	
	
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

	
	#### intersection
	
	# find intersection of points in df and polygon
	int_area_sum <- st_intersection(area_sum_sf, full_poly) 
	

	lats_longs_unique <- int_area_sum %>%
		dplyr::select(latitude2, longitude2) %>%
		rename(latitude = latitude2,
					 longitude = longitude2) %>%
		mutate(long_not_360 = case_when(
			longitude >= 180 ~ longitude - 360,
			longitude < 180 ~ longitude))
		
	fwrite(lats_longs_unique, "./data/unique_coords.csv")
	
