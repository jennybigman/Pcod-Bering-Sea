
  polygon_bsregions <- 
  	bsregions %>%
	  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
	  summarise(geometry = st_combine(geometry)) %>%
	  st_cast("POLYGON") %>%
  	st_combine()
  
	sm_temp_df_sum_sf <- createSF_points(sm_temp_df_sum)

	sm_temp_df_sum_sf_4326 <- st_transform(sm_temp_df_sum_sf, crs = "+proj=longlat +datum=WGS84 +no_defs")
	
	int_points <- st_intersection(sm_temp_df_sum_sf_4326, polygon_bsregions)

	# plot
		ggplot() +
		geom_polygon(data = reg, aes(x = long, y = lat, group = group),
 								 fill = "darkgrey", color = NA) +
		geom_point(data = int_points, aes(x = longitude, y = latitude, color = mean_hs)) +
		coord_sf(xlim = lons, ylim = lats, crs = 3338) 
	
	
	#### another try
	
	  polygon_bsregions <- 
  	bsregions %>%
	  st_as_sf(coords = c("long360", "lat"), crs = 4326) %>%
	  summarise(geometry = st_combine(geometry)) %>%
	  st_cast("POLYGON") %>%
  	st_combine()
	  
	polygon_bsregions_albers <- st_transform(polygon_bsregions, crs = 3338) 

  sm_temp_df_sum_sf_3338 <- st_transform(sm_temp_df_sum_sf, crs = 3338)
	
	int_points_albers <- st_intersection(sm_temp_df_sum_sf_3338, st_make_valid(polygon_bsregions_albers))

	#int_points_albers <- st_intersection(sm_temp_df_sum_sf_3338, polygon_bsregions_albers)

	#int_points_albers <- st_intersection(st_geometry(sm_temp_df_sum_sf_3338), 
																			 st_make_valid(st_geometry(polygon_bsregions_albers)))

  # above does not result in correct format 
  
	# plot
		ggplot() +
		geom_polygon(data = reg, aes(x = long, y = lat, group = group),
 								 fill = "darkgrey", color = NA) +
		geom_point(data = int_points_albers, aes(x = longitude, y = latitude, color = mean_hs)) +
		coord_sf(xlim = lons, ylim = lats, crs = 3338) 
	
 #### works ####
		
bsregions_sf <- st_as_sf(bsregions)
class(bsregions_sf)
(bsregions_geom <- st_geometry(bsregions_sf))

plot(st_geometry(bsregions_sf), col = "grey", border = "white")

bs_poly <- st_union(st_geometry(bsregions_sf), by_feature = FALSE)
plot(bs_poly)

# below doesn't work 
polygon_bs <- bsregions.df_add %>%
	st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
	summarise(geometry = st_combine(geometry)) %>%
	st_cast("POLYGON") %>%
	st_union()
	
plot(polygon_bs)

polygon_bs_v <- st_make_valid(polygon_bs)

plot(polygon_bs_v)


####

older code

# define polygon to save points over 
	
	bsregions <- readOGR("./Mapping Code - Bigman/Ortiz Regions", layer="BSIERP_regions_2012")
	bsregions <- spTransform(bsregions, CRS("+init=epsg:4326"))
	bsregions@data$id = rownames(bsregions@data)
	bsregions.points <- fortify(bsregions, region="id")
	bsregions.df <- left_join(bsregions.points, bsregions@data, by = "id")
	bsregions.df2 <- bsregions.df %>% filter(., DOMAIN != 15)
	
	bsregions.df3 <- bsregions.df2 %>%
		mutate(long360 = case_when(
			long >= -180 ~ long + 360,
			long < -180 ~ long
		))
	
  polygon_bsregions <- 
  	bsregions.df3 %>%
	  st_as_sf(coords = c("long360", "lat"), crs = 4326) %>%
	  summarise(geometry = st_combine(geometry)) %>%
	  st_cast("POLYGON") %>%
  	st_combine()

	int_points <- st_intersection(sm_temp_df_sum_sf, st_make_valid(polygon_bsregions))

	int_points_albers <- st_transform(int_points, crs = 3338)
 
	
	# practice plotting
	lons = c(180, 202)
	lats = c(53, 67)
	
	reg = map_data("world2Hires")
	reg <- reg %>% filter(region == "USSR" | subregion == "Alaska")
 
	
	ggplot() +
 		geom_sf(data = int_points, aes(color = mean_hs)) +
	  coord_sf(xlim = lons, ylim = lats) +
		geom_polygon(data = reg, aes(x = long, y = lat, group = group),
 								 fill = "darkgrey", color = NA)

	
		
#####	code doesn't work
	
		ggplot() +
		geom_polygon(data = reg, aes(x = long, y = lat, group = group),
 								 fill = "darkgrey", color = NA) +
		geom_point(data = int_points, aes(x = longitude, y = latitude, color = mean_hs)) +
		coord_map(xlim = lons, ylim = lats) 


	ggplot() +
		geom_polygon(data = reg, aes(x = long, y = lat, group = group),
 								 fill = "darkgrey", color = NA) +
		geom_point(data = int_points, aes(x = longitude, y = latitude, color = mean_hs)) +
		coord_sf(xlim = lons, ylim = lats, crs = 3338) 
	ggplot_build(pplot)$layout$panel_scales_x[[1]]$range$range

	reg$longitude <- reg$long
	reg$latitude <- reg$lat
	
	reg2 <- reg %>% 
		filter(., between(long, 178, 205)) %>%
		filter(., between(lat, 53, 69))
  
	reg_sf <- st_as_sf(reg2, coords = c("longitude", "latitude"), crs = 3338)
	
	ggplot() +
		geom_sf(data = reg_sf, fill = "darkgrey")
 
	### 
	
		# decompose geom back into lat/long #### doesnt work
	bsregions_sf <- bsregions_sf %>%
			mutate(longitude_not_360 = sf::st_coordinates(.)[, 1],
					   latitude =  sf::st_coordinates(.)[, 2])

	# doesn't work
	
	bsregions_sf_pts <- bsregions_sf %>% st_cast("POINT") %>%
		mutate(longitude_not_360 = sf::st_coordinates(.)[, 1],
					 latitude =  sf::st_coordinates(.)[, 2])

	# fix long of problematic domain # 15 
	bsregions_sf_pts_rm <- bsregions_sf_pts %>% filter(., DOMAIN != 15)
	bsregions_sf_pts_15 <- bsregions_sf_pts %>% filter(., DOMAIN == 15)
	bsregions_sf_pts_15_2 <- bsregions_sf_pts_15 %>% filter(., longitude_not_360 < 0)

	# add domain # 15 back into BS regions and check to make sure all polygons are there
	bsregions_sf_add <- bind_rows(bsregions_sf_pts_rm, bsregions_sf_pts_15_2)
	
	bsregions_sf_add_poly <- bsregions_sf_add %>% 
			  summarise(geometry = st_combine(geometry)) %>%
				st_cast("POLYGON") %>%
				st_union()

	plot(bsregions_sf_add_poly)
	
  int_pts_sum <- st_intersection(sm_temp_df_sum_sf, bsregions_sf_add_poly) 

	
 ## try again 
	
	bsregions_sf_15 <- bsregions_sf %>% filter(., DOMAIN != 15) %>% st_union
	
	plot(bsregions_sf_15)
	
	int_pts_sum <- st_intersection(sm_temp_df_sum_sf, bsregions_sf_15) 

		ggplot() +
 		geom_sf(data = int_pts_sum, aes(color = mean_hs)) +
	  coord_sf(xlim = lons, ylim = lats) +
		geom_polygon(data = reg, aes(x = long, y = lat, group = group),
 								 fill = "darkgrey", color = NA) +
		scale_color_viridis_c() +
		theme_bw()
	
	
	
	mutate(longitude_not_360 = sf::st_coordinates(.)[, 1],
					   latitude =  sf::st_coordinates(.)[, 2])

	
	### try again
bsregions_sf2 <- bsregions_sf %>%
	mutate(long = unlist(map(bsregions_sf$geometry, 1,)),
				 lat = unlist(map(bsregions_sf$geometry, 2)))

geom <- bsregions_sf$geometry

bsregions_sf2 <- bsregions_sf %>%
	mutate(long = lapply(geom, function(x) x[-2]))

long = unlist(lapply(geom, function(x) x[-2]))

z <- geom[[15]] [-2]
	
z <- bsregions_sf$geometry[[1,]
	
bsregions_sf$geometry[which()]	
	## 
	
	# fix long of problematic domain # 15 
	bsregions.df_rm <- bsregions.df %>% filter(., DOMAIN != 15)

	bsregions.df_15 <- bsregions.df %>% filter(., DOMAIN == 15)
	bsregions.df_15_2 <- bsregions.df_15 %>% filter(., long < 0)

	# add domain # 15 back into BS regions and check to make sure all polygons are there
	bsregions.df_add <- bind_rows(bsregions.df_rm, bsregions.df_15_2)
	
	bs_regions_fxd_sf <- st_as_sf(bsregions.df_add, coords = c("long", "lat"), crs = 4326)

	bsregions_sp <- as(bs_regions_fxd_sf, "Spatial")

	plot(bsregions_sp)	
	
	bsregions15 <- bsregions %>% filter(., DOMIAN == 15)
	
	bsregions2 <- bsregions[bsregions@data
	
### 
													
bsregions <- spTransform(bsregions, CRS("+init=epsg:4326"))
	
bsregions15 <-bsregions[bsregions@data$DOMAIN == 15, ] 

plot(bsregions15)

bsregions15@data$id = rownames(bsregions15@data)
bsregions_points15 <- fortify(bsregions15, region="id")
bsregions15_df <- left_join(bsregions_points15, bsregions15@data, by = "id") %>%
	filter(., long < 0)

bsregions15_poly <- st_cast(bsregions15_df_sf, "POLYGON")

ggplot() +
		geom_polygon(data = bsregions15_df, aes(x = long, y = lat, group = group),
 								 fill = "darkgrey", color = NA) +

 polygon_bsregion15 <- 
  	bsregions15_df %>%
	  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
	  summarise(geometry = st_combine(geometry)) %>%
	  st_cast("POLYGON") 

plot(polygon_bsregion15)


full_poly <-  st_union(polygon_bsregion15, bsregions_sf_15)

plot(full_poly)

int_pts_sum2 <- st_intersection(sm_temp_df_sum_sf, full_poly) 

		ggplot() +
 		geom_sf(data = int_pts_sum2, aes(color = mean_hs)) +
	  coord_sf(xlim = lons, ylim = lats) +
		geom_polygon(data = reg, aes(x = long, y = lat, group = group),
 								 fill = "darkgrey", color = NA) +
		scale_color_viridis_c() +
		theme_bw()
