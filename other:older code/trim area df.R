# work on removing points not within a certain polygon and > 250 m depth

	library(data.table)
	library(lubridate)
	library(ggplot2)
	library(dplyr)
	library(marmap)
	library(rgdal)
	library(mapdata)
	library(rgeos)
	library(sf)

	# add column of long not on 360 scale
	temp_df <- temp_df %>%
			rename(latitude = Lat,
						 longitude = Lon) %>%
			mutate(long_not_360 = case_when(
				longitude >= 180 ~ longitude - 360,
				longitude < 180 ~ longitude
			))
	
	# create smaller dataframe for intersection function
	temp_df_sum <- temp_df %>%
		group_by(latitude, longitude) %>%
		summarise(mean_temp = mean(temp))

	# add column of long not on 360 scale
	temp_df_sum <- temp_df_sum %>%
			mutate(long_not_360 = case_when(
				longitude >= 180 ~ longitude - 360,
				longitude < 180 ~ longitude
	))
	
	# convert to shapefile for intersection function
	temp_df_sum_sf <- st_as_sf(temp_df_sum,
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

	full_poly <-  st_union(polygon_bsregion15, poly_bsregions_no_15_sf)
	
	plot(full_poly$geometry)

	int_pts_sum <- st_intersection(temp_df_sum_sf, full_poly) 
	
	int_pts_sum_albers <- st_transform(int_pts_sum, crs = 3338)

	# plot 
	
	world_map_data <- ne_countries(scale = "medium", returnclass = "sf") 

 		combined_poly_plot <-
 		ggplot() +
		geom_sf(data = int_pts_sum_albers, aes(color = mean_hs))  +
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

	ggsave("./Pcod-Bering-Sea/output/plots/combined_poly_plot.png",
		combined_poly_plot,
		width = 10, height = 10, units = "in")
	
	## filter full dataframe by lats/longs in polygon
	
	# decompose geometry back into lat/long
	int_pts_sum_ll <- int_pts_sum %>%
			mutate(longitude_not_360 = sf::st_coordinates(.)[, 1],
					   latitude =  sf::st_coordinates(.)[, 2])
	
	temp_df_poly <- temp_df %>% 
		filter(., long_not_360 %in% int_pts_sum_ll$longitude_not_360) %>%
		filter(., latitude %in% int_pts_sum_ll$latitude)
	 
###############	 	
# restrict to depths < 250 m ####
###############
	
	max(temp_df_poly$long_not_360) 
	min(temp_df_poly$long_not_360) 
	max(temp_df_poly$latitude)
	min(temp_df_poly$latitude)
	
	BS_bathy <- getNOAA.bathy(lon1 = 179.95, lon2 = -158.01, lat1 = 53, 
														lat2 = 65.75, resolution = 1, antimeridian = TRUE,
														keep = TRUE)

	#plot(BS_bathy, image = TRUE, land = TRUE, axes = FALSE, lwd = 0.1,
	#		 bpal = list(c(0, max(BS_bathy), grey(0.7), grey(0.9), grey(0.95)),
	#		 						c(min(BS_bathy), 0, "darkblue", "lightblue")))
	#plot(BS_bathy, n = 1, lwd = 0.5, add = TRUE)
	
	longslats <- temp_df_poly %>% 
		dplyr::select(longitude, latitude) %>%
		distinct(across(everything())) %>%
		filter(., between(longitude, 179, 202))
	
	depths <- get.depth(BS_bathy, longslats, locator = FALSE)
	
	depths <- depths %>%
		rename(latitude = lat,
					 longitude = lon)
	
	temp_df_poly_depth <- merge(temp_df_poly, depths, by = c("longitude", "latitude")) %>%
		filter(., between(depth, -250, 0))
	
	# add name of month for plotting
	temp_df_poly_depth$month_name <- NA
	
	month_name_func <- function(x,y){
		
			temp_df_poly_depth <- temp_df_poly_depth %>%
				rowwise() %>%
				mutate(month_name = 
							 	case_when(month == x ~ y))
			
		  temp_df_poly_depth$month_name[temp_df_poly_depth$month == x] <- y

		  	}
	
	number_list <-1:6
	month_list <- c("January", "February", "March", "April", "May", "June")
	
	df_list <- mapply(month_name_func, x = number_list, y = month_list)
     
	df <- bind_cols(df_list)
	
  temp_df_poly_depth$month_name[temp_df_poly_depth$month == 1] <- "January"
  temp_df_poly_depth$month_name[temp_df_poly_depth$month == 2] <- "February"
	temp_df_poly_depth$month_name[temp_df_poly_depth$month == 3] <- "March"
	temp_df_poly_depth$month_name[temp_df_poly_depth$month == 4] <- "April"
	temp_df_poly_depth$month_name[temp_df_poly_depth$month == 5] <- "May"
	temp_df_poly_depth$month_name[temp_df_poly_depth$month == 6] <- "June"
	

	area_df <- area_df %>%
		filter(., latitude %in% unique(temp_df_poly_depth$latitude)) %>%
		filter(., longitude %in% unique(temp_df_poly_depth$longitude))
	
## match?
	ROMS_df <- merge(area_df, temp_df_poly_depth, by = c("latitude", "longitude"))
	
	ROMS_df <- ROMS_df %>% dplyr::select(-Eta.x, -Eta.y, -Xi.x, -Xi.y)
	
	ROMS_df$date <- as.Date(ROMS_df$DateTime) # date in Date format
	ROMS_df$month <- month(ROMS_df$date) # month of year
	ROMS_df$week <- week(ROMS_df$date) # week of year
	ROMS_df$year <- year(ROMS_df$date)
	
	# months to keep
	months_keep <- c(1:6)
	ROMS_df <- ROMS_df %>% filter(., month %in% months_keep)
	
  ROMS_df$month_name <- NA
	
	ROMS_df$month_name[ROMS_df$month == 1] <- "January"
  ROMS_df$month_name[ROMS_df$month == 2] <- "February"
	ROMS_df$month_name[ROMS_df$month == 3] <- "March"
	ROMS_df$month_name[ROMS_df$month == 4] <- "April"
	ROMS_df$month_name[ROMS_df$month == 5] <- "May"
	ROMS_df$month_name[ROMS_df$month == 6] <- "June"
	
	# reorder for plotting
	ROMS_df$month_name <- factor(ROMS_df$month_name)
	ROMS_df$month_name <- fct_reorder(ROMS_df$month_name, 
  																	ROMS_df$month)
  
	fwrite(ROMS_df, "./data/ROMS_df.csv")
