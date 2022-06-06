# 00c original

	# merge area and bottom temp dfs and trim by shelf region and depth

	library(here)
	library(data.table)

	#### merge dfs ####
	
	# load dfs
	temp_df <- fread("./data/ROMS_all_temp.csv")
	area_df <- fread( "./data/ROMS_area_grid_cells.csv")
	depth_df <- fread("./data/ROMS_depth_df.csv")

	# merge dfs
	area_depth_df <- merge(area_df, depth_df, by = c("latitude", "longitude", "Xi", "Eta"))
	
	ROMS_dat_hind <- merge(temp_df, area_depth_df, by = c("latitude", "longitude"),
												 all = TRUE)
	
	ROMS_dat_hind <- na.omit(ROMS_dat_hind)
	
	# restrict dataset to only those months of spawning (January to June)
	sp_months <- c(1:4)
	
	ROMS_dat_hind$date <- as.Date(ROMS_dat_hind$DateTime) # date in Date format
	ROMS_dat_hind$month <- month(ROMS_dat_hind$date) # month of year
	ROMS_dat_hind$week <- week(ROMS_dat_hind$date) # week of year
	ROMS_dat_hind$year <- year(ROMS_dat_hind$date)
	
	ROMS_dat_hind_trim <- ROMS_dat_hind %>%
		filter(month %in% sp_months)

  # add name of month for plotting
	ROMS_dat_hind_trim$month_name <- NA
     
  ROMS_dat_hind_trim$month_name[ROMS_dat_hind_trim$month == 1] <- "January"
  ROMS_dat_hind_trim$month_name[ROMS_dat_hind_trim$month == 2] <- "February"
	ROMS_dat_hind_trim$month_name[ROMS_dat_hind_trim$month == 3] <- "March"
	ROMS_dat_hind_trim$month_name[ROMS_dat_hind_trim$month == 4] <- "April"

		## remove any area where depth > 250m ####
	
	ROMS_dat_hind_trim <- ROMS_dat_hind_trim %>%
			filter(., between(depth, 0, 250))
	
	#### trim df by shelf ####
	
	# add column of long not on 360 scale
	ROMS_dat_hind_trim <- ROMS_dat_hind_trim %>%
			mutate(long_not_360 = case_when(
				longitude >= 180 ~ longitude - 360,
				longitude < 180 ~ longitude
			))
	
	# create smaller dataframe for intersection function
	ROMS_dat_hind_trim_sum <- ROMS_dat_hind_trim %>%
		group_by(latitude, longitude) %>%
		summarise(mean_temp = mean(temp))
	
	# add column of long not on 360 scale
	ROMS_dat_hind_trim_sum <- ROMS_dat_hind_trim_sum %>%
			mutate(long_not_360 = case_when(
				longitude >= 180 ~ longitude - 360,
				longitude < 180 ~ longitude
	))
	
	ROMS_dat_hind_trim_sum$lats <- ROMS_dat_hind_trim_sum$latitude
	ROMS_dat_hind_trim_sum$longs_not_360 <- ROMS_dat_hind_trim_sum$long_not_360
	
	# convert to shapefile for intersection function
	ROMS_dat_hind_trim_sum_sf <- st_as_sf(ROMS_dat_hind_trim_sum,
		coords = c("long_not_360", "latitude"), crs = 4326)

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
	int_pts_sum <- st_intersection(ROMS_dat_hind_trim_sum_sf, full_poly) 
	
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

#	ggsave(here("./output/plots/combined_poly_plot_ROMSdepth.png"),
#		combined_poly_plot,
#		width = 10, height = 10, units = "in")
	
	
 	## filter full dataframe by lats/longs in polygon
	
	ROMS_hindcast_temp_dat <- ROMS_dat_hind_trim %>% 
		filter(., long_not_360 %in% int_pts_sum$longs_not_360) %>%
		filter(., latitude %in% int_pts_sum$lats)
	 
	# save
	fwrite(ROMS_hindcast_temp_dat, "./data/ROMS_hindcast_temp_dat.csv")
