# use ROMS bathymetry, not NOAA, to define EBS polygon

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
		dplyr::filter(month %in% sp_months)

  # add name of month for plotting
	ROMS_dat_hind_trim$month_name <- NA
     
  ROMS_dat_hind_trim$month_name[ROMS_dat_hind_trim$month == 1] <- "January"
  ROMS_dat_hind_trim$month_name[ROMS_dat_hind_trim$month == 2] <- "February"
	ROMS_dat_hind_trim$month_name[ROMS_dat_hind_trim$month == 3] <- "March"
	ROMS_dat_hind_trim$month_name[ROMS_dat_hind_trim$month == 4] <- "April"
	ROMS_dat_hind_trim$month_name[ROMS_dat_hind_trim$month == 5] <- "May "
	ROMS_dat_hind_trim$month_name[ROMS_dat_hind_trim$month == 6] <- "June"

	## remove any area where depth > 250m ####
	ROMS_dat_hind_trim <- ROMS_dat_hind_trim %>%
			dplyr::filter(., between(depth, 0, 250))
	
	# convert longs
	ROMS_dat_hind_trim <- ROMS_dat_hind_trim %>% 
		mutate(long_not_360 = case_when(
					 longitude >= 180 ~ longitude - 360,
					 longitude < 180 ~ longitude))
	
	
	# summarize to see the domain now
	ROMS_dat_hind_trim_sum <- ROMS_dat_hind_trim %>%
		filter(latitude < 64) %>%
		group_by(latitude, long_not_360) %>%
		summarize(mean_temp = mean(temp)) %>%
  	st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326, remove = FALSE)
	
		ggplot() +
		 	geom_sf(data = ROMS_dat_hind_trim_sum, aes(color = mean_temp))  +
			geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
			coord_sf(crs = 3338) +
		  scale_color_viridis_c() +
			scale_x_continuous(
 				breaks = breaks_x,
 				labels = labels_x,
 				name = "Longitude",
 				limits = limits_x) +
 			scale_y_continuous(
 				breaks = breaks_y,
 				limits = limits_y,
 				name = "Latitude") +
		white_theme()
    	  
	
	
	# try to trim more
		
	ROMS_dat_hind_trim_sum2 <- ROMS_dat_hind_trim %>%
		filter(between(latitude, 53, 64)) 
	
	ROMS_dat_hind_trim_sum3 <-
		filter(ROMS_dat_hind_trim_sum2, latitude > 55 & long_not_360 < -165) %>%
		group_by(latitude, long_not_360) %>%
		summarize(mean_temp = mean(temp)) %>%
  	st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326, remove = FALSE)
	
		ggplot() +
			geom_sf(data = ROMS_dat_hind_trim_sum3, aes(color = mean_temp))  +
			geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
			coord_sf(crs = 3338) +
		  scale_color_viridis_c() +
			scale_x_continuous(
 				breaks = breaks_x,
 				labels = labels_x,
 				name = "Longitude",
 				limits = limits_x) +
 			scale_y_continuous(
 				breaks = breaks_y,
 				limits = limits_y,
 				name = "Latitude") +
		white_theme()
	
	
	
		
		
		
		
		
		
		
		
		
		
		
	
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

	
	## create "new Ortiz regions"
	
	
	
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
