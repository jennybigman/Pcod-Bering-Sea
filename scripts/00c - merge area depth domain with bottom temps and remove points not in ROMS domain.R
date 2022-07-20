#00d - alternative way of trimming to ROMS grid by ROMS domains

	# merge area and bottom temp dfs and trim by shelf region and depth

	library(here)
	library(data.table)

	#### merge dfs ####
	
	# load dfs
	temp_df <- fread("./data/ROMS_all_temp.csv")
	area_df <- fread( "./data/ROMS_area_grid_cells.csv")
	depth_df <- fread("./data/ROMS_depth_df.csv")
	domain_df <- fread("./data/ROMS_domain_df.csv")
	
	# merge
	area_depth_df <- merge(area_df, depth_df, by = c("latitude", "longitude", "Xi", "Eta"))

	area_depth_domain_df <- merge(area_depth_df, domain_df,
													by = c("latitude", "longitude", "Xi", "Eta"))

	ROMS_dat_hind <- merge(temp_df, area_depth_domain_df, 
												 by = c("latitude", "longitude", "Xi", "Eta"),
												 all = TRUE)
	
	ROMS_dat_hind <- na.omit(ROMS_dat_hind)
	
	# trim grid to only survey replicated strata/domains
	
	ROMS_dat_hind_trim <- ROMS_dat_hind %>%
		filter(domain > 0) %>%
		filter(., between(depth, 0, 250))
	
	# restrict dataset to only those months of spawning (January to June)
	sp_months <- c(1:4)
	
	ROMS_dat_hind_trim$date <- as.Date(ROMS_dat_hind_trim$DateTime) # date in Date format
	ROMS_dat_hind_trim$month <- month(ROMS_dat_hind_trim$date) # month of year
	ROMS_dat_hind_trim$week <- week(ROMS_dat_hind_trim$date) # week of year
	ROMS_dat_hind_trim$year <- year(ROMS_dat_hind_trim$date)
	
	ROMS_hindcast_temp_dat <- ROMS_dat_hind_trim %>%
		filter(month %in% sp_months)

  # add name of month for plotting
	ROMS_hindcast_temp_dat$month_name <- NA
     
  ROMS_hindcast_temp_dat$month_name[ROMS_hindcast_temp_dat$month == 1] <- "January"
  ROMS_hindcast_temp_dat$month_name[ROMS_hindcast_temp_dat$month == 2] <- "February"
	ROMS_hindcast_temp_dat$month_name[ROMS_hindcast_temp_dat$month == 3] <- "March"
	ROMS_hindcast_temp_dat$month_name[ROMS_hindcast_temp_dat$month == 4] <- "April"

	
	### plot to see if this works ####
	ROMS_dat_hind_trim_sum <- ROMS_hindcast_temp_dat %>%
		group_by(latitude, longitude) %>%
		summarize(mean_temp = mean(temp))
	
	ROMS_dat_hind_trim_sum_sf <- ROMS_dat_hind_trim_sum	%>% 
		mutate(long_not_360 = case_when(
					 longitude >= 180 ~ longitude - 360,
					 longitude < 180 ~ longitude))  %>%
  	st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326, remove = FALSE)
	
	ROMS_strata_temp_plot <-
 		ggplot() +
		geom_sf(data = ROMS_dat_hind_trim_sum_sf, aes(color = mean_temp))  +
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
		theme_bw() 
 	
		 
	# save
	fwrite(ROMS_hindcast_temp_dat, "./data/ROMS_hindcast_temp_dat.csv")

	
	
	
	
	
	
	
