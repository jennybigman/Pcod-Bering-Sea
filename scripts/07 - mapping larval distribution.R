# 06 - larval distribution


	# map densities of larval distribution from EcoFOCI ichthyo/zooplankton survey

	# read in data
	length_dat <- read_csv("./data/PcodLengthData_BS_60Bon_Feb2022.csv")
	catch0_dat <- read_csv("./data/PcodLarvalCatchwZeros_BS_60Bon_Feb2022.csv")
	
	catch0_dat$GMT_DATE_TIME <- as.POSIXct(catch0_dat$GMT_DATE_TIME)
	
	catch0_dat_trim <- catch0_dat %>%
		filter(LARVALCATCHPER10M2 == 0) %>%
		distinct(across(c(LAT, LON)), .keep_all = TRUE)
	
	years_length <- sort(unique(length_dat$YEAR))
	sort(unique(catch0_dat$YEAR))
	
	catch0_dat_trim <- catch0_dat_trim %>%
		filter(YEAR %in% years_length)
	
	months <- 1:6 
	
	catch0_dat_trim <- catch0_dat_trim %>%
		filter(MONTH %in% months)
	
	length_dat$date <- as.Date(length_dat$GMT_DATE_TIME) # date in Date format
	length_dat$month <- month(length_dat$date) # month of year
	length_dat$week <- week(length_dat$date) # week of year
	length_dat$year <- year(length_dat$date)

	length_dat <- length_dat %>%
		filter(month %in% months)
	
	# convert longs and rename cols
	length_dat_sf <- length_dat %>%
		dplyr::filter(., CORRECTED_LENGTH <= 6) %>%
		mutate(LON_not_360 = case_when(
				LON >= 180 ~ LON - 360,
				LON < 180 ~ LON)) %>%
		st_as_sf(coords = c("LON_not_360", "LAT"), crs = 4326, remove = FALSE)

	catch0_dat_trim_sf <- catch0_dat_trim %>%
		mutate(LON_not_360 = case_when(
				LON >= 180 ~ LON - 360,
				LON < 180 ~ LON)) %>%
		st_as_sf(coords = c("LON_not_360", "LAT"), crs = 4326, remove = FALSE)

	# larval plot year ####

#	larval_yr_plot <- 
#  	ggplot() +
#		geom_sf(data = catch0_dat_trim_sf,
#						aes(color = LARVALCATCHPER10M2),
#						color = "black", shape = 4, alpha = 0.1, size = 1) +
#		geom_sf(data = length_dat_sf, 
#						aes(color = CORRECTED_LENGTH, 
#								size = LARVALCATCHPER10M2),
#						alpha = 0.5) +
#		scale_color_viridis_c() +
#		geom_sf(data = world_map_data, fill = "white", lwd = 0) +
#		coord_sf(crs = 3338) +
# 		scale_x_continuous(
# 			breaks = breaks_x,
# 			labels =labels_x,
# 			name = "Longitude",
# 			limits = limits_x) +
# 		scale_y_continuous(
# 			breaks = breaks_y,
# 			limits = limits_y,
# 			name = "Latitude") +
#		labs(color = "length (mm)", size = expression(paste("catch (per 10m"^{2}*")"))) +
#		theme_bw() +
#		theme(
#			legend.title = element_text(size = 20, color = "white"),
#  		legend.title.align=0.5,
#  		legend.text = element_text(size = 20, color = "white"),
# 			axis.text = element_text(size = 8, colour = "white"),
#  	  axis.ticks = element_line(colour = "white"),
#  	  axis.line = element_line(colour = "white"),
#			axis.title = element_text(size = 30, color = "white"),
#  	  panel.border = element_rect(fill = NA, color = "white"))
	
	#	ggsave("./output/plots/larval_yr_plot.png",
	#		 larval_yr_plot,
	#		 width = 5, height = 5, units = "in")

	## with lat/longs rounded to 0 ##
	
	catch0_dat_trim <- catch0_dat_trim %>%
		mutate(LAT_round = round(LAT, 1),
					 LON_round = round(LON, 1)) %>%
		distinct(across(c(LAT_round, LON_round)), .keep_all = TRUE)

		
	# convert longs and rename cols
	length_dat_sf <- length_dat %>%
		dplyr::filter(., CORRECTED_LENGTH <= 6) %>%
		mutate(LON_not_360 = case_when(
				LON >= 180 ~ LON - 360,
				LON < 180 ~ LON)) %>%
		st_as_sf(coords = c("LON_not_360", "LAT"), crs = 4326, remove = FALSE)

	catch0_dat_trim_sf <- catch0_dat_trim %>%
		mutate(LON_not_360 = case_when(
				LON_round >= 180 ~ LON_round - 360,
				LON_round < 180 ~ LON_round)) %>%
		st_as_sf(coords = c("LON_not_360", "LAT_round"), crs = 4326, remove = FALSE)
	
	larval_yr_plot_LATLON <- 
  	ggplot() +
		geom_sf(data = catch0_dat_trim_sf,
						aes(color = LARVALCATCHPER10M2),
						color = "black", shape = 4, alpha = 0.1, size = 1) +
		geom_sf(data = length_dat_sf, 
						aes(color = CORRECTED_LENGTH, 
								size = LARVALCATCHPER10M2),
						alpha = 0.5) +
		scale_color_viridis_c() +
		geom_sf(data = world_map_data, fill = "lightgrey", lwd = 0) +
		coord_sf(crs = 3338) +
 		scale_x_continuous(
 			breaks = breaks_x,
 			labels = c("-170˚W", "-160˚W"),
 			name = "Longitude",
 			limits = limits_x) +
 		scale_y_continuous(
 			breaks = breaks_y,
 			limits = limits_y,
 			name = "Latitude") +
		labs(color = "length (mm)", size = expression(paste("catch (per 10m"^{2}*")"))) +
		theme_bw() +
		theme(
			legend.title = element_text(size = 14, colour = "black"),
  		legend.title.align=0.5,
  		legend.text = element_text(size = 12, colour = "black"),
 			axis.text = element_text(size = 14, colour = "black"),
  	  axis.ticks = element_line(colour = "black"),
  	  axis.line = element_line(colour = "black"),
			axis.title = element_text(size = 16, color = "black"),
  	  panel.border = element_rect(fill = NA, color = "black"))
	
	#	ggsave("./output/plots/larval_yr_plot_LATLON.png",
	#		 larval_yr_plot_LATLON,
	#		 width = 5, height = 5, units = "in")
		
		 ggsave("./scripts/manuscript figure scripts/used in ms/pngs of figs/Figure8.tiff",
			 larval_yr_plot_LATLON, dpi = 500,
			 width = 7, height = 7, units = "in")
		 
