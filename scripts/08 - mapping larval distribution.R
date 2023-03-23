# 10 - larval distribution

	# map densities of larval distribution from XXX survey?

	# read in data
	length_dat <- read_csv("./data/PcodLengthData_BS_60Bon_Feb2022.csv")
	catch0_dat <- read_csv("./data/PcodLarvalCatchwZeros_BS_60Bon_Feb2022.csv")
	
	catch0_dat$GMT_DATE_TIME <- as.POSIXct(catch0_dat$GMT_DATE_TIME)
	
	catch0_dat_trim <- catch0_dat %>%
		filter(LARVALCATCHPER10M2 == 0) %>%
		distinct(across(c(LAT, LON)), .keep_all = TRUE)
		
	
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

	larval_yr_plot <- 
  	ggplot() +
		geom_sf(data = catch0_dat_trim_sf,
						aes(color = LARVALCATCHPER10M2),
						color = "black", shape = 4, alpha = 0.5, size = 1) +
		geom_sf(data = length_dat_sf, 
						aes(color = CORRECTED_LENGTH, 
								size = LARVALCATCHPER10M2),
						alpha = 0.5) +
		scale_color_viridis_c() +
		geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
		coord_sf(crs = 3338) +
 		scale_x_continuous(
 			breaks = breaks_x,
 			labels =labels_x,
 			name = "Longitude",
 			limits = limits_x) +
 		scale_y_continuous(
 			breaks = breaks_y,
 			limits = limits_y,
 			name = "Latitude") +
		labs(color = "length (mm)", size = expression(paste("catch (per 10m"^{2}*")"))) +
		theme_bw() +
		theme(
			legend.title = element_text(size = 8),
  		legend.title.align=0.5,
  		legend.text = element_text(size = 6),
 			axis.text = element_text(size = 8, colour = "grey50"),
  	  axis.ticks = element_line(colour = "grey50"),
  	  axis.line = element_line(colour = "grey50"),
			axis.title = element_text(size = 10, color = "grey50"),
  	  panel.border = element_rect(fill = NA, color = "grey50"))
	
		ggsave("./output/plots/larval_yr_plot.png",
			 larval_yr_plot,
			 width = 5, height = 5, units = "in")


		
		
		
		
	# larval plot year + month
	
	larval_mo_plot <- 
  	ggplot(catch0_dat_sf) +
		geom_sf(data = length_dat_sf, 
						aes(color = CORRECTED_LENGTH, 
								size = LARVALCATCHPER10M2),
						alpha = 0.5) +
		geom_sf(aes(color = LARVALCATCHPER10M2),
			data = . %>% filter(LARVALCATCHPER10M2 == 0), 
			color = "black", shape = 4, alpha = 0.5) +
		geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
		coord_sf(crs = 3338) +
	  facet_grid(MONTH ~ YEAR) +
 					scale_x_continuous(
 						breaks = breaks_x,
 						labels =labels_x,
 						name = "Longitude",
 						limits = limits_x
 					) +
 					scale_y_continuous(
 						breaks = breaks_y,
 						limits = limits_y,
 						name = "Latitude",
 					) +
					theme_bw() +
 					theme(
 						strip.text = element_text(size = 12, face = "bold"),
 						strip.background = element_blank(),
 						axis.text = element_text(size = 10),	
  					axis.title = element_text(size = 12),
  					legend.title.align=0.5)
	
		ggsave("./output/plots/larval_mo_plot.png",
			 larval_mo_plot,
			 width = 20, height = 10, units = "in")
		
	# plot without summer months because so few data points in those months
		
	# remove summer mo
	month_keep <- 4:6
		
	length_dat_trim_sf <- length_dat_sf %>%
			filter(MONTH %in% month_keep)
	
	catch0_dat_trim_sf <- catch0_dat_sf %>%
			filter(MONTH %in% month_keep)
		
	larval_mo_trim_plot <- 
  	ggplot(catch0_dat_trim_sf) +
		geom_sf(data = length_dat_trim_sf, 
						aes(color = CORRECTED_LENGTH, 
								size = LARVALCATCHPER10M2),
						alpha = 0.5) +
		geom_sf(aes(color = LARVALCATCHPER10M2),
			data = . %>% filter(LARVALCATCHPER10M2 == 0), 
			color = "black", shape = 4, alpha = 0.5) +
		geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
		coord_sf(crs = 3338) +
	  facet_grid(YEAR ~ MONTH) +
 					scale_x_continuous(
 						breaks = breaks_x,
 						labels =labels_x,
 						name = "Longitude",
 						limits = limits_x
 					) +
 					scale_y_continuous(
 						breaks = breaks_y,
 						limits = limits_y,
 						name = "Latitude",
 					) +
					theme_bw() +
 					theme(
 						strip.text = element_text(size = 12, face = "bold"),
 						strip.background = element_blank(),
 						axis.text = element_text(size = 10),	
  					axis.title = element_text(size = 12),
  					legend.title.align=0.5)
	
		ggsave("./output/plots/larval_mo_trim_plot.png",
			 larval_mo_trim_plot,
			 width = 20, height = 10, units = "in")
	
		
	# look at size distribution ####
	larval_dat_trim <- larval_dat %>%
		filter(length <= 6) 
	
	larval_sum <- larval_dat_trim %>%
		group_by(YEAR, MONTH) %>%
		summarise(n())

	size_hist <- 
		ggplot() +
		geom_histogram(data = larval_dat_trim,
									 aes(x = length)) +
	  facet_grid(MONTH ~ YEAR, scales = "free", space = "free") +
		theme_bw() +
 		theme(strip.text = element_text(size = 12, face = "bold"),
 					strip.background = element_blank(),
 					axis.text = element_text(size = 10),	
  				axis.title = element_text(size = 12),
  				legend.title.align=0.5)
	
	ggsave("./output/plots/size_hist.png",
			 size_hist,
			 width = 20, height = 10, units = "in")
	
	size_hist_wrap <- 
		ggplot() +
		geom_histogram(data = larval_dat_trim,
									 aes(x = length)) +
	  facet_wrap(YEAR ~ MONTH) +
		theme_bw() +
 		theme(strip.text = element_text(size = 12, face = "bold"),
 					strip.background = element_blank(),
 					axis.text = element_text(size = 10),	
  				axis.title = element_text(size = 12),
  				legend.title.align=0.5)
	
	ggsave("./output/plots/size_hist_wrap.png",
			 size_hist_wrap,
			 width = 20, height = 10, units = "in")
	
	filter_hist_larval_dat <- 
		ggplot(larval_dat_trim %>% group_by(YEAR, MONTH) %>%
        	 filter(n() >= 15),
        	 aes(length)) + 
    geom_histogram(bins = 15) +
    facet_grid(MONTH ~ YEAR) +
		theme_bw() +
 		theme(strip.text = element_text(size = 12, face = "bold"),
 					strip.background = element_blank(),
 					axis.text = element_text(size = 10),	
  				axis.title = element_text(size = 12),
  				legend.title.align=0.5)

	ggsave("./output/plots/filter_hist_larval_dat.png",
			 filter_hist_larval_dat,
			 width = 20, height = 10, units = "in")





##############################################################
	
	# merging dataframes ####

	# read in data
	length_dat <- read_csv("./data/PcodLengthData_BS_60Bon_Feb2022.csv")
	abund_dat <- read_csv("./data/PcodLarvalCatchwZeros_BS_60Bon_Feb2022.csv")
	
	abund_dat$GMT_DATE_TIME <- as.POSIXct(	abund_dat$GMT_DATE_TIME)
		
	# merge by haul ID 
	names_length <- names(length_dat)
	names_abund <- names(abund_dat)
	match_cols <- intersect(names_length, names_abund)
	match_cols <- match_cols[-(8:10)]
	larval_dat <- merge(length_dat, abund_dat, by = match_cols) 
	
	# make sure get the same number of records if just merge by HAUL ID
	larval_dat2 <- merge(length_dat, catch0_dat, by = "HAUL_ID") # get fewer records? 
	
	larval_dat <- larval_dat %>%
		rename(catch = "LARVALCATCHPER10M2.x",
					 length = "CORRECTED_LENGTH")

	# convert longs and rename cols
	larval_dat_sf <- larval_dat %>%
		filter(length <= 6) %>%
		mutate(LON_not_360 = case_when(
				LON >= 180 ~ LON - 360,
				LON < 180 ~ LON)) %>%
		st_as_sf(coords = c("LON_not_360", "LAT"), crs = 4326, remove = FALSE)
	# map density
	breaks_x <- c(-175, -170, -165, -160)
	labels_x <- c("-175˚", "-170˚", "-165˚", "-160˚") 
	limits_x <- c(-1400000, -150000)
	
	breaks_y <- c(55, 60)
	limits_y <- c(470000, 1900000)
	
	
	larval_sum_plot <- 
    	  	ggplot() +
					geom_sf(data = larval_dat_sum_sf, aes(color = mean_length, size = mean_catch))  +
					geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
					coord_sf(crs = 3338) +
					#scale_color_viridis_c() +
 					scale_x_continuous(
 						breaks = breaks_x,
 						labels =labels_x,
 						name = "Longitude",
 						limits = limits_x
 					) +
 					scale_y_continuous(
 						breaks = breaks_y,
 						limits = limits_y,
 						name = "Latitude",
 					) +
					theme_bw() +
 					theme(
 						strip.text = element_text(size = 14, face = "bold"),
 						strip.background = element_blank(),
 						axis.text = element_text(size = 12),	
  					axis.title = element_text(size = 14),
  					legend.title.align=0.5)
    	  
	# larval plot year
	
	larval_yr_plot <- 
  	ggplot() +
		geom_sf(data = larval_dat_sf, aes(color = length, size = catch))  +
		geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
		coord_sf(crs = 3338) +
	  facet_wrap(~ YEAR, ncol = 7, nrow = 3) +
 					scale_x_continuous(
 						breaks = breaks_x,
 						labels =labels_x,
 						name = "Longitude",
 						limits = limits_x
 					) +
 					scale_y_continuous(
 						breaks = breaks_y,
 						limits = limits_y,
 						name = "Latitude",
 					) +
					theme_bw() +
 					theme(
 						strip.text = element_text(size = 12, face = "bold"),
 						strip.background = element_blank(),
 						axis.text = element_text(size = 10),	
  					axis.title = element_text(size = 12),
  					legend.title.align=0.5)
	
		ggsave("./output/plots/larval_yr_plot.png",
			 larval_yr_plot,
			 width = 20, height = 10, units = "in")

	# larval plot year + month
	
	larval_mo_plot <- 
  	ggplot() +
		geom_sf(data = larval_dat_sf, aes(color = length, size = catch))  +
		geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
		coord_sf(crs = 3338) +
	  facet_grid(MONTH ~ YEAR) +
 					scale_x_continuous(
 						breaks = breaks_x,
 						labels =labels_x,
 						name = "Longitude",
 						limits = limits_x
 					) +
 					scale_y_continuous(
 						breaks = breaks_y,
 						limits = limits_y,
 						name = "Latitude",
 					) +
					theme_bw() +
 					theme(
 						strip.text = element_text(size = 12, face = "bold"),
 						strip.background = element_blank(),
 						axis.text = element_text(size = 10),	
  					axis.title = element_text(size = 12),
  					legend.title.align=0.5)
	
		ggsave("./output/plots/larval_mo_plot.png",
			 larval_mo_plot,
			 width = 20, height = 10, units = "in")
	
		# remove summer mo
		month_keep <- 4:6
		
		larval_dat_sf_trim <- larval_dat_sf %>%
			filter(MONTH %in% month_keep)
			
	larval_mo_trim_plot <- 
  	ggplot() +
		geom_sf(data = larval_dat_sf_trim, aes(color = length, size = catch))  +
		geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
		coord_sf(crs = 3338) +
	  facet_grid(MONTH ~ YEAR) +
 					scale_x_continuous(
 						breaks = breaks_x,
 						labels =labels_x,
 						name = "Longitude",
 						limits = limits_x
 					) +
 					scale_y_continuous(
 						breaks = breaks_y,
 						limits = limits_y,
 						name = "Latitude",
 					) +
					theme_bw() +
 					theme(
 						strip.text = element_text(size = 12, face = "bold"),
 						strip.background = element_blank(),
 						axis.text = element_text(size = 10),	
  					axis.title = element_text(size = 12),
  					legend.title.align=0.5)
	
		ggsave("./output/plots/larval_mo_trim_plot.png",
			 larval_mo_trim_plot,
			 width = 20, height = 10, units = "in")
	
	# size distribution
	larval_dat_trim <- larval_dat %>%
		filter(length <= 6) 
	
	larval_sum <- larval_dat_trim %>%
		group_by(YEAR, MONTH) %>%
		summarise(n())

	size_hist <- 
		ggplot() +
		geom_histogram(data = larval_dat_trim,
									 aes(x = length)) +
	  facet_grid(MONTH ~ YEAR, scales = "free", space = "free") +
		theme_bw() +
 		theme(strip.text = element_text(size = 12, face = "bold"),
 					strip.background = element_blank(),
 					axis.text = element_text(size = 10),	
  				axis.title = element_text(size = 12),
  				legend.title.align=0.5)
	
	ggsave("./output/plots/size_hist.png",
			 size_hist,
			 width = 20, height = 10, units = "in")
	
	
	size_hist_wrap <- 
		ggplot() +
		geom_histogram(data = larval_dat_trim,
									 aes(x = length)) +
	  facet_wrap(YEAR ~ MONTH) +
		theme_bw() +
 		theme(strip.text = element_text(size = 12, face = "bold"),
 					strip.background = element_blank(),
 					axis.text = element_text(size = 10),	
  				axis.title = element_text(size = 12),
  				legend.title.align=0.5)
	
	ggsave("./output/plots/size_hist_wrap.png",
			 size_hist_wrap,
			 width = 20, height = 10, units = "in")
	
	filter_hist_larval_dat <- 
		ggplot(larval_dat_trim %>% group_by(YEAR, MONTH) %>%
        	 filter(n() >= 15),
        	 aes(length)) + 
    geom_histogram(bins = 15) +
    facet_grid(MONTH ~ YEAR) +
		theme_bw() +
 		theme(strip.text = element_text(size = 12, face = "bold"),
 					strip.background = element_blank(),
 					axis.text = element_text(size = 10),	
  				axis.title = element_text(size = 12),
  				legend.title.align=0.5)

	ggsave("./output/plots/filter_hist_larval_dat.png",
			 filter_hist_larval_dat,
			 width = 20, height = 10, units = "in")
