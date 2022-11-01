# bias corrections across all months, not just Jan - April


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
	
	ROMS_dat_hind$date <- as.Date(ROMS_dat_hind$DateTime) # date in Date format
	ROMS_dat_hind$month <- month(ROMS_dat_hind$date) # month of year
	ROMS_dat_hind$week <- week(ROMS_dat_hind$date) # week of year
	ROMS_dat_hind$year <- year(ROMS_dat_hind$date)
	
	# mean and sd of hindcast during reference years
	
	#### hindcast ####

	baseline_years <- 1985:2015 # define baseline/ref years *match Kelly's
	
	ROMS_hind_baseline <- ROMS_dat_hind %>% # select ref yrs from df
		filter(., year %in% baseline_years)
	
	# estimate means and sds of temp for each grid cell from the hindcast during
	# the reference period
	ROMS_baseline_sum <- ROMS_hind_baseline %>% 
		group_by(latitude, longitude) %>%
		summarize(mean_mo_baseline_temp = mean(temp),
							sd_mo_baseline_temp = sd(temp))
	
	#### cesm ####
	
	# trim to reference period (1985 - 2015)
	cesm_baseline_temp_dat <- cesm_dfs %>% 
		filter(., year %in% baseline_years)
	
	# estimate means and sds of temp for each grid cell from the hindcast during
	# the reference period
	cesm_baseline_sum <- cesm_baseline_temp_dat %>%
		group_by(latitude, longitude) %>%
		summarize(mean_proj_baseline = mean(temp),
							sd_proj_baseline = sd(temp))

	# merge hindcast dat and cesm dat 
	cesm_hind_dat <- merge(ROMS_baseline_sum, cesm_baseline_sum,
											 by = c("latitude", "longitude"))
	
	# calculate sd ratio by dividing the sd of the hindcast during the reference 
	# period by the sd ratio of the projection during the reference period
	cesm_sds <- cesm_hind_dat %>%
		mutate(sd_ratio = sd_mo_baseline_temp/sd_proj_baseline)
	
	# plot
	cesm_sds_sum <- cesm_sds %>%
			group_by(latitude, longitude) %>%
			summarize(mean_sd_ratio = mean(sd_ratio)) %>%
  		 mutate(long_not_360 = case_when(
						 longitude >= 180 ~ longitude - 360,
						 longitude < 180 ~ longitude)) 
  
	cesm_sds_sum_sf <- cesm_sds_sum %>% 
  	st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326, remove = FALSE)
  
	variance_ratio_map_cesm <-
			ggplot() +
			geom_sf(data = cesm_sds_sum_sf, aes(color = mean_sd_ratio))  +
			geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
			coord_sf(crs = 3338) +
			scale_color_viridis_c() +
 			scale_x_continuous(
 			 breaks = c(-170, -160),
			 labels = c("-170˚", "-160˚"),
			 limits = c(-1400000, 10000),
 				name = "Longitude") +
 			scale_y_continuous(
 				breaks = breaks_y,
 				limits = limits_y,
 				name = "Latitude") +
    	labs(colour = "variance ratio") +
			theme_bw() +
			theme(plot.title = element_text(hjust = 0.5),
						plot.tag.position = c(0.2, 0.87),
						axis.text = element_text(size = 12, colour = "grey50"),
  		  		axis.ticks.x = element_line(colour = "grey50"),
  		  		axis.line = element_blank(),
  		  		axis.title.x = element_text(size=14, color = "grey50"),
  		  		panel.border = element_rect(fill = NA, color = "grey50"),
						plot.margin = margin(0, 0, 0, 0, "cm"))
	
	 ggsave("./output/plots/variance_ratio_map_cesm.png",
			 variance_ratio_map_cesm,
			 width = 8, height = 8, units = "in")
