# bias correction

	library(here)
	library(ncdf4)
	library(tidync)
	require(tidyverse)
	library(data.table)
	
	#### read in data ####
	
	# read in hindcast data
	ROMS_hindcast_dat  <- fread(file = "./data/ROMS_hindcast_dat.csv")
	
	# read in ROMS projected temperature data --- CHANGED THINGS AROUND SO IGNORE THIS
	#cesm_dat_trim <- fread(file = here("data", "cesm_dat_trim.csv"))
	#gfdl_dat_trim <- fread(file = here("data", "gfdl_dat_trim.csv"))
	#miroc_dat_trim <- fread(file = here("data", "miroc_dat_trim.csv"))
	
	# read in ROMS projected temps but not trimmed to Ortiz regions --- NEED TO CHANGE THIS BACK TO TRIMMED DATA ONCE IT WORKS
	cesm_dfs_trim <- fread("./data/cesm_dfs_trim.csv")
	gfdl_dfs_trim <- fread("./data/gfdl_dfs_trim.csv")
	miroc_dfs_trim <- fread("./data/miroc_dfs_trim.csv")


	#1 calculate the mean of the hindcast during the reference years (needed for all models) ####
	
	baseline_years <- 1980:2014 # define baseline/ref years (here based on Cheng et al 2021)
	
	ROMS_baseline_temp_dat <- ROMS_hindcast_dat %>% # select ref yrs from df
		filter(., year %in% baseline_years)
	
	# estimate a monthly-avg temp for each grid cell for each month 
	# for the reference period
	# (so an avg temp for each month at each grid cell averaged across 1980 - 2014)
	ROMS_baseline_temp_dat_mo <- ROMS_baseline_temp_dat %>% 
		mutate(Lon = longitude,
					 Lat = latitude) %>%
		group_by(month, Lat, Lon) %>%
		summarize(mo_baseline_temp = mean(temp))
	
	#### CESM ####
	
	#2 calculate the mean of the projections during the reference years ####
	
	cesm_baseline_temp_dat <- cesm_dfs_trim %>% # changed this from cesm_dat_trim
		filter(., year %in% baseline_years)
	
	# estimate a monthly-avg temp for each grid cell for each month 
	# for the ref period 
	# (so an avg temp for each month at each grid cell averaged across 1980 - 2014)
	cesm_baseline_temp_dat_mo <- cesm_baseline_temp_dat %>%
		group_by(month, Lat, Lon) %>%
		summarize(mean_proj_baseline = mean(temp))

	#3 estimate the monthly-averaged temps for each grid cell for each yr for projected yrs ####
	#projection_years <- 2015:2099 # can trim to 2021:2099
	#
	#cesm_proj_temp_dat <- cesm_dfs_trim %>% ## changed to cesm_dfs_trim from cesm_dat_trim (not trimmed to ROMS area/depth)
	#	filter(., year %in% projection_years)

	cesm_proj_temp_dat <- cesm_dfs_trim %>% 
		group_by(projection, year, month, Lat, Lon) %>%
		summarise(mo_avg_proj_temp = mean(temp))
	
	#4 calculate deltas (difference btw raw projected temp and mean proj temp across ####
	# ref period)
	
	# combine the monthly means for historical period and projected df into one df
	cesm_delta_dat <- merge(cesm_proj_temp_dat, cesm_baseline_temp_dat_mo,
											 by = c("Lat", "Lon", "month"))
	
	cesm_delta_dat <- cesm_delta_dat %>%
		mutate(delta = (mo_avg_proj_temp - mean_proj_baseline))
	
	#5 add deltas mean of the hindcast during the reference years (step 1)
	cesm_bc_temps <- merge(ROMS_baseline_temp_dat_mo, cesm_delta_dat,
											 by = c("Lat", "Lon", "month"))
	
	cesm_bc_temps <- cesm_bc_temps %>%
		mutate(bc_temp = delta + mo_baseline_temp)
	
	
	#### GFDL ####
	
	#2 calculate the mean of the projections during the reference years ####
	
	gfdl_baseline_temp_dat <- gfdl_dfs_trim %>% ### changed this from gfdl_dat_trim #####
		filter(., year %in% baseline_years)
	
	# estimate a monthly-avg temp for each grid cell for each month 
	# for the ref period 
	# (so an avg temp for each month at each grid cell averaged across 1980 - 2014)
	gfdl_baseline_temp_dat_mo <- gfdl_baseline_temp_dat %>%
		group_by(month, Lat, Lon) %>%
		summarize(mean_proj_baseline = mean(temp))

	#3 estimate the monthly-averaged temps for each grid cell for each yr for projected yrs ####
	#projection_years <- 2015:2099 # can trim to 2021:2099
	#
	#gfdl_proj_temp_dat <- gfdl_dfs_trim %>%  ### changed this from gfdl_dat_trim #####
	#	filter(., year %in% projection_years)

	gfdl_proj_temp_dat <- gfdl_dfs_trim %>% 
		group_by(projection, year, month, Lat, Lon) %>%
		summarise(mo_avg_proj_temp = mean(temp))
	
	#4 estimate deltas (difference btw raw projected temp and mean proj temp across ####
	# ref period)
	
	# combine the monthly means for historical period and projected df into one df
	gfdl_delta_dat <- merge(gfdl_proj_temp_dat, gfdl_baseline_temp_dat_mo,
											 by = c("Lat", "Lon", "month"))
	
	gfdl_delta_dat <- gfdl_delta_dat %>%
		mutate(delta = (mo_avg_proj_temp - mean_proj_baseline))
	
	#5 add deltas mean of the hindcast during the reference years (step 1)
	gfdl_bc_temps <- merge(ROMS_baseline_temp_dat_mo, gfdl_delta_dat,
											 by = c("Lat", "Lon", "month"))
	
	gfdl_bc_temps <- gfdl_bc_temps %>%
		mutate(bc_temp = delta + mo_baseline_temp)
	
	
	#### MIROC ####

	#2 calculate the mean of the projections during the reference years ####
	
	miroc_baseline_temp_dat <- miroc_dfs_trim %>%  ### changed this from miroc_dat_trim #####
		filter(., year %in% baseline_years)
	
	# estimate a monthly-avg temp for each grid cell for each month 
	# for the ref period 
	# (so an avg temp for each month at each grid cell averaged across 1980 - 2014)
	miroc_baseline_temp_dat_mo <- miroc_baseline_temp_dat %>%
		group_by(month, Lat, Lon) %>%
		summarize(mean_proj_baseline = mean(temp))

	#3 estimate the monthly-averaged temps for each grid cell for each yr for projected yrs ####
	#projection_years <- 2015:2099 # can trim to 2021:2099
	#
	#miroc_proj_temp_dat <- miroc_dfs_trim %>% ### changed this from miroc_dat_trim #####
	#	filter(., year %in% projection_years)

	miroc_proj_temp_dat <- miroc_dfs_trim %>% 
		group_by(projection, year, month, Lat, Lon) %>%
		summarise(mo_avg_proj_temp = mean(temp))
	
	#4 estimate deltas (difference btw raw projected temp and mean proj temp across ####
	# ref period)
	
	# combine the monthly means for historical period and projected df into one df
	miroc_delta_dat <- merge(miroc_proj_temp_dat, miroc_baseline_temp_dat_mo,
											 by = c("Lat", "Lon", "month"))
	
	miroc_delta_dat <- miroc_delta_dat %>%
		mutate(delta = (mo_avg_proj_temp - mean_proj_baseline))
	
	#5 add deltas mean of the hindcast during the reference years (step 1)
	miroc_bc_temps <- merge(ROMS_baseline_temp_dat_mo, miroc_delta_dat,
											 by = c("Lat", "Lon", "month"))
	
	miroc_bc_temps <- miroc_bc_temps %>%
		mutate(bc_temp = delta + mo_baseline_temp)
	
	
	#### add together ####
	
	cesm_bc_temps$simulation <- "cesm"
	gfdl_bc_temps$simulation <- "gfdl"
	miroc_bc_temps$simulation <- "miroc"
	
	proj_temp_dat_all <- bind_rows(cesm_bc_temps, gfdl_bc_temps, miroc_bc_temps) ## this is not trimmed
	
	fwrite(proj_temp_dat_all, file = here("./data/ROMS_proj_temp_dat_all.csv"))
	
	## remove grid cells based on depth and location (keep those < 250 m and within Ortiz regions)
	
	proj_temp_dat_all_sum <- proj_temp_dat_all %>%
		group_by(Lat, Lon) %>%
		summarize(mean_bc_temp = mean(bc_temp)) %>%
		mutate(long_not_360 = case_when(
							Lon >= 180 ~ Lon - 360,
							Lon < 180 ~ Lon)) %>%
  	st_as_sf(coords = c("long_not_360", "Lat"), crs = 4326)
  
  # make a summary object of the hindcast data for intersecting the lat/lons
  ROMS_hindcast_dat_sum <- ROMS_hindcast_dat %>%
		group_by(latitude, longitude) %>%
 		summarise(mean_temp = mean(temp)) %>%
		mutate(long_not_360 = case_when(
				   longitude >= 180 ~ longitude - 360,
				   longitude < 180 ~ longitude)) %>%
  	st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326)
	
	dat_ints <- st_intersection(ROMS_hindcast_dat_sum, proj_temp_dat_all_sum)
	
	dat_ints_ol <- dat_ints %>%
		 mutate(latitude = unlist(map(dat_ints$geometry,2)),
            longitude2 = unlist(map(dat_ints$geometry,1)))

	proj_temp_dat <- proj_temp_dat_all %>% 
		filter(., Lon %in% dat_ints_ol$longitude) %>%
		filter(., Lat %in% dat_ints_ol$latitude)

	fwrite(cesm_dat_trim, "./data/cesm_dat_trim.csv")


	#### plot #### REDO THESE PLOTS SO THEY ARE FACETED (DF CHANGED TO ALL DATA)
	
	#### maps ####

	# cesm # 
	# summarize by year
  cesm_bc_temps_sum <- cesm_bc_temps %>%
		group_by(projection, Lat, Lon, year) %>%
		summarise(mean_bc_temp = mean(bc_temp))

  # convert to sf object
  cesm_bc_temps_sum_sf <- cesm_bc_temps_sum %>% 
			mutate(latitude = Lat,
				long_not_360 = case_when(
					Lon >= 180 ~ Lon - 360,
					Lon < 180 ~ Lon)) %>%
  		st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326)
  	
  	cesm_bc_temps_sum_decade_sf <- cesm_bc_temps_sum_sf %>%
		mutate(decade = case_when(
			between(year, 2010, 2019) ~ "2010s",
			between(year, 2020, 2029) ~ "2020s",
			between(year, 2030, 2039) ~ "2030s",
			between(year, 2040, 2049) ~ "2040s",
			between(year, 2050, 2059) ~ "2050s",
			between(year, 2060, 2069) ~ "2060s",
			between(year, 2070, 2079) ~ "2070s",
			between(year, 2080, 2089) ~ "2080s",
			between(year, 2090, 2099) ~ "2090s"))
	
	cesm_bc_proj_decade <- 
  		ggplot() +
					geom_sf(data = cesm_bc_temps_sum_decade_sf, 
									aes(color = mean_bc_temp))  +
					geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
					coord_sf(crs = 3338) +
  		    facet_grid(projection ~ decade) +
					scale_color_viridis_c() +
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
    	  	labs(colour = "bottom temp C") +
					theme_bw() +
 					theme(
 						strip.text = element_text(size = 14, face = "bold"),
 						strip.background = element_blank(),
 						axis.text = element_text(size = 12),	
  					axis.title = element_text(size = 14),
  					legend.title = element_text(hjust = 0.5, size = 12),
 						plot.title = element_text(size = 18, face = "bold")
 						)
  	
		ggsave("./output/plots/cesm_bc_proj_decade.png",
			cesm_bc_proj_decade,
			width = 15, height = 10, units = "in")
	
	
	# gfdl # 
	# summarize by year
  gfdl_bc_temps_sum <- gfdl_bc_temps %>%
		group_by(projection, Lat, Lon, year) %>%
		summarise(mean_bc_temp = mean(bc_temp))

  # convert to sf object
  gfdl_bc_temps_sum_sf <- gfdl_bc_temps_sum %>% 
			mutate(latitude = Lat,
				long_not_360 = case_when(
					Lon >= 180 ~ Lon - 360,
					Lon < 180 ~ Lon)) %>%
  		st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326)
  	
  	gfdl_bc_temps_sum_decade_sf <- gfdl_bc_temps_sum_sf %>%
		mutate(decade = case_when(
			between(year, 2010, 2019) ~ "2010s",
			between(year, 2020, 2029) ~ "2020s",
			between(year, 2030, 2039) ~ "2030s",
			between(year, 2040, 2049) ~ "2040s",
			between(year, 2050, 2059) ~ "2050s",
			between(year, 2060, 2069) ~ "2060s",
			between(year, 2070, 2079) ~ "2070s",
			between(year, 2080, 2089) ~ "2080s",
			between(year, 2090, 2099) ~ "2090s"))
	
	gfdl_bc_proj_decade <- 
  		ggplot() +
					geom_sf(data = gfdl_bc_temps_sum_decade_sf, 
									aes(color = mean_bc_temp))  +
					geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
					coord_sf(crs = 3338) +
  		    facet_grid(projection ~ decade) +
					scale_color_viridis_c() +
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
    	  	labs(colour = "bottom temp C") +
					theme_bw() +
 					theme(
 						strip.text = element_text(size = 14, face = "bold"),
 						strip.background = element_blank(),
 						axis.text = element_text(size = 12),	
  					axis.title = element_text(size = 14),
  					legend.title = element_text(hjust = 0.5, size = 12),
 						plot.title = element_text(size = 18, face = "bold")
 						)
  	
		ggsave("./output/plots/gfdl_bc_proj_decade.png",
			gfdl_bc_proj_decade,
			width = 15, height = 10, units = "in")
		
		
	# miroc #

	# summarize by year
  miroc_bc_temps_sum <- miroc_bc_temps %>%
		group_by(projection, Lat, Lon, year) %>%
		summarise(mean_bc_temp = mean(bc_temp))

  # convert to sf object
  miroc_bc_temps_sum_sf <- miroc_bc_temps_sum %>% 
			mutate(latitude = Lat,
				long_not_360 = case_when(
					Lon >= 180 ~ Lon - 360,
					Lon < 180 ~ Lon)) %>%
  		st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326)
  	
  	miroc_bc_temps_sum_decade_sf <- miroc_bc_temps_sum_sf %>%
		mutate(decade = case_when(
			between(year, 2010, 2019) ~ "2010s",
			between(year, 2020, 2029) ~ "2020s",
			between(year, 2030, 2039) ~ "2030s",
			between(year, 2040, 2049) ~ "2040s",
			between(year, 2050, 2059) ~ "2050s",
			between(year, 2060, 2069) ~ "2060s",
			between(year, 2070, 2079) ~ "2070s",
			between(year, 2080, 2089) ~ "2080s",
			between(year, 2090, 2099) ~ "2090s"))
	
	miroc_bc_proj_decade <- 
  		ggplot() +
					geom_sf(data = miroc_bc_temps_sum_decade_sf, 
									aes(color = mean_bc_temp))  +
					geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
					coord_sf(crs = 3338) +
  		    facet_grid(projection ~ decade) +
					scale_color_viridis_c() +
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
    	  	labs(colour = "bottom temp C") +
					theme_bw() +
 					theme(
 						strip.text = element_text(size = 14, face = "bold"),
 						strip.background = element_blank(),
 						axis.text = element_text(size = 12),	
  					axis.title = element_text(size = 14),
  					legend.title = element_text(hjust = 0.5, size = 12),
 						plot.title = element_text(size = 18, face = "bold")
 						)
  	
		ggsave("./output/plots/miroc_bc_proj_decade.png",
			miroc_bc_proj_decade,
			width = 15, height = 10, units = "in")
		
		
	#### time series ####
		
		
	# plot yearly averages
			
	yearly_temp_dat_hind <- ROMS_hindcast_dat %>%
		group_by(year) %>%
    summarise(mean_temp = mean(temp))

	yearly_temp_dat_proj <- proj_temp_dat_all %>% 
		group_by(simulation, projection, year) %>%
   	summarise(mean_temp = mean(bc_temp))
	
	yearly_temp_dat_proj <- yearly_temp_dat_proj %>%
		tidyr::unite("sim_proj", simulation, projection, remove = F)

	colors <- c("lightgrey", "#efd966", "#b79a00", 
						  "lightgrey", "#7fb27f", "#004700", 
						  "lightgrey", "#6666b2", "#000059")
	
	sim_proj <- unique(yearly_temp_dat_proj$sim_proj)
	
	names(colors) <- unique(yearly_temp_dat_proj$sim_proj)
	
	proj_temp_plots <-    
   	ggplot() +
   	geom_line(data = yearly_temp_dat_hind, 
   						aes(x = year, y = mean_temp), 
   						color = "black", alpha = 0.5) +
		geom_line(data = yearly_temp_dat_proj,
							aes(year, mean_temp, 
									group = sim_proj, 
									color = sim_proj), alpha = 0.5) +
		facet_wrap(~ simulation) +
		xlab("Year") + 
		scale_color_manual(name = "sim_proj", values = colors) +
	  scale_y_continuous(
	  	name = "Yearly-averaged temp",
	  	breaks = c(0, 2, 4, 6),
	  ) +
		scale_x_continuous(
	  	name = "Year",
	  	breaks = c(1980, 2030, 2080)) +
   	theme_bw() +
  	theme(legend.position = "none") +
  	theme(
			strip.background = element_blank(),
  		strip.text = element_text(size = 18, face = "bold"),
			axis.text = element_text(size = 16, colour = "grey50"),
  	  axis.ticks = element_line(colour = "grey50"),
  	  axis.line = element_line(colour = "grey50"),
  	  axis.title = element_text(size=18, color = "grey30"),
  	  panel.grid.major = element_blank(),
  	  panel.grid.minor = element_blank(),
  	  panel.border = element_rect(fill = NA, color = "grey50"))
	
	
		ggsave("./output/plots/proj_temp_plots.png",
			 proj_temp_plots,
			 width = 15, height = 5, units = "in")
	
		
		## monthly
		
			## monthly
	
	mo_temp_hind <- ROMS_hindcast_dat %>%
		group_by(year, month_name, month) %>%
    summarise(mean_temp = mean(temp))
	
	mo_temp_dat_proj <- ROMS_projected_dat %>% 
		group_by(simulation, projection, year, month_name, month) %>%
   	summarise(mean_bctemp = mean(bc_temp)) 
		
	mo_temp_dat_proj <- mo_temp_dat_proj %>%
		tidyr::unite("sim_proj", simulation, projection, remove = F)

	colors <- c("lightgrey", "#efd966", "#b79a00", 
						  "lightgrey", "#7fb27f", "#004700", 
						  "lightgrey", "#6666b2", "#000059")
	
	sim_proj <- unique(mo_temp_dat_proj$sim_proj)
	
	names(colors) <- unique(mo_temp_dat_proj$sim_proj)
	
	# reorder for plotting
	mo_temp_hind$month_name <- factor(mo_temp_hind$month_name)
  mo_temp_hind$month_name <- fct_reorder(mo_temp_hind$month_name, 
  																		mo_temp_hind$month)
  
  mo_temp_dat_proj$month_name <- factor(mo_temp_dat_proj$month_name)
  mo_temp_dat_proj$month_name <- fct_reorder(mo_temp_dat_proj$month_name, 
  																		mo_temp_dat_proj$month)

	mo_bc_temp <-    
   	ggplot() +
   	geom_line(data = mo_temp_hind, 
   						aes(x = year, y = mean_temp), 
   						color = "black", alpha = 0.5) +
		geom_line(data = mo_temp_dat_proj,
							aes(year, mean_bctemp, 
									group = sim_proj, 
									color = sim_proj), alpha = 0.5) +
		facet_wrap(~ simulation + month_name) +
		xlab("Year") + 
		scale_color_manual(name = "sim_proj", values = colors) +
	  scale_y_continuous(
	  	name = "Mean bias-corrected temperature",
	  	breaks = c(0, 2, 4, 6),
	  ) +
		scale_x_continuous(
	  	name = "Year",
	  	breaks = c(1980, 2030, 2080)) +
   	theme_bw() +
  	theme(legend.position = "none") +
  	theme(
			strip.background = element_blank(),
  		strip.text = element_text(size = 18, face = "bold"),
			axis.text = element_text(size = 16, colour = "grey50"),
  	  axis.ticks = element_line(colour = "grey50"),
  	  axis.line = element_line(colour = "grey50"),
  	  axis.title = element_text(size=18, color = "grey30"),
  	  panel.grid.major = element_blank(),
  	  panel.grid.minor = element_blank(),
  	  panel.border = element_rect(fill = NA, color = "grey50"))
	
	
		ggsave("./output/plots/mo_bc_temp.png",
			 mo_bc_temp,
			 width = 15, height = 10, units = "in")
	