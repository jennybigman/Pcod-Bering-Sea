# 00fa -- bias corrections with alternate years

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
	
	# read in area and depth data 
	area_df <- fread( "./data/ROMS_area_grid_cells.csv")
	depth_df <- fread("./data/ROMS_depth_df.csv")

	area_depth_df <- merge(area_df, depth_df, by = c("latitude", "longitude", "Xi", "Eta")) %>%
		dplyr::select(latitude, longitude, depth, area_km2)

	#1 calculate the mean of the hindcast during the reference years (needed for all models) ####
	
	baseline_years <- 2006:2017 # define baseline/ref years (here based on Cheng et al 2021) 
	
	ROMS_baseline_temp_dat <- ROMS_hindcast_dat %>% # select ref yrs from df
		filter(., year %in% baseline_years)
	
	# estimate a monthly-avg temp for each grid cell for each month 
	# for the reference period
	# (so an avg temp for each month at each grid cell averaged across 1980 - 2014)
	ROMS_baseline_temp_dat_mo <- ROMS_baseline_temp_dat %>% 
		mutate(Lon = longitude,
					 Lat = latitude) %>%
		group_by(month, Lat, Lon) %>%
		summarize(mean_mo_baseline_temp = mean(temp),
							sd_mo_baseline_temp = sd(temp))
	
	
	#### CESM ####
	
	#2 calculate the mean of the projections during the reference years ####
	
	cesm_baseline_temp_dat <- cesm_dfs_trim %>% # changed this from cesm_dat_trim
		filter(., year %in% baseline_years)
	
	# estimate a monthly-avg temp for each grid cell for each month 
	# for the ref period 
	# (so an avg temp for each month at each grid cell averaged across 1980 - 2014)
	cesm_baseline_temp_dat_mo <- cesm_baseline_temp_dat %>%
		group_by(month, Lat, Lon) %>%
		summarize(mean_proj_baseline = mean(temp),
							sd_proj_baseline = sd(temp))

	#3 estimate the monthly-averaged temps for each grid cell for each yr for projected yrs ####
	
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
		mutate(sd_ratio = sd_mo_baseline_temp/sd_proj_baseline,
					 bc_temp = delta + mean_mo_baseline_temp,
					 bc_temp_sd = (sd_ratio * delta) + mean_mo_baseline_temp)
	
	
	#### GFDL ####
	
	#2 calculate the mean of the projections during the reference years ####
	
	gfdl_baseline_temp_dat <- gfdl_dfs_trim %>% ### changed this from gfdl_dat_trim #####
		filter(., year %in% baseline_years)
	
	# estimate a monthly-avg temp for each grid cell for each month 
	# for the ref period 
	# (so an avg temp for each month at each grid cell averaged across 1980 - 2014)
	gfdl_baseline_temp_dat_mo <- gfdl_baseline_temp_dat %>%
		group_by(month, Lat, Lon) %>%
		summarize(mean_proj_baseline = mean(temp),
								sd_proj_baseline = sd(temp))

	#3 estimate the monthly-averaged temps for each grid cell for each yr for projected yrs ####
	
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
		mutate(sd_ratio = sd_mo_baseline_temp/sd_proj_baseline,
					 bc_temp = delta + mean_mo_baseline_temp,
					 bc_temp_sd = (sd_ratio * delta) + mean_mo_baseline_temp)
	
	
	#### MIROC ####

	#2 calculate the mean of the projections during the reference years ####
	
	miroc_baseline_temp_dat <- miroc_dfs_trim %>%  ### changed this from miroc_dat_trim #####
		filter(., year %in% baseline_years)
	
	# estimate a monthly-avg temp for each grid cell for each month 
	# for the ref period 
	# (so an avg temp for each month at each grid cell averaged across 1980 - 2014)
	miroc_baseline_temp_dat_mo <- miroc_baseline_temp_dat %>%
		group_by(month, Lat, Lon) %>%
		summarize(mean_proj_baseline = mean(temp),
										sd_proj_baseline = sd(temp))

	#3 estimate the monthly-averaged temps for each grid cell for each yr for projected yrs ####
	
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
		mutate(sd_ratio = sd_mo_baseline_temp/sd_proj_baseline,
					 bc_temp = delta + mean_mo_baseline_temp,
					 bc_temp_sd = (sd_ratio * delta) + mean_mo_baseline_temp)

	
	
	#### add together ####
	
	cesm_bc_temps$simulation <- "cesm"
	gfdl_bc_temps$simulation <- "gfdl"
	miroc_bc_temps$simulation <- "miroc"
	
	proj_temp_dat_all_Holsman <- bind_rows(cesm_bc_temps, gfdl_bc_temps, miroc_bc_temps) ## this is not trimmed
	
	fwrite(proj_temp_dat_all_Holsman, file = here("./data/ROMS_proj_temp_dat_all_Holsman.csv"))
	

	
	## remove grid cells based on depth and location (keep those < 250 m and within Ortiz regions)
	
	proj_temp_dat_all_Holsman_sum <- proj_temp_dat_all_Holsman %>%
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
  	st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326, remove = FALSE)
	
	dat_ints <- st_intersection(ROMS_hindcast_dat_sum, proj_temp_dat_all_Holsman_sum)
	
	proj_temp_dat_Holsman <- proj_temp_dat_all_Holsman %>% 
		mutate(longitude = Lon,
					 latitude = Lat) %>%
		filter(., Lon %in% dat_ints$longitude) %>%
		filter(., Lat %in% dat_ints$latitude)

	proj_temp_dat_Holsman <- merge(proj_temp_dat_Holsman, area_depth_df, by = c("latitude", "longitude"))

	fwrite(proj_temp_dat_Holsman, "./data/proj_temp_dat_Holsman.csv")

	#### time series plot ####
	proj_temp_dat_Holsman <- fread("./data/proj_temp_dat_Holsman.csv")

	yearly_temp_dat_hind <- ROMS_hindcast_dat %>%
		group_by(year) %>%
    summarise(mean_temp = mean(temp))

	yearly_temp_dat_proj_Holsman <- proj_temp_dat_Holsman %>% 
		group_by(simulation, projection, year) %>%
   	summarise(mean_temp = mean(bc_temp),
   						mean_sd_temp = mean(bc_temp_sd))
	
	yearly_temp_dat_proj_Holsman <- yearly_temp_dat_proj_Holsman %>%
		tidyr::unite("sim_proj", simulation, projection, remove = F)

	colors <- c("lightgrey", "#efd966", "#b79a00", 
						  "lightgrey", "#7fb27f", "#004700", 
						  "lightgrey", "#6666b2", "#000059")
	
	sim_proj <- unique(yearly_temp_dat_proj_Holsman$sim_proj)
	
	names(colors) <- unique(yearly_temp_dat_proj_Holsman$sim_proj)
	
	proj_temp_plots_sd_Holsman <-    
   	ggplot() +
   	geom_line(data = yearly_temp_dat_hind, 
   						aes(x = year, y = mean_temp), 
   						color = "black", alpha = 0.5) +
		geom_line(data = yearly_temp_dat_proj_Holsman,
							aes(year, mean_sd_temp, 
									group = sim_proj, 
									color = sim_proj), alpha = 0.5) +
		facet_wrap(~ simulation) +
		xlab("Year") + 
		scale_color_manual(name = "sim_proj", values = colors) +
	  scale_y_continuous(
	  	name = "Yearly-averaged temp",
	  	breaks = c(0,2,4,6),
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
	
	
		ggsave("./output/plots/proj_temp_plots_sd_Holsman.png",
			 proj_temp_plots_sd_Holsman,
			 width = 15, height = 4, units = "in")
	
		proj_temp_plots_sd <-    
   	ggplot() +
   	geom_line(data = yearly_temp_dat_hind, 
   						aes(x = year, y = mean_temp), 
   						color = "black", alpha = 0.5) +
		geom_line(data = yearly_temp_dat_proj,
							aes(year, mean_sd_temp, 
									group = sim_proj, 
									color = sim_proj), alpha = 0.5) +
		facet_wrap(~ simulation) +
		xlab("Year") + 
		scale_color_manual(name = "sim_proj", values = colors) +
	  scale_y_continuous(
	  	name = "Yearly-averaged temp",
	  	breaks = c(0,2,4,6),
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
	
	
		ggsave("./output/plots/proj_temp_plots_sd.png",
			 proj_temp_plots_sd,
			 width = 15, height = 4, units = "in")
	