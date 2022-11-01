# boas-correction at domain/strata level, not grid cell level

# bias correction

	library(here)
	library(ncdf4)
	library(tidync)
	require(tidyverse)
	library(data.table)
	library(sf)
	
	#### read in data ####
	
	# read in hindcast data
	ROMS_hindcast_dat  <- 	fread("./data/ROMS_hindcast_temp_dat.csv")

	# read in ROMS projected temps but not trimmed to Ortiz regions 
	cesm_dfs_trim <- fread("./data/cesm_dfs_trim.csv")
	gfdl_dfs_trim <- fread("./data/gfdl_dfs_trim.csv")
	miroc_dfs_trim <- fread("./data/miroc_dfs_trim.csv")

	# read in area and depth data 
	area_df <- fread( "./data/ROMS_area_grid_cells.csv")
	depth_df <- fread("./data/ROMS_depth_df.csv")
	domain_df <- fread("./data/ROMS_domain_df.csv") %>%
		mutate(Lon = longitude,
					 Lat = latitude)
	
	# merge
	area_depth_df <- merge(area_df, depth_df, by = c("latitude", "longitude", "Xi", "Eta"))

	# add domains to temp data to bias correct at the domain level
	cesm_dfs_trim <- merge(cesm_dfs_trim, domain_df,
												 by = c("Lat", "Lon")) # is this Lat/Lon or latitude/longitude
	
	gfdl_dfs_trim <- merge(gfdl_dfs_trim, domain_df,
												 by = c("Lat", "Lon"))

	miroc_dfs_trim <- merge(miroc_dfs_trim, domain_df,
													 by = c("Lat", "Lon"))


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
		group_by(month, domain) %>%
		summarize(mean_mo_baseline_temp = mean(temp),
							sd_mo_baseline_temp = sd(temp))
	
	
	#### CESM ####
	
	#2 calculate the mean of the projections during the reference years ####
	
	cesm_baseline_temp_dat <- cesm_dfs_trim %>%
		dplyr::filter(., year %in% baseline_years)
	
	# estimate a monthly-avg temp for each grid cell for each month 
	# for the ref period 
	# (so an avg temp for each month at each grid cell averaged across 1980 - 2014)
	cesm_baseline_temp_dat_mo <- cesm_baseline_temp_dat %>%
		group_by(month, domain) %>%
		summarize(mean_proj_baseline = mean(temp),
							sd_proj_baseline = sd(temp))

	#3 estimate the monthly-averaged temps for each grid cell for each yr for projected yrs ####
	#projection_years <- 2015:2099 # can trim to 2021:2099
	#
	#cesm_proj_temp_dat <- cesm_dfs_trim %>% 
	#	filter(., year %in% projection_years)
#
	#cesm_proj_temp_dat <- cesm_proj_temp_dat %>% 
	#	group_by(scenario, year, month, Lat, Lon) %>%
	#	summarise(mo_avg_proj_temp = mean(temp))
	
	cesm_proj_temp_dat <- cesm_dfs_trim %>% 
		group_by(scenario, year, month, Lat, Lon, domain) %>%
		summarise(mo_avg_proj_temp = mean(temp))
	

	#4 calculate deltas (difference btw raw projected temp and mean proj temp across ####
	# ref period)
	
	# combine the monthly means for historical period and projected df into one df
	cesm_delta_dat <- merge(cesm_proj_temp_dat, cesm_baseline_temp_dat_mo,
											 by = c("domain", "month"))

	cesm_delta_dat <- cesm_delta_dat %>%
		mutate(delta = (mo_avg_proj_temp - mean_proj_baseline))
	
	#5 add deltas mean of the hindcast during the reference years (step 1)
	cesm_bc_temps <- merge(ROMS_baseline_temp_dat_mo, cesm_delta_dat,
											 by = c("domain", "month"))
	
	cesm_bc_temps_domain <- cesm_bc_temps %>%
		mutate(sd_ratio = sd_mo_baseline_temp/sd_proj_baseline,
					 bc_temp = delta + mean_mo_baseline_temp,
					 bc_temp_sd = (sd_ratio * delta) + mean_mo_baseline_temp)
	
	fwrite(cesm_bc_temps_domain, file = "./data/cesm_bc_temps_domain.csv")
	
	#### GFDL ####
	
	#2 calculate the mean of the projections during the reference years ####
	
	gfdl_baseline_temp_dat <- gfdl_dfs_trim %>% ### changed this from gfdl_dat_trim #####
		filter(., year %in% baseline_years)
	
	# estimate a monthly-avg temp for each grid cell for each month 
	# for the ref period 
	# (so an avg temp for each month at each grid cell averaged across 1980 - 2014)
	gfdl_baseline_temp_dat_mo <- gfdl_baseline_temp_dat %>%
		group_by(month, domain) %>%
		summarize(mean_proj_baseline = mean(temp),
								sd_proj_baseline = sd(temp))

	#3 estimate the monthly-averaged temps for each grid cell for each yr for projected yrs ####

#	projection_years <- 2015:2099 # can trim to 2021:2099
#	
#	gfdl_proj_temp_dat <- gfdl_dfs_trim %>%
#		filter(., year %in% projection_years)
#
#	gfdl_proj_temp_dat <- gfdl_proj_temp_dat %>% 
#		group_by(scenario, year, month, Lat, Lon) %>%
#		summarise(mo_avg_proj_temp = mean(temp))
	
	gfdl_proj_temp_dat <- gfdl_dfs_trim %>% 
			group_by(scenario, year, month, Lat, Lon, domain) %>%
			summarise(mo_avg_proj_temp = mean(temp))
	
	#4 estimate deltas (difference btw raw projected temp and mean proj temp across ####
	# ref period)
	
	# combine the monthly means for historical period and projected df into one df
	gfdl_delta_dat <- merge(gfdl_proj_temp_dat, gfdl_baseline_temp_dat_mo,
											 by = c("domain", "month"))
	
	gfdl_delta_dat <- gfdl_delta_dat %>%
		mutate(delta = (mo_avg_proj_temp - mean_proj_baseline))
	
	#5 add deltas mean of the hindcast during the reference years (step 1)
	gfdl_bc_temps <- merge(ROMS_baseline_temp_dat_mo, gfdl_delta_dat,
											 by = c("domain", "month"))
	
	gfdl_bc_temps_domain <- gfdl_bc_temps %>%
		mutate(sd_ratio = sd_mo_baseline_temp/sd_proj_baseline,
					 bc_temp = delta + mean_mo_baseline_temp,
					 bc_temp_sd = (sd_ratio * delta) + mean_mo_baseline_temp)
	
	fwrite(gfdl_bc_temps_domain, file = "./data/gfdl_bc_temps_domain.csv")

	#### MIROC ####

	#2 calculate the mean of the projections during the reference years ####
	
	miroc_baseline_temp_dat <- miroc_dfs_trim %>%  ### changed this from miroc_dat_trim #####
		filter(., year %in% baseline_years)
	
	# estimate a monthly-avg temp for each grid cell for each month 
	# for the ref period 
	# (so an avg temp for each month at each grid cell averaged across 1980 - 2014)

		miroc_baseline_temp_dat_mo <- miroc_baseline_temp_dat %>%
		group_by(month, domain) %>%
		summarize(mean_proj_baseline = mean(temp),
										sd_proj_baseline = sd(temp))

	#3 estimate the monthly-averaged temps for each grid cell for each yr for projected yrs ####
	#projection_years <- 2015:2099 # can trim to 2021:2099
	#
	#miroc_proj_temp_dat <- miroc_dfs_trim %>%
	#	filter(., year %in% projection_years)

	#miroc_proj_temp_dat <- miroc_proj_temp_dat %>% 
	#	group_by(scenario, year, month, Lat, Lon) %>%
	#	summarise(mo_avg_proj_temp = mean(temp))
	
	miroc_proj_temp_dat <- miroc_dfs_trim %>% 
		group_by(scenario, year, month, Lat, Lon, domain) %>%
		summarise(mo_avg_proj_temp = mean(temp))
	
	#4 estimate deltas (difference btw raw projected temp and mean proj temp across ####
	# ref period)
	
	# combine the monthly means for historical period and projected df into one df
	miroc_delta_dat <- merge(miroc_proj_temp_dat, miroc_baseline_temp_dat_mo,
											 by = c("domain", "month"))
	
	miroc_delta_dat <- miroc_delta_dat %>%
		mutate(delta = (mo_avg_proj_temp - mean_proj_baseline))
	
	#5 add deltas mean of the hindcast during the reference years (step 1)
	miroc_bc_temps <- merge(ROMS_baseline_temp_dat_mo, miroc_delta_dat,
											 by = c("domain", "month"))
	
	miroc_bc_temps_domain <- miroc_bc_temps %>%
		mutate(sd_ratio = sd_mo_baseline_temp/sd_proj_baseline,
					 bc_temp = delta + mean_mo_baseline_temp,
					 bc_temp_sd = (sd_ratio * delta) + mean_mo_baseline_temp)

	
	fwrite(miroc_bc_temps_domain, file = "./data/miroc_bc_temps_domain.csv")

	#### add together ####
	cesm_bc_temps_domain <- fread(file = "./data/cesm_bc_temps_domain.csv")
	gfdl_bc_temps_domain <- fread(file = "./data/gfdl_bc_temps_domain.csv")
	miroc_bc_temps_domain <- fread(file = "./data/miroc_bc_temps_domain.csv")

	cesm_bc_temps_domain$simulation <- "cesm"
	gfdl_bc_temps_domain$simulation <- "gfdl"
	miroc_bc_temps_domain$simulation <- "miroc"
	
	cesm_bc_temps_domain$projection <- cesm_bc_temps_domain$scenario
	gfdl_bc_temps_domain$projection <- gfdl_bc_temps_domain$scenario
	miroc_bc_temps_domain$projection <- miroc_bc_temps_domain$scenario

	proj_temp_dat_all <- bind_rows(cesm_bc_temps_domain, gfdl_bc_temps_domain, miroc_bc_temps_domain) ## this is not trimmed
	
	fwrite(proj_temp_dat_all, file = here("./data/ROMS_proj_temp_dat_all.csv"))
	
	#proj_temp_dat_all <- fread(file = here("./data/ROMS_proj_temp_dat_all.csv"))
	
	# trim by depth -- keep < 250 m
	proj_temp_dat_all <- proj_temp_dat_all %>%
		mutate(latitude = Lat,
					 longitude = Lon)
	
	proj_temp_dat_all <- merge(area_depth_df, proj_temp_dat_all,
														 by = c("latitude", "longitude"))
	
	proj_temp_dat <- proj_temp_dat_all %>%
		filter(., between(depth, 0, 250))

	## check whether trimmed to region and depth
	
	proj_temp_dat_sum <- proj_temp_dat %>%
		group_by(latitude, longitude) %>%
		summarize(mean_temp = mean(bc_temp))
	
	proj_temp_dat_sum_sf <- proj_temp_dat_sum	%>% 
		mutate(long_not_360 = case_when(
					 longitude >= 180 ~ longitude - 360,
					 longitude < 180 ~ longitude))  %>%
  	st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326, remove = FALSE)
	
	ROMS_strata_temp_proj_plot <-
 		ggplot() +
		geom_sf(data = proj_temp_dat_sum_sf, aes(color = mean_temp))  +
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
	
	fwrite(proj_temp_dat, "./data/proj_temp_dat.csv")

#	proj_temp_dat <- fread("./data/proj_temp_dat.csv")

	
	
	
	
	
	
	
	
	
	
	
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

	yearly_temp_dat_proj <- proj_temp_dat %>% 
		group_by(simulation, projection, year) %>%
   	summarise(mean_temp = mean(bc_temp),
   						mean_sd_temp = mean(bc_temp_sd))
	
	yearly_temp_dat_proj <- yearly_temp_dat_proj %>%
		tidyr::unite("sim_proj", simulation, projection, remove = F)

	colors <- c("lightgrey", "#efd966", "#b79a00", 
						  "lightgrey", "#7fb27f", "#004700", 
						  "lightgrey", "#6666b2", "#000059")
	
	sim_proj <- unique(yearly_temp_dat_proj$sim_proj)
	
	names(colors) <- unique(yearly_temp_dat_proj$sim_proj)
	
	proj_temp_plots_mean <-    
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
	
	
		ggsave("./output/plots/proj_temp_plots_mean.png",
			 proj_temp_plots_mean,
			 width = 15, height = 5, units = "in")
		
		## sd
		
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
	
		## plot together
		
	proj_temp_plots_mean_facet <-    
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
	  	name = "Yearly-averaged\nprojected temperature\nwith means",
	  	breaks = c(0, 2, 4, 6),
	  	labels = c(0, 2, 4, 6),
	  	limits = c(-1.5, 6)
	  ) +
		scale_x_continuous(
	  	name = "Year",
	  	breaks = c(1980, 2030, 2080)) +
   	theme_bw() +
  	theme(legend.position = "none") +
  	theme(
			strip.background = element_blank(),
  		strip.text = element_text(size = 18, face = "bold"),
			axis.text.y = element_text(size = 16, colour = "grey50"),
  	  axis.ticks.y = element_line(colour = "grey50"),
  	  axis.line.y = element_line(colour = "grey50"),
  	  axis.title.y = element_text(size=18, color = "grey30"),
			axis.text.x = element_blank(),
			axis.ticks.x = element_blank(),
			axis.line.x = element_blank(),
			axis.title.x = element_blank(),
  	  panel.grid.major = element_blank(),
  	  panel.grid.minor = element_blank(),
  	  panel.border = element_rect(fill = NA, color = "grey50"))
	
	proj_temp_plots_sd_facet <-    
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
	  	name = "Yearly-averaged\n projected temperature\nwith sd ratio",
	  	breaks = c(0, 2, 4, 6),
	  	labels = c(0, 2, 4, 6),
	  	limits = c(-1.5, 6)
	  ) +
		scale_x_continuous(
	  	name = "Year",
	  	breaks = c(1980, 2030, 2080)) +
   	theme_bw() +
  	theme(legend.position = "none") +
  	theme(
			strip.background = element_blank(),
  		strip.text = element_blank(),
			axis.text = element_text(size = 16, colour = "grey50"),
  	  axis.ticks = element_line(colour = "grey50"),
  	  axis.line = element_line(colour = "grey50"),
  	  axis.title = element_text(size=18, color = "grey30"),
  	  panel.grid.major = element_blank(),
  	  panel.grid.minor = element_blank(),
  	  panel.border = element_rect(fill = NA, color = "grey50"))
	
	proj_temps <- proj_temp_plots_mean_facet/proj_temp_plots_sd_facet
	
	ggsave("./output/plots/proj_temps.png",
			 proj_temps,
			 width = 13, height = 7.5, units = "in")
	
		
	## monthly
		
	mo_temp_hind <- ROMS_hindcast_dat %>%
		group_by(year, month_name, month) %>%
    summarise(mean_temp = mean(temp))
	
	mo_temp_dat_proj <- ROMS_projected_dat %>% 
		group_by(simulation, projection, year, month_name, month) %>%
   	summarise(mean_bctemp = mean(bc_temp),
   						mean_bctempsd = mean(bc_temp_sd))
		
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
	
		
		##### plot frequency distributions of sd_ratios ####
		ROMS_projected_dat <- tidyr::unite(ROMS_projected_dat,"sim_proj",
															 simulation, projection, remove = F)

		colors <- c("#ffabab", "#6dc3a9", "grey",  #  high, low, historical
						    "#ff4040", "#4e8d9c", "grey",   
						    "#ffb733", "#97c3e5", "grey" ) 

		sim_proj <- unique(ROMS_projected_dat$sim_proj)
	
		names(colors) <- sim_proj

		sd_ratios_plot <- ggplot(ROMS_projected_dat) +
			geom_density(aes(x = sd_ratio, fill = sim_proj, group = sim_proj)) +
			facet_grid(simulation ~ projection) +
			scale_fill_manual(name = "sim_proj", values = colors) +
			theme_bw() +
			theme(legend.position = "none") +
			ggtitle("ratio of standard deviations of hindcast/projection during reference period ")

		ggsave("./output/plots/sd_ratios_plot.png",
			 sd_ratios_plot,
			 width = 15, height = 5, units = "in")
	
		delta_plot <- ggplot(ROMS_projected_dat) +
			geom_density(aes(x = delta, fill = sim_proj, group = sim_proj)) +
			facet_grid(simulation ~ projection) +
			scale_fill_manual(name = "sim_proj", values = colors) +
			theme_bw() +
			theme(legend.position = "none") +
			ggtitle("delta (difference between raw temp projection and mean of raw temp projection during ref period")

		ggsave("./output/plots/delta_plot.png",
			 delta_plot,
			 width = 15, height = 5, units = "in")
	
		temp_bc_mean_plot <- ggplot(ROMS_projected_dat) +
			geom_density(aes(x = bc_temp, fill = sim_proj, group = sim_proj)) +
			facet_grid(simulation ~ projection) +
			scale_fill_manual(name = "sim_proj", values = colors) +
			theme_bw() +
			scale_y_continuous(
				breaks = c(0, 0.25, 0.5, 0.75),
				labels = c(0, 0.25, 0.5, 0.75),
				limits = c(0, 0.75)
			) +
			scale_x_continuous(
				breaks = c(-4, 0, 4, 8, 12),
				labels = c(-4, 0, 4, 8, 12),
				limits = c(-4.5, 14)
			) +
			theme(legend.position = "none") +
			ggtitle("bias-corrected temp using just mean")

		ggsave("./output/plots/temp_bc_mean_plot.png",
			 temp_bc_mean_plot,
			 width = 15, height = 5, units = "in")

		temp_bc_meansd_plot <- ggplot(ROMS_projected_dat) +
			geom_density(aes(x = bc_temp_sd, fill = sim_proj, group = sim_proj)) +
			facet_grid(simulation ~ projection) +
			scale_fill_manual(name = "sim_proj", values = colors) +
			scale_y_continuous(
				breaks = c(0, 0.25, 0.5, 0.75),
				labels = c(0, 0.25, 0.5, 0.75),
				limits = c(0, 0.75)
			) +
			scale_x_continuous(
				breaks = c(-4, 0, 4, 8, 12),
				labels = c(-4, 0, 4, 8, 12),
				limits = c(-4.5, 14)
			) +
			theme_bw() +			
			theme(legend.position = "none") +
			ggtitle("bias-corrected temp using mean and sd")

		ggsave("./output/plots/temp_bc_meansd_plot.png",
			 temp_bc_meansd_plot,
			 width = 15, height = 5, units = "in")
		
		# spatially plot variance ratio
		ROMS_projected_dat_sf_sum <- ROMS_projected_dat_sf %>%
			group_by(latitude, longitude) %>%
			summarize(mean_sd_ratio = mean(sd_ratio))
			
		colfunc <- colorRampPalette(c("#005b96", "#b2cddf"))
		rev_cols <- colfunc(6)

		variance_ratio_map_region <-
			ggplot() +
			geom_sf(data = ROMS_projected_dat_sf_sum, aes(color = mean_sd_ratio))  +
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
	
		
