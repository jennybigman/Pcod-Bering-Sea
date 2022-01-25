# summer temps for one projection plot


	library(here)
	library(ncdf4)
	library(tidync)
	require(tidyverse)
	library(data.table)
	library(sf)
	
	# set up lat/lons from area grid file

	# download from server
	url_base <- "https://data.pmel.noaa.gov/aclim/thredds/"
  opendap_area  <- "dodsC/extended_grid/Bering10K_extended_grid.nc"
  
  nc <- nc_open(paste(url_base,opendap_area,sep=""))
  
  # create objects for known lats and longs and xi and eta axes
  lats <- ncvar_get(nc,"lat_rho")
  lons <- ncvar_get(nc,"lon_rho")
 
	nc_close(nc)

  # CESM simulations ####
  
  # read in files
  cesm_file_list <- list.files(path = paste0(here(), ("/data/CMIP6_bottom_temp/cesm")))
 
	# historical baseline period ####
	cesm_historical_baseline_file_list <- cesm_file_list[str_detect(cesm_file_list, "historical")]
	
	prestring <- paste0(here(), ("/data/CMIP6_bottom_temp/cesm/"))
  
  cesm_hist_dat_list <- list()
  
  for(i in cesm_historical_baseline_file_list){
  	cesm_hist_dat_list[[i]] <- paste0(prestring, i)
  	cesm_hist_dat_list
  }

	cesm_hist_df_list <- list()
  	for(i in cesm_hist_dat_list){
  		cesm_hist_df_list[[i]] <- tidync(i) %>% hyper_tibble(select_var = "temp")
  		cesm_hist_df_list
  }
  
  cesm_hist_dfs <- bind_rows(cesm_hist_df_list)
  
  # add in lat/longs matched to xi/eta 
	cesm_hist_dfs$Lon <- lons[cbind(cesm_hist_dfs$xi_rho, cesm_hist_dfs$eta_rho)]
	cesm_hist_dfs$Lat <- lats[cbind(cesm_hist_dfs$xi_rho, cesm_hist_dfs$eta_rho)]

	# create object for time axis
	cesm_hist_dfs$DateTime <- as.POSIXct(cesm_hist_dfs$ocean_time, origin = "1900-01-01", tz = "GMT")

	
  # ssp 126 scenario ####
	
	cesm_ssp126_file_list <- cesm_file_list[str_detect(cesm_file_list, "ssp126")]
	
	prestring <- paste0(here(), ("/data/CMIP6_bottom_temp/cesm/"))
  
  cesm_ssp126_dat_list <- list()
  
  for(i in cesm_ssp126_file_list){
  	cesm_ssp126_dat_list[[i]] <- paste0(prestring, i)
  	cesm_ssp126_dat_list
  }

	cesm_ssp126_df_list <- list()
  	for(i in cesm_ssp126_dat_list){
  		cesm_ssp126_df_list[[i]] <- tidync(i) %>% hyper_tibble(select_var = "temp")
  		cesm_ssp126_df_list
  }
  
  cesm_ssp126_dfs <- bind_rows(cesm_ssp126_df_list)
  
  # add in lat/longs matched to xi/eta 
	cesm_ssp126_dfs$Lon <- lons[cbind(cesm_ssp126_dfs$xi_rho, cesm_ssp126_dfs$eta_rho)]
	cesm_ssp126_dfs$Lat <- lats[cbind(cesm_ssp126_dfs$xi_rho, cesm_ssp126_dfs$eta_rho)]

	# create object for time axis
	cesm_ssp126_dfs$DateTime <- as.POSIXct(cesm_ssp126_dfs$ocean_time, origin = "1900-01-01", tz = "GMT")

 # ssp585 scenario ####
	
	cesm_ssp585_file_list <- cesm_file_list[str_detect(cesm_file_list, "ssp585")]

	prestring <- paste0(here(), ("/data/CMIP6_bottom_temp/cesm/"))
  
  cesm_ssp585_dat_list <- list()
  
  for(i in cesm_ssp585_file_list){
  	cesm_ssp585_dat_list[[i]] <- paste0(prestring, i)
  	cesm_ssp585_dat_list
  }

	cesm_ssp585_df_list <- list()
  	for(i in cesm_ssp585_dat_list){
  		cesm_ssp585_df_list[[i]] <- tidync(i) %>% hyper_tibble(select_var = "temp")
  		cesm_ssp585_df_list
  }
  
  cesm_ssp585_dfs <- bind_rows(cesm_ssp585_df_list)
  
  # add in lat/longs matched to xi/eta 
	cesm_ssp585_dfs$Lon <- lons[cbind(cesm_ssp585_dfs$xi_rho, cesm_ssp585_dfs$eta_rho)]
	cesm_ssp585_dfs$Lat <- lats[cbind(cesm_ssp585_dfs$xi_rho, cesm_ssp585_dfs$eta_rho)]

	# create object for time axis
	cesm_ssp585_dfs$DateTime <- as.POSIXct(cesm_ssp585_dfs$ocean_time, origin = "1900-01-01", tz = "GMT")
	
	# join together
	cesm_ssp126_dfs$scenario <- "ssp126"
	cesm_ssp585_dfs$scenario <- "ssp585"
	cesm_hist_dfs$scenario <- "historical"

	
	cesm_dfs <- bind_rows(cesm_hist_dfs, cesm_ssp126_dfs, cesm_ssp585_dfs) 

	# separate date column into components
	cesm_dfs$date <- as.Date(cesm_dfs$DateTime) # date in Date format
	cesm_dfs$month <- month(cesm_dfs$date) # month of year
	cesm_dfs$week <- week(cesm_dfs$date) # week of year
	cesm_dfs$year <- year(cesm_dfs$date)

	
	# remove all months not May - Aug
	months <- c(5:8)
	
	summer_cesm_dfs_trim <- cesm_dfs %>%
		filter(., month %in% months) %>%
		mutate(latitude = Lat,
					 longitude = Lon)
	
	##### bias correct ####
	
	library(here)
	library(ncdf4)
	library(tidync)
	require(tidyverse)
	library(data.table)
	
	#### read in data ####
	
	# read in hindcast data
	ROMS_hindcast_dat  <- fread(file = "./data/ROMS_hindcast_dat.csv")
	
	# read in area and depth data 
	area_df <- fread( "./data/ROMS_area_grid_cells.csv")
	depth_df <- fread("./data/ROMS_depth_df.csv")

	area_depth_df <- merge(area_df, depth_df, by = c("latitude", "longitude", "Xi", "Eta")) %>%
		dplyr::select(latitude, longitude, depth, area_km2)

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
		summarize(mean_mo_baseline_temp = mean(temp),
							sd_mo_baseline_temp = sd(temp))
	
	
	#### CESM ####
	
	#2 calculate the mean of the projections during the reference years ####
	
	cesm_baseline_temp_dat <- summer_cesm_dfs_trim %>% # changed this from cesm_dat_trim
		filter(., year %in% baseline_years)
	
	# estimate a monthly-avg temp for each grid cell for each month 
	# for the ref period 
	# (so an avg temp for each month at each grid cell averaged across 1980 - 2014)
	cesm_baseline_temp_dat_mo <- cesm_baseline_temp_dat %>%
		group_by(month, Lat, Lon) %>%
		summarize(mean_proj_baseline = mean(temp),
							sd_proj_baseline = sd(temp))

	#3 estimate the monthly-averaged temps for each grid cell for each yr for projected yrs ####
	
	cesm_proj_temp_dat <- summer_cesm_dfs_trim %>% 
		group_by(scenario, year, month, Lat, Lon) %>%
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
					 bc_temp_sd = (sd_ratio * delta) + mean_mo_baseline_temp)
	
	
	#### plot ####
	
	yearly_temp_dat_hind <- ROMS_hindcast_dat %>%
		group_by(year) %>%
    summarise(mean_temp = mean(temp)) 

	yearly_temp_dat_proj <- cesm_bc_temps %>% 
		group_by(scenario, year) %>%
   	summarise(mean_temp =  mean(bc_temp_sd))

	proj_yrs <- 2020:2099

	yearly_temp_dat_proj_sum <- yearly_temp_dat_proj %>%
		group_by(year, scenario) %>%
		summarise(mean_temp = mean(mean_temp)) %>%
		filter(year %in% proj_yrs)
	
	colors <- c("#dbc0f6", "#c69bf1") # light then dark
	
	scenario <- unique(yearly_temp_dat_proj_sum$scenario)
	
	names(colors) <- unique(yearly_temp_dat_proj_sum$scenario)

	time_series_temp <-    
   	ggplot() +
		geom_vline(aes(xintercept = 2020), color = "white", size = 0.15) +
   	geom_line(data = yearly_temp_dat_hind, 
   						aes(x = year, y = mean_temp), 
   						color = "white") +
		geom_line(data = yearly_temp_dat_proj_sum,
							aes(year, mean_temp, 
									group = scenario, 
									color = scenario)) +
		xlab("Year") + 
		scale_color_manual(name = "scenario", values = colors) +
	  scale_y_continuous(
	  	name = "Bottom\ntemperature\n(˚C)",
	  	breaks = c(0, 2, 4, 6),
	  ) +
		scale_x_continuous(
	  	name = "Year",
	  	breaks = c(1970, 2000, 2022, 2030, 2060, 2090)) +
	theme(legend.position = "none") +
  theme(axis.text=element_text(size= 12, colour = "white"),
  			axis.title.y = element_text(angle = 0, vjust = 0.5),
  			axis.title= element_text(size=12, color = "white"),
  			axis.line = element_line(color = "white"),
  			axis.ticks = element_line(colour = "white"),
  			legend.title = element_text(color = "white"),
  			legend.text = element_text(color = "white"),
  			panel.background = element_rect(fill = "black"),
				panel.grid = element_blank(),
  			plot.background = element_rect(fill = "black", color = "black"))
	
	 
  	ggsave(here("./output/plots/time_series_temp_black.png"),
			 time_series_temp,
			 width = 9, height = 4, units = "in")
	
