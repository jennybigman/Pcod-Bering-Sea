# bias-correction

	library(here)
	library(ncdf4)
	library(tidync)
	require(tidyverse)
	library(data.table)
	
	#### read in data ####
	
	# read in ROMS projected temperature data
	cesm_dat_trim <- fread(file = here("data", "cesm_dat_trim.csv"))
	gfdl_dat_trim <- fread(file = here("data", "gfdl_dat_trim.csv"))
	miroc_dat_trim <- fread(file = here("data", "miroc_dat_trim.csv"))
	
	#### estimate monthly mean temps for baseline period ####
	
	# following Cheng et al 2021, historical baseline is 1980 - 2015
	baseline_years <- 1980:2014
	
	cesm_baseline_temp_dat <- cesm_dfs_trim %>%
		filter(., year %in% baseline_years)
	
	# estimate a monthly-avg for each month of all years of the historical baseline period
	cesm_baseline_temp_dat_mo <- cesm_baseline_temp_dat %>%
		group_by(month, Lat, Lon) %>%
		summarize(mean_temp = mean(temp))

	#### estimate deltas #### 
	
	cesm_dfs_trim_mo <- cesm_dfs_trim %>%
		group_by(year, month, Lat, Lon) %>%
		summarise(mo_temp = mean(temp))
	
	cesm_bc_dat <- merge(cesm_dfs_trim_mo, cesm_baseline_temp_dat_mo,
											 by = c("Lat", "Lon", "month"))
		
	cesm_bc_dat <- cesm_bc_dat %>%
		mutate(delta = (mo_temp - mean_temp),
					 bc_temp = (delta + mean_temp))
	
	# trim by ROMS df
	
	cesm_bc_dat_sum <- cesm_bc_dat %>%
		group_by(Lat, Lon) %>%
		summarize(mean_temp = mean(mean_temp)) %>%
		mutate(latitude = Lat,
					 long_not_360 = case_when(
							Lon >= 180 ~ Lon - 360,
							Lon < 180 ~ Lon)) %>%
  	st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326)
	
	names(cesm_bc_dat_sum)
	cesm_bc_dat_sum <- cesm_bc_dat_sum %>%
		rename(latitude = Lat,
					 longitude = Lon)
		
		ROMS_hindcast_dat_sum <- ROMS_hindcast_dat %>%
		group_by(latitude, longitude) %>%
 		summarise(mean_temp = mean(temp)) %>%
		mutate(long_not_360 = case_when(
				  		longitude >= 180 ~ longitude - 360,
				  		longitude < 180 ~ longitude)) %>%
  	st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326)
		
		cesm_int <- st_intersection(cesm_bc_dat_sum, ROMS_hindcast_dat_sum)
		
		cesm_dat <- cesm_dfs_trim %>%
			filter(., Lon %in% cesm_int$longitude) %>%
			filter(., Lat %in% cesm_int$latitude)
	
		