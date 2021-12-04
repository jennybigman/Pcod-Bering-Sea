
		
		####### old code
		
		# bias correction

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
	
	cesm_baseline_temp_dat <- cesm_dat_trim %>%
		filter(., year %in% baseline_years)
	
	# estimate a monthly-avg temp for each grid cell for each month of the historical baseline period
	# (so an avg temp for each month at each grid cell across 1980 - 2014)
	cesm_baseline_temp_dat_mo <- cesm_baseline_temp_dat %>%
		group_by(month, Lat, Lon) %>%
		summarize(mo_baseline_temp = mean(temp))

	#### estimate deltas (on a monthly basis) #### 
	
	# trim projection df to years after historical baseline
	projection_years <- 2015:2099 # can trim to 2021:2099
	
	cesm_proj_temp_dat <- cesm_dat_trim %>%
		filter(., year %in% projection_years)

	# calculate monthly temps for each year for each grid cell for projected temps
	cesm_proj_temp_dat_mo <- cesm_proj_temp_dat %>% 
		group_by(year, month, Lat, Lon) %>%
		summarise(mo_avg_proj_temp = mean(temp))
	
	# combine the monthly means for historical period and projected df into one df
	cesm_bc_dat <- merge(cesm_proj_temp_dat_mo, cesm_baseline_temp_dat_mo,
											 by = c("Lat", "Lon", "month"))
	
	# calculate delta (monthly temp for each year minus the monthly avg temp for historical period)
	# and the bias-corrected temps (add deltas to the monthly avg temps for historical period?)
	cesm_bc_dat <- cesm_bc_dat %>%
		mutate(delta = (mo_avg_proj_temp - mo_baseline_temp),
					 bc_temp = (delta + mo_baseline_temp))
	
	
	#### not on a grid cell basis ####
	
	# estimate a monthly-avg temp for all grid cells for each month of the historical baseline period
	# (so an avg temp for each month across grid cells across 1980 - 2014)
	cesm_baseline_temp_dat_mo2 <- cesm_baseline_temp_dat %>%
		group_by(month) %>%
		summarize(mo_baseline_temp = mean(temp))

	#### estimate deltas (on a monthly basis) #### 
	
	# combine the monthly means for historical period and projected df into one df
	cesm_bc_dat2 <- merge(cesm_proj_temp_dat_mo, cesm_baseline_temp_dat_mo2,
											 by = c("month"))
	
	# calculate delta (monthly temp for each year minus the monthly avg temp for historical period)
	# and the bias-corrected temps (add deltas to the monthly avg temps for historical period?)
	cesm_bc_dat2 <- cesm_bc_dat2 %>%
		mutate(delta = (mo_avg_proj_temp - mo_baseline_temp),
					 bc_temp = (delta + mo_baseline_temp))
	
	
	#### using hindcast data as the historical baseline ####
	
	#### estimate monthly mean temps for baseline period ####
	
	# following Cheng et al 2021, historical baseline is 1980 - 2015
	baseline_years <- 1980:2014
	
	ROMS_baseline_temp_dat <- ROMS_hindcast_dat %>%
		filter(., year %in% baseline_years)
	
	# estimate a monthly-avg temp for each grid cell for each month of the historical baseline period
	# (so an avg temp for each month at each grid cell across 1980 - 2014)
	ROMS_baseline_temp_dat_mo <- ROMS_baseline_temp_dat %>%
		mutate(Lon = longitude,
					 Lat = latitude) %>%
		group_by(month, Lat, Lon) %>%
		summarize(mo_baseline_temp = mean(temp))

	#### estimate deltas (on a monthly basis) #### 
	
	# trim projection df to years after historical baseline
	projection_years <- 2015:2099 # can trim to 2021:2099
	
	cesm_proj_temp_dat <- cesm_dat_trim %>%
		filter(., year %in% projection_years)

	# calculate monthly temps for each year for each grid cell for projected temps
	cesm_proj_temp_dat_mo <- cesm_proj_temp_dat %>% 
		group_by(year, month, Lat, Lon) %>%
		summarise(mo_avg_proj_temp = mean(temp))
	
	# combine the monthly means for historical period and projected df into one df
	cesm_bc_dat_ROMS <- merge(cesm_proj_temp_dat_mo, ROMS_baseline_temp_dat_mo,
											 by = c("Lat", "Lon", "month"))
	
	# calculate delta (monthly temp for each year minus the monthly avg temp for historical period)
	# and the bias-corrected temps (add deltas to the monthly avg temps for historical period?)
	cesm_bc_dat_ROMS <- cesm_bc_dat_ROMS %>%
		mutate(delta = (mo_avg_proj_temp - mo_baseline_temp),
					 bc_temp = (delta + mo_baseline_temp))