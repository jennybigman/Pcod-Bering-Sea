	# download and transform level 2 hindcast and projection ROMS bottom temp output

	library(ncdf4)
	library(thredds)
	library(reshape2)
	library(here)
	library(data.table)
	library(tidync)
	library(tidyverse)


	#### hindcast temps ####
	
	hcsim <- "B10K-K20P19_CORECFS"  # hindcast
	vname1 <- "temp_bottom5m"    # Variable name in filename
	vname2 <- "temp"             # Variable name in file
	tdsbase <- "https://data.pmel.noaa.gov/aclim/thredds/dodsC" # top-level folder, PMEL server
	
	
	hindcast_download_func <- function(yr){

	# set up filepath
  fname <- file.path(tdsbase, 
                     hcsim, 
                     "Level2",
                     paste0(yr, "-", yr + 4), 
                     paste0(hcsim, "_", yr, "-", yr + 4, "_average_", vname1, ".nc"))
  
  # read data
  ncin <- nc_open(fname)
  
  # get ocean_time
  ocean_time <- ncvar_get(ncin, "ocean_time")
  
  # get temp
  BT_temp_array <- ncvar_get(ncin, "temp")
  
  # assign dim names and ocean_time
  xi_axis <- seq(1,182) # Hardcoded axis length, ROMS coordinates
  eta_axis <- seq(1,258) # Hardcoded axis length, ROMS coordinates

  dimnames(BT_temp_array) <- list("Xi" = xi_axis,"Eta" = eta_axis,
															 "Time" = ocean_time)

	# turn array into dataframe 
	BT_temp_df <- reshape2::melt(BT_temp_array, 
														varnames = c("Xi", "Eta", "Time"), 
														value.name = "temp")
	
	# translate to lat/lon and time
	BT_temp_df$DateTime <- as.POSIXct(BT_temp_df$Time, origin = "1900-01-01", tz = "GMT")

	return(BT_temp_df)
	
	# close the connection with the remote netCDF file
	nc_close(ncin)
	
	}
	
	yr <- seq(1970, 2020, by=5) 
	
	BT_temp_hind_dat_list <- lapply(yr, hindcast_download_func)
	
	BT_temp_hind_dat <- bind_rows(BT_temp_hind_dat_list)
	
	BT_temp_hind_dat$month <- month(BT_temp_hind_dat$DateTime) # month of year
	BT_temp_hind_dat$week <- week(BT_temp_hind_dat$DateTime) # week of year
	BT_temp_hind_dat$year <- year(BT_temp_hind_dat$DateTime)
	
	
	# get lats/longs from extended grid file
  url_base <- "https://data.pmel.noaa.gov/aclim/thredds/"
  opendap_area  <- "dodsC/ancillary/Bering10K_extended_grid.nc"
  
  nc <- nc_open(paste(url_base, opendap_area, sep=""))
  
  #nc # to see variables
  
  # create objects for known lats and longs and xi and eta axes
  lats <- ncvar_get(nc,"lat_rho")
  longs <- ncvar_get(nc,"lon_rho")
  
  lats_df <- reshape2::melt(lats, varnames = c("Xi", "Eta"), value.name = "latitude")
  longs_df <- reshape2::melt(longs, varnames = c("Xi", "Eta"), value.name = "longitude")
  
  lats_longs <- merge(lats_df, longs_df, by = c("Xi", "Eta"))
  
  BT_temp_hind_dat <- left_join(BT_temp_hind_dat, lats_longs)


	write_csv(BT_temp_hind_dat, file = here("./scripts/for futR/data/generated/BT_temp_hind_dat_K20P19.csv"))
	
	
	#### projection ####
	
	## historical ####
	
	sim <- "B10K-K20P19_CMIP6_"
	time_period <- "_historical"  
	vname1 <- "temp_bottom5m"    
	vname2 <- "temp"            
	tdsbase <- "https://data.pmel.noaa.gov/aclim/thredds/dodsC" 

	# function to download historical 
	projection_download_func <- function(yr, x){

	# set up sim
	sim_form <- paste0(sim, x, time_period)

	# set up filepath
  fname <- file.path(
  	tdsbase,
  	sim_form,
    "Level2",
    paste0(yr, "-", yr + 4), 
    paste0(sim_form, "_", yr, "-", yr + 4, "_average_", vname1, ".nc"))
  
  # read data
  ncin <- nc_open(fname)
  
  # get ocean_time
  ocean_time <- ncvar_get(ncin, "ocean_time")
  
  # get temp
  BT_temp_array <- ncvar_get(ncin, "temp")
  
  # assign dim names and ocean_time
  xi_axis  <- seq(1,182) # Hardcoded axis length, ROMS coordinates
  eta_axis <- seq(1,258) # Hardcoded axis length, ROMS coordinates

  dimnames(BT_temp_array) <- list("Xi" = xi_axis,"Eta" = eta_axis,
															 "Time" = ocean_time)

	# turn array into dataframe 
	BT_temp_df <- reshape2::melt(BT_temp_array, 
														varnames = c("Xi", "Eta", "Time"), 
														value.name = "temp")
	
	# translate to lat/lon and time
	BT_temp_df$DateTime <- as.POSIXct(BT_temp_df$Time, origin = "1900-01-01", tz = "GMT")

	return(BT_temp_df)
	
	# close the connection with the remote netCDF file
	nc_close(ncin)
	
	}
	
	yr <- seq(1980, 2010, by=5) 
	
	GCMs <- c("cesm", "gfdl", "miroc")
	
	GCM_yr <- expand_grid(GCMs, yr)
	
	BT_hist_temp <- mapply(projection_download_func, GCM_yr$yr, GCM_yr$GCMs, 
													SIMPLIFY = FALSE)
	
	BT_hist_temps <- bind_rows(BT_hist_temp)
	
	BT_hist_temps$month <- month(BT_hist_temps$DateTime) # month of year
	BT_hist_temps$week <- week(BT_hist_temps$DateTime) # week of year
	BT_hist_temps$year <- year(BT_hist_temps$DateTime)
	
	
	# get lats/longs from extended grid file
  url_base <- "https://data.pmel.noaa.gov/aclim/thredds/"
  opendap_area  <- "dodsC/ancillary/Bering10K_extended_grid.nc"
  
  nc <- nc_open(paste(url_base, opendap_area, sep=""))
  
  #nc # to see variables
  
  # create objects for known lats and longs and xi and eta axes
  lats <- ncvar_get(nc,"lat_rho")
  longs <- ncvar_get(nc,"lon_rho")
  
  lats_df <- reshape2::melt(lats, varnames = c("Xi", "Eta"), value.name = "latitude")
  longs_df <- reshape2::melt(longs, varnames = c("Xi", "Eta"), value.name = "longitude")
  
  lats_longs <- merge(lats_df, longs_df, by = c("Xi", "Eta"))
  
  BT_hist_temps <- left_join(BT_hist_temps, lats_longs)

	write_csv(BT_hist_temps, file = here("./scripts/for futR/data/generated/BT_historical_temps.csv"))
	
	
	## future ####
	
	## ssp126 ####
	
	sim <- "B10K-K20P19_CMIP6_"
	vname1 <- "temp_surface5m"    
	vname2 <- "temp"            
	tdsbase <- "https://data.pmel.noaa.gov/aclim/thredds/dodsC" 

	# function to download ssp126 (get error of memory exhausted if try to download all three GCM & both 
	# scenarios for each at once)
	
	projection_download_func_ssp126 <- function(yr, x){

	# set up sim
	sim_form <- paste0(sim, x, "_", "ssp126")

	# set up filepath
  fname <- file.path(
  	tdsbase,
  	sim_form,
    "Level2",
    paste0(yr, "-", yr + 4), 
    paste0(sim_form, "_", yr, "-", yr + 4, "_average_", vname1, ".nc"))
  
  # read data
  ncin <- nc_open(fname)
  
  # get ocean_time
  ocean_time <- ncvar_get(ncin, "ocean_time")
  
  # get temp
  BT_temp_array <- ncvar_get(ncin, "temp")
  
  # assign dim names and ocean_time
  xi_axis  <- seq(1,182) # Hardcoded axis length, ROMS coordinates
  eta_axis <- seq(1,258) # Hardcoded axis length, ROMS coordinates

  dimnames(BT_temp_array) <- list("Xi" = xi_axis,"Eta" = eta_axis,
															 "Time" = ocean_time)

	# turn array into dataframe 
	BT_temp_df <- reshape2::melt(BT_temp_array, 
														varnames = c("Xi", "Eta", "Time"), 
														value.name = "temp")
	
	# translate to lat/lon and time
	BT_temp_df$DateTime <- as.POSIXct(BT_temp_df$Time, origin = "1900-01-01", tz = "GMT")

	return(BT_temp_df)
	
	# close the connection with the remote netCDF file
	nc_close(ncin)
	
	}
	
	yr <- seq(2020, 2095, by=5) 
	
	GCMs <- c("cesm", "gfdl", "miroc")
	
	GCM_yr <- expand_grid(GCMs, yr)
	
	BT_proj_temp_ssp126 <- mapply(projection_download_func_ssp126, 
													yr = GCM_yr$yr, 
													x = GCM_yr$GCMs, 
													SIMPLIFY = FALSE)
	
	
	BT_proj_temp_ssp126_df <- bind_rows(BT_proj_temp_ssp126)
	
	BT_proj_temp_ssp126_df$month <- month(BT_proj_temp_ssp126_df$DateTime) # month of year
	BT_proj_temp_ssp126_df$week <- week(BT_proj_temp_ssp126_df$DateTime) # week of year
	BT_proj_temp_ssp126_df$year <- year(BT_proj_temp_ssp126_df$DateTime)
	
	
	# get lats/longs from extended grid file
  url_base <- "https://data.pmel.noaa.gov/aclim/thredds/"
  opendap_area  <- "dodsC/ancillary/Bering10K_extended_grid.nc"
  
  nc <- nc_open(paste(url_base, opendap_area, sep=""))
  
  #nc # to see variables
  
  # create objects for known lats and longs and xi and eta axes
  lats <- ncvar_get(nc,"lat_rho")
  longs <- ncvar_get(nc,"lon_rho")
  
  lats_df <- reshape2::melt(lats, varnames = c("Xi", "Eta"), value.name = "latitude")
  longs_df <- reshape2::melt(longs, varnames = c("Xi", "Eta"), value.name = "longitude")
  
  lats_longs <- merge(lats_df, longs_df, by = c("Xi", "Eta"))
  
  BT_proj_temp_ssp126_df <- left_join(BT_proj_temp_ssp126_df, lats_longs)

	write_csv(BT_proj_temp_ssp126_df, file = here("./data/BT_proj_temp_ssp126_df.csv"))
	
	
	# function to download ssp126 (get error of memory exhausted if try to download all three GCM & both 
	# scenarios for each at once)
	
	projection_download_func_ssp585 <- function(yr, x){

	# set up sim
	sim_form <- paste0(sim, x, "_", "ssp585")

	# set up filepath
  fname <- file.path(
  	tdsbase,
  	sim_form,
    "Level2",
    paste0(yr, "-", yr + 4), 
    paste0(sim_form, "_", yr, "-", yr + 4, "_average_", vname1, ".nc"))
  
  # read data
  ncin <- nc_open(fname)
  
  # get ocean_time
  ocean_time <- ncvar_get(ncin, "ocean_time")
  
  # get temp
  BT_temp_array <- ncvar_get(ncin, "temp")
  
  # assign dim names and ocean_time
  xi_axis  <- seq(1,182) # Hardcoded axis length, ROMS coordinates
  eta_axis <- seq(1,258) # Hardcoded axis length, ROMS coordinates

  dimnames(BT_temp_array) <- list("Xi" = xi_axis,"Eta" = eta_axis,
															 "Time" = ocean_time)

	# turn array into dataframe 
	BT_temp_df <- reshape2::melt(BT_temp_array, 
														varnames = c("Xi", "Eta", "Time"), 
														value.name = "temp")
	
	# translate to lat/lon and time
	BT_temp_df$DateTime <- as.POSIXct(BT_temp_df$Time, origin = "1900-01-01", tz = "GMT")

	return(BT_temp_df)
	
	# close the connection with the remote netCDF file
	nc_close(ncin)
	
	}
	
	yr <- seq(2020, 2095, by=5) 
	
	GCMs <- c("cesm", "gfdl", "miroc")
	
	GCM_yr <- expand_grid(GCMs, yr)
	
	BT_proj_temp_ssp585 <- mapply(projection_download_func_ssp585, 
													yr = GCM_yr$yr, 
													x = GCM_yr$GCMs, 
													SIMPLIFY = FALSE)
	
	
	BT_proj_temp_ssp585_df <- bind_rows(BT_proj_temp_ssp585)
	
	BT_proj_temp_ssp585_df$month <- month(BT_proj_temp_ssp585_df$DateTime) # month of year
	BT_proj_temp_ssp585_df$week <- week(BT_proj_temp_ssp585_df$DateTime) # week of year
	BT_proj_temp_ssp585_df$year <- year(BT_proj_temp_ssp585_df$DateTime)
	
	
	# get lats/longs from extended grid file
  url_base <- "https://data.pmel.noaa.gov/aclim/thredds/"
  opendap_area  <- "dodsC/ancillary/Bering10K_extended_grid.nc"
  
  nc <- nc_open(paste(url_base, opendap_area, sep=""))
  
  #nc # to see variables
  
  # create objects for known lats and longs and xi and eta axes
  lats <- ncvar_get(nc,"lat_rho")
  longs <- ncvar_get(nc,"lon_rho")
  
  lats_df <- reshape2::melt(lats, varnames = c("Xi", "Eta"), value.name = "latitude")
  longs_df <- reshape2::melt(longs, varnames = c("Xi", "Eta"), value.name = "longitude")
  
  lats_longs <- merge(lats_df, longs_df, by = c("Xi", "Eta"))
  
  BT_proj_temp_ssp585_df <- left_join(BT_proj_temp_ssp585_df, lats_longs)

	write_csv(BT_proj_temp_ssp585_df, file = here("./scripts/for futR/data/generated/BT_proj_temp_ssp585_df.csv"))
	
	
	
	

	
	
	

	
	
	
	
	
