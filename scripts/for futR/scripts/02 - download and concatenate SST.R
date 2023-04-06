	# download and transform level 2 hindcast and projection ROMS SST output

	library(ncdf4)
	library(thredds)
	library(reshape2)
	library(here)
	library(data.table)
	library(tidync)
	library(tidyverse)


	#### hindcast temps ####
	
	hcsim <- "B10K-K20P19_CORECFS"  # hindcast
	vname1 <- "temp_surface5m"    # Variable name in filename
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
  SST_temp_array <- ncvar_get(ncin, "temp")
  
  # assign dim names and ocean_time
  xi_axis  <- seq(1,182) # Hardcoded axis length, ROMS coordinates
  eta_axis <- seq(1,258) # Hardcoded axis length, ROMS coordinates

  dimnames(SST_temp_array) <- list("Xi" = xi_axis,"Eta" = eta_axis,
															 "Time" = ocean_time)

	# turn array into dataframe 
	SST_temp_df <- reshape2::melt(SST_temp_array, 
														varnames = c("Xi", "Eta", "Time"), 
														value.name = "temp")
	
	# translate to lat/lon and time
	SST_temp_df$DateTime <- as.POSIXct(SST_temp_df$Time, origin = "1900-01-01", tz = "GMT")

	return(SST_temp_df)
	
	# close the connection with the remote netCDF file
	nc_close(ncin)
	
	}
	
	yr <- seq(1970, 2020, by=5) 
	
	SST_temp_hind_dat_list <- lapply(yr, hindcast_download_func)
	
	SST_temp_hind_dat <- bind_rows(SST_temp_hind_dat_list)
	
	SST_temp_hind_dat$month <- month(SST_temp_hind_dat$DateTime) # month of year
	SST_temp_hind_dat$week <- week(SST_temp_hind_dat$DateTime) # week of year
	SST_temp_hind_dat$year <- year(SST_temp_hind_dat$DateTime)
	
	
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
  
  SST_temp_hind_dat <- left_join(SST_temp_hind_dat, lats_longs)


	write_csv(SST_temp_hind_dat, file = here("./scripts/for futR/data/generated/SST_temp_hind_dat_K20P19.csv"))
	
	
	#### projection ####
	
	## historical ####
	
	sim <- "B10K-K20P19_CMIP6_"
	time_period <- "_historical"  
	vname1 <- "temp_surface5m"    
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
  SST_temp_array <- ncvar_get(ncin, "temp")
  
  # assign dim names and ocean_time
  xi_axis  <- seq(1,182) # Hardcoded axis length, ROMS coordinates
  eta_axis <- seq(1,258) # Hardcoded axis length, ROMS coordinates

  dimnames(SST_temp_array) <- list("Xi" = xi_axis,"Eta" = eta_axis,
															 "Time" = ocean_time)

	# turn array into dataframe 
	SST_temp_df <- reshape2::melt(SST_temp_array, 
														varnames = c("Xi", "Eta", "Time"), 
														value.name = "temp")
	
	# translate to lat/lon and time
	SST_temp_df$DateTime <- as.POSIXct(SST_temp_df$Time, origin = "1900-01-01", tz = "GMT")

	return(SST_temp_df)
	
	# close the connection with the remote netCDF file
	nc_close(ncin)
	
	}
	
	yr <- seq(1980, 2010, by=5) 
	
	GCMs <- c("cesm", "gfdl", "miroc")
	
	GCM_yr <- expand_grid(GCMs, yr)
	
	SST_hist_temp <- mapply(projection_download_func, GCM_yr$yr, GCM_yr$GCMs, 
													SIMPLIFY = FALSE)
	
	SST_hist_temps <- bind_rows(SST_hist_temp)
	
	SST_hist_temps$month <- month(SST_hist_temps$DateTime) # month of year
	SST_hist_temps$week <- week(SST_hist_temps$DateTime) # week of year
	SST_hist_temps$year <- year(SST_hist_temps$DateTime)
	
	
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
  
  SST_hist_temps <- left_join(SST_hist_temps, lats_longs)

	write_csv(SST_hist_temps, file = here("./scripts/for futR/data/generated/SST_historical_temps_K20P19.csv"))
	
	
	## future ####
	
	sim <- "B10K-K20P19_CMIP6_"
	vname1 <- "temp_surface5m"    
	vname2 <- "temp"            
	tdsbase <- "https://data.pmel.noaa.gov/aclim/thredds/dodsC" 

	## ssp126 ####
	
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
  SST_temp_array <- ncvar_get(ncin, "temp")
  
  # assign dim names and ocean_time
  xi_axis  <- seq(1,182) # Hardcoded axis length, ROMS coordinates
  eta_axis <- seq(1,258) # Hardcoded axis length, ROMS coordinates

  dimnames(SST_temp_array) <- list("Xi" = xi_axis,"Eta" = eta_axis,
															 "Time" = ocean_time)

	# turn array into dataframe 
	SST_temp_df <- reshape2::melt(SST_temp_array, 
														varnames = c("Xi", "Eta", "Time"), 
														value.name = "temp")
	
	# translate to lat/lon and time
	SST_temp_df$DateTime <- as.POSIXct(SST_temp_df$Time, origin = "1900-01-01", tz = "GMT")

	return(SST_temp_df)
	
	# close the connection with the remote netCDF file
	nc_close(ncin)
	
	}
	
	yr <- seq(2020, 2095, by=5) 
	
	GCMs <- c("cesm", "gfdl", "miroc")
	
	GCM_yr <- expand_grid(GCMs, yr)
	
	SST_proj_temp_ssp126 <- mapply(projection_download_func_ssp126, 
													yr = GCM_yr$yr, 
													x = GCM_yr$GCMs, 
													SIMPLIFY = FALSE)
	
	
	SST_proj_temp_ssp126_df <- bind_rows(SST_proj_temp_ssp126)
	
	SST_proj_temp_ssp126_df$month <- month(SST_proj_temp_ssp126_df$DateTime) # month of year
	SST_proj_temp_ssp126_df$week <- week(SST_proj_temp_ssp126_df$DateTime) # week of year
	SST_proj_temp_ssp126_df$year <- year(SST_proj_temp_ssp126_df$DateTime)
	
	
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
  
  SST_proj_temp_ssp126_df <- left_join(SST_proj_temp_ssp126_df, lats_longs)

	write_csv(SST_proj_temp_ssp126_df, file = here("./scripts/for futR/data/generated/SST_proj_temp_ssp126_df.csv"))
	
	## ssp585 ####
	
	sim <- "B10K-K20P19_CMIP6_"
	vname1 <- "temp_surface5m"    
	vname2 <- "temp"            
	tdsbase <- "https://data.pmel.noaa.gov/aclim/thredds/dodsC" 

	# function to download ssp585 (get error of memory exhausted if try to download all three GCM & both 
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
  SST_temp_array <- ncvar_get(ncin, "temp")
  
  # assign dim names and ocean_time
  xi_axis  <- seq(1,182) # Hardcoded axis length, ROMS coordinates
  eta_axis <- seq(1,258) # Hardcoded axis length, ROMS coordinates

  dimnames(SST_temp_array) <- list("Xi" = xi_axis,"Eta" = eta_axis,
															 "Time" = ocean_time)

	# turn array into dataframe 
	SST_temp_df <- reshape2::melt(SST_temp_array, 
														varnames = c("Xi", "Eta", "Time"), 
														value.name = "temp")
	
	# translate to lat/lon and time
	SST_temp_df$DateTime <- as.POSIXct(SST_temp_df$Time, origin = "1900-01-01", tz = "GMT")

	return(SST_temp_df)
	
	# close the connection with the remote netCDF file
	nc_close(ncin)
	
	}
	
	yr <- seq(2020, 2095, by=5) 
	
	GCMs <- c("cesm", "gfdl", "miroc")
	
	GCM_yr <- expand_grid(GCMs, yr)
	
	SST_proj_temp_ssp585 <- mapply(projection_download_func_ssp585, 
													yr = GCM_yr$yr, 
													x = GCM_yr$GCMs, 
													SIMPLIFY = FALSE)
	
	
	SST_proj_temp_ssp585_df <- bind_rows(SST_proj_temp_ssp585)
	
	SST_proj_temp_ssp585_df$month <- month(SST_proj_temp_ssp585_df$DateTime) # month of year
	SST_proj_temp_ssp585_df$week <- week(SST_proj_temp_ssp585_df$DateTime) # week of year
	SST_proj_temp_ssp585_df$year <- year(SST_proj_temp_ssp585_df$DateTime)
	
	
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
  
  SST_proj_temp_ssp585_df <- left_join(SST_proj_temp_ssp585_df, lats_longs)

	write_csv(SST_proj_temp_ssp585_df, file = here("./scripts/for futR/data/generated/SST_proj_temp_ssp585_df.csv"))
	
	
	
	

	
	
	

	
	
	
	
	
