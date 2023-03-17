	# download and transform level 2 hindcast output

	library(ncdf4)
	library(thredds)
	library(reshape2)
	library(here)
	library(data.table)
	library(tidync)
	library(tidyverse)


	#### hindcast temps ####
	
	hcsim <- "B10K-K20_CORECFS"  # hindcast
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
  temp_array <- ncvar_get(ncin, "temp")
  
  # assign dim names and ocean_time
  xi_axis  <- seq(1,182) # Hardcoded axis length, ROMS coordinates
  eta_axis <- seq(1,258) # Hardcoded axis length, ROMS coordinates

  dimnames(temp_array) <- list("Xi" = xi_axis,"Eta" = eta_axis,
															 "Time" = ocean_time)

	# turn array into dataframe 
	temp_df <- reshape2::melt(temp_array, 
														varnames = c("Xi", "Eta", "Time"), 
														value.name = "temp")
	
	# translate to lat/lon and time
	temp_df$DateTime <- as.POSIXct(temp_df$Time, origin = "1900-01-01", tz = "GMT")

	return(temp_df)
	
	# close the connection with the remote netCDF file
	nc_close(ncin)
	
	}
	
	yr <- seq(1970, 2020, by=5) 
	
	temp_hind_dat_list <- lapply(yr, hindcast_download_func)
	
	temp_hind_dat <- bind_rows(temp_hind_dat_list)
	
	temp_hind_dat$month <- month(temp_hind_dat$DateTime) # month of year
	temp_hind_dat$week <- week(temp_hind_dat$DateTime) # week of year
	temp_hind_dat$year <- year(temp_hind_dat$DateTime)
	
	
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
  
  temp_hind_dat <- left_join(temp_hind_dat, lats_longs)


	write_csv(temp_hind_dat, file = here("./data/hindcast_temp_K20.csv"))
	
	
	# what about the K20P19 version?
	
	# download and transform level 2 hindcast output

	library(ncdf4)
	library(thredds)
	library(reshape2)
	library(here)
	library(data.table)
	library(tidync)


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
  
  # get lats, longs, ocean_time
  lats <- ncvar_get(ncin,"lat_rho")
  lons <- ncvar_get(ncin,"lon_rho")
  ocean_time <- ncvar_get(ncin, "ocean_time")
  
  # get temp
  temp_array2 <- ncvar_get(ncin, "temp")
  
  # assign dim names and ocean_time
  xi_axis  <- seq(1,182) # Hardcoded axis length, ROMS coordinates
  eta_axis <- seq(1,258) # Hardcoded axis length, ROMS coordinates

  dimnames(temp_array2) <- list("Xi" = xi_axis,"Eta" = eta_axis,
															 "Time" = ocean_time)

	# turn array into dataframe 
	temp_df2 <- reshape2::melt(temp_array2, 
														varnames = c("Xi", "Eta", "Time"), 
														value.name = "temp")

	# translate to lat/lon and time
	temp_df2$DateTime <- as.POSIXct(temp_df2$Time, origin = "1900-01-01", tz = "GMT")
	temp_df2$longitude <- lons[cbind(temp_df2$Xi, temp_df2$Eta)]
	temp_df2$latitude <- lats[cbind(temp_df2$Xi, temp_df2$Eta)]
	
	return(temp_df2)
	
	# close the connection with the remote netCDF file
	nc_close(ncin)
	
	}
	
	yr <- seq(1970, 2020, by=5) 
	
	temp_hind_dat_list2 <- lapply(yr, hindcast_download_func)
	
	temp_hind_dat2 <- bind_rows(temp_hind_dat_list2)
	
	temp_hind_dat2$month <- month(temp_hind_dat2$DateTime) # month of year
	temp_hind_dat2$week <- week(temp_hind_dat2$DateTime) # week of year
	temp_hind_dat2$year <- year(temp_hind_dat2$DateTime)
	
	

	write_csv(temp_hind_dat2, file = here("./data/hindcast_temp_K20P19.csv"))
	
	
	