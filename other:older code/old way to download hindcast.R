#--------------------
	# Read hindcast data --- old way
	#--------------------


# download bottom temp data -- OLD WAY

	#library(ncdf4)
	#library(thredds)
	#library(reshape2)
	#library(here)
	#library(data.table)
	#library(tidync)
#
#
	## set up download from server 
	#url_base <- "https://data.pmel.noaa.gov/aclim/thredds/"
	#opendap  <- "dodsC/Level2/B10K-K20_CORECFS_bottom5m.nc"
	#
	## test_path <- paste0(url_base, opendap)
	#
	## tidy_temps <- tidync(test_path) %>% hyper_tibble()
	#
	#nc <- nc_open(paste(url_base, opendap, sep = ""))
#
	## create objects for known lats and longs and xi and eta axes
  #lats <- ncvar_get(nc,"lat_rho")
  #lons <- ncvar_get(nc,"lon_rho")
  #
  #xi_axis  <- seq(1,182) # Hardcoded axis length, ROMS coordinates
  #eta_axis <- seq(1,258) # Hardcoded axis length, ROMS coordinates
#
	## create object for time axis
	#t_axis   <- ncvar_get(nc,"ocean_time")
	#time_axis <- as.POSIXct(t_axis, origin = "1900-01-01", tz = "GMT") 
#
	## download temp array -- for some reason, only works without last two time steps
	#temp_array <- ncvar_get(nc, "temp", start = c(1,1,1), count = c(182,258,2662)) 
#
  ##temp_array<- ncvar_get(nc, "temp") doesn't work
#
  # name the dimensions
	#dim(temp_array)
#
	#dimnames(temp_array) <- list("Xi" = xi_axis,"Eta" = eta_axis,
	#														 "Time" = t_axis[1:2662])
#
	## turn array into dataframe 
	#temp_df <- reshape2::melt(temp_array, 
	#													varnames = c("Xi", "Eta", "Time"), 
	#													value.name = "temp")
#
	## translate to lat/lon and time
	#temp_df$DateTime <- as.POSIXct(temp_df$Time, origin = "1900-01-01", tz = "GMT")
	#temp_df$longitude <- lons[cbind(temp_df$Xi, temp_df$Eta)]
	#temp_df$latitude <- lats[cbind(temp_df$Xi, temp_df$Eta)]
#
	## close the connection with the remote netCDF file
	#nc_close(nc)
#
	## save temp df
	#fwrite(temp_df, "./data/ROMS_all_temp.csv")
#

	
	