# read in netcdf files of projected temp from Bering 10k ROMS

	library(here)
	library(ncdf4)
	library(reshape2)
	library(tidync)
	require(tidyverse)
	
	# set up lat/lons from area grid file

	# download from server
	url_base <- "https://data.pmel.noaa.gov/aclim/thredds/"
  opendap_area  <- "dodsC/extended_grid/Bering10K_extended_grid.nc"
  
  nc <- nc_open(paste(url_base,opendap_area,sep=""))
  
  # create objects for known lats and longs and xi and eta axes
  lats <- ncvar_get(nc,"lat_rho")
  lons <- ncvar_get(nc,"lon_rho")
 

  # CESM simulations ####
  
  # read in files
  cesm_file_list <- list.files(path = paste0(here(), ("/data/CMIP6_bottom_temp/cesm")))
  
  prestring <- paste0(here(), ("/data/CMIP6_bottom_temp/cesm/"))
  
  cesm_dat_list <- list()
  
  for(i in cesm_file_list){
  	cesm_dat_list[[i]] <- paste0(prestring, i)
  	cesm_dat_list
  }

	cesm_df_list <- list()
  	for(i in cesm_dat_list){
  		cesm_df_list[[i]] <- tidync(i) %>% hyper_tibble(select_var = "temp")
  		cesm_df_list
  }
  
  cesm_dfs <- bind_rows(cesm_df_list)
  
  # add in lat/longs matched to xi/eta 
	cesm_dfs$Lon <- lons[cbind(cesm_dfs$xi_rho, cesm_dfs$eta_rho)]
	cesm_dfs$Lat <- lats[cbind(cesm_dfs$xi_rho, cesm_dfs$eta_rho)]

	# create object for time axis
	cesm_dfs$DateTime <- as.POSIXct(cesm_dfs$ocean_time, origin = "1900-01-01", tz = "GMT")


	# GFDL simulations ####
  
  # read in files
  gfdl_file_list <- list.files(path = paste0(here(), ("/data/CMIP6_bottom_temp/gfdl")))
  
  prestring <- paste0(here(), ("/data/CMIP6_bottom_temp/gfdl/"))
  
  gfdl_dat_list <- list()
  
  for(i in gfdl_file_list){
  	gfdl_dat_list[[i]] <- paste0(prestring, i)
  	gfdl_dat_list
  }

	gfdl_df_list <- list()
  	for(i in gfdl_dat_list){
  		gfdl_df_list[[i]] <- tidync(i) %>% hyper_tibble(select_var = "temp")
  		gfdl_df_list
  }
  
  gfdl_dfs <- bind_rows(gfdl_df_list)
  
  # add in lat/longs matched to xi/eta 
	gfdl_dfs$Lon <- lons[cbind(gfdl_dfs$xi_rho, gfdl_dfs$eta_rho)]
	gfdl_dfs$Lat <- lats[cbind(gfdl_dfs$xi_rho, gfdl_dfs$eta_rho)]

	# create object for time axis
	gfdl_dfs$DateTime <- as.POSIXct(gfdl_dfs$ocean_time, origin = "1900-01-01", tz = "GMT")
	
	
	# MIROC simulations ####
  
  # read in files
  miroc_file_list <- list.files(path = paste0(here(), ("/data/CMIP6_bottom_temp/miroc")))
  
  prestring <- paste0(here(), ("/data/CMIP6_bottom_temp/miroc/"))
  
  miroc_dat_list <- list()
  
  for(i in miroc_file_list){
  	miroc_dat_list[[i]] <- paste0(prestring, i)
  	miroc_dat_list
  }

	miroc_df_list <- list()
  	for(i in miroc_dat_list){
  		miroc_df_list[[i]] <- tidync(i) %>% hyper_tibble(select_var = "temp")
  		miroc_df_list
  }
  
  miroc_dfs <- bind_rows(miroc_df_list)
  
  # add in lat/longs matched to xi/eta 
	miroc_dfs$Lon <- lons[cbind(miroc_dfs$xi_rho, miroc_dfs$eta_rho)]
	miroc_dfs$Lat <- lats[cbind(miroc_dfs$xi_rho, miroc_dfs$eta_rho)]

	# create object for time axis
	miroc_dfs$DateTime <- as.POSIXct(miroc_dfs$ocean_time, origin = "1900-01-01", tz = "GMT")



