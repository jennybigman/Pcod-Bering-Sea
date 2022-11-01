# read in netcdf files of projected temp from Bering 10k ROMS

	library(here)
	library(ncdf4)
	library(tidync)
	require(tidyverse)
	library(data.table)
	library(sf)
	
	## set up lat/lons from area grid file

	## download from server
	#url_base <- "https://data.pmel.noaa.gov/aclim/thredds/"
  #opendap_area  <- "dodsC/extended_grid/Bering10K_extended_grid.nc"
  
  #nc <- nc_open(paste(url_base,opendap_area,sep=""))
  
  ## create objects for known lats and longs and xi and eta axes
  #lats <- ncvar_get(nc,"lat_rho")
  #lons <- ncvar_get(nc,"lon_rho")
 
	#nc_close(nc)
	
	# above isn't working because of the nc file connection, use already-downloaded
	# lat/lons from area dataframe
	

  #### CESM simulations ####
  
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
  
  # below code needed because nc file connection to obtain lat/lons not working
  cesm_hist_dfs <- cesm_hist_dfs %>%
  	mutate(Xi = xi_rho,
  				 Eta = eta_rho) %>% 
  	left_join(area_df, by = c("Xi", "Eta"))
  
  # add in lat/longs matched to xi/eta 
	#cesm_hist_dfs$Lon <- lons[cbind(cesm_hist_dfs$xi_rho, cesm_hist_dfs$eta_rho)]
	#cesm_hist_dfs$Lat <- lats[cbind(cesm_hist_dfs$xi_rho, cesm_hist_dfs$eta_rho)]

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
	#cesm_ssp126_dfs$Lon <- lons[cbind(cesm_ssp126_dfs$xi_rho, cesm_ssp126_dfs$eta_rho)]
	#cesm_ssp126_dfs$Lat <- lats[cbind(cesm_ssp126_dfs$xi_rho, cesm_ssp126_dfs$eta_rho)]
 
  cesm_ssp126_dfs <- cesm_ssp126_dfs %>%
  	mutate(Xi = xi_rho,
  				 Eta = eta_rho) %>% 
  	left_join(area_df, by = c("Xi", "Eta"))

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
	#cesm_ssp585_dfs$Lon <- lons[cbind(cesm_ssp585_dfs$xi_rho, cesm_ssp585_dfs$eta_rho)]
	#cesm_ssp585_dfs$Lat <- lats[cbind(cesm_ssp585_dfs$xi_rho, cesm_ssp585_dfs$eta_rho)]

  cesm_ssp585_dfs <- cesm_ssp585_dfs %>%
  	mutate(Xi = xi_rho,
  				 Eta = eta_rho) %>% 
  	left_join(area_df, by = c("Xi", "Eta"))

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
	
	# remove all months aside from Jan - April
	months <- c(1:4)
	
	cesm_dfs_trim <- cesm_dfs %>%
		filter(., month %in% months) %>%
		mutate(latitude = Lat,
					 longitude = Lon)
	
  fwrite(cesm_dfs_trim, "./data/cesm_dfs_trim.csv")
	
	
	#### GFDL simulations ####
  
  # read in files
  gfdl_file_list <- list.files(path = paste0(here(), ("/data/CMIP6_bottom_temp/gfdl")))
  
  prestring <- paste0(here(), ("/data/CMIP6_bottom_temp/gfdl/"))
  
  # historical baseline period ####
	gfdl_historical_baseline_file_list <- gfdl_file_list[str_detect(gfdl_file_list, "historical")]
	
	prestring <- paste0(here(), ("/data/CMIP6_bottom_temp/gfdl/"))
  
  gfdl_hist_dat_list <- list()
  
  for(i in gfdl_historical_baseline_file_list){
  	gfdl_hist_dat_list[[i]] <- paste0(prestring, i)
  	gfdl_hist_dat_list
  }

	gfdl_hist_df_list <- list()
  	for(i in gfdl_hist_dat_list){
  		gfdl_hist_df_list[[i]] <- tidync(i) %>% hyper_tibble(select_var = "temp")
  		gfdl_hist_df_list
  }
  
  gfdl_hist_dfs <- bind_rows(gfdl_hist_df_list)
  
  # add in lat/longs matched to xi/eta 
	gfdl_hist_dfs$Lon <- lons[cbind(gfdl_hist_dfs$xi_rho, gfdl_hist_dfs$eta_rho)]
	gfdl_hist_dfs$Lat <- lats[cbind(gfdl_hist_dfs$xi_rho, gfdl_hist_dfs$eta_rho)]

	# create object for time axis
	gfdl_hist_dfs$DateTime <- as.POSIXct(gfdl_hist_dfs$ocean_time, origin = "1900-01-01", tz = "GMT")
  
  # ssp 126 scenario ####
	
	gfdl_ssp126_file_list <- gfdl_file_list[str_detect(gfdl_file_list, "ssp126")]
	
	prestring <- paste0(here(), ("/data/CMIP6_bottom_temp/gfdl/"))
  
  gfdl_ssp126_dat_list <- list()
  
  for(i in gfdl_ssp126_file_list){
  	gfdl_ssp126_dat_list[[i]] <- paste0(prestring, i)
  	gfdl_ssp126_dat_list
  }

	gfdl_ssp126_df_list <- list()
  	for(i in gfdl_ssp126_dat_list){
  		gfdl_ssp126_df_list[[i]] <- tidync(i) %>% hyper_tibble(select_var = "temp")
  		gfdl_ssp126_df_list
  }
  
  gfdl_ssp126_dfs <- bind_rows(gfdl_ssp126_df_list)
  
  # add in lat/longs matched to xi/eta 
	gfdl_ssp126_dfs$Lon <- lons[cbind(gfdl_ssp126_dfs$xi_rho, gfdl_ssp126_dfs$eta_rho)]
	gfdl_ssp126_dfs$Lat <- lats[cbind(gfdl_ssp126_dfs$xi_rho, gfdl_ssp126_dfs$eta_rho)]

	# create object for time axis
	gfdl_ssp126_dfs$DateTime <- as.POSIXct(gfdl_ssp126_dfs$ocean_time, origin = "1900-01-01", tz = "GMT")

 # ssp585 scenario ####
	
	gfdl_ssp585_file_list <- gfdl_file_list[str_detect(gfdl_file_list, "ssp585")]

	prestring <- paste0(here(), ("/data/CMIP6_bottom_temp/gfdl/"))
  
  gfdl_ssp585_dat_list <- list()
  
  for(i in gfdl_ssp585_file_list){
  	gfdl_ssp585_dat_list[[i]] <- paste0(prestring, i)
  	gfdl_ssp585_dat_list
  }

	gfdl_ssp585_df_list <- list()
  	for(i in gfdl_ssp585_dat_list){
  		gfdl_ssp585_df_list[[i]] <- tidync(i) %>% hyper_tibble(select_var = "temp")
  		gfdl_ssp585_df_list
  }
  
  gfdl_ssp585_dfs <- bind_rows(gfdl_ssp585_df_list)
  
  # add in lat/longs matched to xi/eta 
	gfdl_ssp585_dfs$Lon <- lons[cbind(gfdl_ssp585_dfs$xi_rho, gfdl_ssp585_dfs$eta_rho)]
	gfdl_ssp585_dfs$Lat <- lats[cbind(gfdl_ssp585_dfs$xi_rho, gfdl_ssp585_dfs$eta_rho)]

	# create object for time axis
	gfdl_ssp585_dfs$DateTime <- as.POSIXct(gfdl_ssp585_dfs$ocean_time, origin = "1900-01-01", tz = "GMT")
	
	# join together
	gfdl_ssp126_dfs$scenario <- "ssp126"
	gfdl_ssp585_dfs$scenario <- "ssp585"
	gfdl_hist_dfs$scenario <- "historical"
	
	gfdl_dfs <- bind_rows(gfdl_hist_dfs, gfdl_ssp126_dfs, gfdl_ssp585_dfs)
	
	# separate date column into components
	gfdl_dfs$date <- as.Date(gfdl_dfs$DateTime) # date in Date format
	gfdl_dfs$month <- month(gfdl_dfs$date) # month of year
	gfdl_dfs$week <- week(gfdl_dfs$date) # week of year
	gfdl_dfs$year <- year(gfdl_dfs$date)
	
	# remove all months aside from Jan - April
	months <- c(1:4)
	
	gfdl_dfs_trim <- gfdl_dfs %>%
		filter(., month %in% months)
	
	fwrite(gfdl_dfs_trim, "./data/gfdl_dfs_trim.csv")

  
	#### MIROC simulations ####

  # read in files
  miroc_file_list <- list.files(path = paste0(here(), ("/data/CMIP6_bottom_temp/miroc")))
  
  prestring <- paste0(here(), ("/data/CMIP6_bottom_temp/miroc/"))
  
  # historical baseline period ####
	miroc_historical_baseline_file_list <- miroc_file_list[str_detect(miroc_file_list, "historical")]
	
	prestring <- paste0(here(), ("/data/CMIP6_bottom_temp/miroc/"))
  
  miroc_hist_dat_list <- list()
  
  for(i in miroc_historical_baseline_file_list){
  	miroc_hist_dat_list[[i]] <- paste0(prestring, i)
  	miroc_hist_dat_list
  }

	miroc_hist_df_list <- list()
  	for(i in miroc_hist_dat_list){
  		miroc_hist_df_list[[i]] <- tidync(i) %>% hyper_tibble(select_var = "temp")
  		miroc_hist_df_list
  }
  
  miroc_hist_dfs <- bind_rows(miroc_hist_df_list)
  
  # add in lat/longs matched to xi/eta 
	miroc_hist_dfs$Lon <- lons[cbind(miroc_hist_dfs$xi_rho, miroc_hist_dfs$eta_rho)]
	miroc_hist_dfs$Lat <- lats[cbind(miroc_hist_dfs$xi_rho, miroc_hist_dfs$eta_rho)]

	# create object for time axis
	miroc_hist_dfs$DateTime <- as.POSIXct(miroc_hist_dfs$ocean_time, origin = "1900-01-01", tz = "GMT")
  
  # ssp 126 scenario ####
	
	miroc_ssp126_file_list <- miroc_file_list[str_detect(miroc_file_list, "ssp126")]
	
	prestring <- paste0(here(), ("/data/CMIP6_bottom_temp/miroc/"))
  
  miroc_ssp126_dat_list <- list()
  
  for(i in miroc_ssp126_file_list){
  	miroc_ssp126_dat_list[[i]] <- paste0(prestring, i)
  	miroc_ssp126_dat_list
  }

	miroc_ssp126_df_list <- list()
  	for(i in miroc_ssp126_dat_list){
  		miroc_ssp126_df_list[[i]] <- tidync(i) %>% hyper_tibble(select_var = "temp")
  		miroc_ssp126_df_list
  }
  
  miroc_ssp126_dfs <- bind_rows(miroc_ssp126_df_list)
  
  # add in lat/longs matched to xi/eta 
	miroc_ssp126_dfs$Lon <- lons[cbind(miroc_ssp126_dfs$xi_rho, miroc_ssp126_dfs$eta_rho)]
	miroc_ssp126_dfs$Lat <- lats[cbind(miroc_ssp126_dfs$xi_rho, miroc_ssp126_dfs$eta_rho)]

	# create object for time axis
	miroc_ssp126_dfs$DateTime <- as.POSIXct(miroc_ssp126_dfs$ocean_time, origin = "1900-01-01", tz = "GMT")

 # ssp585 scenario ####
	
	miroc_ssp585_file_list <- miroc_file_list[str_detect(miroc_file_list, "ssp585")]

	prestring <- paste0(here(), ("/data/CMIP6_bottom_temp/miroc/"))
  
  miroc_ssp585_dat_list <- list()
  
  for(i in miroc_ssp585_file_list){
  	miroc_ssp585_dat_list[[i]] <- paste0(prestring, i)
  	miroc_ssp585_dat_list
  }

	miroc_ssp585_df_list <- list()
  	for(i in miroc_ssp585_dat_list){
  		miroc_ssp585_df_list[[i]] <- tidync(i) %>% hyper_tibble(select_var = "temp")
  		miroc_ssp585_df_list
  }
  
  miroc_ssp585_dfs <- bind_rows(miroc_ssp585_df_list)
  
  # add in lat/longs matched to xi/eta 
	miroc_ssp585_dfs$Lon <- lons[cbind(miroc_ssp585_dfs$xi_rho, miroc_ssp585_dfs$eta_rho)]
	miroc_ssp585_dfs$Lat <- lats[cbind(miroc_ssp585_dfs$xi_rho, miroc_ssp585_dfs$eta_rho)]

	# create object for time axis
	miroc_ssp585_dfs$DateTime <- as.POSIXct(miroc_ssp585_dfs$ocean_time, origin = "1900-01-01", tz = "GMT")
	
	# join together
	miroc_ssp126_dfs$scenario <- "ssp126"
	miroc_ssp585_dfs$scenario <- "ssp585"
	miroc_hist_dfs$scenario <- "historical"
	
	miroc_dfs <- bind_rows(miroc_hist_dfs, miroc_ssp126_dfs, miroc_ssp585_dfs)


	# separate date column into components
	miroc_dfs$date <- as.Date(miroc_dfs$DateTime) # date in Date format
	miroc_dfs$month <- month(miroc_dfs$date) # month of year
	miroc_dfs$week <- week(miroc_dfs$date) # week of year
	miroc_dfs$year <- year(miroc_dfs$date)
	
	# remove all months aside from Jan - April
	months <- c(1:4)
	
	miroc_dfs_trim <- miroc_dfs %>%
		filter(., month %in% months)
	
	fwrite(miroc_dfs_trim, "./data/miroc_dfs_trim.csv")
	
  	
  