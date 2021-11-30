# read in netcdf files of projected temp from Bering 10k ROMS

	library(here)
	library(ncdf4)
	library(tidync)
	require(tidyverse)
	library(data.table)
	
	## read in data so can skip below
	
	cesm_dfs_trim <- fread(file = here("data", "cesm_dfs_trim.csv"))
	gfdl_dfs_trim <- fread(file = here("data", "gfdl_dfs_trim.csv"))
	miroc_dfs_trim <- fread(file = here("data", "miroc_dfs_trim.csv"))

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
 
  # ssp 126 projection ####
	
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

 # ssp585 projection ####
	
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
	cesm_ssp126_dfs$projection <- "ssp126"
	cesm_ssp585_dfs$projection <- "ssp585"
	
	cesm_dfs <- bind_rows(cesm_ssp126_dfs, cesm_ssp585_dfs)

	# separate date column into components
	cesm_dfs$date <- as.Date(cesm_dfs$DateTime) # date in Date format
	cesm_dfs$month <- month(cesm_dfs$date) # month of year
	cesm_dfs$week <- week(cesm_dfs$date) # week of year
	cesm_dfs$year <- year(cesm_dfs$date)
	
	# remove all months aside from Jan - April
	months <- c(1:4)
	
	cesm_dfs_trim <- cesm_dfs %>%
		filter(., month %in% months)
	
	# write to file
	fwrite(cesm_dfs_trim, "./data/cesm_dfs_trim.csv")

	
  # summarize by year
  cesm_dfs_trim_sum <- cesm_dfs_trim %>%
		group_by(projection, Lat, Lon, year) %>%
		summarise(mean_temp = mean(temp))
 
  # convert to sf object
  cesm_dfs_trim_sum_sf <- cesm_dfs_trim_sum %>% 
			mutate(latitude = Lat,
				long_not_360 = case_when(
					Lon >= 180 ~ Lon - 360,
					Lon < 180 ~ Lon)) %>%
  		st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326)
  		
  # plots ####
  
  # yearly plot
  cesm_yr_plot_func <- function(x){
		
				new_dat <- cesm_dfs_trim_sum_sf %>% filter(., year == x)
    
    	  plot <- 
    	  	ggplot() +
					geom_sf(data = new_dat, aes(color = mean_temp))  +
					geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
  				facet_wrap(~ projection) +
					coord_sf(crs = 3338) +
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
 						axis.text = element_text(size = 12),	
  					axis.title = element_text(size = 14),
  					legend.title.align=0.5)
    	  
    	  plot
    	  
	}
	
	years <- unique(cesm_dfs_trim_sum_sf$year)
	
	yr_plot_list <- lapply(years, cesm_yr_plot_func)
  
  yr_name_func_year <- function(x){
  	year_name <- paste0(x, "_cesm_yr_btemp.png")
  }
   
  names_year <- sapply(years, yr_name_func_year)
  
	yr_name_func_file <- function(x){
  	year_file_name <- paste0("/Users/jenniferbigman/My Drive/NOAA AFSC Postdoc/Pcod Bering Sea Habitat Suitability/Pcod-Bering-Sea/output/plots/yearly plots/projected temps/cesm plots/", x)
  }
   
  yearly_names <- sapply(names_year, yr_name_func_file)

	plot_list <- mapply(ggsave_func, x = yr_plot_list, y = yearly_names)

	
	# plot: summary by decade

	cesm_dfs_trim_decade_sf <- cesm_dfs_trim_sum_sf %>%
		mutate(decade = case_when(
			between(year, 1980, 1989) ~ "1980s",
			between(year, 1990, 1999) ~ "1990s",
			between(year, 2000, 2009) ~ "2000s",
			between(year, 2010, 2019) ~ "2010s",
			between(year, 2020, 2029) ~ "2020s",
			between(year, 2030, 2039) ~ "2030s",
			between(year, 2040, 2049) ~ "2040s",
			between(year, 2050, 2059) ~ "2050s",
			between(year, 2060, 2069) ~ "2060s",
			between(year, 2070, 2079) ~ "2070s",
			between(year, 2080, 2089) ~ "2080s",
			between(year, 2090, 2099) ~ "2090s"))
	
	plot_cesm_decade <- 
  		ggplot() +
					geom_sf(data = cesm_dfs_trim_decade_sf, 
									aes(color = mean_temp))  +
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
  	
		ggsave("./output/plots/plot_cesm_decade.png",
			plot_cesm_decade,
			width = 15, height = 10, units = "in")
	
		

	
	# GFDL simulations ####
  
  # read in files
  gfdl_file_list <- list.files(path = paste0(here(), ("/data/CMIP6_bottom_temp/gfdl")))
  
  prestring <- paste0(here(), ("/data/CMIP6_bottom_temp/gfdl/"))
  
  # ssp 126 projection ####
	
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

 # ssp585 projection ####
	
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
	gfdl_ssp126_dfs$projection <- "ssp126"
	gfdl_ssp585_dfs$projection <- "ssp585"
	
	gfdl_dfs <- bind_rows(gfdl_ssp126_dfs, gfdl_ssp585_dfs)

  # add in lat/longs matched to xi/eta 
	gfdl_dfs$Lon <- lons[cbind(gfdl_dfs$xi_rho, gfdl_dfs$eta_rho)]
	gfdl_dfs$Lat <- lats[cbind(gfdl_dfs$xi_rho, gfdl_dfs$eta_rho)]

	# create object for time axis
	gfdl_dfs$DateTime <- as.POSIXct(gfdl_dfs$ocean_time, origin = "1900-01-01", tz = "GMT")
	

	# separate date column into components
	gfdl_dfs$date <- as.Date(gfdl_dfs$DateTime) # date in Date format
	gfdl_dfs$month <- month(gfdl_dfs$date) # month of year
	gfdl_dfs$week <- week(gfdl_dfs$date) # week of year
	gfdl_dfs$year <- year(gfdl_dfs$date)
	
	# remove all months aside from Jan - April
	months <- c(1:4)
	
	gfdl_dfs_trim <- gfdl_dfs %>%
		filter(., month %in% months)
	
	# write to file
	fwrite(gfdl_dfs_trim, "./data/gfdl_dfs_trim.csv")

  # summarize by year
  gfdl_dfs_trim_sum <- gfdl_dfs_trim %>%
		group_by(projection, Lat, Lon, year) %>%
		summarise(mean_temp = mean(temp))

  # convert to sf object
  gfdl_dfs_trim_sum_sf <- gfdl_dfs_trim_sum %>% 
			mutate(latitude = Lat,
				long_not_360 = case_when(
					Lon >= 180 ~ Lon - 360,
					Lon < 180 ~ Lon)) %>%
  		st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326)
  	
  	
  # plots ####
  
  # yearly plots
  
  gfdl_yr_plot_func <- function(x){
		
				new_dat <- gfdl_dfs_trim_sum_sf %>% filter(., year == x)
    
    	  plot <- 
    	  	ggplot() +
					geom_sf(data = new_dat, aes(color = mean_temp))  +
					geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
    	  	facet_grid(~ projection) +
					coord_sf(crs = 3338) +
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
 						axis.text = element_text(size = 12),	
  					axis.title = element_text(size = 14),
  					legend.title.align=0.5)
    	  
    	  plot
    	  
	}
	
	years <- unique(gfdl_dfs_trim_sum_sf$year)
	
	yr_plot_list <- lapply(years, gfdl_yr_plot_func)
  
  yr_name_func_year <- function(x){
  	year_name <- paste0(x, "_gfdl_yr_btemp.png")
  }
   
  names_year <- sapply(years, yr_name_func_year)
  
	yr_name_func_file <- function(x){
  	year_file_name <- paste0("/Users/jenniferbigman/My Drive/NOAA AFSC Postdoc/Pcod Bering Sea Habitat Suitability/Pcod-Bering-Sea/output/plots/yearly plots/projected temps/gfdl plots/", x)
  }
   
  yearly_names <- sapply(names_year, yr_name_func_file)

	plot_list <- mapply(ggsave_func, x = yr_plot_list, y = yearly_names)

	# plot: summary by decade

	gfdl_dfs_trim_sum_decade_sf <- gfdl_dfs_trim_sum_sf %>%
		mutate(decade = case_when(
			between(year, 1980, 1989) ~ "1980s",
			between(year, 1990, 1999) ~ "1990s",
			between(year, 2000, 2009) ~ "2000s",
			between(year, 2010, 2019) ~ "2010s",
			between(year, 2020, 2029) ~ "2020s",
			between(year, 2030, 2039) ~ "2030s",
			between(year, 2040, 2049) ~ "2040s",
			between(year, 2050, 2059) ~ "2050s",
			between(year, 2060, 2069) ~ "2060s",
			between(year, 2070, 2079) ~ "2070s",
			between(year, 2080, 2089) ~ "2080s",
			between(year, 2090, 2099) ~ "2090s"))
	
	plot_gfdl_decade <- 
  		ggplot() +
					geom_sf(data = gfdl_dfs_trim_sum_decade_sf, 
									aes(color = mean_temp))  +
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
  	
		ggsave("./output/plots/plot_gfdl_decade.png",
			plot_gfdl_decade,
			width = 15, height = 10, units = "in")
	
	
	# MIROC simulations ####

  # read in files
  miroc_file_list <- list.files(path = paste0(here(), ("/data/CMIP6_bottom_temp/miroc")))
  
  prestring <- paste0(here(), ("/data/CMIP6_bottom_temp/miroc/"))
  
  # ssp 126 projection ####
	
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

 # ssp585 projection ####
	
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
	miroc_ssp126_dfs$projection <- "ssp126"
	miroc_ssp585_dfs$projection <- "ssp585"
	
	miroc_dfs <- bind_rows(miroc_ssp126_dfs, miroc_ssp585_dfs)

  # add in lat/longs matched to xi/eta 
	miroc_dfs$Lon <- lons[cbind(miroc_dfs$xi_rho, miroc_dfs$eta_rho)]
	miroc_dfs$Lat <- lats[cbind(miroc_dfs$xi_rho, miroc_dfs$eta_rho)]

	# create object for time axis
	miroc_dfs$DateTime <- as.POSIXct(miroc_dfs$ocean_time, origin = "1900-01-01", tz = "GMT")
	

	# separate date column into components
	miroc_dfs$date <- as.Date(miroc_dfs$DateTime) # date in Date format
	miroc_dfs$month <- month(miroc_dfs$date) # month of year
	miroc_dfs$week <- week(miroc_dfs$date) # week of year
	miroc_dfs$year <- year(miroc_dfs$date)
	
	# remove all months aside from Jan - April
	months <- c(1:4)
	
	miroc_dfs_trim <- miroc_dfs %>%
		filter(., month %in% months)
	
	# write to file
	fwrite(miroc_dfs_trim, "./data/miroc_dfs_trim.csv")

  # summarize by year
  miroc_dfs_trim_sum <- miroc_dfs_trim %>%
		group_by(projection, Lat, Lon, year) %>%
		summarise(mean_temp = mean(temp))

  # convert to sf object
  miroc_dfs_trim_sum_sf <- miroc_dfs_trim_sum %>% 
			mutate(latitude = Lat,
				long_not_360 = case_when(
					Lon >= 180 ~ Lon - 360,
					Lon < 180 ~ Lon)) %>%
  		st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326)
  	
  	
  # plots ####
  
  # yearly plots
  
  miroc_yr_plot_func <- function(x){
		
				new_dat <- miroc_dfs_trim_sum_sf %>% filter(., year == x)
    
    	  plot <- 
    	  	ggplot() +
					geom_sf(data = new_dat, aes(color = mean_temp))  +
					geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
    	  	facet_grid(~ projection) +
					coord_sf(crs = 3338) +
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
 						axis.text = element_text(size = 12),	
  					axis.title = element_text(size = 14),
  					legend.title.align=0.5)
    	  
    	  plot
    	  
	}
	
	years <- unique(miroc_dfs_trim_sum_sf$year)
	
	yr_plot_list <- lapply(years, miroc_yr_plot_func)
  
  yr_name_func_year <- function(x){
  	year_name <- paste0(x, "_miroc_yr_btemp.png")
  }
   
  names_year <- sapply(years, yr_name_func_year)
  
	yr_name_func_file <- function(x){
  	year_file_name <- paste0("/Users/jenniferbigman/My Drive/NOAA AFSC Postdoc/Pcod Bering Sea Habitat Suitability/Pcod-Bering-Sea/output/plots/yearly plots/projected temps/miroc plots/", x)
  }
   
  yearly_names <- sapply(names_year, yr_name_func_file)

	plot_list <- mapply(ggsave_func, x = yr_plot_list, y = yearly_names)

	# plot: summary by decade

	miroc_dfs_trim_sum_decade_sf <- miroc_dfs_trim_sum_sf %>%
		mutate(decade = case_when(
			between(year, 1980, 1989) ~ "1980s",
			between(year, 1990, 1999) ~ "1990s",
			between(year, 2000, 2009) ~ "2000s",
			between(year, 2010, 2019) ~ "2010s",
			between(year, 2020, 2029) ~ "2020s",
			between(year, 2030, 2039) ~ "2030s",
			between(year, 2040, 2049) ~ "2040s",
			between(year, 2050, 2059) ~ "2050s",
			between(year, 2060, 2069) ~ "2060s",
			between(year, 2070, 2079) ~ "2070s",
			between(year, 2080, 2089) ~ "2080s",
			between(year, 2090, 2099) ~ "2090s"))
	
	plot_miroc_decade <- 
  		ggplot() +
					geom_sf(data = miroc_dfs_trim_sum_decade_sf, 
									aes(color = mean_temp))  +
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
  	
		ggsave("./output/plots/plot_miroc_decade.png",
			plot_miroc_decade,
			width = 15, height = 10, units = "in")