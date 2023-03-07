# bias correcting domian level but all year

# bias-correcting temp values at the weekly/grid cell level

	
	library(here)
	library(ncdf4)
	library(tidync)
	require(tidyverse)
	library(data.table)
	library(sf)
	
	# set up meta data (lat/lon, domain, depth)
	depth_df <- fread("./data/ROMS_depth_df.csv") 
	domain_df <- fread("./data/ROMS_domain_df.csv") 

	domain_depth_df <- merge(domain_df, depth_df, by = c("latitude", "longitude", "Xi", "Eta")) %>%
		mutate(xi_rho = Xi,
					 eta_rho = Eta)

	# load hindcast and add meta data, trim to grid and add dates
	temp_df <- fread("./data/ROMS_all_temp.csv")
	
	temp_df <- merge(temp_df, domain_depth_df, 
												 by = c("latitude", "longitude", "Xi", "Eta"))
	
	temp_df_trim <- temp_df %>%
		filter(domain > 0) %>%
		filter(., between(depth, 0, 250)) %>%
		drop_na(temp)
	
	temp_df_trim$date <- as.Date(temp_df_trim$DateTime) # date in Date format
	temp_df_trim$month <- month(temp_df_trim$date) # month of year
	temp_df_trim$week <- week(temp_df_trim$date) # week of year
	temp_df_trim$year <- year(temp_df_trim$date)
	
	
	## calculate mean and sd of hindcast during ref period (1985 - 2014) ##

	baseline_years <- 1985:2014 # define baseline/ref years (here based on Cheng et al 2021) 
	
	temp_df_hind_ref <- temp_df_trim %>% # select ref yrs from df
		filter(., year %in% baseline_years)
	
	# calculate the mean and sd of temp for each grid cell for each week across the ref period
	temp_df_hind_mean_sd <- temp_df_hind_ref %>% 
		group_by(month, domain) %>%
		summarize(mean_hind_temp = mean(temp),
							sd_hind_temp = sd(temp))
	
	
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
  
  # data wrangle -- trim to grid, convert dates, trim to ref period
	
	meta_dat_func <- function(df){
	
		df <- merge(df, domain_depth_df, by = c("xi_rho", "eta_rho"))
	
		df_trim <- df %>%
			filter(domain > 0) %>%
			filter(., between(depth, 0, 250)) 
	
		df %>%
			mutate(DateTime = as.POSIXct(ocean_time, origin = "1900-01-01", tz = "GMT"),
						 date = as.Date(DateTime),
						 month = month(date),
						 week = week(date),
						 year = year(date))
	}
	
	cesm_hist_df_list <-
		lapply(cesm_hist_df_list, meta_dat_func)
	
	cesm_hist_dfs <- bind_rows(cesm_hist_df_list) %>%
		filter(year > 1984) 
	
	## calculate the mean and sd of temp for each grid cell for each week across the ref period
	cesm_hist_mean_sd <-
		cesm_hist_dfs %>% 
		group_by(month, domain) %>%
		summarize(mean_hist_temp = mean(temp),
							sd_hist_temp = sd(temp))

  
	## ssp 126 scenario ##
	
	# read in model output
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
  
	# add meta data
	
	dat_wrang_func <- function(df){
	
		df <- df %>% 
			mutate(DateTime = as.POSIXct(ocean_time, origin = "1900-01-01", tz = "GMT"),
						 date = as.Date(DateTime),
						 month = month(date),
						 week = week(date),
						 year = year(date))

		df <- left_join(df, domain_depth_df)

	
		df <- df %>%
			filter(domain > 0) %>%
			filter(., between(depth, 0, 250)) %>%
			select(-xi_rho, -eta_rho, -ocean_time, -Xi, -Eta)
	}
	
	cesm_ssp126_df_list <-
		lapply(cesm_ssp126_df_list, dat_wrang_func)
	
	# bias correct - calculate deltas, scaling factors, and bc temps
				
	cesm_bc_correction_func <- function(df){
		
		df1 <- left_join(df, cesm_hist_mean_sd) 
		
		df2 <- left_join(df1, temp_df_hind_mean_sd)
		
		df3 <- df2 %>%
			group_by(year, month, latitude, longitude, domain) %>%
			mutate(delta = temp - mean_hist_temp,
						 scaling_factor = sd_hind_temp/sd_hist_temp,
						 bc_temp = mean_hind_temp + (delta * scaling_factor))
	}
	
	cesm_ssp126_bc_dfs <-
		lapply(cesm_ssp126_df_list, cesm_bc_correction_func)

	cesm_ssp126_bc_df <- bind_rows(cesm_ssp126_bc_dfs)
	
	cesm_ssp126_bc_df <- cesm_ssp126_bc_df %>%
		mutate(simulation = "cesm",
					 scenario = "ssp126") 
	
	
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
  
  
	# add meta data
	
	dat_wrang_func <- function(df){
	
		df <- df %>% 
			mutate(DateTime = as.POSIXct(ocean_time, origin = "1900-01-01", tz = "GMT"),
						 date = as.Date(DateTime),
						 month = month(date),
						 week = week(date),
						 year = year(date))

		df <- left_join(df, domain_depth_df)

	
		df <- df %>%
			filter(domain > 0) %>%
			filter(., between(depth, 0, 250)) %>%
			dplyr::select(-xi_rho, -eta_rho, -ocean_time, -Xi, -Eta)
	}
	
	cesm_ssp585_df_list <-
		lapply(cesm_ssp585_df_list, dat_wrang_func)
	
	# bias correct - calculate deltas, scaling factors, and bc temps
	
	cesm_bc_correction_func <- function(df){
		
		df1 <- left_join(df, cesm_hist_mean_sd) 
		
		df2 <- left_join(df1, temp_df_hind_mean_sd)
		
		df3 <- df2 %>%
			group_by(year, month, latitude, longitude, domain) %>%
			mutate(delta = temp - mean_hist_temp,
						 scaling_factor = sd_hind_temp/sd_hist_temp,
						 bc_temp = mean_hind_temp + (delta * scaling_factor))
	}
	
	cesm_ssp585_bc_dfs <-
		lapply(cesm_ssp585_df_list, cesm_bc_correction_func)

	cesm_ssp585_bc_df <- bind_rows(cesm_ssp585_bc_dfs)
	
	cesm_ssp585_bc_df <- cesm_ssp585_bc_df %>%
		mutate(simulation = "cesm",
					 scenario = "ssp585")
	
	#### add together

	cesm_dfs_trim_domain <- full_join(cesm_ssp126_bc_df, cesm_ssp585_bc_df)
	
  fwrite(cesm_dfs_trim_domain, "./data/cesm_dfs_trim_domain.csv")
	
  
 
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
  
  # data wrangle -- trim to grid, convert dates, trim to ref period
	
	meta_dat_func <- function(df){
	
		df <- merge(df, domain_depth_df, by = c("xi_rho", "eta_rho"))
	
		df_trim <- df %>%
			filter(domain > 0) %>%
			filter(., between(depth, 0, 250)) 
	
		df %>%
			mutate(DateTime = as.POSIXct(ocean_time, origin = "1900-01-01", tz = "GMT"),
						 date = as.Date(DateTime),
						 month = month(date),
						 week = week(date),
						 year = year(date))
	}
	
	gfdl_hist_df_list <-
		lapply(gfdl_hist_df_list, meta_dat_func)
	
	gfdl_hist_dfs <- bind_rows(gfdl_hist_df_list) %>%
		filter(year > 1984) 
	
	## calculate the mean and sd of temp for each grid cell for each week across the ref period
	gfdl_hist_mean_sd <-
		gfdl_hist_dfs %>% 
		group_by(month, domain) %>%
		summarize(mean_hist_temp = mean(temp),
							sd_hist_temp = sd(temp))

	
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
  
  # add meta data
	
	dat_wrang_func <- function(df){
	
		df <- df %>% 
			mutate(DateTime = as.POSIXct(ocean_time, origin = "1900-01-01", tz = "GMT"),
						 date = as.Date(DateTime),
						 month = month(date),
						 week = week(date),
						 year = year(date))

		df <- left_join(df, domain_depth_df)

	
		df <- df %>%
			filter(domain > 0) %>%
			filter(., between(depth, 0, 250)) %>%
			dplyr::select(-xi_rho, -eta_rho, -ocean_time, -Xi, -Eta)
	}
	
	gfdl_ssp126_df_list <-
		lapply(gfdl_ssp126_df_list, dat_wrang_func)
	
	# bias correct - calculate deltas, scaling factors, and bc temps
	
	gfdl_bc_correction_func <- function(df){
		
		df1 <- left_join(df, gfdl_hist_mean_sd) 
		
		df2 <- left_join(df1, temp_df_hind_mean_sd)
		
		df3 <- df2 %>%
			group_by(year, month, latitude, longitude, domain) %>%
			mutate(delta = temp - mean_hist_temp,
						 scaling_factor = sd_hind_temp/sd_hist_temp,
						 bc_temp = mean_hind_temp + (delta * scaling_factor))
	}
	
	gfdl_ssp126_bc_dfs <-
		lapply(gfdl_ssp126_df_list, gfdl_bc_correction_func)

	gfdl_ssp126_bc_df <- bind_rows(gfdl_ssp126_bc_dfs)
	
	gfdl_ssp126_bc_df <- gfdl_ssp126_bc_df %>%
		mutate(simulation = "gfdl",
					 scenario = "ssp126")

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
  
  # add meta data
	
	dat_wrang_func <- function(df){
	
		df <- df %>% 
			mutate(DateTime = as.POSIXct(ocean_time, origin = "1900-01-01", tz = "GMT"),
						 date = as.Date(DateTime),
						 month = month(date),
						 week = week(date),
						 year = year(date))

		df <- left_join(df, domain_depth_df)

	
		df <- df %>%
			filter(domain > 0) %>%
			filter(., between(depth, 0, 250)) %>%
			dplyr::select(-xi_rho, -eta_rho, -ocean_time, -Xi, -Eta)
	}
	
	gfdl_ssp585_df_list <-
		lapply(gfdl_ssp585_df_list, dat_wrang_func)
	
	# bias correct - calculate deltas, scaling factors, and bc temps
	
	gfdl_bc_correction_func <- function(df){
		
		df1 <- left_join(df, gfdl_hist_mean_sd) 
		
		df2 <- left_join(df1, temp_df_hind_mean_sd)
		
		df3 <- df2 %>%
			group_by(year, month, latitude, longitude, domain) %>%
			mutate(delta = temp - mean_hist_temp,
						 scaling_factor = sd_hind_temp/sd_hist_temp,
						 bc_temp = mean_hind_temp + (delta * scaling_factor))
	}
	
	gfdl_ssp585_bc_dfs <-
		lapply(gfdl_ssp585_df_list, gfdl_bc_correction_func)

	gfdl_ssp585_bc_df <- bind_rows(gfdl_ssp585_bc_dfs)
	
	gfdl_ssp585_bc_df <- gfdl_ssp585_bc_df %>%
		mutate(simulation = "gfdl",
					 scenario = "ssp585") 

	#### add together

	gfdl_dfs_trim_domain <- full_join(gfdl_ssp126_bc_df, gfdl_ssp585_bc_df)
	
  fwrite(gfdl_dfs_trim_domain, "./data/gfdl_dfs_trim_domain.csv")


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
  
  # data wrangle -- trim to grid, convert dates, trim to ref period
	
	meta_dat_func <- function(df){
	
		df <- merge(df, domain_depth_df, by = c("xi_rho", "eta_rho"))
	
		df_trim <- df %>%
			filter(domain > 0) %>%
			filter(., between(depth, 0, 250)) 
	
		df %>%
			mutate(DateTime = as.POSIXct(ocean_time, origin = "1900-01-01", tz = "GMT"),
						 date = as.Date(DateTime),
						 month = month(date),
						 week = week(date),
						 year = year(date))
	}
	
	miroc_hist_df_list <-
		lapply(miroc_hist_df_list, meta_dat_func)
	
	miroc_hist_dfs <- bind_rows(miroc_hist_df_list) %>%
		filter(year > 1984) 
	
	## calculate the mean and sd of temp for each grid cell for each week across the ref period
	miroc_hist_mean_sd <-
		miroc_hist_dfs %>% 
		group_by(month, domain) %>%
		summarize(mean_hist_temp = mean(temp),
							sd_hist_temp = sd(temp))

  
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
  
  # add meta data
	
	dat_wrang_func <- function(df){
	
		df <- df %>% 
			mutate(DateTime = as.POSIXct(ocean_time, origin = "1900-01-01", tz = "GMT"),
						 date = as.Date(DateTime),
						 month = month(date),
						 week = week(date),
						 year = year(date))

		df <- left_join(df, domain_depth_df)

	
		df <- df %>%
			filter(domain > 0) %>%
			filter(., between(depth, 0, 250)) %>%
			dplyr::select(-xi_rho, -eta_rho, -ocean_time, -Xi, -Eta)
	}
	
	miroc_ssp126_df_list <-
		lapply(miroc_ssp126_df_list, dat_wrang_func)
	
	# bias correct - calculate deltas, scaling factors, and bc temps
	
	miroc_bc_correction_func <- function(df){
		
		df1 <- left_join(df, miroc_hist_mean_sd) 
		
		df2 <- left_join(df1, temp_df_hind_mean_sd)
		
		df3 <- df2 %>%
			group_by(year, week, latitude, longitude) %>%
			mutate(delta = temp - mean_hist_temp,
						 scaling_factor = sd_hind_temp/sd_hist_temp,
						 bc_temp = mean_hind_temp + (delta * scaling_factor))
	}
	
	miroc_ssp126_bc_dfs <-
		lapply(miroc_ssp126_df_list, miroc_bc_correction_func)

	miroc_ssp126_bc_df <- bind_rows(miroc_ssp126_bc_dfs)
	
	miroc_ssp126_bc_df <- miroc_ssp126_bc_df %>%
		mutate(simulation = "miroc",
					 scenario = "ssp126") 
	

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
  
   # add meta data
	
	dat_wrang_func <- function(df){
	
		df <- df %>% 
			mutate(DateTime = as.POSIXct(ocean_time, origin = "1900-01-01", tz = "GMT"),
						 date = as.Date(DateTime),
						 month = month(date),
						 week = week(date),
						 year = year(date))

		df <- left_join(df, domain_depth_df)

	
		df <- df %>%
			filter(domain > 0) %>%
			filter(., between(depth, 0, 250)) %>%
			dplyr::select(-xi_rho, -eta_rho, -ocean_time, -Xi, -Eta)
	}
	
	miroc_ssp585_df_list <-
		lapply(miroc_ssp585_df_list, dat_wrang_func)
	
	# bias correct - calculate deltas, scaling factors, and bc temps
	
	miroc_bc_correction_func <- function(df){
		
		df1 <- left_join(df, miroc_hist_mean_sd) 
		
		df2 <- left_join(df1, temp_df_hind_mean_sd)
		
		df3 <- df2 %>%
			group_by(year, week, latitude, longitude) %>%
			mutate(delta = temp - mean_hist_temp,
						 scaling_factor = sd_hind_temp/sd_hist_temp,
						 bc_temp = mean_hind_temp + (delta * scaling_factor))
	}
	
	miroc_ssp585_bc_dfs <-
		lapply(miroc_ssp585_df_list, miroc_bc_correction_func)

	miroc_ssp585_bc_df <- bind_rows(miroc_ssp585_bc_dfs)
	
	miroc_ssp585_bc_df <- miroc_ssp585_bc_df %>%
		mutate(simulation = "miroc",
					 scenario = "ssp585") 
		
	#### add together

	miroc_dfs_trim_wkgc <- full_join(miroc_ssp126_bc_df, miroc_ssp585_bc_df)
	
  fwrite(miroc_dfs_trim_wkgc, "./data/miroc_dfs_trim_wkgc.csv")
 	
  