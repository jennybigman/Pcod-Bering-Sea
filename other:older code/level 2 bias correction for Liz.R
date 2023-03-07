# bias-correction script for Liz 

	# load libraries
	library(here)
	library(ncdf4)
	library(tidync)
	require(tidyverse)
	library(data.table)
	library(sf)
	
	# load hindcast and meta data (depth, area, domain)
	temp_df <- fread("./data/ROMS_all_temp.csv")

	## calculate mean and sd of hindcast during ref period (1985 - 2014) ##
	baseline_years <- 1985:2014 # define baseline/ref years (here based on Cheng et al 2021) 
	
	temp_df_hind_ref <- temp_df %>% # select ref yrs from df
		filter(., year %in% baseline_years)
	
	# calculate the mean and sd of temp for each grid cell for each week across the ref period
	temp_df_hind_mean_sd <- temp_df_hind_ref %>% 
		group_by(week, latitude, longitude) %>%
		summarize(mean_hind_temp = mean(temp),
							sd_hind_temp = sd(temp))
	

	#### CESM simulations ####
  
  # read in files - I got level 2 output directly from Kirstin so stored locally on my machine
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
		group_by(week, latitude, longitude) %>%
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
			group_by(year, week, latitude, longitude) %>%
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
			group_by(year, week, latitude, longitude) %>%
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

	cesm_dfs_trim_wkgc <- full_join(cesm_ssp126_bc_df, cesm_ssp585_bc_df)
	
  fwrite(cesm_dfs_trim_wkgc, "./data/cesm_dfs_trim_wkgc.csv")
