# bias correct before trimming spatially and temporally

	library(here)
	library(ncdf4)
	library(tidync)
	require(tidyverse)
	library(data.table)
	library(sf)

	# load hindcast temp and meta data dfs
	temp_df <- fread("./data/ROMS_all_temp.csv")
	area_df <- fread( "./data/ROMS_area_grid_cells.csv")
	depth_df <- fread("./data/ROMS_depth_df.csv")
	domain_df <- fread("./data/ROMS_domain_df.csv")

	#### hindcast ####
	area_depth_df <- merge(area_df, depth_df, by = c("latitude", "longitude", "Xi", "Eta"))

	area_depth_domain_df <- merge(area_depth_df, domain_df,
													by = c("latitude", "longitude", "Xi", "Eta"))

	hindcast <- merge(temp_df, area_depth_domain_df, 
												 by = c("latitude", "longitude", "Xi", "Eta"),
												 all = TRUE)
	
	#hindcast <- na.omit(hindcast)
	
	hindcast$date <- as.Date(hindcast$DateTime) # date in Date format
	hindcast$month <- month(hindcast$date) # month of year
	hindcast$week <- week(hindcast$date) # week of year
	hindcast$year <- year(hindcast$date)
	
	
	#  calculate means and sds of temp for each grid cell from the hindcast during
	#  the reference period
	
	baseline_years <- 1985:2015 # define baseline/ref years *match Kelly's
	
	hindcast_baseline <- hindcast %>% # select ref yrs from df
		filter(., year %in% baseline_years)
	
	hindcast_baseline_sum <- hindcast_baseline %>% 
		group_by(week, month, latitude, longitude) %>%
		summarize(mean_mo_baseline_temp = mean(temp),
							sd_mo_baseline_temp = sd(temp))
	
	
	#### CESM simulations ####
  
	# load data #
	
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
  
  # join together
	cesm_ssp126_dfs$scenario <- "ssp126"
	cesm_ssp585_dfs$scenario <- "ssp585"
	cesm_hist_dfs$scenario <- "historical"
	
	cesm_dfs <- bind_rows(cesm_hist_dfs, cesm_ssp126_dfs, cesm_ssp585_dfs) 

	# separate date column into components
	cesm_dfs$Time <- as.POSIXct(cesm_dfs$ocean_time, 
																		origin = "1900-01-01", tz = "GMT") 

	cesm_dfs$date <- as.Date(cesm_dfs$Time) # date in Date format
	
	cesm_dfs$month <- month(cesm_dfs$date) # month of year
	cesm_dfs$week <- week(cesm_dfs$date) # week of year
	cesm_dfs$year <- year(cesm_dfs$date)
	
	# add lat/lon
  cesm_dfs <- cesm_dfs %>%
  	mutate(Xi = xi_rho,
  				 Eta = eta_rho) %>% 
  	left_join(area_df, by = c("Xi", "Eta"))

  # to help with memory
	fwrite(cesm_dfs, "./data/cesm_dfs.csv")
	fwrite(hindcast_baseline_sum, "./data/hindcast_baseline_sum.csv")
  
	# start bias corrections #
	cesm_dfs <- fread("./data/cesm_dfs.csv")
	hindcast_baseline_sum <- fread("./data/hindcast_baseline_sum.csv")
	
	# trim to reference period (1985 - 2015)
	cesm_baseline_temp_dat <- cesm_dfs %>% 
		filter(., year %in% baseline_years)
	
	# estimate means and sds of temp for each grid cell from the hindcast during
	# the reference period
	cesm_baseline_sum <- cesm_baseline_temp_dat %>%
		group_by(week, month, latitude, longitude) %>%
		summarize(mean_proj_baseline = mean(temp),
							sd_proj_baseline = sd(temp))

	#3 estimate the weekly-averaged temps for each grid cell for each yr for projected yrs ####
	cesm_proj_temp_dat <- cesm_dfs %>% 
		group_by(week, month, year, scenario, latitude, longitude) %>%
		summarise(mo_avg_proj_temp = mean(temp))
	
	#4 calculate deltas (difference btw raw projected temp and mean proj temp across ####
	# ref period)
	
	# combine the monthly means for historical period and projected df into one df
	cesm_delta_dat <- merge(cesm_proj_temp_dat, cesm_baseline_sum,
											 by = c("week", "month", "latitude", "longitude"))

	cesm_delta_dat <- cesm_delta_dat %>%
		mutate(delta = (mo_avg_proj_temp - mean_proj_baseline))
	
	#5 add deltas mean of the hindcast during the reference years (step 1)
	cesm_bc_temps <- merge(ROMS_baseline_temp_dat_mo, cesm_delta_dat,
											 by = c("domain", "month"))
	
	cesm_bc_temps_domain <- cesm_bc_temps %>%
		mutate(sd_ratio = sd_mo_baseline_temp/sd_proj_baseline,
					 bc_temp = delta + mean_mo_baseline_temp,
					 bc_temp_sd = (sd_ratio * delta) + mean_mo_baseline_temp)
	
	fwrite(cesm_bc_temps_domain, file = "./data/cesm_bc_temps_domain.csv")
