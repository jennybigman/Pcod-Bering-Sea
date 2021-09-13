	# mapping % change 

	#### maps by yearly bins ####

	# first try 10-yr bins

	# summarize data into year bins

	# create a list of year bins
	year_bins  <- list('1970_1980' = c(1970:1980),
                     '1980_1990' = c(1980:1990),
                     '1990_2000' = c(1990:2000),
                     '2000_2010' = c(2000:2010),
                     '2010_2020' = c(2010:2020)) 

	# create a list of dfs summarized by year bins
	sum_yr_bin <- function(x){
 
		df <- sm_temp_hind_df %>%
		filter(year %in% x) %>%
		group_by(longitude, latitude) %>%
		summarise(mean_hs = mean(hatch_success_cauchy),
							mean_temp = mean(temp),
							mean_sphabsuit = mean(sp_hab_suit)) 
		
		df
	
	}

	df_list <- lapply(year_bins, sum_yr_bin)

	# add a column of the year bin
	df_list_tp <- mapply(cbind, df_list, "time_period"= names(year_bins), SIMPLIFY = FALSE)

	# bind all rows of all dfs together and add a column for longitude not on 360 scale
	sm_temp_hind_df_yr_sum <- bind_rows(df_list_tp) %>% 
  	dplyr::select(latitude, longitude, mean_sphabsuit, time_period)
 
  year_list  <- list('1970_1980',
                     '1980_1990',
                     '1990_2000',
                     '2000_2010',
                     '2010_2020')
 

	reshape_func <- function(x){
		
		new_dat <- sm_temp_hind_df_yr_sum %>% filter(., time_period == x)

  	new_dat <- new_dat %>% 
  		rename(!!paste0("mean_sphabsuit_", x) := mean_sphabsuit) %>%
  		dplyr::select(- time_period)
  			
    new_dat
    
  }
  
  df_list <- lapply(year_list, reshape_func)
  
  sp_yr_sum <- bind_cols(df_list)
  
  sp_yr_sum <- sp_yr_sum[- c(4:5, 7:8, 10:11, 13:14)]
  
  sp_yr_sum <- sp_yr_sum %>%
  	rename(latitude = latitude...1,
  				 longitude = longitude...2)
  
  sp_yr_sum <- sp_yr_sum %>%
  	rowwise() %>%
  	mutate(pct_change_1970_1980_1980_1990 = 
  				 	(mean_sphabsuit_1980_1990 - mean_sphabsuit_1970_1980),
  				 pct_change_1980_1990_1990_2000 = 
  				 	(mean_sphabsuit_1990_2000 - mean_sphabsuit_1980_1990),
  				 pct_change_1990_2000_2000_2010 = 
  				 	(mean_sphabsuit_2000_2010 - mean_sphabsuit_1990_2000),
					 pct_change_2000_2010_2010_2020 = 
  				 	(mean_sphabsuit_2010_2020 - mean_sphabsuit_2000_2010)
					 )
  
  sp_yr_sum <- sp_yr_sum %>%
  	dplyr::select(- contains("mean_sphabsuit"))
 
  sp_yr_sum_long <- sp_yr_sum %>%
  	gather(pct_change, value, -c("latitude", "longitude")) 
  
  sp_yr_sum_long_sf <- sp_yr_sum_long %>%
  	mutate(long_not_360 = longitude - 360) %>%
  	st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326)
  
  sp_yr_sum_long_sf <- sp_yr_sum_long_sf %>%
  	rename(time_period = pct_change,
  				 pct_change = value)
  
  sp_yr_sum_long_sf$time_period <- str_replace(sp_yr_sum_long_sf$time_period,
  																						 "pct_change_", "")

  	
  	plot <- 
  		ggplot() +
					geom_sf(data = sp_yr_sum_long_sf, aes(color = value))  +
					geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
					coord_sf(crs = 3338) +
  		    facet_wrap(~ pct_change, nrow = 1) +
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
    	  	labs(colour = "Percent change\nin spawning\nhabitat suitability") +
					theme_bw() +
 					theme(
 						axis.text = element_text(size = 12),	
  					axis.title = element_text(size = 14),
  					legend.title.align=0.5)
  	

  	ggsave("./output/plots/percent change.png",
		plot,
		width = 15, height = 10, units = "in")
	
  
  	#### specific year bins -- recent vs historic ####
  	
	# summarize data into year bins
  years_keep <- c(2000:2020)
  	
  sm_temp_hind_df_sub <- sm_temp_hind_df %>% filter(., year %in% years_keep)
  

	# create a list of year bins
	year_bins  <- list('2000_2014' = c(2000:2014),
                     '2015_2020' = c(2015:2020)) 

	# create a list of dfs summarized by year bins
	sum_yr_bin <- function(x){
 
		df <- sm_temp_hind_df_sub %>%
		filter(year %in% x) %>%
		group_by(longitude, latitude) %>%
		summarise(mean_hs = mean(hatch_success_cauchy),
							mean_temp = mean(temp),
							mean_sphabsuit = mean(sp_hab_suit)) 
		
		df
	
	}

	df_list <- lapply(year_bins, sum_yr_bin)

	# add a column of the year bin
	df_list_tp <- mapply(cbind, df_list, "time_period"= names(year_bins), SIMPLIFY = FALSE)

	# bind all rows of all dfs together and add a column for longitude not on 360 scale
	sm_temp_hind_df_yr_sum <- bind_rows(df_list_tp) %>% 
  	dplyr::select(latitude, longitude, mean_sphabsuit, time_period)
 
  year_list  <- list("2000_2014",
                     "2015_2020")

  	reshape_func <- function(x){
		
		new_dat <- sm_temp_hind_df_yr_sum %>% filter(., time_period == x)

  	new_dat <- new_dat %>% 
  		rename(!!paste0("mean_sphabsuit_", x) := mean_sphabsuit) %>%
  		dplyr::select(- time_period)
  			
    new_dat
    
  }
  
  df_list <- lapply(year_list, reshape_func)
  
  sp_yr_sum <- bind_cols(df_list)
  
  sp_yr_sum <- sp_yr_sum[- c(4:5)]
  
  sp_yr_sum <- sp_yr_sum %>%
  	rename(latitude = latitude...1,
  				 longitude = longitude...2)
  
  sp_yr_sum <- sp_yr_sum %>%
  	rowwise() %>%
  	mutate(pct_change = 
  				 	(mean_sphabsuit_2015_2020 - mean_sphabsuit_2000_2014) * 100)
  
  sp_yr_sum_sf <- sp_yr_sum %>%
  	mutate(long_not_360 = longitude - 360) %>%
  	st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326)
 
  	
  	plot <- 
  		ggplot() +
					geom_sf(data = sp_yr_sum_sf, aes(color = pct_change))  +
					geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
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
    	  	labs(colour = "% change") +
					theme_bw() +
  				ggtitle(label = "Absolute percent change of spawning habitat\nsuitability from 2000-2014 to 2015-2020") +
 					theme(
 						axis.text = element_text(size = 12),	
  					axis.title = element_text(size = 14),
  					legend.title.align=0.5)
  	

  	ggsave("./output/plots/percent_change_historic_today.png",
		plot,
		width = 5, height = 5, units = "in")
	
  