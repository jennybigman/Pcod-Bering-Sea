
	hist_years <- 1985:2014
	
	proj_hist <- ROMS_projected_dat %>%
		filter(year %in% hist_years)
	
	year_bins_hist <- list('dec_1985_1989' = c(1985:1989),
                    		 'dec_1990_1994' = c(1995:2004),
                    		 'dec_1995_1999' = c(2000:2014),
                    		 'dec_2000_2004' = c(2010:2024),
  											 'dec_2005_2009' = c(2015:2034),
  											 'dec_2010_2014' = c(2020:2044))
												 
	# create a list of dfs summarized by year bins
	sum_yr_bin_hist <- function(x){
 
		df <- ROMS_projected_dat %>%
		group_by(simulation, projection, longitude, latitude) %>%
		summarise(mean_hs = mean(hatch_success_cauchy_var),
							mean_temp = mean(bc_temp_sd),
							mean_sphabsuit = mean(sp_hab_suit_var)) 
		
		df
	
	}

	df_list_hist <- lapply(year_bins_hist, sum_yr_bin_hist)
	
	# add a column of the year bin
	df_list_hist_tp <- mapply(cbind, df_list_hist,
														"time_period"= names(year_bins_hist), SIMPLIFY = FALSE)

	# bind all rows of all dfs together and add a column for longitude not on 360 scale
	proj_hist <- bind_rows(df_list_hist_tp) %>% dplyr::select(- mean_hs, -mean_temp)
	
	year_hist <- list('dec_1985_1989',
                    'dec_1990_1994',
                    'dec_1995_1999',
                    'dec_2000_2004',
  								  'dec_2005_2009',
  								  'dec_2010_2014')
	
	reshape_hist_func <- function(x){
		
		new_dat <- proj_hist %>% filter(., time_period == x)

  	new_dat <- new_dat %>% 
  		rename(!!paste0("mean_sphabsuit_", x) := mean_sphabsuit) %>%
  		dplyr::select(- time_period)
  			
    new_dat
    
  }
  
  df_list_names_hist <- lapply(year_hist, reshape_hist_func)
  

  # join dfs -- make code more concise (possibly using reduce())
	 df1 <- df_list_names_hist[[1]]
	 df2 <- df_list_names_hist[[2]]
	 df3 <- df_list_names_hist[[3]]
	 df4 <- df_list_names_hist[[4]] 
	 df5 <- df_list_names_hist[[5]]
	 df6 <- df_list_names_hist[[6]]

	 df_proj_hist <- inner_join(df1, df2)
	 df_proj_hist <- inner_join(df_proj_hist, df3)
	 df_proj_hist <- inner_join(df_proj_hist, df4)
	 df_proj_hist <- inner_join(df_proj_hist, df5)
	 df_proj_hist <- inner_join(df_proj_hist, df6)
	 
	 # calculate % change
  df_proj_hist2 <- df_proj_hist %>%
  	rowwise() %>%
  	mutate(pchange_85_94 =
  				 	(mean_sphabsuit_dec_1990_1994 - mean_sphabsuit_dec_1985_1989),
  				 pchange_90_99 =
  				 	(mean_sphabsuit_dec_1995_1999 - mean_sphabsuit_dec_1990_1994),
  				 pchange_95_04 = 
  				 	(mean_sphabsuit_dec_2000_2004 - mean_sphabsuit_dec_1995_1999),
  				 pchange_00_09 =
  				 	(mean_sphabsuit_dec_2005_2009 - mean_sphabsuit_dec_2000_2004),
  				 pchange_05_14 =
					  (mean_sphabsuit_dec_2010_2014 - mean_sphabsuit_dec_2005_2009))
  
   label = c("1985 to 1994",'1990 to 1999', "1995 - 2004", "2000 - 2009", "2005 - 2014")
 
  df_proj_hist2 <- df_proj_hist2 %>%
  	dplyr::select(- contains("mean_sphabsuit"))
 
  df_proj_hist2_long <- df_proj_hist2 %>%
  	gather(pct_change, value, -c("latitude", "longitude", "simulation", "projection")) 
  
  df_proj_hist2_long_sf <- df_proj_hist2_long %>%
  	mutate(long_not_360 = longitude - 360) %>%
  	st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326, remove = FALSE)
  
  df_proj_hist2_long_sf <- df_proj_hist2_long_sf %>%
  	rename(time_period = pct_change,
  				 pct_change = value)
  
  df_proj_hist2_long_sf$time_period <- str_replace(df_proj_hist2_long_sf$time_period,
  																						 "pct_change_", "")
  
   	
  	plot <- 
  		ggplot() +
					geom_sf(data = df_proj_hist2_long_sf, aes(color = pct_change))  +
					geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
					coord_sf(crs = 3338) +
  		    facet_grid(simulation ~ time_period) +
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
    	  	labs(colour = "% change in\nspawning habitat\nsuitability") +
					theme_bw() +
 					theme(
 						strip.background = element_blank(),
 						strip.text = element_text(size = 14),
 						axis.text = element_text(size = 12),	
  					axis.title = element_text(size = 14),
  					legend.title.align=0.5)
  	

  	ggsave("./output/plots/percent change_proj.png",
		plot,
		width = 20, height = 10, units = "in")

	 
	 
	 
	 
	 
	 
	 
	   											 'dec_2015_2019' = c(2030:2054),
  											 'dec_2020_2024' = c(2040:2064),
  											 'dec_2025_2029' = c(2050:2074),
  											 'dec_2030_2034' = c(2060:2084),
  											 'dec_2035_2039' = c(2070:2094),
  											 'dec_2040_2044' = c(2070:2094),
  											 'dec_2045_2049' = c(2070:2094),
  											 'dec_2050_2054' = c(2070:2094),
  											 'dec_2055_2059' = c(2070:2094),
  											 'dec_2060_2064' = c(2070:2094),
  											 'dec_2065_2069' = c(2070:2094),
  											 'dec_2070_2074' = c(2070:2094),
  											 'dec_2075_2079' = c(2070:2094),
  											 'dec_2080_2084' = c(2070:2094),
  											 'dec_2085_2089' = c(2070:2094),
  											 'dec_2090_2094' = c(2070:2094),
  											 'dec_2095_2099' = c(2070:2094))
	 