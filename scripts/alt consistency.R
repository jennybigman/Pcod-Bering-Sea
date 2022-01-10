
	hist_years <- 1980:2014
	
	proj_hist <- ROMS_projected_dat %>%
		filter(year %in% hist_years)
	
	year_bins_hist <- list('dec_1985_1989' = c(1985:1994),
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
	df_list_hist_tp <- mapply(cbind, df_list_hist, "time_period"= names(year_bins_hist), SIMPLIFY = FALSE)

	# bind all rows of all dfs together and add a column for longitude not on 360 scale
	proj_hist <- bind_rows(df_list_tp) %>% dplyr::select(- mean_hs, -mean_temp)
	
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
	 