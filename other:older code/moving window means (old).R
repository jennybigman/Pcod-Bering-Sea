#### moving window analysis ####

  # year
  
	# create time series object
  	
	mean_lat_yr05_ts <- ts(data = mean_lats_yr_df$mean_lat_0.5, 
											  frequency = 1, 
										 	  start = mean_lats_yr_df[1, "year"])
  	
	mean_lat_yr09_ts <- ts(data = mean_lats_yr_df$mean_lat_0.9, 
											  frequency = 1, 
										 	  start = mean_lats_yr_df[1, "year"])
		
	# weights for moving average
	fltr_yr <- c(rep(1, times = 3))/3
	
	# estimate and plot trend
	
	# spawning habitat suitability >= 0.5	
	mean_lat_yr0.5_trend <- stats::filter(mean_lat_yr05_ts, filter = fltr_yr,
																				method = "convo", sides = 2)
		
	plot.ts(mean_lat_yr0.5_trend)
	
	# spawning habitat suitability >= 0.9	
	mean_lat_yr0.9_trend <- stats::filter(mean_lat_yr09_ts, filter = fltr_yr,
																				method = "convo", sides = 2)
		
	plot.ts(mean_lat_yr0.9_trend)
	
	
	# month
	
	mean_lat_mo05_ts <- ts(data = mean_lats_mo_df$mean_lat_0.5, 
											   frequency = 1, 
										 	   start = c(mean_lats_mo_df[1, "year"],
										 	  					 mean_lats_mo_df[1, "month"]))
  	
	mean_lat_mo09_ts <- ts(data = mean_lats_mo_df$mean_lat_0.9, 
											   frequency = 1, 
										 	   start = c(mean_lats_mo_df[1, "year"],
										 	  					 mean_lats_mo_df[1, "month"]))
  	
	# weights for moving average
	fltr_mo <- c(1/2, rep(1, times = 5), 1/2)/6
	
	# estimate and plot trend
	
	#spawning habitat suitability >= 0.5	
	mean_lat_mo0.5_trend <- stats::filter(mean_lat_mo05_ts, filter = fltr_mo,
																				method = "convo", sides = 2)
		
	plot.ts(mean_lat_mo0.5_trend)
	
	# spawning habitat suitability >= 0.9	
	mean_lat_mo0.9_trend <- stats::filter(mean_lat_yr09_ts, filter = fltr_mo,
																				method = "convo", sides = 2)
		
	plot.ts(mean_lat_yr0.9_trend)
	
	
	#### differencing ####
	
	# mean lat 0.5
	
	diff_mean_lat_05 <- diff(mean_lats_yr_0.5$mean_lat_0.5)
	
	yrs <- 1:51
	
	diff_meanlat05 <- data.frame(yrs, diff_mean_lat_05)

	ggplot(diff_meanlat05) +
		geom_line(aes(x = yrs, y =  diff_mean_lat_05))
	
	# mean lat 0.9
	
	diff_mean_lat_09 <- diff(mean_lats_yr_0.9$mean_lat_0.9)
	
	yrs <- 1:51
	
	diff_meanlat09 <- data.frame(yrs, diff_mean_lat_09)

	ggplot(diff_meanlat09) +
		geom_line(aes(x = yrs, y =  diff_mean_lat_09))
	