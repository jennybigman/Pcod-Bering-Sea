# calculating the mean latitude of spawning habitat suitability

	#### year ####
	
	mean_lat_yr <- function(x){
		
		new_dat <- ROMS_hindcast_dat %>%
			filter(., sp_hab_suit >= x)
		
		new_dat_sum <- new_dat %>%
			group_by(year) %>%
			summarise(mean_lat = mean(latitude)) 

		new_dat_sum		
	}
	
	sp_hab_thresholds <- c(0.5, 0.9)
	
	mean_lats_yr <- lapply(sp_hab_thresholds, mean_lat_yr)
	
	mean_lats_yr_0.5 <- mean_lats_yr[[1]] %>%
		rename(mean_lat_0.5 = mean_lat)
	
	mean_lats_yr_0.9 <- mean_lats_yr[[2]]	%>%
		rename(mean_lat_0.9 = mean_lat)
	
	mean_lats_yr_df <- merge(mean_lats_yr_0.9, mean_lats_yr_0.5, by = "year") 
	
	mean_lats_yr_df <- mean_lats_yr_df %>% filter(., year != 2021)
	
	# one plot
	
	mean_lat_yearly_plot <-    
   	ggplot(data = mean_lats_yr_df) +
   	geom_line(aes(x = year, y = mean_lat_0.5), alpha = 0.7, color = "#7f7fbf", size = 1) +
		geom_line(aes(x = year, y = mean_lat_0.9), alpha = 0.7, color = "#00345C", size = 1) +
   	xlab("Year") + 
	  scale_y_continuous(
	  	name = "Mean\nlatitude",
	  	breaks = c(56, 57),
	  	labels = c("56˚N", "57˚N")
	  ) +
   	xlim(1970, 2030) +
   	theme_bw() +
  	theme(
  	  axis.text=element_text(size=12, colour = "grey50"),
  	  axis.ticks = element_line(colour = "grey50"),
  	  axis.line = element_line(colour = "grey50"),
  	  axis.title.x = element_text(size=14, color = "grey30"),
  	  axis.title.y = element_text(angle = 360, vjust = 0.5, size=14, color = "grey30"),
  	  panel.grid.major = element_blank(),
  	  panel.grid.minor = element_blank(),
  	  panel.border = element_rect(fill = NA, color = "grey50"))
   
   mean_lat_yearly_plot_text <- mean_lat_yearly_plot +
		annotate(geom = "text", x = 2026, y = 57,
           label = paste("spawning\nhabitat\nsuitability", symbol, "0.5", sep = " "),
           color = "#7f7fbf", size = 4) +
   	annotate(geom = "text", x = 2026, y = 56,
           label = paste("spawning\nhabitat\nsuitability", symbol, "0.9", sep = " "),
           color = "#00345C", size = 4) 
   	
	ggsave("./output/plots/mean_lat_yearly_plot.png",
			 mean_lat_yearly_plot_text,
			 width = 12, height = 7, units = "in")

	
	# try with map
	
	geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
					coord_sf(crs = 3338) +
					scale_color_gradientn(colors = c("#B3E5FC", "#B3E5FC", 
																					 "#01579B", "#01579B",
																					 "#00345C", "#00345C"),
																values = c(0, 0.499, 0.5, 0.899, 0.9, 1),
																breaks = c(0.1, 0.5, 0.9),
																labels = format(c(0.1, 0.5, 0.9)),
																limits = c(0, 1)) +
    	  	facet_wrap(~ month_name, ncol = 3, nrow = 3) +
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
    	  	labs(colour = "Spawning\nhabitat\nsuitability") +
					theme_bw() +
 					theme(
 						strip.text = element_text(size = 14, face = "bold"),
 						strip.background = element_blank(),
 						axis.text = element_text(size = 12),	
  					axis.title = element_text(size = 14),
  					legend.title.align=0.5)
	
	#### month ####

	mean_lat_mo <- function(x){
		
		new_dat <- ROMS_hindcast_dat %>%
			filter(., sp_hab_suit >= x)
		
		new_dat_sum <- new_dat %>%
			group_by(year, month_name, month) %>%
			summarise(mean_lat = mean(latitude)) 

		new_dat_sum		
	}
	
	sp_hab_thresholds <- c(0.5, 0.9)
	
	mean_lats_mo <- lapply(sp_hab_thresholds, mean_lat_mo)
	
	mean_lats_mo_0.5 <- mean_lats_mo[[1]] %>%
		rename(mean_lat_0.5 = mean_lat)
	
	mean_lats_mo_0.9 <- mean_lats_mo[[2]]	%>%
		rename(mean_lat_0.9 = mean_lat)
	
	mean_lats_mo_df <- merge(mean_lats_mo_0.5, mean_lats_mo_0.9, 
													 by = c("year", "month_name", "month")) 
	
	mean_lats_mo_df <- mean_lats_mo_df %>% filter(., year != 2021)
	
	# reorder for plotting
	
	mean_lats_mo_df$month_name <- factor(mean_lats_mo_df$month_name)
  mean_lats_mo_df$month_name <- fct_reorder(mean_lats_mo_df$month_name, 
  																					mean_lats_mo_df$month)
	# plot
  
  	mean_lat_mo_plot <-    
   	ggplot(data = mean_lats_mo_df) +
   	geom_line(aes(x = year, y = mean_lat_0.5), 
   						alpha = 0.7, color = "#7f7fbf", size = 1) +
		geom_line(aes(x = year, y = mean_lat_0.9), 
							alpha = 0.7, color = "#00345C", size = 1) +
  	facet_wrap(~ month_name) +
   	xlab("Year") + 
	  scale_y_continuous(
	  	name = "Mean\nlatitude",
	  	breaks = c(56, 57),
	  	labels = c("56˚N", "57˚N")
	  ) +
   	xlim(1970, 2025) +
   	theme_bw() +
  	theme(
  	  axis.text=element_text(size=12, colour = "grey50"),
  	  axis.ticks = element_line(colour = "grey50"),
  	  axis.line = element_line(colour = "grey50"),
  	  axis.title.x = element_text(size=14, color = "grey30"),
  	  axis.title.y = element_text(angle = 360, vjust = 0.5, size=14, color = "grey30"),
  	  panel.grid.major = element_blank(),
  	  panel.grid.minor = element_blank(),
  	  panel.border = element_rect(fill = NA, color = "grey50"))
  	
  		ggsave("./output/plots/mean_lat_mo_plot.png",
			 mean_lat_mo_plot,
			 width = 10, height = 7, units = "in")

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
	