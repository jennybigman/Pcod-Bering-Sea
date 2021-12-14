# calculating the mean latitude of spawning habitat suitability

	#### year ####
	
	#### hindcasts ####
	
	hind_mean_lat_yr <- function(x){
		
		new_dat <- ROMS_hindcast_dat %>%
			filter(., sp_hab_suit >= x)
		
		new_dat_sum <- new_dat %>%
			group_by(year) %>%
			summarise(hist_mean_lat = mean(latitude)) 

		new_dat_sum		
	}
	
	sp_hab_thresholds <- c(0.5, 0.9)
	
	hind_mean_lats_yr <- lapply(sp_hab_thresholds, hind_mean_lat_yr)
	
	hind_mean_lats_yr_0.5 <- hind_mean_lats_yr[[1]] %>%
		mutate(sp_hab_threshold = 0.5)

	hind_mean_lats_yr_0.9 <- hind_mean_lats_yr[[2]]	%>%
		mutate(sp_hab_threshold = 0.9)
	
	hind_mean_lats_yr_df <- bind_rows(hind_mean_lats_yr_0.5, hind_mean_lats_yr_0.9) 
	
	
	#### projections ####
	
	proj_mean_lat_yr <- function(x){
		
		new_dat <- ROMS_projected_dat %>%
			filter(., sp_hab_suit >= x)
		
		new_dat_sum <- new_dat %>%
			group_by(simulation, projection, year) %>%
			summarise(proj_mean_lat = mean(Lat)) 

		new_dat_sum		
	}
	
	sp_hab_thresholds <- c(0.5, 0.9)
	
	proj_mean_lats_yr <- lapply(sp_hab_thresholds, proj_mean_lat_yr)
	
	proj_mean_lats_yr_0.5 <- proj_mean_lats_yr[[1]] %>%
		mutate(sp_hab_threshold = 0.5)

	proj_mean_lats_yr_0.9 <- proj_mean_lats_yr[[2]]	%>%
		mutate(sp_hab_threshold = 0.9)
	
	proj_mean_lats_yr_df <- bind_rows(proj_mean_lats_yr_0.5, proj_mean_lats_yr_0.9) 
	
		
	proj_mean_lats_yr_df_plot <- proj_mean_lats_yr_df %>%
		filter(!str_detect(sim_proj, "_historical"))
	
	proj_mean_lats_yr_df_plot <- tidyr::unite(proj_mean_lats_yr_df_plot,"sim_proj",
																			 simulation, projection, remove = F)

	colors <- c("#efd966", "#b79a00", 
						  "#7fb27f", "#004700", 
						  "#6666b2", "#000059")

	sim_proj <- unique(proj_mean_lats_yr_df_plot$sim_proj)
	
	names(colors) <- unique(proj_mean_lats_yr_df_plot$sim_proj)
	
	
	mean_latitude_plot <-    
   	ggplot(data = hind_mean_lats_yr_df) +
	 	geom_line(aes(year, hist_mean_lat, alpha = 0.5),
            data = . %>% filter(sp_hab_threshold == 0.5), color = "black") +
		geom_line(data = proj_mean_lats_yr_df_plot, 
							aes(year, proj_mean_lat, color = sim_proj, group = sim_proj, alpha = 0.5)) +
		facet_grid(sp_hab_threshold ~ simulation) +
	  geom_line(aes(year, hist_mean_lat, colour = sp_hab_threshold, alpha = 0.5),
            data = . %>% filter(sp_hab_threshold == 0.9), color = "black") +
		xlab("Year") +
		scale_color_manual(name = "sim_proj", values = colors) +
	  scale_y_continuous(
	  	name = "Mean\nlatitude",
	  	breaks = c(56, 58, 60, 62),
	  	labels = c("56˚N", "58˚N", "60˚N", "62˚N")
	  ) +
   	xlim(1970, 2100) +
    theme_bw() +
  	theme(legend.position = "none") +
  	theme(
			strip.background = element_blank(),
  		strip.text = element_text(size = 18, face = "bold"),
			axis.text = element_text(size = 16, colour = "grey50"),
  	  axis.ticks = element_line(colour = "grey50"),
  	  axis.line = element_line(colour = "grey50"),
  	  axis.title = element_text(size=18, color = "grey30"),
  	  panel.grid.major = element_blank(),
  	  panel.grid.minor = element_blank(),
  	  panel.border = element_rect(fill = NA, color = "grey50"))
	
	
		ggsave("./output/plots/mean_latitude_plot.png",
			 mean_latitude_plot,
			 width = 10, height = 5, units = "in")
	
		
	proj_mean_lats_yr_hist <- proj_mean_lats_yr_df %>%
		filter(str_detect(sim_proj, "_historical"))

	mean_latitude_plot <- mean_latitude_plot +
		ggplot(data = proj_mean_lats_yr_hist) +
	 	geom_line(aes(year, proj_mean_lat, alpha = 0.5),
            data = . %>% filter(sp_hab_threshold == 0.5), color = "grey") 
		
   
	
	

	
	
	
	
	
	
	
	# mean lat 0.5
	mean_lat_yearly_plot <-    
   	ggplot() +
   	geom_line(data = hind_mean_lats_yr_df, 
   						aes(x = year, y = mean_lat_0.5), alpha = 0.7, color = "grey", size = 1) +
		geom_path(data = proj_mean_lats_yr_df_plot, 
							aes(year, mean_lat_0.5, color = sim_proj)) + 
		geom_vline(aes(xintercept = 2020.5)) +
	
  # mean_lat_yearly_plot_text <- mean_lat_yearly_plot +
	#	annotate(geom = "text", x = 2026, y = 57,
  #         label = paste("spawning\nhabitat\nsuitability", symbol, "0.5", sep = " "),
  #         color = "#7f7fbf", size = 4) +
  # 	annotate(geom = "text", x = 2026, y = 56,
  #         label = paste("spawning\nhabitat\nsuitability", symbol, "0.9", sep = " "),
  #         color = "#00345C", size = 4) 
   	
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
	
	#### month #### START HERE #################

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
	