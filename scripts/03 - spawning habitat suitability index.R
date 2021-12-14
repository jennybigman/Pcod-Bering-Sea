	#### index of suitable spawning habitat ####
	
	yearly_hab_dat_hind <- ROMS_hindcast_dat %>%
		group_by(year) %>%
    summarise(annual_hatch_success_cauchy = mean(hatch_success_cauchy),
   					  annual_hatch_success_gaussian = mean(hatch_success_gaus),
    					annual_spawning_hab_suit = mean(sp_hab_suit))
	
	yearly_hab_dat_proj <- ROMS_projected_dat %>% 
		group_by(simulation, projection, year) %>%
   	summarise(annual_hatch_success_cauchy = mean(hatch_success_cauchy),
   					  annual_hatch_success_gaussian = mean(hatch_success_gaus),
   						annual_spawning_hab_suit = mean(sp_hab_suit)) 
	
	# create interaction variable for plotting different colors
	
	yearly_hab_dat_proj <- yearly_hab_dat_proj %>%
		tidyr::unite("sim_proj", simulation, projection, remove = F)

	colors <- c("lightgrey", "#efd966", "#b79a00", 
						  "lightgrey", "#7fb27f", "#004700", 
						  "lightgrey", "#6666b2", "#000059")

	sim_proj <- unique(yearly_hab_dat_proj$sim_proj)
	
	names(colors) <- unique(yearly_hab_dat_proj$sim_proj)
	
	annual_hatch_success_cauchy <-    
   	ggplot() +
   	geom_line(data = yearly_hab_dat_hind, 
   						aes(x = year, y = annual_spawning_hab_suit), 
   						color = "black", alpha = 0.5) +
		geom_line(data = yearly_hab_dat_proj,
							aes(year, annual_spawning_hab_suit, 
									group = sim_proj, 
									color = sim_proj), alpha = 0.5) +
		facet_wrap(~ simulation) +
		xlab("Year") + 
		scale_color_manual(name = "sim_proj", values = colors) +
	  scale_y_continuous(
	  	name = "Annual spawning habitat suitability",
	  	breaks = c(0.20, 0.40, 0.60, 0.80),
	  ) +
		scale_x_continuous(
	  	name = "Year",
	  	breaks = c(1980, 2030, 2080)) +
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
	
	
		ggsave("./output/plots/annual_hatch_success_cauchy_hindproj.png",
			 annual_hatch_success_cauchy,
			 width = 15, height = 5, units = "in")
	
	## monthly
	
	mo_hab_dat_hind <- ROMS_hindcast_dat %>%
		group_by(year, month_name, month) %>%
    summarise(avg_hatch_success_cauchy = mean(hatch_success_cauchy),
   					  avg_hatch_success_gaussian = mean(hatch_success_gaus),
    					avg_spawning_hab_suit = mean(sp_hab_suit))
	
	mo_hab_dat_proj <- ROMS_projected_dat %>% 
		group_by(simulation, projection, year, month_name, month) %>%
   	summarise(avg_hatch_success_cauchy = mean(hatch_success_cauchy),
   					  avg_hatch_success_gaussian = mean(hatch_success_gaus),
   						avg_spawning_hab_suit = mean(sp_hab_suit)) 
		
	
	mo_hab_dat_proj <- mo_hab_dat_proj %>%
		tidyr::unite("sim_proj", simulation, projection, remove = F)

	colors <- c("lightgrey", "#efd966", "#b79a00", 
						  "lightgrey", "#7fb27f", "#004700", 
						  "lightgrey", "#6666b2", "#000059")
	
	sim_proj <- unique(mo_hab_dat_proj$sim_proj)
	
	names(colors) <- unique(mo_hab_dat_proj$sim_proj)
	
	# reorder for plotting
	mo_hab_dat_hind$month_name <- factor(mo_hab_dat_hind$month_name)
  mo_hab_dat_hind$month_name <- fct_reorder(mo_hab_dat_hind$month_name, 
  																		mo_hab_dat_hind$month)
  
  mo_hab_dat_proj$month_name <- factor(mo_hab_dat_proj$month_name)
  mo_hab_dat_proj$month_name <- fct_reorder(mo_hab_dat_proj$month_name, 
  																		mo_hab_dat_proj$month)

	mo_hatch_success_cauchy <-    
   	ggplot() +
   	geom_line(data = mo_hab_dat_hind, 
   						aes(x = year, y = avg_spawning_hab_suit), 
   						color = "black", alpha = 0.5) +
		geom_line(data = mo_hab_dat_proj,
							aes(year, avg_spawning_hab_suit, 
									group = sim_proj, 
									color = sim_proj), alpha = 0.5) +
		facet_wrap(~ simulation + month_name) +
		xlab("Year") + 
		scale_color_manual(name = "sim_proj", values = colors) +
	  scale_y_continuous(
	  	name = "Mean spawning habitat suitability",
	  	breaks = c(0.20, 0.40, 0.60, 0.80),
	  ) +
		scale_x_continuous(
	  	name = "Year",
	  	breaks = c(1980, 2030, 2080)) +
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
	
	
		ggsave("./output/plots/mo_hatch_success_cauchy_hindproj.png",
			 mo_hatch_success_cauchy,
			 width = 10, height = 7, units = "in")
	
		
		
		
		
		
		
		
		
		
		
		

	#### comparing year bins: 2000 - 2014 vs. 2015 - 2020 ####
	
	years_keep <- c(2000:2020)
  	
  ROMS_hindcast_dat_sub <- ROMS_hindcast_dat_sf %>% filter(., year %in% years_keep)
  

	# create a list of year bins
	year_bins  <- list('2000-2014' = c(2000:2014),
                     '2015-2020' = c(2015:2020)) 

	# create a list of dfs summarized by year bins
	sum_yr_bin <- function(x){
 
		df <- ROMS_hindcast_dat_sub %>%
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
 
	sm_temp_hind_df_yr_sum_sf <- sm_temp_hind_df_yr_sum %>%
			mutate(long_not_360 = longitude - 360) %>%
  		st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326)
  
	# plot
	
	plot <- ggplot() +
					geom_sf(data = sm_temp_hind_df_yr_sum_sf, aes(color = mean_sphabsuit))  +
					geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
					coord_sf(crs = 3338) +
  		    facet_wrap(~ time_period, nrow = 1) +
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
    	  	labs(colour = "Spawning\nhabitat suitability") +
					theme_bw() +
 					theme(
						strip.text = element_text(size = 14, face = "bold"),
 						strip.background = element_blank(), 						
						axis.text = element_text(size = 12),	
  					axis.title = element_text(size = 14),
  					legend.title.align=0.5)
	
		ggsave("./output/plots/historic_vs_recent_shs.png",
			 plot,
			 width = 10, height = 7, units = "in")

