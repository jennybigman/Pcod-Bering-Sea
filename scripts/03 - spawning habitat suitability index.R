	#### index of suitable spawning habitat ####
	
	yearly_hab_dat_hind <- ROMS_hindcast_dat %>%
		group_by(year) %>%
    summarise(annual_hatch_success_cauchy = mean(hatch_success_cauchy),
   					  annual_hatch_success_gaussian = mean(hatch_success_gaus),
    					annual_spawning_hab_suit = mean(sp_hab_suit))
	
	yearly_hab_dat_proj <- ROMS_projected_dat %>% 
		group_by(simulation, projection, year) %>%
   	summarise(annual_hatch_success_cauchy_var = mean(hatch_success_cauchy_var),
   						annual_hatch_success_gaussian_var = mean(hatch_success_gaus_var),
   						annual_spawning_hab_suit_var = mean(sp_hab_suit_var)) 
	
	# create interaction variable for plotting different colors
	
	yearly_hab_dat_proj <- yearly_hab_dat_proj %>%
		tidyr::unite("sim_proj", simulation, projection, remove = F)

	colors <- c("lightgrey", "#efd966", "#b79a00", 
						  "lightgrey", "#7fb27f", "#004700", 
						  "lightgrey", "#6666b2", "#000059")

	sim_proj <- unique(yearly_hab_dat_proj$sim_proj)
	
	names(colors) <- unique(yearly_hab_dat_proj$sim_proj)
	
	# with bias-corrected temp with variance ratio
		
	annual_hatch_success_cauchy_var <-    
   	ggplot() +
   	geom_line(data = yearly_hab_dat_hind, 
   						aes(x = year, y = annual_spawning_hab_suit), 
   						color = "black", alpha = 0.5) +
		geom_line(data = yearly_hab_dat_proj,
							aes(year, annual_spawning_hab_suit_var, 
									group = sim_proj, 
									color = sim_proj), alpha = 0.5) +
		facet_wrap(~ simulation) +
		xlab("Year") + 
		scale_color_manual(name = "sim_proj", values = colors) +
	  scale_y_continuous(
	  	name = "Annual spawning\nhabitat suitability\nvar",
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
	
	
		ggsave("./output/plots/annual_hatch_success_cauchy_hindproj_var.png",
			 annual_hatch_success_cauchy_var,
			 width = 15, height = 5, units = "in")
		
	## facet by scenario
	
	yearly_hab_dat_proj <- yearly_hab_dat_proj %>% filter(., projection != "historical")
		
	yearly_hab_dat_proj$scen <- NA
		
	yearly_hab_dat_proj$scen[yearly_hab_dat_proj$projection == "ssp126"] <- "low emission (ssp126)"
	yearly_hab_dat_proj$scen[yearly_hab_dat_proj$projection == "ssp585"] <- "high emission (ssp585)"
	
	yearly_hab_dat_proj <- tidyr::unite(yearly_hab_dat_proj,"sim_proj",
															 simulation, projection, remove = F)

	colors <- c("#6dc3a9", "#ff7373", # cesm low, cesm high
						  "#4e8d9c", "#ff4040", # gfdl low, gfdl high
						  "#97c3e5", "#ffb733") # miroc low, miroc high

	sim_proj <- unique(yearly_hab_dat_proj$sim_proj)
	
	names(colors) <- unique(yearly_hab_dat_proj$sim_proj)
	
	# order facets
	yearly_hab_dat_proj$scen_f = factor(yearly_hab_dat_proj$scen, levels=c('low emission (ssp126)', 
																																				 'high emission (ssp585)'))
	
	#plot
	annual_hatch_success_cauchy <-    
   	ggplot() +
   	geom_line(data = yearly_hab_dat_hind, 
   						aes(x = year, y = annual_spawning_hab_suit), 
   						color = "black", alpha = 0.5) +
		geom_line(data = yearly_hab_dat_proj,
							aes(year, annual_spawning_hab_suit_var, 
									group = sim_proj, 
									color = sim_proj), alpha = 0.5) +
		facet_wrap(~ scen_f) +
		xlab("Year") + 
		scale_color_manual(name = "sim_proj", values = colors) +
	  scale_y_continuous(
	  	name = "Cross-shelf annual spawning\nhabitat suitability index",
	  	breaks = c(0.20, 0.40, 0.60),
	  ) +
		scale_x_continuous(
	  	name = "Year",
	  	breaks = c(1980, 2030, 2080, 2150)) +
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
	
		ggsave("./output/plots/annual_hatch_success_cauchy.png",
			 annual_hatch_success_cauchy,
			 width = 10, height = 4.5, units = "in")
		
	# plot separately to add labels
		
	low_dat <- yearly_hab_dat_proj %>%
		filter(., scen == "low emission (ssp126)")
	
	low_habsuit_time <-    
   	ggplot() +
   	geom_line(data = yearly_hab_dat_hind, 
   						aes(x = year, y = annual_spawning_hab_suit), 
   						color = "black", alpha = 0.5) +
		geom_line(data = low_dat,
							aes(year, annual_spawning_hab_suit_var, 
									group = sim_proj, 
									color = sim_proj), alpha = 0.5) +
		xlab("Year") + 
		scale_color_manual(name = "sim_proj", values = colors) +
	  scale_y_continuous(
	  	name = "Cross-shelf annual spawning\nhabitat suitability index",
	  	breaks = c(0.20, 0.40, 0.60),
	  	labels = c(0.20, 0.40, 0.60),
	  	limits = c(0.2, 0.65)
	  ) +
		scale_x_continuous(
	  	name = "Year",
	  	breaks = c(1980, 2030, 2080),
	  		  	limits = c(1970, 2110)) +
		ggtitle("low emission (ssp126)") +
   	theme_bw() +
  	theme(legend.position = "none") +
  	theme(
  		plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
			axis.text = element_text(size = 12, colour = "grey50"),
  	  axis.ticks = element_line(colour = "grey50"),
  	  axis.line = element_line(colour = "grey50"),
  	  axis.title = element_text(size=14, color = "grey30"),
  	  panel.grid.major = element_blank(),
  	  panel.grid.minor = element_blank(),
  	  panel.border = element_rect(fill = NA, color = "grey50"))
	
	low_habsuit_time_lab <- low_habsuit_time +
			annotate(geom = "text", x = 2107, y = 0.45,
           label = "cesm",
           color = "#6dc3a9", size = 4) +
				annotate(geom = "text", x = 2107, y = 0.30,
           label = "gfdl",
           color = "#4e8d9c", size = 4) +
				annotate(geom = "text", x = 2107, y = 0.37,
           label = "miroc",
           color = "#97c3e5", size = 4)  +
				annotate(geom = "text", x = 1995, y = 0.39,
           label = "hindcast",
           color = "black", alpha = 0.5, size = 4)

	high_dat <- yearly_hab_dat_proj %>%
		filter(., scen == "high emission (ssp585)")
	
	high_habsuit_time <-    
   	ggplot() +
   	geom_line(data = yearly_hab_dat_hind, 
   						aes(x = year, y = annual_spawning_hab_suit), 
   						color = "black", alpha = 0.5) +
		geom_line(data = high_dat,
							aes(year, annual_spawning_hab_suit_var, 
									group = sim_proj, 
									color = sim_proj), alpha = 0.5) +
		xlab("Year") + 
		scale_color_manual(name = "sim_proj", values = colors) +
	  scale_y_continuous(
	  	name = "Cross-shelf annual spawning\nhabitat suitability index",
	  	breaks = c(0.20, 0.40, 0.60),
	  	labels = c(0.20, 0.40, 0.60),
	  	limits = c(0.2, 0.65)
	  ) +
		scale_x_continuous(
	  	name = "Year",
	  	breaks = c(1980, 2030, 2080),
	  	limits = c(1970, 2110)) +
   	theme_bw() +
  	theme(legend.position = "none") +
		ggtitle("high emission (ssp585)") +
  	theme(
  		plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
			axis.text.x = element_text(size = 12, colour = "grey50"),
  	  axis.ticks.x = element_line(colour = "grey50"),
  	  axis.line = element_line(colour = "grey50"),
  	  axis.title.x = element_text(size=14, color = "grey30"),
			axis.text.y = element_blank(),
			axis.ticks.y = element_blank(),
			axis.title.y = element_blank(),
  	  panel.grid.major = element_blank(),
  	  panel.grid.minor = element_blank(),
  	  panel.border = element_rect(fill = NA, color = "grey50"))
	
	high_habsuit_time_lab <- high_habsuit_time +
			annotate(geom = "text", x = 2107, y = 0.46,
           label = "cesm",
           color = "#ff7373", size = 4) +
				annotate(geom = "text", x = 2107, y = 0.5,
           label = "gfdl",
           color = "#ff4040", size = 4) +
				annotate(geom = "text", x = 2107, y = 0.62,
           label = "miroc",
           color = "#ffb733", size = 4) +
				annotate(geom = "text", x = 1994, y = 0.39,
           label = "hindcast",
           color = "black", alpha = 0.5, size = 4)
		
	# plot together
	
	library(patchwork)
	
	plot1 <- low_habsuit_time_lab + theme(plot.margin = unit(c(0.2, 0, 0.2, 0.2), "in"))
	plot2 <- high_habsuit_time_lab + theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0), "in"))

		
	time_series_habsuit <- plot1 + plot2 +
		plot_layout( widths = c(1, 1.1))
	
	ggsave("./output/plots/time_series_habsuit.png",
			 time_series_habsuit,
			 width = 15, height = 4.5, units = "in")
	
	
	
	### on top of each other
	
			
	low_dat <- yearly_hab_dat_proj %>%
		filter(., scen == "low emission (ssp126)")
	
	low_habsuit_time <-    
   	ggplot() +
   	geom_line(data = yearly_hab_dat_hind, 
   						aes(x = year, y = annual_spawning_hab_suit), 
   						color = "black", alpha = 0.5) +
		geom_line(data = low_dat,
							aes(year, annual_spawning_hab_suit_var, 
									group = sim_proj, 
									color = sim_proj), alpha = 0.5) +
		xlab("Year") + 
		scale_color_manual(name = "sim_proj", values = colors) +
	  scale_y_continuous(
	  	name = "Cross-shelf annual spawning\nhabitat suitability index",
	  	breaks = c(0.20, 0.40, 0.60),
	  	labels = c(0.20, 0.40, 0.60),
	  	limits = c(0.2, 0.65)
	  ) +
		scale_x_continuous(
	  	name = "Year",
	  	breaks = c(1980, 2030, 2080),
	  		  	limits = c(1970, 2110)) +
   	theme_bw() +
  	theme(legend.position = "none") +
  	theme(
  		plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
			axis.text.y = element_text(size = 14, colour = "grey50"),
  	  axis.ticks.y = element_line(colour = "grey50"),
  	  axis.line.y = element_line(colour = "grey50"),
  	  axis.title.y = element_text(size=16, color = "grey30"),
			axis.text.x = element_blank(),
			axis.ticks.x = element_blank(),
			axis.title.x = element_blank(),
  	  panel.grid.major = element_blank(),
  	  panel.grid.minor = element_blank(),
  	  panel.border = element_rect(fill = NA, color = "grey50"))
	
	low_habsuit_time_lab <- low_habsuit_time +
			annotate(geom = "text", x = 2107, y = 0.45,
           label = "cesm",
           color = "#6dc3a9", size = 4.5) +
				annotate(geom = "text", x = 2107, y = 0.30,
           label = "gfdl",
           color = "#4e8d9c", size = 4.5) +
				annotate(geom = "text", x = 2107, y = 0.37,
           label = "miroc",
           color = "#97c3e5", size = 4.5)  +
				annotate(geom = "text", x = 1995, y = 0.39,
           label = "hindcast",
           color = "black", alpha = 0.5, size = 4.5) +
				annotate(geom = "text", x = 1990, y = 0.65,
           label = "low emission (ssp126)",
           color = "black", size = 5)

	high_dat <- yearly_hab_dat_proj %>%
		filter(., scen == "high emission (ssp585)")
	
	high_habsuit_time <-    
   	ggplot() +
   	geom_line(data = yearly_hab_dat_hind, 
   						aes(x = year, y = annual_spawning_hab_suit), 
   						color = "black", alpha = 0.5) +
		geom_line(data = high_dat,
							aes(year, annual_spawning_hab_suit_var, 
									group = sim_proj, 
									color = sim_proj), alpha = 0.5) +
		xlab("Year") + 
		scale_color_manual(name = "sim_proj", values = colors) +
	  scale_y_continuous(
	  	name = "Cross-shelf annual spawning\nhabitat suitability index",
	  	breaks = c(0.20, 0.40, 0.60),
	  	labels = c(0.20, 0.40, 0.60),
	  	limits = c(0.2, 0.65)
	  ) +
		scale_x_continuous(
	  	name = "Year",
	  	breaks = c(1980, 2030, 2080),
	  	limits = c(1970, 2110)) +
   	theme_bw() +
  	theme(legend.position = "none") +
  	theme(
  		plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
			axis.text = element_text(size = 14, colour = "grey50"),
  	  axis.ticks = element_line(colour = "grey50"),
  	  axis.line = element_line(colour = "grey50"),
  	  axis.title = element_text(size=16, color = "grey30"),
  	  panel.grid.major = element_blank(),
  	  panel.grid.minor = element_blank(),
  	  panel.border = element_rect(fill = NA, color = "grey50"))
	
	high_habsuit_time_lab <- high_habsuit_time +
			annotate(geom = "text", x = 2107, y = 0.46,
           label = "cesm",
           color = "#ff7373", size = 4.5) +
				annotate(geom = "text", x = 2107, y = 0.5,
           label = "gfdl",
           color = "#ff4040", size = 4.5) +
				annotate(geom = "text", x = 2107, y = 0.62,
           label = "miroc",
           color = "#ffb733", size = 4.5) +
				annotate(geom = "text", x = 1994, y = 0.39,
           label = "hindcast",
           color = "black", alpha = 0.5, size = 4.5) +
			annotate(geom = "text", x = 1990, y = 0.65,
           label = "high emission (ssp585)",
           color = "black", size = 5)

		
	# plot together
	
	library(patchwork)
	
	plot1 <- low_habsuit_time_lab + theme(plot.margin = unit(c(0.2, 0.2, 0, 0.2), "in"))
	plot2 <- high_habsuit_time_lab + theme(plot.margin = unit(c(0, 0.2, 0.2, 0.2), "in"))

		
	time_series_habsuit_top <- plot1 / plot2 +
		plot_layout( widths = c(1.1, 1))
	
	ggsave(here("./output/plots/time_series_habsuit_top.png"),
			 time_series_habsuit_top,
			 width = 10, height = 10, units = "in")
	
	
	## monthly
	
	mo_hab_dat_hind <- ROMS_hindcast_dat %>%
		group_by(year, month_name, month) %>%
    summarise(avg_hatch_success_cauchy = mean(hatch_success_cauchy),
   					  avg_hatch_success_gaussian = mean(hatch_success_gaus),
    					avg_spawning_hab_suit = mean(sp_hab_suit))
	
	mo_hab_dat_proj <- ROMS_projected_dat %>% 
		group_by(simulation, projection, year, month_name, month) %>%
   	summarise(avg_hatch_success_cauchy_var = mean(hatch_success_cauchy_var),
   						avg_hatch_success_gaussian_var = mean(hatch_success_gaus_var),
   						avg_spawning_hab_suit_var = mean(sp_hab_suit_var)) 
		
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
		
  # with bias-corrected temp with variance ratio
  
	mo_hatch_success_cauchy_var <-    
   	ggplot() +
   	geom_line(data = mo_hab_dat_hind, 
   						aes(x = year, y = avg_spawning_hab_suit), 
   						color = "black", alpha = 0.5) +
		geom_line(data = mo_hab_dat_proj,
							aes(year, avg_spawning_hab_suit_var, 
									group = sim_proj, 
									color = sim_proj), alpha = 0.5) +
		facet_wrap(~ simulation + month_name) +
		xlab("Year") + 
		scale_color_manual(name = "sim_proj", values = colors) +
	  scale_y_continuous(
	  	name = "Mean spawning\nhabitat suitability\nvar",
	  	breaks = c(0.20, 0.40, 0.60, 0.80),
	  ) +
		scale_x_continuous(
	  	name = "Year",
	  	breaks = c(1980, 2030, 2080)) +
   	theme_bw() +
  	theme(legend.position = "none") +
  	theme(
			strip.background = element_blank(),
  		strip.text = element_text(size = 16, face = "bold"),
			axis.text = element_text(size = 16, colour = "grey50"),
  	  axis.ticks = element_line(colour = "grey50"),
  	  axis.line = element_line(colour = "grey50"),
  	  axis.title = element_text(size=18, color = "grey30"),
  	  panel.grid.major = element_blank(),
  	  panel.grid.minor = element_blank(),
  	  panel.border = element_rect(fill = NA, color = "grey50"))
	
	
		ggsave("./output/plots/mo_hatch_success_cauchy_hindproj_var.png",
			 mo_hatch_success_cauchy_var,
			 width = 10, height = 7, units = "in")
		
	#### temp ####
		
	# monthly
		
	mo_temp_hind <- ROMS_hindcast_dat %>%
		group_by(year, month_name, month) %>%
    summarise(avg_temp = mean(temp))
	
	mo_temp_proj <- ROMS_projected_dat %>% 
		group_by(simulation, projection, year, month_name, month) %>%
   	summarise(avg_temp_novar = mean(bc_temp),
   						avg_temp_var = mean(bc_temp_sd)) 
		
	mo_temp_proj <- mo_temp_proj %>%
		tidyr::unite("sim_proj", simulation, projection, remove = F)

	colors <- c("lightgrey", "#efd966", "#b79a00", 
						  "lightgrey", "#7fb27f", "#004700", 
						  "lightgrey", "#6666b2", "#000059")
	
	sim_proj <- unique(mo_temp_proj$sim_proj)
	
	names(colors) <- unique(mo_temp_proj$sim_proj)
	
	# reorder for plotting
	mo_temp_hind$month_name <- factor(mo_temp_hind$month_name)
  mo_temp_hind$month_name <- fct_reorder(mo_temp_hind$month_name, 
  																		mo_temp_hind$month)
  
  mo_temp_proj$month_name <- factor(mo_temp_proj$month_name)
  mo_temp_proj$month_name <- fct_reorder(mo_temp_proj$month_name, 
  																		mo_temp_proj$month)

	# with bias-corrected temp with variance ratio

	mo_temp_var <-    
   	ggplot() +
   	geom_line(data = mo_temp_hind, 
   						aes(x = year, y = avg_temp), 
   						color = "black", alpha = 0.5) +
		geom_line(data = mo_temp_proj,
							aes(year, avg_temp_var, 
									group = sim_proj, 
									color = sim_proj), alpha = 0.5) +
		facet_wrap(~ simulation + month_name) +
		xlab("Year") + 
		scale_color_manual(name = "sim_proj", values = colors) +
	  scale_y_continuous(
	  	name = "Mean projected temp\nvar",
	  	breaks = c(0, 2, 4, 6),
	  ) +
		scale_x_continuous(
	  	name = "Year",
	  	breaks = c(1980, 2030, 2080)) +
   	theme_bw() +
  	theme(legend.position = "none") +
  	theme(
			strip.background = element_blank(),
  		strip.text = element_text(size = 16, face = "bold"),
			axis.text = element_text(size = 16, colour = "grey50"),
  	  axis.ticks = element_line(colour = "grey50"),
  	  axis.line = element_line(colour = "grey50"),
  	  axis.title = element_text(size=18, color = "grey30"),
  	  panel.grid.major = element_blank(),
  	  panel.grid.minor = element_blank(),
  	  panel.border = element_rect(fill = NA, color = "grey50"))
	
	
		ggsave("./output/plots/mo_temp_novar_hindproj_var.png",
			 mo_temp_var,
			 width = 10, height = 7, units = "in")
		
		
		
		
	#### code below needs to be organized ####
		
		
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

