
# comparing consistency in future (2020 - 2099)

# figure -- consistency year chunks
	
	#### with bias corrected temps at the monthly, regional level ####

	# projection
	
	last_yrs <- 2080:2099

	ROMS_projected_dat_dec <- ROMS_projected_dat %>% filter(., year %in% last_yrs)
	
	# number of years
	no_yrs <- length(unique(ROMS_projected_dat_dec$year))

	ROMS_projected_dat_yr_sum <- ROMS_projected_dat_dec %>%
		group_by(simulation, projection, year, latitude, longitude) %>%
		summarise(mean_sphabsuit = mean(sp_hab_suit_var))

	# 0.5
	ROMS_projected_dat_yr_sum_05 <- ROMS_projected_dat_yr_sum %>%
		group_by(simulation, projection, latitude, longitude) %>%
		dplyr::summarize(no_yrs = length(which(mean_sphabsuit >= 0.5))) %>%
		mutate(year_tot = 20,
					 pct_yrs = (no_yrs/year_tot) * 100)

	ROMS_projected_dat_yr_sum_05_sf <- ROMS_projected_dat_yr_sum_05 %>%
			mutate(long_not_360 = longitude - 360) %>%
	  	st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326)
  
	# 0.9
	ROMS_projected_dat_yr_sum_09 <- ROMS_projected_dat_yr_sum %>%
		group_by(simulation, projection,latitude, longitude) %>%
		dplyr::summarize(no_yrs = length(which(mean_sphabsuit >= 0.9))) %>%
		mutate(year_tot = 20,
					 pct_yrs = (no_yrs/year_tot) * 100)

	ROMS_projected_dat_yr_sum_09_sf <- ROMS_projected_dat_yr_sum_09 %>%
			mutate(long_not_360 = longitude - 360) %>%
	  	st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326)
	
	# add in scenario as factor
	ROMS_projected_dat_yr_sum_05_sf$scen <- NA
	
  ROMS_projected_dat_yr_sum_05_sf$scen[ROMS_projected_dat_yr_sum_05_sf$projection == "SSP126"] <- "low\nemission\n(SSP126)"
	ROMS_projected_dat_yr_sum_05_sf$scen[ROMS_projected_dat_yr_sum_05_sf$projection == "SSP585"] <- "high\nemission\n(SSP585)"
	
	ROMS_projected_dat_yr_sum_05_sf$scen_f = factor(ROMS_projected_dat_yr_sum_05_sf$scen, 
																									levels=c('low\nemission\n(SSP126)', 
																											     'high\nemission\n(SSP585)'))

	ROMS_projected_dat_yr_sum_09_sf$scen <- NA
	
  ROMS_projected_dat_yr_sum_09_sf$scen[ROMS_projected_dat_yr_sum_09_sf$projection == "SSP126"] <- "low\nemission\n(SSP126)"
	ROMS_projected_dat_yr_sum_09_sf$scen[ROMS_projected_dat_yr_sum_09_sf$projection == "SSP585"] <- "high\nemission\n(SSP585)"
	
	ROMS_projected_dat_yr_sum_09_sf$scen_f = factor(ROMS_projected_dat_yr_sum_09_sf$scen, 
																									levels=c('low\nemission\n(SSP126)', 
																											     'high\nemission\n(SSP585)'))

	# potential habitat

	last20_consistency05 <-	
		ggplot() +
		geom_sf(data = ROMS_projected_dat_yr_sum_05_sf, aes(color = pct_yrs))  +
		geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
		facet_grid(scen_f ~ simulation) +
		coord_sf(crs = 3338) +
		scale_color_viridis_c() +
 		scale_x_continuous(
 			breaks = breaks_x,
 			labels = labels_x,
 			name = "Longitude",
 			limits = limits_x
 		) +
 		scale_y_continuous(
 			breaks = c(55, 60),
 			limits = c(470000, 1900000),
 			name = "Latitude",
 		) +
    labs(colour = "%\nyears") +
		theme_bw() +
 			theme(
 			legend.position = "none",
 			panel.spacing = unit(2, "pt"),
 			panel.border = element_rect(color = "#666666"),
 			strip.text.x = element_text(size = 10, face = "bold",  color = "#808080"),
 			strip.text.y = element_text(size = 10, face = "bold", angle = 0,  color = "#666666"),
 			strip.background = element_blank(),
 			axis.text.x = element_text(size = 8,  color = "#666666"),	
  		axis.title.x = element_text(size = 10,  color = "#666666"),
 			axis.ticks.x = element_line( color = "#666666"),
 			plot.margin = unit(c(0.25,-0.05,-0.05, -0.1), "in"))
	
  ggsave("./output/plots/last20_consistency05.png",
			 last20_consistency05,
			 height = 5,
			 width = 10)


	# core habitat

	last20_consistency09 <-	
		ggplot() +
		geom_sf(data = ROMS_projected_dat_yr_sum_09_sf, aes(color = pct_yrs))  +
		geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
		facet_grid(scen_f ~ simulation) +
		coord_sf(crs = 3338) +
		scale_color_viridis_c() +
 		scale_x_continuous(
 			breaks = breaks_x,
 			labels = labels_x,
 			name = "Longitude",
 			limits = limits_x
 		) +
 		scale_y_continuous(
 			breaks = c(55, 60),
 			limits = c(470000, 1900000),
 			name = "Latitude",
 		) +
    labs(colour = "%\nyears") +
		theme_bw() +
 		theme(
 			legend.position = "none",
 			panel.spacing = unit(2, "pt"),
 			panel.border = element_rect(color = "#666666"),
 			strip.text.x = element_text(size = 10, face = "bold",  color = "#808080"),
 			strip.text.y = element_text(size = 10, face = "bold", angle = 0,  color = "#666666"),
 			strip.background = element_blank(),
 			axis.text.x = element_text(size = 8,  color = "#666666"),	
  		axis.title.x = element_text(size = 10,  color = "#666666"),
 			axis.ticks.x = element_line( color = "#666666"),
 			plot.margin = unit(c(0.25,-0.05,-0.05, -0.1), "in"))
	
	
  ggsave("./output/plots/last20_consistency09.png",
			 last20_consistency09,
			 height = 5,
			 width = 10)
  
  #### with bias corrected temps at the weekly, grid cell level ####
  
	# projection
	
	last_yrs <- 2080:2099

	ROMS_projected_dat_dec <- proj_mo_dfs %>% filter(., year %in% last_yrs)
	
	# number of years
	no_yrs <- length(unique(ROMS_projected_dat_dec$year))

	ROMS_projected_dat_yr_sum <- ROMS_projected_dat_dec %>%
		group_by(simulation, scenario, year, latitude, longitude) %>%
		summarise(mean_sphabsuit = mean(sp_hab_suit))

	# 0.5
	ROMS_projected_dat_yr_sum_05 <- ROMS_projected_dat_yr_sum %>%
		group_by(simulation, scenario, latitude, longitude) %>%
		dplyr::summarize(no_yrs = length(which(mean_sphabsuit >= 0.5))) %>%
		mutate(year_tot = 20,
					 pct_yrs = (no_yrs/year_tot) * 100)

	ROMS_projected_dat_yr_sum_05_sf <- ROMS_projected_dat_yr_sum_05 %>%
			mutate(long_not_360 = longitude - 360) %>%
	  	st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326)
  
	# 0.9
	ROMS_projected_dat_yr_sum_09 <- ROMS_projected_dat_yr_sum %>%
		group_by(simulation, scenario, latitude, longitude) %>%
		dplyr::summarize(no_yrs = length(which(mean_sphabsuit >= 0.9))) %>%
		mutate(year_tot = 20,
					 pct_yrs = (no_yrs/year_tot) * 100)

	ROMS_projected_dat_yr_sum_09_sf <- ROMS_projected_dat_yr_sum_09 %>%
			mutate(long_not_360 = longitude - 360) %>%
	  	st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326)
	
	# add in scenario as factor
	ROMS_projected_dat_yr_sum_05_sf$scen <- NA
	
  ROMS_projected_dat_yr_sum_05_sf$scen[ROMS_projected_dat_yr_sum_05_sf$scenario == "ssp126"] <- "low\nemission\n(SSP126)"
	ROMS_projected_dat_yr_sum_05_sf$scen[ROMS_projected_dat_yr_sum_05_sf$scenario == "ssp585"] <- "high\nemission\n(SSP585)"
	
	ROMS_projected_dat_yr_sum_05_sf$scen_f = factor(ROMS_projected_dat_yr_sum_05_sf$scen, 
																									levels=c('low\nemission\n(SSP126)', 
																											     'high\nemission\n(SSP585)'))

	ROMS_projected_dat_yr_sum_09_sf$scen <- NA
	
  ROMS_projected_dat_yr_sum_09_sf$scen[ROMS_projected_dat_yr_sum_09_sf$scenario == "ssp126"] <- "low\nemission\n(SSP126)"
	ROMS_projected_dat_yr_sum_09_sf$scen[ROMS_projected_dat_yr_sum_09_sf$scenario == "ssp585"] <- "high\nemission\n(SSP585)"
	
	ROMS_projected_dat_yr_sum_09_sf$scen_f = factor(ROMS_projected_dat_yr_sum_09_sf$scen, 
																									levels=c('low\nemission\n(SSP126)', 
																											     'high\nemission\n(SSP585)'))

	# potential habitat

	last20_consistency05_wkgc <-	
		ggplot() +
		geom_sf(data = ROMS_projected_dat_yr_sum_05_sf, aes(color = pct_yrs))  +
		geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
		facet_grid(scen_f ~ simulation) +
		coord_sf(crs = 3338) +
		scale_color_viridis_c() +
 		scale_x_continuous(
 			breaks = breaks_x,
 			labels = labels_x,
 			name = "Longitude",
 			limits = limits_x
 		) +
 		scale_y_continuous(
 			breaks = c(55, 60),
 			limits = c(470000, 1900000),
 			name = "Latitude",
 		) +
    labs(colour = "%\nyears") +
		theme_bw() +
 			theme(
 			legend.position = "none",
 			panel.spacing = unit(2, "pt"),
 			panel.border = element_rect(color = "#666666"),
 			strip.text.x = element_text(size = 10, face = "bold",  color = "#808080"),
 			strip.text.y = element_text(size = 10, face = "bold", angle = 0,  color = "#666666"),
 			strip.background = element_blank(),
 			axis.text.x = element_text(size = 8,  color = "#666666"),	
  		axis.title.x = element_text(size = 10,  color = "#666666"),
 			axis.ticks.x = element_line( color = "#666666"),
 			plot.margin = unit(c(0.25,-0.05,-0.05, -0.1), "in"))
	
 ggsave(here("./scripts/with weekly grid cell level bias correct temps/last20_consistency05_wkgc.png"),
			 last20_consistency05_wkgc,
			 height = 5,
			 width = 10)


	# core habitat

	last20_consistency09_wkgc <-	
		ggplot() +
		geom_sf(data = ROMS_projected_dat_yr_sum_09_sf, aes(color = pct_yrs))  +
		geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
		facet_grid(scen_f ~ simulation) +
		coord_sf(crs = 3338) +
		scale_color_viridis_c() +
 		scale_x_continuous(
 			breaks = breaks_x,
 			labels = labels_x,
 			name = "Longitude",
 			limits = limits_x
 		) +
 		scale_y_continuous(
 			breaks = c(55, 60),
 			limits = c(470000, 1900000),
 			name = "Latitude",
 		) +
    labs(colour = "%\nyears") +
		theme_bw() +
 		theme(
 			legend.position = "none",
 			panel.spacing = unit(2, "pt"),
 			panel.border = element_rect(color = "#666666"),
 			strip.text.x = element_text(size = 10, face = "bold",  color = "#808080"),
 			strip.text.y = element_text(size = 10, face = "bold", angle = 0,  color = "#666666"),
 			strip.background = element_blank(),
 			axis.text.x = element_text(size = 8,  color = "#666666"),	
  		axis.title.x = element_text(size = 10,  color = "#666666"),
 			axis.ticks.x = element_line( color = "#666666"),
 			plot.margin = unit(c(0.25,-0.05,-0.05, -0.1), "in"))
	
	
  ggsave(here("./scripts/with weekly grid cell level bias correct temps/last20_consistency09_wkgc.png"),
			 last20_consistency09_wkgc,
			 height = 5,
			 width = 10)
