
# prep data (summary) ####

	ROMS_hindcast_dat <- fread(file = "./data/ROMS_hindcast_dat.csv") %>% filter(., year != 2021)
	
	ROMS_hindcast_dat_sum_sf <- ROMS_hindcast_dat %>%
		group_by(latitude, long_not_360) %>%
		summarise(mean_temp = mean(temp))  %>%
  	st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326, remove = FALSE)

	ROMS_dat_hind <- fread(file = "./data/ROMS_hindcast_all.csv") %>% filter(., year != 2021)

	summer_mo <- c(6:8)

	ROMS_summer_hind <-ROMS_dat_hind %>%
		filter(month %in% summer_mo) %>%
		mutate(long_not_360 = case_when(
					 longitude >= 180 ~ longitude - 360,
					 longitude < 180 ~ longitude))
	
	ROMS_summer_hind <- ROMS_summer_hind %>% 
			filter(., long_not_360 %in% ROMS_hindcast_dat_sum_sf$long_not_360) %>%
			filter(., latitude %in% ROMS_hindcast_dat_sum_sf$latitude)
	
ROMS_summer_hind_sum_ll <- ROMS_summer_hind %>%
	group_by(latitude, long_not_360) %>%
	summarise(mean_temp = mean(temp))  %>%
  	st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326, remove = FALSE)


# thermal response curve from Laurel & Rogers 2020 #####

#gaus <- function(x) v[3]*exp(-1/2*(x-v[1])^2/v[2]^2)
cauchy <- function(x) v2[3]/(1+((x-v2[1])/v2[2])^2)


therm_response_cuve <- 
	ggplot(eggs) +
	geom_point(aes(x = Temp_act, y = Phatch), color = "white", size = 3) +
#	geom_function(fun = gaus, aes(col = "Temp_act"), color = "white", size = 2) +
	geom_function(fun = cauchy, aes(col = "Temp_act"), color = "white", size = 2) +
	xlab("Temperature ˚C") +
	ylab("Proportion successful hatch") +
	theme(
		legend.position = "none",
  				axis.text =element_text(size= 22, colour = "white"),
  				axis.title = element_text(size=24, color = "white"),
  				axis.line = element_line(color = "white"),
  				axis.ticks = element_line(colour = "white"),
  				panel.background = element_rect(fill = "black"),
					panel.grid = element_blank(),
  				plot.background = element_rect(fill = "black", color = "black"))

 ggsave(here("./output/plots/therm_response_cuve.png"),
		therm_response_cuve,
		width = 10, height = 7, units = "in")
 
 
# just map for title slide ####

Bering_map <- ggplot() +
	geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
					coord_sf(crs = 3338) +
					scale_color_viridis_c() +
	scale_x_continuous(
 						breaks = c(180, -175, -170, -165, -160, -155),
 						labels = c("180˚", "-175˚", "-170˚", "-165˚", "-160˚", "-155˚"),
 						name = "Longitude",
 						limits = c(-1500000, -100),
 					) +
 	scale_y_continuous(
 						breaks = c(55, 60, 65, 70),
 						limits = c(450000, 2500000),
 						name = "Latitude",
 					) +
  	theme(legend.position = c()) +
  	theme(
  	  axis.text=element_text(size= 12, colour = "white"),
  	  axis.title= element_text(size=16, color = "white"),
  	  axis.line = element_line(color = "white"),
  	  axis.ticks = element_line(colour = "white"),
  	  legend.title = element_text(color = "white"),
  	  legend.text = element_text(color = "white"),
  	  panel.grid = element_line(color = "lightgrey"),
  	  panel.background = element_rect(fill = "black"),
  	  plot.background = element_rect(fill = "black", color = "black"))

ggsave(here("./Bering_map.png"),
			 Bering_map,
			 width = 10, height = 7, units = "in")


# gradient of color of ROMS temps over Bering Sea shelf ####

ROMS_temps <- ggplot() +
	geom_sf(data = ROMS_hindcast_dat_sum_sf, aes(color = mean_temp)) +
	geom_sf(data = world_map_data, fill = "lightgrey", lwd = 0) +
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
    	  	labs(colour = "Bottom\ntemperature (˚C)") +
					theme(legend.position = c(),
								legend.background = element_rect(fill = "black"),
  						  axis.text=element_text(size= 12, colour = "white"),
  						  axis.title= element_text(size=16, color = "white"),
  						  axis.line = element_line(color = "white"),
  						  axis.ticks = element_line(colour = "white"),
  						  legend.title = element_text(color = "white"),
  						  legend.text = element_text(color = "white"),
  						  panel.background = element_rect(fill = "black"),
  						  plot.background = element_rect(fill = "black", color = "black"),
								legend.title.align = 0.5)

ROMS_temps_align <- ggdraw(align_legend(ROMS_temps))

ggsave(here("./ROMS_TEMPS_PLOT_BLACK.png"),
			 ROMS_temps_align,
			 width = 7, height = 7, units = "in")


# same but all white for area slide ####

dat2006cesm <- ROMS_projected_dat %>%
	filter(simulation =="cesm") %>%
	filter(year == 2006) %>%
	group_by(latitude, longitude) %>%
	summarize(mean_hs = mean(sp_hab_suit_var)) %>%
  mutate(long_not_360 = case_when(
				 longitude >= 180 ~ longitude - 360,
				 longitude < 180 ~ longitude)) %>%
  st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326, remove = FALSE)


ROMS_temps_white <- ggplot() +
	geom_sf(data = dat2006cesm, aes(color = mean_hs)) +
	geom_sf(data = world_map_data, fill = "lightgrey", lwd = 0) +
					coord_sf(crs = 3338) +
	scale_color_gradient(low = "white", high = "white") +
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
  black_map_theme_no_legend()

ggsave(here("./output/plots/ROMS_TEMPS_area.png"),
			 ROMS_temps_white,
			 width = 7, height = 7, units = "in")


# to show > 0.9 spawning habitat suitability

dat2006cesm <- ROMS_projected_dat %>%
	filter(simulation =="cesm") %>%
	filter(year == 2006) %>%
	group_by(latitude, longitude) %>%
	summarize(mean_hs = mean(sp_hab_suit_var)) %>%
  mutate(long_not_360 = case_when(
				 longitude >= 180 ~ longitude - 360,
				 longitude < 180 ~ longitude)) %>%
  st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326, remove = FALSE)
	

 ex_sphabsuit09 <- 
    	  	ggplot() +
					geom_sf(data = dat2006cesm, aes(color = mean_hs))  +
					geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
					coord_sf(crs = 3338) +
					scale_color_gradientn(colors = c("#B3E5FC", "#B3E5FC", 
																					 "#01579B", "#01579B"),
																values = c(0, 0.899, 0.9, 1),
																breaks = c(0.1, 0.5, 0.9),
																labels = format(c(0.1, 0.5, 0.9)),
																limits = c(0, 1))  +
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
 				  black_map_theme_no_legend()

 ggsave(here("./output/plots/ex_sphabsuit09.png"),
			 ex_sphabsuit09,
			 width = 7, height = 7, units = "in")

 
### show 05 and 09 
 
 # to show > 0.9 spawning habitat suitability

dat2006cesm <- ROMS_projected_dat %>%
	filter(simulation =="cesm") %>%
	filter(year == 2006) %>%
	group_by(latitude, longitude) %>%
	summarize(mean_hs = mean(sp_hab_suit_var)) %>%
  mutate(long_not_360 = case_when(
				 longitude >= 180 ~ longitude - 360,
				 longitude < 180 ~ longitude)) %>%
  st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326, remove = FALSE)
	

 ex_sphabsuit0905 <- 
    	  	ggplot() +
					geom_sf(data = dat2006cesm, aes(color = mean_hs))  +
					geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
					coord_sf(crs = 3338) +
				scale_color_gradientn(colors = c("#B3E5FC","#B3E5FC",
																					 "#3378af","#3378af",
																					 "#00345C","#00345C"),
																values = c(0, 0.499, 0.5, 0.899, 0.9, 1),
																breaks = c(0.1, 0.5, 0.9),
																labels = c(0.1, 0.5, 0.9),
																limits = c(0, 1)) +
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
 				  black_map_theme_no_legend()

 ggsave(here("./output/plots/ex_sphabsuit0905.png"),
			 ex_sphabsuit0905,
			 width = 7, height = 7, units = "in")


## temperature time series ####

	yearly_temp_dat_hind <- ROMS_hindcast_dat %>%
		group_by(year) %>%
    summarise(mean_temp = mean(temp)) 

	yearly_temp_dat_proj <- proj_temp_dat %>% 
		group_by(simulation, projection, year) %>%
   	summarise(mean_temp = mean(bc_temp),
   						mean_sd_temp = mean(bc_temp_sd))
	
	yearly_temp_dat_proj <- yearly_temp_dat_proj %>%
		tidyr::unite("sim_proj", simulation, projection, remove = F)

proj_yrs <- 2020:2099

	yearly_temp_dat_proj_sum <- yearly_temp_dat_proj %>%
		group_by(year, projection) %>%
		summarise(mean_temp = mean(mean_temp)) %>%
		filter(year %in% proj_yrs)
	
	colors <- c("#dbc0f6", "#c69bf1") # light then dark
	
	projection <- unique(yearly_temp_dat_proj_sum$projection)
	
	names(colors) <- unique(yearly_temp_dat_proj_sum$projection)

	time_series_temp <-    
   	ggplot() +
		geom_vline(aes(xintercept = 2020), color = "white", size = 0.15) +
   	geom_line(data = yearly_temp_dat_hind, 
   						aes(x = year, y = mean_temp), 
   						color = "white") +
		geom_line(data = yearly_temp_dat_proj_sum,
							aes(year, mean_temp, 
									group = projection, 
									color = projection)) +
		xlab("Year") + 
		scale_color_manual(name = "projection", values = colors) +
	  scale_y_continuous(
	  	name = "Bottom\ntemperature\n(˚C)",
	  	breaks = c(0, 2, 4, 6),
	  ) +
		scale_x_continuous(
	  	name = "Year",
	  	breaks = c(1970, 2000, 2022, 2030, 2060, 2090)) +
	theme(legend.position = "none") +
  theme(axis.text=element_text(size= 12, colour = "white"),
  			axis.title.y = element_text(angle = 0, vjust = 0.5),
  			axis.title= element_text(size=12, color = "white"),
  			axis.line = element_line(color = "white"),
  			axis.ticks = element_line(colour = "white"),
  			legend.title = element_text(color = "white"),
  			legend.text = element_text(color = "white"),
  			panel.background = element_rect(fill = "black"),
				panel.grid = element_blank(),
  			plot.background = element_rect(fill = "black", color = "black"))
	
	 
  	ggsave(here("./output/plots/time_series_temp_black.png"),
			 time_series_temp,
			 width = 9, height = 4, units = "in")
	
  	### area plots ####
  	
  		# plots
	
	area_plot_yr <-    
   	ggplot(data = hind_area_yr) +
	 	geom_line(aes(year, total_area), alpha = 0.5,
            data = . %>% filter(sp_hab_threshold == "potential"), color = "white") +
		geom_line(data = proj_area_yr, 
							aes(year, total_area, color = sim_proj, group = sim_proj, alpha = 0.5)) +
		facet_grid(sp_hab_threshold ~ simulation) +
	  geom_line(aes(year, total_area, colour = sp_hab_threshold, alpha = 0.5),
            data = . %>% filter(sp_hab_threshold == "core"), color = "white") +
		xlab("Year") +
		scale_color_manual(name = "sim_proj", values = colors) +
	  scale_y_continuous(
	  	breaks = c(0, 200000, 400000),
	  	labels = c(0, 2, 4)
	  ) +
   	xlim(1970, 2100) +
   				theme_bw() +
	 				theme(strip.background = element_blank(),
					strip.text = element_text(size = 14, color = "white"),
					legend.position = ("none"),
  				axis.text=element_text(size= 12, colour = "white"),
  				axis.title= element_text(size=16, color = "white"),
  				axis.line = element_line(color = "white"),
  				axis.ticks = element_line(colour = "white"),
  				panel.background = element_rect(fill = "black", color = "black"),
					panel.grid = element_blank(),
  				plot.background = element_rect(fill = "black", color = "black"))
  	
		ggsave("./output/plots/area_plot_yr_var.png",
			 area_plot_yr_var,
			 width = 10, height = 5, units = "in")
		
	# facet by scenario
	proj_area_yr$scen <- NA
	
	
	proj_area_yr$scen[proj_area_yr$projection == "ssp126"] <- "low"
	proj_area_yr$scen[proj_area_yr$projection == "ssp585"] <- "high"
	
	proj_area_yr <- tidyr::unite(proj_area_yr,"sim_proj",
															 simulation, projection, remove = F)

	colors <- c("#6dc3a9", "#ff7373",
						  "#62b1c4", "#ff4040", 
						  "#97c3e5", "#ffb733")

	sim_proj <- unique(proj_area_yr$sim_proj)
	
	names(colors) <- unique(proj_area_yr$sim_proj)
		
	area_plot_yr <-    
   	ggplot(data = hind_area_yr) +
	 	geom_line(aes(year, total_area), alpha = 0.5,
            data = . %>% filter(sp_hab_threshold == "potential"), color = "white") +
		geom_vline(aes(xintercept = 2020), color = "white", size = 0.15) +
		geom_line(data = proj_area_yr, 
							aes(year, total_area, color = sim_proj, group = sim_proj, alpha = 0.5)) +
		facet_grid(sp_hab_threshold ~ scen) +
	  geom_line(aes(year, total_area, colour = sp_hab_threshold, alpha = 0.5),
            data = . %>% filter(sp_hab_threshold == "core"), color = "white") +
		xlab("Year") +
	  ylab(expression(paste("Area", ' '(km^{2})))) +
		scale_color_manual(name = "sim_proj", values = colors) +
	  scale_y_continuous(
	  	breaks = c(0, 200000, 400000),
	  	labels = c(0, 2, 4)
	  ) +
   	xlim(1970, 2100) +
   	theme_bw() +
	 	theme(strip.background = element_blank(),
					strip.text = element_text(size = 18, color = "white"),
					legend.position = ("none"),
  				axis.text=element_text(size= 16, colour = "grey"),
  				axis.title= element_text(size=16, color = "grey"),
  				axis.line = element_line(color = "grey"),
  				axis.ticks = element_line(colour = "grey"),
  				panel.background = element_rect(fill = "black"),
					panel.border = element_rect(color = "grey"),
					panel.grid = element_blank(),
  				plot.background = element_rect(fill = "black", color = "black"))
  	
  	
		ggsave("./output/plots/area_plot_yr.png",
			 area_plot_yr,
			 width = 10, height = 4.5, units = "in")
	

  #### consistency plots ####
	ex_consistency <- 
		ggplot() +
		geom_sf(data = ROMS_hindcast_dat_dec_yr_sum_05_sf, 
						aes(color = pct_yrs))  +
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
	   	labs(colour = "%\nyears") +
		theme(legend.background = element_rect(fill = "black"),
  				axis.text=element_text(size= 12, colour = "white"),
  				axis.title= element_text(size=16, color = "white"),
  				axis.line = element_line(color = "white"),
  				axis.ticks = element_line(colour = "white"),
  				legend.title = element_text(color = "white", size = 18),
  				legend.text = element_text(color = "white", size = 16),
  				panel.background = element_rect(fill = "black"),
  				plot.background = element_rect(fill = "black", color = "black"),
					legend.title.align = 0.5)
  	

  	ggsave(here("./output/plots/ex_consistency.png"),
		ex_consistency,
		width = 7, height = 7, units = "in")
  	
  # decade
  ex_consistency_dec <- 
		ggplot() +
		geom_sf(data = ROMS_hindcast_dat_dec_yr_sum_05_sf, 
						aes(color = pct_yrs))  +
		geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
		coord_sf(crs = 3338) +
  	facet_wrap(~ decade, nrow = 1) +
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
	   	labs(colour = "%\nyears") +
			theme(strip.background = element_blank(),
					strip.text = element_text(size = 14, color = "white"),
					legend.background = element_rect(fill = "black"),
  				axis.text=element_text(size= 12, colour = "white"),
  				axis.title= element_text(size=16, color = "white"),
  				axis.line = element_line(color = "white"),
  				axis.ticks = element_line(colour = "white"),
  				legend.title = element_text(color = "white", size = 18),
  				legend.text = element_text(color = "white", size = 16),
  				panel.background = element_rect(fill = "black"),
  				plot.background = element_rect(fill = "black", color = "black"),
					legend.title.align = 0.5)
  	

  	ggsave(here("./output/plots/ex_consistency_dec.png"),
		ex_consistency_dec,
		width = 13, height = 7, units = "in")
	
  	
	
  	
  	### plot domains separately
  		
		 BS_regions <- 
    	  	ggplot() +
	 	 			geom_sf(data = inner_poly, color = "salmon", fill = "salmon") +
	 	 		 	geom_sf(data = middle_poly, color = "mediumpurple", fill = "mediumpurple") +
	 	 			geom_sf(data = outer_poly, color = "dodgerblue", fill = "dodgerblue") +
					geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
					coord_sf(crs = 3338) +
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
				black_map_theme()
    	   
	 	ggsave("./output/plots/BS_regions_IMO.png",
			 BS_regions,
			 width = 7, height = 7, units = "in")

  	
	 	
	 	
	 	
	 	
	 	
	 	
	 	
	 	
	 	
	 	
	 	
	 	
	 	
	 	
	 	
	 	
	 	
	 	
	 	
	 	
	 	
	 	
	 	
  	
  	
  	### maps for prelim results based on temp

###############	
# maps ####
###############
	
	temp_2008_sf <- ROMS_dat_hind_trim_sf %>% filter(., year == 2008)
	
	temp_2016_sf <- ROMS_dat_hind_trim_sf %>% filter(., year == 2016)
	
	# maps for cold vs warm years

	hs_2008_plot_noleg <- 
		ggplot() +
  	geom_sf(data = temp_2008_sf, aes(color = sp_hab_suit)) +
  	geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
		coord_sf(crs = 3338) +
 		scale_x_continuous(
 			breaks = c(-175, -170, -165, -160),
 			labels = c("-175˚", "-170˚", "-165˚", "-160˚"),
 			name = "Longitude",
 			limits = c(-1400000, -150000)) +
 	  scale_y_continuous(
 			breaks = c(55, 60),
 			limits = c(470000, 1900000),
 			name = "Latitude") +
		scale_color_gradientn(
				colors = c("#b2cce1", "#b2cce1",
								   "#4d89b9", "#4d89b9",
								   "#01579B", "#01579B"),
				values = c(0, 0.499, 0.5, 0.899, 0.9, 1),
				breaks = c(0.1, 0.5, 0.9),
				labels = format(c(0.1, 0.5, 0.9)),
				limits = c(0, 1)) +
    labs(colour = "Spawning\nhabitat\nsuitability") +
		black_map_theme_no_legend()
  		
	hs_2016_plot_noy <- 
		ggplot() +
  	geom_sf(data = temp_2016_sf, aes(color = sp_hab_suit)) +
  	geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
		coord_sf(crs = 3338) +
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
		scale_color_gradientn(
				colors = c("#b2cce1", "#b2cce1",
								   "#4d89b9", "#4d89b9",
								   "#01579B", "#01579B"),
				values = c(0, 0.499, 0.5, 0.899, 0.9, 1),
				breaks = c(0.1, 0.5, 0.9),
				labels = format(c(0.1, 0.5, 0.9)),
				limits = c(0, 1)) +
    labs(colour = "Spawning\nhabitat\nsuitability") +
  	theme(legend.position = c(),
					legend.background = element_rect(fill = "black"),
  				axis.text.x =element_text(size= 12, colour = "white"),
  				axis.title.x = element_text(size=16, color = "white"),
  				axis.line = element_line(color = "white"),
  				axis.ticks.x = element_line(colour = "white"),
					axis.text.y = element_blank(),
					axis.title.y = element_blank(),
					axis.ticks.y = element_blank(),
  				legend.title = element_text(color = "white"),
  				legend.text = element_text(color = "white"),
  				panel.background = element_rect(fill = "black"),
  				plot.background = element_rect(fill = "black", color = "black"),
					legend.title.align = 0.5)
		
   plot1 <- hs_2008_plot_noleg + 
   	        theme(plot.margin = unit(c(0.2, 0, 0.2, 0.2), "in")) +
   	        ggtitle("2008: Cold Year") +
   	        theme(plot.title = element_text(size = 14, hjust = 0.5,  face = "bold", color = "white"))
   
   plot2 <- hs_2016_plot_noy + 
   	        theme(plot.margin = unit(c(0.2, 0.2, 0.2, -0.05), "in")) +
   	        ggtitle("2016: Warm Year") +
   	        theme(plot.title = element_text(size = 14, hjust = 0.5,  face = "bold", color = "white"))

	 warm_v_cold_hs <- plot1 + plot2 

	 
  	ggsave(here("./output/plots/warm_v_cold_hs_colorblocks_black.png"),
			 warm_v_cold_hs,
			 width = 10, height = 7, units = "in")
		
	### facet by month
  	
  	  	
  	## with color blocks
  	
  	# maps for cold vs warm years

	hs_2008_plot_noleg_mo <- 
		ggplot() +
  	geom_sf(data = temp_2008_sf, aes(color = sp_hab_suit)) +
  	geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
		coord_sf(crs = 3338) +
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
		scale_color_gradientn(colors = c("#B3E5FC", "#B3E5FC", 
																			"#01579B", "#01579B",
																			"#00345C", "#00345C"),
												  values = c(0, 0.499, 0.5, 0.899, 0.9, 1),
												  breaks = c(0.1, 0.5, 0.9),
												  labels = format(c(0.1, 0.5, 0.9)),
												  limits = c(0, 1)) +
    labs(colour = "Spawning\nhabitat\nsuitability") +
  	theme(
  			strip.text = element_text(size = 14, face = "bold", color = "white"),
 				strip.background = element_blank(),
					legend.background = element_rect(fill = "black"),
  				axis.text=element_text(size= 12, colour = "white"),
  				axis.title= element_text(size=16, color = "white"),
  				axis.line = element_line(color = "white"),
  				axis.ticks = element_line(colour = "white"),
  				legend.title = element_text(color = "white"),
  				legend.text = element_text(color = "white"),
  				panel.background = element_rect(fill = "black"),
  			  panel.spacing.x=unit(0, "lines"),
  				plot.background = element_rect(fill = "black", color = "black"),
					legend.title.align = 0.5)

		ggsave(here("./output/plots/hs_2008_plot_noleg_mo.png"),
			 hs_2008_plot_noleg_mo,
			 width = 10, height = 7, units = "in")
		
		
	hs_2016_plot_noy <- 
		ggplot() +
  	geom_sf(data = temp_2016_sf, aes(color = sp_hab_suit)) +
  	geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
	  facet_wrap(~ month_name, ncol = 3, nrow = 3) + 
		coord_sf(crs = 3338) +
 		scale_x_continuous(
 			breaks = c(-175, -170, -165, -160),
 			labels = c("-175˚", "-170˚", "-165˚", "-160˚"),
 			name = "Longitude",
 			limits = c(-1400000, -150000)) +
 	  scale_y_continuous(
 			breaks = c(55, 60),
 			limits = c(470000, 1900000),
 			name = "Latitude") +
		scale_color_gradientn(
			colors = c("#B3E5FC", "#B3E5FC", 
									"#01579B", "#01579B",
									"#00345C", "#00345C"),
			values = c(0, 0.499, 0.5, 0.899, 0.9, 1),
			breaks = c(0.1, 0.5, 0.9),
			labels = format(c(0.1, 0.5, 0.9)),
			limits = c(0, 1)) +
    labs(colour = "Spawning\nhabitat\nsuitability") +
		theme_bw() +
 		theme(
  			strip.text = element_text(size = 14, face = "bold", color = "white"),
 				strip.background = element_blank(),
				legend.background = element_rect(fill = "black"),
  			axis.text.x =element_text(size= 12, colour = "white"),
  			axis.title.x= element_text(size=16, color = "white"),
  			axis.line.x = element_line(color = "white"),
  			axis.ticks.x = element_line(colour = "white"),
  			legend.title = element_text(color = "white"),
  			legend.text = element_text(color = "white"),
  			panel.background = element_rect(fill = "black"),
  			plot.background = element_rect(fill = "black", color = "black"),
  			axis.text.y = element_blank(),
    		axis.ticks.y = element_blank(),
    		axis.title.y = element_blank(),
  			panel.spacing.x=unit(0, "lines"),
  			legend.title.align=0.5)
  
  	ggsave(here("./output/plots/hs_2016_plot_noy.png"),
			 hs_2016_plot_noy,
			 width = 10, height = 7, units = "in")
		


## monthly average
  	
ROMS_dat_hind_trim_current_month_sum <- ROMS_dat_hind_trim_current %>%
	group_by(latitude, longitude, year, month_name) %>%
	summarise(mean_sphabsuit = mean(sp_hab_suit))

ROMS_dat_hind_trim_current_month_sum <- ROMS_dat_hind_trim_current_month_sum %>%
	mutate(decade = case_when(
		between(year, 1970, 1979) ~ "1970s",
		between(year, 1980, 1989) ~ "1980s",
		between(year, 1990, 1999) ~ "1990s",
		between(year, 2000, 2009) ~ "2000s",
		between(year, 2010, 2019) ~ "2010s"))


ROMS_dat_hind_trim_current_month_sum_05 <- ROMS_dat_hind_trim_current_month_sum %>%
	group_by(latitude, longitude, month_name, decade) %>%
	summarize(no_yrs = length(which(mean_sphabsuit >= 0.5))) %>%
	mutate(year_tot = 10,
				 pct_yrs = (no_yrs/year_tot) * 100)

ROMS_dat_hind_trim_current_month_sum_05_sf <- ROMS_dat_hind_trim_current_month_sum_05 %>%
		mutate(long_not_360 = longitude - 360) %>%
  	st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326)
  

plot <- 
  		ggplot() +
					geom_sf(data = ROMS_dat_hind_trim_current_month_sum_05_sf,
									aes(color = pct_yrs))  +
					geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
					coord_sf(crs = 3338) +
  		    facet_grid(decade ~ month_name) +
					scale_color_viridis_c() +
 					scale_x_continuous(
 						breaks = c( -170, -160),
 						labels = c("-170˚", "-160˚"),
 						name = "Longitude",
 						limits = c(-1400000, -150000)
 					) +
 					scale_y_continuous(
 						breaks = c(55, 60),
 						limits = c(470000, 1900000),
 						name = "Latitude",
 					) +
    	  	labs(colour = "%\nyears") +
					ggtitle(label = paste("Percent of years for which spawning habitat suitability", 
																symbol, "0.5", sep =" ")) +
						theme(
  			strip.text = element_text(size = 18, color = "white", face = "bold"),
 				strip.background = element_blank(),
				legend.background = element_rect(fill = "black"),
  			axis.text =element_text(size= 14, colour = "white"),
  			axis.title = element_text(size=18, color = "white"),
  			axis.line  = element_line(color = "white"),
  			axis.ticks  = element_line(colour = "white"),
  			legend.title = element_text(color = "white", size = 18),
  			legend.text = element_text(color = "white", size = 16),
  			panel.background = element_rect(fill = "black"),
  			plot.background = element_rect(fill = "black", color = "black"),
  			panel.spacing.x=unit(0, "lines"),
  			legend.title.align=0.5)
  
  	

  	ggsave("./output/plots/percent_years_months05_BLACK.png",
		plot,
		width = 15, height = 10, units = "in")
	
##### rolling means #####
  	
  	# of spawning habitat suitability
  	
  		# mean 
	rolling_mean_plot <- 
		ggplot(rolling_stats) +
		geom_line(aes(x = years, y = roll_yr_mean), color = "white", size = 2) +
		xlab("Year") +
		ylab("11-year rolling mean") +
  	theme(legend.position = c(),
					legend.background = element_rect(fill = "black"),
  				axis.text=element_text(size= 20, colour = "white"),
  				axis.title= element_text(size=22, color = "white"),
  				axis.line = element_line(color = "white"),
  				axis.ticks = element_line(colour = "white"),
  				legend.title = element_text(color = "white"),
  				legend.text = element_text(color = "white"),
  				panel.background = element_rect(fill = "black"),
					panel.grid = element_blank(),
  				plot.background = element_rect(fill = "black", color = "black"))

  	ggsave("./output/plots/rolling_mean_sp_hab_suit.png",
		rolling_mean_plot,
		width = 10, height = 5, units = "in")
	

  # sd
	rolling_sd_plot <- 
		ggplot(rolling_stats) +
		geom_line(aes(x = years, y = roll_yr_sd), color = "white", size = 2) +
		xlab("Year") +
		ylab("11-year rolling\nstandard deviation")  +
	    	theme(legend.position = c(),
					legend.background = element_rect(fill = "black"),
  				axis.text=element_text(size= 20, colour = "white"),
  				axis.title= element_text(size=22, color = "white"),
  				axis.line = element_line(color = "white"),
  				axis.ticks = element_line(colour = "white"),
  				legend.title = element_text(color = "white"),
  				legend.text = element_text(color = "white"),
  				panel.background = element_rect(fill = "black"),
					panel.grid = element_blank(),
  				plot.background = element_rect(fill = "black", color = "black"))

  	
  	ggsave("./output/plots/rolling_sd_plot_sp_hab_suit.png",
		rolling_sd_plot,
		width = 10, height = 5, units = "in")

  	### by month
  	
		mo_stats_4plot$month_no[mo_stats_4plot$month == "January"] <- 1
		mo_stats_4plot$month_no[mo_stats_4plot$month == "February"] <- 2
		mo_stats_4plot$month_no[mo_stats_4plot$month == "March"] <- 3
		mo_stats_4plot$month_no[mo_stats_4plot$month == "April"] <- 4
		mo_stats_4plot$month_no[mo_stats_4plot$month == "May"] <- 5
		mo_stats_4plot$month_no[mo_stats_4plot$month == "June"] <- 6
		
		# reorder for plotting
		mo_stats_4plot$month <- factor(mo_stats_4plot$month)
  	mo_stats_4plot$month <- fct_reorder(mo_stats_4plot$month, 
  																		mo_stats_4plot$month_no)


  	 # mean
  rolling_mean_plot_mo <- 
		ggplot(mo_stats_4plot) +
		geom_line(aes(x = year, y = roll_mean), color = "white", size = 2) +
  	facet_wrap(~ month, ncol = 3, nrow = 3) +
		xlab("Year") +
		ylab("11-year rolling mean") +
  		theme(	
  			strip.text = element_text(size = 20, face = "bold", color = "white"),
 				strip.background = element_blank(),
  				axis.text=element_text(size= 18, colour = "white"),
  				axis.title= element_text(size=20, color = "white"),
  				axis.line = element_line(color = "white"),
  				axis.ticks = element_line(colour = "white"),
  				panel.background = element_rect(fill = "black"),
  			  panel.grid  = element_blank(),
  				plot.background = element_rect(fill = "black", color = "black")) +
  	 annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey", size = 2)+
     annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey", size = 2)
  
  ggsave("./output/plots/rolling_mean_plot_mo.png",
		rolling_mean_plot_mo,
		width = 15, height = 10, units = "in")
  	
   # sd
  rolling_sd_plot_mo <- 
		ggplot(mo_stats_4plot) +
		geom_line(aes(x = year, y = roll_sd), color = "white", size = 2) +
  	facet_wrap(~ month, ncol = 3, nrow = 3) +
		xlab("Year") +
		ylab("11-year rolling\nstandard deviation") +
  		theme(	
  			strip.text = element_text(size = 20, face = "bold", color = "white"),
 				strip.background = element_blank(),
  				axis.text=element_text(size= 18, colour = "white"),
  				axis.title= element_text(size=20, color = "white"),
  				axis.line = element_line(color = "white"),
  				axis.ticks = element_line(colour = "white"),
  				panel.background = element_rect(fill = "black"),
  			  panel.grid  = element_blank(),
  				plot.background = element_rect(fill = "black", color = "black")) +
  	 annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey", size = 2)+
     annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey", size = 2)
  
  ggsave("./output/plots/rolling_sd_plot_mo.png",
		rolling_sd_plot_mo,
		width = 15, height = 10, units = "in")
  

 
 ## spawning habitat suitability
 
 yearly_hab_dat <- ROMS_dat_hind_trim %>%
   								 group_by(year) %>%
   								 summarise(annual_hatch_success_cauchy = mean(hatch_success_cauchy),
   								 					 annual_hatch_success_gaussian = mean(hatch_success_gaus),
   								  				annual_spawning_hab_suit = mean(sp_hab_suit))
	
	yearly_hab_dat_no_2021 <- yearly_hab_dat %>% filter(year != 2021)
 
	 annual_hatch_success_2020_cauchy <-    
   	ggplot(data = yearly_hab_dat_no_2021) +
   	geom_line(aes(x = year, y = annual_spawning_hab_suit), color = "lightgrey", size = 2) +
   	#geom_point(aes(x = year, y = annual_spawning_hab_suit), color = "white", size  = 4) +
   	xlab("Year") + 
	  scale_y_continuous(
	  	name = "Spawning habitat\nsuitability",
	  	breaks = c(0.30, 0.40, 0.50),
	  ) +
   	xlim(1970, 2020) +
  	theme(legend.position = c(),
					legend.background = element_rect(fill = "black"),
  				axis.text=element_text(size= 18, colour = "white"),
  				axis.title= element_text(size=20, color = "white"),
  				axis.line = element_line(color = "white"),
  				axis.ticks = element_line(colour = "white"),
  				legend.title = element_text(color = "white"),
  				legend.text = element_text(color = "white"),
  				panel.background = element_rect(fill = "black"),
					panel.grid = element_blank(),
  				plot.background = element_rect(fill = "black", color = "black"))

   annual_hatch_success_2020_cauchy_txt <- 
   	annual_hatch_success_2020_cauchy +
		annotate(geom = "text", x = 1980, y = 0.55,
           label = "Average annual spawning habitat suitability",
           color = "lightgrey", size = 6)
 
	ggsave("./output/plots/annual_hatch_success_2020_cauchy.png",
			 annual_hatch_success_2020_cauchy_txt,
			 width = 12, height = 6, units = "in")
