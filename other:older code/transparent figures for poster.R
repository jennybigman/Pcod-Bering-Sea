
#### thermal response curve transparent ####

	eggs <- read.csv("../data and code from Laurel & Rogers 2020 CJFAS/laurelandrogerscode/Pcod and WP hatch probabilities.csv")
	
	eggs<-eggs[,1:8]
	eggs$Ntot<-eggs$Hatched+eggs$Dead
	eggs$Phatch<-eggs$Hatched/(eggs$Hatched+eggs$Dead)
	eggs<-eggs[eggs$Species=="Pcod",] #Pcod
	eggs$pMax<-eggs$Phatch/max(eggs$Phatch)
	
	res2 <- nls( Phatch ~ k/(1+((Temp_act-mu)/sigma)^2), start=c(mu=5,sigma=2,k=1) , data = eggs, weights=Ntot)
	v2 <- summary(res2)$parameters[,"Estimate"]

	cauchy <- function(x) v2[3]/(1+((x-v2[1])/v2[2])^2)


therm_response_cuve <- 
	ggplot(eggs) +
	geom_point(aes(x = Temp_act, y = Phatch), color = "black", size = 3) +
	geom_function(fun = cauchy, aes(col = "Temp_act"), color = "black", size = 2) +
	xlab("Temperature ˚C") +
	ylab("Proportion successful hatch") +
	theme(
		legend.position = "none",
  				axis.text =element_text(size= 22, colour = "black"),
  				axis.title = element_text(size=24, color = "black"),
  				axis.line = element_line(color = "black"),
  				axis.ticks = element_line(colour = "black"),
					panel.grid = element_blank(),
				  panel.background = element_rect(fill = "transparent", color = NA),
			  	plot.background = element_rect(fill = "transparent", color = NA))

 ggsave(here("./output/plots/therm_response_cuve.png"),
		therm_response_cuve,
		width = 10, height = 7, units = "in")
 
 therm_response_cuve_white <- 
	ggplot(eggs) +
	geom_point(aes(x = Temp_act, y = Phatch), color = "white", size = 3) +
	geom_function(fun = cauchy, aes(col = "Temp_act"), color = "white", size = 2) +
	xlab("Temperature ˚C") +
	ylab("Proportion successful hatch") +
	theme(
		legend.position = "none",
  				axis.text =element_text(size= 20, colour = "white"),
  				axis.title = element_text(size=30, color = "white"),
  				axis.line = element_line(color = "white", linewidth = 2),
  				axis.ticks = element_line(colour = "white"),
					panel.grid = element_blank(),
				  panel.background = element_rect(fill = "transparent", color = NA),
			  	plot.background = element_rect(fill = "transparent", color = NA))

 ggsave(here("./output/plots/therm_response_cuve_white.png"),
		therm_response_cuve_white,
		width = 8, height = 7, units = "in")
 
 
 
	#### map of Bering Sea ####
 
	ROMS_hind_sum <- ROMS_hindcast_dat %>%
		group_by(latitude, longitude, temp) %>%
		summarise(mean_temp = mean(temp)) %>%
		mutate(long_not_360 = case_when(
					 longitude >= 180 ~ longitude - 360,
					 longitude < 180 ~ longitude)) %>%
  	st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326, remove = FALSE)

	generic_map_EBS <- 
		ggplot() +
		geom_sf(data = ROMS_hind_sum, aes(color = mean_temp))  +
		geom_sf(data = world_map_data, fill = "white", lwd = 0) +
		coord_sf(crs = 3338) +
		scale_color_viridis_c() +
 		scale_x_continuous(
 		 breaks = c(-170, -160),
		 labels = c("-170˚", "-160˚"),
		 limits = c(-1400000, 10000),
 			name = "Longitude") +
 		scale_y_continuous(
 			breaks = breaks_y,
 			limits = limits_y,
 			name = "Latitude") +
		theme_bw() +
		theme(
					legend.position = c(0.89, 0.72),
					panel.background = element_rect(fill = "transparent", color = NA),
					plot.background = element_rect(fill = "transparent", color = NA),
					axis.text = element_text(size = 20, colour = "white"),
  	  		axis.ticks = element_line(colour = "white"),
  	  		axis.line = element_blank(),
  	  		axis.title = element_text(size= 30, color = "white"),
  	  		panel.border = element_rect(fill = NA, color = "white", linewidth = 2))
	
	generic_map_EBS <- generic_map_EBS + 	guides(fill=guide_legend(title="Bottom\ntemperature\n(˚C)"))

	ggsave(here("./output/plots/generic_map_EBS.png"),
		generic_map_EBS,
		width = 10, height = 7, units = "in")
 

	#### march temps ####
	
		colfunc <- colorRampPalette(c("#005b96", "#b2cddf"))
		rev_cols <- colfunc(6)

	
	years_hind <- 1970:1989
	
	march_temp_hind <- ROMS_hindcast_dat %>%
		filter(year %in% years_hind) %>%
		filter(month == 3) %>%
		group_by(latitude, long_not_360) %>%
		summarize(mean_temp = mean(temp)) %>%
		st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326, remove = FALSE)
		
	march_avg_temps_hind <-
		ggplot() +
		geom_sf(data = march_temp_hind, aes(color = mean_temp))  +
		geom_sf(data = world_map_data, fill = "white", lwd = 0) +
		coord_sf(crs = 3338) +
		binned_scale(
			aesthetics = "color",
			scale_name = "stepsn",
			palette = function(x) rev_cols,
			breaks = c(0, 2, 4, 6, 8),
			limits = c(-2.21, 10.55),
			show.limits = FALSE,
			nice.breaks = TRUE,
			guide = "colorsteps") +
 		scale_x_continuous(
 		 breaks = c(-170, -160),
		 labels = c("-170˚", "-160˚"),
		 limits = c(-1400000, 10000),
 			name = "Longitude") +
 		scale_y_continuous(
 			breaks = breaks_y,
 			limits = limits_y,
 			name = "Latitude") +
    labs(colour = "Mean\nbottom\ntemperature\n(˚C)") +
		theme_bw() +
		theme(legend.position = "none",
					axis.text = element_text(size = 20, colour = "white"),
  	  		axis.ticks = element_line(colour = "white"),
  	  		axis.line = element_blank(),
  	  		axis.title = element_text(size= 30, color = "white"),
  	  		panel.border = element_rect(fill = NA, color = "white", linewidth = 2),
					panel.background = element_rect(fill = "transparent", color = NA),
					plot.background = element_rect(fill = "transparent", color = NA))
	
	ggsave(here("./output/plots/march_avg_temps_hind.png"),
		march_avg_temps_hind,
		width = 10, height = 7, units = "in")
 
		
	years_proj <- 2080:2099
	
	march_temp_proj <- ROMS_projected_dat %>%
		filter(year %in% years_proj) %>%
		filter(month == 3) %>%
		group_by(projection, latitude, long_not_360) %>%
		summarize(mean_temp = mean(bc_temp_sd)) %>%
		st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326, remove = FALSE)
	
	# low emissions scenario
	march_temp_proj_ssp126 <- march_temp_proj %>%
		filter(projection == "SSP126")

	march_avg_temps_proj_ssp126 <-
		ggplot() +
		geom_sf(data = march_temp_proj_ssp126, aes(color = mean_temp))  +
		geom_sf(data = world_map_data, fill = "white", lwd = 0) +
		coord_sf(crs = 3338) +
		binned_scale(
			aesthetics = "color",
			scale_name = "stepsn",
			palette = function(x) rev_cols,
			breaks = c(0, 2, 4, 6, 8),
			limits = c(-2.21, 10.55),
			show.limits = FALSE,
			nice.breaks = TRUE,
			guide = "colorsteps") +
		scale_x_continuous(
 		 breaks = c(-170, -160),
		 labels = c("-170˚","-160˚"),
		 limits = c(-1400000, 10000),
 			name = "Longitude") +
 		scale_y_continuous(
 			breaks = breaks_y,
 			limits = limits_y,
 			name = "Latitude") +
    labs(colour = "Mean\nbottom\ntemperature\n(˚C)") +
		theme_bw() +
		theme(legend.title.align = 0.5,
					legend.position = c(0.89, 0.72),
					legend.background = element_blank(),
					legend.title = element_text(size = 18),
					legend.text = element_text(size = 18),
					axis.text.x = element_text(size =  20, colour = "white"),
  	  		axis.ticks.x = element_line(colour = "white"),
  	  		axis.line = element_blank(),
  	  		axis.title.x = element_text(size= 30, color = "white"),
  	  		panel.border = element_rect(fill = NA, color = "white", linewidth = 2),
					axis.title.y = element_blank(),
					axis.text.y = element_blank(),
					axis.ticks.y = element_blank(),
					panel.background = element_rect(fill = "transparent", color = NA),
					plot.background = element_rect(fill = "transparent", color = NA))
	
	ggsave(here("./output/plots/march_avg_temps_proj_ssp126.png"),
		march_avg_temps_proj_ssp126,
		width = 10, height = 7, units = "in")
 
	# high emissions scenario
	
	march_temp_proj_ssp585 <- march_temp_proj %>%
		filter(projection == "SSP585")

	march_avg_temps_proj_ssp585 <-
		ggplot() +
		geom_sf(data = march_temp_proj_ssp585, aes(color = mean_temp))  +
		geom_sf(data = world_map_data, fill = "white", lwd = 0) +
		coord_sf(crs = 3338) +
		binned_scale(
			aesthetics = "color",
			scale_name = "stepsn",
			palette = function(x) rev_cols,
			breaks = c(0, 2, 4, 6, 8),
			limits = c(-2.21, 10.55),
			show.limits = FALSE,
			nice.breaks = TRUE,
			guide = "colorsteps") +		
		scale_x_continuous(
 		 breaks = c(-170, -160),
		 labels = c("-170˚","-160˚"),
		 limits = c(-1400000, 10000),
 			name = "Longitude") +
		scale_y_continuous(
 			breaks = breaks_y,
 			limits = limits_y,
 			name = "Latitude") +
    labs(colour = "Mean\nbottom\ntemperature\n(˚C)") +
		theme_bw() +
		theme(legend.position = "none",
					axis.text.x = element_text(size = 20, colour = "white"),
  	  		axis.ticks.x = element_line(colour = "white"),
  	  		axis.line = element_blank(),
  	  		axis.title.x = element_text(size=30, color = "white"),
					axis.title.y = element_blank(),
					axis.text.y = element_blank(),
					axis.ticks.y = element_blank(),
  	  		panel.border = element_rect(fill = NA, color = "white", linewidth = 2),
					panel.background = element_rect(fill = "transparent", color = NA),
					plot.background = element_rect(fill = "transparent", color = NA))
	
		
	ggsave(here("./output/plots/march_avg_temps_proj_ssp585.png"),
		march_avg_temps_proj_ssp585,
		width = 10, height = 7, units = "in")

	#### hab suit chunks ####
	
	# format breaks for longitude for these plots
  breaks_x <- c(-170, -160)
	labels_x <- c("-170˚", "-160˚") 
	limits_x <- c(-1400000, -150000)

	# historical period
  hist_yrs <- 1970:1999

	ROMS_hindcast_dat_hist <- ROMS_hindcast_dat %>% filter(., year %in% hist_yrs)

	yearly_hab_dat_hind_hist <- ROMS_hindcast_dat_hist %>%
		group_by(latitude, longitude, long_not_360) %>%
    summarise(mean_hab_suit = mean(sp_hab_suit)) %>%
		st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326, remove = FALSE)

	
  # current period
  current_yrs <- 2001:2020

	ROMS_hindcast_dat_current <- ROMS_hindcast_dat %>% filter(., year %in% current_yrs)

	yearly_hab_dat_hind_current <- ROMS_hindcast_dat_current %>%
		group_by(latitude, longitude, long_not_360) %>%
    summarise(mean_hab_suit = mean(sp_hab_suit)) %>%
		st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326, remove = FALSE)

	
	# last period
	last_yrs <- 2080:2099
	
	ROMS_projected_dat_last <- ROMS_projected_dat %>% filter(., year %in% last_yrs)

	yearly_hab_dat_proj_last <- ROMS_projected_dat_last %>% 
		mutate(long_not_360 = case_when(
					 longitude >= 180 ~ longitude - 360,
					 longitude < 180 ~ longitude)) %>%
		group_by(latitude, longitude, long_not_360, projection) %>%
   	summarise(mean_hab_suit = mean(sp_hab_suit_var))  %>%
		st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326, remove = FALSE)

	
	# add in scenario as factor
	yearly_hab_dat_proj_last$scen <- NA
	
  yearly_hab_dat_proj_last$scen[yearly_hab_dat_proj_last$projection == "SSP126"] <- "low\nemission\n(SSP126)"
	yearly_hab_dat_proj_last$scen[yearly_hab_dat_proj_last$projection == "SSP585"] <- "high\nemission\n(SSP585)"
	
	yearly_hab_dat_proj_last$scen_f = factor(yearly_hab_dat_proj_last$scen, 
																					levels=c('low\nemission\n(SSP126)', 
																									 'high\nemission\n(SSP585)'))

	yearly_hab_dat_proj_last$scen <- NA
	
  yearly_hab_dat_proj_last$scen[yearly_hab_dat_proj_last$projection == "SSP126"] <- "low\nemission\n(SSP126)"
	yearly_hab_dat_proj_last$scen[yearly_hab_dat_proj_last$projection == "SSP585"] <- "high\nemission\n(SSP585)"
	
	
	yearly_hab_dat_proj_last$scen_f = factor(yearly_hab_dat_proj_last$scen, 
																						 levels=c('low\nemission\n(SSP126)', 
																											'high\nemission\n(SSP585)'))
	
	yearly_hab_dat_proj_last$scen_f = factor(yearly_hab_dat_proj_last$scen, 
																						 levels=c('low\nemission\n(SSP126)', 
																											'high\nemission\n(SSP585)'))

	### plot
	
	#historical
	
	historical20 <-	
		ggplot() +
		geom_sf(data = yearly_hab_dat_hind_hist, aes(color = mean_hab_suit))  +
		geom_sf(data = world_map_data, fill = "white", lwd = 0) +
		coord_sf(crs = 3338) +
		scale_color_gradientn(
			colors = c("#B3E5FC", "#B3E5FC", 
								 "#01579B", "#01579B",
								 "#00345C", "#00345C"),
			values = c(0, 0.499, 0.5, 0.899, 0.9, 1),
			breaks = c(0.1, 0.5, 0.9),
			labels = format(c(0.1, 0.5, 0.9)),
			limits = c(0, 1)) +
 		scale_x_continuous(
 			breaks = breaks_x,
 			labels = labels_x,
 			name = "Longitude",
 			limits = limits_x
 		) +
 		scale_y_continuous(
 			breaks = breaks_y,
 			limits = limits_y,
 			name = "Latitude",
 		) +
    labs(colour = "Spawning habitat suitability") +
		theme_bw() +
	 		theme(
 			panel.border = element_rect(color = "white", linewidth = 2),
 			legend.position = "none",
 			axis.text = element_text(size = 20,  color = "white"),	
  		axis.title = element_text(size = 30,  color = "white"),
 			axis.ticks = element_line(color = "white"),
 			plot.margin = unit(c(0.05,-0.1, 0, 0), "in"),
 			panel.background = element_rect(fill = "transparent", color = NA),
			plot.background = element_rect(fill = "transparent", color = NA))
	
	ggsave(here("./output/plots/historical20.png"),
		historical20,
		width = 5, height = 5, units = "in")

	
	# current
	current20 <-	
		ggplot() +
		geom_sf(data = yearly_hab_dat_hind_current, aes(color = mean_hab_suit))  +
		geom_sf(data = world_map_data, fill = "white", lwd = 0) +
		coord_sf(crs = 3338) +
		scale_color_gradientn(
			colors = c("#B3E5FC", "#B3E5FC", 
								 "#01579B", "#01579B",
								 "#00345C", "#00345C"),
			values = c(0, 0.499, 0.5, 0.899, 0.9, 1),
			breaks = c(0.1, 0.5, 0.9),
			labels = format(c(0.1, 0.5, 0.9)),
			limits = c(0, 1)) +
 		scale_x_continuous(
 			breaks = breaks_x,
 			labels = labels_x,
 			name = "Longitude",
 			limits = limits_x
 		) +
 		scale_y_continuous(
 			breaks = breaks_y,
 			limits = limits_y,
 			name = "Latitude",
 		) +
    labs(colour = "Spawning habitat suitability") +
		theme_bw() +
	 		theme(
 			panel.border = element_rect(color = "white", linewidth = 2),
 			legend.position = "none",
 			axis.text.x = element_text(size = 20,  color = "white"),	
  		axis.title.x = element_text(size = 30,  color = "white"),
 			axis.ticks.x = element_line(color = "white"),
 			plot.margin = unit(c(0.05,-0.1, 0, 0), "in"),
 			axis.text.y = element_blank(),
  		axis.title.y =  element_blank(),
 			axis.ticks.y =  element_blank(),
 			panel.background = element_rect(fill = "transparent", color = NA),
			plot.background = element_rect(fill = "transparent", color = NA))

	ggsave(here("./output/plots/current20.png"),
		current20,
		width = 5, height = 5, units = "in")

	# projections
	
	last20 <-	
		ggplot() +
		geom_sf(data = yearly_hab_dat_proj_last, aes(color = mean_hab_suit))  +
		geom_sf(data = world_map_data, fill = "white", lwd = 0) +
		coord_sf(crs = 3338) +
		facet_wrap(~scen_f, nrow = 2) +
		scale_color_gradientn(
			colors = c("#B3E5FC", "#B3E5FC", 
								 "#01579B", "#01579B",
								 "#00345C", "#00345C"),
			values = c(0, 0.499, 0.5, 0.899, 0.9, 1),
			breaks = c(0.1, 0.5, 0.9),
			labels = format(c(0.1, 0.5, 0.9)),
			limits = c(0, 1)) +
 		scale_x_continuous(
 			breaks = breaks_x,
 			labels = labels_x,
 			name = "Longitude",
 			limits = limits_x
 		) +
 		scale_y_continuous(
 			breaks = breaks_y,
 			limits = limits_y,
 			name = "Latitude",
 		) +
    labs(colour = "Spawning habitat suitability") +
		theme_bw() +
 		theme(
 			panel.spacing = unit(0.04, "in"),
 			panel.border = element_rect(color = "white", linewidth = 2),
 			legend.position = "none",
 			strip.text = element_blank(),
 			strip.background = element_blank(),
 			axis.text.x = element_text(size = 20,  color = "white"),	
  		axis.title.x = element_text(size = 30,  color = "white"),
 			axis.ticks.y = element_blank(),
 			axis.title.y = element_blank(),
 			axis.text.y = element_blank(),
 			axis.ticks.x = element_line( color = "white"),
 			panel.background = element_rect(fill = "transparent", color = NA),
			plot.background = element_rect(fill = "transparent", color = NA))

	ggsave(here("./output/plots/last20.png"),
		last20,
		width = 5, height = 10, units = "in")

	#### mean latitude ####
	
	map_mean_lats <- 
		 	ggplot() +
			geom_point(data = coords_hind_sf, aes(color = year, 
  			geometry = geometry), alpha = 0.5, size = 5,
    		stat = "sf_coordinates") +
			geom_point(data = coords_proj_sf, aes(color = year, 
  			geometry = geometry), alpha = 0.5, size = 5,
    		stat = "sf_coordinates") +
			coord_sf(crs = 3338) +
 		scale_x_continuous(
 				breaks = c(-167, -169 ,-171),
 				labels = c("-167˚W", "-169˚W", "-171˚W"),
 				name = "Longitude",
 				limits = c(-1030000, -700000)) +
 			scale_y_continuous(
 				breaks = c(56, 57, 58),
 				limits = c(700000, 1090000),
 				name = "Latitude") +
		facet_grid(thresh ~ scen_f) +
		scale_color_viridis_c(
			breaks = c(1970, 2030, 2090)
		) +
	  theme_bw() +
 		theme(panel.spacing = unit(0.25, "lines"),
			legend.title.align = 0.5,
			legend.text = element_text(size = 20, color = "white"),
			legend.title = element_text(size = 30, color = "white"),
			legend.background = element_rect(fill = "transparent", color = NA),
 			panel.border = element_rect(color = "white", linewidth = 2),
 			strip.text.x = element_text(size = 30, face = "bold",  color = "white"),
 			strip.text.y = element_text(size = 30, face = "bold",  color = "white"),
 			strip.background = element_blank(),
 			axis.text = element_text(size = 20,  color = "white"),	
  		axis.title = element_text(size = 30,  color = "white"),
 			axis.ticks = element_line( color = "white"),
			panel.background = element_rect(fill = "transparent", color = NA),
			plot.background = element_rect(fill = "transparent", color = NA))

		ggsave("./output/plots/mean_lats_map.png",
			 map_mean_lats,
			 width = 10, height = 10, units = "in")
		
		
			
	#### recruitment ####
		
	recruit_habsuit_plot <-
		ggplot(data = recruit_habsuit_en, aes(x = mean_hab_suit, y = log_raw_recruits)) +
		geom_point(color = "white", size = 4) +
		scale_x_continuous(
			breaks = c(0.2, 0.3, 0.4),
			labels = c(0.2, 0.3, 0.4),
			name = "Annual spawning\nhabitat suitability") +
		scale_y_continuous(
			breaks = c(18, 19, 20, 21),
			labels = c(18, 19, 20, 21),
			limits = c(18, 21),
			name = "Log(abundance)") +
		theme_bw() +
		annotate("text", label = "Pearson's correlation\ncoefficient = -0.16",
						 y = 18.25, x = 0.26, size = 7, alpha = 0.7, color = "white") +
		theme(
			axis.text = element_text(size = 20, colour = "white"),
  	  axis.ticks = element_line(colour = "white"),
  	  axis.line = element_line(colour = "white"),
			axis.title.x = element_text(size = 30, color = "white"),
  	  axis.title.y = element_markdown(size= 30, color = "white"),
  	  panel.grid.major = element_blank(),
  	  panel.grid.minor = element_blank(),
  	  panel.border = element_rect(fill = NA, color = "white", linewidth = 2),
			panel.background = element_rect(fill = "transparent", color = NA),
			plot.background = element_rect(fill = "transparent", color = NA))

		ggsave("./output/plots/recruit_habsuit_plot.png",
			 recruit_habsuit_plot,
			 width = 6, height = 6, units = "in")
		
		

	larval_yr_plot_LATLON <- 
  	ggplot() +
		geom_sf(data = catch0_dat_trim_sf,
						aes(color = LARVALCATCHPER10M2),
						color = "white", shape = 4, alpha = 0.5, size = 1) +
		geom_sf(data = length_dat_sf, 
						aes(color = CORRECTED_LENGTH, 
								size = LARVALCATCHPER10M2),
						alpha = 0.5) +
		scale_color_viridis_c() +
		geom_sf(data = world_map_data, fill = "white", lwd = 0) +
		coord_sf(crs = 3338) +
 		scale_x_continuous(
 			breaks = breaks_x,
 			labels = c("-170˚W", "-160˚W"),
 			name = "Longitude",
 			limits = limits_x) +
 		scale_y_continuous(
 			breaks = breaks_y,
 			limits = limits_y,
 			name = "Latitude") +
		labs(color = "length (mm)", size = expression(paste("catch (per 10m"^{2}*")"))) +
		theme_bw() +
		theme(
			legend.position = "none",
 			axis.text = element_text(size = 20, colour = "white"),
  	  axis.ticks = element_line(colour = "white"),
  	  axis.line = element_line(colour = "white"),
			axis.title = element_text(size = 30, color = "white"),
  	  panel.border = element_rect(fill = NA, color = "white", linewidth = 2),
			panel.background = element_rect(fill = "transparent", color = NA),
			plot.background = element_rect(fill = "transparent", color = NA))

	
		ggsave("./output/plots/larval_yr_plot_LATLON.png",
			 larval_yr_plot_LATLON,
			 width = 5, height = 5, units = "in")
		
		### legend 
		
		
	legend_larvalplot <- 
  	ggplot() +
		geom_sf(data = catch0_dat_trim_sf,
						aes(color = LARVALCATCHPER10M2),
						color = "white", shape = 4, alpha = 0.5, size = 1) +
		geom_sf(data = length_dat_sf, 
						aes(color = CORRECTED_LENGTH, 
								size = LARVALCATCHPER10M2),
						alpha = 0.5) +
		scale_color_viridis_c() +
		geom_sf(data = world_map_data, fill = "white", lwd = 0) +
		coord_sf(crs = 3338) +
 		scale_x_continuous(
 			breaks = breaks_x,
 			labels = c("-170˚W", "-160˚W"),
 			name = "Longitude",
 			limits = limits_x) +
 		scale_y_continuous(
 			breaks = breaks_y,
 			limits = limits_y,
 			name = "Latitude") +
		labs(color = "length (mm)", size = expression(paste("catch (per 10m"^{2}*")"))) +
		theme_bw() +
		theme(
				legend.title.align = 0.5,
				legend.text = element_text(size = 20, color = "white"),
				legend.title = element_text(size = 30, color = "white"),
				legend.background = element_rect(fill = "transparent", color = NA),
 			axis.text = element_text(size = 20, colour = "white"),
  	  axis.ticks = element_line(colour = "white"),
  	  axis.line = element_line(colour = "white"),
			axis.title = element_text(size = 30, color = "white"),
  	  panel.border = element_rect(fill = NA, color = "white"),
			panel.background = element_rect(fill = "transparent", color = NA),
			plot.background = element_rect(fill = "transparent", color = NA))

		legend <- get_legend(legend_larvalplot)
		
		legend_larv <- as_ggplot(legend) +
			theme(
				legend.title.align = 0.5,
				legend.text = element_text(size = 20, color = "white"),
				legend.title = element_text(size = 30, color = "white"),
				legend.background = element_rect(fill = "transparent", color = NA))
		
			ggsave("./output/plots/legend_larv.png",
			 legend_larv)