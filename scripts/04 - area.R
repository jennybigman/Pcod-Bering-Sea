# 04 - calculating the area metric 

	#### by year #### each grid cell counted only once
	
	#### hindcasts ####
	
	c_area_hind_dat <- ROMS_hindcast_dat %>%
		filter(sp_hab_suit >= 0.9) 
	
	c_area_hind_dat_sum <- c_area_hind_dat %>%
		group_by(latitude, longitude, year) %>%
		distinct(across(c(latitude, longitude)), .keep_all = TRUE)

	c_area_hind_dat_sum_yr <- c_area_hind_dat_sum %>%
		group_by(year) %>%
		dplyr::summarize(area = sum(area_km2)) %>% 
		mutate(sp_hab_threshold = "core")

	# avg core area 1970 - 2020
	c_area_hind_avg <- mean(c_area_hind_dat_sum_yr$area)

	# potential habitat = sum of area where sps >= 0.5
	
	p_area_hind_dat <- ROMS_hindcast_dat %>%
		filter(sp_hab_suit >= 0.5) 
	
	p_area_hind_dat_sum <- p_area_hind_dat %>%
		group_by(latitude, longitude, year) %>%
		distinct(across(c(latitude, longitude)), .keep_all = TRUE)

	p_area_hind_dat_sum_yr <- p_area_hind_dat_sum %>%
		group_by(year) %>%
		dplyr::summarize(area = sum(area_km2)) %>% ## avg per cell across a given time period
		mutate(sp_hab_threshold = "potential")
	
	# avg potential area 1970 - 2020
	p_area_hind_avg <- mean(p_area_hind_dat_sum_yr$area)
	
	# join together 
	
	hind_area_yr <- bind_rows(c_area_hind_dat_sum_yr, p_area_hind_dat_sum_yr)

	
	#### projections ####
	
	# remove historical years for plotting for presentations
	years_proj <- 2021:2099
	
	ROMS_projected_dat_proj <- ROMS_projected_dat %>%
		filter(year %in% years_proj)
	
	# with bias-corrected temperature using variance ratio
	
	c_area_proj_dat <- ROMS_projected_dat_proj %>%
		filter(sp_hab_suit_var >= 0.9) 

	c_area_proj_dat_sum <- c_area_proj_dat %>%
		group_by(simulation, projection, latitude, longitude, year) %>%
		distinct(across(c(latitude, longitude)), .keep_all = TRUE)
	
	c_area_proj_dat_sum_yr <- c_area_proj_dat_sum %>%
		group_by(simulation, projection, year) %>%
		summarize(area = sum(area_km2)) %>% ## avg per cell across a given time period
		mutate(sp_hab_threshold = "core")

	# max area core
	c_area_proj_max <- c_area_proj_dat_sum_yr %>%
		group_by(scenario) %>%
		dplyr::summarize(max_area = max(area))

	# percent increase
	c_area_low <- ((c_area_proj_max[1,2]) - c_area_hind_avg)/c_area_hind_avg
	c_area_high <- ((c_area_proj_max[2,2]) - c_area_hind_avg)/c_area_hind_avg
	
	# potential habitat = sum of area where sps >= 0.5
	
	p_area_proj_dat <- ROMS_projected_dat_proj %>%
		filter(sp_hab_suit_var >= 0.5) 

	p_area_proj_dat_sum <- p_area_proj_dat %>%
		group_by(simulation, projection, latitude, longitude, year) %>%
		distinct(across(c(latitude, longitude)), .keep_all = TRUE)
	
	p_area_proj_dat_sum_yr <- p_area_proj_dat_sum %>%
		group_by(simulation, projection, year) %>%
		summarize(area = sum(area_km2)) %>% ## avg per cell across a given time period
		mutate(sp_hab_threshold = "potential")
	
	# max area pot
	p_area_proj_max <- p_area_proj_dat_sum_yr %>%
		group_by(scenario) %>%
		dplyr::summarize(max_area = max(area))

	# percent increase
	p_area_low <- ((p_area_proj_max[1,2]) - p_area_hind_avg)/p_area_hind_avg
	p_area_high <- ((p_area_proj_max[2,2]) - p_area_hind_avg)/p_area_hind_avg

	# join together
	
	proj_area_yr <- bind_rows(c_area_proj_dat_sum_yr, p_area_proj_dat_sum_yr)
	
	# for plotting by scenario
		
	proj_area_yr <- proj_area_yr %>% filter(., projection != "historical")
		
	proj_area_yr$scen <- NA
		
	proj_area_yr$scen[proj_area_yr$projection == "ssp126"] <- "low emission\n(ssp126)"
	proj_area_yr$scen[proj_area_yr$projection == "ssp585"] <- "high emission\n(ssp585)"
	
	proj_area_yr <- tidyr::unite(proj_area_yr,"sim_proj",
															 simulation, projection, remove = F)

	colors <- c("#6dc3a9", "#ff7373", # cesm low, cesm high
						  "#4e8d9c", "#ff4040", # gfdl low, gfdl high
						  "#97c3e5", "#ffb733") # miroc low, miroc high

	sim_proj <- unique(proj_area_yr$sim_proj)
	
	names(colors) <- unique(proj_area_yr$sim_proj)
	
	# order facets
	proj_area_yr$scen_f = factor(proj_area_yr$scen, levels=c('low emission\n(ssp126)', 'high emission\n(ssp585)'))
	
	# plot
		

	scientific_10 <- function(x) {   
		parse(text=gsub("e\\+*", " %*% 10^", scales::scientific_format()(x))) 
		}							

	area_plot <-    
   	ggplot(data = hind_area_yr) +
	 	geom_line(aes(year, area, alpha = 0.5),
            data = . %>% filter(sp_hab_threshold == "potential"), color = "black") +
		geom_line(data = proj_area_yr, 
							aes(year, area, color = sim_proj, group = sim_proj, alpha = 0.5)) +
		facet_grid(sp_hab_threshold ~ scen_f) +
	  geom_line(aes(year, area, colour = sp_hab_threshold, alpha = 0.5),
            data = . %>% filter(sp_hab_threshold == "core"), color = "black") +
		xlab("Year") +
		scale_color_manual(name = "sim_proj", values = colors) +
	  scale_y_continuous(
	  	name =	"Area (x 10<sup>5</sup> km<sup>2</sup>)",
	  	breaks = c(100000, 200000, 300000, 400000, 500000),
	  	labels = c(1, 2, 3, 4,5),
	  	limits = c(19000, 567000)) +
   	xlim(1970, 2110) +
    theme_bw() +
  	theme(legend.position = "none") +
  	theme(
			strip.background = element_blank(),
  		strip.text = element_text(size = 18, face = "bold"),
			axis.text = element_text(size = 16, colour = "grey50"),
  	  axis.ticks = element_line(colour = "grey50"),
  	  axis.line = element_line(colour = "grey50"),
			axis.title.x = element_text(size = 16, color = "grey50"),
  	  axis.title.y = element_markdown(size=16, color = "grey30"),
  	  panel.grid.major = element_blank(),
  	  panel.grid.minor = element_blank(),
  	  panel.border = element_rect(fill = NA, color = "grey50"))
	
	area_plot_labs <- 
			ggdraw(area_plot) +
			draw_label("cesm", x = 0.475, y = 0.65, color = "#6dc3a9", size = 12) +
			draw_label("gfdl", x = 0.475, y = 0.55, color = "#4e8d9c", size = 12) +
			draw_label("miroc", x = 0.475, y = 0.60, color = "#97c3e5", size = 12) +
			draw_label("cesm", x = 0.93, y = 0.62, color = "#ffabab", size = 12) +
			draw_label("gfdl", x = 0.93, y = 0.65, color = "#ff4040", size = 12) +
			draw_label("miroc", x = 0.93, y = 0.73, color = "#ffb733", size = 12) 

	ggsave("./output/plots/area_plot_labs.png",
			 area_plot_labs,
			 width = 10, height = 5, units = "in")
	

	## in black 
		
	area_plot_black <-    
   	ggplot(data = hind_area_yr) +
	 	geom_line(aes(year, area, alpha = 0.5),
            data = . %>% filter(sp_hab_threshold == "potential"), color = "lightgrey") +
		geom_line(data = proj_area_yr, 
							aes(year, area, color = sim_proj, group = sim_proj, alpha = 0.5)) +
		facet_grid(sp_hab_threshold ~ scen_f) +
	  geom_line(aes(year, area, colour = sp_hab_threshold, alpha = 0.5),
            data = . %>% filter(sp_hab_threshold == "core"), color = "lightgrey") +
		xlab("Year") +
		scale_color_manual(name = "sim_proj", values = colors) +
	  scale_y_continuous(
	  	name = "Area (x 10<sup>5</sup>km<sup>2</sup>)",
	  	breaks = c(0, 200000, 400000),
	  	labels = scientific_10) +
   	xlim(1970, 2110) +
    theme_bw() +
  	theme(legend.position = "none") +
  	theme(
			strip.background = element_blank(),
  		strip.text = element_text(size = 16, color = "lightgrey"),
			axis.text = element_text(size = 14, colour = "white"),
  	  axis.ticks = element_line(colour = "white"),
  	  axis.line = element_line(colour = "white"),
  	  axis.title.x = element_text(size=16, color = "white"),
			axis.title.y = element_markdown(size = 16, color ="white"),
			panel.background = element_rect(fill = "black", color = "lightgrey", size = 1),
  	  panel.grid.major = element_blank(),
  	  panel.grid.minor = element_blank(),
  	  panel.border = element_rect(fill = NA, color = "black"),
			plot.background = element_rect(fill = "black", color = "black"))
	
	
		ggsave("./output/plots/area_plot_black.png",
			 area_plot_black,
			 width = 10, height = 5, units = "in")
	
		
	#### by month ####
		
	#### hindcasts ####
	
	c_area_hind_dat <- ROMS_hindcast_dat %>%
		filter(sp_hab_suit >= 0.9) 

	c_area_hind_dat_sum_mo <- c_area_hind_dat %>%
		group_by(latitude, longitude, year, month_name, month) %>%
		distinct(across(c(latitude, longitude)), .keep_all = TRUE)

	c_area_hind_dat_sum_mo <- c_area_hind_dat_sum_mo %>%
		group_by(year, month, month_name) %>%
		summarize(area = sum(area_km2)) %>%
		mutate(sp_hab_threshold = "core")

	# potential habitat = sum of area where sps >= 0.5
	
	
	p_area_hind_dat <- ROMS_hindcast_dat %>%
		filter(sp_hab_suit >= 0.5) 

	p_area_hind_dat_sum_mo <- p_area_hind_dat %>%
		group_by(latitude, longitude, year, month_name, month) %>%
		distinct(across(c(latitude, longitude)), .keep_all = TRUE)

	p_area_hind_dat_sum_mo <- p_area_hind_dat_sum_mo %>%
		group_by(year, month, month_name) %>%
		summarize(area = sum(area_km2)) %>%
		mutate(sp_hab_threshold = "potential")
	
	# join together 
	
	hind_area_mo <- bind_rows(c_area_hind_dat_sum_mo, p_area_hind_dat_sum_mo)
	
	# reorder for plotting
	hind_area_mo$month_name <- factor(hind_area_mo$month_name)
  hind_area_mo$month_name <- fct_reorder(hind_area_mo$month_name, 
  																		hind_area_mo$month)
	
	#### projections ####
	
	# core habitat 
	
	c_area_proj_dat <- ROMS_projected_dat %>%
		filter(sp_hab_suit_var >= 0.9) 

	c_area_proj_dat_sum_mo <- c_area_proj_dat %>%
		group_by(simulation, projection, latitude, longitude, year, month_name, month) %>%
		distinct(across(c(latitude, longitude)), .keep_all = TRUE)

	c_area_proj_dat_sum_mo <- c_area_proj_dat_sum_mo %>%
		group_by(simulation, projection, year, month, month_name) %>%
		summarize(area = sum(area_km2)) %>%
		mutate(sp_hab_threshold = "core")

	# potential habitat = sum of area where sps >= 0.5
	
	p_area_proj_dat <- ROMS_projected_dat %>%
		filter(sp_hab_suit_var >= 0.5) 

	p_area_proj_dat_sum_mo <- p_area_proj_dat %>%
		group_by(simulation, projection, latitude, longitude, year, month_name, month) %>%
		distinct(across(c(latitude, longitude)), .keep_all = TRUE)

	p_area_proj_dat_sum_mo <- p_area_proj_dat_sum_mo %>%
		group_by(simulation, projection, year, month, month_name) %>%
		summarize(area = sum(area_km2)) %>%
		mutate(sp_hab_threshold = "potential")

	# join together
	
	proj_area_mo <- bind_rows(c_area_proj_dat_sum_mo, p_area_proj_dat_sum_mo)
	
	# plot
	
	proj_area_mo <- proj_area_mo %>% filter(projection != "historical")
		
	proj_area_mo <- tidyr::unite(proj_area_mo,"sim_proj",
															 simulation, projection, remove = F)

	colors <- c("#6dc3a9", "#ff7373", # cesm low, cesm high
						  "#4e8d9c", "#ff4040", # gfdl low, gfdl high
						  "#97c3e5", "#ffb733") # miroc low, miroc high

	sim_proj <- unique(proj_area_mo$sim_proj)
	
	names(colors) <- unique(proj_area_mo$sim_proj)
	
	# order facets
	proj_area_mo$scen <- NA
		
	proj_area_mo$scen[proj_area_mo$projection == "ssp126"] <- "low emission\n(ssp126)"
	proj_area_mo$scen[proj_area_mo$projection == "ssp585"] <- "high emission\n(ssp585)"

	proj_area_mo$scen_f = factor(proj_area_mo$scen, levels=c('low emission\n(ssp126)', 'high emission\n(ssp585)'))

	# plots
	
	core_area_mo <- proj_area_mo %>%
		filter(sp_hab_threshold == "core")

  core_area_mo$month_name <- factor(core_area_mo$month_name)
  core_area_mo$month_name <- fct_reorder(core_area_mo$month_name, 
  																		core_area_mo$month)

	core_area_mo_plot <- 
		ggplot(hind_area_mo) +
		geom_line(aes(year, area),
		  data = . %>% filter(sp_hab_threshold == "core"), color = "black",  alpha = 0.5) +
		geom_line(data = core_area_mo, 
							aes(year, area, color = sim_proj, group = sim_proj), alpha = 0.5) +
		facet_grid(scen_f ~ month_name) +
		xlab("Year") +
		scale_color_manual(name = "sim_proj", values = colors) +
 scale_y_continuous(
	  	name =	"Area (x 10<sup>5</sup> km<sup>2</sup>)",
	  	breaks = c(100000, 200000, 300000),
	  	labels = c(1, 2, 3),
	  	limits = c(7000, 300000)) +
   	xlim(1970, 2100) +
		ggtitle("Core habitat") +
		theme_bw() +
  	theme(legend.position = "none") +
  	theme(
			strip.background = element_blank(),
  		strip.text = element_text(size = 12),
			axis.text = element_text(size = 10, colour = "grey50"),
  	  axis.ticks = element_line(colour = "grey50"),
  	  axis.line = element_line(colour = "grey50"),
  	  axis.title.x = element_text(size=14, color = "grey30"),
			axis.title.y = element_markdown(size = 14, color = "grey50"),
  	  panel.grid.major = element_blank(),
  	  panel.grid.minor = element_blank(),
			plot.title = element_text(size = 14, face = "bold", color = "black", hjust = 0.5),
  	  panel.border = element_rect(fill = NA, color = "grey50"))
		
	ggsave("./output/plots/core_area_mo_plot.png",
			 core_area_mo_plot,
			 width = 10, height = 7, units = "in")
 
	
	# potential
	
	potential_area_mo <- proj_area_mo %>%
		filter(sp_hab_threshold == "potential")

  potential_area_mo$month_name <- factor(potential_area_mo$month_name)
  potential_area_mo$month_name <- fct_reorder(potential_area_mo$month_name, 
  																		potential_area_mo$month)

  
	potential_area_mo_plot <- 
		ggplot(hind_area_mo) +
		geom_line(aes(year, area),
		  data = . %>% filter(sp_hab_threshold == "potential"), color = "black",  alpha = 0.5) +
		geom_line(data = potential_area_mo, 
							aes(year, area, color = sim_proj, group = sim_proj), alpha = 0.5) +
		facet_grid(scen_f ~ month_name) +
		xlab("Year") +
		scale_color_manual(name = "sim_proj", values = colors) +
 scale_y_continuous(
	  	name =	"Area (x 10<sup>5</sup> km<sup>2</sup>)",
	  	breaks = c(100000, 200000, 300000),
	  	labels = c(1, 2, 3),
	  	limits = c(35000, 400000)) +
   	xlim(1970, 2100) +
		ggtitle("Potential habitat") +
		theme_bw() +
  	theme(legend.position = "none") +
  	theme(
			strip.background = element_blank(),
  		strip.text= element_text(size = 12),
			axis.text = element_text(size = 10, colour = "grey50"),
  	  axis.ticks = element_line(colour = "grey50"),
  	  axis.line = element_line(colour = "grey50"),
 			axis.title.x = element_text(size=14, color = "grey30"),
			axis.title.y = element_markdown(size = 14, color = "grey50"),  	  
			panel.grid.major = element_blank(),
  	  panel.grid.minor = element_blank(),
			plot.title = element_text(size = 14, face = "bold", color = "black", hjust = 0.5),
  	  panel.border = element_rect(fill = NA, color = "grey50"))
		
	ggsave("./output/plots/potential_area_mo_plot.png",
			 potential_area_mo_plot,
			 width = 10, height = 7, units = "in")
 
  	# plot side by side
	
	plot1 <- core_area_mo_plot + theme(plot.margin = unit(c(0.2, 0, 0.2, 0.2), "in"))
	plot2 <- potential_area_mo_plot + theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0), "in"))
	
	area_mo_plot_combined <- plot1 + plot2
	 		
  ggsave("./output/plots/area_mo_plot_combined.png",
			 area_mo_plot_combined,
			 width = 10, height = 5, units = "in")

  
  #### maps of area averaged across first 20 years and last 20 years ####
	first_yrs <- 2001:2020
  
  first20_dat <- ROMS_hindcast_dat_sf %>%
  	filter(year %in% first_yrs) %>%
  	group_by(latitude, longitude, long_not_360) %>%
  	dplyr::summarize(mean_sp_hab_suit = mean(sp_hab_suit))
  
  # plot
   first20_plot <- 
    	  	ggplot() +
					geom_sf(data = first20_dat, aes(color = mean_sp_hab_suit))  +
					geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
					coord_sf(crs = 3338) +
					scale_color_gradientn(colors = c("#B3E5FC", "#B3E5FC", 
																					 "#01579B", "#01579B",
																					 "#00345C", "#00345C"),
																values = c(0, 0.499, 0.5, 0.899, 0.9, 1),
																breaks = c(0.1, 0.5, 0.9),
																labels = format(c(0.1, 0.5, 0.9)),
																limits = c(0, 1)) +
 					scale_x_continuous(
 						breaks = c(-170, -160),
 						labels = c("-170˚", "-160˚"),
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
 						panel.border = element_rect(color = "#666666"),
 						legend.position = "none",
 						axis.text = element_text(size = 8,  color = "#666666"),	
  					axis.title = element_text(size = 10,  color = "#666666"),
 						axis.ticks = element_line(color = "#666666"),
 						plot.margin = unit(c(-0.05,-0.05,-0.05, -0.05), "in"))
    	  
  last_yrs <- 2080:2099
  
 # last20_dat <- ROMS_projected_dat_sf %>%
 # 	filter(year %in% last_yrs) %>%
 # 	group_by(simulation, projection, latitude, longitude, long_not_360) %>%
 # 	dplyr::summarize(mean_sp_hab_suit = mean(sp_hab_suit_var))
 
  # plot
  last20_dat$scen <- NA
  last20_dat$scen[last20_dat$projection == "ssp126"] <- "low\nemission\n(ssp126)"
	last20_dat$scen[last20_dat$projection == "ssp585"] <- "high\nemission\n(ssp585)"
	
	last20_dat$scen_f = factor(last20_dat$scen, levels=c('low\nemission\n(ssp126)', 
																											 'high\nemission\n(ssp585)'))

  last20_plot <- 
    	  	ggplot() +
					geom_sf(data = last20_dat, aes(color = mean_sp_hab_suit))  +
					geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
					coord_sf(crs = 3338) +
    			facet_grid(scen_f ~ simulation) +
					scale_color_gradientn(colors = c("#B3E5FC", "#B3E5FC", 
																					 "#01579B", "#01579B",
																					 "#00345C", "#00345C"),
																values = c(0, 0.499, 0.5, 0.899, 0.9, 1),
																breaks = c(0.1, 0.5, 0.9),
																labels = format(c(0.1, 0.5, 0.9)),
																limits = c(0, 1)) +
 					scale_x_continuous(
 						breaks = c(-170, -160),
 						labels = c("-170˚", "-160˚"),
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
 						panel.border = element_rect(color = "#666666"),
 						legend.position = "none",
 						strip.text = element_text(size = 10, face = "bold",  color = "#808080"),
 						strip.text.y = element_text(angle = 0,  color = "#666666"),
 						strip.background = element_blank(),
 						axis.text.x = element_text(size = 8,  color = "#666666"),	
  					axis.title.x = element_text(size = 10,  color = "#666666"),
 						axis.ticks.y = element_blank(),
 						axis.title.y = element_blank(),
 						axis.text.y = element_blank(),
 						axis.ticks.x = element_line( color = "#666666"),
 						panel.spacing = unit(0.25, "lines"),
 						plot.margin = unit(c(-0.05,-0.05,-0.05, -0.05), "in"))

    
   # extract legend
   legend_plot <-     
    	  	ggplot() +
					geom_sf(data = first20_dat, aes(color = mean_sp_hab_suit))  +
					geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
					coord_sf(crs = 3338) +
					scale_color_gradientn(colors = c("#B3E5FC", "#B3E5FC", 
																					 "#01579B", "#01579B",
																					 "#00345C", "#00345C"),
																values = c(0, 0.499, 0.5, 0.899, 0.9, 1),
																breaks = c(0.1, 0.5, 0.9),
																labels = format(c(0.1, 0.5, 0.9)),
																limits = c(0, 1)) +
 					scale_x_continuous(
 						breaks = c(-170, -160),
 						labels = c("-170˚", "-160˚"),
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
 						strip.text = element_text(size = 12, face = "bold"),
 						strip.background = element_blank(),
 						axis.text = element_text(size = 8),	
  					axis.title = element_text(size = 10),
 						legend.title.align = 0.5,
 						legend.title = element_text( color = "#666666", size = 9),
 						legend.text = element_text(color = "#666666", size = 7),
 						plot.margin = unit(c(-0.05,-0.05,-0.05, -0.05), "in"))

 
  legend_plot <- cowplot::get_legend(legend_plot) 
  legend_plot <- 	ggpubr::as_ggplot(legend_plot) 

  # plot together
	hab_maps <- first20_plot + last20_plot + legend_plot + plot_layout(ncol = 3, widths = c(1.3,4,1))

	hab_maps2 <- hab_maps +  annotate("text", label = "Current\n(2001 - 2020)", 
  																	x = -5.2, y = 0.83, size = 5, fontface = 2)
	hab_maps_form <- hab_maps2 + 
		theme(plot.margin = unit(c(0.25, 0, 0, 0), "in")) +
		annotate("text", label = "Projected (2080 - 2099)",	
						 x = -2.5, y = 1.1, size = 5, fontface = 2)
		
  ggsave("./output/plots/hab_maps_form.png",
			 hab_maps_form,
			 height = 5,
			 width = 10)
  
  
  
  

  
  
  
  
  
  
    
    
  # averaged across all simulations
    
   last20_dat_avg <- ROMS_projected_dat_sf %>%
  	filter(year %in% last_yrs) %>%
  	group_by(projection, latitude, longitude, long_not_360) %>%
  	summarize(mean_sp_hab_suit = mean(sp_hab_suit_var))
  
  # plot
   
   # ssp126
   last20_avg_ssp126_plot <- 
    	  	ggplot(last20_dat_avg) +
					geom_sf(aes(color = mean_sp_hab_suit),
						data = . %>% filter(projection == "ssp126")) +
					geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
					coord_sf(crs = 3338) +
					scale_color_gradientn(colors = c("#B3E5FC", "#B3E5FC", 
																					 "#01579B", "#01579B",
																					 "#00345C", "#00345C"),
																values = c(0, 0.499, 0.5, 0.899, 0.9, 1),
																breaks = c(0.1, 0.5, 0.9),
																labels = format(c(0.1, 0.5, 0.9)),
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
    	  	labs(colour = "Spawning\nhabitat\nsuitability") +
					theme_bw() +
 					theme(
 						strip.text = element_text(size = 14, face = "bold"),
 						strip.background = element_blank(),
 						axis.text.y = element_text(size = 12),	
  					axis.title.y = element_text(size = 14),
 						axis.text.x = element_blank(),
 						axis.title.x = element_blank(),
 						axis.ticks.x = element_blank(),
  					legend.title.align=0.5)
   
   # no legend
   last20_avg_ssp126_plot_noleg <- 
    	  	ggplot(last20_dat_avg) +
					geom_sf(aes(color = mean_sp_hab_suit),
						data = . %>% filter(projection == "ssp126")) +
					geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
					coord_sf(crs = 3338) +
					scale_color_gradientn(colors = c("#B3E5FC", "#B3E5FC", 
																					 "#01579B", "#01579B",
																					 "#00345C", "#00345C"),
																values = c(0, 0.499, 0.5, 0.899, 0.9, 1),
																breaks = c(0.1, 0.5, 0.9),
																labels = format(c(0.1, 0.5, 0.9)),
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
    	  	labs(colour = "Spawning\nhabitat\nsuitability") +
					theme_bw() +
 					theme(
 						legend.position = "none",
 						strip.text = element_text(size = 14, face = "bold"),
 						strip.background = element_blank(),
 						axis.text.y = element_text(size = 12),	
  					axis.title.y = element_text(size = 14),
 						axis.text.x = element_blank(),
 						axis.title.x = element_blank(),
 						axis.ticks.x = element_blank(),
  					legend.title.align=0.5)
    
  # ssp585
  last20_avg_ssp1585_plot <- 
    	  	ggplot(last20_dat_avg) +
					geom_sf(aes(color = mean_sp_hab_suit),
						data = . %>% filter(projection == "ssp585")) +
					geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
					coord_sf(crs = 3338) +
					scale_color_gradientn(colors = c("#B3E5FC", "#B3E5FC", 
																					 "#01579B", "#01579B",
																					 "#00345C", "#00345C"),
																values = c(0, 0.499, 0.5, 0.899, 0.9, 1),
																breaks = c(0.1, 0.5, 0.9),
																labels = format(c(0.1, 0.5, 0.9)),
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
    	  	labs(colour = "Spawning\nhabitat\nsuitability") +
					theme_bw() +
 					theme(
 						legend.position = "none",
 						strip.text = element_text(size = 14, face = "bold"),
 						strip.background = element_blank(),
 						axis.text = element_text(size = 12),	
  					axis.title = element_text(size = 14),
  					legend.title.align=0.5)  
  
  legend_plot <- cowplot::get_legend(last20_avg_ssp126_plot) 
  
  # put plots together
	first20_plot <- first20_plot + theme(plot.margin = unit(c(0.2, 0, 0.2, 0.2), "in"))
	legend_plot <- 	ggpubr::as_ggplot(legend_plot) + theme(plot.margin = unit(c(1, 1, 0.2, 0.2), "in"))
	
	proj_maps <- last20_avg_ssp126_plot_noleg / last20_avg_ssp1585_plot
	
  hab_maps <- first20_plot + proj_maps + plot_layout(ncol = 2, widths = c(1, 1.5))
  
  
  + legend_plot
  
  ggsave("./output/plots/hab_maps.png",
			 hab_maps)
	
  
  	# try fixing labels
  		
  facet_labeller_top <- function(variable, value) {
  c(
    "cesm", 
    "",
    "gfdl",
    "",
    "miroc",
    ""
  )
}

facet_labeller_bottom <- function(variable, value) {
  c(
    "0.5", 
    "0.9",
    "0.5",
    "0.9",
    "0.5", 
    "0.9"
  )
}
  	
	area_plot_mo <-    
   	ggplot(data = hind_area_mo) +
	 	geom_line(aes(year, total_area), alpha = 0.5,
            data = . %>% filter(sp_hab_threshold == 0.5), color = "black") +
		geom_line(data = proj_area_mo_plot, 
							aes(year, total_area, color = sim_proj, group = sim_proj, alpha = 0.5)) +
		facet_grid(month_name ~ simulation + sp_hab_threshold,
							  labeller = labeller(
							 	simulation=as_labeller(facet_labeller_top),
                sp_hab_threshold = as_labeller(facet_labeller_bottom))) +
	  geom_line(aes(year, total_area, colour = sp_hab_threshold, alpha = 0.5),
            data = . %>% filter(sp_hab_threshold == 0.9), color = "black") +
		geom_line(data = hist_data, 
							aes(year, total_area), color = "lightgrey", alpha = 0.5) +
		xlab("Year") +
		scale_color_manual(name = "sim_proj", values = colors) +
	  scale_y_continuous(
	  	name = expression(paste("Total area x",' '(10^{5}),' '(km^{2}))),
	  	#name = expression(paste("Total area", ' '(km^{2}))),
	  	breaks = c(0, 500000, 1000000, 15000000),
	  	labels = c(0, 5, 10, 15)
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
	
	
		ggsave("./output/plots/area_plot_mo.png",
			 area_plot_mo,
			 width = 13, height = 7, units = "in")
	
	
	
	
	
	
	
			
		
		
		
		
	


	# plot

plot <-    
   	ggplot(data = core_area_hind_dat_sum_yr) +
   	geom_line(aes(x = year, y = total_area), color = "#00345C", size = 1) +
	  geom_line(data = pot_area_hind_dat_sum_yr, aes(x = year, y = total_area), 
	  					color = "#01579B", size = 1) +
   	xlab("Year") + 
	  scale_y_continuous(
	  	name = expression(paste("Total area", ' '(km^{2}))),
	  	breaks = c(0, 20000, 40000, 60000),
	  	labels = c(0, 20000, 40000, 60000)
	  ) +
   	xlim(1970, 2020) +
   	theme_bw() +
  	theme(legend.position = "none") +
  	theme(
  	  axis.text=element_text(size=10, colour = "grey50"),
  	  axis.ticks = element_line(colour = "grey50"),
  	  axis.line = element_line(colour = "grey50"),
  	  axis.text.x = element_text(size = 12),
  	  axis.title= element_text(size = 14, color = "grey30"),
  	  panel.grid.major = element_blank(),
  	  panel.grid.minor = element_blank(),
  	  panel.border = element_rect(fill = NA, color = "grey50"))
   
	ggsave("./output/plots/total_area_year.png",
			 plot,
			 width = 10, height = 7, units = "in")

# add a key for cold vs warm years
	
	years <- c(1995:2010)
	

	temp_index <- c("cold", "average", "cold", "warm", "cold", "average", "warm", 
									"warm", "warm", "warm", "warm", "average", "cold", "cold", 
									"cold", "cold")
	yr_stanzas <- data.frame(years, temp_index)
	
	temp_index <- c("cold", "average", "warm")
	colors <- c("blue", "grey", "red")
	
	color_key <- data.frame(temp_index, colors)
	
	yr_stanzas <- merge(yr_stanzas, color_key, by = "temp_index")
	
	
	
	##### fix this #### 
	
	plot <-    
   	ggplot(data = core_area_dat_sum_yr) +
   	geom_line(aes(x = year, y = total_core_area), color = "#00345C", size = 1) +
	  geom_line(data = pot_area_dat_sum_yr, aes(x = year, y = total_pot_area), 
	  					color = "#01579B", size = 1) +
		geom_rect(data = yr_stanzas, aes(xmin = years)) +
   	xlab("Year") + 
	  scale_y_continuous(
	  	name = expression(paste("Total area", ' '(km^{2}))),
	  	breaks = c(0, 20000, 40000, 60000),
	  	labels = c(0, 20000, 40000, 60000)
	  ) +
   	xlim(1970, 2020) +
   	theme_bw() +
  	theme(legend.position = "none") +
  	theme(
  	  axis.text=element_text(size=10, colour = "grey50"),
  	  axis.ticks = element_line(colour = "grey50"),
  	  axis.line = element_line(colour = "grey50"),
  	  axis.text.x = element_text(size = 12),
  	  axis.title= element_text(size = 14, color = "grey30"),
  	  panel.grid.major = element_blank(),
  	  panel.grid.minor = element_blank(),
  	  panel.border = element_rect(fill = NA, color = "grey50"))
	
#### by month ###

# core habitat = sum of area where sps >= 0.9

core_area_dat_sum_mo <- core_area_dat %>%
	group_by(month_name, year) %>%
	summarize(total_core_area = sum(sp_hab_suit))

# potential habitat = sum of area where sps >= 0.5

pot_area_dat_sum_mo <- pot_area_dat %>%
	group_by(month_name, year) %>%
	summarize(total_pot_area = sum(sp_hab_suit))


# plot

plot <-    
   	ggplot(data = core_area_dat_sum_mo) +
   	geom_line(aes(x = year, y = total_core_area), color = "#00345C", size = 1) +
	  geom_line(data = pot_area_dat_sum_mo, aes(x = year, y = total_pot_area), 
	  					color = "#01579B", size = 1) +
		facet_wrap(~ month_name) +
   	xlab("Year") + 
	  scale_y_continuous(
	  	name = expression(paste("Total area", ' '(km^{2}))),
	  	breaks = c(0, 5000, 10000, 15000),
	  ) +
   	xlim(1970, 2020) +
   	theme_bw() +
  	theme(legend.position = "none") +
  	theme(
  		strip.background = element_blank(),
  		strip.text = element_text(size = 12, face = "bold"),
  	  axis.text=element_text(size=12, colour = "grey50"),
  	  axis.ticks = element_line(colour = "grey50"),
  	  axis.line = element_line(colour = "grey50"),
  	  axis.text.x = element_text(size = 12),
  	  axis.title= element_text(size = 14, color = "grey30"),
  	  panel.grid.major = element_blank(),
  	  panel.grid.minor = element_blank(),
  	  panel.border = element_rect(fill = NA, color = "grey50"))
   
	ggsave("./output/plots/total_area_month_year.png",
			 plot,
			 width = 10, height = 7, units = "in")
	
	
	### total area with temp ####
	
	core_area_dat_sum_yr_temp <- core_area_dat %>%
		group_by(year) %>%
		summarize(total_core_area = sum(sp_hab_suit),
							mean_temp = mean(temp))

# potential habitat = sum of area where sps >= 0.5
	
	pot_area_dat_sum_yr_temp <- pot_area_dat %>%
		group_by(year) %>%
		summarize(total_pot_area = sum(sp_hab_suit),
							mean_temp = mean(temp))

	plot <-    
   	ggplot(data = core_area_dat_sum_yr_temp) +
   	geom_line(aes(x = year, y = total_core_area), color = "#00345C", size = 1) +
	  geom_line(data = pot_area_dat_sum_yr, aes(x = year, y = total_pot_area), 
	  					color = "#01579B", size = 1) +
   	xlab("Year") + 
	  scale_y_continuous(
	  	name = expression(paste("Total area", ' '(km^{2}))),
	  	breaks = c(0, 20000, 40000, 60000),
	  	labels = c(0, 20000, 40000, 60000)
	  ) +
   	xlim(1970, 2020) +
   	theme_bw() +
  	theme(legend.position = "none") +
  	theme(
  	  axis.text=element_text(size=10, colour = "grey50"),
  	  axis.ticks = element_line(colour = "grey50"),
  	  axis.line = element_line(colour = "grey50"),
  	  axis.text.x = element_text(size = 12),
  	  axis.title= element_text(size = 14, color = "grey30"),
  	  panel.grid.major = element_blank(),
  	  panel.grid.minor = element_blank(),
  	  panel.border = element_rect(fill = NA, color = "grey50"))
   
	ggsave("./output/plots/total_area_year.png",
			 plot,
			 width = 10, height = 7, units = "in")


	######### playing with sf functions
	
	grid_test <- st_make_grid(ROMS_hindcast_dat_sf) 

		ggplot() +
					geom_sf(data = grid_test)  +
					geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
					coord_sf(crs = 3338) +
					scale_color_viridis_c(breaks = c(0, 0.50, 1.0), limits = c(0,1)) +
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
					theme_bw()
		

