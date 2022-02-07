# area metric 

	#### by year ####
	
	#### hindcasts ####
	
	c_area_hind_dat <- ROMS_hindcast_dat %>%
		filter(sp_hab_suit >= 0.9) 

	c_area_hind_dat_sum_yr <- c_area_hind_dat %>%
		group_by(year) %>%
		summarize(total_area = sum(area_km2)) %>% ## avg per cell across a given time period
		mutate(sp_hab_threshold = "core")

	# potential habitat = sum of area where sps >= 0.5
	
	p_area_hind_dat <- ROMS_hindcast_dat %>%
		filter(sp_hab_suit >= 0.5) 
	
	p_area_hind_dat_sum_yr <- p_area_hind_dat %>%
		group_by(year) %>%
		summarize(total_area = sum(area_km2)) %>%
		mutate(sp_hab_threshold = "potential")
	
	# join together 
	
	hind_area_yr <- bind_rows(c_area_hind_dat_sum_yr, p_area_hind_dat_sum_yr)
	
#	hind_area_yr_0.9 <- hind_area_yr %>% filter(., sp_hab_threshold == 0.9)
#	hind_area_yr_0.5 <- hind_area_yr %>% filter(., sp_hab_threshold == 0.5)

	#### projections ####
	
	# with bias-corrected temperature using variance ratio
	
	c_area_proj_dat_var <- ROMS_projected_dat %>%
		filter(sp_hab_suit_var >= 0.9) 

	c_area_proj_dat_var_sum_yr <- c_area_proj_dat_var %>%
		group_by(simulation, projection, year) %>%
		summarize(total_area = sum(area_km2)) %>%
		mutate(sp_hab_threshold = "core")

	# potential habitat = sum of area where sps >= 0.5
	
	p_area_proj_dat_var <- ROMS_projected_dat %>%
		filter(sp_hab_suit_var >= 0.5)
	
	p_area_proj_dat_var_sum_yr <- p_area_proj_dat_var %>%
		group_by(simulation, projection, year) %>%
		summarize(total_area = sum(area_km2))  %>%
		mutate(sp_hab_threshold = "potential")
	
	# join together
	
	proj_area_yr_var <- bind_rows(c_area_proj_dat_var_sum_yr, p_area_proj_dat_var_sum_yr)
	
	# for plotting
	
	proj_area_yr_var <- tidyr::unite(proj_area_yr_var,"sim_proj",
															 simulation, projection, remove = F)

	proj_area_yr_var_plot <- proj_area_yr_var %>%
		filter(!str_detect(sim_proj, "_historical"))
	
	hist_data <- proj_area_yr_var %>%
		filter(str_detect(sim_proj, "_historical"))

	colors <- c("#efd966", "#b79a00", 
						  "#7fb27f", "#004700", 
						  "#6666b2", "#000059")

	sim_proj <- unique(proj_area_yr_var_plot$sim_proj)
	
	names(colors) <- unique(proj_area_yr_var_plot$sim_proj)
	
	# plots
	
	area_plot_yr_var <-    
   	ggplot(data = hind_area_yr) +
	 	geom_line(aes(year, total_area), alpha = 0.5,
            data = . %>% filter(sp_hab_threshold == "potential"), color = "black") +
		geom_line(data = proj_area_yr_var_plot, 
							aes(year, total_area, color = sim_proj, group = sim_proj, alpha = 0.5)) +
		facet_grid(sp_hab_threshold ~ simulation) +
	  geom_line(aes(year, total_area, colour = sp_hab_threshold, alpha = 0.5),
            data = . %>% filter(sp_hab_threshold == "core"), color = "black") +
		geom_line(data = hist_data, 
							aes(year, total_area), color = "lightgrey", alpha = 0.5) +
		xlab("Year") +
		scale_color_manual(name = "sim_proj", values = colors) +
	  #scale_y_continuous(
	  #	name = expression(paste("Total area x",' '(10^{6}),' '(km^{2}))),
	  	#name = expression(paste("Total area", ' '(km^{2}))),
	  #	breaks = c(0, 2000000, 4000000),
	  #	labels = c(0, 2, 4)
	  #) +
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
	
	
		ggsave("./output/plots/area_plot_yr_var.png",
			 area_plot_yr_var,
			 width = 10, height = 5, units = "in")
	
	
	#### by month ####
		
	#### hindcasts ####
	
	carea_hind_dat <- ROMS_hindcast_dat %>%
		filter(sp_hab_suit >= 0.9) 

	carea_hind_dat_sum_mo <- carea_hind_dat %>%
		group_by(year, month_name, month) %>%
		summarize(total_area = sum(area_km2)) %>%
		mutate(sp_hab_threshold = 0.9)

	# potential habitat = sum of area where sps >= 0.5
	
	parea_hind_dat <- ROMS_hindcast_dat %>%
		filter(sp_hab_suit >= 0.5) 
	
	parea_hind_dat_sum_mo <- parea_hind_dat %>%
		group_by(year, month_name, month) %>%
		summarize(total_area = sum(area_km2)) %>%
		mutate(sp_hab_threshold = 0.5)
	
	# join together 
	
	hind_area_mo <- bind_rows(carea_hind_dat_sum_mo, parea_hind_dat_sum_mo)
	
	#### projections ####
	
0
	# potential habitat = sum of area where sps >= 0.5
	
	parea_proj_dat <- ROMS_projected_dat %>%
		filter(sp_hab_suit >= 0.5)
	
	parea_proj_dat_sum_mo <- parea_proj_dat %>%
		group_by(simulation, projection, year, month_name, month) %>%
		summarize(total_area = sum(area_km2))  %>%
		mutate(sp_hab_threshold = 0.5)
	
	# join together
	
	proj_area_mo <- bind_rows(carea_proj_dat_sum_mo, parea_proj_dat_sum_mo)
	
	# plot
	
	proj_area_mo <- tidyr::unite(proj_area_mo,"sim_proj",
															 simulation, projection, remove = F)

	proj_area_mo_plot <- proj_area_mo %>%
		filter(!str_detect(sim_proj, "_historical"))
	
	hist_data <- proj_area_mo %>%
		filter(str_detect(sim_proj, "_historical"))

	colors <- c("#efd966", "#b79a00", 
						  "#7fb27f", "#004700", 
						  "#6666b2", "#000059")

	sim_proj <- unique(proj_area_mo_plot$sim_proj)
	
	names(colors) <- unique(proj_area_mo_plot$sim_proj)

	#### is this correct? ####
	
		  #	name = expression(paste("Total area x",' '(10^{6}),' '(km^{2}))),

	# reorder for plotting
	
  hind_area_mo$month_name <- factor(hind_area_mo$month_name)
  hind_area_mo$month_name <- fct_reorder(hind_area_mo$month_name, 
  																		hind_area_mo$month)


  proj_area_mo_plot$month_name <- factor(proj_area_mo_plot$month_name)
  proj_area_mo_plot$month_name <- fct_reorder(proj_area_mo_plot$month_name, 
  																		proj_area_mo_plot$month)

  
  hist_data$month_name <- factor(hist_data$month_name)
  hist_data$month_name <- fct_reorder(hist_data$month_name, 
  																		hist_data$month)

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
		

