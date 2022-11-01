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
	
	hind_mean_lat_yr <- bind_rows(hind_mean_lats_yr_0.5, hind_mean_lats_yr_0.9) 
	
	# mean latitude for both core and potential habitat during historical period
	mean_lat_hist <- hind_mean_lat_yr %>%
		group_by(sp_hab_threshold) %>%
		dplyr::summarize(mean_lat = mean(hist_mean_lat))
	
	#### projections ####
	
	# bias-corrected temp with variance ratio
		
	proj_mean_lat_yr <- function(x){
		
		new_dat <- ROMS_projected_dat %>%
			filter(., sp_hab_suit_var >= x)
		
		new_dat_sum <- new_dat %>%
			group_by(simulation, projection, year) %>%
			summarise(proj_mean_lat = mean(Lat)) 

		new_dat_sum		
	}
	
	sp_hab_thresholds <- c(0.5, 0.9)
	
	proj_mean_lat_yr <- lapply(sp_hab_thresholds, proj_mean_lat_yr)
	
	proj_mean_lats_yr_0.5 <- proj_mean_lat_yr[[1]] %>%
		mutate(sp_hab_threshold = 0.5)

	proj_mean_lats_yr_0.9 <- proj_mean_lat_yr[[2]]	%>%
		mutate(sp_hab_threshold = 0.9)
	
	proj_mean_lat_yr <- bind_rows(proj_mean_lats_yr_0.5, proj_mean_lats_yr_0.9) 
	
	proj_mean_lat_yr <- tidyr::unite(proj_mean_lat_yr,"sim_proj",
																			 simulation, projection, remove = F)
	
	# mean lat for core and potential habitat in 2100 
	mean_lat_proj <- proj_mean_lat_yr %>%
		filter(year == 2099) %>%
		group_by(sp_hab_threshold, projection) %>%
		dplyr::summarise(mean_lat_proj = mean(proj_mean_lat))
	
	mean_lat_proj <- merge(mean_lat_proj, mean_lat_hist, by = "sp_hab_threshold")
	
	mean_lat_proj <- mean_lat_proj %>%
		mutate(lat_increase = mean_lat - mean_lat_proj)

#	proj_mean_lats_yr_var_df_plot <- proj_mean_lats_yr_var_df %>%
#		filter(!str_detect(sim_proj, "_historical"))
#	
#	hist_data <- proj_mean_lats_yr_var_df %>%
#		filter(str_detect(sim_proj, "_historical"))
#
#	
#	colors <- c("#efd966", "#b79a00", 
#						  "#7fb27f", "#004700", 
#						  "#6666b2", "#000059")
#
#	sim_proj <- unique(proj_mean_lats_yr_var_df_plot$sim_proj)
#	
#	names(colors) <- unique(proj_mean_lats_yr_var_df_plot$sim_proj)
	
	
#	mean_latitude_plot_var <-    
#   	ggplot(data = hind_mean_lats_yr_df) +
#	 	geom_line(aes(year, hist_mean_lat, alpha = 0.5),
#            data = . %>% filter(sp_hab_threshold == 0.5), color = "black") +
#		geom_line(data = proj_mean_lats_yr_var_df_plot, 
#							aes(year, proj_mean_lat, color = sim_proj, group = sim_proj, alpha = 0.5)) +
#		facet_grid(sp_hab_threshold ~ simulation) +
#	  geom_line(aes(year, hist_mean_lat, colour = sp_hab_threshold, alpha = 0.5),
#            data = . %>% filter(sp_hab_threshold == 0.9), color = "black") +
#		geom_line(data = hist_data, 
#							aes(year, proj_mean_lat, color = "lightgrey", alpha = 0.5)) +
#		xlab("Year") +
#		scale_color_manual(name = "sim_proj", values = colors) +
#	  scale_y_continuous(
#	  	name = "Meanlatitude\nvar",
#	  	breaks = c(56, 58, 60, 62),
#	  	labels = c("56˚N", "58˚N", "60˚N", "62˚N")
#	  ) +
#   	xlim(1970, 2100) +
#    theme_bw() +
#  	theme(legend.position = "none") +
#  	theme(
#			strip.background = element_blank(),
#  		strip.text = element_text(size = 18, face = "bold"),
#			axis.text = element_text(size = 16, colour = "grey50"),
#  	  axis.ticks = element_line(colour = "grey50"),
#  	  axis.line = element_line(colour = "grey50"),
#  	  axis.title = element_text(size=18, color = "grey30"),
#  	  panel.grid.major = element_blank(),
#  	  panel.grid.minor = element_blank(),
#  	  panel.border = element_rect(fill = NA, color = "grey50"))
#	
#	
#		ggsave("./output/plots/mean_latitude_var_plot.png",
#			 mean_latitude_plot_var,
#			 width = 10, height = 5, units = "in")
#	
	## facet by scenario ####
	
	proj_mean_lat_yr <- proj_mean_lat_yr %>% filter(projection != "historical")
		
	proj_mean_lat_yr$scen <- NA
		
	proj_mean_lat_yr$scen[proj_mean_lat_yr$projection == "ssp126"] <- "low emission (ssp126)"
	proj_mean_lat_yr$scen[proj_mean_lat_yr$projection == "ssp585"] <- "high emission (ssp585)"
	
	proj_mean_lat_yr$thresh <- NA

	proj_mean_lat_yr$thresh[proj_mean_lat_yr$sp_hab_threshold == 0.9] <- "core"
	proj_mean_lat_yr$thresh[proj_mean_lat_yr$sp_hab_threshold == 0.5] <- "potential"
	
	hind_mean_lat_yr$thresh <- NA
	
	hind_mean_lat_yr$thresh[hind_mean_lat_yr$sp_hab_threshold == 0.9] <- "core"
	hind_mean_lat_yr$thresh[hind_mean_lat_yr$sp_hab_threshold == 0.5] <- "potential"
	

	colors <- c("#6dc3a9", "#ffabab", # cesm low, cesm high
						  "#4e8d9c", "#ff4040", # gfdl low, gfdl high
						  "#97c3e5", "#ffb733") # miroc low, miroc high

	sim_proj <- unique(proj_mean_lat_yr$sim_proj)
	
	names(colors) <- unique(proj_mean_lat_yr$sim_proj)
	
	# order facets
	proj_mean_lat_yr$scen_f = factor(proj_mean_lat_yr$scen, 
																	 levels=c('low emission (ssp126)', 'high emission (ssp585)'))
	# plot
	
	mean_latitude_plot <-    
   	ggplot(data = hind_mean_lat_yr) +
	 	geom_line(aes(year, hist_mean_lat),
            data = . %>% filter(sp_hab_threshold == 0.5), color = "black", alpha = 0.5) +
		geom_line(data = proj_mean_lat_yr, 
							aes(year, proj_mean_lat, color = sim_proj, group = sim_proj), alpha = 0.5) +
		facet_grid(thresh ~ scen_f) +
	  geom_line(aes(year, hist_mean_lat, colour = sp_hab_threshold),
            data = . %>% filter(sp_hab_threshold == 0.9), color = "black",  alpha = 0.5) +
		xlab("Year") +
		scale_color_manual(name = "sim_proj", values = colors) +
	  scale_y_continuous(
	  	name = "Mean latitude (˚N)",
	  	breaks = c(56, 57, 58, 59),
	  	labels = c(56, 57, 58, 59),
	  	limits = c(55.2, 59.7)
	  ) +
   	xlim(1970, 2110) +
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
	
		
	mean_latitude_plot_labs <- 
		ggdraw(mean_latitude_plot) +
		draw_label("cesm", x = 0.485, y = 0.79, color = "#6dc3a9", size = 12) +
		draw_label("gfdl", x = 0.485, y = 0.735, color = "#4e8d9c", size = 12) +
		draw_label("miroc", x = 0.485, y = 0.68, color = "#97c3e5", size = 12) +
		draw_label("cesm", x = 0.93, y = 0.85, color = "#ffabab", size = 12) +
		draw_label("gfdl", x = 0.93, y = 0.76, color = "#ff4040", size = 12) +
		draw_label("miroc", x = 0.93, y = 0.81, color = "#ffb733", size = 12) 

	ggsave("./output/plots/mean_latitude_plot.png",
			 mean_latitude_plot_labs,
			 width = 10, height = 5, units = "in")
	
	
	### in black ####
		
	mean_latitude_plot_black <-    
   	ggplot(data = hind_mean_lats_yr_df) +
	 	geom_line(aes(year, hist_mean_lat, alpha = 0.5),
            data = . %>% filter(sp_hab_threshold == 0.5), color = "lightgrey") +
		geom_line(data = proj_mean_lats_yr_var_df_plot, 
							aes(year, proj_mean_lat, color = sim_proj, group = sim_proj, alpha = 0.5)) +
		facet_grid(thresh ~ scen_f) +
	  geom_line(aes(year, hist_mean_lat, colour = sp_hab_threshold, alpha = 0.5),
            data = . %>% filter(sp_hab_threshold == 0.9), color = "lightgrey") +
		xlab("Year") +
		scale_color_manual(name = "sim_proj", values = colors) +
	  scale_y_continuous(
	  	name = "Mean latitude (˚N)",
	  	breaks = c(56, 57, 58, 59),
	  	labels = c(56, 57, 58, 59),
	  	limits = c(55.5, 59.7)
	  ) +
   	xlim(1970, 2100) +
     theme_bw() +
  	theme(legend.position = "none") +
  	theme(
			strip.background = element_blank(),
  		strip.text = element_text(size = 16, color = "lightgrey"),
			axis.text = element_text(size = 14, colour = "white"),
  	  axis.ticks = element_line(colour = "white"),
  	  axis.line = element_line(colour = "white"),
  	  axis.title = element_text(size=16, color = "white"),
			panel.background = element_rect(fill = "black", color = "lightgrey", size = 1),
  	  panel.grid.major = element_blank(),
  	  panel.grid.minor = element_blank(),
  	  panel.border = element_rect(fill = NA, color = "black"),
			plot.background = element_rect(fill = "black", color = "black"))
	
	
	
		ggsave("./output/plots/mean_latitude_black.png",
			 mean_latitude_plot_black,
			 width = 10, height = 5, units = "in")
	
	#### month ####

	# hindcasts 
		
	mean_lat_mo_hind_func <- function(x){
		
		new_dat <- ROMS_hindcast_dat %>%
			filter(., sp_hab_suit >= x)
		
		new_dat_sum <- new_dat %>%
			group_by(year, month_name, month) %>%
			summarise(hist_mean_lat = mean(latitude)) 

		new_dat_sum		
	}
	
	sp_hab_thresholds <- c(0.5, 0.9)
	
	mean_lats_mo_hind <- lapply(sp_hab_thresholds, mean_lat_mo_hind_func)
	
	mean_lats_mo_hind_0.5 <- mean_lats_mo_hind[[1]] %>%
		mutate(sp_hab_threshold = "potential")
	
	mean_lats_mo_hind_0.9 <- mean_lats_mo_hind[[2]]	%>%
		mutate(sp_hab_threshold = "core")
	
	mean_lat_mo_hind <- bind_rows(mean_lats_mo_hind_0.5, mean_lats_mo_hind_0.9)
	
	# reorder for plotting
	
	mean_lat_mo_hind$month_name <- factor(mean_lat_mo_hind$month_name)
  mean_lat_mo_hind$month_name <- fct_reorder(mean_lat_mo_hind$month_name, 
  																					mean_lat_mo_hind$month)
  
  # projections
  
	# bias-corrected temp with variance ratio
  		
  mean_lat_mo_proj_func <- function(x){
		
		new_dat <- ROMS_projected_dat %>%
			filter(., sp_hab_suit_var >= x)
		
		new_dat_sum <- new_dat %>%
			group_by(simulation, projection, year, month_name, month) %>%
			summarise(proj_mean_lat = mean(latitude)) 

		new_dat_sum		
	}
	
	sp_hab_thresholds <- c(0.5, 0.9)
	
	mean_lat_mo_proj <- lapply(sp_hab_thresholds, mean_lat_mo_proj_func)
	
	mean_lat_mo_proj_0.5 <- mean_lat_mo_proj[[1]] %>%
		mutate(sp_hab_threshold = "potential")
	
	mean_lat_mo_proj_0.9 <- mean_lat_mo_proj[[2]]	%>%
		mutate(sp_hab_threshold = "core")
	
	mean_lat_mo_proj <- bind_rows(mean_lat_mo_proj_0.5, mean_lat_mo_proj_0.9) 
	
	mean_lat_mo_proj <- mean_lat_mo_proj %>% filter(projection != "historical")
	
	mean_lat_mo_proj$scen <- NA
	
	mean_lat_mo_proj$scen[mean_lat_mo_proj$projection == "ssp126"] <- "low emission\n(ssp126)"
	mean_lat_mo_proj$scen[mean_lat_mo_proj$projection == "ssp585"] <- "high emission\n(ssp585)"

	
	# plot
  

	mean_lat_mo_proj <- tidyr::unite(mean_lat_mo_proj,"sim_proj",
																			 simulation, projection, remove = F)

	mean_lat_mo_proj <- mean_lat_mo_proj %>%
		filter(!str_detect(sim_proj, "_historical"))
	
	mean_lat_mo_proj <- mean_lat_mo_proj %>%
		tidyr::unite("sim_proj", simulation, projection, remove = F)
	
	colors <- c("#6dc3a9", "#ffabab", # cesm low, cesm high
						  "#4e8d9c", "#ff4040", # gfdl low, gfdl high
						  "#97c3e5", "#ffb733") # miroc low, miroc high

	sim_proj <- unique(mean_lat_mo_proj$sim_proj)
	
	names(colors) <- unique(mean_lat_mo_proj$sim_proj)
	
	# reorder for plotting
	mean_lat_mo_proj$month_name <- factor(mean_lat_mo_proj$month_name)
  mean_lat_mo_proj$month_name <- fct_reorder(mean_lat_mo_proj$month_name, 
  																		mean_lat_mo_proj$month)
  
 
	# order facets
	mean_lat_mo_proj$scen_f = factor(mean_lat_mo_proj$scen, levels=c('low emission\n(ssp126)', 
																																 'high emission\n(ssp585)'))
	
	core_mean_lat <- mean_lat_mo_proj %>%
		filter(., sp_hab_threshold == "core")
	
	core_mean_lat_plot <- 
		ggplot(mean_lat_mo_hind) +
		geom_line(aes(year, hist_mean_lat),
		  data = . %>% filter(sp_hab_threshold == "core"), color = "black",  alpha = 0.5) +
		geom_line(data = core_mean_lat, 
							aes(year, proj_mean_lat, color = sim_proj, group = sim_proj), alpha = 0.5) +
		facet_grid(scen_f ~ month_name) +
		xlab("Year") +
		scale_color_manual(name = "sim_proj", values = colors) +
		scale_y_continuous(
	  	name = "Mean latitude (˚N)",
	  	breaks = c(55, 56, 57, 58, 59),
	  	labels = c(55, 56, 57, 58, 59),
	  	limits = c(54.9, 59.7)
	  ) +
   	xlim(1970, 2100) +
		ggtitle("Core habitat") +
		theme_bw() +
  	theme(legend.position = "none") +
  	theme(
			strip.background = element_blank(),
			strip.text.y = element_blank(),
  		strip.text.x = element_text(size = 12),
			axis.text = element_text(size = 10, colour = "grey50"),
  	  axis.ticks = element_line(colour = "grey50"),
  	  axis.line = element_line(colour = "grey50"),
  	  axis.title = element_text(size=12, color = "grey30"),
  	  panel.grid.major = element_blank(),
  	  panel.grid.minor = element_blank(),
			plot.title = element_text(size = 14, face = "bold", color = "black", hjust = 0.5),
  	  panel.border = element_rect(fill = NA, color = "grey50"))
	
	#ggsave("./output/plots/core_mean_lat_plot.png",
	#		 core_mean_lat_plot,
	#		 width = 10, height = 7, units = "in")
  		
  # potential
	
	potential_mean_lat <- mean_lat_mo_proj %>%
		filter(., sp_hab_threshold == "potential")
	
	potential_mean_lat_plot <- 
		ggplot(mean_lat_mo_hind) +
		geom_line(aes(year, hist_mean_lat),
		  data = . %>% filter(sp_hab_threshold == "potential"), color = "black",  alpha = 0.5) +
		geom_line(data = potential_mean_lat, 
							aes(year, proj_mean_lat, color = sim_proj, group = sim_proj), alpha = 0.5) +
		facet_grid(scen_f ~ month_name) +
		#xlab("Year") +
		scale_color_manual(name = "sim_proj", values = colors) +
		scale_x_continuous(
			name = "Year",
			breaks = c(1990, 2080),
			limits = c(1970, 2100)
		) +
		scale_y_continuous(
	  	name = "Mean latitude (˚N)",
	  	breaks = c(55, 56, 57, 58, 59),
	  	labels = c(55, 56, 57, 58, 59),
	  	limits = c(54.9, 59.7)
	  ) +
   	#xlim(1970, 2100) +
		ggtitle("Potential habitat") +
		theme_bw() +
  	theme(legend.position = "none") +
  	theme(
			strip.background = element_blank(),
  		strip.text = element_text(size = 12),
			axis.text.x = element_text(size = 10, colour = "grey50"),
  	  axis.ticks.x = element_line(colour = "grey50"),
  	  axis.line.x = element_line(colour = "grey50"),
  	  axis.title.x = element_text(size=12, color = "grey30"),
			axis.title.y = element_blank(),
			axis.text.y = element_blank(),
			axis.line.y = element_blank(),
			axis.ticks.y = element_blank(),
  	  panel.grid.major = element_blank(),
  	  panel.grid.minor = element_blank(),
			plot.title = element_text(size = 14, face = "bold", color = "black", hjust = 0.5),
  	  panel.border = element_rect(fill = NA, color = "grey50"))
	
##	ggsave("./output/plots/potential_mean_lat_plot.png",
##			 potential_mean_lat_plot,
##			 width = 10, height = 7, units = "in")
  		
 
	# plot side by side
	
	plot1 <- core_mean_lat_plot + theme(plot.margin = unit(c(0.2, 0, 0.2, 0.2), "in"))
	plot2 <- potential_mean_lat_plot + theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0), "in"))
	
	mean_lat_mo <- plot1 + plot2
	 		
  ggsave("./output/plots/mean_lat_mo.png",
			 mean_lat_mo,
			 width = 10, height = 5, units = "in")
  
  
  
  
  
  
  
  
  
  		
#  	# try fixing labels
#  		
#  facet_labeller_top <- function(variable, value) {
#  c(
#    "", 
#    "",
#    "",
#    "",
#    "",
#    ""
#  )
#}
#
#facet_labeller_bottom <- function(variable, value) {
#  c(
#    "0.5", 
#    "0.9",
#    "0.5",
#    "0.9",
#    "0.5", 
#    "0.9"
#  )
#}
#  		
#  mean_lat_mo_plot <- 
#		ggplot(mean_lats_mo_hind_df) +
#	 	geom_line(aes(year, hist_mean_lat, alpha = 0.5),
#		  data = . %>% filter(sp_hab_threshold == 0.5), color = "black") +
#		geom_line(data = mean_lats_mo_proj_df_plot, 
#							aes(year, proj_mean_lat, color = sim_proj, group = sim_proj, alpha = 0.5)) +
#		facet_grid(month_name ~ simulation + sp_hab_threshold,
#							 labeller = labeller(
#							 	simulation=as_labeller(facet_labeller_top),
#                sp_hab_threshold = as_labeller(facet_labeller_bottom))) +
#		geom_line(aes(year, hist_mean_lat, alpha = 0.5),
#		  data = . %>% filter(sp_hab_threshold == 0.9), color = "black") +
#		geom_line(data = hist_data, 
#							aes(year, proj_mean_lat, color = "lightgrey", alpha = 0.5)) +
#		xlab("Year") +
#		scale_color_manual(name = "sim_proj", values = colors) +
#	  scale_y_continuous(
#	  	name = "Mean\nlatitude",
#	  	breaks = c(56, 58, 60, 62),
#	  	labels = c("56˚N", "58˚N", "60˚N", "62˚N")
#	  ) +
#   	xlim(1970, 2100) +
#		theme_bw() +
#  	theme(legend.position = "none") +
#  	theme(
#  		plot.title = element_text(size = 18, face = "bold"),
#			strip.background = element_blank(),
#  		strip.text = element_text(size = 18, face = "bold"),
#			axis.text = element_text(size = 16, colour = "grey50"),
#  	  axis.ticks = element_line(colour = "grey50"),
#  	  axis.line = element_line(colour = "grey50"),
#  	  axis.title = element_text(size=18, color = "grey30"),
#  	  panel.grid.major = element_blank(),
#  	  panel.grid.minor = element_blank(),
#  	  panel.border = element_rect(fill = NA, color = "grey50"))
#	
#  
# mean_lat_mo_plot2 <- 
# 	mean_lats_mo_proj_df %>% 
#  ggplot(aes(year, proj_mean_lat)) + 
#  geom_blank() + 
#  facet_grid(~ simulation, scales = "free_x") +
#  theme(panel.spacing.x = unit(0,"cm"),
#        axis.text = element_blank(),
#        axis.ticks = element_blank(),
#        axis.title = element_blank(),
#        plot.margin = margin(b = -2),
#				strip.background = element_blank(),
#  			strip.text = element_text(size = 18, face = "bold"),
#				panel.background = element_rect(colour = "white", fill = "white"), 
#        panel.grid.major = element_blank(),
#        panel.grid.minor = element_blank())
# 
# 
# 
# plot1 <- mean_lat_mo_plot2 + theme(plot.margin = unit(c(-10, -10, -10, -10), "in"))
# plot2 <- mean_lat_mo_plot + theme(plot.margin = unit(c(0, 0.2, 0.2, 0.2), "in"))
# 
#	mean_lat_mo_plot_form <- plot1/plot2 + plot_layout(heights = c(0.1,100) ) 
# 
# 
# 
#  		ggsave("./output/plots/mean_lat_mo_plot_form.png",
#			 mean_lat_mo_plot_form,
#			 width = 15, height = 7, units = "in")
#  		
#  		
#  		
#  		
#
#	#
  
  #### mapping ####
  
  ## hindcast 
  
 	coords_hind_func <- function(x){
		
		new_dat <- ROMS_hindcast_dat %>%
			filter(., sp_hab_suit >= x)
		
		new_dat_sum <- new_dat %>%
			group_by(year) %>%
			summarise(latitude = mean(latitude),
								longitude = mean(longitude)) %>%
			mutate(sp_hab_threshold = x)

		new_dat_sum		
	}
	
	sp_hab_thresholds <- c(0.5, 0.9)
	
	coords_hind <- lapply(sp_hab_thresholds, coords_hind_func)
	
	coords_hind <- bind_rows(coords_hind) 
	
	coords_hind_sf <- coords_hind %>%
		mutate(long_not_360 = case_when(
				longitude >= 180 ~ longitude - 360,
				longitude < 180 ~ longitude)) %>%
  	st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326, remove = FALSE)

	
	## projection
	
	coords_proj_func <- function(x){
		
		new_dat <- ROMS_projected_dat %>%
			filter(., sp_hab_suit_var >= x)
		
		new_dat_sum <- new_dat %>%
			group_by(projection, year) %>%
			summarise(proj_mean_lat = mean(latitude),
								proj_mean_long = mean(long_not_360))  %>%
			mutate(sp_hab_threshold = x)

		new_dat_sum		
	}
	
	sp_hab_thresholds <- c(0.5, 0.9)
	
	coords_proj <- lapply(sp_hab_thresholds, coords_proj_func)
	
	coords_proj <- bind_rows(coords_proj) 
	
	years_proj <- 2020:2099
	
	coords_proj <- coords_proj %>%
		filter(year %in% years_proj)
	
	coords_proj_sf <- coords_proj %>%
  	st_as_sf(coords = c("proj_mean_long", "proj_mean_lat"), crs = 4326, remove = FALSE)
		
	
	# map

	map <- 
		 	ggplot(coords_hind_sf) +
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
  		geom_point(aes(color = year, alpha = 0.5,
  			geometry = geometry),
    		stat = "sf_coordinates") +
		scale_color_viridis_c()
	
	ggsave("./output/plots/map_yrcentroids_05.png",
			 map_yrcentroids_05,
			 width = 10, height = 7, units = "in")

	#### keep playing
	
	
		map <- 
		 	ggplot() +
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
  		geom_point(data = coords_hind_sf, aes(color = year, alpha = 0.5,
  			geometry = geometry),
    		stat = "sf_coordinates") +
			geom_point(data = coords_proj_sf, aes(color = year, alpha = 0.5,
  			geometry = geometry),
    		stat = "sf_coordinates") +
		scale_color_viridis_c()
		
		
	coords_proj_sf$scen <- NA
	
	coords_proj_sf$scen[coords_proj_sf$projection == "ssp126"] <- "low emission\n(ssp126)"
	coords_proj_sf$scen[coords_proj_sf$projection == "ssp585"] <- "high emission\n(ssp585)"

	coords_proj_sf$scen_f = factor(coords_proj_sf$scen, 
																		levels=c('low emission\n(ssp126)', 'high emission\n(ssp585)'))
	


	coords_proj_sf$thresh <- NA
	coords_hind_sf$thresh <- NA

	coords_proj_sf$thresh[coords_proj_sf$sp_hab_threshold == 0.5] <- "potential"
	coords_proj_sf$thresh[coords_proj_sf$sp_hab_threshold == 0.9] <- "core"
	
	coords_hind_sf$thresh[coords_hind_sf$sp_hab_threshold == 0.5] <- "potential"
	coords_hind_sf$thresh[coords_hind_sf$sp_hab_threshold == 0.9] <- "core"
	
	
	coords_proj_sf$scen_f = factor(coords_proj_sf$scen, 
																		levels=c('low emission\n(ssp126)', 'high emission\n(ssp585)'))

	
		map_mean_lats <- 
		 	ggplot() +
			geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
			coord_sf(crs = 3338) +
 			scale_x_continuous(
 				breaks = c(-175, -170, -165, -160),
 				labels = c("-175˚", "-170˚", "-165˚", "-160˚"),
 				name = "Longitude",
 				limits = c(-1400000, 10000)) +
 			scale_y_continuous(
 				breaks = c(55, 60),
 				limits = c(470000, 1900000),
 				name = "Latitude") +
  		geom_point(data = coords_hind_sf, aes(color = year, 
  			geometry = geometry), alpha = 0.5, 
    		stat = "sf_coordinates") +
			geom_point(data = coords_proj_sf, aes(color = year, 
  			geometry = geometry), alpha = 0.5, 
    		stat = "sf_coordinates") +
		facet_grid(thresh ~ scen_f) +
		scale_color_viridis_c(
			breaks = c(1980, 2020, 2060, 2099)
		) +
	  theme_bw() +
 		theme(
			legend.title.align = 0.5,
			#legend.position = c(0.89, 0.72),
			#legend.background = element_blank(),
			#legend.title = element_text(size = 9),
			#legend.text = element_text(size = 8),
 			panel.border = element_rect(color = "#666666"),
 			strip.text.x = element_text(size = 12, face = "bold",  color = "black"),
 			strip.text.y = element_text(size = 12, face = "bold",  color = "black"),
 			strip.background = element_blank(),
 			axis.text = element_text(size = 10,  color = "#666666"),	
  		axis.title = element_text(size = 12,  color = "#666666"),
 			axis.ticks = element_line( color = "#666666"))

		ggsave("./output/plots/mean_lats_map.png",
			 map_mean_lats,
			 width = 10, height = 5, units = "in")
