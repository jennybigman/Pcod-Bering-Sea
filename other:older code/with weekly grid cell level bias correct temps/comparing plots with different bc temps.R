# save plots to file with axis labels

	#### with monthly, region level bias corrected values ####

	# temp ####
	
	temp_plot <- 
	 	ggplot(yearly_temp_proj, aes(year, avg_temp)) +
		geom_line(data = rolling_mean_temp_hind, 
   						aes(x = years_hind, y = means_hind), 
   						color = light_black) +
		geom_line(data = rolling_mean_temp_proj, 
   						aes(x = years_proj, y = means_proj), 
   						color = light_black) +
   	geom_line(data = yearly_temp_hind, 
   						aes(x = year, y = avg_temp), 
   						color = "black", alpha = 0.3) +
		geom_line(data = yearly_temp_proj,
							aes(year, avg_temp, 
									group = sim_proj, 
									color = sim_proj), alpha = 0.3) +
		facet_wrap(~ scen_f) +
		xlab("Year") + 
		geom_segment(x = 2020, y = -1, xend = 2020, yend = 4.2,
								 color = "lightgrey", size = 0.5) +
		scale_color_manual(name = "sim_proj", values = colors) +
	  scale_y_continuous(
	  	name = "Temperature (˚C)",
	  	breaks = c(0, 2, 4),
	  	labels = c(0, 2, 4),
	  	limits =  temp_range_y#c(-0.8, 4.5)
	  ) +
		scale_x_continuous(
	  	name = "Year",
	  	breaks = c(1980, 2030, 2080),
	  		  	limits = c(1970, 2115)) +
		theme_bw() +
		theme(legend.position = "none")

	model_ids_low <- tibble(
		year = c(2108, 2108, 2108, 1995), 
		avg_temp = c(2.1, 0.7, 0.1, 1), 
		lab = c("CESM", "GFDL", "MIROC", "hindcast"),
		scen_f = factor("low emission (SSP126)"),
		cols = c("#6dc3a9", "#4e8d9c", "#97c3e5", "black"))
	
	model_ids_high <- tibble(
		year = c(2108, 2108, 2108, 1994), 
		avg_temp = c(3.7, 2.2, 3.1, 1), 
		lab = c("CESM", "GFDL", "MIROC", "hindcast"),
		scen_f = factor("high emission (SSP585)"),
		cols = c("#ffabab", "#ff4040", "#ffb733", "black"))

	temp_plot_form <- 
		temp_plot +
		geom_text(data = model_ids_low,
							label = model_ids_low$lab,
							color = model_ids_low$cols, 
							size = 4,
							alpha = 0.5) +
		geom_text(data = model_ids_high,
							label = model_ids_high$lab,
							color = model_ids_high$cols, 
							size = 4,
							alpha = 0.5) +
		theme(plot.title = element_text(size = 22, face = "bold", color = "black")) +
		coord_cartesian(ylim = c(-0.8, 4.5), expand = T)

		ggsave(file = "./output/plots/temp_plot_form.png", 
				 temp_plot_form, height = 5, width = 7)

	#ggplot_build(temp_plot_form)$layout$panel_scales_y[[1]]$range$range

	
	# habitat suit ####
	
	habsuit_plot <- 
	 	ggplot(data = yearly_hab_dat_proj, aes(x = year, y = mean_hab_suit)) +
		geom_line(data = rolling_mean_habsuit_hind, 
   						aes(x = years_hind, y = means_hind), 
   						color = light_black) +
		geom_line(data = rolling_mean_habsuit_proj, 
   						aes(x = years_proj, y = means_proj), 
   						color = light_black) +
   	geom_line(data = yearly_hab_dat_hind, 
   						aes(x = year, y = mean_hab_suit), 
   						color = "black", alpha = 0.3) +
		geom_line(data = yearly_hab_dat_proj,
							aes(year, mean_hab_suit, 
									group = sim_proj, 
									color = sim_proj), alpha = 0.3) +
		facet_wrap(~ scen_f) +
		xlab("Year") + 
		geom_segment(x = 2020, y = 0.1, xend = 2020, yend = 0.6,
				color = "lightgrey", size = 0.5) +		
		scale_color_manual(name = "sim_proj", values = colors) +
	  scale_y_continuous(
	  	name = "Spawning habitat\nsuitability index",
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
		coord_cartesian(ylim = c(0.21, 0.64))

		#ggplot_build(habsuit_plot)$layout$panel_scales_y[[1]]$range$range

		ggsave(file = "./output/plots/habsuit_plot.png", 
				 habsuit_plot, height = 5, width = 7)

	# area ####
	
	area_plot <-    
   	ggplot(data = proj_area_yr, aes(year, area)) +
		geom_line(data = rolling_area_hind, 
   						aes(x = year, y = area), 
   						color = light_black) +
		geom_line(data = rolling_area_proj, 
   						aes(x = year, y = area), 
   						color = light_black) +
	 	geom_line(aes(year, area), alpha = 0.3,
            data = hind_area_yr %>% filter(sp_hab_threshold == "potential"), color = "black") +
		geom_line(data = proj_area_yr, 
							aes(year, area, color = sim_proj, group = sim_proj), alpha = 0.5) +
		facet_grid(sp_hab_threshold ~ scen_f) +
	  geom_line(aes(year, area, colour = sp_hab_threshold), alpha = 0.3,
            data = hind_area_yr %>% filter(sp_hab_threshold == "core"), color = "black") +
		xlab("Year") +
		scale_color_manual(name = "sim_proj", values = colors) +
	  scale_y_continuous(
	  	name =	"Area (x 10^5 km^2)",
	  	breaks = c(100000, 300000, 500000),
	  	labels = c(1,3,5),
	  	limits = c(19000, 567000)) +
   	xlim(1970, 2110) +
		geom_segment(x = 2020, y = 19000, xend = 2020, yend = 480000,
								 color = "lightgrey", size = 0.5) +
		theme_bw() +
		theme(legend.position = "none") +
		coord_cartesian(ylim = c(16600, 575000))
	
	#ggplot_build(area_plot)$layout$panel_scales_y[[1]]$range$range
	
	ggsave(file = "./output/plots/area_plot.png", 
				 area_plot, height = 5, width = 7)

	# mean lat ####
	
	mean_latitude_plot <-    
   	ggplot(proj_mean_lat_yr, 
							aes(year, proj_mean_lat)) +
		geom_line(data = rolling_mean_lat_hind, 
   						aes(x = year, y = mean_lat), 
   						color = light_black) +
		geom_line(data = rolling_mean_lat_proj, 
   						aes(x = year, y = mean_lat), 
   						color = light_black) +
	 	geom_line(aes(year, hist_mean_lat),
            data = hind_mean_lat_yr %>% filter(sp_hab_threshold == 0.5), color = "black", alpha = 0.3) +
		geom_line(data = proj_mean_lat_yr, 
							aes(year, proj_mean_lat, color = sim_proj, group = sim_proj), alpha = 0.3) +
		facet_grid(factor(thresh) ~ factor(scen_f)) +
	  geom_line(aes(year, hist_mean_lat, colour = sp_hab_threshold),
            data = hind_mean_lat_yr %>% filter(sp_hab_threshold == 0.9), color = "black",  alpha = 0.3) +
		xlab("Year") +
		scale_color_manual(name = "sim_proj", values = colors) +
	  scale_y_continuous(
	  	name = "Mean latitude (˚N)",
	  	breaks = c(56, 57, 58, 59),
	  	labels = c(56, 57, 58, 59),
	  	limits = c(55.4, 59.7)
	  ) +
   	xlim(1970, 2110) +
		geom_segment(x = 2020, y = 55, xend = 2020, yend = 59.3,
								 color = "lightgrey", size = 0.5) +
		theme_bw() +
		theme(legend.position = "none") +
		coord_cartesian(ylim = c(55.5, 59.7))
	
	ggsave(file = "./output/plots/mean_latitude_plot.png", 
				 mean_latitude_plot, height = 5, width = 7)

	
	#### with weekly, grid cell level bias corrected values ####

	# temp ####

	temp_plot_wkgc <- 
	 	ggplot() +
		geom_line(data = rolling_mean_temp_hind, 
   						aes(x = years_hind, y = means_hind), 
   						color = light_black) +
		geom_line(data = rolling_mean_temp_proj, 
   						aes(x = years_proj, y = means_proj), 
   						color = light_black) +
   	geom_line(data = yearly_temp_hind, 
   						aes(x = year, y = avg_temp), 
   						color = "black", alpha = 0.3) +
		geom_line(data = yearly_temp_proj,
							aes(year, avg_temp, 
									group = sim_proj, 
									color = sim_proj), alpha = 0.3) +
		facet_wrap(~ scen_f) +
		xlab("Year") + 
		geom_segment(x = 2020, y = -1, xend = 2020, yend = 4.2,
								 color = "lightgrey", size = 0.5) +
		scale_color_manual(name = "sim_proj", values = colors) +
	  scale_y_continuous(
	  	name = "Temperature (˚C)",
	  	breaks = c(0, 2, 4),
	  	labels = c(0, 2, 4),
	  	limits = c(-0.8, 4.5)
	  ) +
		scale_x_continuous(
	  	name = "Year",
	  	breaks = c(1980, 2030, 2080),
	  		  	limits = c(1970, 2115)) +
		theme_bw() +
		theme(legend.position = "none")
	
	model_ids_low <- tibble(
		year = c(2108, 2108, 2108, 1995), 
		avg_temp = c(2.1, 0.7, 0.1, 1), 
		lab = c("CESM", "GFDL", "MIROC", "hindcast"),
		scen_f = factor("low emission (SSP126)"),
		cols = c("#6dc3a9", "#4e8d9c", "#97c3e5", "black"))
	
	model_ids_high <- tibble(
		year = c(2108, 2108, 2108, 1994), 
		avg_temp = c(3.7, 2.2, 3.1, 1), 
		lab = c("CESM", "GFDL", "MIROC", "hindcast"),
		scen_f = factor("high emission (SSP585)"),
		cols = c("#ffabab", "#ff4040", "#ffb733", "black"))

	temp_plot_wkgc_form <- 
		temp_plot_wkgc +
		geom_text(data = model_ids_low,
							aes(x = year, y = avg_temp),
							label = model_ids_low$lab,
							color = model_ids_low$cols, 
							size = 4,
							alpha = 0.5) +
		geom_text(data = model_ids_high,
							aes(x = year, y = avg_temp),
							label = model_ids_high$lab,
							color = model_ids_high$cols, 
							size = 4,
							alpha = 0.5) +
		coord_cartesian(ylim = c(-0.8, 4.5), expand = T)


	#ggplot_build(temp_plot_wkgc)$layout$panel_scales_y[[1]]$range$range

	ggsave(file = "./scripts/with weekly grid cell level bias correct temps/temp_plot_wkgc_form.png", 
				 temp_plot_wkgc_form, height = 5, width = 7)
	
	# habitat suit ####
	
	habsuit_plot_wkgc <- 
	 	ggplot() +
		geom_line(data = rolling_mean_habsuit_hind, 
   						aes(x = years_hind, y = means_hind), 
   						color = light_black) +
		geom_line(data = rolling_mean_habsuit_proj, 
   						aes(x = years_proj, y = means_proj), 
   						color = light_black) +
   	geom_line(data = yearly_hab_dat_hind, 
   						aes(x = year, y = mean_hab_suit), 
   						color = "black", alpha = 0.3) +
		geom_line(data = yearly_hab_dat_proj,
							aes(year, mean_hab_suit, 
									group = sim_proj, 
									color = sim_proj), alpha = 0.3) +
		facet_wrap(~ scen_f) +
		xlab("Year") + 
		geom_segment(x = 2020, y = 0.1, xend = 2020, yend = 0.6,
				color = "lightgrey", size = 0.5) +		
		scale_color_manual(name = "sim_proj", values = colors) +
	  scale_y_continuous(
	  	name = "Spawning habitat\nsuitability index",
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
		coord_cartesian(ylim = c(0.21, 0.64))	
	
	#ggplot_build(habsuit_plot_wkgc)$layout$panel_scales_y[[1]]$range$range

	ggsave(file = "./scripts/with weekly grid cell level bias correct temps/habsuit_plot_wkgc.png", 
				 habsuit_plot_wkgc, height = 5, width = 7)

	# area ####
	
	area_plot_wkgc <-    
   	ggplot(data = proj_area_yr, aes(year, area)) +
		geom_line(data = rolling_area_hind, 
   						aes(x = year, y = area), 
   						color = light_black) +
		geom_line(data = rolling_area_proj, 
   						aes(x = year, y = area), 
   						color = light_black) +
	 	geom_line(aes(year, area), alpha = 0.3,
            data = hind_area_yr %>% filter(sp_hab_threshold == "potential"), color = "black") +
		geom_line(data = proj_area_yr, 
							aes(year, area, color = sim_proj, group = sim_proj), alpha = 0.5) +
		facet_grid(sp_hab_threshold ~ scen_f) +
	  geom_line(aes(year, area, colour = sp_hab_threshold), alpha = 0.3,
            data = hind_area_yr %>% filter(sp_hab_threshold == "core"), color = "black") +
		xlab("Year") +
		scale_color_manual(name = "sim_proj", values = colors) +
	  scale_y_continuous(
	  	name =	"Area (x 10^5 km^2)",
	  	breaks = c(100000, 300000, 500000),
	  	labels = c(1,3,5),
	  	limits = c(19000, 567000)) +
   	xlim(1970, 2110) +
		geom_segment(x = 2020, y = 19000, xend = 2020, yend = 480000,
								 color = "lightgrey", size = 0.5) +
		theme_bw() +
		theme(legend.position = "none") +
		coord_cartesian(ylim = c(16600, 575000))
	
		#ggplot_build(area_plot_wkgc)$layout$panel_scales_y[[1]]$range$range

		ggsave(file = "./scripts/with weekly grid cell level bias correct temps/area_plot_wkgc.png", 
				 area_plot_wkgc, height = 5, width = 7)

	# mean latitude ####
	
	mean_latitude_plot_wkgc <-    
   	ggplot(proj_mean_lat_yr, 
							aes(year, proj_mean_lat)) +
		geom_line(data = rolling_mean_lat_hind, 
   						aes(x = year, y = mean_lat), 
   						color = light_black) +
		geom_line(data = rolling_mean_lat_proj, 
   						aes(x = year, y = mean_lat), 
   						color = light_black) +
	 	geom_line(aes(year, hist_mean_lat),
            data = hind_mean_lat_yr %>% filter(sp_hab_threshold == 0.5), color = "black", alpha = 0.3) +
		geom_line(data = proj_mean_lat_yr, 
							aes(year, proj_mean_lat, color = sim_proj, group = sim_proj), alpha = 0.3) +
		facet_grid(factor(thresh) ~ factor(scen_f)) +
	  geom_line(aes(year, hist_mean_lat, colour = sp_hab_threshold),
            data = hind_mean_lat_yr %>% filter(sp_hab_threshold == 0.9), color = "black",  alpha = 0.3) +
		xlab("Year") +
		scale_color_manual(name = "sim_proj", values = colors) +
	  scale_y_continuous(
	  	name = "Mean latitude (˚N)",
	  	breaks = c(56, 57, 58, 59),
	  	labels = c(56, 57, 58, 59),
	  	limits = c(55.4, 59.7)
	  ) +
   	xlim(1970, 2110) +
		geom_segment(x = 2020, y = 55, xend = 2020, yend = 59.3,
								 color = "lightgrey", size = 0.5) +
		theme_bw() +
		theme(legend.position = "none") +
		coord_cartesian(ylim = c(55.5, 59.7))

		ggsave(file = "./scripts/with weekly grid cell level bias correct temps/mean_latitude_plot_wkgc.png", 
				 mean_latitude_plot_wkgc, width = 7, height = 5)

		#ggplot_build(mean_latitude_plot_wkgc)$layout$panel_scales_y[[1]]$range$range
