	therm_response_curve <- 
		ggplot(eggs) +
		geom_point(aes(x = Temp_act, y = Phatch), color = "black", size = 2) +
		geom_function(fun = cauchy, aes(col = "Temp_act"), color = "black", size = 0.5) +
		xlab("Temperature (˚C)") +
		scale_y_continuous(
			name = "Proportion\nsuccessful hatch") +
		ggtitle("Thermal response of hatch success") +
	#	labs(tag = "(a)") +
		theme_bw() +
		theme(legend.position = "none",
					plot.title = element_text(hjust = 0.5),
				#	plot.tag.position = c(0.05, 0.87),
  				axis.text = element_text(size= 12, colour = "grey50"),
  				axis.title = element_text(size=14, color = "grey50"),
  				axis.line = element_line(color = "grey50"),
  				axis.ticks = element_line(colour = "grey50"),
					panel.grid = element_blank())

		march_avg_temps_hind <-
		ggplot() +
		geom_sf(data = march_temp_hind, aes(color = mean_temp))  +
		geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
		coord_sf(crs = 3338) +
		binned_scale(
			aesthetics = "color",
			scale_name = "stepsn",
			palette = function(x) c("#666666", "#66b266", "#666666"),
			breaks = c(3, 6),
			limits = c(-2.5, 12.5),
			show.limits = TRUE,
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
		#labs(tag = "(b)") +
		ggtitle("Historical:\n1970 - 1999") +
		theme_bw() +
		theme(legend.position = "none",
				#	plot.tag.position = c(0.2, 0.87),
					legend.background = element_blank(),
					legend.title = element_text(size = 9),
					legend.text = element_text(size = 8),
					plot.tag.position = c(0.06, 0.87),
					plot.title = element_text(hjust = 0.5),
					axis.text = element_text(size = 12, colour = "grey50"),
  	  		axis.ticks = element_line(colour = "grey50"),
  	  		axis.line = element_line(colour = "grey50"),
  	  		axis.title = element_text(size=14, color = "grey50"),
  	  		panel.border = element_rect(fill = NA, color = "grey50"))
			
	march_avg_temps_proj_ssp126 <-
		ggplot() +
		geom_sf(data = march_temp_proj_ssp126, aes(color = mean_temp))  +
		geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
		coord_sf(crs = 3338) +
		binned_scale(
			aesthetics = "color",
			scale_name = "stepsn",
			palette = function(x) c("#666666", "#66b266", "#666666"),
			breaks = c(3, 6),
			limits = c(-2.5, 12.5),
			show.limits = TRUE,
			nice.breaks = TRUE,
			guide = "colorsteps"
		) + 		scale_x_continuous(
 		 breaks = c(-170, -160),
		 labels = c("-170˚","-160˚"),
		 limits = c(-1400000, 10000),
 			name = "Longitude") +
 		scale_y_continuous(
 			breaks = breaks_y,
 			limits = limits_y,
 			name = "Latitude") +
    labs(colour = "Mean\nbottom\ntemperature\n(˚C)") +
		#labs(tag = "(c)") +
		ggtitle("Low emission (ssp126):\n2080 - 2099") +
		theme_bw() +
		white_map_theme() +
		theme(legend.position = "none",
				#	plot.tag.position = c(0.06, 0.87),
					plot.title = element_text(hjust = 0.5),
					axis.text.x = element_text(size = 12, colour = "grey50"),
  	  		axis.ticks.x = element_line(colour = "grey50"),
  	  		axis.line.x = element_line(colour = "grey50"),
  	  		axis.title.x = element_text(size=14, color = "grey50"),
					axis.text.y = element_blank(),
  	  		axis.ticks.y = element_blank(),
  	  		axis.line.y = element_blank(),
  	  		axis.title.y = element_blank(),
  	  		panel.border = element_rect(fill = NA, color = "grey50"))
	

	march_avg_temps_proj_ssp585 <-
		ggplot() +
		geom_sf(data = march_temp_proj_ssp585, aes(color = mean_temp))  +
		geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
		coord_sf(crs = 3338) +
		binned_scale(
			aesthetics = "color",
			scale_name = "stepsn",
			palette = function(x) c("#666666", "#66b266", "#666666"),
			breaks = c(3, 6),
			limits = c(-2.5, 12.5),
			show.limits = TRUE,
			nice.breaks = TRUE,
			guide = "colorsteps"
		) + 		
		scale_x_continuous(
 		 breaks = c(-170, -160),
		 labels = c("-170˚", "-160˚"),
		 limits = c(-1400000, 10000),
 			name = "Longitude") +
 		scale_y_continuous(
 			position = "right",
 			breaks = c(55, 60),
 			limits = limits_y,
 			name = "Latitude") +
    labs(colour = "Mean\nbottom\ntemperature\n(˚C)") +
		#labs(tag = "(d)") +
		ggtitle("High emission (ssp585):\n2080 - 2099") +
		theme_bw() +
		white_map_theme() +
		theme(legend.title.align = 0.5,
				#	legend.position = c(0.89, 0.72),
					plot.title = element_text(hjust = 0.5),
					axis.text.x = element_text(size = 12, colour = "grey50"),
  	  		axis.ticks.x = element_line(colour = "grey50"),
  	  		axis.line.x = element_line(colour = "grey50"),
					axis.text.y = element_blank(),
					axis.ticks.y = element_blank(),
					axis.title.y = element_blank(),
  	  		axis.title.x = element_text(size=14, color = "grey50"),
  	  		panel.border = element_rect(fill = NA, color = "grey50"))
	
			
 plot1 <- therm_response_curve + theme(plot.margin = unit(c(0.2, 0, 0.2, 0.2), "in"))

 plot2 <- march_avg_temps_hind + theme(plot.margin = unit(c(0.2, 0, 0.2, -0.05), "in"))
 
 plot3 <- march_avg_temps_proj_ssp126 + theme(plot.margin = unit(c(0.2, 0, 0.2, -0.05), "in"))
 
 plot4 <- march_avg_temps_proj_ssp585 + theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0), "in"))
 
 Figure1_other <- 
 	plot1 + plot_spacer() +
 	plot2 + 
 	plot3 + 
 	plot4 +
 	plot_layout(nrow = 1, widths = c(2, 0.5, 1, 1, 1),
 							heights = c(0.5, 0.5, 1, 1, 1))

 ggsave("./output/plots/Figure1_other.png",
			 Figure1_other,
			 width = 16, height = 8, units = "in")
	
