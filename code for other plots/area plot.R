# for presentations for area icon

plot <- 
  		ggplot() +
					geom_sf(data = sm_temp_hind_df_yr_sum_sf, aes(fill = mean_sphabsuit))  +
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
					theme_bw() +
 					theme(
 						legend.position = "none",
 						strip.text = element_text(size = 12, face = "bold"),
 						strip.background = element_blank(),
 						axis.text = element_text(size = 10),	
  					axis.title = element_text(size = 12))
 						

	ggsave("./output/plots/area_icon.png",
		plot,
		width = 5, height = 5, units = "in")
	
 
	### with different colors
	
	plot <- 
  		ggplot() +
					geom_sf(data = sm_temp_hind_df_yr_sum_sf, aes(color = mean_sphabsuit))  +
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
					scale_color_gradientn(colors = c("#B3E5FC", "#B3E5FC", 
																					 "#01579B", "#01579B",
																					 "#00345C", "#00345C"),
																values = c(0, 0.099, 0.1, 0.899, 0.9, 1),
																breaks = c(0.1, 0.5, 0.9),
																labels = format(c(0.1, 0.5, 0.9)),
																limits = c(0, 1)) +
					theme_bw() +
 					theme(
 						legend.position = "none",
 						strip.text = element_text(size = 12, face = "bold"),
 						strip.background = element_blank(),
 						axis.text = element_text(size = 10),	
  					axis.title = element_text(size = 12))
 						

	ggsave("./output/plots/area_icon_color_09.png",
		plot,
		width = 5, height = 5, units = "in")

	## three colors
	
		plot <- 
  		ggplot() +
					geom_sf(data = sm_temp_hind_df_yr_sum_sf, aes(color = mean_sphabsuit))  +
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
					scale_color_gradientn(colors = c("#B3E5FC", "#B3E5FC", 
																					 "#01579B", "#01579B",
																					 "#00345C", "#00345C"),
																values = c(0, 0.499, 0.5, 0.899, 0.9, 1),
																breaks = c(0.1, 0.5, 0.9),
																labels = format(c(0.1, 0.5, 0.9)),
																limits = c(0, 1)) +
					theme_bw() +
 					theme(
 						legend.position = "none",
 						strip.text = element_text(size = 12, face = "bold"),
 						strip.background = element_blank(),
 						axis.text = element_text(size = 10),	
  					axis.title = element_text(size = 12))
 						

	ggsave("./output/plots/area_icon_color_0509.png",
		plot,
		width = 5, height = 5, units = "in")
	
	
 ## try adding horizontal lines
	
			plot <- 
  		ggplot() +
					geom_sf(data = sm_temp_hind_df_yr_sum_sf, aes(color = mean_sphabsuit))  +
					geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
					geom_line()
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
					scale_color_gradientn(colors = c("#B3E5FC", "#B3E5FC", 
																					 "#01579B", "#01579B",
																					 "#00345C", "#00345C"),
																values = c(0, 0.499, 0.5, 0.899, 0.9, 1),
																breaks = c(0.1, 0.5, 0.9),
																labels = format(c(0.1, 0.5, 0.9)),
																limits = c(0, 1)) +
					theme_bw() +
 					theme(
 						legend.position = "none",
 						strip.text = element_text(size = 12, face = "bold"),
 						strip.background = element_blank(),
 						axis.text = element_text(size = 10),	
  					axis.title = element_text(size = 12))
 						

	ggsave("./output/plots/area_icon_color_0509.png",
		plot,
		width = 5, height = 5, units = "in")
	
	