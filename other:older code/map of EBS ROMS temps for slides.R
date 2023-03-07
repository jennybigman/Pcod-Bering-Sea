# map showing ROMS grid of temps used for Pcod study	

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
		geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
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
		ggtitle("Bottom temperature\naveraged Jan - April\nfrom the Bering10k ROMS") +
		theme_bw() +
		theme(
					legend.position = c(0.87, 0.72),
					legend.background = element_rect("transparent"),
					legend.title = element_blank(),
					plot.title = element_text(hjust = 0.5, size = 14),
					legend.text = element_text(size = 12),
					panel.background = element_rect(fill = "white", color = NA),
					plot.background = element_rect(fill = "white", color = NA),
					axis.text = element_text(size = 14, colour = "black"),
  	  		axis.ticks = element_line(colour = "black"),
  	  		axis.line = element_blank(),
  	  		axis.title = element_text(size= 16, color = "black"),
  	  		panel.border = element_rect(fill = NA, color = "black", linewidth = 1))
	
	ggsave(here("./output/plots/generic_map_EBS.png"),
		generic_map_EBS,
		width = 5, height = 5, units = "in")
 

		### without title
	
	
	generic_map_EBS <- 
		ggplot() +
		geom_sf(data = ROMS_hind_sum, aes(color = mean_temp))  +
		geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
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
		ggtitle("Bottom temperature\naveraged Jan - April\nfrom the Bering10k ROMS") +
		theme_bw() +
		theme(
					legend.position = c(0.87, 0.72),
					legend.background = element_rect("transparent"),
					legend.title = element_blank(),
					plot.title = element_text(hjust = 0.5, size = 14),
					legend.text = element_text(size = 12),
					panel.background = element_rect(fill = "white", color = NA),
					plot.background = element_rect(fill = "white", color = NA),
					axis.text = element_text(size = 14, colour = "black"),
  	  		axis.ticks = element_line(colour = "black"),
  	  		axis.line = element_blank(),
  	  		axis.title = element_text(size= 16, color = "black"),
  	  		panel.border = element_rect(fill = NA, color = "black", linewidth = 1))
	
	ggsave(here("./output/plots/generic_map_EBS.png"),
		generic_map_EBS,
		width = 5, height = 5, units = "in")
 