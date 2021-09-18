# plotting with smoothing

	ggplot() +
		geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
		#coord_sf(crs = 3338) +
 		scale_x_continuous(
 			breaks = c(-175, -170, -165, -160),
 			labels = c("-175˚", "-170˚", "-165˚", "-160˚"),
 			name = "Longitude",
 			limits = c(-179, -153)
 		) +
 		scale_y_continuous(
 			breaks = c(55, 60),
 			limits = c(53, 67),
 			name = "Latitude",
 		) +
		geom_tile(data = dat1970, aes(x = longitude360, y = latitude, color = sp_hab_suit)) 
	
	
	+
					scale_color_viridis_c(breaks = c(0, 0.50, 1.0), limits = c(0,1)) +

    	  	labs(colour = "Spawning\nhabitat\nsuitability") +
					theme_bw() +
 					theme(
 						strip.text = element_text(size = 14, face = "bold"),
 						strip.background = element_blank(),
 						axis.text = element_text(size = 12),	
  					axis.title = element_text(size = 14),
  					legend.title.align=0.5)