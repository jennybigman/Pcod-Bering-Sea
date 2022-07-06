# experiment with geom_tile

ROMS_strata_temp_proj_plot <-
 		ggplot(proj_temp_dat_sum, aes(x = longitude, y = latitude)) +
		geom_tile(aes(fill = mean_temp, width = 0.25, height = 0.25))
	#	geom_sf(data = world_map_data, fill = "darkgrey", lwd = 0) +
	#	coord_sf(crs = 3338) +
		scale_color_viridis_c() +
 		scale_x_continuous(
 			breaks = c(-175, -170, -165, -160),
 			labels = c("-175˚", "-170˚", "-165˚", "-160˚"),
 			limits = c(-1400000, -150000)
 		) +
 		scale_y_continuous(
 			breaks = c(55, 60),
 			limits = c(470000, 1900000)
 		) +
		theme_bw() 
