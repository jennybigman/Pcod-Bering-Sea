
#dat_1970 <- sm_temp_df_poly_depth_sf %>% filter(year == 1970)
	
	ggplot() +
	geom_sf(data = dat_1970, aes(color = sp_hab_suit))  +
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
	scale_color_stepsn(n.breaks = 3,
										 colours = c("red", "yellow", "green"))
    								 #values = c(0, 0.1, 0.9, 1))
										 #limits = c(0,1))
										

										 
										 
												values = c(0, 0.1, 0.9, 1),
												#breaks = c(0.1, 0.5, 0.9), 
												#labels = format(c(0.1, 0.5, 0.9)), 
												limits = c(0,1))  +
  labs(colour = "Spawning\nhabitat\nsuitability") +
	theme_bw() +
 	theme(
 		strip.text = element_text(size = 14, face = "bold"),
 		strip.background = element_blank(),
 		axis.text = element_text(size = 12),	
  	axis.title = element_text(size = 14),
  	legend.title.align=0.5)



	ggplot() +
	geom_sf(data = dat_1970, aes(color = hatch_success_cauchy))  +
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
	scale_color_gradientn(colors = c("#B3E5FC",
																	 "#B3E5FC",
																	 "#01579B", 
																	 "#01579B", 
																	 "navy",
																	 "navy"), 
												values = c(0, 0.099, 0.1, 0.899, 0.9, 1.0),
												breaks = c(0.1, 0.5, 0.9), 
												labels = format(c(0.1, 0.5, 0.9)), 
												limits = c(0,1))  +
  labs(colour = "Spawning\nhabitat\nsuitability") +
	theme_bw() +
 	theme(
 		strip.text = element_text(size = 14, face = "bold"),
 		strip.background = element_blank(),
 		axis.text = element_text(size = 12),	
  	axis.title = element_text(size = 14),
  	legend.title.align=0.5)
	
	
	
  




	ggsave("./output/plots/testplot1.png",
	p1,
	width = 10, height = 10, units = "in")
	
					
p2 <- 
	ggplot() +
	geom_sf(data = dat_1970_sum, aes(color = mean_sphab))  +
	geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
	coord_sf(crs = 3338) +
	scale_color_gradientn(colors = c( "#B3E5FC", 
																	 "#01579B", "#01579B",
																	 "navy", "navy"),
												values = c(0.499, 0.5, 0.899, 0.9, 1),
												breaks = c(0.1, 0.5, 0.9),
												labels = format(c(0.1, 0.5, 0.9)),
												limits = c(0, 1)
												)  +
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
 		axis.text = element_text(size = 12),	
  	axis.title = element_text(size = 14),
  	legend.title.align=0.5)
  
	ggsave("./output/plots/testplot2.png",
	p2,
	width = 10, height = 10, units = "in")
	
	
	### try with a column of colors
	
	dat_1970 <- dat_1970 %>%
		mutate(colors = case_when(
			between(sp_hab_suit, 0, 0.5) ~ ("#B3E5FC"),
			between(sp_hab_suit, 0.5, 0.9) ~ ("#01579B"),
			sp_hab_suit >= 0.9 ~ ("navy")))
	
	dat_1970$colors <- paste0(" ' ", dat_1970$colors, " ' ")
	
	dat_1970 <- dat_1970 %>%
		mutate(latitude =  sf::st_coordinates(.)[, 2])
	
	dat_1970_sum <- dat_1970 %>%
		group_by(longitude, latitude) %>%
		summarise(mean_sphab = mean(sp_hab_suit))
	
		dat_1970_sum <- dat_1970_sum %>%
		mutate(colors = case_when(
			between(mean_sphab, 0, 0.5) ~ ("#B3E5FC"),
			between(mean_sphab, 0.5, 0.9) ~ ("#01579B"),
			mean_sphab >= 0.9 ~ ("navy")))
	
	
p3 <- 	ggplot() +
	geom_sf(data = dat_1970_sum, aes(fill = as.factor(mean_sphab)))  +
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
	scale_fill_manual(values = (dat_1970_sum$colors))

	ggsave("./output/plots/testplot3.png",
	p3,
	width = 10, height = 10, units = "in")
	
	
	
	
	(values = dat_1970$colors)
	
	scale_color_gradientn(colors = c("#B3E5FC", "#B3E5FC", 
																	 "#01579B", "#01579B",
																	 "navy", "navy"),
												values = c(0, 0.499, 0.5, 0.899, 0.9, 1),
												breaks = c(0.1, 0.5, 0.9),
												labels = format(c(0.1, 0.5, 0.9))) +
												#limits = c(0, 1))  +
 +
  labs(colour = "Spawning\nhabitat\nsuitability") +
	theme_bw() +
 	theme(
 		strip.text = element_text(size = 14, face = "bold"),
 		strip.background = element_blank(),
 		axis.text = element_text(size = 12),	
  	axis.title = element_text(size = 14),
  	legend.title.align=0.5)
  
	ggsave("./output/plots/testplot1.png",
	p1,
	width = 10, height = 10, units = "in")
	
					
p2 <- 
	ggplot() +
	geom_sf(data = dat_1970, aes(color = sp_hab_suit))  +
	geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
	coord_sf(crs = 3338) +
	scale_color_gradientn(colors = c("#B3E5FC", "#B3E5FC", 
																	 "#01579B", "#01579B",
																	 "navy", "navy"),
												values = c(0, 0.499, 0.5, 0.899, 0.9, 1),
												breaks = c(0.1, 0.5, 0.9),
												labels = format(c(0.1, 0.5, 0.9)),
												limits = c(0, 1))  +
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
 		axis.text = element_text(size = 12),	
  	axis.title = element_text(size = 14),
  	legend.title.align=0.5)
  