# plot monthly hatch success by year

ggplot() +
		geom_sf(data = int_pts_sum, aes(color = mean_hs)) +
	  coord_sf(xlim = lons, ylim = lats) +
		geom_polygon(data = reg, aes(x = long, y = lat, group = group),
 								 fill = "darkgrey", color = NA) +
		scale_color_viridis_c() +
		theme_bw()

int_pts_sum_albers <- st_transform(int_pts_sum, crs = 3338)
	
ggplot() +
		geom_sf(data = int_pts_sum_albers, aes(color = mean_hs))

p <-
	ggplot() +
	#geom_sf(data = int_pts_sum_albers, aes(color = mean_hs)) +
	geom_polygon(data = reg, aes(x = long, y = lat, group = group),
 							 fill = "darkgrey", color = NA) +
	coord_sf(xlim = c(-180, -158), ylim = c(53, 67), crs = 3338) 

ggplot_build(p)$layout$panel_scales_x[[1]]$range$range
ggplot_build(p)$layout$panel_scales_y[[1]]$range$range

p <-
	ggplot() +
	geom_point(data = int_pts_sum_ll, aes(x = longitude_not_360, y = latitude, color = mean_hs)) +
	geom_sf(data = reg, aes(x = long, y = lat, group = group),
 							 fill = "darkgrey", color = NA) +
	coord_map(xlim = c(-180, -158), ylim = c(53, 67)) 

p <- 
ggplot() +
geom_sf(data = int_pts_sum_albers, aes(color = mean_hs))  +
geom_sf(data=BASE, fill="gray60") +
coord_sf(xlim = c(-1700000, 92641), ylim = c(415756.6, 2000000)) +
	theme_bw() +
	theme(legend.position = "none")

	ggsave("./Pcod-Bering-Sea/output/plots/ak_hs_sum.png",
		 p,
		 width = 10, height = 7, units = "in")

reg_sf <- st_as_sf(reg, coords = c("long", "lat"), crs = 3338)

ggplot() +
	geom_sf(data = reg_sf, color = "black", fill = "grey") +
	coord_sf(xlim = c(-180, -158), ylim = c(53, 67)) +
	theme_bw()


ggplot(data = reg_sf) +
	geom_sf(aes(color = "black", fill = "grey"), lwd = 1) +
	coord_sf(xlim = c(-180, -158), ylim = c(53, 67)) +
	theme_bw()

world_map_data <- ne_countries(scale = "medium", returnclass = "sf")


ggplot(data = world_map_data) +
	geom_sf() +
	coord_sf(xlim = c(-180, -158), ylim = c(53, 67)) +
	theme_bw()

P <- ggplot(data = world_map_data) +
	geom_sf() +
	coord_sf(xlim = c(-1700000, 92641), ylim = c(415756.6, 2000000), crs = 3338) +
	theme_bw()

########### WORKS #############
world_map_data <- ne_countries(scale = "medium", returnclass = "sf") 


ggplot() +
	geom_sf(data = world_map_data) +
	coord_sf(xlim = c(-1700000, 92641), ylim = c(415756.6, 2000000), crs = 3338) +
	theme_bw()

##########3
ggplot() +
	#geom_sf(data = int_pts_sum_albers, aes(color = mean_hs))  +
	geom_sf(data = world_map_data) +
	coord_sf(xlim = c(-1700000, 92641), ylim = c(415756.6, 2000000), crs = 3338) +
	theme_bw()

 	combined_poly_plot <-
 		ggplot() +
		geom_sf(data = int_pts_sum_albers, aes(color = mean_hs))  +
		geom_sf(data = world_map_data, fill = "darkgrey", lwd = 0) +
		coord_sf(crs = 3338) +
		scale_color_viridis_c() +
 		scale_x_continuous(
 			breaks = c(-175, -165, -155),
 			labels = c("-175˚", "-165˚", "-155˚"),
 			limits = c(-1700000, 92641)
 		) +
 		scale_y_continuous(
 			limits = c(410000, 2000000)
 		) +
		theme_bw() +
 		theme(
 			axis.text = element_text(size = 12),	
  		axis.title = element_text(size = 14),
  		legend.title.align=0.5)

 	
 		ggplot() +
		geom_sf(data = world_map_data, fill = "darkgrey") 
 		
 		AK_RUS <- world_map_data %>% st_crop(xmin = -155, xmax = -180,
 																				 ymin = 53,  ymax = 67)
 		
 		
 		ggplot() +
		geom_sf(data = AK_RUS, fill = "darkgrey") +
 					coord_sf(crs = 3338) 

 		
 		
 		combined_poly_plot <-
 		ggplot() +
		geom_sf(data = int_pts_sum_albers, aes(color = mean_hs))  +
		geom_sf(data = AK_RUS, fill = "darkgrey", lwd = 0) +
		coord_sf(crs = 3338) +
		scale_color_viridis_c() +
 		scale_x_continuous(
 			breaks = c(-175, -165, -155),
 			labels = c(-175, -165, -155),
 			limits = c(-1700000, 92641)
 		) +
 		scale_y_continuous(
 			limits = c(410000, 2000000)
 		) +
		theme_bw()
