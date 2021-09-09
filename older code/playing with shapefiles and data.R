# Dave's other shapefiles

setwd("~/Google Drive/NOAA AFSC Postdoc/Pcod Bering Sea Habitat Suitability")

DK_AK <- st_read("./Mapping Code - Bigman/Shape_files_Alaska_dcw/Alaska_dcw_polygon_Project.shp")

raster::crs(DK_AK)

xmin <- 180
xmax <- -155
ymin <- 54
ymax <- 67

DK_AK_albers <- st_transform(DK_AK, 3338)

pplot <- 
	ggplot() +
  geom_sf(data=DK_AK_albers, fill="gray60") +
	geom_sf(data = sm_albers, aes(color = mean_hs)) +
	coord_sf(xlim=c(-2200000, 100000)) 

DK_AK_albers_plot <- 
ggplot() +
  geom_sf(data=DK_AK_albers, fill="gray60") +
	geom_sf(data = sm_albers, aes(color = mean_hs)) +
		scale_color_viridis_c() +
	scale_x_continuous(
  	breaks = c(180, 190, 200),
  	#labels = c("180˚", "170˚W", "160˚W"),
  	limits = c(-1500000, 100000)
  	) +
	scale_y_continuous(
		breaks = c(50, 55, 60, 65)) +
	labs(colour = "Hatch success\nprobability") +
  theme_bw() +
  theme(legend.title = element_text(size = 10),
  			legend.position = "right",
  			strip.background = element_rect(colour="white", fill="white"),
      	panel.border = element_rect(colour = "black"),
  			strip.text = element_text(size = 12, face = "bold"),
  			panel.spacing.x=unit(0, "lines"),
  			legend.title.align=0.5)

ggsave("./Pcod-Bering-Sea/output/plots/DK_AK_albers_plot.png",
			 DK_AK_albers_plot,
			 width = 10, height = 7, units = "in", dpi = 600)

	

	
#ggplot_build(pplot)$layout$panel_scales_y[[1]]$range$range
#ggplot_build(pplot)$layout$panel_scales_x[[1]]$range$range
