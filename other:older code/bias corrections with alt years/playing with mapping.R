
library(ggOceanMapsData)
library(ggOceanMaps)

catch0_dat_trim <- catch0_dat_sf %>%
	filter(., LARVALCATCHPER10M2 == 0)

map_test <- 
	basemap(limits = c(180, -155, 50, 70), rotate = TRUE, bathymetry =  TRUE) +
	geom_sf(data = catch0_dat_trim,
				 aes(color = LARVALCATCHPER10M2),
				 color = "black", shape = 4, alpha = 0.5) +
	geom_sf(data = length_dat_sf, 
					aes(color = CORRECTED_LENGTH, 
							size = LARVALCATCHPER10M2),
						  alpha = 0.5) + 
	facet_wrap(~ YEAR, nrow = 4) +
	scale_color_viridis_c() +
	theme_bw()

ggsave("./output/plots/map_test.png",
			 map_test,
			 width = 20, height = 10, units = "in")


map_test_nofacet <- 
	basemap(limits = c(180, -155, 50, 70), rotate = TRUE, bathymetry =  TRUE) +
	geom_sf(data = catch0_dat_trim,
				 aes(color = LARVALCATCHPER10M2),
				 color = "black", shape = 4, alpha = 0.5) +
	geom_sf(data = length_dat_sf, 
					aes(color = CORRECTED_LENGTH, 
							size = LARVALCATCHPER10M2),
						  alpha = 0.5) + 
	scale_color_viridis_c() +
	theme_bw()

ggsave("./output/plots/map_test.png",
			 map_test,
			 width = 20, height = 10, units = "in")

# playing with colors 

	basemap(limits = c(180, -155, 50, 70), 
					rotate = TRUE, bathymetry =  TRUE,
					bathy.style = "poly_greys") 

	basemap(limits = c(180, -155, 50, 70), 
					rotate = TRUE, bathymetry =  TRUE,
					bathy.style = "contour_grey") 
	
	map_test_contour <- 
	basemap(limits = c(180, -155, 50, 70), 
					rotate = TRUE, bathymetry =  TRUE, bathy.style = "contour_grey") +
	geom_sf(data = catch0_dat_trim,
				 aes(color = LARVALCATCHPER10M2),
				 color = "black", shape = 4, alpha = 0.5) +
	geom_sf(data = length_dat_sf, 
					aes(color = CORRECTED_LENGTH, 
							size = LARVALCATCHPER10M2),
						  alpha = 0.5) + 
	scale_color_viridis_c() +
	theme_bw()

ggsave("./output/plots/map_test_contour.png",
			 map_test_contour,
			 width = 20, height = 10, units = "in")
