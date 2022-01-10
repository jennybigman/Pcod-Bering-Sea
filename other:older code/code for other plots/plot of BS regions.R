# plot of Bering Sea Regions

	library(here)
	library(rnaturalearth)
	library(dplyr)
	library(tidyverse)
	library(ggplot2)
	library(data.table)
	library(sf)
	library(forcats)
	library(lubridate)
	library(patchwork)
	library(raster)
	library(rgdal)
	library(stars)
	library(marmap)
	library(zoo)
	library(RColorBrewer)
	library(beepr)
	library(mapdata)
	library(mapproj)


	# get regional polygons
	reg = map_data("world2Hires")
	reg = subset(reg, region %in% c('USSR', 'USA'))
	
	# convert lat longs
	reg$long = (360 - reg$long)*-1
	
	# set map limits
	lons = c(-179.9, -158)
	lats = c(53, 67)

	#Bring in the Ortiz region files, these are polygons that divide the Bering Sea 
	#into various physiographic regions
	bsregions <- readOGR("./other:older code/Mapping Code - Bigman/Ortiz Regions", layer="BSIERP_regions_2012")
	bsregions <- spTransform(bsregions, CRS("+init=epsg:4326"))
	bsregions@data$id = rownames(bsregions@data)
	bsregions.points <- fortify(bsregions, region="id")
	bsregions.df <- left_join(bsregions.points, bsregions@data, by = "id")
	
	# inital map
	ggplot() +
  	geom_polygon(data = reg, aes(x = long, y = lat, group = group), 
  	             fill = "darkgrey", color = NA) + 
  	geom_polygon(data = bsregions.df, aes(long, lat, group=group), 
  							 linetype = "solid", colour = "black", alpha = 0)+
  	coord_map(xlim = lons, ylim = lats)+
  	theme_bw()+
  	xlab("Longitude")+
  	ylab("Latitude")
	
	# check to make sure it is domain 15 that is problematic
	bsregions.df_rm <- bsregions.df %>% filter(., DOMAIN != 15)

	ggplot() +
  	geom_polygon(data = reg, aes(x = long, y = lat, group = group), 
  	             fill = "darkgrey", color = NA) + 
  	geom_polygon(data = bsregions.df_rm, aes(long, lat, group=group), 
  							 linetype = "solid", colour = "black", alpha = 0)+
  	coord_map(xlim = lons, ylim = lats)+
  	theme_bw()+
  	xlab("Longitude")+
  	ylab("Latitude")
	
	# fix long of problematic domain # 15 
	bsregions.df_15 <- bsregions.df %>% filter(., DOMAIN == 15)
	bsregions.df_15_2 <- bsregions.df_15 %>% filter(., long < 0)

	# add domain # 15 back into BS regions and check to make sure all polygons are there
	bsregions.df_add <- bind_rows(bsregions.df_rm, bsregions.df_15_2)
	 
	BS_regions <-
		ggplot() +
  	geom_polygon(data = reg, aes(x = long, y = lat, group = group), 
  	             fill = "darkgrey", color = NA) + 
  	geom_polygon(data = bsregions.df_add, aes(long, lat, group=group), 
  							 linetype = "solid", colour = "black", alpha = 0)+
  	coord_map(xlim = lons, ylim = lats)+
  	theme_bw()+
  	xlab("Longitude")+
  	ylab("Latitude")
	 
	 	ggsave("./Pcod-Bering-Sea/output/plots/BS_regions.png",
			 BS_regions,
			 width = 10, height = 10, units = "in")

	# turn bsregions.df_add back into spatial polygon dataframe and export
	 bsregions.df_add_sf <- bsregions.df_add %>%
	 		mutate(longitude = long,
  					 latitude = lat,
  					 long_not_360 = case_when(
						 longitude >= 180 ~ longitude - 360,
						 longitude < 180 ~ longitude)) %>%
  	st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326, remove = FALSE) 
	
	 	
	 	bsregions_all <- as(bsregions.df_add_sf, "Spatial") 
	 
	 	writeOGR(bsregions_all, 
	 					 dsn = "bsregions_all.shp", 
	 					 layer = "bsregions_all.shp", 
	 					 driver = "ESRI Shapefile")
	 	