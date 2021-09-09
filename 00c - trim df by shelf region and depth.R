# work on removing points not within a certain polygon and > 250 m depth

	library(data.table)
	library(lubridate)
	library(ggplot2)
	library(dplyr)
	library(marmap)
	library(rgdal)
	library(mapdata)
	library(rgeos)
	library(sf)

	setwd("~/Google Drive/NOAA AFSC Postdoc/Pcod Bering Sea Habitat Suitability")
	sm_temp_df <- fread("./Pcod-Bering-Sea/data/SpawnMonths_Temp_HatchSuccess.csv")
	
	# add column of long not on 360 scale
	sm_temp_df <- sm_temp_df %>%
			mutate(long_not_360 = case_when(
				longitude >= 180 ~ longitude - 360,
				longitude < 180 ~ longitude
			))
	
	# covert to shapefile
	#sm_temp_df_sf <- st_as_sf(sm_temp_df, coords = c("long_not_360", "latitude"), crs = 4326)
	
	# create smaller dataframe for intersection function
	sm_temp_df_sum <- sm_temp_df %>%
		group_by(latitude, longitude) %>%
		summarise(mean_hs = mean(hatch_success_cauchy))

	# add column of long not on 360 scale
	sm_temp_df_sum <- sm_temp_df_sum %>%
			mutate(long_not_360 = case_when(
				longitude >= 180 ~ longitude - 360,
				longitude < 180 ~ longitude
	))
	
	# convert to shapefile for intersection function
	sm_temp_df_sum_sf <- st_as_sf(sm_temp_df_sum,
		coords = c("long_not_360", "latitude"), crs = 4326)

###############	 	
# remove points not on Bering Sea Shelf ####
###############
	
	# load Ortiz Bering Sea regions and convert to sf object
	bsregions <- readOGR("./Mapping Code - Bigman/Ortiz Regions", layer="BSIERP_regions_2012")
	bsregions <- spTransform(bsregions, CRS("+init=epsg:4326"))
	bsregions_sf <- st_as_sf(bsregions)
	bsregions_no_15 <-bsregions[bsregions@data$DOMAIN != 15, ] # remove domain 15 

	plot(bsregions_no_15)
	
	bsregions_no_15_sf <- bsregions_no_15 %>% 	  
		st_as_sf(coords = c("long", "lat"), crs = 4326) 
	poly_bsregions_no_15_sf <- st_union(bsregions_no_15_sf)
	
	plot(poly_bsregions_no_15_sf)

	# remove domain 15 to fix it
	bsregions15 <-bsregions[bsregions@data$DOMAIN == 15, ] 
	plot(bsregions15)

	# fix domain 15
	bsregions15@data$id = rownames(bsregions15@data)
	bsregions_points15 <- fortify(bsregions15, region="id")
	bsregions15_df <- left_join(bsregions_points15, bsregions15@data, by = "id") %>%
		filter(., long < 0)
	bsregions_sf_15 <- bsregions15_df %>% st_as_sf(coords = c("long", "lat"), crs = 4326)
	
	ggplot() +
		geom_polygon(data = bsregions15_df, aes(x = long, y = lat, group = group),
 								 fill = "darkgrey", color = NA) 

	polygon_bsregion15 <- 
  	bsregions15_df %>%
	  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
	  summarise(geometry = st_combine(geometry)) %>%
	  st_cast("POLYGON") 

	plot(polygon_bsregion15)

	full_poly <-  st_union(polygon_bsregion15, poly_bsregions_no_15_sf)
	
	plot(full_poly$geometry)

	int_pts_sum <- st_intersection(sm_temp_df_sum_sf, full_poly) 
	
	int_pts_sum_albers <- st_transform(int_pts_sum, crs = 3338)

	# plot 
	
	world_map_data <- ne_countries(scale = "medium", returnclass = "sf") 

 		combined_poly_plot <-
 		ggplot() +
		geom_sf(data = int_pts_sum_albers, aes(color = mean_hs))  +
		geom_sf(data = world_map_data, fill = "darkgrey", lwd = 0) +
		coord_sf(crs = 3338) +
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
		theme_bw() +
 		theme(
 			axis.text = element_text(size = 12),	
  		axis.title = element_text(size = 14),
  		legend.title.align=0.5)

	ggsave("./Pcod-Bering-Sea/output/plots/combined_poly_plot.png",
		combined_poly_plot,
		width = 10, height = 10, units = "in")
	
	## filter full dataframe by lats/longs in polygon
	
	# decompose geometry back into lat/long
	int_pts_sum_ll <- int_pts_sum %>%
			mutate(longitude_not_360 = sf::st_coordinates(.)[, 1],
					   latitude =  sf::st_coordinates(.)[, 2])
	
	sm_temp_df_poly <- sm_temp_df %>% 
		filter(., long_not_360 %in% int_pts_sum_ll$longitude_not_360) %>%
		filter(., latitude %in% int_pts_sum_ll$latitude)
	 
###############	 	
# restrict to depths < 250 m ####
###############
	
	max(sm_temp_df_poly$long_not_360) 
	min(sm_temp_df_poly$long_not_360) 
	max(sm_temp_df_poly$latitude)
	min(sm_temp_df_poly$latitude)
	
	BS_bathy <- getNOAA.bathy(lon1 = 179.95, lon2 = -158.01, lat1 = 53, 
														lat2 = 65.75, resolution = 1, antimeridian = TRUE,
														keep = TRUE)

	plot(BS_bathy, image = TRUE, land = TRUE, axes = FALSE, lwd = 0.1,
			 bpal = list(c(0, max(BS_bathy), grey(0.7), grey(0.9), grey(0.95)),
			 						c(min(BS_bathy), 0, "darkblue", "lightblue")))
	plot(BS_bathy, n = 1, lwd = 0.5, add = TRUE)
	
	longslats <- sm_temp_df_poly %>% 
		dplyr::select(longitude, latitude) %>%
		distinct(across(everything())) %>%
		filter(., between(longitude, 179, 202))
	
	depths <- get.depth(BS_bathy, longslats, locator = FALSE)
	
	depths <- depths %>%
		rename(latitude = lat,
					 longitude = lon)
	
	sm_temp_df_poly_depth <- merge(sm_temp_df_poly, depths, by = c("longitude", "latitude")) %>%
		filter(., between(depth, -250, 0))
	
	# add name of month for plotting
	sm_temp_df_poly_depth$month_name <- NA
     
  sm_temp_df_poly_depth$month_name[sm_temp_df_poly_depth$month == 1] <- "January"
  sm_temp_df_poly_depth$month_name[sm_temp_df_poly_depth$month == 2] <- "February"
	sm_temp_df_poly_depth$month_name[sm_temp_df_poly_depth$month == 3] <- "March"
	sm_temp_df_poly_depth$month_name[sm_temp_df_poly_depth$month == 4] <- "April"
	sm_temp_df_poly_depth$month_name[sm_temp_df_poly_depth$month == 5] <- "May"
	sm_temp_df_poly_depth$month_name[sm_temp_df_poly_depth$month == 6] <- "June"
	
	# reorder for plotting
	sm_temp_df_poly_depth$month_name <- factor(sm_temp_df_poly_depth$month_name)
  sm_temp_df_poly_depth$month_name <- fct_reorder(sm_temp_df_poly_depth$month_name, 
  																		sm_temp_df_poly_depth$month)
  
	fwrite(sm_temp_df_poly_depth, "./Pcod-Bering-Sea/data/sm_temp_df_poly_depth.csv")


	
	
	
	
	
	