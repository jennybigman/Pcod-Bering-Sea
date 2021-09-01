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
	
	sm_temp_df <- sm_temp_df %>%
			mutate(long_not_360 = case_when(
				longitude >= 180 ~ longitude - 360,
				longitude < 180 ~ longitude
			))
	
	# covert to shapefile
	sm_temp_df_sf <- st_as_sf(sm_temp_df, coords = c("long_not_360", "latitude"), crs = 4326)
	
	# create smaller dataframe for plotting practice
	sm_temp_df_sum <- sm_temp_df %>%
		group_by(latitude, longitude) %>%
		summarise(mean_hs = mean(hatch_success_cauchy))

	sm_temp_df_sum <- sm_temp_df_sum %>%
			mutate(long_not_360 = case_when(
				longitude >= 180 ~ longitude - 360,
				longitude < 180 ~ longitude
			))

	sm_temp_df_sum_sf <- st_as_sf(sm_temp_df_sum,
		coords = c("long_not_360", "latitude"), crs = 4326)

###############	 	
# remove points not on Bering Sea Shelf ####
###############
	
	bsregions_sf <- st_as_sf(bsregions)

	polygon_bs <- bsregions_sf %>%
		st_union()
	
	plot(polygon_bs)

	#polygon_bs_v <- st_make_valid(polygon_bs)

	#plot(polygon_bs_v)

	int_pts <- st_intersection(sm_temp_df_sf, polygon_bs)

	int_pts_sum <- st_intersection(sm_temp_df_sum_sf, polygon_bs)

#### plot ####

	# get regional polygons
	reg = map_data("world2Hires")
	reg <- reg %>% filter(region == "USSR" | subregion == "Alaska")
	
	# convert lat longs
	reg$long = (360 - reg$long)*-1

	# set map limits
	lons = c(-179.9, -158)
	lats = c(53, 67)

	ggplot() +
 		geom_sf(data = int_pts_sum, aes(color = mean_hs)) +
	  coord_sf(xlim = lons, ylim = lats) +
		geom_polygon(data = reg, aes(x = long, y = lat, group = group),
 								 fill = "darkgrey", color = NA)


###############	 	
# restrict to depts < 250 m ####
###############

	max(sm_temp_df$longitude) - 360
	min(sm_temp_df$longitude) 
	max(sm_temp_df$latitude)
	min(sm_temp_df$latitude)
 
	BS_bathy <- getNOAA.bathy(lon1 = 179, lon2 = -158, lat1 = 53, 
														lat2 = 69, resolution = 1, antimeridian = TRUE,
														keep = TRUE)
	#summary(BS_bathy)
	#
	#plot(BS_bathy, image = TRUE, land = TRUE, axes = FALSE, lwd = 0.1,
	#		 bpal = list(c(0, max(BS_bathy), grey(0.7), grey(0.9), grey(0.95)),
	#		 						c(min(BS_bathy), 0, "darkblue", "lightblue")))
	#plot(BS_bathy, n = 1, lwd = 0.5, add = TRUE)
	
	longslats <- sm_temp_df %>% 
		dplyr::select(longitude, latitude) %>%
		distinct(across(everything())) %>%
		filter(., between(longitude, 179, 202))
	
	depths <- get.depth(BS_bathy, longslats, locator = FALSE)
	
	depths <- depths %>%
		rename(latitude = lat,
					 longitude = lon)
	
	sm_temp_df_depth <- merge(sm_temp_df, depths, by = c("longitude", "latitude")) %>%
		filter(., between(depth, -250, 0))
	
