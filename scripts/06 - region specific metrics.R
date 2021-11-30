# 06 - metrics by region

	sf_use_s2(FALSE)


	#### inner shelf ####
	
	# load Ortiz Bering Sea regions and convert to sf object
	bsregions <- readOGR("./other:older code/Mapping Code - Bigman/Ortiz Regions", layer="BSIERP_regions_2012")
	bsregions <- spTransform(bsregions, CRS("+init=epsg:4326"))
	bsregions@data$id = rownames(bsregions@data)
	bsregions_points <- fortify(bsregions, region="id")
	bsregions_df <- left_join(bsregions_points, bsregions@data, by = "id")

	# remove domain 15 because needs to be fixed
	bsregions_df_no15 <- bsregions_df %>%
		filter(DOMAIN != 15)
	
	# fix domain 15
	bsregions15 <-bsregions[bsregions@data$DOMAIN == 15, ] 
	bsregions15@data$id = rownames(bsregions15@data)
	bsregions_points15 <- fortify(bsregions15, region="id")
	bsregions15_df <- left_join(bsregions_points15, bsregions15@data, by = "id") %>%
		filter(., long < 0)
	
	#add back in
	bsregions_df <- bind_rows(bsregions15_df, bsregions_df_no15)

	# get bathymetry data 
	bathy = getNOAA.bathy(lon1 = -178, lon2 = -158, lat1 = 56, lat2 = 67, 
                  resolution = 1)

	# convert bathymetry to data frame
	bathy_df = fortify.bathy(bathy)

	# plot

	# create a pseudo-centroid for labeling
	centroid <- aggregate(cbind(long,lat) ~ DOMAIN, data = bsregions_df, FUN = mean)

	plot_domains <- 
		ggplot() +
		geom_polygon(data = bsregions_df, 
								 aes(x = long, y = lat, group = group),
								 fill = "white", color = "black") +
		geom_text(data = centroid, aes(x = long, y = lat, label = DOMAIN)) +
  	geom_contour(data = bathy_df, # 50m contour
              	 aes(x = x, y = y, z = z),
              	 breaks = c(-50),
              	 size = c(0.3),
              	 colour = "grey") +
  	geom_contour(data = bathy_df, # 100m contour 
	             aes(x = x, y=y, z=z),
	             breaks=c(-100),
	             size=c(0.3),
	             colour="red") +
		geom_contour(data = bathy_df, 
		             aes(x=x, y=y, z=z),
		             breaks=c(-200),
		             size=c(0.3),
		             colour="black")
	
	# define which domains are part of inner, middle, and outer shelf 
	
	inner_domains <- c(2, 7, 11, 13, 14)
	
	middle_domains <- c(1, 3, 4, 5, 6, 9, 10, 12)
	
	outer_domains <- c(8, 15, 16)
	
	inner_domain <- bsregions_df %>%
		filter(., DOMAIN %in% inner_domains)
	
	middle_domain <- bsregions_df %>%
		filter(., DOMAIN %in% middle_domains)
	
	outer_domain <- bsregions_df %>%
		filter(., DOMAIN %in% outer_domains)
	
	# turn each df into sf object and combine polygons
	
	inner_domain_sf <- inner_domain %>%
		  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  		group_by(DOMAIN) %>%
  		summarise(geometry = st_combine(geometry)) %>%
  		st_cast("POLYGON")
	
	inner_poly <-  st_union(inner_domain_sf)

	plot(inner_poly)
	
	
	middle_domain_sf <- middle_domain %>%
		  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  		group_by(DOMAIN) %>%
  		summarise(geometry = st_combine(geometry)) %>%
  		st_cast("POLYGON")
	
	middle_poly <-  st_union(middle_domain_sf)

	plot(middle_poly)
	

	outer_domain_sf <- outer_domain %>%
		  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  		group_by(DOMAIN) %>%
  		summarise(geometry = st_combine(geometry)) %>%
  		st_cast("POLYGON")
	
	outer_poly <-  st_union(outer_domain_sf)

	plot(outer_poly)
	
	
	# create smaller dataframe for intersection function
	ROMS_hindcast_dat_sum <- ROMS_hindcast_dat %>%
		group_by(latitude, longitude) %>%
		summarise(mean_temp = mean(temp))
	
	# add column of long not on 360 scale
	ROMS_hindcast_dat_sum <- ROMS_hindcast_dat_sum %>%
			mutate(long_not_360 = case_when(
				longitude >= 180 ~ longitude - 360,
				longitude < 180 ~ longitude
	))
	
	ROMS_hindcast_dat_sum$lats <- ROMS_hindcast_dat_sum$latitude
	ROMS_hindcast_dat_sum$longs_not_360 <- ROMS_hindcast_dat_sum$long_not_360
	
	# convert to shapefile for intersection function
	ROMS_hindcast_dat_sum_sf <- st_as_sf(ROMS_hindcast_dat_sum,
		coords = c("long_not_360", "latitude"), crs = 4326)

	
	# find the datapoints for each region -- inner, middle, outer ####
	
	# inner ####
	
	inner_domain_overlap <- st_intersection(ROMS_hindcast_dat_sum_sf, inner_poly) 

	inner_domain_overlap_albers <- st_transform(inner_domain_overlap, crs = 3338) # in albers for plotting
	
 	inner_plot <-
 		ggplot() +
		geom_sf(data = inner_domain_overlap_albers, aes(color = mean_temp))  +
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

	
 	## filter full df by lats/longs in polygon
	inner_domain_df <- ROMS_hindcast_dat %>% 
		filter(., long_not_360 %in% inner_domain_overlap$longs_not_360) %>%
		filter(., latitude %in% inner_domain_overlap$lats) 
	
	inner_domain_sf <- inner_domain_df %>%
		  st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326)
	
	
	# middle ####
	
	middle_domain_overlap <- st_intersection(ROMS_hindcast_dat_sum_sf, middle_poly) 

	middle_domain_overlap_albers <- st_transform(middle_domain_overlap, crs = 3338) # in albers for plotting
	
 	middle_plot <-
 		ggplot() +
		geom_sf(data = middle_domain_overlap_albers, aes(color = mean_temp))  +
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

	
 	## filter full df by lats/longs in polygon
	middle_domain_df <- ROMS_hindcast_dat %>% 
		filter(., long_not_360 %in% middle_domain_overlap$longs_not_360) %>%
		filter(., latitude %in% middle_domain_overlap$lats)
	
	middle_domain_sf <- middle_domain_df %>%
		  st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326)

	
	# outer ####
	
	outer_domain_overlap <- st_intersection(ROMS_hindcast_dat_sum_sf, outer_poly) 

	outer_domain_overlap_albers <- st_transform(outer_domain_overlap, crs = 3338) # in albers for plotting
	
 	outer_plot <- ### why are there no points in some areas? 
 		ggplot() +
		geom_sf(data = outer_domain_overlap_albers, aes(color = mean_temp))  +
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

	
 	## filter full df by lats/longs in polygon
	outer_domain_df <- ROMS_hindcast_dat %>% 
		filter(., long_not_360 %in% outer_domain_overlap$longs_not_360) %>%
		filter(., latitude %in% outer_domain_overlap$lats)
	 
	outer_domain_sf <- outer_domain_df %>%
		  st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326)
	
	
	# combine into one df
	
	inner_domain_sf$region <- "inner"
 	middle_domain_sf$region <- "middle"
	outer_domain_sf$region <- "outer"

	reg_df <- bind_rows(inner_domain_sf, middle_domain_sf, outer_domain_sf)
	
	# spawning habitat suitability ####
	
	# plotting avg spawning habitat suitabiltiy per year for each region separately
	
	#### index of suitable spawning habitat ####
	
	yr_hab_dat <- reg_df %>%
   								  group_by(region, year) %>%
   								  summarise(annual_hatch_success_cauchy = mean(hatch_success_cauchy),
   								 					  annual_hatch_success_gaussian = mean(hatch_success_gaus),
   								  					annual_spawning_hab_suit = mean(sp_hab_suit))
	
	yr_hab_dat <- yr_hab_dat %>% filter(year != 2021)
 
	yrly_sphab_region_plot <-    
   	ggplot(data = yr_hab_dat) +
	 	geom_line(aes(year, annual_spawning_hab_suit, group = region, colour = region),
            data = . %>% filter(region == "inner"), color = "red") +
	  geom_line(aes(year, annual_spawning_hab_suit, group = region, colour = region),
            data = . %>% filter(region == "middle"), color = "blue") +
	 	geom_line(aes(year, annual_spawning_hab_suit, group = region, colour = region),
            data = . %>% filter(region == "outer"), color = "black") +
   	xlab("Year") + 
	  scale_y_continuous(
	  	name = "Spawning habitat suitability",
	  	breaks = c(0, 0.20, 0.40, 0.60, 0.8),
	  ) +
   	xlim(1970, 2040) +
   	theme_bw() +
  	theme(legend.position = "none") +
  	theme(
  	  axis.text=element_text(size=20, colour = "grey50"),
  	  axis.ticks = element_line(colour = "grey50"),
  	  axis.line = element_line(colour = "grey50"),
  	  axis.text.x = element_text(size = 18),
  	  axis.title= element_text(size=20, color = "grey30"),
  	  panel.grid.major = element_blank(),
  	  panel.grid.minor = element_blank(),
  	  panel.border = element_rect(fill = NA, color = "grey50"))
   
   yrly_sphab_region_plot <- yrly_sphab_region_plot +
		annotate(geom = "text", x = 2030, y = 0.65,
           label = "outer shelf",
           color = "#000000", size = 6) +
		annotate(geom = "text", x = 2032, y = 0.38,
           label = "middle shelf",
           color = "blue", size = 6) +
		annotate(geom = "text", x = 2032, y = 0.15,
           label = "inner shelf",
           color = "red", size = 6)
 
	ggsave("./output/plots/yrly_sphab_region_plot.png",
			 yrly_sphab_region_plot,
			 width = 10, height = 7, units = "in")

	# standardized by area ####
	

