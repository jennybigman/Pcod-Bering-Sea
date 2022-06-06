# 07 - metrics by region

	#### create polygons of separate regions ####
	
	sf_use_s2(FALSE)
	
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
		filter(., DOMAIN %in% inner_domains) %>%
		mutate(latitude = lat)
	
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
	
	
	### plot
	
		 plot <- 
    	  	ggplot() +
	 	 			geom_sf(data = inner_poly, color = "salmon", fill = "salmon", alpha = 0.5) +
	 	 		 	geom_sf(data = middle_poly, color = "mediumpurple", fill = "mediumpurple", alpha = 0.5) +
	 	 			geom_sf(data = outer_poly, color = "dodgerblue", fill = "dodgerblue", alpha = 0.5) +
					geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
					coord_sf(crs = 3338) +
					scale_color_viridis_c(breaks = c(0, 0.50, 1.0), limits = c(0,1)) +
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
					theme_bw() +
 					theme(
 						axis.text = element_text(size = 16),	
  					axis.title = element_text(size = 18),
  					legend.title.align=0.5)
    	   
	 	ggsave("./output/plots/BS_regions_IMO.png",
			 plot,
			 width = 10, height = 10, units = "in")

	#### hindcast df ####
	
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
		coords = c("long_not_360", "latitude"), crs = 4326, remove = FALSE)

	
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
	inner_domain_hind_df <- ROMS_hindcast_dat %>% 
		filter(., long_not_360 %in% inner_domain_overlap$longs_not_360) %>%
		filter(., latitude %in% inner_domain_overlap$lats) %>%
		mutate(lats = latitude) 
	
	inner_domain_hind_sf <- inner_domain_hind_df %>%
		  st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326, remove = FALSE)
	
	
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
	middle_domain_hind_df <- ROMS_hindcast_dat %>% 
		filter(., long_not_360 %in% middle_domain_overlap$longs_not_360) %>%
		filter(., latitude %in% middle_domain_overlap$lats)
	
	middle_domain_hind_sf <- middle_domain_hind_df %>%
		  st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326, remove = FALSE)

	
	# outer #### NOT SURE THIS IS CORRECT -- SEEMS LIKE A SMALL AREA
	
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
	outer_domain_hind_df <- ROMS_hindcast_dat %>% 
		filter(., long_not_360 %in% outer_domain_overlap$longs_not_360) %>%
		filter(., latitude %in% outer_domain_overlap$lats)
	 
	outer_domain_hind_sf <- outer_domain_hind_df %>%
			mutate(lats = latitude) %>%
		  st_as_sf(coords = c("long_not_360", "lats"), crs = 4326)
	
	# combine into one df
	inner_domain_hind_sf$region <- "inner"
 	middle_domain_hind_sf$region <- "middle"
	outer_domain_hind_sf$region <- "outer"

	hind_reg_df <- bind_rows(inner_domain_hind_sf, middle_domain_hind_sf, outer_domain_hind_sf)
	
	
	#### projection df ####
	
	# create smaller dataframe for intersection function
	ROMS_projection_dat_sum <- ROMS_projected_dat %>%
		group_by(latitude, longitude) %>%
		summarise(mean_temp = mean(bc_temp_sd))
	
	# add column of long not on 360 scale
	ROMS_projection_dat_sum <- ROMS_projection_dat_sum %>%
			mutate(long_not_360 = case_when(
				longitude >= 180 ~ longitude - 360,
				longitude < 180 ~ longitude
	))
	
	ROMS_projection_dat_sum$lats <- ROMS_projection_dat_sum$latitude
	ROMS_projection_dat_sum$longs_not_360 <- ROMS_projection_dat_sum$long_not_360
	
	# convert to shapefile for intersection function
	ROMS_projection_dat_sum_sf <- st_as_sf(ROMS_projection_dat_sum,
		coords = c("long_not_360", "latitude"), crs = 4326, remove = FALSE)

	# find the datapoints for each region -- inner, middle, outer ####
	
	# inner ####
	
	inner_domain_overlap_proj <- st_intersection(ROMS_projection_dat_sum_sf, inner_poly) 

	inner_domain_overlap_albers_proj <- st_transform(inner_domain_overlap_proj, crs = 3338) # in albers for plotting
	
 	inner_plot_proj <-
 		ggplot() +
		geom_sf(data = inner_domain_overlap_albers_proj, aes(color = mean_temp))  +
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
	inner_domain_proj_df <- ROMS_projected_dat_sf %>% 
		filter(., long_not_360 %in% inner_domain_overlap_proj$longs_not_360) %>%
		filter(., latitude %in% inner_domain_overlap_proj$lats) %>%
		mutate(lats = latitude) 
	
	# middle ####
	
	middle_domain_overlap_proj <- st_intersection(ROMS_projection_dat_sum_sf, middle_poly) 

	middle_domain_overlap_albers_proj <- st_transform(middle_domain_overlap_proj, crs = 3338) # in albers for plotting
	
 	middle_plot <-
 		ggplot() +
		geom_sf(data = middle_domain_overlap_albers_proj, aes(color = mean_temp))  +
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
	middle_domain_proj_df <- ROMS_projected_dat_sf %>% 
		filter(., long_not_360 %in% middle_domain_overlap_albers_proj$longs_not_360) %>%
		filter(., latitude %in% middle_domain_overlap_albers_proj$lats)
	
	# outer #### NOT SURE THIS IS CORRECT -- SEEMS LIKE A SMALL AREA
	
	outer_domain_overlap_proj <- st_intersection(ROMS_projection_dat_sum_sf, outer_poly) 

	outer_domain_overlap_albers_proj <- st_transform(outer_domain_overlap_proj, crs = 3338) # in albers for plotting
	
 	outer_plot <- ### why are there no points in some areas? 
 		ggplot() +
		geom_sf(data = outer_domain_overlap_albers_proj, aes(color = mean_temp))  +
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
	outer_domain_proj_df <- ROMS_projected_dat_sf %>% 
		filter(., long_not_360 %in% outer_domain_overlap_proj$longs_not_360) %>%
		filter(., latitude %in% outer_domain_overlap_proj$lats)
	 
	# combine into one df
	inner_domain_proj_df$region <- "inner"
 	middle_domain_proj_df$region <- "middle"
	outer_domain_proj_df$region <- "outer"

	proj_reg_df <- bind_rows(inner_domain_proj_df, middle_domain_proj_df, outer_domain_proj_df)
	

	# 1. spawning habitat suitability ####
	
	# hindcast ####
	
	hind_yr_hab_dat <- hind_reg_df %>%
   								  group_by(region, year) %>%
   								  summarise(annual_hatch_success_cauchy = mean(hatch_success_cauchy),
   								 					  annual_hatch_success_gaussian = mean(hatch_success_gaus),
   								  					annual_spawning_hab_suit = mean(sp_hab_suit))
	
	hind_yr_hab_dat <- hind_yr_hab_dat %>% filter(year != 2021)
 
	hind_yrly_sphab_region_plot <-    
   	ggplot(data = hind_yr_hab_dat) +
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
   
   hind_yrly_sphab_region_plot <- hind_yrly_sphab_region_plot +
		annotate(geom = "text", x = 2030, y = 0.65,
           label = "outer shelf",
           color = "#000000", size = 6) +
		annotate(geom = "text", x = 2032, y = 0.38,
           label = "middle shelf",
           color = "blue", size = 6) +
		annotate(geom = "text", x = 2032, y = 0.15,
           label = "inner shelf",
           color = "red", size = 6)
 
	ggsave("./output/plots/hind_yrly_sphab_region_plott.png",
			 hind_yrly_sphab_region_plot,
			 width = 10, height = 7, units = "in")

	
	# projections ####

	proj_yr_hab_dat <- proj_reg_df %>%
   								  group_by(simulation, projection, region, year) %>%
   								  summarise(annual_spawning_hab_suit = mean(sp_hab_suit_var))
	
	proj_yr_hab_dat_hist <- proj_yr_hab_dat %>%
		filter(projection == "historical")
	
	colors <- c("red", "blue", "black")
	
	regions <- unique(proj_yr_hab_dat_hist$region)
	
	names(colors) <- regions
	
	proj_yrly_sphab_hist_plot <-    
   	ggplot(data = proj_yr_hab_dat_hist) +
	 	geom_line(aes(year, annual_spawning_hab_suit, group = region, colour = region))  +
		facet_grid(simulation  ~ region) +
   	xlab("Year") + 
		scale_color_manual(name = "region", values = colors) +
	  scale_y_continuous(
	  	name = "Spawning habitat suitability",
	  	breaks = c(0, 0.20, 0.40, 0.60, 0.8),
	  ) +
   	theme_bw() +
  	theme(legend.position = "none") +
  	theme(
  		strip.background = element_blank(),
  		strip.text = element_text(size = 16, face = "bold"),
  	  axis.text=element_text(size=16, colour = "grey50"),
  	  axis.ticks = element_line(colour = "grey50"),
  	  axis.line = element_line(colour = "grey50"),
  	  axis.text.x = element_text(size = 16),
  	  axis.title= element_text(size=16, color = "grey30"),
  	  panel.grid.major = element_blank(),
  	  panel.grid.minor = element_blank(),
  	  panel.border = element_rect(fill = NA, color = "grey50"))
   
	ggsave("./output/plots/proj_yrly_sphab_hist_plot.png",
			 proj_yrly_sphab_hist_plot,
			 width = 10, height = 7, units = "in")
	
	## plot hist and proj together ####
	
	proj_yr_hab_dat <- tidyr::unite(proj_yr_hab_dat,"sim_proj",
																			 simulation, projection, remove = F)

	proj_yr_hab_dat_plot <- proj_yr_hab_dat %>%
		filter(!str_detect(sim_proj, "_historical"))
	
	hist_data <- proj_yr_hab_dat %>%
		filter(str_detect(sim_proj, "_historical"))
	
	colors <- c("#efd966", "#b79a00", 
						  "#7fb27f", "#004700", 
						  "#6666b2", "#000059")

	sim_proj <- unique(proj_yr_hab_dat_plot$sim_proj)
	
	names(colors) <- unique(proj_yr_hab_dat_plot$sim_proj)
	
	proj_ts_sp_hab <- 
		ggplot(hind_yr_hab_dat) +
	 	geom_line(aes(year, annual_spawning_hab_suit, alpha = 0.5),
		  data = . %>% filter(region == "inner"), color = "black") +
		geom_line(data = proj_yr_hab_dat_plot, 
							aes(year, annual_spawning_hab_suit, color = sim_proj, group = sim_proj, alpha = 0.5)) +
		facet_grid(simulation ~ region) +
		geom_line(aes(year, annual_spawning_hab_suit, alpha = 0.5),
		  data = . %>% filter(region == "middle"), color = "black") +
		geom_line(aes(year, annual_spawning_hab_suit, alpha = 0.5),
		  data = . %>% filter(region == "outer"), color = "black") +
		geom_line(data = hist_data, 
							aes(year, annual_spawning_hab_suit, color = "pink", alpha = 0.5)) +
		xlab("Year") +
		scale_color_manual(name = "sim_proj", values = colors) +
	   scale_y_continuous(
	  	name = "Spawning habitat suitability",
	  	breaks = c(0, 0.20, 0.40, 0.60, 0.8),
	  ) +
		theme_bw() +
  	theme(legend.position = "none") +
  	theme(
			strip.background = element_blank(),
  		strip.text = element_text(size = 16, face = "bold"),
			axis.text = element_text(size = 10, colour = "grey50"),
  	  axis.ticks = element_line(colour = "grey50"),
  	  axis.line = element_line(colour = "grey50"),
  	  axis.title = element_text(size=16, color = "grey30"),
  	  panel.grid.major = element_blank(),
  	  panel.grid.minor = element_blank(),
  	  panel.border = element_rect(fill = NA, color = "grey50"))
	
  		ggsave("./output/plots/hind_proj_ts_sp_hab.png",
			 proj_ts_sp_hab,
			 width = 13, height = 7, units = "in")
  		
  		
  #### rolling mean and sd  ####
	
  #### hindcasts ####
	
  # summarize spawning habitat suitability by year 
	yr_stats_hind_reg <- hind_reg_df %>%
		group_by(region, year) %>%
		summarise(mean_sp_hab_suit = mean(sp_hab_suit))

	sds_hind <- NA
  
  for(i in 6:153){
  	win <- (i - 5):(i + 5)
  	sds_hind[i] <- sd(yr_stats_hind_reg$mean_sp_hab_suit[win])
  }
  
  means_hind <- NA
  
  for(i in 6:153){
  	win <- (i - 5):(i + 5)
  	means_hind[i] <- mean(yr_stats_hind_reg$mean_sp_hab_suit[win])
  }
  
  yr_stats_hind_reg$rollmean11 <- means_hind
	yr_stats_hind_reg$rollsd11 <- sds_hind
	yr_stats_hind_reg <- na.omit(yr_stats_hind_reg)
  
	#### projections ####
	
  # summarize spawning habitat suitability by year 
	yr_stats_proj_reg <- proj_reg_df %>%
		group_by(simulation, projection, region, year) %>%
		summarise(mean_sp_hab_suit = mean(sp_hab_suit_var))
  
  # historical ref period
  yr_stats_proj_reg_hist <- filter(yr_stats_proj_reg, projection == "historical")

	sds_proj_hist <- NA
  
  for(i in 6:315){
  	win <- (i - 5):(i + 5)
  	sds_proj_hist[i] <- sd(yr_stats_proj_reg_hist$mean_sp_hab_suit[win])
  }
  
  means_proj_hist <- NA
  
  for(i in 6:315){
  	win <- (i - 5):(i + 5)
  	means_proj_hist[i] <- mean(yr_stats_proj_reg_hist$mean_sp_hab_suit[win])
  }
  
  yr_stats_proj_reg_hist$rollsd11 <- sds_proj_hist
  yr_stats_proj_reg_hist$rollmean11 <- means_proj_hist
  
	yr_stats_proj_reg_hist <- na.omit(yr_stats_proj_reg_hist)
	
	# scenarios
	# historical ref period
  yr_stats_proj_reg_scens <- filter(yr_stats_proj_reg, projection != "historical")

	sds_proj_scens <- NA
  
  for(i in 6:1530){
  	win <- (i - 5):(i + 5)
  	sds_proj_scens[i] <- sd(yr_stats_proj_reg_scens$mean_sp_hab_suit[win])
  }
  
  means_proj_scens <- NA
  
  for(i in 6:1530){
  	win <- (i - 5):(i + 5)
  	means_proj_scens[i] <- mean(yr_stats_proj_reg_scens$mean_sp_hab_suit[win])
  }
  
  yr_stats_proj_reg_scens$rollsd11 <- sds_proj_scens
  yr_stats_proj_reg_scens$rollmean11 <- means_proj_scens
  
	yr_stats_proj_reg_scens <- na.omit(yr_stats_proj_reg_scens)
	
	
	# plot
	
	yr_stats_proj_reg_scens <- tidyr::unite(yr_stats_proj_reg_scens,"sim_proj",
																		simulation, projection, remove = F)

	colors <- c("#efd966", "#b79a00", 
						  "#7fb27f", "#004700", 
						  "#6666b2", "#000059")

	sim_proj <- unique(yr_stats_proj_reg_scens$sim_proj)
	
	names(colors) <- unique(yr_stats_proj_reg_scens$sim_proj)
	
	
	yr_stats_hind_reg <- yr_stats_hind_reg %>% filter(year %in% 1975:2015)
	yr_stats_proj_reg_hist <- yr_stats_proj_reg_hist %>% filter(year %in% 1985:2009)
	yr_stats_proj_reg_scens <- yr_stats_proj_reg_scens %>% filter(year %in% 2020:2094)
	
	rollmean_plot <-    
   	ggplot() +
	 	geom_line(data = yr_stats_hind_reg, aes(year, rollmean11, alpha = 0.5)) +
		geom_line(data = yr_stats_proj_reg_scens, 
							aes(year, rollmean11, color = sim_proj, group = sim_proj, alpha = 0.5)) +
		facet_grid(simulation ~ region) +
		geom_line(data = yr_stats_proj_reg_hist, 
							aes(year, rollmean11, color = "lightgrey", alpha = 0.5)) +
		xlab("Year") +
		scale_color_manual(name = "sim_proj", values = colors) +
	  scale_y_continuous(
	  	name = "11-year rolling mean of\nspawning habitat suitability",
	  	breaks = c(0.2, 0.4, 0.6, 0.8),
	  	labels = c(0.2, 0.4, 0.6, 0.8)
	  ) +
   	xlim(1970, 2100) +
    theme_bw() +
  	theme(legend.position = "none") +
  	theme(
			strip.background = element_blank(),
  		strip.text = element_text(size = 18, face = "bold"),
			axis.text = element_text(size = 16, colour = "grey50"),
  	  axis.ticks = element_line(colour = "grey50"),
  	  axis.line = element_line(colour = "grey50"),
  	  axis.title = element_text(size=18, color = "grey30"),
  	  panel.grid.major = element_blank(),
  	  panel.grid.minor = element_blank(),
  	  panel.border = element_rect(fill = NA, color = "grey50"))
	
	
		ggsave("./output/plots/rollmean_plot.png",
			 rollmean_plot,
			 width = 13, height = 7, units = "in")
	
	rollsd_plot <-    
   	ggplot() +
	 	geom_line(data = yr_stats_hind_reg, aes(year, rollsd11, alpha = 0.5)) +
		geom_line(data = yr_stats_proj_reg_scens, 
							aes(year, rollsd11, color = sim_proj, group = sim_proj, alpha = 0.5)) +
		facet_grid(simulation ~ region) +
		geom_line(data = yr_stats_proj_reg_hist, 
							aes(year, rollsd11, color = "lightgrey", alpha = 0.5)) +
		xlab("Year") +
		scale_color_manual(name = "sim_proj", values = colors) +
	  scale_y_continuous(
	  	name = "11-year rolling of\nstandard deviation\nspawning habitat suitability",
	  	breaks = c(0.2, 0.4, 0.6, 0.8),
	  	labels = c(0.2, 0.4, 0.6, 0.8)
	  ) +
   	xlim(1970, 2100) +
    theme_bw() +
  	theme(legend.position = "none") +
  	theme(
			strip.background = element_blank(),
  		strip.text = element_text(size = 18, face = "bold"),
			axis.text = element_text(size = 16, colour = "grey50"),
  	  axis.ticks = element_line(colour = "grey50"),
  	  axis.line = element_line(colour = "grey50"),
  	  axis.title = element_text(size=18, color = "grey30"),
  	  panel.grid.major = element_blank(),
  	  panel.grid.minor = element_blank(),
  	  panel.border = element_rect(fill = NA, color = "grey50"))
	
	
		ggsave("./output/plots/rollsd_plot.png",
			 rollsd_plot,
			 width = 13, height = 7, units = "in")
	
	