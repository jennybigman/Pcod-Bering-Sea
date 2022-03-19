# figure 1

	#### plot of domain ####
	
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

	# seaparate domains in order for merging later
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

	middle_domain_sf <- middle_domain %>%
		  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  		group_by(DOMAIN) %>%
  		summarise(geometry = st_combine(geometry)) %>%
  		st_cast("POLYGON")
	
	middle_poly <-  st_union(middle_domain_sf)

	outer_domain_sf <- outer_domain %>%
		  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  		group_by(DOMAIN) %>%
  		summarise(geometry = st_combine(geometry)) %>%
  		st_cast("POLYGON")
	
	outer_poly <-  st_union(outer_domain_sf)

	# combine all regions into one polygon
	full_poly1 <-  st_union(inner_poly, middle_poly)
	full_poly <- st_union(full_poly1, outer_poly)

	#### plot ####
	
	study_domain_plot  <- 
    	ggplot() +
	 	 	geom_sf(data = full_poly, color = "#bfbfbf", fill = "#bfbfbf", alpha = 0.5) +
			geom_sf(data = world_map_data, fill = "#808080", lwd = 0) +
			coord_sf(crs = 3338) +
 			scale_x_continuous(
 				breaks = breaks_x,
 				labels = labels_x,
 				name = "Longitude",
 				limits = limits_x
 			) +
 			scale_y_continuous(
 				breaks = breaks_y,
 				limits = limits_y,
 				name = "Latitude",
 			) +
			labs(tag = "(a)") +
			theme_bw() +
 			theme(
 			axis.text = element_text(size = 12, colour = "grey50"),
  	  axis.ticks = element_line(colour = "grey50"),
  	  axis.line = element_line(colour = "grey50"),
  	  axis.title = element_text(size=14, color = "grey50"),
 			plot.tag.position = c(0.21, 0.9))

	
	#### temperature plot ####
	
	# temp data
	
	yearly_temp_hind <- ROMS_hindcast_dat %>%
		group_by(year) %>%
    summarise(avg_temp = mean(temp))
		
	years_proj <- 2020:2099
	
	yearly_temp_proj <- ROMS_projected_dat %>% 
		filter(year %in% years_proj) %>%
		group_by(simulation, projection, year) %>%
   	summarise(avg_temp = mean(bc_temp_sd)) 
	
	yearly_temp_proj$scen <- NA
		
	yearly_temp_proj$scen[yearly_temp_proj$projection == "ssp126"] <- "low emission (ssp126)"
	yearly_temp_proj$scen[yearly_temp_proj$projection == "ssp585"] <- "high emission (ssp585)"
	
	yearly_temp_proj <- tidyr::unite(yearly_temp_proj,"sim_proj",
															 simulation, projection, remove = F)

	colors <- c("#6dc3a9", "#ff7373", # cesm low, cesm high
						  "#4e8d9c", "#ff4040", # gfdl low, gfdl high
						  "#97c3e5", "#ffb733") # miroc low, miroc high

	sim_proj <- unique(yearly_temp_proj$sim_proj)
	
	names(colors) <- unique(yearly_temp_proj$sim_proj)
	
	# order facets
	yearly_temp_proj$scen_f = factor(yearly_temp_proj$scen, levels=c('low emission (ssp126)', 
																																	 'high emission (ssp585)'))

	
	# rolling means of temp
	
	# hind
	means_hind <- NA
  
  for(i in 6:51){
  	win <- (i - 5):(i + 5)
  	means_hind[i] <- mean(yearly_temp_hind$avg_temp[win])
  }
  
	years_hind <- c(1970:2020) # does this need to be 1980?
	
	rolling_mean_hind <- as.data.frame(cbind(years_hind, means_hind)) 
	
	# proj
	yearly_temp_proj_sum <- yearly_temp_proj %>%
		group_by(year, scen_f) %>%
		summarise(mean_temp = mean(avg_temp))
	
	yearly_temp_proj_sum_low <- yearly_temp_proj_sum %>%
		filter(scen_f == "low emission (ssp126)")
	
	means_proj_low <- NA
  
  for(i in 6:80){
  	win <- (i - 5):(i + 5)
  	means_proj_low[i] <- mean(yearly_temp_proj_sum_low$mean_temp[win])
  }
	
	yearly_temp_proj_sum_high <- yearly_temp_proj_sum %>%
		filter(scen_f == "high emission (ssp585)")
	
	means_proj_high <- NA
  
  for(i in 6:80){
  	win <- (i - 5):(i + 5)
  	means_proj_high[i] <- mean(yearly_temp_proj_sum_high$mean_temp[win])
  }
	
	years_proj <- c(2020:2099) # does this need to be 1980?
	
	rolling_mean_proj <- as.data.frame(cbind(years_proj, means_proj_low, means_proj_high)) 

	#### plots ####
	
	### low emissions ####
	
	low_temp_proj <- yearly_temp_proj %>%
		filter(., scen == "low emission (ssp126)")
	
	low_temp_time <-    
   	ggplot() +
		geom_line(data = rolling_mean_hind, 
   						aes(x = years_hind, y = means_hind), 
   						color = "#e5e5e5") +
		geom_line(data = rolling_mean_proj, 
   						aes(x = years_proj, y = means_proj_low), 
   						color = "#e5e5e5") +
   	geom_line(data = yearly_temp_hind, 
   						aes(x = year, y = avg_temp), 
   						color = "black", alpha = 0.5) +
		geom_line(data = low_temp_proj,
							aes(year, avg_temp, 
									group = sim_proj, 
									color = sim_proj), alpha = 0.5) +
		xlab("Year") + 
		scale_color_manual(name = "sim_proj", values = colors) +
	  scale_y_continuous(
	  	name = "Temperature (˚C)",
	  	breaks = c(-1, 0, 1 ,2, 3, 4),
	  	limits = c(-1, 4)
	  ) +
		labs(tag = "(b)") +
		geom_vline(aes(xintercept = 2020, color = "#f2f2f2"), alpha = 0.5) +
		scale_x_continuous(
	  	name = "Year",
	  	breaks = c(1980, 2030, 2080),
	  		  	limits = c(1970, 2110)) +
		ggtitle("low emission (ssp126)") +
   	theme_bw() +
  	theme(legend.position = "none") +
  	theme(
  		plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
			axis.text.x = element_text(size = 12, colour = "grey50"),
  	  axis.ticks.x = element_line(colour = "grey50"),
  	  axis.line = element_line(colour = "grey50"),
			axis.title.y = element_blank(),
			axis.text.y = element_blank(),
			axis.ticks.y = element_blank(),
  	  axis.title = element_text(size=14, color = "grey50"),
  	  panel.grid.major = element_blank(),
  	  panel.grid.minor = element_blank(),
  	  panel.border = element_rect(fill = NA, color = "grey50"),
			plot.tag.position = c(0.04, 0.9))
	
		low_temp_time_lab <- low_temp_time +
			annotate(geom = "text", x = 2107, y = 1.9,
           label = "cesm",
           color = "#6dc3a9", size = 4.5) +
				annotate(geom = "text", x = 2107, y = 0.9,
           label = "gfdl",
           color = "#4e8d9c", size = 4.5) +
				annotate(geom = "text", x = 2107, y = 0.25,
           label = "miroc",
           color = "#97c3e5", size = 4.5)  +
				annotate(geom = "text", x = 1995, y = 1,
           label = "hindcast",
           color = "black", alpha = 0.5, size = 4.5)
		
				
	high_temp_dat <- yearly_temp_proj %>%
		filter(., scen == "high emission (ssp585)")
	
	high_temp_time <-    
   	ggplot() +
		geom_line(data = rolling_mean_hind, 
   						aes(x = years_hind, y = means_hind), 
   						color = "#e5e5e5") +
		geom_line(data = rolling_mean_proj, 
   						aes(x = years_proj, y = means_proj_high), 
   						color = "#e5e5e5") +
   	geom_line(data = yearly_temp_hind, 
   						aes(x = year, y = avg_temp), 
   						color = "black", alpha = 0.5) +
		geom_line(data = high_temp_dat,
							aes(year, avg_temp, 
									group = sim_proj, 
									color = sim_proj), alpha = 0.5) +
		xlab("Year") +
		geom_vline(aes(xintercept = 2020, color = "#f2f2f2"), alpha = 0.5) +
		scale_color_manual(name = "sim_proj", values = colors) +
	  scale_y_continuous(
	  	position = "right",
	  	name = "Temperature (˚C)",
	  	breaks = c(-1, 0, 1, 2, 3, 4),
	  	limits = c(-1, 4)
	  ) +
		scale_x_continuous(
	  	name = "Year",
	  	breaks = c(1980, 2030, 2080),
	  		  	limits = c(1970, 2110)) +
		ggtitle("high emission (ssp585)") +
		labs(tag = "(c)") +
   	theme_bw() +
  	theme(legend.position = "none") +
  	theme(
  		plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
			axis.text = element_text(size = 12, colour = "grey50"),
  	  axis.ticks = element_line(colour = "grey50"),
  	  axis.line = element_line(colour = "grey50"),
  	  axis.title = element_text(size=14, color = "grey50"),
  	  panel.grid.major = element_blank(),
  	  panel.grid.minor = element_blank(),
  	  panel.border = element_rect(fill = NA, color = "grey50"),
			plot.tag.position = c(0.03, 0.9))
	
		
		high_temp_time_lab <- high_temp_time +
			annotate(geom = "text", x = 2107, y = 3.2,
           label = "cesm",
           color = "#ffabab", size = 4.5) +
				annotate(geom = "text", x = 2107, y = 2.2,
           label = "gfdl",
           color = "#ff4040", size = 4.5) +
				annotate(geom = "text", x = 2107, y = 3.5,
           label = "miroc",
           color = "#ffb733", size = 4.5) +
				annotate(geom = "text", x = 1994, y = 1,
           label = "hindcast",
           color = "black", alpha = 0.5, size = 4.5)
		
	#plot together
	plot1 <- study_domain_plot + theme(plot.margin = unit(c(0.2, 0, 0.2, 0.2), "in"))
	plot2 <- low_temp_time_lab + theme(plot.margin = unit(c(0.2, 0.025, 0.2, 0), "in"))
	plot3 <- high_temp_time_lab + theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0), "in"))
		
	time_series_temp_domain <- plot1 + plot2 + plot3
	
	ggsave("./output/plots/time_series_temp_domain.png",
			 time_series_temp_domain,
			 width = 15, height = 5, units = "in")

