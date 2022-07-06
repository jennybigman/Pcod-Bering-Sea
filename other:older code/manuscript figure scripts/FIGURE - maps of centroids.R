## centroids calculated with denominator as the sum of all suitable spawning habitat for that year

######################## A WORK IN PROGRES ######################################################
#################################################################################################
#################################################################################################
#################################################################################################
#################################################################################################

	#### calculate centroid per year ####

	# hindcast 
	
	hind_centroid_func <- function(x){
		
		new_dat <- ROMS_hindcast_dat %>% 
			filter(., year == x) %>%
			rowwise() %>%
			mutate(lat_cent_top = (sp_hab_suit * latitude),
						 long_cent_top = (sp_hab_suit * longitude))
		
		lat_cent_num = sum(new_dat$lat_cent_top)
		long_cent_num = sum(new_dat$long_cent_top)	
		sp_hab_suit_sum = sum(new_dat$sp_hab_suit)
	 
		lat_centroid = lat_cent_num/sp_hab_suit_sum
		long_centroid = long_cent_num/sp_hab_suit_sum
		
		years_hind <- c(1970:2020)

		centroids <- 
			data.frame(lat_centroid, 
								 long_centroid, 
								 years_hind) %>%
			rename(year = years_hind)

		}
	
	years_hind <- c(1970:2020)
	
	hind_centroid <- lapply(years_hind,  hind_centroid_func)
	
	hind_centroid_df <- bind_rows(hind_centroid)
	
	hind_centroid_df_sf <- hind_centroid_df %>%
			mutate(long_centroid_not_360 = case_when(
				long_centroid >= 180 ~ long_centroid - 360,
				long_centroid < 180 ~ long_centroid)) %>%
  		st_as_sf(coords = c("long_centroid_not_360", "lat_centroid"), crs = 4326, remove = FALSE)

	
	
	# projections
	
	years_proj <- 2020:2099

	ROMS_projected_dat_trim <- ROMS_projected_dat %>%
		filter(year %in% years_proj) %>%
		dplyr::select(year, sp_hab_suit_var, simulation, projection, latitude, longitude)
	
	proj_centroid_func <- function(x){
		
		new_dat <- ROMS_projected_dat_trim %>% 
			group_by(year, projection) %>%
			filter(., year == x) %>%
			rowwise() %>%
			mutate(lat_cent_top = (sp_hab_suit_var * latitude),
						 long_cent_top = (sp_hab_suit_var * longitude))
	
		new_dat2 <- new_dat %>%
			group_by(year, projection) %>%
			summarize(lat_cent_num = sum(lat_cent_top),
								long_cent_num <-sum(long_cent_top),
								sp_hab_suit_sum = sum(sp_hab_suit_var),
								lat_centroid = lat_cent_num/sp_hab_suit_sum,
								long_centroid = long_cent_num/sp_hab_suit_sum)

		}
	

		proj_centroid <- lapply(years_proj,  proj_centroid_func)
	
		proj_centroid_df <- bind_rows(proj_centroid) 
		
		proj_centroid_df_sf <- proj_centroid_df %>%
			mutate(long_centroid_not_360 = case_when(
				long_centroid >= 180 ~ long_centroid - 360,
				long_centroid < 180 ~ long_centroid)) %>%
  		st_as_sf(coords = c("long_centroid_not_360", "lat_centroid"), crs = 4326, remove = FALSE)

	
	#### plot ####
	
	# set up plots
		
				
	proj_centroid_df_sf$scen <- NA
	
	proj_centroid_df_sf$scen[proj_centroid_df_sf$projection == "ssp126"] <- "low emission\n(ssp126)"
	proj_centroid_df_sf$scen[proj_centroid_df_sf$projection == "ssp585"] <- "high emission\n(ssp585)"

	proj_centroid_df_sf$scen_f = factor(proj_centroid_df_sf$scen, 
																		levels=c('low emission\n(ssp126)', 'high emission\n(ssp585)'))

	proj_centroid_df_sf$scen_f = factor(proj_centroid_df_sf$scen, 
																		levels=c('low emission\n(ssp126)', 'high emission\n(ssp585)'))

		
	
	map_centroids <- 
		 	ggplot() +
			geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
			coord_sf(crs = 3338) +
 			scale_x_continuous(
 				breaks = c(-175, -170, -165, -160),
 				labels = c("-175˚", "-170˚", "-165˚", "-160˚"),
 				name = "Longitude",
 				limits = c(-1400000, 10000)) +
 			scale_y_continuous(
 				breaks = c(55, 60),
 				limits = c(470000, 1900000),
 				name = "Latitude") +
  		geom_point(data = hind_centroid_df_sf, aes(color = year, 
  			geometry = geometry), alpha = 0.5, 
    		stat = "sf_coordinates") +
			geom_point(data = proj_centroid_df_sf, aes(color = year, 
  			geometry = geometry), alpha = 0.5, 
    		stat = "sf_coordinates") +
		facet_grid(~ scen_f) +
		scale_color_viridis_c(
			breaks = c(1980, 2020, 2060, 2099)
		) +
	  theme_bw() +
 		theme(
			legend.title.align = 0.5,
 			panel.border = element_rect(color = "#666666"),
 			strip.text.x = element_text(size = 12, face = "bold",  color = "black"),
 			strip.text.y = element_text(size = 12, face = "bold",  color = "black"),
 			strip.background = element_blank(),
 			axis.text = element_text(size = 10,  color = "#666666"),	
  		axis.title = element_text(size = 12,  color = "#666666"),
 			axis.ticks = element_line( color = "#666666"))

		ggsave("./output/plots/mean_lats_map.png",
			 map_mean_lats,
			 width = 10, height = 5, units = "in")