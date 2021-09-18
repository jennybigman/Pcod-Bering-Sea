  # accessing the grid file to get areas

	# libraries
  library(ncdf4)
  library(thredds)   
  library(reshape2)
  library(data.table)
  
  # set up download from server 
  url_base <- "https://data.pmel.noaa.gov/aclim/thredds/"
  opendap_area  <- "dodsC/extended_grid/Bering10K_extended_grid.nc"
  
  nc <- nc_open(paste(url_base,opendap_area,sep=""))
  
  # create objects for known lats and longs and xi and eta axes
  lats <- ncvar_get(nc,"lat_rho")
  lons <- ncvar_get(nc,"lon_rho")
  
  xi_axis  <- seq(1,182) # Hardcoded axis length, ROMS coordinates
  eta_axis <- seq(1,258) # Hardcoded axis length, ROMS coordinates

  # download area array
  area_array<- ncvar_get(nc, "area_feast")
  
  # name the dimensions
  dim(area_array)
  
  dimnames(area_array) <- list("Xi" = xi_axis,"Eta" = eta_axis)

  # turn into dataframe from array
  area_df <- reshape2::melt(area_array, 
                  varnames=c("Xi", "Eta"), 
                  value.name="area_km2")

  # add lat/long cols
  area_df$longitude <- lons[cbind(area_df$Xi,area_df$Eta)]
  area_df$latitude <- lats[cbind(area_df$Xi,area_df$Eta)]

  
	# save df
  fwrite(area_df, "./data/ROMS_area_grid_cells.csv")
  
  # close the connection
  nc_close(nc)



  
  ##### extra code ####
  
  # plot
  area_df_sf <- area_df %>%
  	  mutate(long_not_360 = case_when(
  	        	longitude > 180 ~ longitude - 360,
  	          longitude <= 180 ~ longitude)) %>%
  	  st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326)
  
  area_df_sf <- 

  int_pts_sum <- st_intersection(area_df_sf, full_poly) 
	
	int_pts_sum_albers <- st_transform(int_pts_sum, crs = 3338)

	 world_map_data <- ne_countries(scale = "medium", returnclass = "sf") 

 		ggplot() +
		geom_sf(data = int_pts_sum_albers, aes(color = area_km2))  +
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

  
  int_pts_sum_ll <- int_pts_sum %>%
			mutate(longitude_not_360 = sf::st_coordinates(.)[, 1],
					   latitude =  sf::st_coordinates(.)[, 2])
	
area_df_sf2 <- area_df %>% 
    mutate(long_not_360 = longitude - 360) %>%
		filter(., long_not_360 %in% int_pts_sum_ll$longitude_not_360) %>%
		filter(., latitude %in% int_pts_sum_ll$latitude)
	 
  
  # remove lats and longs not dataset

  lats_df <- unique(sm_temp_hind_df$latitude)
  longs_df <- unique(sm_temp_hind_df$longitude)

  area_df2 <- area_df %>% 
    filter(., latitude %in% lats_df) %>%
    filter(., longitude %in% longs_df)


    length(unique(sm_temp_hind_df$longitude))
    
    z <- (unique(area_df$longitude))
    
    	area_df_sf <- area_df %>%
    	  mutate(long_not_360 =
  	           case_when(
  	             longitude > 180 ~ longitude - 360,
  	             longitude <= 180 ~ longitude)) %>%
  	  st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326)
    	  
# plot area 
ggplot() +
  geom_sf(data = area_df_sf, aes(color = area_km2))  +
  geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
  coord_sf(crs = 3338) +
  scale_color_viridis_c() +
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
  	axis.text = element_text(size = 12),	
  	axis.title = element_text(size = 14),
  	legend.title.align=0.5)




  
   # check out variable info 

    nc_handle <- nc_open(paste(url_base,opendap,sep="")) 

    # Show metadata from this dataset
    nc_handle

   
