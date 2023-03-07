  # accessing the grid file to get area, depth, and domain ---- doesnt work with new organization on thredds server

	# libraries
  library(ncdf4)
  library(thredds)   
  library(reshape2)
  library(data.table)
  
  # set up download from server 
  url_base <- "https://data.pmel.noaa.gov/aclim/thredds/"
  opendap_area  <- "dodsC/ancillary/Bering10K_extended_grid.nc"
  
  nc <- nc_open(paste(url_base, opendap_area, sep=""))
  
  nc # to see variables
  
  # create objects for known lats and longs and xi and eta axes
  lats <- ncvar_get(nc,"lat_rho")
  lons <- ncvar_get(nc,"lon_rho")
  
  xi_axis  <- seq(1,182) # Hardcoded axis length, ROMS coordinates
  eta_axis <- seq(1,258) # Hardcoded axis length, ROMS coordinates


  # download area array
  area_array<- ncvar_get(nc, 'area_feast') 
  
  # name the dimensions
  dim(area_array)
  
  # turn into dataframe from array
  area_df <- reshape2::melt(area_array, 
                  varnames=c("Xi", "Eta"),
                  value.name="area_km2") %>% na.omit()
  
  # add lat/long cols
  area_df$longitude <- lons[cbind(area_df$Xi,area_df$Eta)]
  area_df$latitude <- lats[cbind(area_df$Xi,area_df$Eta)]
  
  # plot
  area_df_sf <- area_df	%>% 
		mutate(long_not_360 = case_when(
					 longitude >= 180 ~ longitude - 360,
					 longitude < 180 ~ longitude))  %>%
  	st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326, remove = FALSE)

	# plot
  ggplot() +
		geom_sf(data = area_df_sf, aes(color = area_km2))  +
		geom_sf(data = world_map_data, fill = "darkgrey", lwd = 0) +
		coord_sf(crs = 3338) +
		scale_color_viridis_c() +
 		scale_x_continuous(
 			breaks = breaks_x,
 			labels = labels_x,
 			limits = limits_x
 		) +
 		scale_y_continuous(
 			breaks = breaks_y,
 			limits = limits_y
 		) +
		theme_bw() 
  
  
	# save df
  fwrite(area_df, "./data/ROMS_area_grid_cells.csv")
  
  #### depth ####
  
  # download array
  final_bathy_array <- ncvar_get(nc, "h") # long name is 'final bathymetry at RHO-points'
  
  dim(final_bathy_array)
  # turn into dataframe from array
  
  depth_df <- reshape2::melt(final_bathy_array, 
                  	varnames=c("Xi", "Eta"), 
                  	value.name="depth")

  # add lat/long cols
  depth_df$longitude <- lons[cbind(depth_df$Xi,depth_df$Eta)]
  depth_df$latitude <- lats[cbind(depth_df$Xi,depth_df$Eta)]
	
  # plot
  depth_df_sf <- depth_df	%>% 
		mutate(long_not_360 = case_when(
					 longitude >= 180 ~ longitude - 360,
					 longitude < 180 ~ longitude))  %>%
  	st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326, remove = FALSE)

	# plot
  ggplot() +
		geom_sf(data = depth_df_sf, aes(color = depth))  +
		geom_sf(data = world_map_data, fill = "darkgrey", lwd = 0) +
		coord_sf(crs = 3338) +
		scale_color_viridis_c() +
 		scale_x_continuous(
 			breaks = breaks_x,
 			labels = labels_x,
 			limits = limits_x
 		) +
 		scale_y_continuous(
 			breaks = breaks_y,
 			limits = limits_y
 		) +
		theme_bw() 
  
  ### grid cells < 250 m depth
  
  depth_df_sf2 <- depth_df	%>% 
  	filter(depth <= 250) %>%
		mutate(long_not_360 = case_when(
					 longitude >= 180 ~ longitude - 360,
					 longitude < 180 ~ longitude))  %>%
  	st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326, remove = FALSE)

	# plot
  ggplot() +
		geom_sf(data = depth_df_sf2, aes(color = as.numeric(depth)))  +
		geom_sf(data = world_map_data, fill = "darkgrey", lwd = 0) +
		coord_sf(crs = 3338) +
		scale_color_viridis_c() +
 		scale_x_continuous(
 			breaks = breaks_x,
 			labels = labels_x,
 			limits = limits_x
 		) +
 		scale_y_continuous(
 			breaks = breaks_y,
 			limits = limits_y
 		) +
		theme_bw() 
  
 	# save df
  fwrite(depth_df, "./data/ROMS_depth_df.csv")
 
  #### domain ####
  
  # download area array
  domain_array<- ncvar_get(nc, "domain_feast")
  
   # name the dimensions
  dim(domain_array)
  
  dimnames(domain_array) <- list("Xi" = xi_axis,"Eta" = eta_axis)

  # turn into dataframe from array
  domain_df <- reshape2::melt(domain_array, 
                  varnames=c("Xi", "Eta"), 
                  value.name="domain")
  
  # add lat/long cols
  domain_df$longitude <- lons[cbind(domain_df$Xi,domain_df$Eta)]
  domain_df$latitude <- lats[cbind(domain_df$Xi,domain_df$Eta)]

  
  #### plot 
  
  domain_df_sf <- domain_df	%>% 
		mutate(long_not_360 = case_when(
					 longitude >= 180 ~ longitude - 360,
					 longitude < 180 ~ longitude))  %>%
  	st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326, remove = FALSE)

	# plot
  ggplot() +
		geom_sf(data = domain_df_sf, aes(color = domain))  +
		geom_sf(data = world_map_data, fill = "darkgrey", lwd = 0) +
		coord_sf(crs = 3338) +
		scale_color_viridis_c() +
 		scale_x_continuous(
 			breaks = breaks_x,
 			labels = labels_x,
 			limits = limits_x
 		) +
 		scale_y_continuous(
 			breaks = breaks_y,
 			limits = limits_y
 		) +
		theme_bw() 
  
  ## remove domain 0
  domain_df_sf2 <- domain_df	%>% 
   	filter(domain > 0) %>%
		mutate(long_not_360 = case_when(
					 longitude >= 180 ~ longitude - 360,
					 longitude < 180 ~ longitude))  %>%
  	st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326, remove = FALSE)

	# plot
  ggplot() +
		geom_sf(data = domain_df_sf2, aes(color = domain))  +
		geom_sf(data = world_map_data, fill = "darkgrey", lwd = 0) +
		coord_sf(crs = 3338) +
		scale_color_viridis_c() +
 		scale_x_continuous(
 			breaks = breaks_x,
 			labels = labels_x,
 			limits = limits_x
 		) +
 		scale_y_continuous(
 			breaks = breaks_y,
 			limits = limits_y
 		) +
		theme_bw() 
  
  # save df
  fwrite(domain_df, "./data/ROMS_domain_df.csv")
  
  
  # close the connection
  nc_close(nc)


