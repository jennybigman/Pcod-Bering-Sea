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
  
  ## see dataset
  #nc_handle <- nc_open(paste(url_base,opendap_area,sep="")) 
  #nc_handle
  
  # create objects for known lats and longs and xi and eta axes
  lats <- ncvar_get(nc,"lat_rho")
  lons <- ncvar_get(nc,"lon_rho")
  
  xi_axis  <- seq(1,182) # Hardcoded axis length, ROMS coordinates
  eta_axis <- seq(1,258) # Hardcoded axis length, ROMS coordinates

  # download arrays
  area_array<- ncvar_get(nc, "area_feast")
  
  lat_psi_array<- ncvar_get(nc, "lat_psi")
  
  lon_psi_array<- ncvar_get(nc, "lon_psi")
  
  lat_rho_array<- ncvar_get(nc, "lat_rho")
  
  lon_rho_array<- ncvar_get(nc, "lon_rho")
  
  domain_array <- ncvar_get(nc, "domain_feast")

  bathy_array <- ncvar_get(nc, "h")
  
  
  
  
  # name the dimensions
  dim(area_array)
  
  dimnames(area_array) <- list("Xi" = xi_axis,"Eta" = eta_axis)

  ## PSI lat/long
  
  xi_axis_psi  <- seq(1,181) # Hardcoded axis length, ROMS coordinates
  eta_axis_psi <- seq(1,257) # Hardcoded axis length, ROMS coordinates

  dimnames(lat_psi_array) <- list("Xi" = xi_axis_psi,"Eta" = eta_axis_psi)
	dimnames(lon_psi_array) <- list("Xi" = xi_axis_psi,"Eta" = eta_axis_psi)

	lat_psi_df <- reshape2::melt(lat_psi_array, 
                varnames=c("Xi", "Eta"), 
                value.name="lat_psi")
	 
	lon_psi_df <- reshape2::melt(lon_psi_array, 
                varnames=c("Xi", "Eta"), 
                value.name="lon_psi")
	
	coords_psi <- merge(lat_psi_df, lon_psi_df, by = c("Xi", "Eta"))
	
	coords_psi <- coords_psi %>%
			mutate(long_not_360 = case_when(
				lon_psi >= 180 ~ lon_psi - 360,
				lon_psi < 180 ~ lon_psi)) %>%
		 mutate(latitude = lat_psi,
		 			  longitude_not_360 = long_not_360)
    
  coords_psi_sf <- coords_psi %>%
  	  	st_as_sf(coords = c("long_not_360", "lat_psi"), crs = 4326)
  
	ggplot() +
		geom_sf(data = coords_psi_sf) +
		coord_sf(crs = 3338)

	## RHO lat/long
	
	dimnames(lat_rho_array) <- list("Xi" = xi_axis,"Eta" = eta_axis)
	dimnames(lon_rho_array) <- list("Xi" = xi_axis,"Eta" = eta_axis)

	lat_rho_df <- reshape2::melt(lat_rho_array, 
                varnames=c("Xi", "Eta"), 
                value.name="lat_rho") 

	 
	lon_rho_df <- reshape2::melt(lon_rho_array, 
                varnames=c("Xi", "Eta"), 
                value.name="lon_rho")
	
	coords_rho <- merge(lat_rho_df, lon_rho_df, by = c("Xi", "Eta"))
	
	coords_rho <- coords_rho %>%
			mutate(long_not_360 = case_when(
				lon_rho >= 180 ~ lon_rho - 360,
				lon_rho < 180 ~ lon_rho)) %>%
		 mutate(latitude = lat_rho,
		 			  longitude_not_360 = long_not_360)
    
  coords_rho_sf <- coords_rho %>%
  	  	st_as_sf(coords = c("long_not_360", "lat_rho"), crs = 4326)
  

	ggplot() +
		geom_sf(data = coords_rho_sf) +
		coord_sf(crs = 3338)


	### trim dfs
	
	coords_rho_sf_sum <- st_intersection(coords_rho_sf, full_poly) 
	
	coords_rho_sf_sum_albers <- st_transform(coords_rho_sf_sum, crs = 3338) # in albers for plotting
	
	
	
	coords_psi_sf_sum <- st_intersection(coords_psi_sf, full_poly) 
	
	coords_psi_sf_sum_albers <- st_transform(coords_psi_sf_sum, crs = 3338) # in albers for plotting
	
	
	
	coords_psi_sf_sum$id <- rownames(coords_psi_sf_sum)
	
	ggplot(coords_psi_sf_sum, aes(x = longitude_not_360, y = latitude)) +
		geom_polygon(aes(group = factor(id)), color = "black") +
		geom_point(data = )
	
  # turn into dataframe from array
  area_df <- reshape2::melt(area_array, 
                  varnames=c("Xi", "Eta"), 
                  value.name="area_km2")

  # add lat/long cols
  area_df$longitude <- lons[cbind(area_df$Xi,area_df$Eta)]
  area_df$latitude <- lats[cbind(area_df$Xi,area_df$Eta)]

  plot(coords_psi)
 
	# save df
  fwrite(area_df, "./data/ROMS_area_grid_cells.csv")
  
  # close the connection
  nc_close(nc)



  
 
