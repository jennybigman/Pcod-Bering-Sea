  # accessing the grid file to get area, depth, and domain 

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

  #### area ####
  
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
  
  #### depth ####
  
  # download array
  final_bathy_array <- ncvar_get(nc, "h") # long name is 'final bathymetry at RHO-points'
  
  # turn into dataframe from array
  
  depth_df <- reshape2::melt(final_bathy_array, 
                  	varnames=c("Xi", "Eta"), 
                  	value.name="depth")

  # add lat/long cols
  depth_df$longitude <- lons[cbind(depth_df$Xi,depth_df$Eta)]
  depth_df$latitude <- lats[cbind(depth_df$Xi,depth_df$Eta)]
	
  # close the connection
  nc_close(nc)
  
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

  # save df
  fwrite(domain_df, "./data/ROMS_domain_df.csv")
  
  
  # close the connection
  nc_close(nc)


