# 00c accessing depth

	library(here)
	library(marmap)
  library(ncdf4)
  library(thredds)   
  library(reshape2)
  library(data.table)

	#### ROMS bathy ####
  		
	# download ROMS bathy

	url_base <- "https://data.pmel.noaa.gov/aclim/thredds/"
  opendap  <- "dodsC/extended_grid/Bering10K_extended_grid.nc"
  
  nc <- nc_open(paste(url_base,opendap,sep=""))
  
  ## to see data 
  
  # Open ncdf4 connection with dataset
  #nc_handle <- nc_open(paste(url_base,opendap,sep="")) 

  # Show metadata from this dataset
	#nc_handle

  # Close the connection
  #nc_close(nc_handle)
    
    
  # create objects for known lats and longs and xi and eta axes
  lats <- ncvar_get(nc,"lat_rho")
  lons <- ncvar_get(nc,"lon_rho")
  
  xi_axis  <- seq(1,182) # Hardcoded axis length, ROMS coordinates
  eta_axis <- seq(1,258) # Hardcoded axis length, ROMS coordinates

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
 