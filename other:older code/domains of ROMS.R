	# libraries
  library(ncdf4)
  library(thredds)   
  library(reshape2)
  library(data.table)
  
  # set up download from server 
  url_base <- "https://data.pmel.noaa.gov/aclim/thredds/"
  opendap_area  <- "dodsC/ancillary/Bering10K_extended_grid.nc"
  
  nc <- nc_open(paste(url_base,opendap_area,sep=""))
  
  # create objects for known lats and longs and xi and eta axes
  lats <- ncvar_get(nc,"lat_rho")
  lons <- ncvar_get(nc,"lon_rho")
  
  xi_axis  <- seq(1,182) # Hardcoded axis length, ROMS coordinates
  eta_axis <- seq(1,258) # Hardcoded axis length, ROMS coordinates

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

  domain_df <- fread(here("./data/ROMS_domain_df.csv"))
  

  # plot domains with sf
  ggplot() +
  geom_sf(data = domain_df_sf, aes(color = domain)) +
  scale_x_continuous(
  		limits = c(-150, -179),
 			name = "Longitude") +
 		scale_y_continuous(
 			breaks = c(50, 60, 70),
 			limits = c(48, 70),
 			name = "Latitude") 
  
  domain_df_sf0 <- domain_df_sf %>%
  	filter(domain > 0)
  
  ggplot() +
  geom_sf(data = domain_df_sf0, aes(color = domain)) +
  scale_x_continuous(
  		limits = c(-150, -179),
 			name = "Longitude") +
 		scale_y_continuous(
 			breaks = c(50, 60, 70),
 			limits = c(48, 70),
 			name = "Latitude") 
  