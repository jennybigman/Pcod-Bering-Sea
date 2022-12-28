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
  
  area_df <- fread( "./data/ROMS_area_grid_cells.csv")
	depth_df <- fread("./data/ROMS_depth_df.csv")

	# merge
	area_depth_df <- merge(area_df, depth_df, by = c("latitude", "longitude", "Xi", "Eta"))

	area_depth_domain_df <- merge(area_depth_df, domain_df,
													by = c("latitude", "longitude", "Xi", "Eta"))

  area_depth_domain_df_sf <- area_depth_domain_df %>%
  		mutate(long_not_360 = case_when(
						 longitude >= 180 ~ longitude - 360,
						 longitude < 180 ~ longitude))  %>%
  	st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326, remove = FALSE)
	
  area_depth_domain_df_sf <- area_depth_domain_df_sf %>%
  	filter(domain > 0)
  
  area_depth_domain_df_sf <- area_depth_domain_df_sf %>%
		filter(., between(depth, 0, 250))
  
  area_depth_domain_df_sf$domain_f <- as.factor(area_depth_domain_df_sf$domain)
  
  domain_map <- 
  	ggplot() +
  	geom_sf(data = area_depth_domain_df_sf, aes(color = domain_f)) +
  	coord_sf(crs = 3338) +
 		scale_y_continuous(
 				breaks = c(52, 56, 60, 64),
 				name = "Latitude") +
  	scale_x_continuous(
 			breaks = c(-160, - 170),
 			name = "Longitude")
  
  ggsave(here("./output/plots/domain_map.png"),
			 domain_map,
			 width = 4, height = 4, units = "in")

