  # accessing the grid file to get areas

	# libraries
  library(ncdf4)
  library(thredds)   
  library(reshape2)
  
  # create objects for known lats and longs
  lats <- ncvar_get(nc,"lat_rho")
  lons <- ncvar_get(nc,"lon_rho")

  # download area data
  url_base <- "https://data.pmel.noaa.gov/aclim/thredds/"
  opendap  <- "dodsC/extended_grid/Bering10K_extended_grid.nc"
  
  nc <- nc_open(paste(url_base,opendap,sep=""))

  area_array<- ncvar_get(nc, "area_feast")
  
  # name the dimensions
  dim(area_array)
  
  dimnames(area_array)<-list("Xi"=xi_axis,"Eta"=eta_axis)

  # turn into dataframe from array
  area_df <-
    melt(area_array, varnames=c("Xi", "Eta"), value.name="area_km2")

  # add lat/long cols
  area_df$longitude <- lons[cbind(area_df$Xi,area_df$Eta)]
  area_df$latitude <- lats[cbind(area_df$Xi,area_df$Eta)]

  # remove lats and longs not dataset

  lats_df <- unique(sm_temp_hind_df$latitude)
  longs_df <- unique(sm_temp_hind_df$longitude)

  area_df2 <- area_df %>% 
    filter(., latitude %in% lats_df) %>%
    filter(., longitude %in% longs_df)


    length(unique(sm_temp_hind_df$longitude))
    
    z <- (unique(area_df$Lon))
    
    	area_df_sf <- area_df %>%
    	  mutate(long_not_360 = longitude - 360) %>%
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





fwrite(temp_df, "./data/ROMS_area_grid_cells.csv")

    # check out variable info 

    nc_handle <- nc_open(paste(url_base,opendap,sep="")) 

    # Show metadata from this dataset
    nc_handle

    # Close the connection
    nc_close(nc_handle)


