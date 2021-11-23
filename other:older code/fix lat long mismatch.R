	# fixing lat/long issue (where they don't overlap)

	# libraries

  library(ncdf4)
  library(thredds)   
  library(reshape2)
  library(data.table)
  
  #### download full ROMS grid ####
  
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

  full_ROMS_coords <- area_df %>% 
  	dplyr::select(latitude, longitude)
  
  #### create polygon for shelf region ####
  
  # load in datasets
 	temp_df <- fread("./data/ROMS_all_temp.csv")
	area_df <- fread( "./data/ROMS_area_grid_cells.csv")

	# merge dfs
	ROMS_dat_hind <- merge(temp_df, area_df, by = c("latitude", "longitude"),
												 all = TRUE)
	
	ROMS_dat_hind <- na.omit(ROMS_dat_hind)
	
	# create smaller df for intersection function
	ROMS_dat_hind_sum <- ROMS_dat_hind %>%
		group_by(latitude, longitude) %>%
		summarise(mean_temp = mean(temp))

	# add column of long not on 360 scale
	ROMS_dat_hind_sum <- ROMS_dat_hind_sum %>%
			mutate(long_not_360 = case_when(
				longitude >= 180 ~ longitude - 360,
				longitude < 180 ~ longitude
	))
	
	
	ROMS_dat_hind_sum$lats <- ROMS_dat_hind_sum$latitude
	ROMS_dat_hind_sum$longs_not_360 <- ROMS_dat_hind_sum$long_not_360
	
	# convert to shapefile for intersection function
	ROMS_dat_hind_sum_sf <- st_as_sf(ROMS_dat_hind_sum,
		coords = c("long_not_360", "latitude"), crs = 4326)
	

  # load Ortiz Bering Sea regions and convert to sf object
	bsregions <- readOGR("./Mapping Code - Bigman/Ortiz Regions", layer="BSIERP_regions_2012")
	bsregions <- spTransform(bsregions, CRS("+init=epsg:4326"))
	bsregions_sf <- st_as_sf(bsregions)
	bsregions_no_15 <-bsregions[bsregions@data$DOMAIN != 15, ] # remove domain 15 b/c needs to be fixed

	#plot(bsregions_no_15)
	
	bsregions_no_15_sf <- bsregions_no_15 %>% 	  
		st_as_sf(coords = c("long", "lat"), crs = 4326) 
	poly_bsregions_no_15_sf <- st_union(bsregions_no_15_sf)
	
	#plot(poly_bsregions_no_15_sf)

	# remove domain 15 to fix it
	bsregions15 <-bsregions[bsregions@data$DOMAIN == 15, ] 
	#plot(bsregions15)

	# fix domain 15
	bsregions15@data$id = rownames(bsregions15@data)
	bsregions_points15 <- fortify(bsregions15, region="id")
	bsregions15_df <- left_join(bsregions_points15, bsregions15@data, by = "id") %>%
		filter(., long < 0)
	bsregions_sf_15 <- bsregions15_df %>% st_as_sf(coords = c("long", "lat"), crs = 4326)
	
	ggplot() +
		geom_polygon(data = bsregions15_df, aes(x = long, y = lat, group = group),
 								 fill = "darkgrey", color = NA) 

	polygon_bsregion15 <- 
  	bsregions15_df %>%
	  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
	  summarise(geometry = st_combine(geometry)) %>%
	  st_cast("POLYGON") 

	plot(polygon_bsregion15)

	# add back into polygin
	full_poly <-  st_union(polygon_bsregion15, poly_bsregions_no_15_sf)
	
	plot(full_poly$geometry)

	# find intersection of points in df and polygon
	int_pts_sum <- st_intersection(ROMS_dat_hind_sum_sf, full_poly) 
	
	long_lats <- int_pts_sum %>% 
		dplyr::select(longitude, longs_not_360, lats)
	
	long_lats <- long_lats %>%
		rename(latitude = lats) %>%
		dplyr::select(longitude, latitude) %>%
		st_drop_geometry()
	
	# not working 
	
	Ortiz_ROMS_coords <- inner_join(long_lats, full_ROMS_coords)
	