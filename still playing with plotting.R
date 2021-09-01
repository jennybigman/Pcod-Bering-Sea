 # try without built in ACLIM function

 bering_crs_test <- CRS("+init=epsg:3832")
 
 bering_crs_test2 <-  "+init=epsg:3571 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
 
 AK_proj <- "+init=epsg:3832 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
 
   test_sf <- st_as_sf(data_long_sum, coords = c("latitude", "longitude"), crs = bering_crs_test)
   											
   											("+init=epsg:3571 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  
   
   plot_stations_basemap(sfIN = df1,fillIN = "mean_temp",
   																								 colorIN = "mean_temp", 
   																								 sizeIN = 0.6) + 
  														scale_color_viridis_c() +
  														scale_fill_viridis_c()
   
  												ggplot() +
  												geom_sf(data = test_sf, aes(color = mean_temp)) + 
  										  	coord_sf(crs = crs_bering) +
   												scale_x_continuous(
  													breaks = c(160, 170, 180, 190, 200, 210),
   												) +
  												scale_color_viridis_c() +
   												labs(colour = "Bottom temperature (˚C)") +
   												theme_bw() 
  												