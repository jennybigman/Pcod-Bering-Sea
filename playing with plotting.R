# 02 - plot bottom temps

		setwd("~/ACLIM2") 
		
		library(devtools)
		library(ncdf4)
		library(thredds)
		library(lubridate)
		library(stringr)

    source("R/make.R")
    source("R/sub_scripts/load_maps.R") 

    setwd("~/Google Drive/NOAA AFSC Postdoc/Pcod Bering Sea Habitat Suitability/Pcod-Bering-Sea")
		library(data.table)
    library(tidyverse)
		all_temp_dat <- fread( "./data/all_temp_dat.csv")
		
		# did 
    
		# add two columns: one with the date in Date format and one for just the month number
		all_temp_dat$date <- as.Date(all_temp_dat$time)
		
		all_temp_dat$month <- month(all_temp_dat$date)
		
		all_temp_dat$day <- day(all_temp_dat$date)

    #### make summaries of data to test plots -----------------------
    
    # all data
    data_long_sum <- all_temp_dat %>%
    								 group_by(latitude, longitude) %>%
    								 summarise(mean_temp = mean(val)) 
    
    data_long_sum_NA <- na.omit(data_long_sum)
    	
	 # does not project properly
	  ggplot(data_long_sum, aes(x = longitude, y = latitude)) + 
    geom_tile(aes(fill=mean_temp)) +
    scale_fill_viridis_c(name = "Temperature (C)") + 
    theme_void() + 
    coord_quickmap() 

	 # convert to shape format for plotting
   test_sf <- convert2shp(data_long_sum)
   
   # plot using ACLIM function
   bottomT_aclim_func_plot <- plot_stations_basemap(sfIN = test_sf,fillIN = "mean_temp",
   																								 colorIN = "mean_temp", 
   																								 sizeIN = 0.6) + 
  														scale_color_viridis_c() +
  														scale_fill_viridis_c() 
   														

   
   ggsave(file = ("./output/plots/bottomT_aclim_func_plot.pdf"),
   			  plot = bottomT_aclim_func_plot,
					height = 7.5, width = 12, units = "in")

   
   # build ggplot from scratch
   bottomT_ggplot_plot <- ggplot() +
  												geom_sf(data = test_sf, aes(fill = mean_temp, color = mean_temp)) +
  												geom_sf(data=st_transform(bering_sf,crs=crs_bering),fill="white", # this is the part I need to figure out
  																color="black",lwd=0.5) +
  												coord_sf(crs = crs_bering) +
  												     scale_color_viridis_c() +
  												     scale_fill_viridis_c() 
   
    ggsave(file = ("./output/plots/bottomT_ggplot_plot.pdf"),
   			  plot = bottomT_ggplot_plot,
					height = 7.5, width = 12, units = "in")

  
   # lat range 52 - 70, long range 180 -210 (resolution of land shape)
   
   data_long_sum2 <-data_long_sum %>% 
    								filter(between(latitude, 52,70))
    	
   data_long_sum2 <-data_long_sum %>% 
    								filter(between(longitude,180, 210))
   
   # convert to shapefile format
   test_sf2 <- convert2shp(data_long_sum2)

   # plot using ACLIM function
   bottomT_aclim_func_plot_trunc <- plot_stations_basemap(sfIN = test_sf2,fillIN = "mean_temp", 
   																											 colorIN = "mean_temp") + 
  																  scale_color_viridis_c() +
  																  scale_fill_viridis_c() 
   
   ggsave(file = ("./output/plots/bottomT_aclim_func_plot_trunc.pdf"),
   			  plot = bottomT_aclim_func_plot_trunc,
					height = 7.5, width = 12, units = "in")

   
   # build ggplot from scratch
   bottomT_ggplot_plot_trunc <- ggplot() +
  											        geom_sf(data = test_sf2, aes(fill = mean_temp, color = mean_temp)) +
  											        coord_sf(crs = crs_bering) +
  											             scale_color_viridis_c() +
  											             scale_fill_viridis_c()
   
    ggsave(file = ("./output/plots/bottomT_ggplot_plot_trunc.pdf"),
   			  plot = bottomT_aclim_func_plot_trunc,
					height = 7.5, width = 12, units = "in")

    	
 