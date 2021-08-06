# playing with plotting

		setwd("/Users/jenniferbigman/ACLIM2") 
		
		library(devtools)
		library(ncdf4)
		library(thredds)
		library(lubridate)
		library(stringr)
		
		main   <- getwd()  #"~/GitHub_new/ACLIM2

    source("R/make.R")
    source("R/sub_scripts/load_maps.R") 

    setwd("~/Google Drive/NOAA AFSC Postdoc/Pcod Bering Sea Habitat Suitability")
		library(data.table)
    library(tidyverse)
		all_temp_dat <- fread("./data/all_temp_dat.csv")
		all_temp_dat_NA <- na.omit(all_temp_dat)
		
		test_sf <- fread("./data/test_sf.csv")
		
		# add columns
		all_temp_dat$date <- as.Date(all_temp_dat$time) # date in Date format
		
		all_temp_dat$month <- month(all_temp_dat$date) # month of year
		
		all_temp_dat$day <- day(all_temp_dat$date) # day of year

	  all_temp_dat$week <- week(all_temp_dat$date) # week of year
	  
	  # restrict dataset to months of spawning Jan - June
	  months_spawn <- c("January", "February", "March", "April", "May", "June")
	  months_spawn_num <- c(1:6)
	  
	  spawn_dates_temp_dat <- filter(all_temp_dat, month %in% months_spawn_num)
	  
	  temp_dat_sf <- convert2shp(spawn_dates_temp_dat)
	  
		# convert to shape file
	  
	  #temp_dat_1970 <- all_temp_dat %>% filter(year == 1970)
	  #temp_dat_1970_sf <- convert2shp(temp_dat_1970)
    #### make summaries of data to test plots -----------------------
    
    # all data
    data_long_sum <- all_temp_dat %>%
    								 group_by(latitude, longitude) %>%
    								 summarise(mean_temp = mean(val)) 
    
    data_long_sum_NA <- na.omit(data_long_sum)
    
	 # does not project properly
	  # ggplot(data_long_sum, aes(x = longitude, y = latitude)) + 
    # geom_tile(aes(fill=mean_temp)) +
    # scale_fill_viridis_c(name = "Temperature (C)") + 
    # theme_void() + 
    # coord_quickmap() 

	 # convert to shape format for plotting
   test_sf <- convert2shp(data_long_sum)
   
   #setwd("~/Google Drive/NOAA AFSC Postdoc/Pcod Bering Sea Habitat Suitability")
	 #fwrite(test_sf, "./data/test_sf_try.csv")
	
   st_write(test_sf, "./data/test_sf.shp")
   filename <- system.file("./data/test_sf.shp", package = "sf")
   test_sf <- st_read(filename())

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
   
   test_sf_NA <- na.omit(test_sf)
   
   bottomT_ggplot_plot <- ggplot() +
  												geom_sf(data = test_sf_NA, aes(color = mean_temp)) + 
  												geom_sf(data=st_transform(bering_sf,crs=crs_bering),fill="lightgrey", # this is the part I need to figure out
  																color="black",lwd=0.25) +
                        	xlab("Longitude") + ylab("Latitude") +
  										  	coord_sf(crs = crs_bering) +
   												scale_x_continuous(
  													breaks = c(160, 170, 180, 190, 200, 210),
   												) +
  												scale_color_viridis_c() +
   												labs(colour = "Bottom temperature (˚C)") +
   												theme_bw() 
   
    ggsave(file = ("./output/plots/bottomT_ggplot_plot.pdf"),
   			  plot = bottomT_ggplot_plot,
					height = 7.5, width = 12, units = "in")


    ## add bathymetry
    
    
    
    ## multiply by hatch success model
    hatch_success <- function(x){
    H <- (0.453 / (1 + 
    		(((x - 4.192)/2.125))^2)
    	) 
    	
    }
    
    
    test_sf_hatch <- test_sf %>%
    								 mutate(hatch_succ = sapply(mean_temp, hatch_success))
    
    hatch_success_ggplot_plot <- ggplot() +
  												geom_sf(data = test_sf_hatch, aes(color = hatch_succ)) + 
  												geom_sf(data=st_transform(bering_sf,crs=crs_bering),fill="lightgrey", # this is the part I need to figure out
  																color="black",lwd=0.25) +
                        	xlab("Longitude") + ylab("Latitude") +
  										  	coord_sf(crs = crs_bering) +
   												scale_x_continuous(
  													breaks = c(160, 170, 180, 190, 200, 210),
   												) +
  												scale_color_viridis_c() +
   												labs(colour = "Proportion hatch success") +
   												theme_bw() 
   
    ggsave(file = ("./Pcod-Bering-Sea/output/plots/hatch_success_ggplot_plot.png"),
   			  plot = hatch_success_ggplot_plot,
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

    	
	#### plot by week and year ####
    
	# first try with all weeks of one year
    
    