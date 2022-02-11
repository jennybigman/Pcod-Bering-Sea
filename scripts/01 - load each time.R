	# load each time 

	#### libraries ####
	library(here)
	library(rnaturalearth)
	library(dplyr)
	library(tidyverse)
	library(ggplot2)
	library(data.table)
	library(sf)
	library(forcats)
	library(lubridate)
	library(patchwork)
	library(raster)
	library(rgdal)
	library(stars)
	library(marmap)
	library(zoo)
	library(RColorBrewer)
	library(beepr)
	library(mapdata)
	library(zoo)
	library(runner)
	library(tidync)
	library(cowplot)
	library(scales)
	library(ggtext)
	library(ggpubr)
	
	# turn off spherical geometry
	sf_use_s2(FALSE)
	
	# create object for greater than or equal to symbol
	symbol<-"\u2265"

	#### load and transform data ####
	
	# hindcast data
	ROMS_hindcast_dat  <- fread(file = "./data/ROMS_hindcast_dat.csv") %>% filter(., year != 2021)
	
	# projected data

	ROMS_projected_dat <- fread(file = "./data/ROMS_projected_dat.csv") %>%
		mutate(latitude = Lat,
					 longitude = Lon)
	
	# add name of month for plotting
	ROMS_projected_dat$month_name <- NA
     
  ROMS_projected_dat$month_name[ROMS_projected_dat$month == 1] <- "January"
  ROMS_projected_dat$month_name[ROMS_projected_dat$month == 2] <- "February"
	ROMS_projected_dat$month_name[ROMS_projected_dat$month == 3] <- "March"
	ROMS_projected_dat$month_name[ROMS_projected_dat$month == 4] <- "April"
#	
#	## add in area and depth
#	
#	area_depth_df <- ROMS_hindcast_dat %>%
#		dplyr::select(latitude, longitude, depth, area_km2)
#	
#	ROMS_projected_dat <- merge(ROMS_projected_dat, area_depth_df,
#															 by = c("latitude", "longitude"))
#

	# reorder for plotting
	ROMS_hindcast_dat$month_name <- factor(ROMS_hindcast_dat$month_name)
  ROMS_hindcast_dat$month_name <- fct_reorder(ROMS_hindcast_dat$month_name, 
  																		ROMS_hindcast_dat$month)
  
  ROMS_projected_dat$month_name <- factor(ROMS_projected_dat$month_name)
  ROMS_projected_dat$month_name <- fct_reorder(ROMS_projected_dat$month_name, 
  																		ROMS_projected_dat$month)


  # convert to a shapefile
  ROMS_hindcast_dat_sf <- ROMS_hindcast_dat %>%
  	st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326, remove = FALSE)
  
  ROMS_projected_dat_sf <- ROMS_projected_dat %>%
  		mutate(longitude = Lon,
  					 latitude = Lat,
  					 long_not_360 = case_when(
						 longitude >= 180 ~ longitude - 360,
						 longitude < 180 ~ longitude)) %>%
  	st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326, remove = FALSE)
	
	
	#### load map ####
	world_map_data <- ne_countries(scale = "medium", returnclass = "sf") 
	
	#### plotting function ####
  ggsave_func <- function(x,y){
  	ggsave(plot = x,
    file = paste(y,".png",sep=""),
    width = 3, height = 3, units = "in")
  }
  

