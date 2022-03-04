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
	library(grid.text)
	
	# turn off spherical geometry
	sf_use_s2(FALSE)
	
	# create object for greater than or equal to symbol
	symbol<-"\u2265"

	#### load and transform data ####
	
	# projected data

	ROMS_projected_dat_drp <- fread(file = "./data/ROMS_projected_dat_drp.csv") %>%
		mutate(latitude = Lat,
					 longitude = Lon)
	
	# add name of month for plotting
	ROMS_projected_dat$month_name <- NA
     
  ROMS_projected_dat_drp$month_name[ROMS_projected_dat_drp$month == 1] <- "January"
  ROMS_projected_dat_drp$month_name[ROMS_projected_dat_drp$month == 2] <- "February"
	ROMS_projected_dat_drp$month_name[ROMS_projected_dat_drp$month == 3] <- "March"
	ROMS_projected_dat_drp$month_name[ROMS_projected_dat_drp$month == 4] <- "April"

	# reorder for plotting

  ROMS_projected_dat_drp$month_name <- factor(ROMS_projected_dat_drp$month_name)
  ROMS_projected_dat_drp$month_name <- fct_reorder(ROMS_projected_dat_drp$month_name, 
  																		ROMS_projected_dat_drp$month)


  # convert to a shapefile
  
  ROMS_projected_dat_drp_sf <- ROMS_projected_dat_drp %>%
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
    width = 7, height = 7, units = "in")
  }
  

