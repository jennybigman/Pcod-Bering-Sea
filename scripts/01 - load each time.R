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
	
	# turn off spherical geometry
	sf_use_s2(FALSE)
	
	# create object for greater than or equal to symbol
	symbol<-"\u2265"

	#### load and transform data ####
	
	# hindcast data
	ROMS_hindcast_dat  <- fread(file = "./data/ROMS_hindcast_dat.csv") %>% filter(., year != 2021)
	
	# projected data
	years_drop <- c(2021:2099)
	
	ROMS_projected_dat <- fread(file = "./data/ROMS_projected_dat.csv") %>%
		filter(., year %in% years_drop) %>%
		mutate(latitude = Lat,
					 longitude = Lon)

	
	# reorder for plotting
	ROMS_hindcast_dat$month_name <- factor(ROMS_hindcast_dat$month_name)
  ROMS_hindcast_dat$month_name <- fct_reorder(ROMS_hindcast_dat$month_name, 
  																		ROMS_hindcast_dat$month)
  
  ROMS_projected_dat$month_name <- factor(ROMS_projected_dat$month_name)
  ROMS_projected_dat$month_name <- fct_reorder(ROMS_projected_dat$month_name, 
  																		ROMS_projected_dat$month)


  # convert to a shapefile
  ROMS_hindcast_dat_sf <- ROMS_hindcast_dat %>%
  	st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326)
  
  ROMS_projected_dat_sf <- ROMS_projected_dat %>%
  		mutate(long_not_360 = case_when(
				longitude >= 180 ~ longitude - 360,
				longitude < 180 ~ longitude)) %>%
  	st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326)
	
	
	#### load map ####
	world_map_data <- ne_countries(scale = "medium", returnclass = "sf") 
	
	#### plotting function ####
  ggsave_func <- function(x,y){
  	ggsave(plot = x,
    file = paste(y,".png",sep=""),
    width = 10, height = 10, units = "in")
  }
  

