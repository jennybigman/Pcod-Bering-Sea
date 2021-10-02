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
	
	# turn off spherical geometry
	sf_use_s2(FALSE)
	
	# create object for greater than or equal to symbol
	symbol<-"\u2265"

	#### load and transform data ####
	ROMS_dat_hind_trim <-	fread(file = "./data/SpawnMonths_ROMS_dat_hind_trim.csv")
	
	# reorder for plotting
	ROMS_dat_hind_trim$month_name <- factor(ROMS_dat_hind_trim$month_name)
  ROMS_dat_hind_trim$month_name <- fct_reorder(ROMS_dat_hind_trim$month_name, 
  																		ROMS_dat_hind_trim$month)

  # convert to a shapefile
  ROMS_dat_hind_trim_sf <- ROMS_dat_hind_trim %>%
  	st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326)
	
	#### load map ####
	world_map_data <- ne_countries(scale = "medium", returnclass = "sf") 
	
	#### plotting function ####
  ggsave_func <- function(x,y){
  	ggsave(plot = x,
    file = paste(y,".png",sep=""),
    width = 10, height = 10, units = "in")
  }
  

  