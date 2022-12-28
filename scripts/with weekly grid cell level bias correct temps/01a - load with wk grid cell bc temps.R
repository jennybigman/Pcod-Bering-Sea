# 01a load each time weekly grid cell level bias correction

	# load each time 

	beep <- function(x = "fanfare"){
		beepr::beep(x)
	}
	
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
	#library(rgdal)
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
	library(glue)
	library(MuMIn)

	# turn off spherical geometry
	sf_use_s2(FALSE)
	
	# create object for greater than or equal to symbol
	symbol<-"\u2265"
	
		#### load map ####
	world_map_data <- ne_countries(scale = "medium", returnclass = "sf") 
	
	#### plotting function ####
  ggsave_func <- function(x,y,w = 10,h = 10){
  	ggsave(plot = x,
    file = paste(y,".png",sep=""),
    width = w, height = h, units = "in")
  }
  
	# for plotting
  breaks_x <- c(-170, -160)
	labels_x <- c("-170˚", "-160˚") 
	limits_x <- c(-1400000, -150000)
	
	breaks_y <- c(55, 60)
	limits_y <- c(470000, 1900000)


	#### load and transform data ####
	
	# hindcast data
	ROMS_hindcast_dat  <- fread(file = here("./data/ROMS_hindcast_dat.csv")) %>% filter(., year != 2021)

	ROMS_hindcast_dat <- ROMS_hindcast_dat %>%
	 mutate(grid_cell_id = paste0(latitude, longitude))
	
	grid_cells <- ROMS_hindcast_dat %>%
	 distinct(grid_cell_id) %>%
	 mutate(ID = 1: length(unique(ROMS_hindcast_dat$grid_cell_id)))

	ROMS_hindcast_dat <- merge(ROMS_hindcast_dat, grid_cells, by = "grid_cell_id")

	# reorder for plotting
	ROMS_hindcast_dat$month_name <- factor(ROMS_hindcast_dat$month_name)
  ROMS_hindcast_dat$month_name <- fct_reorder(ROMS_hindcast_dat$month_name, 
  																		ROMS_hindcast_dat$month)
  
   # convert to an sf object
  ROMS_hindcast_dat <- ROMS_hindcast_dat %>%
  		 mutate(long_not_360 = case_when(
						 longitude >= 180 ~ longitude - 360,
						 longitude < 180 ~ longitude))  %>% 
  	st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326, remove = FALSE)
 
  # projection temps
  
  # read in bias corrected temps
 	proj_mo_dfs <- fread(file = "./data/proj_mo_dfs.csv")

 	# add in area
 	
 	area_df <- fread( "./data/ROMS_area_grid_cells.csv")
 	
 	proj_mo_dfs <- left_join(proj_mo_dfs, area_df)
