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
	
	sm_temp_hind_df <- fread( "./data/sm_temp_df_poly_depth.csv")
	
  # standardize hatch success (calculating spawning habitat suitability)
  sm_temp_hind_df <- sm_temp_hind_df %>%
  	mutate(sp_hab_suit = hatch_success_cauchy/max(hatch_success_cauchy))
  
  # reorder for plotting
	sm_temp_hind_df$month_name <- factor(sm_temp_hind_df$month_name)
  sm_temp_hind_df$month_name <- fct_reorder(sm_temp_hind_df$month_name, 
  																		sm_temp_hind_df$month)
  
  # convert to a shapefile
  sm_temp_hind_df_sf <- sm_temp_hind_df %>%
  	st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326)
	
	#### load map ####
	world_map_data <- ne_countries(scale = "medium", returnclass = "sf") 
	
	#### plotting function ####
  ggsave_func <- function(x,y){
  	ggsave(plot = x,
    file = paste(y,".png",sep=""),
    width = 10, height = 10, units = "in")
	}