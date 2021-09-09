# work on removing points not within a certain polygon and > 250 m depth

	library(data.table)
	library(lubridate)
	library(ggplot2)
	library(dplyr)
	library(marmap)

	setwd("~/Google Drive/NOAA AFSC Postdoc/Pcod Bering Sea Habitat Suitability")
	temp_df <- fread("./data/ROMS_all_temp.csv")

# restrict dataset to only those months of spawning (January to June)

	sp_months <- c(1:6)
	
	temp_df$date <- as.Date(temp_df$DateTime) # date in Date format
	temp_df$month <- month(temp_df$date) # month of year
	temp_df$week <- week(temp_df$date) # week of year
	temp_df$year <- year(temp_df$date)
	
	sm_temp_df <- temp_df %>% filter(month %in% sp_months)
	
	
	hatch_success_cauchy_func <- function(x, k = 0.453, mu = 4.192, sigma = 2.125 ){
  			 		(k / (1 + (((x - mu)/sigma))^2)) 
	}

	hatch_success_gaus_func <- function(x, k = 0.395, mu = 4.50, sigma = 2.58){
        	k * exp(-1/2 * (x - mu)^2/sigma^2)
	} 
    
	sm_temp_df <- sm_temp_df %>%
		mutate(hatch_success_cauchy = sapply(temp, hatch_success_cauchy_func),
					 hatch_success_gaus = sapply(temp, hatch_success_gaus_func))
 
#	sm_temp_df <- sm_temp_df %>%
#		filter(between(Lon, 180, 202)) %>%
# 	  filter(Lat >= 53)

###############	 	
# remove points not on Bering Sea Shelf ####
###############
	
	
	#lons = c(-178, -158)
  #lats = c(56, 67)
  #
  #lon_coords <- c(-178, -158, -158, -178)
  #lat_coords <- c(67, 67, 56, 56)
  #
  #BS_coords <- cbind(lon_coords, lat_coords)
  #BS_polygon1 <- sp::Polygon(BS_coords)
	#BS_polygon2 <- Polygons(list(BS_polygon1), 1)
	#BS_polygon_sps <- SpatialPolygons(list(BS_polygon2))
	#plot(BS_polygon_sps)
	#
	#geoms <- sm_temp_df_250_sf$geom
	#
	#sf_dat <- over(geoms, BS_polygon_sps)
	
 
###############	 	
# restrict to depts < 250 m ####
###############

 #sm_temp_df$Lon <- round(sm_temp_df$Lon, 2)
 #sm_temp_df$Lat <- round(sm_temp_df$Lat, 2)

 max_lon <-  max(sm_temp_df$Lon) - 360
 min_lon <-  min(sm_temp_df$Lon) 
 
 max_lat <- max(sm_temp_df$Lat)
 min_lat <- min(sm_temp_df$Lat)
 
 b = getNOAA.bathy(lon1 = min_lon, lon2 = max_lon, lat1 = min_lat, lat2 = max_lat, 
                  resolution = 1, antimeridian = TRUE)

 
## convert bathymetry to data frame and create new df with lats/long < 250 m
#	bf = fortify.bathy(b)
#	
#	bf_250 <- bf %>% filter(between(z, 0, 250))
#
#	bf_250 <- bf_250 %>%
#		rename(Lat = y) %>%
#		mutate(Lon = x + 360)
#	
#	bf_250$Lat <- round(bf_250$Lat, 2)
 # bf_250$Lon <- round(bf_250$Lon, 2)
#
#	# filter df by those lats/longs with depths < 250
#	
#	sm_temp_df_250 <- sm_temp_df %>%
#		filter(., Lat %in% bf_250$Lat, Lon %in% bf_250$Lon)
#	
 # # merge
#	
#	sm_temp_df_250 <- merge(sm_temp_df_250, bf_250, by = c("Lat", "Lon"))
	
#	sm_temp_df_250 <- sm_temp_df_250 %>%
#		dplyr::select(., -Xi, -Eta) %>%
#		rename(latitude = Lat,
#					 longitude = Lon,
#					 depth = z)
#	
#	range(sm_temp_df_250$latitude)
#	range(sm_temp_df_250$x)
#	
#	sm_temp_df <- sm_temp_df %>% filter(between(x, -178, -158))
#	sm_temp_df <- sm_temp_df %>% filter(between(latitude, 56, 67))
#
	sm_temp_df <- sm_temp_df %>%
			rename(latitude = Lat,
			   		 longitude = Lon)
		
	sm_temp_df_sf <-	createSF_points(sm_temp_df)
	
	
	############### 
	# Maps ####
	###############
	
	# cold year: 2008
	
	sm_temp_2008 <- sm_temp_df_sf %>% filter(., year == 2008)
	
							 ggplot() +
  						 geom_sf(data = sm_temp_2008, aes(color = hatch_success_cauchy)) +
  						 geom_sf(data=st_transform(bering_sf,crs=crs_bering),fill="lightgrey", 
  										 color="black",lwd=0.25) +
               xlab("Longitude") + ylab("Latitude") +
  						 coord_sf(crs = crs_bering) +
  					   scale_x_continuous(
   						 breaks = c(180, -170, -160),
   						 labels = c("180˚", "-170˚", "-160˚"),
   						 limits = c(-4, 1986042)) +
							 scale_y_continuous(
							 	breaks = c(50, 60, 70)
							 ) +
  						 scale_color_viridis_c() +
   						 labs(colour = "Hatch success\nprobability") +
   						 theme_bw() +
  						 theme(legend.title = element_text(size = 10),
  						 			 legend.position = "right",
  						 			 strip.background = element_rect(colour="white", fill="white"),
      						   panel.border = element_rect(colour = "black"),
  						 			 strip.text = element_text(size = 12, face = "bold"),
  						 			 panel.spacing.x=unit(0, "lines"),
  						 		   legend.title.align=0.5)
	
	
	############### 
	# Time Series plot ####
	###############
	
	#for time series, remove 2021 data because incomplete 
	
	sm_temp_df_sf <- sm_temp_df_sf %>% filter(year != "2021")
	
	yearly_hab_dat <- sm_temp_df_sf %>%
   								 group_by(year) %>%
   								 summarise(annual_hatch_success_cauchy = mean(hatch_success_cauchy),
   								 					 annual_hatch_success_gaussian = mean(hatch_success_gaus))
 
	 annual_hatch_success_2020_cauchy <-    
   	ggplot(data = yearly_hab_dat) +
   	geom_line(aes(x = year, y = annual_hatch_success_cauchy), color = "black", size = 1) +
   	geom_point(aes(x = year, y = annual_hatch_success_cauchy), color = "black", size  = 4) +
   	xlab("Year") + 
	  #scale_y_continuous(
	  #	name = "Hatch success probability",
	  #	breaks = c(0.18, 0.19, 0.20),
	  #) +
   	xlim(1970, 2022) +
   	theme_bw() +
  	theme(legend.position = "none") +
  	theme(
  	  axis.text=element_text(size=20, colour = "grey50"),
  	  axis.ticks = element_line(colour = "grey50"),
  	  axis.line = element_line(colour = "grey50"),
  	  axis.text.x = element_text(size = 18),
  	  axis.title= element_text(size=20, color = "grey30"),
  	  panel.grid.major = element_blank(),
  	  panel.grid.minor = element_blank(),
  	  panel.border = element_rect(fill = NA, color = "grey50"))
   
   annual_hatch_success_2020_cauchy_txt <- annual_hatch_success_2020_cauchy +
		annotate(geom = "text", x = 1982, y = 0.2058091,
           label = "Average annual hatch success probability",
           color = "#000000", size = 6)
 
	ggsave("./Pcod-Bering-Sea/output/plots/annual_hatch_success_2020_cauchy.png",
			 annual_hatch_success_2020_cauchy_txt,
			 width = 10, height = 7, units = "in", dpi = 600)
