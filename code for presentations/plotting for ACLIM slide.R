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

	##### older code #####
	
	# estimate index of spawning habitat suitability

	library(data.table)
	library(lubridate)
	library(ggplot2)
	library(dplyr)
	library(patchwork)

	setwd("~/Google Drive/NOAA AFSC Postdoc/Pcod Bering Sea Habitat Suitability")
	temp_df <- fread("./data/ROMS_all_temp.csv")

# restrict dataset to only those months of spawning (January to June)

	sp_months <- c(1:6)
	
	temp_df$date <- as.Date(temp_df$DateTime) # date in Date format
	temp_df$month <- month(temp_df$date) # month of year
	temp_df$week <- week(temp_df$date) # week of year
	temp_df$year <- year(temp_df$date)
	
	sm_temp_df <- temp_df %>% filter(month %in% sp_months)
	

# calculate spawning habitat suitability

	hatch_success_cauchy_func <- function(x, k = 0.453, mu = 4.192, sigma = 2.125 ){
  			 		(k / (1 + (((x - mu)/sigma))^2)) 
	}

	hatch_success_gaus_func <- function(x, k = 0.395, mu = 4.50, sigma = 2.58){
        	k * exp(-1/2 * (x - mu)^2/sigma^2)
	} 
    
	sm_temp_df <- sm_temp_df %>%
					     mutate(hatch_success_cauchy = sapply(temp, hatch_success_cauchy_func),
					            hatch_success_gaus = sapply(temp, hatch_success_gaus_func))
 
	sm_temp_df <- sm_temp_df %>%
 	 filter(between(Lon, 180, 202)) %>%
 	 filter(Lat >= 53)
 
	sm_temp_df <- sm_temp_df %>%
			rename(latitude = Lat,
			   		 longitude = Lon)
		
 ## plot annual index of spawning habitat suitability
  
	yearly_hab_dat <- sm_temp_df_depth %>%
   								  group_by(year) %>%
   								  summarise(annual_hatch_success_cauchy = mean(hatch_success_cauchy),
   								 					  annual_hatch_success_gaussian = mean(hatch_success_gaus))
	
	yearly_hab_dat_2020 <- yearly_hab_dat %>% filter(., year != "2021")

   annual_hatch_success_2020_cauchy <-    
   	ggplot(data = yearly_hab_dat_2020) +
   	geom_line(aes(x = year, y = annual_hatch_success_cauchy), color = "black", size = 1) +
   	geom_point(aes(x = year, y = annual_hatch_success_cauchy), color = "black", size  = 4) +
   	xlab("Year") + 
	  scale_y_continuous(
	  	name = "Hatch success\nprobability",
	  	breaks = c(0.12, 0.16, 0.20),
	  	limits = c(0.114, 0.215)
	  ) +
   	xlim(1970, 2020) +
   	theme_bw() +
  	theme(legend.position = "none") +
  	theme(
  	  axis.text=element_text(size=18, colour = "grey50"),
  	  axis.ticks = element_line(colour = "grey50"),
  	  axis.line = element_line(colour = "grey50"),
  	  axis.text.x = element_text(size = 16),
  	  axis.title= element_text(size=18, color = "grey30"),
  	  panel.grid.major = element_blank(),
  	  panel.grid.minor = element_blank(),
  	  panel.border = element_rect(fill = NA, color = "grey50"))
   
   annual_hatch_success_2020_cauchy_txt <- annual_hatch_success_2020_cauchy +
		annotate(geom = "text", x = 1982, y = 0.215,
           label = "Average annual hatch success probability",
           color = "#000000", size = 6)
 
	ggsave("./Pcod-Bering-Sea/output/plots/annual_hatch_success_2020_cauchy.png",
			 annual_hatch_success_2020_cauchy_txt,
			 width = 10, height = 3, units = "in", dpi = 600)

###############	
# maps ####
###############
	
	temp_2008 <- sm_temp_df_depth %>% filter(., year == 2008)
	temp_2008_rm <- temp_2008 %>% filter(., longitude > 180.1) %>%
		filter(., latitude > 53)
	temp_2008_sf <- createSF_points(temp_2008_rm)
	
	temp_2016 <- sm_temp_df_depth %>% filter(., year == 2016)
	temp_2016_rm <- temp_2016 %>% filter(., longitude > 180.1) %>%
		filter(., latitude > 53)

	temp_2016_sf <- createSF_points(temp_2016_rm)

# maps for col vs warm years

	hs_2008_plot_noleg <- 
		ggplot() +
  	geom_sf(data = temp_2008_sf, aes(color = hatch_success_cauchy)) +
  	geom_sf(data=st_transform(bering_sf,crs=crs_bering),fill="lightgrey", 
  		color="black",lwd=0.25) +
    xlab("Longitude") + 
		ylab("Latitude") +
  	coord_sf(crs = crs_bering) +
  	scale_color_viridis_c() +
  		scale_x_continuous(
   		breaks = c(180, -170, -160, -150),
   		labels = c("180˚", "-170˚", "-160˚", "-150˚"),
   		limits = c(-3, 1500000)) +
		ylim(-4000000, -1974305) +
   	labs(colour = "Hatch success\nprobability") +
   	theme_bw() +
  	theme(
  		legend.position = "none",
  		strip.background = element_rect(colour="white", fill="white"),
    	panel.border = element_rect(colour = "black"),
  		strip.text = element_text(size = 12, face = "bold"),
  		axis.text = element_text(size = 12),	
  		axis.title = element_text(size = 14),
  		panel.spacing.x=unit(0, "lines"),
  		legend.title.align=0.5)
	
	#ggplot_build(hs_2008_plot_noleg)$layout$panel_scales_y[[1]]$range$range

	#	ggsave("./Pcod-Bering-Sea/output/plots/hs_2008_plot_noleg.png",
	#		 hs_2008_plot_noleg,
	#		 width = 10, height = 7, units = "in")

	hs_2016_plot_noy <- 
		ggplot() +
  	geom_sf(data = temp_2016_sf, aes(color = hatch_success_cauchy)) +
  	geom_sf(data=st_transform(bering_sf,crs=crs_bering),fill="lightgrey", 
  		color="black",lwd=0.25) +
    xlab("Longitude") + 
		ylab("Latitude") +
  	coord_sf(crs = crs_bering) +
  	scale_color_viridis_c() +
  		scale_x_continuous(
   		breaks = c(180, -170, -160, -150),
   		labels = c("180˚", "-170˚", "-160˚", "-150˚"),
   		limits = c(-3, 1500000)) +
		ylim(-4000000, -1974305) +
   	labs(colour = "Hatch\nsuccess\nprobability") +
   	theme_bw() +
  	theme(legend.title = element_text(size = 12),
  				legend.text = element_text(size = 12),
  		axis.text.x = element_text(size = 12),		
  		legend.position = "right",
    	panel.border = element_rect(colour = "black"),
  		legend.title.align=0.5,
  		axis.text.y = element_blank(),
    	axis.ticks.y = element_blank(),
    	axis.title.y = element_blank(),
  		axis.title.x = element_text(size = 14))
  		
		#ggplot_build(hs_2016_plot_noy)$layout$panel_scales_y[[1]]$range$range

   plot1 <- hs_2008_plot_noleg + 
   	        theme(plot.margin = unit(c(0.2, 0, 0.2, 0.2), "in")) +
   	        ggtitle("2008: Cold Year") +
   	        theme(plot.title = element_text(size = 14, hjust = 0.5,  face = "bold"))
   
   plot2 <- hs_2016_plot_noy + 
   	        theme(plot.margin = unit(c(0.2, 0.2, 0.2, -0.05), "in")) +
   	        ggtitle("2016: Warm Year") +
   	        theme(plot.title = element_text(size = 14, hjust = 0.5,  face = "bold"))

	 warm_v_cold_hs <- plot1 + plot2 
	 
  	ggsave("./Pcod-Bering-Sea/output/plots/warm_v_cold_hs.png",
			 warm_v_cold_hs,
			 width = 10, height = 7, units = "in")
		
  		
	
	

##############
	# CODE FOR OTHER PLOTS
##############
	
	
		annual_hatch_success <-    
		ggplot(data = yearly_hab_dat_2020) +
   	geom_line(aes(x = year, y = annual_hatch_success_cauchy), color = "black") +
   	geom_point(aes(x = year, y = annual_hatch_success_cauchy), color = "black") +
   	geom_line(aes(x = year, y = annual_hatch_success_gaussian), color = "grey") +
   	geom_point(aes(x = year, y = annual_hatch_success_gaussian), color = "grey") +
   		xlab("Year") + 
	  scale_y_continuous(
	  	name = "Mean proportion hatch success",
	  	breaks = c(0.18, 0.20, 0.22),
	  	labels = c(0.18, 0.20, 0.22)
	  ) +
		xlim(1970, 2022) +
   	theme_bw() +
  	theme(legend.position = "none") +
  	theme(
  	  axis.text=element_text(size=14, colour = "grey50"),
  	  axis.ticks = element_line(colour = "grey50"),
  	  axis.line = element_line(colour = "grey50"),
  	  axis.text.x = element_text(size = 14),
  	  axis.title= element_text(size=18, color = "grey30"),
  	  panel.grid.major = element_blank(),
  	  panel.grid.minor = element_blank(),
  	  panel.border = element_rect(fill = NA, color = "grey50"))

		annual_hatch_success <- 
			annual_hatch_success +
			annotate(geom = "text", x = 2022, y = 0.2,
		           label = "Gaussian",
		           color = "#000000", size = 4) +
			annotate(geom = "text", x = 2022, y = 0.18,
		           label = "Cauchy",
		           color = "#000000", size = 4)
							
	ggsave("./Pcod-Bering-Sea/output/plots/annual_hatch_success.png",
				 annual_hatch_success,
				 width = 10, height = 7, units = "in")

   #### maps by yearly bins ####

  time_seg   <- list( '1970-1980' = c(1970:1980),
                      '1980-1990' = c(1980:1990),
                      '1990-2000' = c(1990:2000),
                      '2000-2010' = c(2000:2010),
                      '2010-2020' = c(2010:2020)) 

	group_tp <- function(x){
 
		df <- sm_temp_df %>%
		filter(year %in% x) %>%
		group_by(Lat, Lon) %>%
		summarise(mean_hs = mean(hatch_success_cauchy),
							mean_temp = mean(temp)) 
		df
	
	}

	df_list <- lapply(time_seg, group_tp)

	df_list_tp <- mapply(cbind, df_list, "time_period"= names(time_seg), SIMPLIFY = FALSE)

  temp_sum <- bind_rows(df_list_tp) %>%
						  rename(latitude = Lat,
									   longitude = Lon) %>%
						  filter(longitude > 180)
  
	temp_sum_sf <- createSF_points(temp_sum)
	
	sf_obj <- st_transform(bering_sf,crs=crs_bering)

	hs_time_per <- ggplot() +
  						 geom_sf(data = temp_sum_sf, aes(color = mean_hs)) +
  						 geom_sf(data=st_transform(bering_sf,crs=crs_bering),fill="lightgrey", 
  										 color="black",lwd=0.25) +
  						 facet_wrap(~ time_period, ncol = 5) +
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
	
	# if want to put legend in a panel
	#hs_time_per_leg <-  reposition_legend(hs_time_per, "bottom right", panel = 'panel-2-3') +
	#	theme(legend.title.align = 0.5)
	
	#ggplot_build(hs_time_per)$layout$panel_scales_x[[1]]$range$range

 	
	ggsave("./Pcod-Bering-Sea/output/plots/hs_time_per.png",
			 hs_time_per,
			 width = 10, height = 7, units = "in")
	

##########################
	
	# summary dataset to practice with
	
	sm_temp_df_sum <- sm_temp_df %>%
		group_by(latitude, longitude) %>%
    summarise(mean_temp = mean(temp),
    					mean_hs = mean(hatch_success_cauchy)) 
	
	sm_temp_df_sum_sf <- createSF_points(sm_temp_df_sum)
	
	sm_albers <- st_transform(sm_temp_df_sum_sf, crs = 3338)
	
	sm_4326 <- st_transform(sm_temp_df_sum_sf, crs = 4326)

	
pplot <-
	ggplot()+
  geom_sf(data=BASE %>% st_shift_longitude(), fill="gray90") +
	geom_point(data = sm_temp_df_sum, aes(x = longitude, y = latitude, 
																		color = mean_hs)) +
	xlab("Longitude") + 
	ylab("Latitude") +
	scale_color_viridis_c() +
  scale_x_continuous(
   						 breaks = c(180, 200, 210),
   						 labels = c(180, 200, 210)) +
							 scale_y_continuous(
							 	breaks = c(50, 60, 70)
							 ) +
   						 labs(colour = "Hatch success\nprobability") +
   						 theme_bw() +
  						 theme(legend.title = element_text(size = 10),
  						 			 legend.position = "right",
  						 			 strip.background = element_rect(colour="white", fill="white"),
      						   panel.border = element_rect(colour = "black"),
  						 			 strip.text = element_text(size = 12, face = "bold"),
  						 			 panel.spacing.x=unit(0, "lines"),
  						 		   legend.title.align=0.5)

hs_summ_albers <- 
	ggplot() +
	geom_sf(data=BASE, fill="gray90")+
	geom_sf(data = sm_albers, aes(color = mean_hs)) +
	coord_sf(crs=3338) +
	scale_color_viridis_c() +
	scale_x_continuous(
  	breaks = c(180, 190, 200),
  	labels = c("180˚", "170˚W", "160˚W"),
  	limits = c(-1800000, 100000)
  	) +
	scale_y_continuous(
		breaks = c(55, 60, 65),
		limits = c(450000, 2374440)
		) +
  labs(colour = "Hatch success\nprobability") +
  theme_bw() +
  theme(legend.title = element_text(size = 10),
  			legend.position = "right",
  			strip.background = element_rect(colour="white", fill="white"),
      	panel.border = element_rect(colour = "black"),
  			strip.text = element_text(size = 12, face = "bold"),
  			panel.spacing.x=unit(0, "lines"),
  			legend.title.align=0.5)

#	ggplot_build(pplot)$layout$panel_scales_y[[1]]$range$range

	
	ggsave("./Pcod-Bering-Sea/output/plots/hs_summ_albersr.png",
			 hs_summ_albers,
			 width = 10, height = 7, units = "in")
	

######### THIS WORKS ##########
# set map limits
lons = c(178, 202)
lats = c(53, 67)

	ggplot() +
				geom_polygon(data = reg, aes(x = long, y = lat, group = group), 
               fill = "darkgrey", color = NA) +
				#geom_sf(data = sm_temp_df_sum_sf, aes(color = mean_hs)) +
			  geom_point(data = sm_4326, aes(x = longitude, y = latitude, 
					color = mean_hs)) +
				coord_sf(xlim = lons, ylim = lats,
								 crs = 4326) +
			 scale_color_viridis_c() 
	
		ggplot() +
				geom_polygon(data = reg, aes(x = long, y = lat, group = group), 
               fill = "darkgrey", color = NA) +
				#geom_sf(data = sm_temp_df_sum_sf, aes(color = mean_hs)) +
			  geom_point(data = sm_albers, aes(x = longitude, y = latitude, 
					color = mean_hs)) +
				coord_sf(xlim = lons, ylim = lats,
								 crs = 3338) +
			 scale_color_viridis_c() 


				