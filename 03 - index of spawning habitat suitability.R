# estimate index of spawning habitat suitability

	library(data.table)
	library(lubridate)
	library(ggplot2)
	library(dplyr)

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
   
 yearly_hab_dat <- sm_temp_df %>%
   								 group_by(year) %>%
   								 summarise(annual_hatch_success_cauchy = mean(hatch_success_cauchy),
   								 					 annual_hatch_success_gaussian = mean(hatch_success_gaus))
   
 monthly_hab_dat <- sm_temp_df %>%
   									group_by(month, year) %>%
   									summarise(monthly_hatch_success_cauchy = mean(hatch_success_cauchy),
   									 					monthly_hatch_success_gaussian = mean(hatch_success_gaus))
   
## plot annual index of spawning habitat suitability
   
	annual_hatch_success <-    
		ggplot(data = yearly_hab_dat) +
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
   
	# remove 2021
  yearly_hab_dat_end2020 <- yearly_hab_dat %>%
   													filter(year != "2021")
   
   
   annual_hatch_success_2020 <-    ggplot(data = yearly_hab_dat_end2020) +
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
   
annual_hatch_success_2020 <- annual_hatch_success_2020 +
	annotate(geom = "text", x = 2022, y = 0.2,
           label = "Gaussian",
           color = "#000000", size = 4) +
	annotate(geom = "text", x = 2022, y = 0.18,
           label = "Cauchy",
           color = "#000000", size = 4)


ggsave("./Pcod-Bering-Sea/output/plots/annual_hatch_success_2020.png",
			 annual_hatch_success_2020,
			 width = 10, height = 7, units = "in")

## just cauchy

   annual_hatch_success_2020_cauchy <-    
   	ggplot(data = yearly_hab_dat_end2020) +
   	geom_line(aes(x = year, y = annual_hatch_success_cauchy), color = "black", size = 1) +
   	geom_point(aes(x = year, y = annual_hatch_success_cauchy), color = "black", size  = 4) +
   	xlab("Year") + 
	  scale_y_continuous(
	  	name = "Hatch success probability",
	  	breaks = c(0.18, 0.19, 0.20),
	  ) +
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

	ggplot_build(annual_hatch_success_2020_cauchy)$layout$panel_scales_y[[1]]$range$range



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
	
