# mapping years for which spawning habitat suitability >= 0.5

# remove 2021 because incomplete data
sm_temp_hind_df_current <- sm_temp_hind_df %>% filter(., year != 2020)
sm_temp_hind_df_current <- sm_temp_hind_df_current %>% filter(., year != 2021)

# number of years
no_yrs <- length(unique(sm_temp_hind_df_current$year))

# annual average
sm_temp_hind_df_current_yrsum <- sm_temp_hind_df_current %>%
	group_by(latitude, longitude, year) %>%
	summarise(mean_sphabsuit = mean(sp_hab_suit))

sm_temp_hind_df_current_yrsum <- sm_temp_hind_df_current_yrsum %>%
	mutate(decade = case_when(
		between(year, 1970, 1979) ~ "1970s",
		between(year, 1980, 1989) ~ "1980s",
		between(year, 1990, 1999) ~ "1990s",
		between(year, 2000, 2009) ~ "2000s",
		between(year, 2010, 2019) ~ "2010s"))


sm_temp_hind_df_current_yrsum_sum <- sm_temp_hind_df_current_yrsum %>%
	group_by(latitude, longitude, decade) %>%
	summarize(no_yrs = length(which(mean_sphabsuit >= 0.5))) %>%
	mutate(year_tot = 10,
				 pct_yrs = (no_yrs/year_tot) * 100)

df_sf <- sm_temp_hind_df_current_yrsum_sum %>%
		mutate(long_not_360 = longitude - 360) %>%
  	st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326)
  

plot <- 
  		ggplot() +
					geom_sf(data = df_sf, aes(color = pct_yrs))  +
					geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
					coord_sf(crs = 3338) +
  		    facet_wrap(~ decade, nrow = 1) +
					scale_color_viridis_c() +
 					scale_x_continuous(
 						breaks = c(-175, -170, -165, -160),
 						labels = c("-175˚", "-170˚", "-165˚", "-160˚"),
 						name = "Longitude",
 						limits = c(-1400000, -150000)
 					) +
 					scale_y_continuous(
 						breaks = c(55, 60),
 						limits = c(470000, 1900000),
 						name = "Latitude",
 					) +
    	  	labs(colour = "%\nyears") +
					ggtitle(label = paste("Percent of years for which spawning habitat suitability", 
																symbol, "0.5", sep =" ")) +
					theme_bw() +
 					theme(
 						strip.text = element_text(size = 14, face = "bold"),
 						strip.background = element_blank(),
 						axis.text = element_text(size = 12),	
  					axis.title = element_text(size = 14),
  					legend.title = element_text(hjust = 0.5, size = 12),
 						plot.title = element_text(size = 18, face = "bold")
 						)
  	

  	ggsave("./output/plots/percent_years.png",
		plot,
		width = 15, height = 10, units = "in")
	
## monthly average
  	
sm_temp_hind_df_current_month_sum <- sm_temp_hind_df_current %>%
	group_by(latitude, longitude, year, month_name) %>%
	summarise(mean_sphabsuit = mean(sp_hab_suit))

sm_temp_hind_df_current_month_sum <- sm_temp_hind_df_current_month_sum %>%
	mutate(decade = case_when(
		between(year, 1970, 1979) ~ "1970s",
		between(year, 1980, 1989) ~ "1980s",
		between(year, 1990, 1999) ~ "1990s",
		between(year, 2000, 2009) ~ "2000s",
		between(year, 2010, 2019) ~ "2010s"))


sm_temp_hind_df_current_month_sum <- sm_temp_hind_df_current_month_sum %>%
	group_by(latitude, longitude, month_name, decade) %>%
	summarize(no_yrs = length(which(mean_sphabsuit >= 0.5))) %>%
	mutate(year_tot = 10,
				 pct_yrs = (no_yrs/year_tot) * 100)

df_sf <- sm_temp_hind_df_current_month_sum %>%
		mutate(long_not_360 = longitude - 360) %>%
  	st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326)
  

plot <- 
  		ggplot() +
					geom_sf(data = df_sf, aes(color = pct_yrs))  +
					geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
					coord_sf(crs = 3338) +
  		    facet_grid(decade ~ month_name) +
					scale_color_viridis_c() +
 					scale_x_continuous(
 						breaks = c(-175, -170, -165, -160),
 						labels = c("-175˚", "-170˚", "-165˚", "-160˚"),
 						name = "Longitude",
 						limits = c(-1400000, -150000)
 					) +
 					scale_y_continuous(
 						breaks = c(55, 60),
 						limits = c(470000, 1900000),
 						name = "Latitude",
 					) +
    	  	labs(colour = "%\nyears") +
					ggtitle(label = paste("Percent of years for which spawning habitat suitability", 
																symbol, "0.5", sep =" ")) +
					theme_bw() +
 					theme(
 						strip.text = element_text(size = 14, face = "bold"),
 						strip.background = element_blank(),
 						axis.text = element_text(size = 12),	
  					axis.title = element_text(size = 14),
  					legend.title = element_text(hjust = 0.5, size = 12),
 						plot.title = element_text(size = 18, face = "bold")
 						)
  	

  	ggsave("./output/plots/percent_years_months.png",
		plot,
		width = 15, height = 10, units = "in")
	
