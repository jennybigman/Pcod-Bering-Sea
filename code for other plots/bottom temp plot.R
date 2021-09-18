# bottom temp plot

### all years

bottom_temp_sum <- sm_temp_hind_df %>%
	group_by(latitude, longitude) %>%
	summarize(mean_temp = mean(temp))

bottom_temp_sum_sf <- bottom_temp_sum %>%
	 mutate(long_not_360 = longitude - 360) %>%
   st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326)


bt_plot <- ggplot() +
  geom_sf(data = sm_temp_hind_df_sf, aes(color = temp))  +
  geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
  coord_sf(crs = 3338) +
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
	labs(colour = "Temperature (˚C)") +
  theme_bw() +
  theme(
  	axis.text = element_text(size = 12),	
  	axis.title = element_text(size = 14),
  	legend.title.align=0.5)


	ggsave("./output/plots/bt_sum_plot.png",
		bt_plot,
		width = 10, height = 10, units = "in")
	
	
		year_bins  <- list('1970_1980' = c(1970:1980),
                     '1980_1990' = c(1980:1990),
                     '1990_2000' = c(1990:2000),
                     '2000_2010' = c(2000:2010),
                     '2010_2020' = c(2010:2020)) 

	# create a list of dfs summarized by year bins
	sum_yr_bin <- function(x){
 
		df <- sm_temp_hind_df %>%
		filter(year %in% x) %>%
		group_by(longitude, latitude) %>%
		summarise(mean_hs = mean(hatch_success_cauchy),
							mean_temp = mean(temp),
							mean_sphabsuit = mean(sp_hab_suit)) 
		
		df
	
	}

	df_list <- lapply(year_bins, sum_yr_bin)

	# add a column of the year bin
	df_list_tp <- mapply(cbind, df_list, "time_period"= names(year_bins), SIMPLIFY = FALSE)

	# bind all rows of all dfs together and add a column for longitude not on 360 scale
	sm_temp_hind_df_yr_sum <- bind_rows(df_list_tp) %>% 
  	dplyr::select(latitude, longitude, mean_sphabsuit, mean_temp, time_period)
 
	sm_temp_hind_df_yr_sum_sf <- sm_temp_hind_df_yr_sum %>%
	 mutate(long_not_360 = longitude - 360) %>%
   st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326)

	bt_plot <- ggplot() +
  geom_sf(data = sm_temp_hind_df_yr_sum_sf, aes(color = mean_temp))  +
  geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
  coord_sf(crs = 3338) +
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
	labs(colour = "Temperature\n(˚C)") +
  theme_bw() +
  theme(
  	axis.text = element_text(size = 10),	
  	axis.title = element_text(size = 12),
  	legend.title.align=0.5)


	ggsave("./output/plots/bt_sum_plot.png",
		bt_plot,
		width = 5, height = 5, units = "in")
	
	