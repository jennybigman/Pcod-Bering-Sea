
# prep data (summary)

ROMS_dat_hind_trim_sum <- ROMS_dat_hind_trim %>%
	group_by(latitude, long_not_360) %>%
	summarise(mean_temp = mean(temp),
						mean_sp_hab_suit = mean(sp_hab_suit))

ROMS_dat_hind_trim_sum_sf <- ROMS_dat_hind_trim_sum %>%
		st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326)

# just map

Bering_map <- ggplot() +
	geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
					coord_sf(crs = 3338) +
					scale_color_viridis_c() +
	scale_x_continuous(
 						breaks = c(180, -175, -170, -165, -160, -155),
 						labels = c("180˚", "-175˚", "-170˚", "-165˚", "-160˚", "-155˚"),
 						name = "Longitude",
 						limits = c(-1500000, -100),
 					) +
 	scale_y_continuous(
 						breaks = c(55, 60, 65, 70),
 						limits = c(450000, 2500000),
 						name = "Latitude",
 					) +
  	theme(legend.position = c()) +
  	theme(
  	  axis.text=element_text(size= 12, colour = "white"),
  	  axis.title= element_text(size=16, color = "white"),
  	  axis.line = element_line(color = "white"),
  	  axis.ticks = element_line(colour = "white"),
  	  legend.title = element_text(color = "white"),
  	  legend.text = element_text(color = "white"),
  	  panel.grid = element_line(color = "lightgrey"),
  	  panel.background = element_rect(fill = "black"),
  	  plot.background = element_rect(fill = "black", color = "black"))

ggsave(here("./Bering_map.png"),
			 Bering_map,
			 width = 10, height = 7, units = "in")


# gradient of color of ROMS temps over Bering Sea shelf

ROMS_temps <- ggplot() +
	geom_sf(data = ROMS_dat_hind_trim_sum_sf, aes(color = mean_temp)) +
	geom_sf(data = world_map_data, fill = "lightgrey", lwd = 0) +
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
    	  	labs(colour = "Bottom\ntemperature (˚C)") +
					theme(legend.position = c(),
								legend.background = element_rect(fill = "black"),
  						  axis.text=element_text(size= 12, colour = "white"),
  						  axis.title= element_text(size=16, color = "white"),
  						  axis.line = element_line(color = "white"),
  						  axis.ticks = element_line(colour = "white"),
  						  legend.title = element_text(color = "white"),
  						  legend.text = element_text(color = "white"),
  						  panel.background = element_rect(fill = "black"),
  						  plot.background = element_rect(fill = "black", color = "black"),
								legend.title.align = 0.5)

ROMS_temps_align <- ggdraw(align_legend(ROMS_temps))

ggsave(here("./ROMS_TEMPS_PLOT_BLACK.png"),
			 ROMS_temps_align,
			 width = 7, height = 7, units = "in")

### all ROMS spatial res

all_ROMS_sum <- temp_df %>%
		mutate(long_not_360 = case_when(
				longitude >= 180 ~ longitude - 360,
				longitude < 180 ~ longitude)) %>%
		group_by(latitude, long_not_360) %>%
		summarise(mean_temp = mean(temp))

all_ROMS_sum_sf <- all_ROMS_sum %>%
		st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326)


ROMS_temps_all <- ggplot() +
	geom_sf(data = all_ROMS_sum_sf, aes(color = mean_temp)) +
	geom_sf(data = world_map_data, fill = "lightgrey", lwd = 0) +
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
    	  	labs(colour = "Bottom\ntemperature (˚C)") +
					theme(legend.position = c(),
								legend.background = element_rect(fill = "black"),
  						  axis.text=element_text(size= 12, colour = "white"),
  						  axis.title= element_text(size=16, color = "white"),
  						  axis.line = element_line(color = "white"),
  						  axis.ticks = element_line(colour = "white"),
  						  legend.title = element_text(color = "white"),
  						  legend.text = element_text(color = "white"),
  						  panel.background = element_rect(fill = "black"),
  						  plot.background = element_rect(fill = "black", color = "black"),
								legend.title.align = 0.5)

ROMS_temps_all_align <- ggdraw(align_legend(ROMS_temps_all))

ggsave(here("./ROMS_temps_all_align"),
			 ROMS_temps_all_align,
			 width = 7, height = 7, units = "in")

# same but all black for area slide

ROMS_temps_white <- ggplot() +
	geom_sf(data = ROMS_dat_hind_trim_sum_sf, aes(color = mean_temp)) +
	geom_sf(data = world_map_data, fill = "lightgrey", lwd = 0) +
					coord_sf(crs = 3338) +
	scale_color_gradient(low = "white", high = "white") +
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
	theme(legend.position = "none") +
  black_map_theme_no_legend()

ggsave(here("./ROMS_TEMPS_area.png"),
			 ROMS_temps_white,
			 width = 7, height = 7, units = "in")


### trying to plot the cold pool

# early subset

subset_yrs_82_86 <- c(1981:1986)

yr_sub<- ROMS_dat_hind_trim %>%
	group_by(year, latitude, long_not_360) %>%
	summarise(mean_temp = mean(temp),
						mean_sp_hab_suit = mean(sp_hab_suit))

subset_82_82 <- yr_sub %>%
		filter(year %in% subset_yrs_82_86) %>%
		group_by(latitude, long_not_360) %>%
		summarise(mean_temp = mean(mean_temp))  %>%
		st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326)

mid = 1

cold_pool_8286 <- ggplot() +
	geom_sf(data = subset_82_82, aes(color = mean_temp)) +
	geom_sf(data = world_map_data, fill = "lightgrey", lwd = 0) +
	coord_sf(crs = 3338) +
	scale_color_gradient2(low = "red", mid = "limegreen", high = "yellow") +
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
    	  	labs(colour = "Bottom\ntemperature (˚C)") +
					theme(legend.position = c(),
								legend.background = element_rect(fill = "black"),
  						  axis.text=element_text(size= 12, colour = "white"),
  						  axis.title= element_text(size=16, color = "white"),
  						  axis.line = element_line(color = "white"),
  						  axis.ticks = element_line(colour = "white"),
  						  legend.title = element_text(color = "white"),
  						  legend.text = element_text(color = "white"),
  						  panel.background = element_rect(fill = "black"),
  						  plot.background = element_rect(fill = "black", color = "black"),
								legend.title.align = 0.5)

ggsave(here("./cold_pool_8286.png"),
			 cold_pool_8286,
			 width = 7, height = 7, units = "in")



# late subset

subset_yrs_02_06 <- c(2002:2006)

yr_sub<- ROMS_dat_hind_trim %>%
	group_by(year, latitude, long_not_360) %>%
	summarise(mean_temp = mean(temp),
						mean_sp_hab_suit = mean(sp_hab_suit))

subset_02_06 <- yr_sub %>%
		filter(year %in% subset_yrs_02_06) %>%
		group_by(latitude, long_not_360) %>%
		summarise(mean_temp = mean(mean_temp))  %>%
		st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326)

mid = 2

cold_pool_0206 <- ggplot() +
	geom_sf(data = subset_02_06, aes(color = mean_temp)) +
	geom_sf(data = world_map_data, fill = "lightgrey", lwd = 0) +
	coord_sf(crs = 3338) +
	scale_color_gradient2(low = "red", mid = "limegreen", high = "yellow") +
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
    	  	labs(colour = "Bottom\ntemperature (˚C)") +
					theme(legend.position = c(),
								legend.background = element_rect(fill = "black"),
  						  axis.text=element_text(size= 12, colour = "white"),
  						  axis.title= element_text(size=16, color = "white"),
  						  axis.line = element_line(color = "white"),
  						  axis.ticks = element_line(colour = "white"),
  						  legend.title = element_text(color = "white"),
  						  legend.text = element_text(color = "white"),
  						  panel.background = element_rect(fill = "black"),
  						  plot.background = element_rect(fill = "black", color = "black"),
								legend.title.align = 0.5)

ggsave(here("./cold_pool_0206.png"),
			 cold_pool_0206,
			 width = 7, height = 7, units = "in")

### cold vs warm year

# cold


yr_sub<- ROMS_dat_hind_trim %>%
	group_by(year, latitude, long_not_360) %>%
	summarise(mean_temp = mean(temp),
						mean_sp_hab_suit = mean(sp_hab_suit))

subset_2008 <- yr_sub %>%
		filter(year == 2008) %>%
		group_by(latitude, long_not_360) %>%
		summarise(mean_temp = mean(mean_temp))  %>%
		st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326)

mid = 2

cold_pool_2008 <- ggplot() +
	geom_sf(data = subset_2008, aes(color = mean_temp)) +
	geom_sf(data = world_map_data, fill = "lightgrey", lwd = 0) +
	coord_sf(crs = 3338) +
	scale_color_gradient2(low = "white", mid = "limegreen", high = "yellow") +
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
    	  	labs(colour = "Bottom\ntemperature (˚C)") +
					theme(legend.position = c(),
								legend.background = element_rect(fill = "black"),
  						  axis.text=element_text(size= 18, colour = "white"),
  						  axis.title= element_text(size=20, color = "white"),
  						  axis.line = element_line(color = "white"),
  						  axis.ticks = element_line(colour = "white"),
  						  legend.title = element_text(color = "white", size = 20),
  						  legend.text = element_text(color = "white", size = 18),
  						  panel.background = element_rect(fill = "black"),
  						  plot.background = element_rect(fill = "black", color = "black"),
								legend.title.align = 0.5)

ggsave(here("./cold_pool_2008.png"),
			 cold_pool_2008,
			 width = 7, height = 7, units = "in")

# warm year

subset_2016 <- yr_sub %>%
		filter(year == 2016) %>%
		group_by(latitude, long_not_360) %>%
		summarise(mean_temp = mean(mean_temp))  %>%
		st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326)

mid = 2

cold_pool_2016 <- ggplot() +
	geom_sf(data = subset_2016, aes(color = mean_temp)) +
	geom_sf(data = world_map_data, fill = "lightgrey", lwd = 0) +
	coord_sf(crs = 3338) +
	scale_color_gradient2(low = "white", mid = "limegreen", high = "yellow") +
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
    	  	labs(colour = "Bottom\ntemperature (˚C)") +
					theme(legend.position = c(),
								legend.background = element_rect(fill = "black"),
  						  axis.text=element_text(size= 18, colour = "white"),
  						  axis.title= element_text(size=20, color = "white"),
  						  axis.line = element_line(color = "white"),
  						  axis.ticks = element_line(colour = "white"),
  						  legend.title = element_text(color = "white", size = 20),
  						  legend.text = element_text(color = "white", size = 18),
  						  panel.background = element_rect(fill = "black"),
  						  plot.background = element_rect(fill = "black", color = "black"),
								legend.title.align = 0.5)

ggsave(here("./cold_pool_2016.png"),
			 cold_pool_2016,
			 width = 7, height = 7, units = "in")

#### with discrete scale

### cold vs warm year

# cold

yr_sub<- ROMS_dat_hind_trim %>%
	group_by(year, latitude, long_not_360) %>%
	summarise(mean_temp = mean(temp),
						mean_sp_hab_suit = mean(sp_hab_suit))

subset_2008 <- yr_sub %>%
		filter(year == 2008) %>%
		group_by(latitude, long_not_360) %>%
		summarise(mean_temp = mean(mean_temp))  %>%
		st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326)

mid = 2

cold_pool_2008 <- ggplot() +
	geom_sf(data = subset_82_82, aes(color = mean_temp)) +
	geom_sf(data = world_map_data, fill = "lightgrey", lwd = 0) +
	coord_sf(crs = 3338) +
	scale_color_gradientn(
				colors = c("red", "red",
								   "limegreen", "limegreen",
								   "#01579B", "#01579B"),
				values = c(0, 0.099, 1.0, 0.899, 0.9, 1),
				breaks = c(0.1, 0.5, 0.9),
				labels = format(c(0.1, 0.5, 0.9)),
				limits = c(0, 1))
	scale_color_gradient2(low = "red", mid = "limegreen", high = "yellow") +
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
    	  	labs(colour = "Bottom\ntemperature (˚C)") +
					theme(legend.position = c(),
								legend.background = element_rect(fill = "black"),
  						  axis.text=element_text(size= 12, colour = "white"),
  						  axis.title= element_text(size=16, color = "white"),
  						  axis.line = element_line(color = "white"),
  						  axis.ticks = element_line(colour = "white"),
  						  legend.title = element_text(color = "white"),
  						  legend.text = element_text(color = "white"),
  						  panel.background = element_rect(fill = "black"),
  						  plot.background = element_rect(fill = "black", color = "black"),
								legend.title.align = 0.5)

ggsave(here("./cold_pool_2008.png"),
			 cold_pool_2008,
			 width = 7, height = 7, units = "in")

# warm year

subset_2016 <- yr_sub %>%
		filter(year == 2016) %>%
		group_by(latitude, long_not_360) %>%
		summarise(mean_temp = mean(mean_temp))  %>%
		st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326)

mid = 2

cold_pool_2016 <- ggplot() +
	geom_sf(data = subset_2016, aes(color = mean_temp)) +
	geom_sf(data = world_map_data, fill = "lightgrey", lwd = 0) +
	coord_sf(crs = 3338) +
	scale_color_gradient2(low = "red", mid = "limegreen", high = "yellow") +
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
    	  	labs(colour = "Bottom\ntemperature (˚C)") +
					theme(legend.position = c(),
								legend.background = element_rect(fill = "black"),
  						  axis.text=element_text(size= 12, colour = "white"),
  						  axis.title= element_text(size=16, color = "white"),
  						  axis.line = element_line(color = "white"),
  						  axis.ticks = element_line(colour = "white"),
  						  legend.title = element_text(color = "white"),
  						  legend.text = element_text(color = "white"),
  						  panel.background = element_rect(fill = "black"),
  						  plot.background = element_rect(fill = "black", color = "black"),
								legend.title.align = 0.5)

ggsave(here("./cold_pool_2016.png"),
			 cold_pool_2016,
			 width = 7, height = 7, units = "in")


## core habitat

ROMS_core_hab <- ggplot() +
	geom_sf(data = ROMS_dat_hind_trim_sum_sf, aes(color = mean_sp_hab_suit)) +
	geom_sf(data = world_map_data, fill = "lightgrey", lwd = 0) +
	coord_sf(crs = 3338) +
		scale_color_gradientn(
			colors = c("#4d89b9", "#4d89b9",
								 "#01579B", "#01579B"),
			values = c(0, 0.899, 0.9, 1),
			breaks = c(0.1, 0.5, 0.9),
			labels = format(c(0.1, 0.5, 0.9)),
			limits = c(0, 1))  +
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
	theme(legend.position = "none") +
  black_map_theme_no_legend()

ggsave(here("./ROMS_core_hab.png"),
			 ROMS_core_hab,
			 width = 7, height = 7, units = "in")

## potential habitat
ROMS_pot_hab <- ggplot() +
	geom_sf(data = ROMS_dat_hind_trim_sum_sf, aes(color = mean_sp_hab_suit)) +
	geom_sf(data = world_map_data, fill = "lightgrey", lwd = 0) +
	coord_sf(crs = 3338) +
	scale_color_gradientn(
	colors = c("#b2cce1", "#b2cce1",
						 "#4d89b9", "#4d89b9",
						 "#01579B", "#01579B"),
		values = c(0, 0.499, 0.5, 0.899, 0.9, 1),
		breaks = c(0.1, 0.5, 0.9),
		labels = format(c(0.1, 0.5, 0.9)),
		limits = c(0, 1))	+
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
	theme(legend.position = "none") +
  black_map_theme_no_legend()

ggsave(here("./ROMS_pot_hab.png"),
			 ROMS_pot_hab,
			 width = 7, height = 7, units = "in")

ROMS_temps_align <- ggdraw(align_legend(ROMS_temps))

ggsave(here("./ROMS_TEMPS_PLOT_BLACK.png"),
			 ROMS_temps_align,
			 width = 7, height = 7, units = "in")


# for title slide

bering_sea <- ggplot() +
	geom_sf(data = world_map_data, fill = "black", lwd = 0) +
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
					theme_bw() 

ggsave(here("./bering_sea.png"),
			 bering_sea,
			 width = 7, height = 7, units = "in")


bering_sea <- ggplot() +
	geom_sf(data = world_map_data, fill = "black", lwd = 0) +
					coord_sf(crs = 3338) +
					scale_color_viridis_c() +
	scale_x_continuous(
 						breaks = c(180, -175, -170, -165, -160, -155),
 						labels = c("180˚", "-175˚", "-170˚", "-165˚", "-160˚", "-155˚"),
 						name = "Longitude",
 						limits = c(-1500000, -100)
 					) +
 	scale_y_continuous(
 						breaks = c(55, 60, 65, 70),
 						limits = c(450000, 2500000),
 						name = "Latitude",
 					) +
					theme_bw() +
	theme(
  	  axis.text=element_text(size= 12, colour = "black"),
  	  axis.title= element_text(size=16))

ggsave(here("./bering_sea.png"),
			 bering_sea,
			 width = 7, height = 7, units = "in")


bering_sea <- ggplot() +
	geom_sf(data = world_map_data, fill = "lightgrey", lwd = 0) +
					coord_sf(crs = 3338) +
					scale_color_viridis_c() +
	scale_x_continuous(
 						breaks = c(180, -175, -170, -165, -160, -155),
 						labels = c("180˚", "-175˚", "-170˚", "-165˚", "-160˚", "-155˚"),
 						name = "Longitude",
 						limits = c(-1500000, -100),
 					) +
 	scale_y_continuous(
 						breaks = c(55, 60, 65, 70),
 						limits = c(450000, 2500000),
 						name = "Latitude",
 					) +
  	theme(legend.position = c()) +
  	theme(
  	  axis.text=element_text(size= 12, colour = "white"),
  	  axis.title= element_text(size=16, color = "white"),
  	  axis.line = element_line(color = "white"),
  	  axis.ticks = element_line(colour = "white"),
  	  legend.title = element_text(color = "white"),
  	  legend.text = element_text(color = "white"),
  	  panel.background = element_rect(fill = "black"),
  	  plot.background = element_rect(fill = "black", color = "black"))

ggsave(here("./bering_sea.png"),
			 bering_sea,
			 width = 7, height = 7, units = "in")


### maps for prelim results based on temp

###############	
# maps ####
###############
	
	temp_2008_sf <- ROMS_dat_hind_trim_sf %>% filter(., year == 2008)
	
	temp_2016_sf <- ROMS_dat_hind_trim_sf %>% filter(., year == 2016)
	
	# maps for cold vs warm years

	hs_2008_plot_noleg <- 
		ggplot() +
  	geom_sf(data = temp_2008_sf, aes(color = sp_hab_suit)) +
  	geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
		coord_sf(crs = 3338) +
 		scale_x_continuous(
 			breaks = c(-175, -170, -165, -160),
 			labels = c("-175˚", "-170˚", "-165˚", "-160˚"),
 			name = "Longitude",
 			limits = c(-1400000, -150000)) +
 	  scale_y_continuous(
 			breaks = c(55, 60),
 			limits = c(470000, 1900000),
 			name = "Latitude") +
		scale_color_gradientn(
				colors = c("#b2cce1", "#b2cce1",
								   "#4d89b9", "#4d89b9",
								   "#01579B", "#01579B"),
				values = c(0, 0.499, 0.5, 0.899, 0.9, 1),
				breaks = c(0.1, 0.5, 0.9),
				labels = format(c(0.1, 0.5, 0.9)),
				limits = c(0, 1)) +
    labs(colour = "Spawning\nhabitat\nsuitability") +
		black_map_theme_no_legend()
  		
	hs_2016_plot_noy <- 
		ggplot() +
  	geom_sf(data = temp_2016_sf, aes(color = sp_hab_suit)) +
  	geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
		coord_sf(crs = 3338) +
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
		scale_color_gradientn(
				colors = c("#b2cce1", "#b2cce1",
								   "#4d89b9", "#4d89b9",
								   "#01579B", "#01579B"),
				values = c(0, 0.499, 0.5, 0.899, 0.9, 1),
				breaks = c(0.1, 0.5, 0.9),
				labels = format(c(0.1, 0.5, 0.9)),
				limits = c(0, 1)) +
    labs(colour = "Spawning\nhabitat\nsuitability") +
  	theme(legend.position = c(),
					legend.background = element_rect(fill = "black"),
  				axis.text.x =element_text(size= 12, colour = "white"),
  				axis.title.x = element_text(size=16, color = "white"),
  				axis.line = element_line(color = "white"),
  				axis.ticks.x = element_line(colour = "white"),
					axis.text.y = element_blank(),
					axis.title.y = element_blank(),
					axis.ticks.y = element_blank(),
  				legend.title = element_text(color = "white"),
  				legend.text = element_text(color = "white"),
  				panel.background = element_rect(fill = "black"),
  				plot.background = element_rect(fill = "black", color = "black"),
					legend.title.align = 0.5)
		
   plot1 <- hs_2008_plot_noleg + 
   	        theme(plot.margin = unit(c(0.2, 0, 0.2, 0.2), "in")) +
   	        ggtitle("2008: Cold Year") +
   	        theme(plot.title = element_text(size = 14, hjust = 0.5,  face = "bold", color = "white"))
   
   plot2 <- hs_2016_plot_noy + 
   	        theme(plot.margin = unit(c(0.2, 0.2, 0.2, -0.05), "in")) +
   	        ggtitle("2016: Warm Year") +
   	        theme(plot.title = element_text(size = 14, hjust = 0.5,  face = "bold", color = "white"))

	 warm_v_cold_hs <- plot1 + plot2 

	 
  	ggsave(here("./output/plots/warm_v_cold_hs_colorblocks_black.png"),
			 warm_v_cold_hs,
			 width = 10, height = 7, units = "in")
		
	### facet by month
  	
  	  	
  	## with color blocks
  	
  	# maps for cold vs warm years

	hs_2008_plot_noleg_mo <- 
		ggplot() +
  	geom_sf(data = temp_2008_sf, aes(color = sp_hab_suit)) +
  	geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
		coord_sf(crs = 3338) +
		facet_wrap(~ month_name, ncol = 3, nrow = 3) + 
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
		scale_color_gradientn(colors = c("#B3E5FC", "#B3E5FC", 
																			"#01579B", "#01579B",
																			"#00345C", "#00345C"),
												  values = c(0, 0.499, 0.5, 0.899, 0.9, 1),
												  breaks = c(0.1, 0.5, 0.9),
												  labels = format(c(0.1, 0.5, 0.9)),
												  limits = c(0, 1)) +
    labs(colour = "Spawning\nhabitat\nsuitability") +
  	theme(
  			strip.text = element_text(size = 14, face = "bold", color = "white"),
 				strip.background = element_blank(),
					legend.background = element_rect(fill = "black"),
  				axis.text=element_text(size= 12, colour = "white"),
  				axis.title= element_text(size=16, color = "white"),
  				axis.line = element_line(color = "white"),
  				axis.ticks = element_line(colour = "white"),
  				legend.title = element_text(color = "white"),
  				legend.text = element_text(color = "white"),
  				panel.background = element_rect(fill = "black"),
  			  panel.spacing.x=unit(0, "lines"),
  				plot.background = element_rect(fill = "black", color = "black"),
					legend.title.align = 0.5)

		ggsave(here("./output/plots/hs_2008_plot_noleg_mo.png"),
			 hs_2008_plot_noleg_mo,
			 width = 10, height = 7, units = "in")
		
		
	hs_2016_plot_noy <- 
		ggplot() +
  	geom_sf(data = temp_2016_sf, aes(color = sp_hab_suit)) +
  	geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
	  facet_wrap(~ month_name, ncol = 3, nrow = 3) + 
		coord_sf(crs = 3338) +
 		scale_x_continuous(
 			breaks = c(-175, -170, -165, -160),
 			labels = c("-175˚", "-170˚", "-165˚", "-160˚"),
 			name = "Longitude",
 			limits = c(-1400000, -150000)) +
 	  scale_y_continuous(
 			breaks = c(55, 60),
 			limits = c(470000, 1900000),
 			name = "Latitude") +
		scale_color_gradientn(
			colors = c("#B3E5FC", "#B3E5FC", 
									"#01579B", "#01579B",
									"#00345C", "#00345C"),
			values = c(0, 0.499, 0.5, 0.899, 0.9, 1),
			breaks = c(0.1, 0.5, 0.9),
			labels = format(c(0.1, 0.5, 0.9)),
			limits = c(0, 1)) +
    labs(colour = "Spawning\nhabitat\nsuitability") +
		theme_bw() +
 		theme(
  			strip.text = element_text(size = 14, face = "bold", color = "white"),
 				strip.background = element_blank(),
				legend.background = element_rect(fill = "black"),
  			axis.text.x =element_text(size= 12, colour = "white"),
  			axis.title.x= element_text(size=16, color = "white"),
  			axis.line.x = element_line(color = "white"),
  			axis.ticks.x = element_line(colour = "white"),
  			legend.title = element_text(color = "white"),
  			legend.text = element_text(color = "white"),
  			panel.background = element_rect(fill = "black"),
  			plot.background = element_rect(fill = "black", color = "black"),
  			axis.text.y = element_blank(),
    		axis.ticks.y = element_blank(),
    		axis.title.y = element_blank(),
  			panel.spacing.x=unit(0, "lines"),
  			legend.title.align=0.5)
  
  	ggsave(here("./output/plots/hs_2016_plot_noy.png"),
			 hs_2016_plot_noy,
			 width = 10, height = 7, units = "in")
		
	
  	### % change plots
  	
  		### for a black background
  	
  	 	plot <- 
  		ggplot() +
					geom_sf(data = sp_yr_sum_long_sf, aes(color = pct_change))  +
					geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
					coord_sf(crs = 3338) +
  		    facet_wrap(~ time_period, nrow = 1) +
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
    	  	labs(colour = "% change in\nspawning habitat\nsuitability") +
					theme(
  			strip.text = element_blank(),
 				strip.background = element_blank(),
				legend.background = element_rect(fill = "black"),
  			axis.text =element_text(size= 12, colour = "white"),
  			axis.title = element_text(size=16, color = "white"),
  			axis.line  = element_line(color = "white"),
  			axis.ticks  = element_line(colour = "white"),
  			legend.title = element_text(color = "white"),
  			legend.text = element_text(color = "white"),
  			panel.background = element_rect(fill = "black"),
  			plot.background = element_rect(fill = "black", color = "black"),
  			panel.spacing.x=unit(0, "lines"),
  			legend.title.align=0.5)
  

  	ggsave("./output/plots/percent_change_black.png",
		plot,
		width = 20, height = 10, units = "in")
	
  # consistency per decade
  	
  	plot <- 
  		ggplot() +
					geom_sf(data = ROMS_dat_hind_trim_current_yr_sum_05_sf, 
									aes(color = pct_yrs))  +
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
																symbol, "0.5", sep =" ", color = "white")) +
				theme(
  			strip.text = element_text(size = 16, color = "white", face = "bold"),
 				strip.background = element_blank(),
				legend.background = element_rect(fill = "black"),
  			axis.text =element_text(size= 12, colour = "white"),
  			axis.title = element_text(size=16, color = "white"),
  			axis.line  = element_line(color = "white"),
  			axis.ticks  = element_line(colour = "white"),
  			legend.title = element_text(color = "white", size = 16),
  			legend.text = element_text(color = "white", size = 14),
  			panel.background = element_rect(fill = "black"),
  			plot.background = element_rect(fill = "black", color = "black"),
  			panel.spacing.x=unit(0, "lines"),
  			legend.title.align=0.5)

  	ggsave("./output/plots/percent_years05_BLACK.png",
		plot,
		width = 15, height = 10, units = "in")
  	
  	## 0.9
  	
  		plot09 <- 
  		ggplot() +
					geom_sf(data = ROMS_dat_hind_trim_current_yr_sum_09_sf, 
									aes(color = pct_yrs))  +
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
																symbol, "0.5", sep =" ", color = "white")) +
				theme(
  			strip.text = element_blank(),
 				strip.background = element_blank(),
				legend.background = element_rect(fill = "black"),
  			axis.text =element_text(size= 12, colour = "white"),
  			axis.title = element_text(size=16, color = "white"),
  			axis.line  = element_line(color = "white"),
  			axis.ticks  = element_line(colour = "white"),
  			legend.title = element_text(color = "white", size = 16),
  			legend.text = element_text(color = "white", size = 14),
  			panel.background = element_rect(fill = "black"),
  			plot.background = element_rect(fill = "black", color = "black"),
  			panel.spacing.x=unit(0, "lines"),
  			legend.title.align=0.5)

  	ggsave("./output/plots/percent_years09_BLACK.png",
		plot09,
		width = 15, height = 10, units = "in")
	

## monthly average
  	
ROMS_dat_hind_trim_current_month_sum <- ROMS_dat_hind_trim_current %>%
	group_by(latitude, longitude, year, month_name) %>%
	summarise(mean_sphabsuit = mean(sp_hab_suit))

ROMS_dat_hind_trim_current_month_sum <- ROMS_dat_hind_trim_current_month_sum %>%
	mutate(decade = case_when(
		between(year, 1970, 1979) ~ "1970s",
		between(year, 1980, 1989) ~ "1980s",
		between(year, 1990, 1999) ~ "1990s",
		between(year, 2000, 2009) ~ "2000s",
		between(year, 2010, 2019) ~ "2010s"))


ROMS_dat_hind_trim_current_month_sum_05 <- ROMS_dat_hind_trim_current_month_sum %>%
	group_by(latitude, longitude, month_name, decade) %>%
	summarize(no_yrs = length(which(mean_sphabsuit >= 0.5))) %>%
	mutate(year_tot = 10,
				 pct_yrs = (no_yrs/year_tot) * 100)

ROMS_dat_hind_trim_current_month_sum_05_sf <- ROMS_dat_hind_trim_current_month_sum_05 %>%
		mutate(long_not_360 = longitude - 360) %>%
  	st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326)
  

plot <- 
  		ggplot() +
					geom_sf(data = ROMS_dat_hind_trim_current_month_sum_05_sf,
									aes(color = pct_yrs))  +
					geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
					coord_sf(crs = 3338) +
  		    facet_grid(decade ~ month_name) +
					scale_color_viridis_c() +
 					scale_x_continuous(
 						breaks = c( -170, -160),
 						labels = c("-170˚", "-160˚"),
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
						theme(
  			strip.text = element_text(size = 18, color = "white", face = "bold"),
 				strip.background = element_blank(),
				legend.background = element_rect(fill = "black"),
  			axis.text =element_text(size= 14, colour = "white"),
  			axis.title = element_text(size=18, color = "white"),
  			axis.line  = element_line(color = "white"),
  			axis.ticks  = element_line(colour = "white"),
  			legend.title = element_text(color = "white", size = 18),
  			legend.text = element_text(color = "white", size = 16),
  			panel.background = element_rect(fill = "black"),
  			plot.background = element_rect(fill = "black", color = "black"),
  			panel.spacing.x=unit(0, "lines"),
  			legend.title.align=0.5)
  
  	

  	ggsave("./output/plots/percent_years_months05_BLACK.png",
		plot,
		width = 15, height = 10, units = "in")
	
##### rolling means #####
  	
  	# of spawning habitat suitability
  	
  		# mean 
	rolling_mean_plot <- 
		ggplot(rolling_stats) +
		geom_line(aes(x = years, y = roll_yr_mean), color = "white", size = 2) +
		xlab("Year") +
		ylab("11-year rolling mean") +
  	theme(legend.position = c(),
					legend.background = element_rect(fill = "black"),
  				axis.text=element_text(size= 20, colour = "white"),
  				axis.title= element_text(size=22, color = "white"),
  				axis.line = element_line(color = "white"),
  				axis.ticks = element_line(colour = "white"),
  				legend.title = element_text(color = "white"),
  				legend.text = element_text(color = "white"),
  				panel.background = element_rect(fill = "black"),
					panel.grid = element_blank(),
  				plot.background = element_rect(fill = "black", color = "black"))

  	ggsave("./output/plots/rolling_mean_sp_hab_suit.png",
		rolling_mean_plot,
		width = 10, height = 5, units = "in")
	

  # sd
	rolling_sd_plot <- 
		ggplot(rolling_stats) +
		geom_line(aes(x = years, y = roll_yr_sd), color = "white", size = 2) +
		xlab("Year") +
		ylab("11-year rolling\nstandard deviation")  +
	    	theme(legend.position = c(),
					legend.background = element_rect(fill = "black"),
  				axis.text=element_text(size= 20, colour = "white"),
  				axis.title= element_text(size=22, color = "white"),
  				axis.line = element_line(color = "white"),
  				axis.ticks = element_line(colour = "white"),
  				legend.title = element_text(color = "white"),
  				legend.text = element_text(color = "white"),
  				panel.background = element_rect(fill = "black"),
					panel.grid = element_blank(),
  				plot.background = element_rect(fill = "black", color = "black"))

  	
  	ggsave("./output/plots/rolling_sd_plot_sp_hab_suit.png",
		rolling_sd_plot,
		width = 10, height = 5, units = "in")

  	### by month
  	
		mo_stats_4plot$month_no[mo_stats_4plot$month == "January"] <- 1
		mo_stats_4plot$month_no[mo_stats_4plot$month == "February"] <- 2
		mo_stats_4plot$month_no[mo_stats_4plot$month == "March"] <- 3
		mo_stats_4plot$month_no[mo_stats_4plot$month == "April"] <- 4
		mo_stats_4plot$month_no[mo_stats_4plot$month == "May"] <- 5
		mo_stats_4plot$month_no[mo_stats_4plot$month == "June"] <- 6
		
		# reorder for plotting
		mo_stats_4plot$month <- factor(mo_stats_4plot$month)
  	mo_stats_4plot$month <- fct_reorder(mo_stats_4plot$month, 
  																		mo_stats_4plot$month_no)


  	 # mean
  rolling_mean_plot_mo <- 
		ggplot(mo_stats_4plot) +
		geom_line(aes(x = year, y = roll_mean), color = "white", size = 2) +
  	facet_wrap(~ month, ncol = 3, nrow = 3) +
		xlab("Year") +
		ylab("11-year rolling mean") +
  		theme(	
  			strip.text = element_text(size = 20, face = "bold", color = "white"),
 				strip.background = element_blank(),
  				axis.text=element_text(size= 18, colour = "white"),
  				axis.title= element_text(size=20, color = "white"),
  				axis.line = element_line(color = "white"),
  				axis.ticks = element_line(colour = "white"),
  				panel.background = element_rect(fill = "black"),
  			  panel.grid  = element_blank(),
  				plot.background = element_rect(fill = "black", color = "black")) +
  	 annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey", size = 2)+
     annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey", size = 2)
  
  ggsave("./output/plots/rolling_mean_plot_mo.png",
		rolling_mean_plot_mo,
		width = 15, height = 10, units = "in")
  	
   # sd
  rolling_sd_plot_mo <- 
		ggplot(mo_stats_4plot) +
		geom_line(aes(x = year, y = roll_sd), color = "white", size = 2) +
  	facet_wrap(~ month, ncol = 3, nrow = 3) +
		xlab("Year") +
		ylab("11-year rolling\nstandard deviation") +
  		theme(	
  			strip.text = element_text(size = 20, face = "bold", color = "white"),
 				strip.background = element_blank(),
  				axis.text=element_text(size= 18, colour = "white"),
  				axis.title= element_text(size=20, color = "white"),
  				axis.line = element_line(color = "white"),
  				axis.ticks = element_line(colour = "white"),
  				panel.background = element_rect(fill = "black"),
  			  panel.grid  = element_blank(),
  				plot.background = element_rect(fill = "black", color = "black")) +
  	 annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey", size = 2)+
     annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey", size = 2)
  
  ggsave("./output/plots/rolling_sd_plot_mo.png",
		rolling_sd_plot_mo,
		width = 15, height = 10, units = "in")
  
  #### thermal response curve in black
  
  
gaus <- function(x) v[3]*exp(-1/2*(x-v[1])^2/v[2]^2)
cauchy <- function(x) v2[3]/(1+((x-v2[1])/v2[2])^2)


therm_response_cuve <- 
	ggplot(eggs) +
	geom_point(aes(x = Temp_act, y = Phatch), color = "white", size = 3) +
	geom_function(fun = gaus, aes(col = "Temp_act"), color = "white", size = 2) +
	geom_function(fun = cauchy, aes(col = "Temp_act"), color = "grey", size = 2) +
	xlab("Temperature ˚C") +
	ylab("Proportion successful hatch") +
	theme(
		legend.position = "none",
  				axis.text =element_text(size= 22, colour = "white"),
  				axis.title = element_text(size=24, color = "white"),
  				axis.line = element_line(color = "white"),
  				axis.ticks = element_line(colour = "white"),
  				panel.background = element_rect(fill = "black"),
					panel.grid = element_blank(),
  				plot.background = element_rect(fill = "black", color = "black"))

 ggsave(here("./output/plots/therm_response_cuve.png"),
		therm_response_cuve,
		width = 10, height = 10, units = "in")
 
 ## spawning habitat suitability
 
 yearly_hab_dat <- ROMS_dat_hind_trim %>%
   								 group_by(year) %>%
   								 summarise(annual_hatch_success_cauchy = mean(hatch_success_cauchy),
   								 					 annual_hatch_success_gaussian = mean(hatch_success_gaus),
   								  				annual_spawning_hab_suit = mean(sp_hab_suit))
	
	yearly_hab_dat_no_2021 <- yearly_hab_dat %>% filter(year != 2021)
 
	 annual_hatch_success_2020_cauchy <-    
   	ggplot(data = yearly_hab_dat_no_2021) +
   	geom_line(aes(x = year, y = annual_spawning_hab_suit), color = "lightgrey", size = 2) +
   	#geom_point(aes(x = year, y = annual_spawning_hab_suit), color = "white", size  = 4) +
   	xlab("Year") + 
	  scale_y_continuous(
	  	name = "Spawning habitat\nsuitability",
	  	breaks = c(0.30, 0.40, 0.50),
	  ) +
   	xlim(1970, 2020) +
  	theme(legend.position = c(),
					legend.background = element_rect(fill = "black"),
  				axis.text=element_text(size= 18, colour = "white"),
  				axis.title= element_text(size=20, color = "white"),
  				axis.line = element_line(color = "white"),
  				axis.ticks = element_line(colour = "white"),
  				legend.title = element_text(color = "white"),
  				legend.text = element_text(color = "white"),
  				panel.background = element_rect(fill = "black"),
					panel.grid = element_blank(),
  				plot.background = element_rect(fill = "black", color = "black"))

   annual_hatch_success_2020_cauchy_txt <- 
   	annual_hatch_success_2020_cauchy +
		annotate(geom = "text", x = 1980, y = 0.55,
           label = "Average annual spawning habitat suitability",
           color = "lightgrey", size = 6)
 
	ggsave("./output/plots/annual_hatch_success_2020_cauchy.png",
			 annual_hatch_success_2020_cauchy_txt,
			 width = 12, height = 6, units = "in")
