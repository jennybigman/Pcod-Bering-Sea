## centroids  of all suitable spawning habitat per month and year


# (probably need to do this with actual grid cells that are scaled to the 
# correct size)

	#### centroid per year with all habitat ####

	yr_centroid_all <- function(x){
		
		new_dat <- ROMS_dat_hind_trim %>% 
			filter(., year == x)
		
		new_dat <- new_dat %>%
			rowwise() %>%
			mutate(lat_cent_top = (sp_hab_suit * latitude),
						 long_cent_top = (sp_hab_suit * longitude))
		
		lat_cent_num = sum(new_dat$lat_cent_top)
		long_cent_num = sum(new_dat$long_cent_top)	
		sp_hab_suit_sum = sum(new_dat$sp_hab_suit)
	 
		lat_centroid = lat_cent_num/sp_hab_suit_sum
		long_centroid = long_cent_num/sp_hab_suit_sum
		
		centroids <- data.frame(lat_centroid, long_centroid)
		
		centroids
		
		}
	
	years <- c(1970:2020)
	
	yr_centroids_all <- lapply(years,  yr_centroid_all)
	
	yr_centroids_all_df <- bind_rows(yr_centroids_all)
	
	yr_centroids_all_df <- bind_cols(years, yr_centroids_all_df) %>% 
		setNames(c("year", "lat_centroid", "long_centroid"))
	

	#### plot ####
	
	# latitude
	
	lat_centroid_all_yr_plot <-    
   	ggplot() +
   	geom_line(data = yr_centroids_all_df, 
   						aes(x = year, y = lat_centroid), alpha = 0.7, 
   						color = "#7f7fbf", size = 1) +
		xlab("Year") + 
	  scale_y_continuous(
	  	name = "Latitude centroid of\nspawning habitat suitability"
	  	#breaks = c(56, 57),
	  	#labels = c("56˚N", "57˚N"),
	  	#limits = c(56, 57.5)
	  ) +
   	xlim(1970, 2020) +
   	theme_bw() +
  	theme(
  	  axis.text=element_text(size=12, colour = "grey50"),
  	  axis.ticks = element_line(colour = "grey50"),
  	  axis.line = element_line(colour = "grey50"),
  	  axis.title.x = element_text(size=14, color = "grey30"),
  	  #axis.title.y = element_text(angle = 360, vjust = 0.5, size=14, color = "grey30"),
  	  panel.grid.major = element_blank(),
  	  panel.grid.minor = element_blank(),
  	  panel.border = element_rect(fill = NA, color = "grey50"))
  	
		ggsave("./output/plots/lat_centroid_yr_plot_allhab.png",
			 lat_centroid_all_yr_plot,
			 width = 10, height = 7, units = "in")

	## longitude ##
		
	long_centroid_yr_all_plot <-    
   	ggplot() +
   	geom_line(data = yr_centroids_all_df, 
   						aes(x = year, y = long_centroid), alpha = 0.7, 
   						color = "#7f7fbf", size = 1) +
		xlab("Year") + 
	  scale_y_continuous(
	  	name = "Longitude centroid of\nspawning habitat suitability"
	  	#breaks = c(56, 57),
	  	#labels = c("56˚N", "57˚N"),
	  	#limits = c(56, 57.5)
	  ) +
   	xlim(1970, 2020) +
   	theme_bw() +
  	theme(
  	  axis.text=element_text(size=12, colour = "grey50"),
  	  axis.ticks = element_line(colour = "grey50"),
  	  axis.line = element_line(colour = "grey50"),
  	  axis.title.x = element_text(size=14, color = "grey30"),
  	  #axis.title.y = element_text(angle = 360, vjust = 0.5, size=14, color = "grey30"),
  	  panel.grid.major = element_blank(),
  	  panel.grid.minor = element_blank(),
  	  panel.border = element_rect(fill = NA, color = "grey50"))
	
		ggsave("./output/plots/long_centroid_yr_plot_allhab.png",
			 long_centroid_yr_all_plot,
			 width = 10, height = 7, units = "in")

	
	#### centroid by month ####		
		
	# latitude
		
	lat_centroid_mo_all <- function(x){
		
		new_dat <- ROMS_dat_hind_trim %>% 
			filter(., year == x)
		
		new_dat <- new_dat %>%
			rowwise() %>%
			mutate(lat_cent_top = (sp_hab_suit * latitude),
						 long_cent_top = (sp_hab_suit * longitude))
		
		months <- unique(ROMS_dat_hind_trim$month_name)
		
		mo_lat_centroid <- list()
		
			
		for (i in months){
			
			new_dat2 <- new_dat %>% filter(., month_name == i)
			
			lat_cent_num = sum(new_dat2$lat_cent_top)
			sp_hab_suit_sum = sum(new_dat2$sp_hab_suit)
	 
			mo_lat_centroid[i] = lat_cent_num/sp_hab_suit_sum

			mo_lat_centroid[i]
			
		}
		
		mo_lat_centroid
		
		}

	
	mo_lat_centroid_all_dfs <- lapply(years, lat_centroid_mo_all)
	
	mo_lat_centroid_all_dfs <- bind_rows(mo_lat_centroid_all_dfs)
	
	mo_lat_centroid_all_dfs <- gather(mo_lat_centroid_all_dfs, 
														 key = "month", value = "lat_centroid")
	
	mo_lat_centroid_all_dfs$year <- rep(c(1970:2020), 6)
	
	mo_lat_centroid_all_dfs <- mo_lat_centroid_all_dfs %>%
		rename(lat_centroid_mo = lat_centroid)
	
 # longitude
	
	long_centroid_all_mo <- function(x){
		
		new_dat <- ROMS_dat_hind_trim %>% 
			filter(., year == x)
		
		new_dat <- new_dat %>%
			rowwise() %>%
			mutate(lat_cent_top = (sp_hab_suit * latitude),
						 long_cent_top = (sp_hab_suit * longitude))

		months <- unique(ROMS_dat_hind_trim$month_name)
		
		mo_long_centroid <- list()
		
			
		for (i in months){
			
			new_dat2 <- new_dat %>% filter(., month_name == i)
			
			long_cent_num = sum(new_dat2$long_cent_top)
			sp_hab_suit_sum = sum(new_dat2$sp_hab_suit)
	 
			mo_long_centroid[i] = long_cent_num/sp_hab_suit_sum

			mo_long_centroid[i]
			
		}
		
		mo_long_centroid
		
		}

	
	mo_long_centroid_all_dfs <- lapply(years, long_centroid_all_mo)
	
	mo_long_centroid_all_dfs <- bind_rows(mo_long_centroid_all_dfs)
	
	mo_long_centroid_all_dfs <- gather(mo_long_centroid_all_dfs, 
														 key = "month", value = "long_centroid")
	
	mo_long_centroid_all_dfs$year <- rep(c(1970:2020), 6)
	
	mo_long_centroid_all_dfs <- mo_long_centroid_all_dfs %>%
		rename(long_centroid_mo = long_centroid)

	
	# bind together
	mo_centroids <- left_join(mo_long_centroid_all_dfs, mo_lat_centroid_all_dfs,
															by = c("year", "month"))
	
	#### month plots ####
	
	# reorder for plotting
	mo_centroids$month <- factor(mo_centroids$month)
	mo_centroids$month_no <- rep(1:5, each = 51)
  mo_centroids$month <- fct_reorder(mo_centroids$month, 
  																	mo_centroids$month_no)

	## latitude ##
	
	lat_centroid_mo_plot <-    
   	ggplot() +
   	geom_line(data = mo_centroids, 
   						aes(x = year, y = lat_centroid_mo), alpha = 0.7, 
   						color = "#7f7fbf", size = 1) +
		xlab("Year") +
		facet_wrap(~month) +
	  scale_y_continuous(
	  	name = "Latitude centroid of\nspawning habitat suitability",
	  #	breaks = c(55, 57, 59),
	  #	labels = c("55˚N", "57˚N", "59˚N"),
	  ) +
   	xlim(1970, 2020) +
   	theme_bw() +
  	theme(
  		strip.background = element_blank(),
  		strip.text = element_text(face = "bold", size= 12),
  	  axis.text=element_text(size=12, colour = "grey50"),
  	  axis.ticks = element_line(colour = "grey50"),
  	  axis.line = element_line(colour = "grey50"),
  	  axis.title.x = element_text(size=14, color = "grey30"),
  	  panel.grid.major = element_blank(),
  	  panel.grid.minor = element_blank(),
  	  panel.border = element_rect(fill = NA, color = "grey50"))
  	
		ggsave("./output/plots/lat_centroid_mo_plot_allhab.png",
			 lat_centroid_mo_plot,
			 width = 10, height = 7, units = "in")

		## longitude ##
		
		long_centroid_mo_plot <-    
   	ggplot() +
   	geom_line(data = mo_centroids, 
   						aes(x = year, y = long_centroid_mo), alpha = 0.7, 
   						color = "#7f7fbf", size = 1) +
		xlab("Year") +
		facet_wrap(~month) +
	  scale_y_continuous(
	  	name = "Longitude centroid of\nspawning habitat suitability"
	  ) +
   	xlim(1970, 2020) +
   	theme_bw() +
  	theme(
  		strip.background = element_blank(),
  		strip.text = element_text(face = "bold", size= 12),
  	  axis.text=element_text(size=12, colour = "grey50"),
  	  axis.ticks = element_line(colour = "grey50"),
  	  axis.line = element_line(colour = "grey50"),
  	  axis.title.x = element_text(size=14, color = "grey30"),
  	  panel.grid.major = element_blank(),
  	  panel.grid.minor = element_blank(),
  	  panel.border = element_rect(fill = NA, color = "grey50"))
  	
		ggsave("./output/plots/long_centroid_mo_plot_allhab.png",
			 long_centroid_mo_plot,
			 width = 10, height = 7, units = "in")

		# map #