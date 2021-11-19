	# map monthly-averaged spawning habitat suitability 

  #### map with a color gradient for spawning habitat suitability  ####
  
		hab_suit_monthly_plot_func <- function(x){
		
		    new_dat <- ROMS_dat_hind_trim_sf %>% filter(., year == 1970)
    
    	  plot <- 
    	  	ggplot() +
					geom_sf(data = new_dat, aes(color = sp_hab_suit))  +
					geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
					coord_sf(crs = 3338) +
					scale_color_viridis_c(breaks = c(0, 0.50, 1.0), limits = c(0,1)) +
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
    	  	labs(colour = "Spawning\nhabitat\nsuitability") +
					theme_bw() +
 					theme(
 						strip.text = element_text(size = 14, face = "bold"),
 						strip.background = element_blank(),
 						axis.text = element_text(size = 12),	
  					axis.title = element_text(size = 14),
  					legend.title.align=0.5)
    	  
    	  plot
    	  
	}
	
	years <- c(1970:2020)

	monthly_plot_list <- lapply(years, hab_suit_monthly_plot_func)
  
  mo_name_func_year <- function(x){
  	year_month <- paste0(x, "_habitat_suitability_gradient_check")
  }
   
  month_names_year <- sapply(years, mo_name_func_year)
  
	mo_name_func_file <- function(x){
  	year_month <- paste0("/Users/jenniferbigman/Google Drive/NOAA AFSC Postdoc/Pcod Bering Sea Habitat Suitability/Pcod-Bering-Sea/output/plots/monthly plots/habitat suitability/habitat suitability gradient/", x)
  }
   
  month_names <- sapply(month_names_year, mo_name_func_file)

	plot_list <- mapply(ggsave_func, x = monthly_plot_list, y = month_names)


  #### map spawning habitat suitability where > 0.9 colored dark blue ####

	hab_suit_monthly_plot_func <- function(x){
		
		    new_dat <- ROMS_dat_hind_trim_sf %>% filter(., year == x)
    
    	  plot <- 
    	  	ggplot() +
					geom_sf(data = new_dat, aes(color = sp_hab_suit))  +
					geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
					coord_sf(crs = 3338) +
					scale_color_gradientn(colors = c("#B3E5FC", "#B3E5FC", 
																					 "#01579B", "#01579B"),
																values = c(0, 0.899, 0.9, 1),
																breaks = c(0.1, 0.5, 0.9),
																labels = format(c(0.1, 0.5, 0.9)),
																limits = c(0, 1))  +
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
    	  	labs(colour = "Spawning\nhabitat\nsuitability") +
					theme_bw() +
 					theme(
 						strip.text = element_text(size = 14, face = "bold"),
 						strip.background = element_blank(),
 						axis.text = element_text(size = 12),	
  					axis.title = element_text(size = 14),
  					legend.title.align=0.5)
    	  	
    	  plot
    	  
	}
	
	years <- c(1970:2020)

	monthly_plot_list <- lapply(years, hab_suit_monthly_plot_func)
  

  mo_name_func_year <- function(x){
  	year_month <- paste0(x, "_habitat_suitability_90")
  }
   
  month_names_year <- sapply(years, mo_name_func_year)
  
	mo_name_func_file <- function(x){
  	year_month <- paste0("/Users/jenniferbigman/Google Drive/NOAA AFSC Postdoc/Pcod Bering Sea Habitat Suitability/Pcod-Bering-Sea/output/plots/monthly plots/habitat suitability/habitat suitability 90/", x)
  }
   
  month_names <- sapply(month_names_year, mo_name_func_file)
				
	plot_list <- mapply(ggsave_func, x = monthly_plot_list, y = month_names)

  
	#### map spawning habitat suitability where > 0.1 < 0.9 and > 0.9 colored different blues ####

		hab_suit_monthly_plot_func <- function(x){
		
		    new_dat <- ROMS_dat_hind_trim_sf %>% filter(., year == x)
    
    	  plot <- 
    	  	ggplot() +
					geom_sf(data = new_dat, aes(color = sp_hab_suit))  +
					geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
					coord_sf(crs = 3338) +
					scale_color_gradientn(colors = c("#B3E5FC","#B3E5FC",
																					 "#01579B","#01579B",
																					 "#00345C","#00345C"),
																values = c(0, 0.099, 0.1, 0.899, 0.9, 1),
																breaks = c(0.1, 0.5, 0.9),
																labels = c(0.1, 0.5, 0.9),
																limits = c(0, 1)) +
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
    	  	labs(colour = "Spawning\nhabitat\nsuitability") +
					theme_bw() +
 					theme(
 						strip.text = element_text(size = 14, face = "bold"),
 						strip.background = element_blank(),
 						axis.text = element_text(size = 12),	
  					axis.title = element_text(size = 14),
  					legend.title.align=0.5)
    	  
    	  plot
    	  
	}
	
	years <- c(1970:2020)

	monthly_plot_list <- lapply(years, hab_suit_monthly_plot_func)
  
	mo_name_func_year <- function(x){
  	year_month <- paste0(x, "_habitat_suitability_10")
  }
   
  month_names_year <- sapply(years, mo_name_func_year)
  
	mo_name_func_file <- function(x){
  	year_month <- paste0("/Users/jenniferbigman/Google Drive/NOAA AFSC Postdoc/Pcod Bering Sea Habitat Suitability/Pcod-Bering-Sea/output/plots/monthly plots/habitat suitability/habitat suitability 10/", x)
  }
   
  month_names <- sapply(month_names_year, mo_name_func_file)
  
	plot_list <- mapply(ggsave_func, x = monthly_plot_list, y = month_names)

	
	#### map spawning habitat suitability where > 0.5 < 0.9 and > 0.9 colored different blues ####
	
		hab_suit_monthly_plot_func <- function(x){
		
		    new_dat <- ROMS_dat_hind_trim_sf %>% filter(., year == x)
    
    	  plot <- 
    	  	ggplot() +
					geom_sf(data = new_dat, aes(color = sp_hab_suit))  +
					geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
					coord_sf(crs = 3338) +
					scale_color_gradientn(colors = c("#B3E5FC", "#B3E5FC", 
																					 "#01579B", "#01579B",
																					 "#00345C", "#00345C"),
																values = c(0, 0.499, 0.5, 0.899, 0.9, 1),
																breaks = c(0.1, 0.5, 0.9),
																labels = format(c(0.1, 0.5, 0.9)),
																limits = c(0, 1)) +
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
    	  	labs(colour = "Spawning\nhabitat\nsuitability") +
					theme_bw() +
 					theme(
 						strip.text = element_text(size = 14, face = "bold"),
 						strip.background = element_blank(),
 						axis.text = element_text(size = 12),	
  					axis.title = element_text(size = 14),
  					legend.title.align=0.5)
    	  
    	  plot
    	  
	}
	
	years <- c(1970:2020)

	monthly_plot_list <- lapply(years, hab_suit_monthly_plot_func)
  
  mo_name_func_year <- function(x){
  	year_month <- paste0(x, "_habitat_suitability_5090")
  }
   
  month_names_year <- sapply(years, mo_name_func_year)
  
	mo_name_func_file <- function(x){
  	year_month <- paste0("/Users/jenniferbigman/Google Drive/NOAA AFSC Postdoc/Pcod Bering Sea Habitat Suitability/Pcod-Bering-Sea/output/plots/monthly plots/habitat suitability/habitat suitability 50_90/", x)
  }
   
  month_names <- sapply(month_names_year, mo_name_func_file)
				
	plot_list <- mapply(ggsave_func, x = monthly_plot_list, y = month_names)

	
	#### plot frequency histograms for each month for each year #### NOT COMPLETE
	
	dat_1970 <- dat_1970 %>%
		mutate(group1 = case_when(
			between(sp_hab_suit, 0, 0.1) ~ "A",
			between(sp_hab_suit, 0.1, 0.9) ~ "B",
			sp_hab_suit >= 0.9 ~ "C"
		))
	
	
		histo_plot_func <- function(x){
		
		    new_dat <- ROMS_dat_hind_trim_sf %>% filter(., year == x)
		    
		    new_dat <- new_dat %>%
					mutate(group = case_when(
						between(sp_hab_suit, 0, 0.1) ~ "A",
						between(sp_hab_suit, 0.1, 0.9) ~ "B",
						sp_hab_suit >= 0.9 ~ "C"))
    
    	  plot <- 
    	  	ggplot(new_dat, aes(sp_hab_suit, fill = group)) +
    	  	geom_histogram(bins = 10, color = "black") +
    	  	scale_fill_manual(values = c("#B3E5FC", "#01579B","#00345C"),
    	  										breaks = unique(new_dat$group1)) +
    	  	xlab("Spawning habitat suitability") +
    	  	geom_vline(xintercept = 0.1, color = "lightgrey") +
    	  	geom_vline(xintercept = 0.5, color = "grey") +
    	  	geom_vline(xintercept = 0.9, color = "darkgrey") +
    	  	facet_wrap(~ month_name, ncol = 3, nrow = 3) +
    	  	scale_x_continuous(
    	  		breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1.0),
    	  		labels =format(c(0, 0.2, 0.4, 0.6, 0.8, 1.0))) +
				  scale_y_continuous(
				  	name = "Count",
        		breaks = c(0, 5000, 10000),
    				labels = format(c(0, 5000, 10000))) +
					theme_bw() +
 					theme(
 						strip.text = element_text(size = 14, face = "bold"),
 						strip.background = element_blank(),
    				axis.text = element_text(size=12, colour = "grey50"),
    				axis.ticks  = element_line(colour = "grey50"),
    				axis.line = element_line(colour = "grey50"),
    				axis.title = element_text(size=14, color = "grey30"),
    				panel.grid.major = element_blank(),
    				panel.grid.minor = element_blank(),
    				panel.border = element_rect(fill = NA, color = "grey50")
 					)
    	  
    	  plot

		}
	
	monthly_plot_list <- lapply(years, histo_plot_func)
  
  mo_name_func_year <- function(x){
  	year_month <- paste0(x, "_hist_sp_hab_suit")
  }
   
  month_names_year <- sapply(years, mo_name_func_year)
  
	mo_name_func_file <- function(x){
  	year_month <- paste0("/Users/jenniferbigman/Google Drive/NOAA AFSC Postdoc/Pcod Bering Sea Habitat Suitability/Pcod-Bering-Sea/output/plots/monthly plots/habitat suitability/histograms/", x)
  }
   
  month_names <- sapply(month_names_year, mo_name_func_file)
	
   ggsave_func2 <- function(x,y){
  	ggsave(plot = x,
    file = paste(y,".png",sep=""),
    width = 15, height = 10, units = "in")
	}
  
	plot_list <- mapply(ggsave_func2, x = monthly_plot_list, y = month_names)

	
	#### index of suitable spawning habitat ####
	
	yearly_hab_dat <- ROMS_dat_hind_trim_sf %>%
   								  group_by(year) %>%
   								  summarise(annual_hatch_success_cauchy = mean(hatch_success_cauchy),
   								 					  annual_hatch_success_gaussian = mean(hatch_success_gaus),
   								  					annual_spawning_hab_suit = mean(sp_hab_suit))
	
	yearly_hab_dat_no_2021 <- yearly_hab_dat %>% filter(year != 2021)
 
	 annual_hatch_success_2020_cauchy <-    
   	ggplot(data = yearly_hab_dat_no_2021) +
   	geom_line(aes(x = year, y = annual_spawning_hab_suit), color = "black", size = 1) +
   	geom_point(aes(x = year, y = annual_spawning_hab_suit), color = "black", size  = 4) +
   	xlab("Year") + 
	  scale_y_continuous(
	  	name = "Spawning habitat suitability",
	  	breaks = c(0.30, 0.40, 0.50),
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
		annotate(geom = "text", x = 1983, y = 0.55,
           label = "Average annual spawning habitat suitability",
           color = "#000000", size = 6)
 
	ggsave("./output/plots/annual_hatch_success_2020_cauchy.png",
			 annual_hatch_success_2020_cauchy_txt,
			 width = 10, height = 7, units = "in")

	#### comparing year bins: 2000 - 2014 vs. 2015 - 2020 ####
	
	years_keep <- c(2000:2020)
  	
  ROMS_dat_hind_trim_sf_sub <- ROMS_dat_hind_trim_sf %>% filter(., year %in% years_keep)
  

	# create a list of year bins
	year_bins  <- list('2000-2014' = c(2000:2014),
                     '2015-2020' = c(2015:2020)) 

	# create a list of dfs summarized by year bins
	sum_yr_bin <- function(x){
 
		df <- ROMS_dat_hind_trim_sf_sub %>%
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
  	dplyr::select(latitude, longitude, mean_sphabsuit, time_period)
 
	sm_temp_hind_df_yr_sum_sf <- sm_temp_hind_df_yr_sum %>%
			mutate(long_not_360 = longitude - 360) %>%
  		st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326)
  
	# plot
	
	plot <- ggplot() +
					geom_sf(data = sm_temp_hind_df_yr_sum_sf, aes(color = mean_sphabsuit))  +
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
    	  	labs(colour = "Spawning\nhabitat suitability") +
					theme_bw() +
 					theme(
						strip.text = element_text(size = 14, face = "bold"),
 						strip.background = element_blank(), 						
						axis.text = element_text(size = 12),	
  					axis.title = element_text(size = 14),
  					legend.title.align=0.5)
	
		ggsave("./output/plots/historic_vs_recent_shs.png",
			 plot,
			 width = 10, height = 7, units = "in")


		