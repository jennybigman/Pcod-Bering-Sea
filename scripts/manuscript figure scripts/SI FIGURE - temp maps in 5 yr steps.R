# SI figure of temps every 5 years


	#### hindcasts ####
	
	# summarize data in 5-yr chunks
	# annual average
	ROMS_hindcast_yr_sum <- ROMS_hindcast_dat %>%
		group_by(latitude, longitude, year) %>%
		summarise(mean_temp = mean(temp))
	
	ROMS_hindcast_5yr_sum <- ROMS_hindcast_yr_sum %>%
		mutate(ints = cut(year, breaks = 10)) %>%
		group_by(ints, latitude, longitude) %>%
		summarize(mean_temp = mean(mean_temp)) %>%
		mutate(long_not_360 = case_when(
					 longitude >= 180 ~ longitude - 360,
					 longitude < 180 ~ longitude))  %>%
  	st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326, remove = FALSE) 
	
	# fix names
	ROMS_hindcast_5yr_sum$ints <- gsub(",", "-", ROMS_hindcast_5yr_sum$ints)
	ROMS_hindcast_5yr_sum$ints <- gsub("\\(", "", ROMS_hindcast_5yr_sum$ints)
	ROMS_hindcast_5yr_sum$ints <- gsub("\\]", "", ROMS_hindcast_5yr_sum$ints)

	# plot #
	hist_temp_5yr_sum_plot <- 
   	  	ggplot() +
				geom_sf(data = ROMS_hindcast_5yr_sum, aes(color = mean_temp))  +
				geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
				coord_sf(crs = 3338) +
				scale_color_viridis_c() +
   	  	facet_wrap(~ ints, ncol = 5, nrow = 2) +
 				scale_x_continuous(
 					breaks = breaks_x,
 					labels = labels_x,
 					name = "Longitude",
 					limits = limits_x
 				) +
 				scale_y_continuous(
 					breaks = breaks_y,
 					limits = limits_y,
 					name = "Latitude",
 				) +
   	  	labs(colour = "Temperature (˚C)") +
				theme_bw() +
 				theme(
 					strip.text = element_text(size = 14, face = "bold"),
 					strip.background = element_blank(),
 					axis.text = element_text(size = 12),	
  				axis.title = element_text(size = 14),
  				legend.title.align=0.5)
 
			ggsave(hist_temp_5yr_sum_plot, filename = "./output/plots/temp_5yr_sum_plot_SI.png",
				 height = 10, width = 25, units = "cm")
			
	#### projections ####

	# summarize data in 5-yr chunks
			
	years_proj <- 2021:2099
			
	# annual average
	ROMS_projected_yr_sum <- ROMS_projected_dat %>%
		filter(year %in% years_proj) %>%
		group_by(simulation, projection, latitude, longitude, year) %>%
		summarise(mean_temp = mean(bc_temp_sd))
	
	ROMS_projected_5yr_sum <- ROMS_projected_yr_sum %>%
		mutate(ints = cut(year, breaks = 15)) %>%
		group_by(simulation, projection, ints, latitude, longitude) %>%
		summarize(mean_temp = mean(mean_temp)) %>%
		mutate(long_not_360 = case_when(
					 longitude >= 180 ~ longitude - 360,
					 longitude < 180 ~ longitude))  %>%
  	st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326, remove = FALSE) 
	
	# fix names
	ROMS_projected_5yr_sum$ints <- gsub(",", "-", ROMS_projected_5yr_sum$ints)
	ROMS_projected_5yr_sum$ints <- gsub("\\(", "", ROMS_projected_5yr_sum$ints)
	ROMS_projected_5yr_sum$ints <- gsub("\\]", "", ROMS_projected_5yr_sum$ints)

	# plot #
	
	# one for each simulation
	
	proj_temp_plot_func <- function(x){
		
		new_dat <- ROMS_projected_5yr_sum %>% filter(simulation == x)
		
		plot <- 
   	  	ggplot() +
				geom_sf(data = new_dat, aes(color = mean_temp))  +
				geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
				coord_sf(crs = 3338) +
				scale_color_viridis_c(limits = c(-2.14, 10.53)) +
   	  	facet_grid(projection ~ simulation + ints) +
 				scale_x_continuous(
 					breaks = breaks_x,
 					labels = labels_x,
 					name = "Longitude",
 					limits = limits_x
 				) +
 				scale_y_continuous(
 					breaks = breaks_y,
 					limits = limits_y,
 					name = "Latitude",
 				) +
   	  	labs(colour = "Temperature (˚C)") +
				theme_bw() +
 				theme(
 					strip.text = element_text(size = 12, face = "bold"),
 					strip.background = element_blank(),
 					axis.text = element_text(size = 12),	
  				axis.title = element_text(size = 12),
  				legend.title.align=0.5,
 					plot.margin = margin(0, 0, 0, 0, "cm"))
		
		plot
		}
 
	sims <- unique(ROMS_projected_dat$simulation)
 
	plot_list <- lapply(sims, proj_temp_plot_func)
  
  name_func <- function(x){
  	year_month <- paste0(x, "_proj_temp_5yr_sum_plot")
  }
   
  names_plots <- sapply(sims, name_func)
  
	name_func_file <- function(x){
  	year_month <- paste0("/Users/jenniferbigman/Dropbox/NOAA AFSC Postdoc/Pcod Project/Pcod Bering Sea Habitat Suitability/Pcod-Bering-Sea/output/plots/", x)
  }
   
  names_file <- sapply(names_plots, name_func_file)

   ggsave_func <- function(x,y){
  	ggsave(plot = x,
    file = paste(y,".png",sep=""),
    width = 30, height = 10, units = "in")

   }
   
	plot_list_save <- mapply(ggsave_func, x = plot_list, y = names_file)

	# put plots together
	plot1 <- plot_list[[1]]
	plot2 <- plot_list[[2]]
	plot3 <- plot_list[[3]]
	
	proj_5yr_temp_plot_combined <- plot1/plot2/plot3 + plot_layout(guides = "collect")
	
	ggsave("./output/plots/proj_5yr_temp_plot_combined.png",
			 proj_5yr_temp_plot_combined,
			 width = 15, height = 10, units = "in")

	
	
	
	