	############### 
	# Maps ####
	###############

	# make two maps, one for cold year (2008), one for warm year (2012)
							 

	hab_suit_yr_plot_func_hind <- function(x){
		
		    new_dat <- ROMS_hindcast_dat %>% filter(., year == x)
		    
		    new_dat_sum <- new_dat %>%
					group_by(year, latitude, long_not_360) %>%
					summarise(mean_sphabsuit = mean(sp_hab_suit))
	
				new_dat_sum <- new_dat_sum	%>%
  				st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326, remove = FALSE)
	
    	  plot <- 
    	  	ggplot() +
					geom_sf(data = new_dat_sum, aes(color = mean_sphabsuit))  +
					geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
					coord_sf(crs = 3338) +
					scale_color_gradientn(colors = c("#B3E5FC","#B3E5FC",
																					 "#3378af","#3378af",
																					 "#00345C","#00345C"),
																values = c(0, 0.499, 0.5, 0.899, 0.9, 1),
																breaks = c(0.1, 0.5, 0.9),
																labels = c(0.1, 0.5, 0.9),
																limits = c(0, 1)) +
 					scale_x_continuous(
 						breaks = c(-170, -160),
 						labels = c( "-170˚", "-160˚"),
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
    	  	ggtitle(paste0(x)) + 
  		theme(
 						strip.text = element_text(size = 14, face = "bold"),
 						strip.background = element_blank(),
 						axis.text = element_text(size = 12),	
  					axis.title = element_text(size = 14),
  					legend.title.align=0.5)      
    	  	
    	  plot
    	  
	}
	
	years_hind <- c(2008, 2016)
	
	hind_plot_list <- lapply(years_hind, hab_suit_yr_plot_func_hind)
  
	habsuit_hind_2008_plot <- hind_plot_list[[1]]
	habsuit_hind_2016_plot <- hind_plot_list[[2]]
	
	ggsave(here("./output/plots/habsuit_hind_2008_plot_SSC.png"),
			habsuit_hind_2008_plot,
			width = 5, height = 5, units = "in")
	
	ggsave(here("./output/plots/habsuit_hind_2016_plot_SSC.png"),
		habsuit_hind_2016_plot,
		width = 5, height = 5, units = "in")
 	
	