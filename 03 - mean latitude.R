	# mean latitude

	mean_lat_function <- function(x){
		
		new_dat <- sm_temp_hind_df %>%
			filter(., sp_hab_suit >= x)
		
		new_dat_sum <- new_dat %>%
			group_by(year) %>%
			summarise(mean_lat = mean(latitude)) 

		new_dat_sum		
	}
	
	sp_hab_thresholds <- c(0.5, 0.9)
	
	mean_lats <- lapply(sp_hab_thresholds, mean_lat_function)
	
	mean_lats_0.5 <- mean_lats[[1]] %>%
		rename(mean_lat_0.5 = mean_lat)
	
	mean_lats_0.9 <- mean_lats[[2]]	%>%
		rename(mean_lat_0.9 = mean_lat)
	
	mean_lats <- merge(mean_lats_0.5, mean_lats_0.9, by = "year") 
	
	mean_lats_2021 <- mean_lats %>% filter(., year != 2021)
	
	# one plot
	
	mean_lat_yearly_plot <-    
   	ggplot(data = mean_lats_2021) +
   	geom_line(aes(x = year, y = mean_lat_0.5), alpha = 0.7, color = "#7f7fbf", size = 1) +
		#geom_point(aes(x = year, y = mean_lat_0.5), alpha = 0.7, color = "#7f7fbf", size = 1) +
		geom_line(aes(x = year, y = mean_lat_0.9), alpha = 0.7, color = "#00345C", size = 1) +
		#geom_point(aes(x = year, y = mean_lat_0.9), alpha = 0.7, color = "#00345C", size = 1) +
   	xlab("Year") + 
	  scale_y_continuous(
	  	name = "Mean\nlatitude",
	  	breaks = c(56, 57),
	  	labels = c("56˚N", "57˚N")
	  ) +
   	xlim(1970, 2030) +
   	theme_bw() +
  	theme(
  	  axis.text=element_text(size=12, colour = "grey50"),
  	  axis.ticks = element_line(colour = "grey50"),
  	  axis.line = element_line(colour = "grey50"),
  	  axis.title.x = element_text(size=14, color = "grey30"),
  	  axis.title.y = element_text(angle = 360, vjust = 0.5, size=14, color = "grey30"),
  	  panel.grid.major = element_blank(),
  	  panel.grid.minor = element_blank(),
  	  panel.border = element_rect(fill = NA, color = "grey50"))
   
   mean_lat_yearly_plot_text <- mean_lat_yearly_plot +
		annotate(geom = "text", x = 2026, y = 57,
           label = paste("spawning\nhabitat\nsuitability", symbol, "0.5", sep = " "),
           color = "#7f7fbf", size = 4) +
   	annotate(geom = "text", x = 2026, y = 56,
           label = paste("spawning\nhabitat\nsuitability", symbol, "0.9", sep = " "),
           color = "#00345C", size = 4) 
   	
	ggsave("./output/plots/mean_lat_yearly_plot.png",
			 mean_lat_yearly_plot_text,
			 width = 10, height = 7, units = "in")

	
	# try with map
	
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
	
	