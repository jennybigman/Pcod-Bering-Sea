	# plotting annual index of habitat suitability and maps for cold/warm year for slide for Pcod NPRB mtg

	#### plot annual index of spawning habitat suitability ####
  
	yearly_hab_dat <- sm_temp_hind_df %>%
   								  group_by(year) %>%
   								  summarise(mean_sp_hab_suit = mean(sp_hab_suit))
	
	yearly_hab_dat_2020 <- yearly_hab_dat %>% filter(., year != "2021")

	annual_hatch_success_2020_cauchy <- 
		ggplot(data = yearly_hab_dat_2020) +
  	geom_line(aes(x = year, y = mean_sp_hab_suit), color = "black", size = 1) +
  	geom_point(aes(x = year, y = mean_sp_hab_suit), color = "black", size  = 4) +
  	xlab("Year") + 
		scale_y_continuous(
	  	name = "Spawning\nhabitat\nsuitability",
	  	breaks = c(0.3, 0.4, 0.5),
	  	limits = c(0.3, 0.55)
		) +
  	xlim(1970, 2020) +
  	theme_bw() +
		theme(legend.position = "none") +
  	theme(
  	  axis.text=element_text(size=14, colour = "grey50"),
  	  axis.ticks = element_line(colour = "grey50"),
  	  axis.line = element_line(colour = "grey50"),
  	  axis.text.x = element_text(size = 12),
  	  axis.title.y = element_text(angle = 360, vjust = 0.5, size=14, color = "grey30"),
  	  axis.title.x  = element_text(size=14, color = "grey30"),
  	  panel.grid.major = element_blank(),
  	  panel.grid.minor = element_blank(),
  	  panel.border = element_rect(fill = NA, color = "grey50"))
   
   annual_hatch_success_2020_cauchy_txt <- 
   	annual_hatch_success_2020_cauchy +
		annotate(geom = "text", x = 1982, y = 0.55,
           label = "Average annual spawning habitat suitability",
           color = "#000000", size = 5.5)
 
	ggsave("./output/plots/annual_hatch_success_2020_cauchy.png",
			 annual_hatch_success_2020_cauchy_txt,
			 width = 10, height = 7, units = "in")
	
###############	
# maps ####
###############
	
	temp_2008_sf <- sm_temp_hind_df_sf %>% filter(., year == 2008)
	
	temp_2016_sf <- sm_temp_hind_df_sf %>% filter(., year == 2016)
	
# maps for col vs warm years

	hs_2008_plot_noleg <- 
		ggplot() +
  	geom_sf(data = temp_2008_sf, aes(color = sp_hab_suit)) +
  	geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
		coord_sf(crs = 3338) +
		scale_color_viridis_c(breaks = c(0, 0.50, 1.0), limits = c(0,1)) +
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
 			legend.position = "none",
  		strip.background = element_rect(colour="white", fill="white"),
    	panel.border = element_rect(colour = "black"),
  		strip.text = element_text(size = 14, face = "bold"),
  		axis.text = element_text(size = 10),	
  		axis.title = element_text(size = 14),
  		panel.spacing.x=unit(0, "lines"),
  		legend.title.align=0.5)
  		
	hs_2016_plot_noy <- 
		ggplot() +
  	geom_sf(data = temp_2016_sf, aes(color = sp_hab_suit)) +
  	geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
		coord_sf(crs = 3338) +
		scale_color_viridis_c(breaks = c(0, 0.50, 1.0), limits = c(0,1)) +
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
			legend.title = element_text(size = 12),
  		legend.text = element_text(size = 12),  	
			strip.background = element_rect(colour="white", fill="white"),
    	panel.border = element_rect(colour = "black"),
  		strip.text = element_text(size = 14, face = "bold"),
			axis.text.x = element_text(size = 10),		
  		axis.text.y = element_blank(),
    	axis.ticks.y = element_blank(),
    	axis.title.y = element_blank(),
  		axis.title.x = element_text(size = 14),
  		panel.spacing.x=unit(0, "lines"),
  		legend.title.align=0.5)
  

   plot1 <- hs_2008_plot_noleg + 
   	        theme(plot.margin = unit(c(0.2, 0, 0.2, 0.2), "in")) +
   	        ggtitle("2008: Cold Year") +
   	        theme(plot.title = element_text(size = 14, hjust = 0.5,  face = "bold"))
   
   plot2 <- hs_2016_plot_noy + 
   	        theme(plot.margin = unit(c(0.2, 0.2, 0.2, -0.05), "in")) +
   	        ggtitle("2016: Warm Year") +
   	        theme(plot.title = element_text(size = 14, hjust = 0.5,  face = "bold"))

	 warm_v_cold_hs <- plot1 + plot2 
	 
  	ggsave("./output/plots/warm_v_cold_hs.png",
			 warm_v_cold_hs,
			 width = 10, height = 7, units = "in")
		
  		