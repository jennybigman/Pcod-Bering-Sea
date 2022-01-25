# animation of habitat suitability

library(gganimate)
library(magick)

test_yrs <- 1980:1982

subset_dat <- ROMS_hindcast_dat_sf %>%
	filter(year %in% test_yrs)


    	  plot <- 
    	  	ggplot() +
					geom_sf(data = subset_dat, aes(color = sp_hab_suit))  +
					geom_sf(data = world_map_data, fill = "grey", lwd = 0,
									frame = year) +
					coord_sf(crs = 3338) +
					scale_color_gradientn(colors = c("#B3E5FC", "#B3E5FC", 
																					 "#01579B", "#01579B"),
																values = c(0, 0.899, 0.9, 1),
																breaks = c(0.1, 0.5, 0.9),
																labels = format(c(0.1, 0.5, 0.9)),
																limits = c(0, 1))  +
    	  	facet_wrap(~ month_name, ncol = 2, nrow = 2) +
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
					theme_bw() +
 					theme(
 						strip.text = element_text(size = 14, face = "bold"),
 						strip.background = element_blank(),
 						axis.text = element_text(size = 12),	
  					axis.title = element_text(size = 14),
  					legend.title.align=0.5) +
    	  	labs(title = 'Year: {frame_time}') +
  				transition_states(year, state_length = 0, wrap = FALSE) 

    	    	  
    	num_yrs <- length(unique(subset_dat$year))
    	animate(plot, nframes = num_yrs)
    	animate(plot, renderer = gifski_renderer())
			anim_save("hind_habitat_suitability.gif")
			

	
	# using magick package ####
			
	library(magick)
			
	dir_out <- ("~/My Drive/NOAA AFSC Postdoc/Pcod Bering Sea Habitat Suitability/Pcod-Bering-Sea/output/plots/monthly plots/habitat suitability/hindcasts/habitat suitability 10_90")

			imgs <- list.files(dir_out, full.names = TRUE)
			img_list <- lapply(imgs, image_read)

			## join the images together
			img_joined <- image_join(img_list)
			
			## animate at 2 frames per second
			img_animated <- image_animate(img_joined, fps = 2)
			
			## view animated image
			img_animated
			
			## save to disk
			image_write(image = img_animated,
			            path = "habsuit90_hindcast.gif")			
	