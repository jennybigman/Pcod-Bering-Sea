
	# making a map of the NEP
	
	# save polygons
	world <- ne_countries(scale = "medium",
                      returnclass = "sf") 

	
	# grab those needed
	NorthAm <- world %>%
  	filter(name %in% c("Canada", "United States"))

	Russia <- world %>%
		filter(name == "Russia")
	
	
	# set lat long limits
	xmin <- 150
	xmax <- 240
	ymin <- 35
	ymax <- 71
	
	
	# make map
	NEP_map <-
		ggplot() + 
		geom_sf(data = NorthAm %>%
             st_shift_longitude(), color = "lightgrey", fill="white") +
		geom_sf(data = Russia %>%
             st_shift_longitude(), color = "lightgrey", fill="white") +
		xlim(c(xmin, xmax)) +
		xlab("Longitude") +
		scale_y_continuous(
			breaks = c(70, 60, 50, 40),
			labels = c("70˚N", "60˚N", "50˚N", "40˚N"),
			name = "Latitude",
		 limits = c(ymin, ymax)
		) +
		theme_bw() +
		theme(
					panel.background = element_rect(fill = "transparent", color = NA),
					plot.background = element_rect(fill = "transparent", color = NA),
					axis.text = element_text(size = 10, colour = "white"),
  	  		axis.ticks = element_line(colour = "white"),
  	  		axis.line = element_blank(),
  	  		axis.title = element_text(size= 12, color = "white"),
  	  		panel.border = element_rect(fill = NA, color = "white"),
					panel.grid = element_blank())
	
	 ggsave("./scripts/manuscript figure scripts/used in ms/pngs of figs/NEP_map.tiff",
			 NEP_map, dpi = 500,
			 width = 5, height = 4, units = "in")

	# add names
	NEP_map_form <- 
		NEP_map +
		annotate("text", x = 207, y = 65, label = "Alaska", size = 3, color = "black") +
		annotate("text", x = 237, y = 60, label = "Canada", size = 3, color = "black") +
		annotate("text", x = 240, y = 42, label = "USA", size = 3, color = "black") +
		annotate("text", x = 160, y = 65, label = "Russia", size = 3, color = "black") +
		annotate("text", x = 190, y = 57.25, label = "eastern\nBering Sea", size = 3, color = "white") 
	
 ggsave("./scripts/manuscript figure scripts/used in ms/pngs of figs/NEP_map_form.tiff",
			 NEP_map_form, dpi = 500,
			 width = 5, height = 4, units = "in")
	  

