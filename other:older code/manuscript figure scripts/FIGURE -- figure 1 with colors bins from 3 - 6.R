### possible figure 1: map of historical temps, map of projected temps, thermal hatch success curve

	### panel a: hatch success & temperature relationship ####
	
	# load in data
	eggs <- read.csv("../data and code from Laurel & Rogers 2020 CJFAS/laurelandrogerscode/Pcod and WP hatch probabilities.csv")
	
	eggs<-eggs[,1:8]
	eggs$Ntot<-eggs$Hatched+eggs$Dead
	eggs$Phatch<-eggs$Hatched/(eggs$Hatched+eggs$Dead)
	eggs<-eggs[eggs$Species=="Pcod",] #Pcod
	eggs$pMax<-eggs$Phatch/max(eggs$Phatch)
	
	res2 <- nls( Phatch ~ k/(1+((Temp_act-mu)/sigma)^2), start=c(mu=5,sigma=2,k=1) , data = eggs, weights=Ntot)
	v2 <- summary(res2)$parameters[,"Estimate"]

	cauchy <- function(x) v2[3]/(1+((x-v2[1])/v2[2])^2)

	therm_response_curve <- 
		ggplot(eggs) +
		geom_point(aes(x = Temp_act, y = Phatch), color = "black", size = 2) +
		geom_function(fun = cauchy, aes(col = "Temp_act"), color = "black", size = 0.5) +
		xlab("Temperature (˚C)") +
		scale_y_continuous(
			name = "Proportion\nsuccessful hatch") +
		#ggtitle("Thermal response of hatch success") +
	#	labs(tag = "(a)") +
		theme_bw() +
		theme(legend.position = "none",
					#plot.title = element_text(hjust = 0.5),
				#	plot.tag.position = c(0.05, 0.87),
  				axis.text = element_text(size= 12, colour = "grey50"),
  				axis.title = element_text(size=14, color = "grey50"),
  				axis.line = element_line(color = "grey50"),
  				axis.ticks = element_line(colour = "grey50"),
					panel.grid = element_blank())

	#### panel b: map of historical averaged temps in march ####

	years_hind <- 1970:1989
	
	march_temp_hind <- ROMS_hindcast_dat %>%
		filter(year %in% years_hind) %>%
		filter(month == 3) %>%
		group_by(latitude, long_not_360) %>%
		summarize(mean_temp = mean(temp)) %>%
		st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326, remove = FALSE)

	march_avg_temps_hind <-
		ggplot() +
		geom_sf(data = march_temp_hind, aes(color = mean_temp))  +
		geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
		coord_sf(crs = 3338) +
		binned_scale(
			aesthetics = "color",
			scale_name = "stepsn",
			palette = function(x) c("#666666", "#66b266", "#666666"),
			breaks = c(3, 6),
			limits = c(-2.5, 12.5),
			show.limits = TRUE,
			nice.breaks = TRUE,
			guide = "colorsteps") +
 		scale_x_continuous(
 		 breaks = c(-170, -160),
		 labels = c("-170˚", "-160˚"),
		 limits = c(-1400000, 10000),
 			name = "Longitude") +
 		scale_y_continuous(
 			breaks = breaks_y,
 			limits = limits_y,
 			name = "Latitude") +
    labs(colour = "Mean\nbottom\ntemperature\n(˚C)") +
		#labs(tag = "(b)") +
		#ggtitle("Historical:\n1970 - 1999") +
		theme_bw() +
		theme(legend.position = "none",
				#	plot.title = element_text(hjust = 0.5),
				#	plot.tag.position = c(0.2, 0.87),
					axis.text.x = element_text(size = 12, colour = "grey50"),
  	  		axis.ticks.x = element_line(colour = "grey50"),
  	  		axis.line.x = element_line(colour = "grey50"),
					axis.text.y = element_blank(),
					axis.ticks.y = element_blank(),
					axis.title.y = element_blank(),
  	  		axis.title.x = element_text(size=14, color = "grey50"),
  	  		panel.border = element_rect(fill = NA, color = "grey50"))
	
	#### panel c and d: map of projected averaged temps in march avg across models ####

	years_proj <- 2080:2099
	
	march_temp_proj <- ROMS_projected_dat %>%
		filter(year %in% years_proj) %>%
		filter(month == 3) %>%
		group_by(projection, latitude, long_not_360) %>%
		summarize(mean_temp = mean(bc_temp_sd)) %>%
		st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326, remove = FALSE)
	
	# low emissions scenario
	march_temp_proj_ssp126 <- march_temp_proj %>%
		filter(projection == "ssp126")

	march_avg_temps_proj_ssp126 <-
		ggplot() +
		geom_sf(data = march_temp_proj_ssp126, aes(color = mean_temp))  +
		geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
		coord_sf(crs = 3338) +
		binned_scale(
			aesthetics = "color",
			scale_name = "stepsn",
			palette = function(x) c("#666666", "#66b266", "#666666"),
			breaks = c(3, 6),
			limits = c(-2.5, 12.5),
			show.limits = TRUE,
			nice.breaks = TRUE,
			guide = "colorsteps") +
		scale_x_continuous(
 		 breaks = c(-170, -160),
		 labels = c("-170˚","-160˚"),
		 limits = c(-1400000, 10000),
 			name = "Longitude") +
 		scale_y_continuous(
 			breaks = breaks_y,
 			limits = limits_y,
 			name = "Latitude") +
    labs(colour = "Mean\nbottom\ntemperature\n(˚C)") +
		#labs(tag = "(c)") +
		#ggtitle("Low emission (ssp126):\n2080 - 2099") +
		theme_bw() +
		white_map_theme() +
		theme(legend.position = "none",
				#	plot.tag.position = c(0.06, 0.87),
					#plot.title = element_text(hjust = 0.5),
					axis.text.x = element_text(size = 12, colour = "grey50"),
  	  		axis.ticks.x = element_line(colour = "grey50"),
  	  		axis.line.x = element_line(colour = "grey50"),
  	  		axis.title.x = element_text(size=14, color = "grey50"),
					axis.text.y = element_blank(),
  	  		axis.ticks.y = element_blank(),
  	  		axis.line.y = element_blank(),
  	  		axis.title.y = element_blank(),
  	  		panel.border = element_rect(fill = NA, color = "grey50"))
	
	# high emissions scenario
	
	march_temp_proj_ssp585 <- march_temp_proj %>%
		filter(projection == "ssp585")

	march_avg_temps_proj_ssp585 <-
		ggplot() +
		geom_sf(data = march_temp_proj_ssp585, aes(color = mean_temp))  +
		geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
		coord_sf(crs = 3338, label_graticule = "E") +
		binned_scale(
			aesthetics = "color",
			scale_name = "stepsn",
			palette = function(x) c("#666666", "#66b266", "#666666"),
			breaks = c(3, 6),
			limits = c(-2.5, 12.5),
			show.limits = TRUE,
			nice.breaks = TRUE,
			guide = "colorsteps") + 		
		scale_x_continuous(
 		 breaks = c(-170, -160),
		 labels = c("-170˚", "-160˚"),
		 limits = c(-1400000, 10000),
 			name = "Longitude") +
 		scale_y_continuous(
 			position = "right",
 			breaks = c(55, 60, 65),
 			limits = limits_y,
 			name = "Latitude") +
    labs(colour = "Mean\nbottom\ntemperature\n(˚C)") +
		#labs(tag = "(d)") +
		#ggtitle("High emission (ssp585):\n2080 - 2099") +
		theme_bw() +
		white_map_theme() +
		theme(legend.title.align = 0.5,
				#	legend.position = c(0.89, 0.72),
					legend.background = element_blank(),
					legend.title = element_text(size = 9),
					legend.text = element_text(size = 8),
					plot.tag.position = c(0.06, 0.87),
				#	plot.title = element_text(hjust = 0.5),
					axis.text = element_text(size = 12, colour = "grey50"),
  	  		axis.ticks = element_line(colour = "grey50"),
  	  		axis.line = element_line(colour = "grey50"),
  	  		axis.title = element_text(size=14, color = "grey50"),
  	  		panel.border = element_rect(fill = NA, color = "grey50"))
	
 #### plot together ####
	
 plot1 <- therm_response_curve + theme(plot.margin = unit(c(0.2, 0, 0.2, 0.2), "in"))

 plot2 <- march_avg_temps_hind + theme(plot.margin = unit(c(0.2, 0, 0.2, -0.05), "in"))
 
 plot3 <- march_avg_temps_proj_ssp126 + theme(plot.margin = unit(c(0.2, 0, 0.2, -0.05), "in"))
 
 plot4 <- march_avg_temps_proj_ssp585 + theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0), "in"))
 
 temp_response_plot <- 
 	plot1 + plot_spacer() 
 
 temp_map_plots <-
 	plot2 + plot_spacer() +
 	plot3 + plot_spacer() +
 	plot4 +
 	plot_layout(nrow = 1, widths = c(1, -0.065, 1, -0.065, 1),
 							heights = c(1, 1, 1, 1))

 ggsave("./output/plots/Figure1_other.png",
			 Figure1_other,
			 width = 16, height = 8, units = "in")
	

 #### another way ####
 
 plot1 <- therm_response_curve + theme(plot.margin = unit(c(0.2, 0, 0.2, 0.2), "in"))

 plot2 <- march_avg_temps_hind + theme(plot.margin = unit(c(0.2, 0, 0.2, -0.08), "in"))
 
 plot3 <- march_avg_temps_proj_ssp126 + theme(plot.margin = unit(c(0.2, 0, 0.2, 0), "in"))
 
 plot4 <- march_avg_temps_proj_ssp585 + theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0), "in"))
 
 Figure1_other <- 
 	plot1 + plot2 + plot3 + plot4 + 
 	plot_layout(ncol = 4)

 ggsave("./output/plots/Figure1_other.png",
			 Figure1_other,
			 width = 16, height = 8, units = "in")
	
