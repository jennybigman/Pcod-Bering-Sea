# figure 2 -- time series of spawning habitat suitability

	#### spawning habitat suitability index ####
	
	# yearly ####
	
	yearly_hab_dat_hind <- ROMS_hindcast_dat %>%
		group_by(year) %>%
    summarise(mean_hab_suit = mean(sp_hab_suit))
	
	years_proj <- 2020:2099

	yearly_hab_dat_proj <- ROMS_projected_dat %>% 
		filter(year %in% years_proj) %>%
		group_by(simulation, projection, year) %>%
   	summarise(mean_hab_suit = mean(sp_hab_suit_var)) 

	yearly_hab_dat_proj$scen <- NA
		
	yearly_hab_dat_proj$scen[yearly_hab_dat_proj$projection == "ssp126"] <- "low emission (ssp126)"
	yearly_hab_dat_proj$scen[yearly_hab_dat_proj$projection == "ssp585"] <- "high emission (ssp585)"
	
	yearly_hab_dat_proj <- tidyr::unite(yearly_hab_dat_proj,"sim_proj",
															 simulation, projection, remove = F)

	colors <- c("#6dc3a9", "#ffabab", # cesm low, cesm high
						  "#4e8d9c", "#ff4040", # gfdl low, gfdl high
						  "#97c3e5", "#ffb733") # miroc low, miroc high

	sim_proj <- unique(yearly_hab_dat_proj$sim_proj)
	
	names(colors) <- unique(yearly_hab_dat_proj$sim_proj)
	
	# order facets
	yearly_hab_dat_proj$scen_f = factor(yearly_hab_dat_proj$scen, levels=c('low emission (ssp126)', 
									
																																																															 'high emission (ssp585)'))
	#### rolling means of hab suit ####
	
	# hind
	means_hind <- NA
  
  for(i in 6:51){
  	win <- (i - 5):(i + 5)
  	means_hind[i] <- mean(yearly_hab_dat_hind$mean_hab_suit[win])
  }
  
	years_hind <- c(1970:2020) # does this need to be 1980?
	
	rolling_mean_hind <- as.data.frame(cbind(years_hind, means_hind)) 
	
	# proj
	yearly_hab_dat_proj_sum <- yearly_hab_dat_proj %>%
		group_by(year, scen_f) %>%
		summarise(mean_hab_suit = mean(mean_hab_suit))
	
	yearly_hab_dat_proj_sum_low <- yearly_hab_dat_proj_sum %>%
		filter(scen_f == "low emission (ssp126)")
	
	means_proj_low <- NA
  
  for(i in 6:80){
  	win <- (i - 5):(i + 5)
  	means_proj_low[i] <- mean(yearly_hab_dat_proj_sum_low$mean_hab_suit[win])
  }
	
	yearly_hab_dat_proj_sum_high <- yearly_hab_dat_proj_sum %>%
		filter(scen_f == "high emission (ssp585)")
	
	means_proj_high <- NA
  
  for(i in 6:80){
  	win <- (i - 5):(i + 5)
  	means_proj_high[i] <- mean(yearly_hab_dat_proj_sum_high$mean_hab_suit[win])
  }
	
	years_proj <- c(2020:2099) 
	
	rolling_mean_proj <- as.data.frame(cbind(years_proj, means_proj_low, means_proj_high)) 
	
	#### plot ####
	
	low_dat <- yearly_hab_dat_proj %>%
		filter(., scen == "low emission (ssp126)")
	
	low_habsuit_time <-    
   	ggplot() +
		geom_line(data = rolling_mean_hind, 
   						aes(x = years_hind, y = means_hind), 
   						color = "#e5e5e5") +
		geom_line(data = rolling_mean_proj, 
   						aes(x = years_proj, y = means_proj_low), 
   						color = "#e5e5e5") +
   	geom_line(data = yearly_hab_dat_hind, 
   						aes(x = year, y = mean_hab_suit), 
   						color = "black", alpha = 0.5) +
		geom_line(data = low_dat,
							aes(year, mean_hab_suit, 
									group = sim_proj, 
									color = sim_proj), alpha = 0.5) +
		xlab("Year") + 
		geom_vline(aes(xintercept = 2020, color = "#f2f2f2"), alpha = 0.5) +
		scale_color_manual(name = "sim_proj", values = colors) +
	  scale_y_continuous(
	  	name = "Cross-shelf annual spawning\nhabitat suitability index",
	  	breaks = c(0.20, 0.40, 0.60),
	  	labels = c(0.20, 0.40, 0.60),
	  	limits = c(0.2, 0.65)
	  ) +
		scale_x_continuous(
	  	name = "Year",
	  	breaks = c(1980, 2030, 2080),
	  		  	limits = c(1970, 2110)) +
		ggtitle("low emission (ssp126)") +
		labs(tag = "(a)") +
   	theme_bw() +
  	theme(legend.position = "none") +
  	theme(
  		plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
			axis.text = element_text(size = 12, colour = "grey50"),
  	  axis.ticks = element_line(colour = "grey50"),
  	  axis.line = element_line(colour = "grey50"),
  	  axis.title = element_text(size=14, color = "grey50"),
  	  panel.grid.major = element_blank(),
  	  panel.grid.minor = element_blank(),
  	  panel.border = element_rect(fill = NA, color = "grey50"),
			plot.tag.position = c(0.125, 0.9))
	
	low_habsuit_time_lab <- low_habsuit_time +
			annotate(geom = "text", x = 2107, y = 0.45,
           label = "cesm",
           color = "#6dc3a9", size = 4) +
				annotate(geom = "text", x = 2107, y = 0.30,
           label = "gfdl",
           color = "#4e8d9c", size = 4) +
				annotate(geom = "text", x = 2107, y = 0.37,
           label = "miroc",
           color = "#97c3e5", size = 4)  +
				annotate(geom = "text", x = 1995, y = 0.39,
           label = "hindcast",
           color = "black", alpha = 0.5, size = 4)

	high_dat <- yearly_hab_dat_proj %>%
		filter(., scen == "high emission (ssp585)")
	
	high_habsuit_time <-    
   	ggplot() +
		geom_line(data = rolling_mean_hind, 
   						aes(x = years_hind, y = means_hind), 
   						color = "#e5e5e5") +
		geom_line(data = rolling_mean_proj, 
   						aes(x = years_proj, y = means_proj_high), 
   						color = "#e5e5e5") +
   	geom_line(data = yearly_hab_dat_hind, 
   						aes(x = year, y = mean_hab_suit), 
   						color = "black", alpha = 0.5) +
		geom_line(data = high_dat,
							aes(year, mean_hab_suit, 
									group = sim_proj, 
									color = sim_proj), alpha = 0.5) +
		xlab("Year") + 
		geom_vline(aes(xintercept = 2020, color = "#f2f2f2"), alpha = 0.5) +
		scale_color_manual(name = "sim_proj", values = colors) +
	  scale_y_continuous(
	  	name = "Cross-shelf annual spawning\nhabitat suitability index",
	  	breaks = c(0.20, 0.40, 0.60),
	  	labels = c(0.20, 0.40, 0.60),
	  	limits = c(0.2, 0.65)
	  ) +
		scale_x_continuous(
	  	name = "Year",
	  	breaks = c(1980, 2030, 2080),
	  	limits = c(1970, 2110)) +
   	theme_bw() +
  	theme(legend.position = "none") +
		ggtitle("high emission (ssp585)") +
		labs(tag = "(b)") +
  	theme(
  		plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
			axis.text.x = element_text(size = 12, colour = "grey50"),
  	  axis.ticks.x = element_line(colour = "grey50"),
  	  axis.line = element_line(colour = "grey50"),
  	  axis.title.x = element_text(size=14, color = "grey30"),
			axis.text.y = element_blank(),
			axis.ticks.y = element_blank(),
			axis.title.y = element_blank(),
  	  panel.grid.major = element_blank(),
  	  panel.grid.minor = element_blank(),
  	  panel.border = element_rect(fill = NA, color = "grey50"),
			plot.tag.position = c(0.03, 0.9))
	
	high_habsuit_time_lab <- high_habsuit_time +
			annotate(geom = "text", x = 2107, y = 0.46,
           label = "cesm",
           color = "#ffabab", size = 4) +
				annotate(geom = "text", x = 2107, y = 0.5,
           label = "gfdl",
           color = "#ff4040", size = 4) +
				annotate(geom = "text", x = 2107, y = 0.62,
           label = "miroc",
           color = "#ffb733", size = 4) +
				annotate(geom = "text", x = 1994, y = 0.39,
           label = "hindcast",
           color = "black", alpha = 0.5, size = 4)
		
	# plot together side by side
	
	plot1 <- low_habsuit_time_lab + theme(plot.margin = unit(c(0.2, 0, 0.2, 0.2), "in"))
	plot2 <- high_habsuit_time_lab + theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0), "in"))

		
	time_series_habsuit <- plot1 + plot2 +
		plot_layout( widths = c(1, 1.1))
	
	ggsave("./output/plots/time_series_habsuit.png",
			 time_series_habsuit,
			 width = 15, height = 5, units = "in")
	