# just a time series of spawning habitat suitability


	light_black <- "#306364"
	

	#### habitat suitability ####
	
	yearly_hab_dat_hind <- ROMS_hindcast_dat %>%
	#	filter(year < 2019) %>%
		group_by(year) %>%
    summarise(mean_hab_suit = mean(sp_hab_suit))
	
	years_proj <- 2020:2099

	yearly_hab_dat_proj <- ROMS_projected_dat %>% 
		filter(year %in% years_proj) %>%
		group_by(simulation, projection, year) %>%
   	summarise(mean_hab_suit = mean(sp_hab_suit_var))

	yearly_hab_dat_proj$scen <- NA
		
	yearly_hab_dat_proj$scen[yearly_hab_dat_proj$projection == "SSP126"] <- "Low emission (SSP126)"
	yearly_hab_dat_proj$scen[yearly_hab_dat_proj$projection == "SSP585"] <- "High emission (SSP585)"
	
	yearly_hab_dat_proj <- tidyr::unite(yearly_hab_dat_proj,"sim_proj",
															 simulation, projection, remove = F)

	colors <- c("#6dc3a9", "#ffabab", # cesm low, cesm high
						  "#4e8d9c", "#ff4040", # gfdl low, gfdl high
						  "#97c3e5", "#ffb733") # miroc low, miroc high

	sim_proj <- unique(yearly_hab_dat_proj$sim_proj)
	
	names(colors) <- unique(yearly_hab_dat_proj$sim_proj)
	
	## rolling means of spawning habitat suitability ##
	
	# hind
	means_hind <- NA
  
  for(i in 6:51){
  	win <- (i - 5):(i + 5)
  	means_hind[i] <- mean(yearly_hab_dat_hind$mean_hab_suit[win])
  }
  
	years_hind <- c(1970:2020) 
	
	rolling_mean_habsuit_hind <- as.data.frame(cbind(years_hind, means_hind)) 
	
	# proj
	yearly_hab_dat_proj_sum <- yearly_hab_dat_proj %>%
		group_by(year, scen) %>%
		summarise(mean_hab_suit = mean(mean_hab_suit))
	
	yearly_hab_dat_proj_sum_low <- yearly_hab_dat_proj_sum %>%
		filter(scen == "Low emission (SSP126)")
	
	means_proj_low <- NA
  
  for(i in 6:80){
  	win <- (i - 5):(i + 5)
  	means_proj_low[i] <- mean(yearly_hab_dat_proj_sum_low$mean_hab_suit[win])
  }
	
	yearly_hab_dat_proj_sum_high <- yearly_hab_dat_proj_sum %>%
		filter(scen == "High emission (SSP585)")
	
	means_proj_high <- NA
  
  for(i in 6:80){
  	win <- (i - 5):(i + 5)
  	means_proj_high[i] <- mean(yearly_hab_dat_proj_sum_high$mean_hab_suit[win])
  }
	
	years_proj <- c(2020:2099) 
	
	rolling_mean_habsuit_low <- as.data.frame(cbind(years_proj, means_proj_low)) %>%
		mutate(scen = "Low emission (SSP126)") %>%
		rename(means_proj = means_proj_low)

	rolling_mean_habsuit_high <- as.data.frame(cbind(years_proj, means_proj_high)) %>%
		mutate(scen = "High emission (SSP585)")  %>%
		rename(means_proj = means_proj_high)
	
	rolling_mean_habsuit_proj <- rbind(rolling_mean_habsuit_low, 
																		 rolling_mean_habsuit_high)
	
	rolling_mean_habsuit_proj$scen_f <- as.factor(rolling_mean_habsuit_proj$scen)
	
	# order facets
	yearly_hab_dat_proj$scen_f = factor(yearly_hab_dat_proj$scen, 
																			levels=c('Low emission (SSP126)',  
																							 'High emission (SSP585)'))
	
	rolling_mean_habsuit_proj$scen_f = factor(rolling_mean_habsuit_proj$scen, 
																			levels=c('Low emission (SSP126)',  
																							 'High emission (SSP585)'))
	
	#### habitat suitability plot ####
	
	habsuit_plot <- 
	 	ggplot(data = yearly_hab_dat_proj, aes(x = year, y = mean_hab_suit)) +
		geom_line(data = rolling_mean_habsuit_hind, 
   						aes(x = years_hind, y = means_hind), 
   						color = light_black) +
		geom_line(data = rolling_mean_habsuit_proj, 
   						aes(x = years_proj, y = means_proj), 
   						color = light_black) +
   	geom_line(data = yearly_hab_dat_hind, 
   						aes(x = year, y = mean_hab_suit), 
   						color = "black", alpha = 0.3) +
		geom_line(data = yearly_hab_dat_proj,
							aes(year, mean_hab_suit, 
									group = sim_proj, 
									color = sim_proj), alpha = 0.3) +
		facet_wrap(~ scen_f) +
		xlab("Year") + 
		geom_segment(x = 2020, y = 0.1, xend = 2020, yend = 0.6,
				color = "darkgrey", size = 0.5) +		
		scale_color_manual(name = "sim_proj", values = colors) +
	  scale_y_continuous(
	  	name = "Spawning habitat\nsuitability index",
	  	breaks = c(0.20, 0.40, 0.60),
	  	labels = c(0.20, 0.40, 0.60),
	  	limits = c(0.2, 0.65)
	  ) +
		scale_x_continuous(
	  	name = "Year",
	  	breaks = c(1980, 2030, 2080),
	  		  	limits = c(1970, 2115)) +
	theme(
				strip.background = element_blank(),
				strip.text = element_text(size = 16),
				panel.spacing = unit(0, "lines"),
  			legend.position = "none",
  			panel.background = element_rect(fill = NA),
  			axis.text = element_text(size = 12),
  			axis.ticks.y = element_line(),
				axis.ticks.x = element_blank(),
  			axis.line = element_blank(),
  			axis.title =  element_text(size = 16),
  			panel.grid.major = element_blank(),
  			panel.grid.minor = element_blank(),
  			panel.border = element_rect(fill = NA, color = "grey50"),
				plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"))

	model_ids_low <- tibble(
		year = c(2110, 2110, 2110, 1995), 
		mean_hab_suit = c(0.46, 0.35, 0.27, 0.45), 
		lab = c("CESM", "GFDL", "MIROC", "hindcast"),
		scen_f = factor("Low emission (SSP126)"),
		cols = c("#6dc3a9", "#4e8d9c", "#97c3e5", "black"))
	
	model_ids_high <- tibble(
		year = c(2110, 2110, 2110, 1994), 
		mean_hab_suit = c(0.5, 0.4, 0.6, 0.45), 
		lab = c("CESM", "GFDL", "MIROC", "hindcast"),
		scen_f = factor("High emission (SSP585)"),
		cols = c("#ffabab", "#ff4040", "#ffb733", "black"))

	habsuit_plot_form <- 
		habsuit_plot +
		geom_text(data = model_ids_low,
							label = model_ids_low$lab,
							color = model_ids_low$cols, 
							size = 5,
							alpha = 0.5) + 
		geom_text(data = model_ids_high,
							label = model_ids_high$lab,
							color = model_ids_high$cols, 
							size = 5,
							alpha = 0.5) 
		
	ggsave(habsuit_plot_form, filename = "./output/plots/habsuit_plot_form.png",
				 height = 5, width = 10, units = "in")
	


	