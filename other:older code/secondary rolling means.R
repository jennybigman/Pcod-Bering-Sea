
	#### rolling means and sds ####

	
	yr_stats_proj <- ROMS_projected_dat %>%
		group_by(simulation, projection, year) %>%
		summarise(mean_sp_hab_suit = mean(sp_hab_suit),
							sd_sp_hab_suit = sd(sp_hab_suit),
							CV = sd_sp_hab_suit/mean_sp_hab_suit)
	
	yr_stats_proj <- tidyr::unite(yr_stats_proj,"sim_proj",
										simulation, projection, remove = F)
	
	new_df <- function(x){
		new_dat <- yr_stats_proj %>% filter(simulation == x)
		new_dat
	}

	sims <- list("cesm", "gfdl", "miroc")

	dfs <- lapply(sims, new_df)
	
	cesm_df <- dfs[[1]]
	gfdl_df <- dfs[[2]]
	miroc_df <- dfs[[3]]
	
# projections ####
  
	for(i in dfs){
	
  sds_proj <- NA
  
  for(i in 6:615){
  	win <- (i - 5):(i + 5)
  	sds_proj[i] <- sd(yr_stats_proj$mean_sp_hab_suit[win])
  }
  
  means_proj <- NA
  
  for(i in 6:615){
  	win <- (i - 5):(i + 5)
  	means_proj[i] <- mean(yr_stats_proj$mean_sp_hab_suit[win])
  }
	}
	
	
  
 	years_proj_hist <- (c(1980:2014))
	years_proj_scenario <- c(rep(c(2015:2099), 2))
	years_proj <- c(years_proj_hist, years_proj_scenario)
	years_proj_all <- rep(years_proj, 3)
	
	
	sims <- c("cesm", "gfdl", "miroc")
	simulations <- list()
	
	for(i in sims){
		simulations[[i]] <- rep(i, times = 205)
		simulations
	}
	
	simulation <- as.data.frame(bind_rows(simulations)) %>%
		gather(., key = simulation)
	
 
	scens <- list("ssp126", "ssp585")
	scenarios <- list()
	
	for(i in scens){
		scenarios[[i]] <- rep(i, times = (85))
		scenarios
	}
	
	scenarios <- as.vector(c(scenarios[[1]], scenarios[[2]]))
	scenario_pre <- c(rep("historical", 35), scenarios)
	scenario <- rep(scenario, 3)
	
  rolling_stats_proj <- as.data.frame(cbind(sds_proj, means_proj, years_proj, simulation, 
  																					scenario)) %>% 
  	dplyr::select(-value) %>%
  	na.omit()

  
 ##  plots 
  
  rolling_stats_proj <- tidyr::unite(rolling_stats_proj,"sim_proj",
													simulation, scenario, remove = F)

	rolling_stats_proj_plot <- rolling_stats_proj %>%
		filter(!str_detect(sim_proj, "_historical"))
	
	hist_data <- rolling_stats_proj %>%
		filter(str_detect(sim_proj, "_historical"))

	colors <- c("#efd966", "#b79a00", 
						  "#7fb27f", "#004700", 
						  "#6666b2", "#000059")

	sim_proj <- unique(rolling_stats_proj_plot$sim_proj)
	
	names(colors) <- unique(rolling_stats_proj_plot$sim_proj)
	
		
  ## plot together
		
	rolling_mean_plot_form <-    
   	ggplot(data = rolling_stats_hind) +
	 	geom_line(aes(years_hind, means_hind), alpha = 0.5) +
		geom_line(data = rolling_stats_proj_plot, 
							aes(years_proj, means_proj, color = sim_proj, group = sim_proj), alpha = 0.5) +
		facet_wrap(~ simulation) +
		geom_line(data = hist_data, 
							aes(years_proj, means_proj, color = "lightgrey"), alpha = 0.5) +
		xlab("Year") +
		scale_color_manual(name = "sim_proj", values = colors) +
	  scale_y_continuous(
	  	name = "11-year rolling mean",
	  	breaks = c(0.3, 0.4, 0.5, 0.6),
	  	labels = c(0.3, 0.4, 0.5, 0.6)
	  ) +
   	xlim(1970, 2100) +
    theme_bw() +
  	theme(legend.position = "none") +
  	theme(
			strip.background = element_blank(),
  		strip.text = element_text(size = 18, face = "bold"),
			axis.text.y = element_text(size = 16, colour = "grey50"),
  	  axis.ticks.y = element_line(colour = "grey50"),
  	  axis.line.y = element_line(colour = "grey50"),
  	  axis.title.y = element_text(size=18, color = "grey30"),
			axis.text.x = element_blank(),
			axis.ticks.x = element_blank(),
			axis.line.x = element_blank(),
			axis.title.x = element_blank(),
  	  panel.grid.major = element_blank(),
  	  panel.grid.minor = element_blank(),
  	  panel.border = element_rect(fill = NA, color = "grey50"))
	
	rolling_sd_plot_form <-    
   	ggplot(data = rolling_stats_hind) +
	 	geom_line(aes(years_hind, sds_hind), alpha = 0.5) +
		geom_line(data = rolling_stats_proj_plot, 
							aes(years_proj, sds_proj, color = sim_proj, group = sim_proj), alpha = 0.5) +
		facet_wrap(~ simulation) +
		geom_line(data = hist_data, 
							aes(years_proj, sds_proj, color = "lightgrey"), alpha = 0.5) +
		xlab("Year") +
		scale_color_manual(name = "sim_proj", values = colors) +
	  scale_y_continuous(
	  	name = "11-year rolling\n standard deviation",
	  	breaks = c(0.05, 0.10, 0.15, 0.20),
	  	labels = c(0.05, 0.10, 0.15, 0.20)
	  ) +
   	xlim(1970, 2100) +
    theme_bw() +
  	theme(legend.position = "none") +
  	theme(
			strip.background = element_blank(),
  		strip.text = element_blank(),
			axis.text = element_text(size = 16, colour = "grey50"),
  	  axis.ticks = element_line(colour = "grey50"),
  	  axis.line = element_line(colour = "grey50"),
  	  axis.title = element_text(size=18, color = "grey30"),
  	  panel.grid.major = element_blank(),
  	  panel.grid.minor = element_blank(),
  	  panel.border = element_rect(fill = NA, color = "grey50"))
	
	
	rolling_stats_plot <- rolling_mean_plot_form/rolling_sd_plot_form + plot_layout(heights = 1, 1.1)
	
		ggsave("./output/plots/rolling_stats_plot.png",
			 rolling_stats_plot,
			 width = 13, height = 7.5, units = "in")
  
  
	
