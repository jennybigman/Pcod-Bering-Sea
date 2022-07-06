 # rolling sds with 5 year time window

# figure -- rolling means and sds of spawning habitat suitability

	#### summarize data by year #### 
	yr_stats_hind <- ROMS_hindcast_dat %>%
		group_by(year) %>%
		summarise(mean_sp_hab_suit = mean(sp_hab_suit),
							sd_sp_hab_suit = sd(sp_hab_suit))
	
	yr_stats_proj <- ROMS_projected_dat %>%
		group_by(projection, year) %>%
		summarise(mean_sp_hab_suit_var = mean(sp_hab_suit_var))
	
	yr_stats_proj <- tidyr::unite(yr_stats_proj,"sim_proj",
										simulation, projection, remove = F)

	sim_proj <- yr_stats_proj$sim_proj
	
	
	#### calculate rolling mean of mean and sd with 5 year window ####
	
	# hindcast ####
	
  sds_hind <- NA
  
  for(i in 6:51){
  	win <- (i - 5):(i + -1)
  	sds_hind[i] <- sd(yr_stats_hind$mean_sp_hab_suit[win])
  }
  
  means_hind <- NA
  
  for(i in 6:51){
  	win <- (i - 5):(i + -1)
  	means_hind[i] <- mean(yr_stats_hind$mean_sp_hab_suit[win])
  }
  
	years_hind <- c(1970:2020) 
	
  rolling_sd_hind <- as.data.frame(cbind(years_hind, sds_hind))
  
	rolling_mean_hind <- as.data.frame(cbind(years_hind, means_hind)) 

  rolling_stats_hind <- merge(rolling_mean_hind, rolling_sd_hind, by = "years_hind") %>%
  		rename(
					 year = years_hind,
					 sds = sds_hind,
					 means = means_hind)
		
  
 # projections #####
		
	years_proj <- 2020:2099
	
	yr_stats_proj_trim <- yr_stats_proj %>%
		filter(year %in% years_proj)
	
	rolling_proj_func <- function(x){
 	
 		new_dat <- yr_stats_proj_trim %>%
 			filter(projection == x)
 		
  	sds_proj <- NA
  	
  	for(i in 6:length(new_dat$year)){
  		win <- (i - 5):(i + -1)
  		sds_proj[i] <- sd(new_dat$mean_sp_hab_suit_var[win])
  	}
  	
  	means_proj <- NA
  	
  	for(i in 6:length(new_dat$year)){
  		win <- (i - 5):(i + -1)
  		means_proj[i] <- mean(new_dat$mean_sp_hab_suit_var[win]) }
  		
  	data.frame(sds_proj, means_proj, years_proj, x)
  	
  	}
	
	projs <- unique(yr_stats_proj_trim$projection)
	

	rolling_stats_proj <- 
		lapply(projs, rolling_proj_func) %>% 
		bind_rows() %>%
		rename(projection = x,
					 year = years_proj,
					 sds = sds_proj,
					 means = means_proj)
		
	# set up plotting ####
	
	rolling_stats_proj$scen <- NA
		
	rolling_stats_proj$scen[rolling_stats_proj$projection == "ssp126"] <- "low emission\n(ssp126)"
	rolling_stats_proj$scen[rolling_stats_proj$projection == "ssp585"] <- "high emission\n(ssp585)"
	
	# order facets
	rolling_stats_proj$scen_f = factor(rolling_stats_proj$scen, 
																		 levels=c('low emission\n(ssp126)', 'high emission\n(ssp585)'))
									
	#### plot ####
	
	rolling_5sd_plot <-   
		ggplot() +
	 	geom_line(data = rolling_stats_hind, aes(year, sds), alpha = 0.5) +
		geom_line(data = rolling_stats_proj, 
							aes(year, sds, color = projection, group = projection), alpha = 0.5) +
		facet_wrap(~ scen_f) +
		xlab("Year") +
		#scale_color_manual() +
	  scale_y_continuous(
	  	name = "5-year rolling\nstandard deviation",
	  	breaks = c(0, 0.05, 0.1),
	  	labels = c(0, 0.05, 0.1),
	  	limits = c(0, 0.1)) +
		geom_vline(data = rolling_stats_hind, 
							 aes(xintercept = 2020, color = "grey50", alpha = 0.5)) +
   	xlim(1970, 2110) +
    theme_bw() +
  	theme(legend.position = "none") +
		theme(
  		strip.background = element_blank(),
  		strip.text = element_text(size = 18, face = "bold", colour = "grey50"),
			axis.text = element_text(size = 16, colour = "grey50"),
  	  axis.ticks = element_line(colour = "grey50"),
  	  axis.line = element_line(colour = "grey50"),
			axis.title.x = element_text(size = 16, color = "grey50"),
  	  axis.title.y = element_markdown(size=16, color = "grey50"),
  	  panel.grid.major = element_blank(),
  	  panel.grid.minor = element_blank(),
  	  panel.border = element_rect(fill = NA, color = "grey50"))
	
	rolling_5sd_plot_labs <- 
			ggdraw(rolling_5sd_plot) +
			draw_label("cesm", x = 0.523, y = 0.41, color = "#6dc3a9", size = 10, alpha = 0.5) +
			draw_label("gfdl", x = 0.52, y = 0.55, color = "#4e8d9c", size = 10, alpha = 0.5) +
			draw_label("miroc", x = 0.52, y = 0.30, color = "#97c3e5", size = 10,  alpha = 0.5) +
			draw_label("cesm", x = 0.965, y = 0.30, color = "#ffabab", size = 10,  alpha = 0.5) +
			draw_label("gfdl", x = 0.965, y = 0.65, color = "#ff4040", size = 10,  alpha = 0.5) +
			draw_label("miroc", x = 0.965, y = 0.41, color = "#ffb733", size = 10,  alpha = 0.5) 

		
	ggsave(here("./output/plots/rolling_5sd_plot_labs.png"),
			 rolling_5sd_plot_labs,
			 width = 10, height = 5, units = "in")

	### try another way
	
	rolling_hind_sd_plot <-    
   	ggplot() +
	 	geom_line(data = rolling_stats_hind, aes(year, sds), alpha = 0.5) +
		xlab("Year") +
	  scale_y_continuous(
	  	breaks = c(0, 0.05, 0.1),
	  	labels = c(0, 0.05, 0.1),
	  	limits = c(0, 0.11)) +
   	xlim(1975, 2015) +
    theme_bw() +
		ylab("5-year rolling\nstandard deviation") +
  	theme(legend.position = "none") +
		theme(
			axis.text = element_text(size = 8, colour = "grey50"),
  	  axis.ticks = element_line(colour = "grey50"),
  	  axis.line = element_line(colour = "grey50"),
			axis.title = element_text(size = 10, color = "grey50"),
  	  panel.grid.major = element_blank(),
  	  panel.grid.minor = element_blank(),
  	  panel.border = element_rect(fill = NA, color = "grey50"),
			plot.margin = unit(c(0.05,-0.1, 0, 0), "in"))
	
	
	rolling_proj_sd_plot <-    
   	ggplot() +
		geom_line(data = rolling_stats_proj, 
							aes(year, sds, color = sim_proj, group = sim_proj), alpha = 0.5) +
		facet_wrap(~ scen_f) +
		xlab("Year") +
		scale_color_manual(name = "sim_proj", values = colors) +
	  scale_y_continuous(
	  	breaks = c(0, 0.05, 0.1),
	  	labels = c(0, 0.05, 0.1),
	  	limits = c(0, 0.11)) +
   	xlim(2025, 2107) +
    theme_bw() +
  	theme(legend.position = "none") +
		theme(
			panel.spacing = unit(2, "pt"),
  		strip.background = element_blank(),
  		strip.text = element_blank(),
			axis.text.x = element_text(size = 8, colour = "grey50"),
  	  axis.ticks.x = element_line(colour = "grey50"),
  	  axis.line = element_line(colour = "grey50"),
			axis.title.x = element_text(size = 10, color = "grey50"),
  	  axis.title.y = element_blank(),
			axis.text.y = element_blank(),
  	  axis.ticks.y = element_blank(),
  	  panel.grid.major = element_blank(),
  	  panel.grid.minor = element_blank(),
  	  panel.border = element_rect(fill = NA, color = "grey50"),
			plot.margin = unit(c(0.05, 0, 0, -0.05), "in"))
	

	rolling_sds <- rolling_hind_sd_plot + rolling_proj_sd_plot +
		plot_layout(widths = c(0.5, 1))
	
	rolling_sds_form <- 
			ggdraw(rolling_sds) +
			draw_label("hindcast", x = 0.11, y = 0.93, color = "grey50", size = 10) +
			draw_label("projection: low emission (ssp126)", x = 0.503, y = 0.93, color = "grey50", size = 10) +
			draw_label("projection: high emission (ssp585)", x = 0.812, y = 0.93, color = "grey50", size = 10) +
			draw_label("cesm", x = 0.658, y = 0.5, color = "#6dc3a9", size = 10, alpha = 0.5) +
			draw_label("gfdl", x = 0.658, y = 0.63, color = "#4e8d9c", size = 10, alpha = 0.5) +
			draw_label("miroc", x = 0.658, y = 0.38, color = "#97c3e5", size = 10,  alpha = 0.5) +
			draw_label("cesm", x = 0.96, y = 0.28, color = "#ffabab", size = 10,  alpha = 0.5) +
			draw_label("gfdl", x = 0.96, y = 0.42, color = "#ff4040", size = 10,  alpha = 0.5) +
			draw_label("miroc", x = 0.96, y = 0.65, color = "#ffb733", size = 10,  alpha = 0.5) 
	
	
	ggsave("./output/plots/rolling_sds_form.png",
		rolling_sds_form,
		height = 3,
		width = 9)

	
	#### look at all three models combined, not each separately
	
	# hindcast ####
	
  sds_hind <- NA
  
  for(i in 6:51){
  	win <- (i - 5):(i + -1)
  	sds_hind[i] <- sd(yr_stats_hind$mean_sp_hab_suit[win])
  }
  
  means_hind <- NA
  
  for(i in 6:51){
  	win <- (i - 5):(i + -1)
  	means_hind[i] <- mean(yr_stats_hind$mean_sp_hab_suit[win])
  }
  
	years_hind <- c(1970:2020) 
	
  rolling_sd_hind <- as.data.frame(cbind(years_hind, sds_hind))
  
	rolling_mean_hind <- as.data.frame(cbind(years_hind, means_hind)) 

  rolling_stats_hind <- merge(rolling_mean_hind, rolling_sd_hind, by = "years_hind") %>%
  		rename(
					 year = years_hind,
					 sds = sds_hind,
					 means = means_hind)
		
  
 # projections #####
		
	years_proj <- 2020:2099
	
	yr_stats_proj_trim <- yr_stats_proj %>%
		filter(year %in% years_proj)
	
 
	rolling_proj_func <- function(x){
 	
 		new_dat <- yr_stats_proj_trim %>%
 			filter(projection == x)
 		
  	sds_proj <- NA
  	
  	for(i in 6:length(new_dat$year)){
  		win <- (i - 5):(i + -1)
  		sds_proj[i] <- sd(new_dat$mean_sp_hab_suit_var[win])
  	}
  	
  	means_proj <- NA
  	
  	for(i in 6:length(new_dat$year)){
  		win <- (i - 5):(i + -1)
  		means_proj[i] <- mean(new_dat$mean_sp_hab_suit_var[win]) }
  		
  	data.frame(sds_proj, means_proj, years_proj, x)
  	
  	}
	
	projs <- unique(yr_stats_proj_trim$projection)
	
	rolling_stats_proj <- lapply(projs, rolling_proj_func) %>%
		bind_rows() %>%
		rename(projection = x,
					 year = years_proj,
					 sds = sds_proj,
					 means = means_proj)
		
	# set up plotting ####
	
	rolling_stats_proj$scen <- NA
		
	rolling_stats_proj$scen[rolling_stats_proj$projection == "ssp126"] <- "low emission\n(ssp126)"
	rolling_stats_proj$scen[rolling_stats_proj$projection == "ssp585"] <- "high emission\n(ssp585)"
	
	# order facets
	rolling_stats_proj$scen_f = factor(rolling_stats_proj$scen, 
																		 levels=c('low emission\n(ssp126)', 'high emission\n(ssp585)'))
									
																																			 																												 'high emission\n(ssp585)'))
	#### plot ####
	
	rolling_5sd_plot <-    
   	ggplot(data = rolling_stats_hind) +
	 	geom_line(aes(year, sds), alpha = 0.5) +
		geom_line(data = rolling_stats_proj, 
							aes(year, sds, color = projection, group = projection), alpha = 0.5) +
		facet_wrap(~ scen_f) +
		xlab("Year") +
		scale_color_manual(values = c("blue", "red")) +
	  scale_y_continuous(
	  	name = "5-year rolling\nstandard deviation",
	  	breaks = c(0, 0.05, 0.1),
	  	labels = c(0, 0.05, 0.1),
	  	limits = c(0, 0.11)) +
		geom_vline(data = rolling_stats_hind, 
							 aes(xintercept = 2020, color = "grey50", alpha = 0.5)) +
   	xlim(1970, 2110) +
    theme_bw() +
  	theme(legend.position = "none") +
		theme(
  		strip.background = element_blank(),
  		strip.text = element_text(size = 18, face = "bold", colour = "grey50"),
			axis.text = element_text(size = 16, colour = "grey50"),
  	  axis.ticks = element_line(colour = "grey50"),
  	  axis.line = element_line(colour = "grey50"),
			axis.title.x = element_text(size = 16, color = "grey50"),
  	  axis.title.y = element_markdown(size=16, color = "grey50"),
  	  panel.grid.major = element_blank(),
  	  panel.grid.minor = element_blank(),
  	  panel.border = element_rect(fill = NA, color = "grey50"))
	
	rolling_5sd_plot_labs <- 
			ggdraw(rolling_5sd_plot) +
			draw_label("cesm", x = 0.523, y = 0.41, color = "#6dc3a9", size = 10, alpha = 0.5) +
			draw_label("gfdl", x = 0.52, y = 0.55, color = "#4e8d9c", size = 10, alpha = 0.5) +
			draw_label("miroc", x = 0.52, y = 0.30, color = "#97c3e5", size = 10,  alpha = 0.5) +
			draw_label("cesm", x = 0.965, y = 0.30, color = "#ffabab", size = 10,  alpha = 0.5) +
			draw_label("gfdl", x = 0.965, y = 0.65, color = "#ff4040", size = 10,  alpha = 0.5) +
			draw_label("miroc", x = 0.965, y = 0.41, color = "#ffb733", size = 10,  alpha = 0.5) 

		
	ggsave(here("./output/plots/rolling_5sd_plot_labs.png"),
			 rolling_5sd_plot_labs,
			 width = 10, height = 5, units = "in")

	### try another way
	
	rolling_hind_sd_plot <-    
   	ggplot() +
	 	geom_line(data = rolling_stats_hind, aes(year, sds), alpha = 0.5) +
		xlab("Year") +
	  scale_y_continuous(
	  	breaks = c(0, 0.05, 0.1),
	  	labels = c(0, 0.05, 0.1),
	  	limits = c(0, 0.11)) +
   	xlim(1975, 2015) +
    theme_bw() +
		ylab("5-year rolling\nstandard deviation") +
  	theme(legend.position = "none") +
		theme(
			axis.text = element_text(size = 8, colour = "grey50"),
  	  axis.ticks = element_line(colour = "grey50"),
  	  axis.line = element_line(colour = "grey50"),
			axis.title = element_text(size = 10, color = "grey50"),
  	  panel.grid.major = element_blank(),
  	  panel.grid.minor = element_blank(),
  	  panel.border = element_rect(fill = NA, color = "grey50"),
			plot.margin = unit(c(0.05,-0.1, 0, 0), "in"))
	
	
	rolling_proj_sd_plot <-    
   	ggplot() +
		geom_line(data = rolling_stats_proj, 
							aes(year, sds, color = sim_proj, group = sim_proj), alpha = 0.5) +
		facet_wrap(~ scen_f) +
		xlab("Year") +
		scale_color_manual(name = "sim_proj", values = colors) +
	  scale_y_continuous(
	  	breaks = c(0, 0.05, 0.1),
	  	labels = c(0, 0.05, 0.1),
	  	limits = c(0, 0.11)) +
   	xlim(2025, 2107) +
    theme_bw() +
  	theme(legend.position = "none") +
		theme(
			panel.spacing = unit(2, "pt"),
  		strip.background = element_blank(),
  		strip.text = element_blank(),
			axis.text.x = element_text(size = 8, colour = "grey50"),
  	  axis.ticks.x = element_line(colour = "grey50"),
  	  axis.line = element_line(colour = "grey50"),
			axis.title.x = element_text(size = 10, color = "grey50"),
  	  axis.title.y = element_blank(),
			axis.text.y = element_blank(),
  	  axis.ticks.y = element_blank(),
  	  panel.grid.major = element_blank(),
  	  panel.grid.minor = element_blank(),
  	  panel.border = element_rect(fill = NA, color = "grey50"),
			plot.margin = unit(c(0.05, 0, 0, -0.05), "in"))
	

	rolling_sds <- rolling_hind_sd_plot + rolling_proj_sd_plot +
		plot_layout(widths = c(0.5, 1))
	
	rolling_sds_form <- 
			ggdraw(rolling_sds) +
			draw_label("hindcast", x = 0.11, y = 0.93, color = "grey50", size = 10) +
			draw_label("projection: low emission (ssp126)", x = 0.503, y = 0.93, color = "grey50", size = 10) +
			draw_label("projection: high emission (ssp585)", x = 0.812, y = 0.93, color = "grey50", size = 10) +
			draw_label("cesm", x = 0.658, y = 0.5, color = "#6dc3a9", size = 10, alpha = 0.5) +
			draw_label("gfdl", x = 0.658, y = 0.63, color = "#4e8d9c", size = 10, alpha = 0.5) +
			draw_label("miroc", x = 0.658, y = 0.38, color = "#97c3e5", size = 10,  alpha = 0.5) +
			draw_label("cesm", x = 0.96, y = 0.28, color = "#ffabab", size = 10,  alpha = 0.5) +
			draw_label("gfdl", x = 0.96, y = 0.42, color = "#ff4040", size = 10,  alpha = 0.5) +
			draw_label("miroc", x = 0.96, y = 0.65, color = "#ffb733", size = 10,  alpha = 0.5) 
	
	
	ggsave("./output/plots/rolling_sds_form.png",
		rolling_sds_form,
		height = 3,
		width = 9)

	
	#### rolling mean plots ####
	
		
	rolling_hind_mean_plot <-    
   	ggplot() +
	 	geom_line(data = rolling_stats_hind, aes(year, means), alpha = 0.5) +
		xlab("Year") +
	  scale_y_continuous(
	  	breaks = c(0, 0.20, 0.4, 0.6),
	  	labels = c(0, 0.20, 0.4, 0.6),
	  	limits = c(0, 0.6)) +
   	xlim(1975, 2015) +
    theme_bw() +
		ylab("5-year rolling mean") +
  	theme(legend.position = "none") +
		theme(
			axis.text = element_text(size = 8, colour = "grey50"),
  	  axis.ticks = element_line(colour = "grey50"),
  	  axis.line = element_line(colour = "grey50"),
			axis.title = element_text(size = 10, color = "grey50"),
  	  panel.grid.major = element_blank(),
  	  panel.grid.minor = element_blank(),
  	  panel.border = element_rect(fill = NA, color = "grey50"),
			plot.margin = unit(c(0.05,-0.1, 0, 0), "in"))
	
	
	rolling_proj_mean_plot <-    
   	ggplot() +
		geom_line(data = rolling_stats_proj, 
							aes(year, means, color = sim_proj, group = sim_proj), alpha = 0.5) +
		facet_wrap(~ scen_f) +
		xlab("Year") +
		scale_color_manual(name = "sim_proj", values = colors) +
	  scale_y_continuous(
	  	breaks = c(0, 0.20, 0.4, 0.6),
	  	labels = c(0, 0.20, 0.4, 0.6),
	  	limits = c(0, 0.6)) +
   	xlim(2025, 2107) +
    theme_bw() +
  	theme(legend.position = "none") +
		theme(
			panel.spacing = unit(2, "pt"),
  		strip.background = element_blank(),
  		strip.text = element_blank(),
			axis.text.x = element_text(size = 8, colour = "grey50"),
  	  axis.ticks.x = element_line(colour = "grey50"),
  	  axis.line = element_line(colour = "grey50"),
			axis.title.x = element_text(size = 10, color = "grey50"),
  	  axis.title.y = element_blank(),
			axis.text.y = element_blank(),
  	  axis.ticks.y = element_blank(),
  	  panel.grid.major = element_blank(),
  	  panel.grid.minor = element_blank(),
  	  panel.border = element_rect(fill = NA, color = "grey50"),
			plot.margin = unit(c(0.05, 0, 0, -0.05), "in"))
	

	rolling_means <- rolling_hind_mean_plot + rolling_proj_mean_plot +
		plot_layout(widths = c(0.5, 1))
	
	rolling_means_form_5 <- 
			ggdraw(rolling_means) +
			draw_label("hindcast", x = 0.11, y = 0.93, color = "grey50", size = 10) +
			draw_label("projection: low emission (ssp126)", x = 0.503, y = 0.93, color = "grey50", size = 10) +
			draw_label("projection: high emission (ssp585)", x = 0.812, y = 0.93, color = "grey50", size = 10) +
			draw_label("cesm", x = 0.658, y = 0.5, color = "#6dc3a9", size = 10, alpha = 0.5) +
			draw_label("gfdl", x = 0.658, y = 0.63, color = "#4e8d9c", size = 10, alpha = 0.5) +
			draw_label("miroc", x = 0.658, y = 0.38, color = "#97c3e5", size = 10,  alpha = 0.5) +
			draw_label("cesm", x = 0.96, y = 0.28, color = "#ffabab", size = 10,  alpha = 0.5) +
			draw_label("gfdl", x = 0.96, y = 0.42, color = "#ff4040", size = 10,  alpha = 0.5) +
			draw_label("miroc", x = 0.96, y = 0.65, color = "#ffb733", size = 10,  alpha = 0.5) 
	
	
	ggsave("./output/plots/rolling_means_form_5.png",
		rolling_means,
		height = 3,
		width = 9)
