# figure -- mean latitude time series

# calculating the mean latitude of spawning habitat suitability

	#### year ####
	
	#### hindcasts ####
	
	hind_mean_lat_yr <- function(x){
		
		new_dat <- ROMS_hindcast_dat %>%
			filter(., sp_hab_suit >= x)
		
		new_dat_sum <- new_dat %>%
			group_by(year) %>%
			summarise(hist_mean_lat = mean(latitude)) 

		new_dat_sum		
	}
	
	sp_hab_thresholds <- c(0.5, 0.9)
	
	hind_mean_lats_yr <- lapply(sp_hab_thresholds, hind_mean_lat_yr)
	
	hind_mean_lats_yr_0.5 <- hind_mean_lats_yr[[1]] %>%
		mutate(sp_hab_threshold = 0.5)

	hind_mean_lats_yr_0.9 <- hind_mean_lats_yr[[2]]	%>%
		mutate(sp_hab_threshold = 0.9)
	
	hind_mean_lat_yr <- bind_rows(hind_mean_lats_yr_0.5, hind_mean_lats_yr_0.9) 
	
	# mean latitude
	mean_lat_hind <- hind_mean_lat_yr %>%
		group_by(sp_hab_threshold) %>%
		summarize(mean_lat = mean(hist_mean_lat))
	
	#### projections ####
	
	# bias-corrected temp with variance ratio
		
	proj_mean_lat_yr <- function(x){
		
		new_dat <- ROMS_projected_dat %>%
			filter(., sp_hab_suit_var >= x)
		
		new_dat_sum <- new_dat %>%
			group_by(simulation, projection, year) %>%
			summarise(proj_mean_lat = mean(Lat)) 

		new_dat_sum		
	}
	
	sp_hab_thresholds <- c(0.5, 0.9)
	
	proj_mean_lat_yr <- lapply(sp_hab_thresholds, proj_mean_lat_yr)
	
	proj_mean_lats_yr_0.5 <- proj_mean_lat_yr[[1]] %>%
		mutate(sp_hab_threshold = 0.5)

	proj_mean_lats_yr_0.9 <- proj_mean_lat_yr[[2]]	%>%
		mutate(sp_hab_threshold = 0.9)
	
	proj_mean_lat_yr <- bind_rows(proj_mean_lats_yr_0.5, proj_mean_lats_yr_0.9) 
	

	years_proj <- c(2020:2099)
	proj_mean_lat_yr <- proj_mean_lat_yr %>%
		filter(year %in% years_proj)
	
	mean_lat_proj <- proj_mean_lat_yr %>%
		group_by(year, projection, simulation, sp_hab_threshold) %>%
		summarize(mean_lat = mean(proj_mean_lat)) %>%
		filter(year == 2099)

	#### rolling means ####
	
	# hind
	means_hind_core <- NA
  
  for(i in 6:51){
  	win <- (i - 5):(i + 5)
  	means_hind_core[i] <- mean(hind_mean_lats_yr_0.9$hist_mean_lat[win])
  }
  
	means_hind_pot <- NA
  
  for(i in 6:51){
  	win <- (i - 5):(i + 5)
  	means_hind_pot[i] <- mean(hind_mean_lats_yr_0.5$hist_mean_lat[win])
  }
	
	years_hind <- c(1970:2020) 
	
	core <- rep("core", 51)
	potential <- rep("potential", 51)
	
	means_core <- as.data.frame(cbind(means_hind_core, core, years_hind)) %>%
		rename(mean_lat = means_hind_core, 
					 sp_hab_threshold = core,
					 year = years_hind)
	
	means_pot <- as.data.frame(cbind(means_hind_pot, potential, years_hind)) %>%
		rename(mean_lat = means_hind_pot, 
					 sp_hab_threshold = potential,
					  year = years_hind)
	
	rolling_mean_hind <- rbind(means_core, means_pot)
	
	rolling_mean_hind$mean_lat <- as.numeric(rolling_mean_hind$mean_lat)
	rolling_mean_hind$year <- as.numeric(rolling_mean_hind$year)

	# proj
	proj_mlat_yr_sum <- proj_mean_lat_yr %>%
		group_by(year, projection, sp_hab_threshold) %>%
		summarise(mean_lat = mean(proj_mean_lat))
	
	proj_mlat_yr_sum_core <- proj_mlat_yr_sum %>%
		filter(sp_hab_threshold == 0.9)
	
	proj_mlat_yr_sum_pot <- proj_mlat_yr_sum %>%
		filter(sp_hab_threshold == 0.5)

	proj_mlat_yr_sum_core_low <- proj_mlat_yr_sum_core %>%
		filter(projection == "ssp126") 
	
	proj_mlat_yr_sum_pot_low <- proj_mlat_yr_sum_pot %>%
		filter(projection == "ssp126")

	means_proj_core_low <- NA
	means_proj_pot_low <- NA

  
  for(i in 6:80){
  	win <- (i - 5):(i + 5)
  	means_proj_core_low[i] <- mean(proj_mlat_yr_sum_core_low$mean_lat[win])
  }
	
	for(i in 6:80){
  	win <- (i - 5):(i + 5)
  	means_proj_pot_low[i] <- mean(proj_mlat_yr_sum_pot_low$mean_lat[win])
  }
	
	
	proj_mlat_yr_sum_core_high <- proj_mlat_yr_sum_core %>%
		filter(projection == "ssp585") 
	
	proj_mlat_yr_sum_pot_high <- proj_mlat_yr_sum_pot %>%
		filter(projection == "ssp585")

	means_proj_core_high <- NA
	means_proj_pot_high <- NA

  
  for(i in 6:80){
  	win <- (i - 5):(i + 5)
  	means_proj_core_high[i] <- mean(proj_mlat_yr_sum_core_high$mean_lat[win])
  }
	
	for(i in 6:80){
  	win <- (i - 5):(i + 5)
  	means_proj_pot_high[i] <- mean(proj_mlat_yr_sum_pot_high$mean_lat[win])
  }
	
	core <- rep("core", 80)
	potential <- rep("potential", 80)
	low <- rep("low emission\n(ssp126)", 80)
	high <- rep("high emission\n(ssp585)", 80)


	core_low <- as.data.frame(cbind(means_proj_core_low, core, low, years_proj)) %>% 
		rename(mean_lat = means_proj_core_low, 
					 sp_hab_threshold = core,
					 scen = low,
					 year = years_proj)
	
	core_high <- as.data.frame(cbind(means_proj_core_high, core, high, years_proj)) %>%
		rename(mean_lat = means_proj_core_high, 
					 sp_hab_threshold = core,
					 scen = high,
					 year = years_proj)

	pot_low <- as.data.frame(cbind(means_proj_pot_low, potential, low, years_proj)) %>% 
		rename(mean_lat = means_proj_pot_low, 
					 sp_hab_threshold = potential,
					 scen = low,
					 year = years_proj)
	
	pot_high <- as.data.frame(cbind(means_proj_pot_high, potential, high, years_proj)) %>%
		rename(mean_lat = means_proj_pot_high, 
					 sp_hab_threshold = potential,
					 scen = high,
					 year = years_proj)

	
	rolling_mean_proj <- rbind(core_low, core_high,
														 pot_low, pot_high)

	rolling_mean_proj$mean_lat <- as.numeric(rolling_mean_proj$mean_lat)
	rolling_mean_proj$year <- as.numeric(rolling_mean_proj$year)

	
	
	## facet by scenario ####
	
	# for plotting by scenario
		

	proj_mean_lat_yr$scen <- NA
		
	proj_mean_lat_yr$scen[proj_mean_lat_yr$projection == "ssp126"] <- "low emission\n(ssp126)"
	proj_mean_lat_yr$scen[proj_mean_lat_yr$projection == "ssp585"] <- "high emission\n(ssp585)"
	
	proj_mean_lat_yr <- tidyr::unite(proj_mean_lat_yr,"sim_proj",
															 simulation, projection, remove = F)

	colors <- c("#6dc3a9", "#ff7373", # cesm low, cesm high
						  "#4e8d9c", "#ff4040", # gfdl low, gfdl high
						  "#97c3e5", "#ffb733") # miroc low, miroc high

	sim_proj <- unique(proj_mean_lat_yr$sim_proj)
	
	names(colors) <- unique(proj_mean_lat_yr$sim_proj)
	
	# order factors for plotting
	
	hind_mean_lat_yr <- hind_mean_lat_yr %>%
		mutate(thresh = case_when(
			sp_hab_threshold == 0.9 ~ "core",
			sp_hab_threshold == 0.5 ~ "potential"))
		
	rolling_mean_hind <- rolling_mean_hind %>%
		mutate(thresh = sp_hab_threshold)
	
	rolling_mean_proj <- rolling_mean_proj %>%
		mutate(thresh = sp_hab_threshold)
	
	proj_mean_lat_yr$scen_f = factor(proj_mean_lat_yr$scen, 
															 levels=c('low emission\n(ssp126)', 'high emission\n(ssp585)'))
	rolling_mean_proj$scen_f = factor(rolling_mean_proj$scen, 
																		levels=c('low emission\n(ssp126)', 'high emission\n(ssp585)'))
	
	#### plot ####
	
	mean_latitude_plot <-    
   	ggplot() +
		geom_line(data = rolling_mean_hind, 
   						aes(x = year, y = mean_lat), 
   						color = "#e5e5e5") +
		geom_line(data = rolling_mean_proj, 
   						aes(x = year, y = mean_lat), 
   						color = "#e5e5e5") +
	 	geom_line(aes(year, hist_mean_lat),
            data = hind_mean_lat_yr %>% filter(sp_hab_threshold == 0.5), color = "black", alpha = 0.5) +
		geom_line(data = proj_mean_lat_yr, 
							aes(year, proj_mean_lat, color = sim_proj, group = sim_proj), alpha = 0.5) +
		facet_grid(thresh ~ scen_f) +
	  geom_line(aes(year, hist_mean_lat, colour = sp_hab_threshold),
            data = hind_mean_lat_yr %>% filter(sp_hab_threshold == 0.9), color = "black",  alpha = 0.5) +
		xlab("Year") +
		scale_color_manual(name = "sim_proj", values = colors) +
	  scale_y_continuous(
	  	name = "Mean latitude (˚N)",
	  	breaks = c(56, 57, 58, 59),
	  	labels = c(56, 57, 58, 59),
	  	limits = c(55.2, 59.7)
	  ) +
   	xlim(1970, 2110) +
		geom_vline(data = hind_mean_lat_yr, aes(xintercept = 2020, color = "#f2f2f2"), alpha = 0.5) +
    theme_bw() +
  	theme(legend.position = "none") +
  	theme(
			strip.background = element_blank(),
  		strip.text = element_text(size = 18, face = "bold"),
			axis.text = element_text(size = 16, colour = "grey50"),
  	  axis.ticks = element_line(colour = "grey50"),
  	  axis.line = element_line(colour = "grey50"),
  	  axis.title = element_text(size=18, color = "grey30"),
  	  panel.grid.major = element_blank(),
  	  panel.grid.minor = element_blank(),
  	  panel.border = element_rect(fill = NA, color = "grey50"))
	
		
	mean_latitude_plot_labs <- 
		ggdraw(mean_latitude_plot) +
		draw_label("cesm", x = 0.485, y = 0.74, color = "#6dc3a9", size = 12) +
		draw_label("gfdl", x = 0.485, y = 0.67, color = "#4e8d9c", size = 12) +
		draw_label("miroc", x = 0.485, y = 0.63, color = "#97c3e5", size = 12) +
		draw_label("cesm", x = 0.93, y = 0.82, color = "#ffabab", size = 12) +
		draw_label("gfdl", x = 0.93, y = 0.71, color = "#ff4040", size = 12) +
		draw_label("miroc", x = 0.93, y = 0.77, color = "#ffb733", size = 12) +
		draw_label("(a)", x = 0.08, y = 0.84, color = "black", size = 12) +
		draw_label("(b)", x = 0.53, y = 0.84, color = "black", size = 12) +
		draw_label("(c)", x = 0.08, y = 0.465, color = "black", size = 12) +
		draw_label("(d)", x = 0.53, y = 0.465, color = "black", size = 12) 

	ggsave("./output/plots/mean_latitude_plot.png",
			 mean_latitude_plot_labs,
			 width = 10, height = 5, units = "in")
	
	
	
	
	
	
	
