	ROMS_hindcast_dat_gc <- ROMS_hindcast_dat %>%
	 mutate(grid_cell_id = paste0(latitude, longitude))
	
	grid_cells <- ROMS_hindcast_dat_gc %>%
	 distinct(grid_cell_id) %>%
	 mutate(ID = 1:5024)

	ROMS_hindcast_dat_gc <- merge(ROMS_hindcast_dat_gc, grid_cells, by = "grid_cell_id")
	
	ROMS_hindcast_dat_gc <- ROMS_hindcast_dat_gc %>%
		mutate(WOY = lubridate::week(DateTime))

	hist_max <- ROMS_hindcast_dat_gc %>%
		group_by(year, WOY, grid_cell_id) %>%
		slice_max(sp_hab_suit) 
	
	hist_sum <- hist_max %>%
		group_by(year) %>%
		summarize(mean_WOY = mean(WOY),
							med_WOY = median(WOY),
							Q1_WOY = quantile(WOY, prob = 0.25),
							Q3_WOY = quantile(WOY, prob = 0.75),
							sd_WOY = sd(WOY))

	time_habsuit_hind_plot <-
		ggplot(data = hist_sum) +
		geom_point(aes(x = year, y = mean_WOY)) +
		geom_line(aes(x = year, y = mean_WOY)) +
		scale_y_reverse(
			breaks = c(13, 9, 5, 1),
			labels = c("April", "March", "February", "January"),
			limits = c(14, 1)
		) +
		theme_bw()
		
	ggsave(path = "./output/plots/", filename = "time_habsuit_hind_plot.png",
				 plot = time_habsuit_hind_plot,
	       height = 5, width = 10, units = "in")
	
	## with month
	
	hist_max_mo <- ROMS_hindcast_dat_gc %>%
		group_by(year, month, grid_cell_id) %>%
		slice_max(sp_hab_suit) 
	
	hist_sum_mo <- hist_max_mo %>%
		group_by(year) %>%
		summarize(mean_mo = mean(month),
							med_mo = median(month),
							Q1_mo = quantile(month, prob = 0.25),
							Q3_mo = quantile(month, prob = 0.75),
							sd_mo = sd(month))

	time_habsuit_hind_plot <-
		ggplot(data = hist_sum_mo) +
		geom_point(aes(x = year, y = mean_mo)) +
		geom_line(aes(x = year, y = mean_mo)) +
		scale_y_reverse(
			breaks = c(13, 9, 5, 1),
			labels = c("April", "March", "February", "January"),
			limits = c(14, 1)
		) +
		theme_bw()
		
	ggsave(path = "./output/plots/", filename = "time_habsuit_hind_plot.png",
				 plot = time_habsuit_hind_plot,
	       height = 5, width = 10, units = "in")
	

	## projections
	
	years_proj <- 2020:2099
	
	ROMS_projected_dat_trim <- ROMS_projected_dat %>%
		filter(year %in% years_proj)
	
	ROMS_projected_dat_trim_gc <- ROMS_projected_dat_trim %>%
	 mutate(grid_cell_id = paste0(latitude, longitude))
	
	grid_cells <- ROMS_projected_dat_trim_gc %>%
	 distinct(grid_cell_id) %>%
	 mutate(ID = 1:5024)

	ROMS_projected_dat_trim_gc <- merge(ROMS_projected_dat_trim_gc, grid_cells, by = "grid_cell_id")
	
	proj_max <- ROMS_projected_dat_trim_gc %>%
		group_by(year, month, grid_cell_id, simulation, projection) %>%
		slice_max(sp_hab_suit_var) 

	proj_sum <- proj_max %>%
		group_by(year, simulation, projection) %>%
		summarize(mean_WOY = mean(WOY),
							med_WOY = median(WOY),
							Q1_WOY = quantile(WOY, prob = 0.25),
							Q3_WOY = quantile(WOY, prob = 0.75),
							sd_WOY = sd(WOY))

	time_habsuit_proj_plot <-
		ggplot(data = proj_sum) +
		geom_point(aes(x = year, y = mean_WOY)) +
		geom_line(aes(x = year, y = mean_WOY)) +
		facet_grid(simulation ~ projection) +
		scale_y_reverse(
			breaks = c(13, 9, 5, 1),
			labels = c("April", "March", "February", "January"),
			limits = c(14, 1)
		) +
		theme_bw()
		
	ggsave(path = "./output/plots/", filename = "time_habsuit_hind_plot.png",
				 plot = time_habsuit_hind_plot,
	       height = 5, width = 10, units = "in")

	# plot hindcast and projections together
	
	proj_sum$scen <- NA
		
	proj_sum$scen[proj_sum$projection == "ssp126"] <- "low emission\n(ssp126)"
	proj_sum$scen[proj_sum$projection == "ssp585"] <- "high emission\n(ssp585)"
	
	proj_sum <- tidyr::unite(proj_sum,"sim_proj",
		simulation, projection, remove = F)

	colors <- c("#6dc3a9", "#ff7373", # cesm low, cesm high
						  "#4e8d9c", "#ff4040", # gfdl low, gfdl high
						  "#97c3e5", "#ffb733") # miroc low, miroc high

	sim_proj <- unique(proj_sum$sim_proj)
	
	names(colors) <- unique(proj_sum$sim_proj)
	
	# order factors for plotting

	proj_sum$scen_f = factor(proj_sum$scen, 
		levels=c('low emission\n(ssp126)', 'high emission\n(ssp585)'))

	time_habsuit_proj_plot <-
		ggplot() +
		geom_line(data = hist_sum, aes(x = year, y = mean_WOY)) +
		geom_line(data = proj_sum, aes(x = year, y = mean_WOY, color = sim_proj)) +
		facet_wrap(~ projection) +
		scale_color_manual(name = "sim_proj", values = colors) +
		scale_y_reverse(
			breaks = c(13, 9, 5, 1),
			labels = c("April", "March", "February", "January"),
			limits = c(14, 1)
		) +
		theme_bw()
		
	ggsave(path = "./output/plots/", filename = "time_habsuit_hind_plot.png",
				 plot = time_habsuit_hind_plot,
	       height = 5, width = 10, units = "in")
