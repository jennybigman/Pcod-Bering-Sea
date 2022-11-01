 # monthly max hab suit

	# add a column for grid cell
	ROMS_hindcast_dat_gc <- ROMS_hindcast_dat %>%
	 mutate(grid_cell_id = paste0(latitude, longitude))
	
	grid_cells <- ROMS_hindcast_dat_gc %>%
	 distinct(grid_cell_id) 
	
	grid_cells <- grid_cells %>%
	 mutate(ID = 1:nrow(grid_cells))
	
	ROMS_hindcast_dat_gc <- merge(ROMS_hindcast_dat_gc, grid_cells, by = "grid_cell_id")

	# find the max sp hab suit value for each year for grid cell
	max_habsuit_yr <- ROMS_hindcast_dat_gc %>%
		dplyr::group_by(grid_cell_id, ID, year) %>%
		dplyr::summarise(max_sphabsuit = max(sp_hab_suit))
	
	# try with mutate and distinct
	max_habsuit_yr <- ROMS_hindcast_dat_gc %>%
		dplyr::group_by(grid_cell_id, ID, year) %>%
		dplyr::summarise(max_sphabsuit = max(sp_hab_suit))
	
	# join this back with the date
	date_dat <- ROMS_hindcast_dat_gc %>%
		dplyr::select(sp_hab_suit, DateTime, month, week, year)
	
	max_hab_vals <- max_habsuit_yr$max_sphabsuit
	
	date_dat <- date_dat %>%
		filter(sp_hab_suit %in% max_hab_vals)
	
	# merge
	max_habsuit_month <- merge(max_habsuit_yr, date_dat, by.x = "max_sphabsuit", by.y = "sp_hab_suit")
	
	# calculate mean, median, and quartiles of the month with the highest spawning habitat suitability
	phen_dat_hist_sum <- phen_dat_hist %>%
		group_by(year) %>%
		summarize(mean_WOY = mean(WOY),
							med_WOY = median(WOY),
							Q1_WOY = quantile(WOY, prob = 0.25),
							Q3_WOY = quantile(WOY, prob = 0.75),
							sd_WOY = sd(WOY))
