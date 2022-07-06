# temporal shifts ala Cote et al 2021


	#### month with highest spawning habitat suitability over time ####
	
	## for each grid cell, find the month with the highest spawning habitat suitability index
	## and take the mean and some measure of variance
	
	## hindcast ##

	ROMS_hindcast_dat_gc <- ROMS_hindcast_dat %>%
	 mutate(grid_cell_id = paste0(latitude, longitude))
	
	grid_cells <- ROMS_hindcast_dat_gc %>%
	 distinct(grid_cell_id) %>%
	 mutate(ID = 1:5024)

	ROMS_hindcast_dat_gc <- merge(ROMS_hindcast_dat_gc, grid_cells, by = "grid_cell_id")
	
	max_mo_func <- function(x, y){
		
		new_dat <- ROMS_hindcast_dat_gc %>%
			filter(year == x, ID == y)
		
		max <- new_dat[which.max(new_dat$sp_hab_suit), ]
		
	}
	
	years_hind <- unique(ROMS_hindcast_dat_gc$year)
	grid_cells_hind <- unique(ROMS_hindcast_dat_gc$ID)
	year_cells <- expand_grid(years_hind, grid_cells_hind)
	
	phen_dat_hist <- mapply(max_mo_func,
										 x = year_cells$years_hind, 
										 y = year_cells$grid_cells_hind,
										 SIMPLIFY = FALSE) 
	
	phen_dat_hist <- phen_dat %>% bind_rows()
	
	#	write.csv(phen_dat_hist, file = "./data/phen_dat_hist.csv")
	
	# phen_dat_hist <- fread(file = "./data/phen_dat.csv")	

	phen_dat_hist <- phen_dat_hist %>%
		mutate(WOY = lubridate::week(DateTime))
		
	phen_dat_hist_sum <- phen_dat_hist %>%
		group_by(year) %>%
		summarize(mean_WOY = mean(WOY),
							med_WOY = median(WOY),
							Q1_WOY = quantile(WOY, prob = 0.25),
							Q3_WOY = quantile(WOY, prob = 0.75),
							sd_WOY = sd(WOY))


	
	## plot
	
	time_habsuit_hind_plot <-
		ggplot(data = phen_dat_hist_sum) +
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

	## projections ##

	years_proj <- 2020:2099
	
	ROMS_projected_dat_trim <- ROMS_projected_dat %>%
		filter(year %in% years_proj)
	
	ROMS_projected_dat_trim_gc <- ROMS_projected_dat_trim %>%
	 mutate(grid_cell_id = paste0(latitude, longitude))
	
	grid_cells <- ROMS_projected_dat_trim_gc %>%
	 distinct(grid_cell_id) %>%
	 mutate(ID = 1:5024)

	ROMS_projected_dat_trim_gc <- merge(ROMS_projected_dat_trim_gc, grid_cells, by = "grid_cell_id")
	
	proj_sums <- ROMS_projected_dat_trim %>%
		group_by(year, WOY, grid_cell_id, simulation, projection) %>%
		slice_max(sp_hab_suit_var) 
	
	fwrite(proj_sums, "./data/phen_dat_proj.csv")

	phen_dat_proj_sum2 <- proj_sums2 %>%
		summarize(mean_WOY = mean(WOY),
							med_WOY = median(WOY),
							Q1_WOY = quantile(WOY, prob = 0.25),
							Q3_WOY = quantile(WOY, prob = 0.75),
							sd_WOY = sd(WOY))

	fwrite(phen_dat_proj_sum, "./data/phen_dat_proj_sum.csv")
	
	## plot 

	time_habsuit_proj_plot <-
		ggplot(data = phen_dat_proj_sum2) +
		geom_line(aes(x = year, y = mean_WOY)) +
		facet_grid(simulation ~ projection) +
		scale_y_reverse(
			breaks = c(13, 9, 5, 1),
			labels = c("April", "March", "February", "January"),
			limits = c(14, 1)
		) +
		theme_bw()
		
		ggsave(path = "./output/plots/", filename = "time_habsuit_proj_plot.png",
				 plot = time_habsuit_proj_plot,
	       height = 5, width = 10, units = "in")

	### try another way
		
	test_dat <- proj_sums %>%
		filter(simulation == "cesm", projection == "ssp126")
	
	test_dat_sum <- test_dat %>%
			summarize(mean_WOY = mean(WOY),
							med_WOY = median(WOY),
							Q1_WOY = quantile(WOY, prob = 0.25),
							Q3_WOY = quantile(WOY, prob = 0.75),
							sd_WOY = sd(WOY))
