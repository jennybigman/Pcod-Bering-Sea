 # percentages for comparison in paper

  percent_change_func <- function(x, y){
  	((y - x)/x) * 100
  }
  
  #### temperature ####
  
  # beginning of time series
  temp_1970 <- ROMS_hindcast_dat %>%
		filter(year == 1970) %>%
  	group_by(month) %>%
		summarize(mean_temp = mean(temp))
	
	temp_1970_min <- min(temp_1970$mean_temp)
	
	# mid century
	
	# low
	temp_2050_low <- ROMS_projected_dat %>%
		filter(year == 2050 & projection == "SSP126") %>%
		group_by(month, simulation) %>%
		summarise(mean_temp = mean(bc_temp_sd))
	
	temp_2050_max_low <- max(temp_2050_low$mean_temp)
	
	temp_2050_max_low - temp_1970_min

	# high
	temp_2050_high <- ROMS_projected_dat %>%
		filter(year == 2050 & projection == "SSP585") %>%
		group_by(month, simulation) %>%
		summarise(mean_temp = mean(bc_temp_sd))
	
	temp_2050_max_high <- max(temp_2050_high$mean_temp)
	
	temp_2050_max_high - temp_1970_min
	
	# end of century
	temp_2099_low <- ROMS_projected_dat %>%
		filter(year == 2099 & projection == "SSP126") %>%
		group_by(month, simulation) %>%
		summarise(mean_temp = mean(bc_temp_sd))
	
	temp_2099_max_low <- max(temp_2099_low$mean_temp)
	
	temp_2099_max_low - temp_1970_min

	# high
	temp_2099_high <- ROMS_projected_dat %>%
		filter(year == 2099 & projection == "SSP585") %>%
		group_by(month, simulation) %>%
		summarise(mean_temp = mean(bc_temp_sd))
	
	temp_2099_max_high <- max(temp_2099_high$mean_temp)
	
	temp_2099_max_high - temp_1970_min


	#### spawning habitat suitabiltiy index ####
	
	# beginning of time series
	sphabsuit_1970 <- ROMS_hindcast_dat %>%
		filter(year == 1970) %>%
		group_by(month) %>%
		summarise(mean_sphabsuit = mean(sp_hab_suit)) 
	
	mean_sphabsuit_1970 <- min(sphabsuit_1970$mean_sphabsuit)
	
	#mid century 
	
	# low
	sphabsuit_2050_low <- ROMS_projected_dat %>%
		filter(year == 2050 & projection == "SSP126") %>%
		group_by(month, simulation) %>%
		summarise(mean_sphabsuit = mean(sp_hab_suit_var))
	
	sphabsuit_2050_low_max <- max(sphabsuit_2050_low$mean_sphabsuit)
	
	percent_change_func(x = mean_sphabsuit_1970, y = sphabsuit_2050_low_max)

	# high
	sphabsuit_2050_high <- ROMS_projected_dat %>%
		filter(year == 2050 & projection == "SSP585") %>%
		group_by(month, simulation) %>%
		summarise(mean_sphabsuit = mean(sp_hab_suit_var))
	
	sphabsuit_2050_high_max <- max(sphabsuit_2050_high$mean_sphabsuit)
	
	percent_change_func(x = mean_sphabsuit_1970, y = sphabsuit_2050_high_max)

	#### area ####
	
	# need to run 05-area script first
	
	## core ##
	core_1970_area <- as.numeric(c_area_hind_dat_sum_yr[1,2])
	
	core_2016_area <- c_area_hind_dat_sum_yr %>%
		filter(year == 2016)
	
	## test
	yrs <- 2017:2020
	dat_2017_2020 <- ROMS_hindcast_dat %>% filter(year %in% yrs)
	
	hist_incresae <- as.numeric((core_2016_area)[1,2]) - core_1970_area

	# mid century
	
	#  low
	core_area_2050_low <- c_area_proj_dat_sum_yr %>%
		filter(year == 2050 & projection == "SSP126") %>%
		group_by(simulation) %>%
		summarise(mean_core_area = mean(area))

	core_area_2050_low_max <- max(core_area_2050_low$mean_core_area)
	
	abs_change_core_area_low <- core_area_2050_low_max - core_1970_area
	
	percent_change_func(x = core_1970_area, y = core_area_2050_low_max)

	#  high
	core_area_2050_high <- c_area_proj_dat_sum_yr %>%
		filter(year == 2050 & projection == "SSP585") %>%
		group_by(simulation) %>%
		summarise(mean_core_area = mean(area))

	core_area_2050_high_max <- max(core_area_2050_high$mean_core_area)
	
	abs_change_core_area_high <- core_area_2050_high_max - core_1970_area
	
	percent_change_func(x = core_1970_area, y = core_area_2050_high_max)

	# end of century
	
	#  low
	core_area_2099_low <- c_area_proj_dat_sum_yr %>%
		filter(year == 2099 & projection == "SSP126") %>%
		group_by(simulation) %>%
		summarise(mean_core_area = mean(area))

	core_area_2099_low_max <- max(core_area_2099_low$mean_core_area)
	
	abs_change_core_area_low <- core_area_2099_low_max - core_area_2050_low_max
	
	percent_change_func(x = core_area_2050_low_max, y = core_area_2099_low_max)

	#  high
	core_area_2099_high <- c_area_proj_dat_sum_yr %>%
		filter(year == 2099 & projection == "SSP585") %>%
		group_by(simulation) %>%
		summarise(mean_core_area = mean(area))

	core_area_2099_high_max <- max(core_area_2099_high$mean_core_area)
	
	abs_change_core_area_high <- core_area_2099_high_max - core_area_2050_high_max
	
	percent_change_func(x = core_area_2050_high_max, y = core_area_2099_high_max)
	
	## potential
	pot_1970_area <- as.numeric(p_area_hind_dat_sum_yr[1,2])
	
	pot_2016_area <- p_area_hind_dat_sum_yr %>%
		filter(year == 2016)
	
	hist_incresae <- as.numeric((pot_2016_area)[1,2]) - pot_1970_area

	# mid century
	
	# low
	pot_area_2050_low <- p_area_proj_dat_sum_yr %>%
		filter(year == 2050 & projection == "SSP126") %>%
		group_by(simulation) %>%
		summarise(mean_pot_area = mean(area))

	pot_area_2050_low_max <- max(pot_area_2050_low$mean_pot_area)
	
	abs_change_pot_area_low <- pot_area_2050_low_max - pot_1970_area
	
	percent_change_func(x = pot_1970_area, y = pot_area_2050_low_max)

	#  high
	pot_area_2050_high <- p_area_proj_dat_sum_yr %>%
		filter(year == 2050 & projection == "SSP585") %>%
		group_by(simulation) %>%
		summarise(mean_pot_area = mean(area))

	pot_area_2050_high_max <- max(pot_area_2050_high$mean_pot_area)
	
	abs_change_pot_area_high <- pot_area_2050_high_max - pot_1970_area
	
	percent_change_func(x = pot_1970_area, y = pot_area_2050_high_max)

	# end of century
	
	#  low
	pot_area_2099_low <- p_area_proj_dat_sum_yr %>%
		filter(year == 2099 & projection == "SSP126") %>%
		group_by(simulation) %>%
		summarise(mean_pot_area = mean(area))

	pot_area_2099_low_max <- max(pot_area_2099_low$mean_pot_area)
	
	abs_change_pot_area_low <- pot_area_2099_low_max - pot_area_2050_low_max
	
	percent_change_func(x = pot_area_2050_low_max, y = pot_area_2099_low_max)

	#  high
	pot_area_2099_high <- p_area_proj_dat_sum_yr %>%
		filter(year == 2099 & projection == "SSP585") %>%
		group_by(simulation) %>%
		summarise(mean_pot_area = mean(area))

	pot_area_2099_high_max <- max(pot_area_2099_high$mean_pot_area)
	
	abs_change_pot_area_high <- pot_area_2099_high_max - pot_area_2050_high_max
	
	percent_change_func(x = pot_area_2050_high_max, y = pot_area_2099_high_max)