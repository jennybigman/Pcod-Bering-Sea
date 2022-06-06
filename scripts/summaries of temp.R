hind_temp_sum <- ROMS_hindcast_dat %>%
	group_by(month, year) %>%
	summarise(mean_temp = mean(temp))

temp_proj_sum <- ROMS_projected_dat %>%
	group_by(month, year, simulation, projection) %>%
	summarize(mean_temp = mean(bc_temp_sd))

temp_proj_sum_ssp126 <- temp_proj_sum %>%
	filter(projection == "ssp126")

temp_proj_sum_ssp585 <- temp_proj_sum %>%
	filter(projection == "ssp585")