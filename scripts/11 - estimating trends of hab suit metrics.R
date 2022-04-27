# test for 0 slope

	#### temperature ####
	
	# hindcast
	hind_temp_lm <- lm(avg_temp ~ year, data = yearly_temp_hind)
	confint(hind_temp_lm)
	summary(hind_temp_lm)
	
	# projection
	proj_temp_lm <- lme4::lmer(avg_temp ~ year * projection + (1 | simulation), data = yearly_temp_proj)
	confint(proj_temp_lm)
	summary(proj_temp_lm)

	#### spawning habitat suitability ####
	
	# hindcast
	hind_habsuit_lm <- lm(mean_hab_suit ~ year, data = yearly_hab_dat_hind)
	confint(hind_habsuit_lm)
	summary(hind_habsuit_lm)

	# projection
	proj_habsuit_lm <- lme4::lmer(mean_hab_suit ~ year * projection + (1 | simulation), 
																data = yearly_hab_dat_proj)
	confint(proj_habsuit_lm)
	summary(proj_habsuit_lm)

	#### area ####
	
	# hindcast core
	core_area_hind_lm <- lm(area ~ year, data = c_area_hind_dat_sum_yr)
	confint(core_area_hind_lm)
	summary(core_area_hind_lm)

	# hindcast potential
	pot_area_hind_lm <- lm(area ~ year, data = p_area_hind_dat_sum_yr)
	confint(pot_area_hind_lm)
	summary(pot_area_hind_lm)

	# projection core
	core_area_proj_lm <- lme4::lmer(area ~ year * projection + (1 | simulation), 
																 data = c_area_proj_dat_sum_yr)
	confint(core_area_proj_lm)
	summary(core_area_proj_lm)

	# projection potential
	pot_area_proj_lm <- lme4::lmer(area ~ year * projection + (1 | simulation), 
																 data = p_area_proj_dat_sum_yr)
	confint(pot_area_proj_lm)
	summary(pot_area_proj_lm)

		#### mean latitude ####
	
	# hindcast core
	hind_meanlat_core_lm <- lm(hist_mean_lat ~ year, data = hind_mean_lats_yr_0.9)
	confint(hind_meanlat_core_lm)
	summary(hind_meanlat_core_lm)
	
	# hindcast potential
	hind_meanlat_pot_lm <- lm(hist_mean_lat ~ year, data = hind_mean_lats_yr_0.5)
	confint(hind_meanlat_pot_lm)
	summary(hind_meanlat_pot_lm)

	# projection core
	mean_lat_proj_core <- mean_lat_proj %>%
		filter(sp_hab_threshold == 0.9)
	
	proj_meanlat_core_lm <- lme4::lmer(mean_lat ~ year * projection + (1 | simulation), 
																data = mean_lat_proj_core)
	confint(proj_meanlat_core_lm)
	summary(proj_meanlat_core_lm)
	
	# projection potential
	mean_lat_proj_pot <- mean_lat_proj %>%
		filter(sp_hab_threshold == 0.5)
	
	proj_meanlat_pot_lm <- lme4::lmer(mean_lat ~ year * projection + (1 | simulation), 
																data = mean_lat_proj_pot)
	confint(proj_meanlat_pot_lm)
	summary(proj_meanlat_pot_lm)
