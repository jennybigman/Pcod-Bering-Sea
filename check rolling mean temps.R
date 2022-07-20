# rolling means of temp
	
	# summarize projected temp by projection, not sim
	yearly_temp_proj_rm <- ROMS_projected_dat %>% 
		filter(year %in% years_proj) %>%
		group_by(projection, year) %>%
   	summarise(avg_temp = mean(bc_temp_sd)) 
	
	yearly_temp_proj_rm_low <- bind_rows(yearly_temp_hind, yearly_temp_proj_rm) %>%
		filter(projection %in% c("ssp126", "historical"))
	
	yearly_temp_proj_rm_high <- bind_rows(yearly_temp_hind, yearly_temp_proj_rm) %>%
		filter(projection %in% c("ssp585", "historical"))
	
	
	rolling_meantemp_func <- function(x){
 	
  	means_proj <- NA
  	
  	for(i in 5:length(x$year)){
  		win <- (i - 4):(i + 4)
  		means_proj[i] <- mean(x$avg_temp[win]) }
  		
  	years_dat <- 1970:2099
  	
  	data.frame(means_proj, years_dat)
  	
  	}
	
	
	rolling_means_temp <- lapply(list(yearly_temp_proj_rm_low, yearly_temp_proj_rm_high),
															 rolling_meantemp_func)
	
	
	rolling_means_temp_low <- rolling_means_temp[[1]] %>%
		mutate(projection = "ssp126")

	rolling_means_temp_high <- rolling_means_temp[[2]] %>%
		mutate(projection = "ssp585")
	
	rolling_means_temp <- bind_rows(rolling_means_temp_low, rolling_means_temp_high)

	# order facets
	yearly_temp_proj$scen_f = factor(yearly_temp_proj$scen, 
																			levels=c('low emission (ssp126)',  
																							 'high emission (ssp585)'))
	
	rolling_means_temp$scen <- NA
		
	rolling_means_temp$scen[rolling_means_temp$projection == "ssp126"] <- "low emission (ssp126)"
	rolling_means_temp$scen[rolling_means_temp$projection == "ssp585"] <- "high emission (ssp585)"
	
	rolling_means_temp$scen_f = factor(rolling_means_temp$scen, 
																		levels=c('low emission (ssp126)',  
																						 'high emission (ssp585)'))
