	# mean latitude

	#### mean latitude of core habitat (>90%) ####

  sp_hab_90 <- sm_temp_hind_df %>% filter(., sp_hab_suit >= 0.9)	
  
  mean_lat_90 <- mean(sp_hab_90$latitude)
  