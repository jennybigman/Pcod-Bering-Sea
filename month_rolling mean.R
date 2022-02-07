# projections

	mo_stats_proj <- ROMS_projected_dat %>%
		group_by(simulation, projection,year, month, month_name) %>%
		summarise(mean_sp_hab_suit = mean(sp_hab_suit_var),
						  sd_sp_hab_suit = sd(sp_hab_suit_var)) 

	roll_mean_func <- function(x){
 	
		new_dat <- mo_stats_proj %>% filter(month_name == x)
  	roll_mean_mo <- rollmean(new_dat$mean_sp_hab_suit, 11, fill = NA) 
  	roll_mean_mo
  
	}
 
	month_names <- as.character(unique(mo_stats$month_name))
	month <- 1:4
 
	mo_mean_proj_dfs <- lapply(month_names, roll_mean_func) %>% set_names(month_names)

	years_proj_hist <- (c(1980:2014))
	years_proj_scenario <- c(rep(c(2015:2099), 2))
	years_proj <- c(years_proj_hist, years_proj_scenario)
	years_proj_all <- rep(years_proj, 3)
	
	sims <- c("cesm", "gfdl", "miroc")
	simulations <- list()
	
	for(i in sims){
		simulations[[i]] <- rep(i, times = 205)
		simulations
	}
	
	simulation <- unlist(simulations)
	
 
	scens <- list("ssp126", "ssp585")
	scenarios <- list()
	
	for(i in scens){
		scenarios[[i]] <- rep(i, times = (85))
		scenarios
	}
	
	scenarios <- as.vector(c(scenarios[[1]], scenarios[[2]]))
	scenario <- c(rep("historical", 35), scenarios)
	scenario_all <- rep(scenario, 3)
	
	
	mo_mean_proj <- bind_rows(mo_mean_proj_dfs) %>%
		mutate(year = years_proj_all,
					 simulation = simulation,
					 scenario = scenario_all) %>%
		na.omit()
	
	year <- mo_mean_proj$year
	
	mo_mean_proj <- mo_mean_proj %>%
				gather(month_name, roll_mean, January:April)
	
	# add month names
  mo_mean_proj$month <- NA
  mo_mean_proj$month[mo_mean_proj$month_name == "January"] <- 1
  mo_mean_proj$month[mo_mean_proj$month_name == "February"] <- 2
	mo_mean_proj$month[mo_mean_proj$month_name == "March"] <- 3
	mo_mean_proj$month[mo_mean_proj$month_name == "April"] <- 4

	# alt way
	
	mo_stats_proj <- tidyr::unite(mo_stats_proj,"sim_proj",
																simulation, projection, remove = F)

	sim_proj <- unique(mo_stats_proj$sim_proj)
	
	sim_projs <- sim_proj[!grepl("_historical", sim_proj)]
		
	sim_hists <- sim_proj[grepl("_historical", sim_proj)]
	
	## for historical part of proj
	
	sds_func_sim_hists <- function(x){
		
	new_dat <- mo_stats_proj %>% filter(., sim_proj == x)
	
	sds_proj <- NA
  
  for(i in 6:105){
  	win <- (i - 5):(i + 5)
  	sds_proj[i] <- sd(new_dat$mean_sp_hab_suit[win])
  }
  
  means_proj <- NA
  
  for(i in 6:105){
  	win <- (i - 5):(i + 5)
  	means_proj[i] <- mean(new_dat$mean_sp_hab_suit[win])
  }
  
  stats <- data.frame(sds_proj, means_proj) %>% na.omit()
  
  stats
  
	}
	
	stats_proj_hist_list <- lapply(sim_hists, sds_func_sim_hists)
	
	df1<-stats_proj_hist_list[[1]]
	
	df1$year <- rep(1985:2009, 3)
	
	## for projections 2015 onwards

	sds_func_sim_prjs <- function(x){
		
	new_dat <- mo_stats_proj %>% filter(., sim_proj == x)
	
	sds_proj <- NA
  
  for(i in 6:240){
  	win <- (i - 5):(i + 5)
  	sds_proj[i] <- sd(new_dat$mean_sp_hab_suit[win])
  }
  
  means_proj <- NA
  
  for(i in 6:240){
  	win <- (i - 5):(i + 5)
  	means_proj[i] <- mean(new_dat$mean_sp_hab_suit[win])
  }
  
  stats <- data.frame(sds_proj, means_proj) %>% na.omit()
  
  stats
  
	}
	
	sims <- unique(mo_stats_proj$simulation)
	
	
	stats_proj_list <- lapply(sim_projs, sds_func_proj)
	
	df1<-stats_proj_list[[1]]
	
	
