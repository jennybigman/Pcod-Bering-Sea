# calculating hatch success	

	library(here)

	# read in data
	ROMS_hindcast_temp_dat <- fread("./data/ROMS_hindcast_temp_dat.csv") %>% filter(., year != 2021)

	# projected data
	ROMS_proj_temp_dat_hm <- fread(file = here("./data/proj_temp_dat_Holsman.csv"))

	# calculate hatch success
	hatch_success_cauchy_func <- function(x, k = 0.453, mu = 4.192, sigma = 2.125 ){
  			 		(k / (1 + (((x - mu)/sigma))^2)) 
	}

	hatch_success_gaus_func <- function(x, k = 0.395, mu = 4.50, sigma = 2.58){
        	k * exp(-1/2 * (x - mu)^2/sigma^2)
	} 
    
	ROMS_hindcast_temp_dat <- ROMS_hindcast_temp_dat %>%
		mutate(hatch_success = sapply(temp, hatch_success_cauchy_func))
	
	ROMS_proj_temp_dat_hm <- ROMS_proj_temp_dat_hm %>%
		mutate(hatch_success = sapply(bc_temp_sd, hatch_success_cauchy_func))
	
	
	# standardize hatch success (calculating spawning habitat suitability)
  ROMS_hindcast_temp_dat <- ROMS_hindcast_temp_dat %>%
  	mutate(sp_hab_suit = hatch_success/max(hatch_success))
  
	ROMS_hindcast_dat <- ROMS_hindcast_temp_dat %>%
		rename(Xi = Xi.x,
					 Eta = Eta.x) %>%
		dplyr::select(-Xi.y, -Eta.y)
	
	fwrite(ROMS_hindcast_dat, file = "./data/ROMS_hindcast_dat.csv")
	
	
	ROMS_projected_dat_hm <- ROMS_proj_temp_dat_hm %>%
  	mutate(sp_hab_suit = hatch_success/max(hatch_success))
	 
	 ROMS_projected_dat_hm_sum <- ROMS_projected_dat_hm %>%
	 	group_by(simulation, projection, year) %>%
	 	summarise(n())
  
	fwrite(ROMS_projected_dat_hm, file = "./data/ROMS_projected_dat_hm.csv")
	