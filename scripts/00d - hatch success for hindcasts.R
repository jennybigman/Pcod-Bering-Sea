# hatch success for hindcast


	library(here)
	library(data.table)

	# read in data
	ROMS_hindcast_temp_dat <- fread("./data/ROMS_hindcast_temp_dat.csv") %>% filter(., year != 2021)

	# calculate hatch success
	hatch_success_cauchy_func <- function(x, k = 0.453, mu = 4.192, sigma = 2.125 ){
  			 		(k / (1 + (((x - mu)/sigma))^2)) 
	}

	hatch_success_gaus_func <- function(x, k = 0.395, mu = 4.50, sigma = 2.58){
        	k * exp(-1/2 * (x - mu)^2/sigma^2)
	} 
    
	ROMS_hindcast_temp_dat <- ROMS_hindcast_temp_dat %>%
		mutate(hatch_success_cauchy = sapply(temp, hatch_success_cauchy_func),
					 hatch_success_gaus = sapply(temp, hatch_success_gaus_func))

	# standardize hatch success (calculating spawning habitat suitability)
	ROMS_hindcast_temp_dat <- ROMS_hindcast_temp_dat %>%
  	mutate(sp_hab_suit = hatch_success_cauchy/max(hatch_success_cauchy))
  
  # remove cols hat aren't needed
	ROMS_hindcast_dat <- ROMS_hindcast_temp_dat %>%
		rename(Xi = Xi.x,
					 Eta = Eta.x) %>%
		dplyr::select(-Xi.y, -Eta.y)
	
	# save file
	fwrite(ROMS_hindcast_dat, file = "./data/ROMS_hindcast_dat.csv")
	