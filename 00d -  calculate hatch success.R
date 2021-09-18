# calculating hatch success	

	library(here)

	# read in data
	ROMS_dat_hind_trim <- fread("./data/ROMS_dat_hind_trim.csv")


	# calculate hatch success
	hatch_success_cauchy_func <- function(x, k = 0.453, mu = 4.192, sigma = 2.125 ){
  			 		(k / (1 + (((x - mu)/sigma))^2)) 
	}

	hatch_success_gaus_func <- function(x, k = 0.395, mu = 4.50, sigma = 2.58){
        	k * exp(-1/2 * (x - mu)^2/sigma^2)
	} 
    
	ROMS_dat_hind_trim <- ROMS_dat_hind_trim %>%
		mutate(hatch_success_cauchy = sapply(temp, hatch_success_cauchy_func),
					 hatch_success_gaus = sapply(temp, hatch_success_gaus_func))
	
	# standardize hatch success (calculating spawning habitat suitability)
  ROMS_dat_hind_trim <- ROMS_dat_hind_trim %>%
  	mutate(sp_hab_suit = hatch_success_cauchy/max(hatch_success_cauchy))
  
  
  
	ROMS_dat_hind_trim <- ROMS_dat_hind_trim %>%
		rename(Xi = Xi.x,
					 Eta = Eta.x) %>%
		dplyr::select(-Xi.y, -Eta.y)
	
	fwrite(ROMS_dat_hind_trim, file = "./data/SpawnMonths_ROMS_dat_hind_trim.csv")
	