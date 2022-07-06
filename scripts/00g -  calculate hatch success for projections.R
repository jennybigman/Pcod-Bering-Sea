# calculating hatch success	

	library(here)
	library(data.table)

	# projected data
	proj_temp_dat <- fread(file = here("./data/proj_temp_dat.csv"))

	# calculate hatch success
	hatch_success_cauchy_func <- function(x, k = 0.453, mu = 4.192, sigma = 2.125 ){
  			 		(k / (1 + (((x - mu)/sigma))^2)) 
	}

	hatch_success_gaus_func <- function(x, k = 0.395, mu = 4.50, sigma = 2.58){
        	k * exp(-1/2 * (x - mu)^2/sigma^2)
	} 
    
	ROMS_proj_temp_dat <- proj_temp_dat %>%
		mutate(hatch_success_cauchy_var = sapply(bc_temp_sd, hatch_success_cauchy_func),
					 hatch_success_gaus_var = sapply(bc_temp_sd, hatch_success_gaus_func))
	
	
	# standardize hatch success (calculating spawning habitat suitability)
 
	ROMS_projected_dat <- ROMS_proj_temp_dat %>%
  	mutate(sp_hab_suit_var = hatch_success_cauchy_var/max(hatch_success_cauchy_var))
	 

	fwrite(ROMS_projected_dat, file = "./data/ROMS_projected_dat.csv")
	