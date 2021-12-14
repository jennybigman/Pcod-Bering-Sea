	library(here)

	# read in data
	ROMS_hindcast_temp_dat <- fread("./data/ROMS_hindcast_temp_dat.csv")

	# projected data
	cesm_bc_temps <- fread(file = here("./data/cesm_bc_temps.csv"))
	gfdl_bc_temps <- fread(file = here("./data/gfdl_bc_temps.csv"))
	miroc_bc_temps <-fread(file = here("./data/miroc_bc_temps.csv"))

	# calculate hatch success
	hatch_success_cauchy_func <- function(x, k = 0.453, mu = 4.192, sigma = 2.125 ){
  			 		(k / (1 + (((x - mu)/sigma))^2)) 
	}

	hatch_success_gaus_func <- function(x, k = 0.395, mu = 4.50, sigma = 2.58){
        	k * exp(-1/2 * (x - mu)^2/sigma^2)
	} 
   
	# calculate hatch success for hindcast data 
	ROMS_hindcast_temp_dat <- ROMS_hindcast_temp_dat %>%
		mutate(hatch_success_cauchy = sapply(temp, hatch_success_cauchy_func),
					 hatch_success_gaus = sapply(temp, hatch_success_gaus_func))
	
	# calculate hatch success for projection data 
	hs_func <- function(df){
		df <- df %>%
				mutate(hatch_success_cauchy = sapply(bc_temp, hatch_success_cauchy_func),
							 hatch_success_gaus = sapply(bc_temp, hatch_success_gaus_func),
							 sp_hab_suit = hatch_success_cauchy/max(hatch_success_cauchy))

		df
		
	}
	
	df_list <- list(cesm_bc_temps, gfdl_bc_temps, miroc_bc_temps)
	
	proj_hs_list <- lapply(df_list, hs_func)
	
	cesm_proj_dat <- proj_hs_list[[1]]
	gfdl_proj_dat <- proj_hs_list[[2]]
	miroc_proj_dat <- proj_hs_list[[3]]
	
	df_list2 <- list("cesm_proj_dat", "gfdl_proj_dat", "miroc_proj_dat")

	# standardize hatch success (calculating spawning habitat suitability)
  ROMS_hindcast_temp_dat <- ROMS_hindcast_temp_dat %>%
  	mutate(sp_hab_suit = hatch_success_cauchy/max(hatch_success_cauchy))
  
  
	ROMS_hindcast_dat <- ROMS_hindcast_temp_dat %>%
		rename(Xi = Xi.x,
					 Eta = Eta.x) %>%
		dplyr::select(-Xi.y, -Eta.y)
	
	fwrite(ROMS_hindcast_dat, file = "./data/ROMS_hindcast_dat.csv")

	fwrite(cesm_proj_dat, file = "./data/cesm_proj_dat.csv")
	fwrite(gfdl_proj_dat, file = "./data/gfdl_proj_dat.csv")
	fwrite(miroc_proj_dat, file = "./data/miroc_proj_dat.csv")

	
	cesm_proj_dat <- fread(file = "./data/cesm_proj_dat.csv") %>%
		mutate(latitude = Lat,
					 longitude = Lon)
	gfdl_proj_dat <- fread(file = "./data/gfdl_proj_dat.csv")  %>%
		mutate(latitude = Lat,
					 longitude = Lon)
	miroc_proj_dat <- fread(file = "./data/miroc_proj_dat.csv")  %>%
		mutate(latitude = Lat,
					 longitude = Lon)
	
	df_list <- list(cesm_proj_dat, gfdl_proj_dat, miroc_proj_dat)
