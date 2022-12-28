# calculating hatch success	

	library(here)
	library(data.table)

	#### using bias correction at the monthly/domain level ####
	
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
	
	#### using bias correction at the weekly/grid cell level ####
	
	proj_df_list <- list(cesm_dfs_trim_wkgc, 
  										 gfdl_dfs_trim_wkgc,
  										 miroc_dfs_trim_wkgc)
 

	# summarize scaling factors across months
   
	mo_keep <- 1:4
	
   sum_mo_func <- function(df){
   	
   	df <- df %>%
   	filter(month %in% mo_keep) %>%
   	group_by(year, simulation, scenario, month, latitude, longitude) %>%
   	summarize(bc_temp_mo = mean(bc_temp))
   	
   }
   
   mo_dfs <- lapply(proj_df_list, sum_mo_func)
   
   proj_mo_dfs <- bind_rows(mo_dfs)
  
   
	# calculate hatch success
	hatch_success_cauchy_func <- function(x, k = 0.453, mu = 4.192, sigma = 2.125 ){
  			 		(k / (1 + (((x - mu)/sigma))^2)) 
	}
	
	proj_mo_dfs <- proj_mo_dfs %>%
		mutate(hatch_success = sapply(bc_temp_mo, hatch_success_cauchy_func))

	# standardize hatch success (calculating spawning habitat suitability)
 
	max_hatch <- max(proj_mo_dfs$hatch_success)
	
	proj_mo_dfs <- proj_mo_dfs %>%
		rowwise() %>%
  	mutate(sp_hab_suit = hatch_success/max_hatch)

	fwrite(proj_mo_dfs, file = "./data/proj_mo_dfs.csv")
	
	proj_mo_dfs <- fread(file = "./data/proj_mo_dfs.csv")
