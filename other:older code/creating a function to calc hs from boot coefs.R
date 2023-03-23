 
	ROMS_hind <- ROMS_dat_hind_trim %>%
		dplyr::select(temp, year, latitude, longitude)

	row_nums <- 1:10
	
	dat_combine_func <- function(x){
 
 		k <- rep(cauchy_boot_sample$k[x], nrow(ROMS_hind))
		mu <- rep(cauchy_boot_sample$mu[x], nrow(ROMS_hind))
		sigma <- rep(cauchy_boot_sample$sigma[x], nrow(ROMS_hind))
		
		coefs <- tibble(k, mu, sigma)
		
		ROMS_temp_hind_sens <- bind_cols(ROMS_hind, coefs) 
		
		
	}
	
 dat_list <- lapply(row_nums, dat_combine_func)
	

 hs_func_cauchy <- function(df){
 	
	df <- df %>% 
	 rowwise() %>% 
	 mutate(hs_cauch = (k / (1 + (((temp - mu)/sigma))^2)))
	
	max_hs <- max(df$hs_cauch)
	
	df <- df %>%
		mutate(sphabsuit_cauch = hs_cauch/max_hs)
 }
 
 dat_list2 <- lapply(dat_list, hs_func_cauchy)


 
 
 
 
 

	hs_cauch_func <- function(k, temp, mu, sigma){
  		 		(k / (1 + (((temp - mu)/sigma))^2))}
	

	mapply(function(df, z) mutate_(df, z=z), dfs, 3:4, SIMPLIFY=FALSE)
	
	df_list <- mapply(hs_cauch_func)