# hatch success for hindcast


	library(here)
	library(data.table)

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
					 hatch_success_gaus = sapply(temp, hatch_success_gaus_func)) %>% na.omit()
	
	# standardize hatch success (calculating spawning habitat suitability)
	ROMS_dat_hind_trim <- ROMS_dat_hind_trim %>%
  	mutate(sp_hab_suit = hatch_success_cauchy/max(hatch_success_cauchy))
	
	### plot to see if this works ####
	ROMS_dat_hind_trim_sum <- ROMS_dat_hind_trim %>%
		mutate(latitude = lat,
					 longitude - lon) %>%
		group_by(latitude, longitude) %>%
		summarize(mean_sp_hab_suit = mean(sp_hab_suit))
	
	ROMS_dat_hind_trim_sum_sf <- ROMS_dat_hind_trim_sum	%>% 
		mutate(long_not_360 = case_when(
					 longitude >= 180 ~ longitude - 360,
					 longitude < 180 ~ longitude))  %>%
  	st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326, remove = FALSE)
	
 		ggplot() +
		geom_sf(data = ROMS_dat_hind_trim_sum_sf, aes(color = mean_sp_hab_suit))  +
		geom_sf(data = world_map_data, fill = "darkgrey", lwd = 0) +
		coord_sf(crs = 3338) +
		scale_color_viridis_c() +
 		scale_x_continuous(
 			breaks = breaks_x,
 			labels = labels_x,
 			limits = limits_x
 		) +
 		scale_y_continuous(
 			breaks = breaks_y,
 			limits = limits_y
 		) +
		theme_bw() 
 	
	
	
	
	
	# save file
	fwrite(ROMS_dat_hind_trim, file = "./data/ROMS_dat_hind_trim.csv")
	