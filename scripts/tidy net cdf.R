

	# set up download from server 
	url_base <- "https://data.pmel.noaa.gov/aclim/thredds/"
	opendap  <- "dodsC/Level2/B10K-K20_CORECFS_bottom5m.nc"
	
	file_path <- paste0(url_base, opendap)
	
	# function to download temp from time steps until 2660 
	temp_hind_func <- function(x, y){
	 	
	 	tidy_temps <- tidync(file_path) %>% 
			activate("temp") %>% 
	 		hyper_filter(ocean_time = between(index, x, y)) %>%
	 		hyper_tibble()
	 	
	 	tidy_temps
	 	
	 }
	 
	 start_step <- seq(from = 1, to = 2660, by = 10)
	 end_step <- seq(from = 10, to = 2660, by = 10)

	 tidy_temps_out <- mapply(temp_hind_func,
	 													x = start_step,
	 													y = end_step, 
	 												  SIMPLIFY = FALSE)
	 

	 	 # download remaining time steps (doesn't work all at once for some reason)
	 tidy_temps2 <- tidync(file_path) %>% 
			activate("temp") %>% 
	 		hyper_filter(ocean_time = between(index, 2660, 2680)) %>%
	 		hyper_tibble()
	 
	# bind all time steps together
	 tidy_temps_out_all <- rbind(tidy_temps_out) 
	 
	 df <- data.frame(matrix(unlist(tidy_temps_out),
	 												byrow=TRUE),stringsAsFactors=FALSE)
