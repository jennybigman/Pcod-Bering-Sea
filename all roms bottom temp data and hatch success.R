# reading in all ROMS bottom temp data and calculating hatch success	

	setwd("~/Google Drive/NOAA AFSC Postdoc/Pcod Bering Sea Habitat Suitability")
	temp_df <- fread("./data/ROMS_all_temp.csv")

# restrict dataset to only those months of spawning (January to June)

	sp_months <- c(1:6)

	temp_df$date <- as.Date(temp_df$DateTime) # date in Date format
	temp_df$month <- month(temp_df$date) # month of year
	temp_df$week <- week(temp_df$date) # week of year
	temp_df$year <- year(temp_df$date)
	
	sm_temp_df <- temp_df %>% filter(month %in% sp_months)
	
	
	hatch_success_cauchy_func <- function(x, k = 0.453, mu = 4.192, sigma = 2.125 ){
  			 		(k / (1 + (((x - mu)/sigma))^2)) 
	}

	hatch_success_gaus_func <- function(x, k = 0.395, mu = 4.50, sigma = 2.58){
        	k * exp(-1/2 * (x - mu)^2/sigma^2)
	} 
    
	sm_temp_df <- sm_temp_df %>%
		mutate(hatch_success_cauchy = sapply(temp, hatch_success_cauchy_func),
					 hatch_success_gaus = sapply(temp, hatch_success_gaus_func))
 
	sm_temp_df <- sm_temp_df %>%
		rename(latitude = Lat,
					 longitude = Lon) %>%
		filter(., between(latitude, 53, 69)) %>%
		filter(., between(longitude, 179, 202)) %>%
		mutate(longitude360 = 
					 case_when(longitude > 180 ~ longitude - 360,
										 longitude <= 180 ~ longitude)) 
	
	fwrite(sm_temp_df, file = "./Pcod-Bering-Sea/data/SpawnMonths_Temp_HatchSuccess.csv")