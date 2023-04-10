
	#00d - trimming to ROMS grid by ROMS domains

	# merge area and bottom temp dfs and trim by shelf region and depth

	library(here)
	library(data.table)
	library(tidyverse)
	library(sf)
	library(rnaturalearth)

	# for plotting
	world_map_data <- ne_countries(scale = "medium", returnclass = "sf") 
	
	breaks_x <- c(-170, -160)
	labels_x <- c("-170˚", "-160˚") 
	limits_x <- c(-1400000, -150000)
	
	breaks_y <- c(55, 60)
	limits_y <- c(470000, 1900000)

	
	#### merge dfs ####
	
	# load dfs
	temp_hind_dat <- fread(file = here("./data/hindcast_temp_K20.csv")) 
	area_df <-   fread(file = "./data/ROMS_area_grid_cells.csv") 
	depth_df <- fread(file =  "./data/ROMS_depth_df.csv")  
	domain_df <- fread(file = "./data/ROMS_domain_df.csv")  

	# join dfs
	
	vars <- c("Xi", "Eta")
		
	area_df <- area_df  %>%
  		mutate_at(vars, as.character)
	
	depth_df <- depth_df  %>%
  		mutate_at(vars, as.character)

	domain_df <- domain_df  %>%
  		mutate_at(vars, as.character)
	
	temp_df <- temp_hind_dat %>%
  		mutate_at(vars, as.character)

	area_depth_df <- full_join(area_df, depth_df, by = c("Xi", "Eta"))

	area_depth_domain_df <- full_join(area_depth_df, domain_df,
					by = c("Xi", "Eta")) %>%
		  		mutate_at(vars, as.character)

	ROMS_dat_hind <- full_join(temp_df, area_depth_domain_df,
														 by = c("Xi", "Eta"))
	
	ROMS_dat_hind <- ROMS_dat_hind %>%
		rename(latitude = latitude.y,
					 longitude = longitude.x)

	ROMS_dat_hind <- ROMS_dat_hind %>%
		dplyr::select(-contains("x"))
	
	ROMS_dat_hind <- ROMS_dat_hind %>%
		dplyr::select(-contains("y"))
	
	# add date and month name
	ROMS_dat_hind$month_name <- NA
     
  ROMS_dat_hind$month_name[ROMS_dat_hind$month == 1] <- "January"
  ROMS_dat_hind$month_name[ROMS_dat_hind$month == 2] <- "February"
	ROMS_dat_hind$month_name[ROMS_dat_hind$month == 3] <- "March"
	ROMS_dat_hind$month_name[ROMS_dat_hind$month == 4] <- "April"


	# trim by month
	sp_months <- c(1:4)
	
	ROMS_dat_hind_trim <- ROMS_dat_hind %>%
		filter(month %in% sp_months)

	ROMS_dat_hind_trim <- ROMS_dat_hind_trim %>%
			filter(., between(depth, 0, 250)) %>%
			filter(domain > 0) 

	# summarize to plot
	ROMS_dat_hind_trim_sum <-  ROMS_dat_hind_trim %>%
		group_by(latitude, longitude) %>%
		summarize(mean_temp = mean(temp))
	
	ROMS_dat_hind_trim_sum_sf <- ROMS_dat_hind_trim_sum	%>% 
		mutate(long_not_360 = case_when(
					 longitude >= 180 ~ longitude - 360,
					 longitude < 180 ~ longitude))  %>%
  	st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326, remove = FALSE)
	
 	ggplot() +
		geom_sf(data = ROMS_dat_hind_trim_sum_sf, aes(color = mean_temp))  +
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
 	
	# save
#	fwrite(ROMS_dat_hind_trim, "./data/ROMS_dat_hind_trim.csv")

	
	
	