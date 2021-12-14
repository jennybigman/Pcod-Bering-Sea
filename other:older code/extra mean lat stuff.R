
# calculating the mean latitude of spawning habitat suitability

	#### year ####
	
	#### hindcasts ####
	
	hind_mean_lat_yr <- function(x){
		
		new_dat <- ROMS_hindcast_dat %>%
			filter(., sp_hab_suit >= x)
		
		new_dat_sum <- new_dat %>%
			group_by(year) %>%
			summarise(hist_mean_lat = mean(latitude)) 

		new_dat_sum		
	}
	
	sp_hab_thresholds <- c(0.5, 0.9)
	
	hind_mean_lats_yr <- lapply(sp_hab_thresholds, hind_mean_lat_yr)
	
	hind_mean_lats_yr_0.5 <- hind_mean_lats_yr[[1]] %>%
		mutate(sp_hab_threshold = 0.5)

	hind_mean_lats_yr_0.9 <- hind_mean_lats_yr[[2]]	%>%
		mutate(sp_hab_threshold = 0.9)
	

	
	#### projections ####
	
	proj_mean_lat_yr <- function(x){
		
		new_dat <- ROMS_proj_dat %>%
			filter(., sp_hab_suit >= x)
		
		new_dat_sum <- new_dat %>%
			group_by(simulation, projection, year) %>%
			summarise(proj_mean_lat = mean(latitude)) 

		new_dat_sum		
	}
	
	sp_hab_thresholds <- c(0.5, 0.9)
	
	proj_mean_lats_yr <- lapply(sp_hab_thresholds, proj_mean_lat_yr)
	
	proj_mean_lats_yr_0.5 <- proj_mean_lats_yr[[1]] %>%
		mutate(sp_hab_threshold = 0.5)

	proj_mean_lats_yr_0.9 <- proj_mean_lats_yr[[2]]	%>%
		mutate(sp_hab_threshold = 0.9)
	
	proj_mean_lats_yr_0.5 <- tidyr::unite(proj_mean_lats_yr_0.5,"sim_proj",
																			 simulation, projection, remove = F)
	
	proj_mean_lats_yr_0.9 <- tidyr::unite(proj_mean_lats_yr_0.9,"sim_proj",
																			 simulation, projection, remove = F)
	
	
	# plot ####
	
	## 0.5 ####
	
	mean_lat_05 <-    
   	ggplot() +
   	geom_line(data = hind_mean_lats_yr_0.5, 
   						aes(x = year, y = hist_mean_lat), 
   						color = "grey", alpha = 0.5) +
		geom_line(data = proj_mean_lats_yr_0.5,
							aes(year, proj_mean_lat, 
									group = sim_proj, 
									color = sim_proj), alpha = 0.5) +
		facet_wrap(~ simulation) +
		xlab("Year") + 
		scale_color_manual(name = "sim_proj", values = colors) +
	  scale_y_continuous(
	  	name = "Mean latitude",
	  	breaks = c(57, 60, 63),
	  ) +
		scale_x_continuous(
	  	name = "Year",
	  	breaks = c(1980, 2030, 2080)) +
   	theme_bw() +
  	theme(legend.position = "none") +
  	theme(
			strip.background = element_blank(),
  		strip.text = element_text(size = 18, face = "bold"),
			axis.text = element_text(size = 16, colour = "grey50"),
  	  axis.ticks = element_line(colour = "grey50"),
  	  axis.line = element_line(colour = "grey50"),
  	  axis.title = element_text(size=18, color = "grey30"),
  	  panel.grid.major = element_blank(),
  	  panel.grid.minor = element_blank(),
  	  panel.border = element_rect(fill = NA, color = "grey50"))
	
	
	
	# mean_lat separately
	
	cesm_mean_lat05 <- cesm_proj_dat %>%
		group_by(projection, year) %>%
		summarize(mean_lat = mean(latitude))

		
	cesm_mean_lat09 <- cesm_proj_dat %>%
		group_by(projection, year) %>%
		summarize(mean_lat = mean(latitude))
