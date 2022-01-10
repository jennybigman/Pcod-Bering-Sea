# code for scripts 02 - 0X with bias-corrected temps without variance ratio

#### 02 #### maps of temp and habitat suitability 

# plots

# no variance ratio
	
		temp_novar_monthly_plot_func <- function(x){
		
		    new_dat <- cesm_df %>% filter(., year == x)
    
    	  plot <- 
    	  	ggplot() +
					geom_sf(data = new_dat, aes(color = bc_temp))  +
					geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
					coord_sf(crs = 3338) +
					scale_color_viridis_c() +
    	  	facet_wrap(~ month_name, ncol = 3, nrow = 3) +
 					scale_x_continuous(
 						breaks = c(-175, -170, -165, -160),
 						labels = c("-175˚", "-170˚", "-165˚", "-160˚"),
 						name = "Longitude",
 						limits = c(-1400000, -150000)
 					) +
 					scale_y_continuous(
 						breaks = c(55, 60),
 						limits = c(470000, 1900000),
 						name = "Latitude",
 					) +
    	  	labs(colour = "projected temp no var") +
					theme_bw() +
 					theme(
 						strip.text = element_text(size = 14, face = "bold"),
 						strip.background = element_blank(),
 						axis.text = element_text(size = 12),	
  					axis.title = element_text(size = 14),
  					legend.title.align=0.5)
    	  
    	  plot
    	  
	}
	
	years <- c(1980:2099)

	monthly_plot_list <- lapply(years, temp_novar_monthly_plot_func)
  
  mo_name_func_year <- function(x){
  	year_month <- paste0(x, "_proj_temp_novar_cesm")
  }
   
  month_names_year <- sapply(years, mo_name_func_year)
  
	mo_name_func_file <- function(x){
  	year_month <- paste0("/Users/jenniferbigman/My Drive/NOAA AFSC Postdoc/Pcod Bering Sea Habitat Suitability/Pcod-Bering-Sea/output/plots/monthly plots/habitat suitability/projections/", x)
  }
   
  month_names <- sapply(month_names_year, mo_name_func_file)
				
	plot_list <- mapply(ggsave_func, x = monthly_plot_list, y = month_names)

	# no var ####
	
	hab_suit_novar_monthly_plot_func <- function(x){
		
		    new_dat <- cesm_df %>% filter(., year == x)
    
    	  plot <- 
    	  	ggplot() +
					geom_sf(data = new_dat, aes(color = sp_hab_suit_novar))  +
					geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
					coord_sf(crs = 3338) +
					scale_color_gradientn(colors = c("#B3E5FC", "#B3E5FC", 
																					 "#01579B", "#01579B",
																					 "#00345C", "#00345C"),
																values = c(0, 0.499, 0.5, 0.899, 0.9, 1),
																breaks = c(0.1, 0.5, 0.9),
																labels = format(c(0.1, 0.5, 0.9)),
																limits = c(0, 1)) +
    	  	facet_wrap(~ month_name, ncol = 3, nrow = 3) +
 					scale_x_continuous(
 						breaks = c(-175, -170, -165, -160),
 						labels = c("-175˚", "-170˚", "-165˚", "-160˚"),
 						name = "Longitude",
 						limits = c(-1400000, -150000)
 					) +
 					scale_y_continuous(
 						breaks = c(55, 60),
 						limits = c(470000, 1900000),
 						name = "Latitude",
 					) +
    	  	labs(colour = "Spawning\nhabitat\nsuitability") +
					theme_bw() +
 					theme(
 						strip.text = element_text(size = 14, face = "bold"),
 						strip.background = element_blank(),
 						axis.text = element_text(size = 12),	
  					axis.title = element_text(size = 14),
  					legend.title.align=0.5)
    	  
    	  plot
    	  
	}
	
	years <- c(1980:2099)

	monthly_plot_list <- lapply(years, hab_suit_novar_monthly_plot_func)
  
  mo_name_func_year <- function(x){
  	year_month <- paste0(x, "_proj_habitat_suitability_novar_5090_cesm")
  }
   
  month_names_year <- sapply(years, mo_name_func_year)
  
	mo_name_func_file <- function(x){
  	year_month <- paste0("/Users/jenniferbigman/My Drive/NOAA AFSC Postdoc/Pcod Bering Sea Habitat Suitability/Pcod-Bering-Sea/output/plots/monthly plots/habitat suitability/projections/", x)
  }
   
  month_names <- sapply(month_names_year, mo_name_func_file)
				
	plot_list <- mapply(ggsave_func, x = monthly_plot_list, y = month_names) ### this does't work

	
		# temp ####
	
	# no variance ratio
	
		temp_novar_monthly_plot_func <- function(x){
		
		    new_dat <- gfdl_df %>% filter(., year == x)
    
    	  plot <- 
    	  	ggplot() +
					geom_sf(data = new_dat, aes(color = bc_temp))  +
					geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
					coord_sf(crs = 3338) +
					scale_color_viridis_c() +
    	  	facet_wrap(~ month_name, ncol = 3, nrow = 3) +
 					scale_x_continuous(
 						breaks = c(-175, -170, -165, -160),
 						labels = c("-175˚", "-170˚", "-165˚", "-160˚"),
 						name = "Longitude",
 						limits = c(-1400000, -150000)
 					) +
 					scale_y_continuous(
 						breaks = c(55, 60),
 						limits = c(470000, 1900000),
 						name = "Latitude",
 					) +
    	  	labs(colour = "projected temp no var") +
					theme_bw() +
 					theme(
 						strip.text = element_text(size = 14, face = "bold"),
 						strip.background = element_blank(),
 						axis.text = element_text(size = 12),	
  					axis.title = element_text(size = 14),
  					legend.title.align=0.5)
    	  
    	  plot
    	  
	}
	
	years <- c(1980:2099)

	monthly_plot_list <- lapply(years, temp_novar_monthly_plot_func)
  
  mo_name_func_year <- function(x){
  	year_month <- paste0(x, "_proj_temp_novar_gfdl")
  }
   
  month_names_year <- sapply(years, mo_name_func_year)
  
	mo_name_func_file <- function(x){
  	year_month <- paste0("/Users/jenniferbigman/My Drive/NOAA AFSC Postdoc/Pcod Bering Sea Habitat Suitability/Pcod-Bering-Sea/output/plots/projections/temperature maps/gfdl/temp with no var/", x)
  }
   
  month_names <- sapply(month_names_year, mo_name_func_file)
				
	plot_list <- mapply(ggsave_func, x = monthly_plot_list, y = month_names)

	
		hab_suit_novar_monthly_plot_func <- function(x){
		
		    new_dat <- gfdl_df %>% filter(., year == x)
    
    	  plot <- 
    	  	ggplot() +
					geom_sf(data = new_dat, aes(color = sp_hab_suit_novar))  +
					geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
					coord_sf(crs = 3338) +
					scale_color_gradientn(colors = c("#B3E5FC", "#B3E5FC", 
																					 "#01579B", "#01579B",
																					 "#00345C", "#00345C"),
																values = c(0, 0.499, 0.5, 0.899, 0.9, 1),
																breaks = c(0.1, 0.5, 0.9),
																labels = format(c(0.1, 0.5, 0.9)),
																limits = c(0, 1)) +
    	  	facet_wrap(~ month_name, ncol = 3, nrow = 3) +
 					scale_x_continuous(
 						breaks = c(-175, -170, -165, -160),
 						labels = c("-175˚", "-170˚", "-165˚", "-160˚"),
 						name = "Longitude",
 						limits = c(-1400000, -150000)
 					) +
 					scale_y_continuous(
 						breaks = c(55, 60),
 						limits = c(470000, 1900000),
 						name = "Latitude",
 					) +
    	  	labs(colour = "Spawning\nhabitat\nsuitability") +
					theme_bw() +
 					theme(
 						strip.text = element_text(size = 14, face = "bold"),
 						strip.background = element_blank(),
 						axis.text = element_text(size = 12),	
  					axis.title = element_text(size = 14),
  					legend.title.align=0.5)
    	  
    	  plot
    	  
	}
	
	years <- c(1980:2099)

	monthly_plot_list <- lapply(years, hab_suit_novar_monthly_plot_func)
  
  mo_name_func_year <- function(x){
  	year_month <- paste0(x, "_proj_habitat_suitability_novar_5090_gfdl")
  }
   
  month_names_year <- sapply(years, mo_name_func_year)
  
	mo_name_func_file <- function(x){
  	year_month <- paste0("/Users/jenniferbigman/My Drive/NOAA AFSC Postdoc/Pcod Bering Sea Habitat Suitability/Pcod-Bering-Sea/output/plots/projections/habitat suitability maps/gfdl/with no var/", x)
  }
   
  month_names <- sapply(month_names_year, mo_name_func_file)
				
	plot_list <- mapply(ggsave_func, x = monthly_plot_list, y = month_names)

	
		# no variance ratio
	
		temp_novar_monthly_plot_func <- function(x){
		
		    new_dat <- miroc_df %>% filter(., year == x)
    
    	  plot <- 
    	  	ggplot() +
					geom_sf(data = new_dat, aes(color = bc_temp))  +
					geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
					coord_sf(crs = 3338) +
					scale_color_viridis_c() +
    	  	facet_wrap(~ month_name, ncol = 3, nrow = 3) +
 					scale_x_continuous(
 						breaks = c(-175, -170, -165, -160),
 						labels = c("-175˚", "-170˚", "-165˚", "-160˚"),
 						name = "Longitude",
 						limits = c(-1400000, -150000)
 					) +
 					scale_y_continuous(
 						breaks = c(55, 60),
 						limits = c(470000, 1900000),
 						name = "Latitude",
 					) +
    	  	labs(colour = "projected temp no var") +
					theme_bw() +
 					theme(
 						strip.text = element_text(size = 14, face = "bold"),
 						strip.background = element_blank(),
 						axis.text = element_text(size = 12),	
  					axis.title = element_text(size = 14),
  					legend.title.align=0.5)
    	  
    	  plot
    	  
	}
	
	years <- c(1980:2099)

	monthly_plot_list <- lapply(years, temp_novar_monthly_plot_func)
  
  mo_name_func_year <- function(x){
  	year_month <- paste0(x, "_proj_temp_novar_miroc")
  }
   
  month_names_year <- sapply(years, mo_name_func_year)
  
	mo_name_func_file <- function(x){
  	year_month <- paste0("/Users/jenniferbigman/My Drive/NOAA AFSC Postdoc/Pcod Bering Sea Habitat Suitability/Pcod-Bering-Sea/output/plots/projections/temperature maps/miroc/temp with no var/", x)
  }
   
  month_names <- sapply(month_names_year, mo_name_func_file)
				
	plot_list <- mapply(ggsave_func, x = monthly_plot_list, y = month_names)

		hab_suit_novar_monthly_plot_func <- function(x){
		
		    new_dat <- miroc_df %>% filter(., year == x)
    
    	  plot <- 
    	  	ggplot() +
					geom_sf(data = new_dat, aes(color = sp_hab_suit_novar))  +
					geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
					coord_sf(crs = 3338) +
					scale_color_gradientn(colors = c("#B3E5FC", "#B3E5FC", 
																					 "#01579B", "#01579B",
																					 "#00345C", "#00345C"),
																values = c(0, 0.499, 0.5, 0.899, 0.9, 1),
																breaks = c(0.1, 0.5, 0.9),
																labels = format(c(0.1, 0.5, 0.9)),
																limits = c(0, 1)) +
    	  	facet_wrap(~ month_name, ncol = 3, nrow = 3) +
 					scale_x_continuous(
 						breaks = c(-175, -170, -165, -160),
 						labels = c("-175˚", "-170˚", "-165˚", "-160˚"),
 						name = "Longitude",
 						limits = c(-1400000, -150000)
 					) +
 					scale_y_continuous(
 						breaks = c(55, 60),
 						limits = c(470000, 1900000),
 						name = "Latitude",
 					) +
    	  	labs(colour = "Spawning\nhabitat\nsuitability") +
					theme_bw() +
 					theme(
 						strip.text = element_text(size = 14, face = "bold"),
 						strip.background = element_blank(),
 						axis.text = element_text(size = 12),	
  					axis.title = element_text(size = 14),
  					legend.title.align=0.5)
    	  
    	  plot
    	  
	}
	
	years <- c(1980:2099)

	monthly_plot_list <- lapply(years, hab_suit_novar_monthly_plot_func)
  
  mo_name_func_year <- function(x){
  	year_month <- paste0(x, "_proj_habitat_suitability_novar_5090_miroc")
  }
   
  month_names_year <- sapply(years, mo_name_func_year)
  
	mo_name_func_file <- function(x){
  	year_month <- paste0("/Users/jenniferbigman/My Drive/NOAA AFSC Postdoc/Pcod Bering Sea Habitat Suitability/Pcod-Bering-Sea/output/plots/projections/habitat suitability maps/miroc/with no var/", x)
  }
   
  month_names <- sapply(month_names_year, mo_name_func_file)
				
	plot_list <- mapply(ggsave_func, x = monthly_plot_list, y = month_names)

	
	#### 03 ####
	
		#### index of suitable spawning habitat ####
	
	yearly_hab_dat_hind <- ROMS_hindcast_dat %>%
		group_by(year) %>%
    summarise(annual_hatch_success_cauchy = mean(hatch_success_cauchy),
   					  annual_hatch_success_gaussian = mean(hatch_success_gaus),
    					annual_spawning_hab_suit = mean(sp_hab_suit))
	
	yearly_hab_dat_proj <- ROMS_projected_dat %>% 
		group_by(simulation, projection, year) %>%
   	summarise(annual_hatch_success_cauchy_novar = mean(hatch_success_cauchy_novar),
   						annual_hatch_success_cauchy_var = mean(hatch_success_cauchy_var),
   					  annual_hatch_success_gaussian_novar = mean(hatch_success_gaus_novar),
   						annual_hatch_success_gaussian_var = mean(hatch_success_gaus_var),
   						annual_spawning_hab_suit_novar = mean(sp_hab_suit_novar),
   						annual_spawning_hab_suit_var = mean(sp_hab_suit_var)) 
	
	# create interaction variable for plotting different colors
	
	yearly_hab_dat_proj <- yearly_hab_dat_proj %>%
		tidyr::unite("sim_proj", simulation, projection, remove = F)

	colors <- c("lightgrey", "#efd966", "#b79a00", 
						  "lightgrey", "#7fb27f", "#004700", 
						  "lightgrey", "#6666b2", "#000059")

	sim_proj <- unique(yearly_hab_dat_proj$sim_proj)
	
	names(colors) <- unique(yearly_hab_dat_proj$sim_proj)
	
	annual_hatch_success_cauchy_novar <-    
   	ggplot() +
   	geom_line(data = yearly_hab_dat_hind, 
   						aes(x = year, y = annual_spawning_hab_suit), 
   						color = "black", alpha = 0.5) +
		geom_line(data = yearly_hab_dat_proj,
							aes(year, annual_spawning_hab_suit_novar, 
									group = sim_proj, 
									color = sim_proj), alpha = 0.5) +
		facet_wrap(~ simulation) +
		xlab("Year") + 
		scale_color_manual(name = "sim_proj", values = colors) +
	  scale_y_continuous(
	  	name = "Annual spawning\nhabitat suitability\nno var",
	  	breaks = c(0.20, 0.40, 0.60, 0.80),
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
	
	
		ggsave("./output/plots/annual_hatch_success_cauchy_hindproj_novar.png",
			 annual_hatch_success_cauchy_novar,
			 width = 15, height = 5, units = "in")
		
		# with variance
		
	annual_hatch_success_cauchy_var <-    
   	ggplot() +
   	geom_line(data = yearly_hab_dat_hind, 
   						aes(x = year, y = annual_spawning_hab_suit), 
   						color = "black", alpha = 0.5) +
		geom_line(data = yearly_hab_dat_proj,
							aes(year, annual_spawning_hab_suit_var, 
									group = sim_proj, 
									color = sim_proj), alpha = 0.5) +
		facet_wrap(~ simulation) +
		xlab("Year") + 
		scale_color_manual(name = "sim_proj", values = colors) +
	  scale_y_continuous(
	  	name = "Annual spawning\nhabitat suitability\nvar",
	  	breaks = c(0.20, 0.40, 0.60, 0.80),
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
	
	
		ggsave("./output/plots/annual_hatch_success_cauchy_hindproj_var.png",
			 annual_hatch_success_cauchy_var,
			 width = 15, height = 5, units = "in")
		
	
	## monthly
	
	mo_hab_dat_hind <- ROMS_hindcast_dat %>%
		group_by(year, month_name, month) %>%
    summarise(avg_hatch_success_cauchy = mean(hatch_success_cauchy),
   					  avg_hatch_success_gaussian = mean(hatch_success_gaus),
    					avg_spawning_hab_suit = mean(sp_hab_suit))
	
	mo_hab_dat_proj <- ROMS_projected_dat %>% 
		group_by(simulation, projection, year, month_name, month) %>%
   	summarise(avg_hatch_success_cauchy_novar = mean(hatch_success_cauchy_novar),
   						avg_hatch_success_cauchy_var = mean(hatch_success_cauchy_var),
   					  avg_hatch_success_gaussian_novar = mean(hatch_success_gaus_novar),
   						avg_hatch_success_gaussian_var = mean(hatch_success_gaus_var),
   						avg_spawning_hab_suit_novar = mean(sp_hab_suit_novar),
   						avg_spawning_hab_suit_var = mean(sp_hab_suit_var)) 
		
	
	mo_hab_dat_proj <- mo_hab_dat_proj %>%
		tidyr::unite("sim_proj", simulation, projection, remove = F)

	colors <- c("lightgrey", "#efd966", "#b79a00", 
						  "lightgrey", "#7fb27f", "#004700", 
						  "lightgrey", "#6666b2", "#000059")
	
	sim_proj <- unique(mo_hab_dat_proj$sim_proj)
	
	names(colors) <- unique(mo_hab_dat_proj$sim_proj)
	
	# reorder for plotting
	mo_hab_dat_hind$month_name <- factor(mo_hab_dat_hind$month_name)
  mo_hab_dat_hind$month_name <- fct_reorder(mo_hab_dat_hind$month_name, 
  																		mo_hab_dat_hind$month)
  
  mo_hab_dat_proj$month_name <- factor(mo_hab_dat_proj$month_name)
  mo_hab_dat_proj$month_name <- fct_reorder(mo_hab_dat_proj$month_name, 
  																		mo_hab_dat_proj$month)

	mo_hatch_success_cauchy_novar <-    
   	ggplot() +
   	geom_line(data = mo_hab_dat_hind, 
   						aes(x = year, y = avg_spawning_hab_suit), 
   						color = "black", alpha = 0.5) +
		geom_line(data = mo_hab_dat_proj,
							aes(year, avg_spawning_hab_suit_novar, 
									group = sim_proj, 
									color = sim_proj), alpha = 0.5) +
		facet_wrap(~ simulation + month_name) +
		xlab("Year") + 
		scale_color_manual(name = "sim_proj", values = colors) +
	  scale_y_continuous(
	  	name = "Mean spawning\nhabitat suitability\nno var",
	  	breaks = c(0.20, 0.40, 0.60, 0.80),
	  ) +
		scale_x_continuous(
	  	name = "Year",
	  	breaks = c(1980, 2030, 2080)) +
   	theme_bw() +
  	theme(legend.position = "none") +
  	theme(
			strip.background = element_blank(),
  		strip.text = element_text(size = 16, face = "bold"),
			axis.text = element_text(size = 16, colour = "grey50"),
  	  axis.ticks = element_line(colour = "grey50"),
  	  axis.line = element_line(colour = "grey50"),
  	  axis.title = element_text(size=18, color = "grey30"),
  	  panel.grid.major = element_blank(),
  	  panel.grid.minor = element_blank(),
  	  panel.border = element_rect(fill = NA, color = "grey50"))
	
	
		ggsave("./output/plots/mo_hatch_success_cauchy_hindproj_novar.png",
			 mo_hatch_success_cauchy_novar,
			 width = 10, height = 7, units = "in")
	
		
	mo_hatch_success_cauchy_var <-    
   	ggplot() +
   	geom_line(data = mo_hab_dat_hind, 
   						aes(x = year, y = avg_spawning_hab_suit), 
   						color = "black", alpha = 0.5) +
		geom_line(data = mo_hab_dat_proj,
							aes(year, avg_spawning_hab_suit_var, 
									group = sim_proj, 
									color = sim_proj), alpha = 0.5) +
		facet_wrap(~ simulation + month_name) +
		xlab("Year") + 
		scale_color_manual(name = "sim_proj", values = colors) +
	  scale_y_continuous(
	  	name = "Mean spawning\nhabitat suitability\nvar",
	  	breaks = c(0.20, 0.40, 0.60, 0.80),
	  ) +
		scale_x_continuous(
	  	name = "Year",
	  	breaks = c(1980, 2030, 2080)) +
   	theme_bw() +
  	theme(legend.position = "none") +
  	theme(
			strip.background = element_blank(),
  		strip.text = element_text(size = 16, face = "bold"),
			axis.text = element_text(size = 16, colour = "grey50"),
  	  axis.ticks = element_line(colour = "grey50"),
  	  axis.line = element_line(colour = "grey50"),
  	  axis.title = element_text(size=18, color = "grey30"),
  	  panel.grid.major = element_blank(),
  	  panel.grid.minor = element_blank(),
  	  panel.border = element_rect(fill = NA, color = "grey50"))
	
	
		ggsave("./output/plots/mo_hatch_success_cauchy_hindproj_var.png",
			 mo_hatch_success_cauchy_var,
			 width = 10, height = 7, units = "in")
	
		
		
		
	#### temp ####
		
	# monthly
		
	mo_temp_hind <- ROMS_hindcast_dat %>%
		group_by(year, month_name, month) %>%
    summarise(avg_temp = mean(temp))
	
	mo_temp_proj <- ROMS_projected_dat %>% 
		group_by(simulation, projection, year, month_name, month) %>%
   	summarise(avg_temp_novar = mean(bc_temp),
   						avg_temp_var = mean(bc_temp_sd)) 
		
	
	mo_temp_proj <- mo_temp_proj %>%
		tidyr::unite("sim_proj", simulation, projection, remove = F)

	colors <- c("lightgrey", "#efd966", "#b79a00", 
						  "lightgrey", "#7fb27f", "#004700", 
						  "lightgrey", "#6666b2", "#000059")
	
	sim_proj <- unique(mo_temp_proj$sim_proj)
	
	names(colors) <- unique(mo_temp_proj$sim_proj)
	
	# reorder for plotting
	mo_temp_hind$month_name <- factor(mo_temp_hind$month_name)
  mo_temp_hind$month_name <- fct_reorder(mo_temp_hind$month_name, 
  																		mo_temp_hind$month)
  
  mo_temp_proj$month_name <- factor(mo_temp_proj$month_name)
  mo_temp_proj$month_name <- fct_reorder(mo_temp_proj$month_name, 
  																		mo_temp_proj$month)

	mo_temp_novar <-    
   	ggplot() +
   	geom_line(data = mo_temp_hind, 
   						aes(x = year, y = avg_temp), 
   						color = "black", alpha = 0.5) +
		geom_line(data = mo_temp_proj,
							aes(year, avg_temp_novar, 
									group = sim_proj, 
									color = sim_proj), alpha = 0.5) +
		facet_wrap(~ simulation + month_name) +
		xlab("Year") + 
		scale_color_manual(name = "sim_proj", values = colors) +
	  scale_y_continuous(
	  	name = "Mean projected temp\nno var",
	  	breaks = c(0, 2, 4, 6),
	  ) +
		scale_x_continuous(
	  	name = "Year",
	  	breaks = c(1980, 2030, 2080)) +
   	theme_bw() +
  	theme(legend.position = "none") +
  	theme(
			strip.background = element_blank(),
  		strip.text = element_text(size = 16, face = "bold"),
			axis.text = element_text(size = 16, colour = "grey50"),
  	  axis.ticks = element_line(colour = "grey50"),
  	  axis.line = element_line(colour = "grey50"),
  	  axis.title = element_text(size=18, color = "grey30"),
  	  panel.grid.major = element_blank(),
  	  panel.grid.minor = element_blank(),
  	  panel.border = element_rect(fill = NA, color = "grey50"))
	
	
		ggsave("./output/plots/mo_temp_novar_hindproj_novar.png",
			 mo_temp_novar,
			 width = 10, height = 7, units = "in")
	
		
	mo_temp_var <-    
   	ggplot() +
   	geom_line(data = mo_temp_hind, 
   						aes(x = year, y = avg_temp), 
   						color = "black", alpha = 0.5) +
		geom_line(data = mo_temp_proj,
							aes(year, avg_temp_var, 
									group = sim_proj, 
									color = sim_proj), alpha = 0.5) +
		facet_wrap(~ simulation + month_name) +
		xlab("Year") + 
		scale_color_manual(name = "sim_proj", values = colors) +
	  scale_y_continuous(
	  	name = "Mean projected temp\nvar",
	  	breaks = c(0, 2, 4, 6),
	  ) +
		scale_x_continuous(
	  	name = "Year",
	  	breaks = c(1980, 2030, 2080)) +
   	theme_bw() +
  	theme(legend.position = "none") +
  	theme(
			strip.background = element_blank(),
  		strip.text = element_text(size = 16, face = "bold"),
			axis.text = element_text(size = 16, colour = "grey50"),
  	  axis.ticks = element_line(colour = "grey50"),
  	  axis.line = element_line(colour = "grey50"),
  	  axis.title = element_text(size=18, color = "grey30"),
  	  panel.grid.major = element_blank(),
  	  panel.grid.minor = element_blank(),
  	  panel.border = element_rect(fill = NA, color = "grey50"))
	
	
		ggsave("./output/plots/mo_temp_novar_hindproj_var.png",
			 mo_temp_var,
			 width = 10, height = 7, units = "in")
		
		
		
		
		

	#### comparing year bins: 2000 - 2014 vs. 2015 - 2020 ####
	
	years_keep <- c(2000:2020)
  	
  ROMS_hindcast_dat_sub <- ROMS_hindcast_dat_sf %>% filter(., year %in% years_keep)
  

	# create a list of year bins
	year_bins  <- list('2000-2014' = c(2000:2014),
                     '2015-2020' = c(2015:2020)) 

	# create a list of dfs summarized by year bins
	sum_yr_bin <- function(x){
 
		df <- ROMS_hindcast_dat_sub %>%
		filter(year %in% x) %>%
		group_by(longitude, latitude) %>%
		summarise(mean_hs = mean(hatch_success_cauchy),
							mean_temp = mean(temp),
							mean_sphabsuit = mean(sp_hab_suit)) 
		
		df
	
	}

	df_list <- lapply(year_bins, sum_yr_bin)

	# add a column of the year bin
	df_list_tp <- mapply(cbind, df_list, "time_period"= names(year_bins), SIMPLIFY = FALSE)

	# bind all rows of all dfs together and add a column for longitude not on 360 scale
	sm_temp_hind_df_yr_sum <- bind_rows(df_list_tp) %>% 
  	dplyr::select(latitude, longitude, mean_sphabsuit, time_period)
 
	sm_temp_hind_df_yr_sum_sf <- sm_temp_hind_df_yr_sum %>%
			mutate(long_not_360 = longitude - 360) %>%
  		st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326)
  
	# plot
	
	plot <- ggplot() +
					geom_sf(data = sm_temp_hind_df_yr_sum_sf, aes(color = mean_sphabsuit))  +
					geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
					coord_sf(crs = 3338) +
  		    facet_wrap(~ time_period, nrow = 1) +
					scale_color_viridis_c() +
 					scale_x_continuous(
 						breaks = c(-175, -170, -165, -160),
 						labels = c("-175˚", "-170˚", "-165˚", "-160˚"),
 						name = "Longitude",
 						limits = c(-1400000, -150000)
 					) +
 					scale_y_continuous(
 						breaks = c(55, 60),
 						limits = c(470000, 1900000),
 						name = "Latitude",
 					) +
    	  	labs(colour = "Spawning\nhabitat suitability") +
					theme_bw() +
 					theme(
						strip.text = element_text(size = 14, face = "bold"),
 						strip.background = element_blank(), 						
						axis.text = element_text(size = 12),	
  					axis.title = element_text(size = 14),
  					legend.title.align=0.5)
	
		ggsave("./output/plots/historic_vs_recent_shs.png",
			 plot,
			 width = 10, height = 7, units = "in")


		#### O4 ####
		
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
	
	hind_mean_lats_yr_df <- bind_rows(hind_mean_lats_yr_0.5, hind_mean_lats_yr_0.9) 
	
	
	#### projections ####
	
	# no var
	
	proj_mean_lat_yr_novar <- function(x){
		
		new_dat <- ROMS_projected_dat %>%
			filter(., sp_hab_suit_novar >= x)
		
		new_dat_sum <- new_dat %>%
			group_by(simulation, projection, year) %>%
			summarise(proj_mean_lat = mean(Lat)) 

		new_dat_sum		
	}
	
	sp_hab_thresholds <- c(0.5, 0.9)
	
	proj_mean_lat_yr_novar <- lapply(sp_hab_thresholds, proj_mean_lat_yr_novar)
	
	proj_mean_lats_novar_yr_0.5 <- proj_mean_lat_yr_novar[[1]] %>%
		mutate(sp_hab_threshold = 0.5)

	proj_mean_lats_novar_yr_0.9 <- proj_mean_lat_yr_novar[[2]]	%>%
		mutate(sp_hab_threshold = 0.9)
	
	proj_mean_lats_yr_novar_df <- bind_rows(proj_mean_lats_novar_yr_0.5, proj_mean_lats_novar_yr_0.9) 
	
	proj_mean_lats_yr_novar_df <- tidyr::unite(proj_mean_lats_yr_novar_df,"sim_proj",
																			 simulation, projection, remove = F)

	proj_mean_lats_yr_novar_df_plot <- proj_mean_lats_yr_novar_df %>%
		filter(!str_detect(sim_proj, "_historical"))
	
	hist_data <- proj_mean_lats_yr_novar_df %>%
		filter(str_detect(sim_proj, "_historical"))

	
	colors <- c("#efd966", "#b79a00", 
						  "#7fb27f", "#004700", 
						  "#6666b2", "#000059")

	sim_proj <- unique(proj_mean_lats_yr_novar_df_plot$sim_proj)
	
	names(colors) <- unique(proj_mean_lats_yr_novar_df_plot$sim_proj)
	
	
	mean_latitude_plot_novar <-    
   	ggplot(data = hind_mean_lats_yr_df) +
	 	geom_line(aes(year, hist_mean_lat, alpha = 0.5),
            data = . %>% filter(sp_hab_threshold == 0.5), color = "black") +
		geom_line(data = proj_mean_lats_yr_novar_df_plot, 
							aes(year, proj_mean_lat, color = sim_proj, group = sim_proj, alpha = 0.5)) +
		facet_grid(sp_hab_threshold ~ simulation) +
	  geom_line(aes(year, hist_mean_lat, colour = sp_hab_threshold, alpha = 0.5),
            data = . %>% filter(sp_hab_threshold == 0.9), color = "black") +
		geom_line(data = hist_data, 
							aes(year, proj_mean_lat, color = "lightgrey", alpha = 0.5)) +
		xlab("Year") +
		scale_color_manual(name = "sim_proj", values = colors) +
	  scale_y_continuous(
	  	name = "Meanlatitude\nno var",
	  	breaks = c(56, 58, 60, 62),
	  	labels = c("56˚N", "58˚N", "60˚N", "62˚N")
	  ) +
   	xlim(1970, 2100) +
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
	
	
		ggsave("./output/plots/mean_latitude_novar_plot.png",
			 mean_latitude_plot_novar,
			 width = 10, height = 5, units = "in")
	
	# var
		
		
	proj_mean_lat_yr_var <- function(x){
		
		new_dat <- ROMS_projected_dat %>%
			filter(., sp_hab_suit_var >= x)
		
		new_dat_sum <- new_dat %>%
			group_by(simulation, projection, year) %>%
			summarise(proj_mean_lat = mean(Lat)) 

		new_dat_sum		
	}
	
	sp_hab_thresholds <- c(0.5, 0.9)
	
	proj_mean_lat_yr_var <- lapply(sp_hab_thresholds, proj_mean_lat_yr_var)
	
	proj_mean_lats_var_yr_0.5 <- proj_mean_lat_yr_var[[1]] %>%
		mutate(sp_hab_threshold = 0.5)

	proj_mean_lats_var_yr_0.9 <- proj_mean_lat_yr_var[[2]]	%>%
		mutate(sp_hab_threshold = 0.9)
	
	proj_mean_lats_yr_var_df <- bind_rows(proj_mean_lats_var_yr_0.5, proj_mean_lats_var_yr_0.9) 
	
	proj_mean_lats_yr_var_df <- tidyr::unite(proj_mean_lats_yr_var_df,"sim_proj",
																			 simulation, projection, remove = F)

	proj_mean_lats_yr_var_df_plot <- proj_mean_lats_yr_var_df %>%
		filter(!str_detect(sim_proj, "_historical"))
	
	hist_data <- proj_mean_lats_yr_var_df %>%
		filter(str_detect(sim_proj, "_historical"))

	
	colors <- c("#efd966", "#b79a00", 
						  "#7fb27f", "#004700", 
						  "#6666b2", "#000059")

	sim_proj <- unique(proj_mean_lats_yr_var_df_plot$sim_proj)
	
	names(colors) <- unique(proj_mean_lats_yr_var_df_plot$sim_proj)
	
	
	mean_latitude_plot_var <-    
   	ggplot(data = hind_mean_lats_yr_df) +
	 	geom_line(aes(year, hist_mean_lat, alpha = 0.5),
            data = . %>% filter(sp_hab_threshold == 0.5), color = "black") +
		geom_line(data = proj_mean_lats_yr_var_df_plot, 
							aes(year, proj_mean_lat, color = sim_proj, group = sim_proj, alpha = 0.5)) +
		facet_grid(sp_hab_threshold ~ simulation) +
	  geom_line(aes(year, hist_mean_lat, colour = sp_hab_threshold, alpha = 0.5),
            data = . %>% filter(sp_hab_threshold == 0.9), color = "black") +
		geom_line(data = hist_data, 
							aes(year, proj_mean_lat, color = "lightgrey", alpha = 0.5)) +
		xlab("Year") +
		scale_color_manual(name = "sim_proj", values = colors) +
	  scale_y_continuous(
	  	name = "Meanlatitude\nvar",
	  	breaks = c(56, 58, 60, 62),
	  	labels = c("56˚N", "58˚N", "60˚N", "62˚N")
	  ) +
   	xlim(1970, 2100) +
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
	
	
		ggsave("./output/plots/mean_latitude_var_plot.png",
			 mean_latitude_plot_var,
			 width = 10, height = 5, units = "in")
	
	

	#### month ####

	# hindcasts 
		
	mean_lat_mo_hind <- function(x){
		
		new_dat <- ROMS_hindcast_dat %>%
			filter(., sp_hab_suit >= x)
		
		new_dat_sum <- new_dat %>%
			group_by(year, month_name, month) %>%
			summarise(hist_mean_lat = mean(latitude)) 

		new_dat_sum		
	}
	
	sp_hab_thresholds <- c(0.5, 0.9)
	
	mean_lats_mo_hind <- lapply(sp_hab_thresholds, mean_lat_mo_hind)
	
	mean_lats_mo_hind_0.5 <- mean_lats_mo_hind[[1]] %>%
		mutate(sp_hab_threshold = 0.5)
	
	mean_lats_mo_hind_0.9 <- mean_lats_mo_hind[[2]]	%>%
		mutate(sp_hab_threshold = 0.9)
	
	mean_lats_mo_hind_df <- bind_rows(mean_lats_mo_hind_0.5, mean_lats_mo_hind_0.9)
	
	# reorder for plotting
	
	mean_lats_mo_hind_df$month_name <- factor(mean_lats_mo_hind_df$month_name)
  mean_lats_mo_hind_df$month_name <- fct_reorder(mean_lats_mo_hind_df$month_name, 
  																					mean_lats_mo_hind_df$month)
  
  # projections
  
  # no var
  
	mean_lat_mo_proj_novar <- function(x){
		
		new_dat <- ROMS_projected_dat %>%
			filter(., sp_hab_suit_novar >= x)
		
		new_dat_sum <- new_dat %>%
			group_by(simulation, projection, year, month_name, month) %>%
			summarise(proj_mean_lat = mean(latitude)) 

		new_dat_sum		
	}
	
	sp_hab_thresholds <- c(0.5, 0.9)
	
	mean_lat_mo_proj_novar <- lapply(sp_hab_thresholds, mean_lat_mo_proj_novar)
	
	mean_lat_mo_proj_novar_0.5 <- mean_lat_mo_proj_novar[[1]] %>%
		mutate(sp_hab_threshold = 0.5)
	
	mean_lat_mo_proj_novar_0.9 <- mean_lat_mo_proj_novar[[2]]	%>%
		mutate(sp_hab_threshold = 0.9)
	
	mean_lat_mo_proj_novar_df <- bind_rows(mean_lat_mo_proj_novar_0.5, mean_lat_mo_proj_novar_0.9) 
	
	# reorder for plotting
	
	mean_lat_mo_proj_novar_df$month_name <- factor(mean_lat_mo_proj_novar_df$month_name)
  mean_lat_mo_proj_novar_df$month_name <- fct_reorder(mean_lat_mo_proj_novar_df$month_name, 
  																					mean_lat_mo_proj_novar_df$month)
 
	# plot
  

	mean_lat_mo_proj_novar_df <- tidyr::unite(mean_lat_mo_proj_novar_df,"sim_proj",
																			 simulation, projection, remove = F)

	mean_lat_mo_proj_novar_df_plot <- mean_lat_mo_proj_novar_df %>%
		filter(!str_detect(sim_proj, "_historical"))
	
	hist_data <- mean_lat_mo_proj_novar_df %>%
		filter(str_detect(sim_proj, "_historical"))
	
	colors <- c("#efd966", "#b79a00", 
						  "#7fb27f", "#004700", 
						  "#6666b2", "#000059")

	sim_proj <- unique(mean_lat_mo_proj_novar_df$sim_proj)
	
	names(colors) <- unique(mean_lat_mo_proj_novar_df_plot$sim_proj)
	
	mean_lat_mo_plot_novar <- 
		ggplot(mean_lats_mo_hind_df) +
	 	geom_line(aes(year, hist_mean_lat, alpha = 0.5),
		  data = . %>% filter(sp_hab_threshold == 0.5), color = "black") +
		geom_line(data = mean_lat_mo_proj_novar_df, 
							aes(year, proj_mean_lat, color = sim_proj, group = sim_proj, alpha = 0.5)) +
		facet_grid(month_name ~ simulation + sp_hab_threshold) +
		geom_line(aes(year, hist_mean_lat, alpha = 0.5),
		  data = . %>% filter(sp_hab_threshold == 0.9), color = "black") +
		geom_line(data = hist_data, 
							aes(year, proj_mean_lat, color = "lightgrey", alpha = 0.5)) +
		xlab("Year") +
		scale_color_manual(name = "sim_proj", values = colors) +
	  scale_y_continuous(
	  	name = "Mean latitude\nno var",
	  	breaks = c(56, 58, 60, 62),
	  	labels = c("56˚N", "58˚N", "60˚N", "62˚N"),
	  	limits = c(55, 63)
	  ) +
   	xlim(1970, 2100) +
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
	
  		ggsave("./output/plots/mean_lat_mo_plot_novar.png",
			 mean_lat_mo_plot_novar,
			 width = 15, height = 7, units = "in")
  		
  	# var
  		
  	mean_lat_mo_proj_var <- function(x){
		
		new_dat <- ROMS_projected_dat %>%
			filter(., sp_hab_suit_var >= x)
		
		new_dat_sum <- new_dat %>%
			group_by(simulation, projection, year, month_name, month) %>%
			summarise(proj_mean_lat = mean(latitude)) 

		new_dat_sum		
	}
	
	sp_hab_thresholds <- c(0.5, 0.9)
	
	mean_lat_mo_proj_var <- lapply(sp_hab_thresholds, mean_lat_mo_proj_var)
	
	mean_lat_mo_proj_var_0.5 <- mean_lat_mo_proj_var[[1]] %>%
		mutate(sp_hab_threshold = 0.5)
	
	mean_lat_mo_proj_var_0.9 <- mean_lat_mo_proj_var[[2]]	%>%
		mutate(sp_hab_threshold = 0.9)
	
	mean_lat_mo_proj_var_df <- bind_rows(mean_lat_mo_proj_var_0.5, mean_lat_mo_proj_var_0.9) 
	
	# reorder for plotting
	
	mean_lat_mo_proj_var_df$month_name <- factor(mean_lat_mo_proj_var_df$month_name)
  mean_lat_mo_proj_var_df$month_name <- fct_reorder(mean_lat_mo_proj_var_df$month_name, 
  																					mean_lat_mo_proj_var_df$month)
 
	# plot
  

	mean_lat_mo_proj_var_df <- tidyr::unite(mean_lat_mo_proj_var_df,"sim_proj",
																			 simulation, projection, remove = F)

	mean_lat_mo_proj_var_df_plot <- mean_lat_mo_proj_var_df %>%
		filter(!str_detect(sim_proj, "_historical"))
	
	hist_data <- mean_lat_mo_proj_var_df %>%
		filter(str_detect(sim_proj, "_historical"))
	
	colors <- c("#efd966", "#b79a00", 
						  "#7fb27f", "#004700", 
						  "#6666b2", "#000059")

	sim_proj <- unique(mean_lat_mo_proj_var_df$sim_proj)
	
	names(colors) <- unique(mean_lat_mo_proj_var_df_plot$sim_proj)
	
	mean_lat_mo_plot_var <- 
		ggplot(mean_lats_mo_hind_df) +
	 	geom_line(aes(year, hist_mean_lat, alpha = 0.5),
		  data = . %>% filter(sp_hab_threshold == 0.5), color = "black") +
		geom_line(data = mean_lat_mo_proj_var_df, 
							aes(year, proj_mean_lat, color = sim_proj, group = sim_proj, alpha = 0.5)) +
		facet_grid(month_name ~ simulation + sp_hab_threshold) +
		geom_line(aes(year, hist_mean_lat, alpha = 0.5),
		  data = . %>% filter(sp_hab_threshold == 0.9), color = "black") +
		geom_line(data = hist_data, 
							aes(year, proj_mean_lat, color = "lightgrey", alpha = 0.5)) +
		xlab("Year") +
		scale_color_manual(name = "sim_proj", values = colors) +
	  scale_y_continuous(
	  	name = "Mean latitude\nvar",
	  	breaks = c(56, 58, 60, 62),
	  	labels = c("56˚N", "58˚N", "60˚N", "62˚N"),
	  	limits = c(55, 63)
	  ) +
   	xlim(1970, 2100) +
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
	
  		ggsave("./output/plots/mean_lat_mo_plot_var.png",
			 mean_lat_mo_plot_var,
			 width = 15, height = 7, units = "in")
  		
  		
  		
  	# try fixing labels
  		
  facet_labeller_top <- function(variable, value) {
  c(
    "", 
    "",
    "",
    "",
    "",
    ""
  )
}

facet_labeller_bottom <- function(variable, value) {
  c(
    "0.5", 
    "0.9",
    "0.5",
    "0.9",
    "0.5", 
    "0.9"
  )
}
  		
  mean_lat_mo_plot <- 
		ggplot(mean_lats_mo_hind_df) +
	 	geom_line(aes(year, hist_mean_lat, alpha = 0.5),
		  data = . %>% filter(sp_hab_threshold == 0.5), color = "black") +
		geom_line(data = mean_lats_mo_proj_df_plot, 
							aes(year, proj_mean_lat, color = sim_proj, group = sim_proj, alpha = 0.5)) +
		facet_grid(month_name ~ simulation + sp_hab_threshold,
							 labeller = labeller(
							 	simulation=as_labeller(facet_labeller_top),
                sp_hab_threshold = as_labeller(facet_labeller_bottom))) +
		geom_line(aes(year, hist_mean_lat, alpha = 0.5),
		  data = . %>% filter(sp_hab_threshold == 0.9), color = "black") +
		geom_line(data = hist_data, 
							aes(year, proj_mean_lat, color = "lightgrey", alpha = 0.5)) +
		xlab("Year") +
		scale_color_manual(name = "sim_proj", values = colors) +
	  scale_y_continuous(
	  	name = "Mean\nlatitude",
	  	breaks = c(56, 58, 60, 62),
	  	labels = c("56˚N", "58˚N", "60˚N", "62˚N")
	  ) +
   	xlim(1970, 2100) +
		theme_bw() +
  	theme(legend.position = "none") +
  	theme(
  		plot.title = element_text(size = 18, face = "bold"),
			strip.background = element_blank(),
  		strip.text = element_text(size = 18, face = "bold"),
			axis.text = element_text(size = 16, colour = "grey50"),
  	  axis.ticks = element_line(colour = "grey50"),
  	  axis.line = element_line(colour = "grey50"),
  	  axis.title = element_text(size=18, color = "grey30"),
  	  panel.grid.major = element_blank(),
  	  panel.grid.minor = element_blank(),
  	  panel.border = element_rect(fill = NA, color = "grey50"))
	
  
 mean_lat_mo_plot2 <- 
 	mean_lats_mo_proj_df %>% 
  ggplot(aes(year, proj_mean_lat)) + 
  geom_blank() + 
  facet_grid(~ simulation, scales = "free_x") +
  theme(panel.spacing.x = unit(0,"cm"),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.margin = margin(b = -2),
				strip.background = element_blank(),
  			strip.text = element_text(size = 18, face = "bold"),
				panel.background = element_rect(colour = "white", fill = "white"), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
 
 
 
 plot1 <- mean_lat_mo_plot2 + theme(plot.margin = unit(c(-10, -10, -10, -10), "in"))
 plot2 <- mean_lat_mo_plot + theme(plot.margin = unit(c(0, 0.2, 0.2, 0.2), "in"))
 
	mean_lat_mo_plot_form <- plot1/plot2 + plot_layout(heights = c(0.1,100) ) 
 
 
 
  		ggsave("./output/plots/mean_lat_mo_plot_form.png",
			 mean_lat_mo_plot_form,
			 width = 15, height = 7, units = "in")
  		
  		
  		
  		

	