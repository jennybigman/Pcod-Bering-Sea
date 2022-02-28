# 09 - correlations

	# habitat suitability index
  	
	yearly_hab_dat_hind <- ROMS_hindcast_dat %>%
		group_by(year) %>%
    summarise(annual_hatch_success_cauchy = mean(hatch_success_cauchy),
   					  annual_hatch_success_gaussian = mean(hatch_success_gaus),
    					annual_spawning_hab_suit = mean(sp_hab_suit))
	
	yearly_hab_dat_proj <- ROMS_projected_dat %>% 
		group_by(simulation, projection, year) %>%
   	summarise(annual_hatch_success_cauchy_var = mean(hatch_success_cauchy_var),
   						annual_hatch_success_gaussian_var = mean(hatch_success_gaus_var),
   						annual_spawning_hab_suit_var = mean(sp_hab_suit_var)) 

	#### correlation between habitat suitability index and area ####

	# area 	#### by year #### each grid cell counted only once
	
	#### hindcasts ####
	
	c_area_hind_dat <- ROMS_hindcast_dat %>%
		filter(sp_hab_suit >= 0.9) 
	
	c_area_hind_dat_sum <- c_area_hind_dat %>%
		group_by(latitude, longitude, year) %>%
		distinct(across(c(latitude, longitude)), .keep_all = TRUE)

	c_area_hind_dat_sum_yr <- c_area_hind_dat_sum %>%
		group_by(year) %>%
		summarize(area = sum(area_km2)) %>% 
		mutate(sp_hab_threshold = "core")

	# potential habitat = sum of area where sps >= 0.5
	
	p_area_hind_dat <- ROMS_hindcast_dat %>%
		filter(sp_hab_suit >= 0.5) 
	
	p_area_hind_dat_sum <- p_area_hind_dat %>%
		group_by(latitude, longitude, year) %>%
		distinct(across(c(latitude, longitude)), .keep_all = TRUE)

	p_area_hind_dat_sum_yr <- p_area_hind_dat_sum %>%
		group_by(year) %>%
		summarize(area = sum(area_km2)) %>% ## avg per cell across a given time period
		mutate(sp_hab_threshold = "potential")
	
	# join area with habitat data 

	core_area_habsuit <- merge(c_area_hind_dat_sum_yr, yearly_hab_dat_hind)
	potential_area_habsuit <- merge(p_area_hind_dat_sum_yr, yearly_hab_dat_hind)

	## correlation plots
	
	core_area_habsuit_plot <- 
						ggscatter(core_area_habsuit, x = "area", y = "annual_spawning_hab_suit", 
  	        add = "reg.line", conf.int = TRUE, 
  	        cor.coef = TRUE, cor.method = "pearson",
  	        xlab = "core area", ylab = "index of spawning\nhabitat suitability") +
						white_theme()
					  
	ggsave("./output/plots/correlation plots/core_area_habsuit.png",
			 core_area_habsuit_plot,
			 width = 5, height = 5, units = "in")
	
	potential_area_habsuit_plot <- 
						ggscatter(potential_area_habsuit, x = "area", y = "annual_spawning_hab_suit", 
  	        add = "reg.line", conf.int = TRUE, 
  	        cor.coef = TRUE, cor.method = "pearson",
  	        xlab = "potential area", ylab = "index of spawning\nhabitat suitability") +
						white_theme()
					  
	ggsave("./output/plots/correlation plots/potential_area_habsuit.png",
			 potential_area_habsuit_plot,
			 width = 5, height = 5, units = "in")
	
	
	#### correlation with recruitment data ####
	
	# read in data 
	
	recruitment_dat <- read_csv(here("./data/SAFE_Pcod_EBS_2021_Recruits.csv")) %>% 
		na.omit() %>%
		mutate(raw_recruits = recruits * 1000,
					 log_raw_recruits = log(raw_recruits))
	
	abundance_dat <- read_csv(here("./data/SAFE_Pcod_EBS_2021_Abundance.csv")) %>% 
		na.omit()
	
	abundance_dat_long <- abundance_dat %>% gather(key = "age", abundance, "0":"4") %>% 
		mutate(raw_abundance = abundance * 1000,
					 log_raw_abundance = log(raw_abundance)) %>%
		filter(age != 0)

	# prelim plots
	recruitment_plot <- ggplot(recruitment_dat) +
		geom_point(aes(x = year, y = log_raw_recruits, color = model)) +
		white_theme()
	
	abundance_plot <- ggplot(abundance_dat_long) +
		geom_point(aes(x = year, y = log_raw_abundance, color = age, shape = model)) +
		white_theme()

	
	abundance_plot_func <- function(x){
	
		plot <- ggplot(abundance_dat_long) +
						geom_point(aes(x = year, y = log_raw_abundance, color = model),
								data = . %>% filter(age == x)) +
						ggtitle(paste("age", x)) +
						white_theme()
			
		plot
		
	}
	
	ages <- unique(abundance_dat_long$age)
	
	plot_list <- lapply(ages, abundance_plot_func)
	
	# add data together
	
	years_recruit <- (min(recruitment_dat$year):max(recruitment_dat$year))
	hab_suit_recruit <- yearly_hab_dat_hind %>% filter(year %in% years_recruit)
	recruit_habsuit <- merge(hab_suit_recruit, recruitment_dat, by = "year")
	
	years_abund <- (min(abundance_dat_long$year):max(abundance_dat_long$year))
	hab_suit_abund <- yearly_hab_dat_hind %>% filter(year %in% years_abund)
	abund_habsuit <- merge(hab_suit_abund, abundance_dat_long, by = "year") 
	
	
	# recruitment plots ####
	
	recruitment_habsuit_plot_func <- function(x){
		
		new_dat <- recruit_habsuit %>% filter(model == x)
		
		plot <- ggscatter(new_dat, x = "annual_spawning_hab_suit", y = "log_raw_recruits", 
  	        add = "reg.line", conf.int = TRUE, 
  	        cor.coef = TRUE, cor.method = "pearson",
						title = x,
  	        xlab = "index of spawning\nhabitat suitability", ylab = "log(# recruits)") +
						white_theme()
		
		plot
	}
	
	# apply function
	mods <- unique(recruit_habsuit$model)
	recruit_plot_list <- lapply(mods, recruitment_habsuit_plot_func)
	
	# name plots
	plot_name_func <- function(x){
  	paste0(x, "_recruitment_habsuit_corr_plot")
	}
	
  plot_names <- sapply(mods, plot_name_func)
  
  # set file paths
	file_path_name <- function(x){
  	paste0("/Users/jenniferbigman/My Drive/NOAA AFSC Postdoc/Pcod Bering Sea Habitat Suitability/Pcod-Bering-Sea/output/plots/correlation plots/", x)
  }
   
  full_names <- sapply(plot_names, file_path_name)
	
  # save plots to file path
  ggsave_func2 <- function(x,y){
  	ggsave(plot = x,
    file = paste(y,".png",sep=""),
    width = 7, height = 7, units = "in")
  }
  			
	mapply(ggsave_func2, x = recruit_plot_list, y = full_names)

	
	# abundance plots ####
	
	abund_habsuit <- abund_habsuit %>%
		tidyr::unite("age_mod", age, model, remove = F)
	
	abundance_habsuit_plot_func <- function(x){
		
		new_dat <- abund_habsuit %>% dplyr::filter(age_mod == x)
		
		plot <- ggscatter(new_dat, x = "annual_spawning_hab_suit", y = "log_raw_abundance", 
  	        add = "reg.line", conf.int = TRUE, 
  	        cor.coef = TRUE, cor.method = "pearson",
						title = paste("age", x),
  	        xlab = "index of spawning\nhabitat suitability", ylab = "log(abundance)") +
						white_theme()
		
		plot
	}
	
	# apply function
	age_mods <- unique(abund_habsuit$age_mod)
	abund_plot_list <- lapply(age_mods, abundance_habsuit_plot_func)
	

	# name plots
	plot_name_func <- function(x){
  	paste0(x, "_abundance_habsuit_corr_plot")
	}
  plot_names <- sapply(age_mods, plot_name_func)
  
  # set file paths
	file_path_name <- function(x){
  	paste0("/Users/jenniferbigman/My Drive/NOAA AFSC Postdoc/Pcod Bering Sea Habitat Suitability/Pcod-Bering-Sea/output/plots/correlation plots/", x)
  }
   
  full_names <- sapply(plot_names, file_path_name)
	
  # save plots to file path
  ggsave_func2 <- function(x,y){
  	ggsave(plot = x,
    file = paste(y,".png",sep=""),
    width = 7, height = 7, units = "in")
  }
  			
	mapply(ggsave_func2, x = abund_plot_list, y = full_names)

	
	#### lag by 1-yr ####
		
	abundance_dat <- read_csv(here("./data/SAFE_Pcod_EBS_2021_Abundance.csv")) %>% 
		na.omit() 
	
	col_names <- c("year", "age0", "age1", "age2", "age3", "age4", "model")
	
	abundance_dat2 <- abundance_dat
	
	names(abundance_dat2) <- col_names
	
	lag_function <- function(x){

		new_dat <- abundance_dat2 %>% filter(model == x)
		
		new_dat_rep <- new_dat %>%
				naniar::replace_with_na(replace = 
																	list(age1 = c(new_dat[1,3]),
																			 age2 = c(new_dat[1:2, 4]),
																			 age3 = c(new_dat[1:3, 5]),
																			 age4 = c(new_dat[1:4, 6])))
		
		return(new_dat_rep)
	}
	
	models <- unique(abundance_dat2$model)

	lag_dfs <- lapply(models, lag_function)
	
	abundance_dat_lag <- bind_rows(lag_dfs)
	
	# convert to long format
	abundance_dat_lag_long <- abundance_dat_lag %>% gather(key = "age", abundance, age0:age4) %>% 
		mutate(raw_abundance = abundance * 1000,
					 log_raw_abundance = log(raw_abundance)) %>%
		filter(age != 0)

	years_abund_lag <- (min(abundance_dat_lag_long$year):max(abundance_dat_lag_long$year))
	hab_suit_abund_lag <- yearly_hab_dat_hind %>% filter(year %in% years_abund_lag)
	abund_habsuit_lag <- merge(hab_suit_abund_lag, abundance_dat_lag_long, by = "year") # this step is wrong

	### plots ####
	

	abund_habsuit_lag <- abund_habsuit_lag %>%
		tidyr::unite("age_mod", age, model, remove = F)
	
	abundance_habsuit_lag_plot_func <- function(x){
		
		new_dat <- abund_habsuit_lag %>% dplyr::filter(age_mod == x)
		
		plot <- ggscatter(new_dat, x = "annual_spawning_hab_suit", y = "log_raw_abundance", 
  	        add = "reg.line", conf.int = TRUE, 
  	        cor.coef = TRUE, cor.method = "pearson",
						title = paste("age", x),
  	        xlab = "index of spawning\nhabitat suitability", ylab = "log(abundance)") +
						white_theme()
		
		plot
	}
	
	# apply function
	age_mods <- unique(abund_habsuit_lag$age_mod)
	abund_lag_plot_list <- lapply(age_mods, abundance_habsuit_lag_plot_func)
	

	# name plots
	plot_name_func <- function(x){
  	paste0(x, "_abundance_habsuit_lag_corr_plot")
	}
  plot_names <- sapply(age_mods, plot_name_func)
  
  # set file paths
	file_path_name <- function(x){
  	paste0("/Users/jenniferbigman/My Drive/NOAA AFSC Postdoc/Pcod Bering Sea Habitat Suitability/Pcod-Bering-Sea/output/plots/correlation plots/", x)
  }
   
  full_names <- sapply(plot_names, file_path_name)
	
  # save plots to file path
  ggsave_func2 <- function(x,y){
  	ggsave(plot = x,
    file = paste(y,".png",sep=""),
    width = 7, height = 7, units = "in")
  }
  			
	mapply(ggsave_func2, x = abund_lag_plot_list, y = full_names)
