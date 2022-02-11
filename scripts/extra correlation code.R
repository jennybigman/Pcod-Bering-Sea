# 09 - correlations


	# habitat suitability index
	
	yearly_hab_dat_hind <- ROMS_hindcast_dat %>%
		group_by(year) %>%
    summarise(annual_spawning_hab_suit = mean(sp_hab_suit))
	
	years_proj <- c(2021:2099)
	
	yearly_hab_dat_proj <- ROMS_projected_dat %>% 
		filter(year %in% years_proj) %>%
		group_by(simulation, projection, year) %>%
   	summarise(annual_spawning_hab_suit = mean(sp_hab_suit_var)) 
	
	# create time series for each combo of simulation & scenario
	
	yearly_hab_dat_proj <- tidyr::unite(yearly_hab_dat_proj,"sim_proj",
															 simulation, projection, remove = F)
	
	sim_projs <- unique(yearly_hab_dat_proj$sim_proj)

	time_series_hab_suit <- function(x){
		new_dat <- yearly_hab_dat_proj %>% filter(sim_proj == x)
		new_dat
	}

	dat_list_hab_suit <- lapply(sim_projs, time_series_hab_suit)


	names(dat_list_hab_suit) <- c("cesm_ssp126", "cesm_ssp585",
														    "gfdl_ssp126", "gfdl_ssp585",
														    "miroc_ssp126", "miroc_ssp585")

	
	yearly_hab_dat_hind <- yearly_hab_dat_hind %>%
		mutate(simulation = "hindcast",
					 projection ="hindcast",
					 sim_proj = "hindcast")
	
	add_hist_func <- function(x){
		list_new <- bind_rows(x, yearly_hab_dat_hind)
	}
	
	hab_suit_dat_all <- lapply(dat_list_hab_suit, add_hist_func)


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
	
	# join together 
	
	hind_area_yr <- bind_rows(c_area_hind_dat_sum_yr, p_area_hind_dat_sum_yr)

	
	#### projections ####
	
	# remove historical years for plotting for presentations
	years_proj <- 2021:2099
	
	ROMS_projected_dat_proj <- ROMS_projected_dat %>%
		filter(year %in% years_proj)
	
	# with bias-corrected temperature using variance ratio
	
	c_area_proj_dat <- ROMS_projected_dat_proj %>%
		filter(sp_hab_suit_var >= 0.9) 

	c_area_proj_dat_sum <- c_area_proj_dat %>%
		group_by(simulation, projection, latitude, longitude, year) %>%
		distinct(across(c(latitude, longitude)), .keep_all = TRUE)
	
	c_area_proj_dat_sum_yr <- c_area_proj_dat_sum %>%
		group_by(simulation, projection, year) %>%
		summarize(area = sum(area_km2)) %>% ## avg per cell across a given time period
		mutate(sp_hab_threshold = "core")


	# potential habitat = sum of area where sps >= 0.5
	
	p_area_proj_dat <- ROMS_projected_dat_proj %>%
		filter(sp_hab_suit_var >= 0.5) 

	p_area_proj_dat_sum <- p_area_proj_dat %>%
		group_by(simulation, projection, latitude, longitude, year) %>%
		distinct(across(c(latitude, longitude)), .keep_all = TRUE)
	
	p_area_proj_dat_sum_yr <- p_area_proj_dat_sum %>%
		group_by(simulation, projection, year) %>%
		summarize(area = sum(area_km2)) %>% ## avg per cell across a given time period
		mutate(sp_hab_threshold = "potential")
	
	# join together
	
	proj_area_yr <- bind_rows(c_area_proj_dat_sum_yr, p_area_proj_dat_sum_yr)
	
	# create time series for each combo of simulation & scenario for area

	proj_area_yr <- tidyr::unite(proj_area_yr,"sim_proj",
															 simulation, projection, remove = F)
	
	sim_projs <- unique(proj_area_yr$sim_proj)

	time_series_area <- function(x, y){
		new_dat <- proj_area_yr %>% filter(sim_proj == x, sp_hab_threshold == y)
		new_dat
	}

	cases <- unique(proj_area_yr[ , c("sim_proj", "sp_hab_threshold")])
	
	dat_list_area <- mapply(time_series_area,
													x = cases$sim_proj,
													y = cases$sp_hab_threshold,
													SIMPLIFY = FALSE)
	
	names(dat_list_area) <- c("cesm_ssp126_core", "cesm_ssp585_core",
														"gfdl_ssp126_core", "gfdl_ssp585_core",
														"miroc_ssp126_core", "miroc_ssp585_core",
														"cesm_ssp126_potential", "cesm_ssp585_potential",
														"gfdl_ssp126_potential", "gfdl_ssp585_potential",
														"miroc_ssp126_potential", "miroc_ssp585_potential")

	core_area_dat <- dat_list_area[grep("core", names(dat_list_area))] 
	potential_area_dat <- dat_list_area[grep("potential", names(dat_list_area))] 
	
	hind_area_core <- hind_area_yr %>% filter(sp_hab_threshold == "core") %>%
		mutate(simulation = "hindcast",
					 projection ="hindcast",
					 sim_proj = "hindcast")
	
	add_core_hist_func <- function(x){
		list_new <- bind_rows(x, hind_area_core)
	}
	
	core_area_dat_all <- lapply(core_area_dat, add_core_hist_func)

	hind_area_potential <- hind_area_yr %>% filter(sp_hab_threshold == "potential") %>%
		mutate(simulation = "hindcast",
					 projection ="hindcast",
					 sim_proj = "hindcast")
	
	add_potential_hist_func <- function(x){
		list_new <- bind_rows(x, hind_area_potential)
	}
	
	potential_area_dat_all <- lapply(potential_area_dat, add_potential_hist_func)

	
	### now need to add the hab suit data to the area data by list 
	
	# core lists 
	core_append_func <- function(x){
	
		list_hab <- hab_suit_dat_all[grep(x, names(hab_suit_dat_all))]
		list_area <- core_area_dat_all[grep(x, names(core_area_dat_all))]
		new_df <- bind_cols(list_hab, list_area)
		new_df <- new_df %>%
			mutate(sim_proj = sim_proj...1,
						 simulation = simulation...2,
						 projection = projection...3,
						 year = year...4) %>%
			dplyr::select(-contains("..."))
		new_df
	}
	
	sim_projs <- unique(proj_area_yr$sim_proj)

	core_lists <- lapply(sim_projs, core_append_func)
	
	names(core_lists) <- c("cesm_ssp126_core", "cesm_ssp585_core",
												 "gfdl_ssp126_core", "gfdl_ssp585_core",
												 "miroc_ssp126_core", "miroc_ssp585_core")
	
	# potential lists 
	potential_append_func <- function(x){
	
		list_hab <- hab_suit_dat_all[grep(x, names(hab_suit_dat_all))]
		list_area <- potential_area_dat_all[grep(x, names(potential_area_dat_all))]
		new_df <- bind_cols(list_hab, list_area)
		new_df <- new_df %>%
			mutate(sim_proj = sim_proj...1,
						 simulation = simulation...2,
						 projection = projection...3,
						 year = year...4) %>%
			dplyr::select(-contains("..."))
		new_df
	}
	
	sim_projs <- unique(proj_area_yr$sim_proj)

	potential_lists <- lapply(sim_projs, potential_append_func)
	
	names(potential_lists) <- c("cesm_ssp126_potential", "cesm_ssp585_potential",
												 "gfdl_ssp126_potential", "gfdl_ssp585_potential",
												 "miroc_ssp126_potential", "miroc_ssp585_potential")
	
	## correlation plots
	
	library("ggpubr")
	
		plot_func <- function(x,y){

		plot <- ggscatter(x, x = "area", y = "annual_spawning_hab_suit", 
  	        add = "reg.line", conf.int = TRUE, 
  	        cor.coef = TRUE, cor.method = "pearson",
						title = y,
  	        xlab = "area", ylab = "index of spawning\nhabitat suitability") +
					  theme(
					  	axis.text = element_text(size = 10),
					  	axis.title = element_text(size = 12)
					  )
		plot
	
	}

	all_lists <- c(core_lists, potential_lists)
	plot_names <- names(all_lists)

	plot_lists <- mapply(plot_func,
											 x = all_lists,
											 y = plot_names,
											 SIMPLIFY = FALSE)

	plot_name_func <- function(x){
  	paste0(x, "_area_habsuit_corr_plot")
  }
   
  plot_names <- sapply(names(all_lists), plot_name_func)
  
	file_path_name <- function(x){
  	paste0("/Users/jenniferbigman/My Drive/NOAA AFSC Postdoc/Pcod Bering Sea Habitat Suitability/Pcod-Bering-Sea/output/plots/correlation plots/", x)
  }
   
  full_names <- sapply(plot_names, file_path_name)
	
  ggsave_func2 <- function(x,y){
  	ggsave(plot = x,
    file = paste(y,".png",sep=""),
    width = 7, height = 7, units = "in")
  }
  			
	plot_list <- mapply(ggsave_func2, x = plot_lists, y = full_names)

	
	#### correlation with recruitment data ####
	
	# read in data 
	
	recruitment_dat <- read_csv(here("./data/SAFE_Pcod_2020_Recruitment.csv")) %>% 
		rename(year = Year) %>%
		na.omit() %>%
		mutate(across(Recruitment_lastyr:Recruitment_AB, ~ .x * 1000))
	
	recruitment_dat_long <- recruitment_dat %>% gather(key = "group", recruitment, Recruitment_lastyr:Recruitment_AB)
	
	abundance_dat <- read_csv(here("./data/SAFE_Pcod_2020_Abundance_AgeX.csv")) %>% 
		rename(year = Year) %>%
		na.omit()
	
	abundance_dat <- abundance_dat %>%
		mutate(across(Number_Age0_1912:Numbers_Age4_A, ~ .x * 1000))
	
	abundance_dat_long <- abundance_dat %>% gather(key = "group", abundance, Number_Age0_1912:Numbers_Age4_A) %>% 
		separate(group, c("number", "age", "model")) %>%
		dplyr::select(-number) 
	
	# prelim plots
	recruitment_plot <- ggplot(recruitment_dat_long) +
		geom_point(aes(x = year, y = log(recruitment), color = group)) +
		white_theme()
	
	abundance_plot_func <- function(x){
	
			plot <- ggplot(abundance_dat_long) +
							geom_point(aes(x = year, y = log(abundance), color = model),
								data = . %>% filter(age == x)) +
						  ggtitle(x) +
							white_theme()
			
			plot
	}
	
	ages <- unique(abundance_dat_long$age)
	
	plot_list <- lapply(ages, abundance_plot_func)
	
	## all model predictions for both recruitment and abundance look similar so just do a correlation with one
	
	## add recruitment and abundance data to lists
	list1 <- hab_suit_dat_all[[1]]
	
	recruit_append_func <- function(x){
		new_df <- merge(list1, recruitment_dat, by = "year")
		new_df
	}
	
	recruit_dats <- lapply(dat_list_hab_suit, recruit_append_func)
	df1 <- recruit_dats[[1]]
	
	recruitment_corr_plot_func <- function(x, y){

		plot <- ggscatter(new_df, x = "annual_spawning_hab_suit", y = "recruitment", combine = TRUE, 
  	        add = "reg.line", conf.int = TRUE, 
  	        cor.coef = TRUE, cor.method = "pearson",
						#title = y,
  	        xlab  = "index of spawning\nhabitat suitability", ylab = "log(recruitment)") +
					  theme(
					  	axis.text = element_text(size = 10),
					  	axis.title = element_text(size = 12)
					  )
		plot
	
	}

	plot_names <- names(recruit_dats)

	recruitment_corr_plot_lists <- mapply(recruitment_corr_plot_func,
																			  x = recruit_dats,
																			  y = plot_names,
																			  SIMPLIFY = FALSE)

	plot_name_func <- function(x){
  	paste0(x, "_recruitment_corr_plot")
  }
   
  plot_names <- sapply(names(recruit_dats), plot_name_func)
  
	file_path_name <- function(x){
  	paste0("/Users/jenniferbigman/My Drive/NOAA AFSC Postdoc/Pcod Bering Sea Habitat Suitability/Pcod-Bering-Sea/output/plots/correlation plots/", x)
  }
   
  full_names <- sapply(plot_names, file_path_name)
  			
	plot_list <- mapply(ggsave_func2, x = recruitment_corr_plot_lists, y = full_names)

		plot_func <- function(x,y){

		plot <- ggscatter(x, x = "area", y = "annual_spawning_hab_suit", 
  	        add = "reg.line", conf.int = TRUE, 
  	        cor.coef = TRUE, cor.method = "pearson",
						title = y,
  	        xlab = "area", ylab = "index of spawning\nhabitat suitability") +
					  theme(
					  	axis.text = element_text(size = 10),
					  	axis.title = element_text(size = 12)
					  )
		plot
	
	}

	all_lists <- c(core_lists, potential_lists)
	plot_names <- names(all_lists)

	plot_lists <- mapply(plot_func,
											 x = all_lists,
											 y = plot_names,
											 SIMPLIFY = FALSE)

	plot_name_func <- function(x){
  	paste0(x, "_area_habsuit_corr_plot")
  }
   
  plot_names <- sapply(names(all_lists), plot_name_func)
  
	file_path_name <- function(x){
  	paste0("/Users/jenniferbigman/My Drive/NOAA AFSC Postdoc/Pcod Bering Sea Habitat Suitability/Pcod-Bering-Sea/output/plots/correlation plots/", x)
  }
   
  full_names <- sapply(plot_names, file_path_name)
	
  ggsave_func2 <- function(x,y){
  	ggsave(plot = x,
    file = paste(y,".png",sep=""),
    width = 7, height = 7, units = "in")
  }
  			
	plot_list <- mapply(ggsave_func2, x = plot_lists, y = full_names)

	
	
		
	
			  
	ggsave("./output/plots/correlation plots/recruitment_habsuit_plot.png",
			 recruitment_habsuit_plot,
			 width = 5, height = 5, units = "in")


	# abundance
	
	# age 0
	abun_habsuit_plot_age0 <- 
						ggscatter(abun_habsuit, x = "annual_spawning_hab_suit", y = "Numbers_Age0_A", 
  	        add = "reg.line", conf.int = TRUE, 
  	        cor.coef = TRUE, cor.method = "pearson",
						title = "Age 0",
  	        xlab = "index of spawning\nhabitat suitability", ylab = "log(abundance)") +
						white_theme()
					  
	ggsave("./output/plots/correlation plots/abun_habsuit_plot_age0.png",
			 abun_habsuit_plot_age0,
			 width = 5, height = 5, units = "in")
	
	# age 1
	abun_habsuit_plot_age1 <- 
						ggscatter(abun_habsuit, x = "annual_spawning_hab_suit", y = "Numbers_Age1_A", 
  	        add = "reg.line", conf.int = TRUE, 
  	        cor.coef = TRUE, cor.method = "pearson",
						title = "Age 1",
  	        xlab = "index of spawning\nhabitat suitability", ylab = "log(abundance)") +
						white_theme()
					  
	ggsave("./output/plots/correlation plots/abun_habsuit_plot_age1.png",
			 abun_habsuit_plot_age1,
			 width = 5, height = 5, units = "in")


	# age 2
	abun_habsuit_plot_age2 <- 
						ggscatter(abun_habsuit, x = "annual_spawning_hab_suit", y = "Numbers_Age2_A", 
  	        add = "reg.line", conf.int = TRUE, 
  	        cor.coef = TRUE, cor.method = "pearson",
						title = "Age 2",
  	        xlab = "index of spawning\nhabitat suitability", ylab = "log(abundance)") +
						white_theme()
					  
	ggsave("./output/plots/correlation plots/abun_habsuit_plot_age2.png",
			 abun_habsuit_plot_age2,
			 width = 5, height = 5, units = "in")


	# age3
	abun_habsuit_plot_age3 <- 
						ggscatter(abun_habsuit, x = "annual_spawning_hab_suit", y = "Numbers_Age3_A", 
  	        add = "reg.line", conf.int = TRUE, 
  	        cor.coef = TRUE, cor.method = "pearson",
						title = "Age 3",
  	        xlab = "index of spawning\nhabitat suitability", ylab = "log(abundance)") +
						white_theme()
					  
	ggsave("./output/plots/correlation plots/abun_habsuit_plot_age3.png",
			 abun_habsuit_plot_age3,
			 width = 5, height = 5, units = "in")



	#age4
	abun_habsuit_plot_age4 <- 
						ggscatter(abun_habsuit, x = "annual_spawning_hab_suit", y = "Numbers_Age4_A", 
  	        add = "reg.line", conf.int = TRUE, 
  	        cor.coef = TRUE, cor.method = "pearson",
						title = "Age 4",
  	        xlab = "index of spawning\nhabitat suitability", ylab = "log(abundance)") +
						white_theme()
					  
	ggsave("./output/plots/correlation plots/abun_habsuit_plot_age4.png",
			 abun_habsuit_plot_age4,
			 width = 5, height = 5, units = "in")

