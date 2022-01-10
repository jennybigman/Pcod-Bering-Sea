# rolling mean and sd (following Litzow et al 2018)

	library(zoo)
	library(runner)
	
	#### spawning habitat suitability ####
	
	# year ####
	
	# summarize spawning habitat suitability by year 
	yr_stats_hind <- ROMS_hindcast_dat %>%
		group_by(year) %>%
		summarise(mean_sp_hab_suit = mean(sp_hab_suit),
							sd_sp_hab_suit = sd(sp_hab_suit),
							CV = sd_sp_hab_suit/mean_sp_hab_suit)
	
	yr_stats_proj <- ROMS_projected_dat %>%
		group_by(simulation, projection, year) %>%
		summarise(mean_sp_hab_suit = mean(sp_hab_suit),
							sd_sp_hab_suit = sd(sp_hab_suit),
							CV = sd_sp_hab_suit/mean_sp_hab_suit)
	
	
	# calculate rolling mean of mean and sd
	
	# hindcast ####
	
  sds_hind <- NA
  
  for(i in 6:51){
  	win <- (i - 5):(i + 5)
  	sds_hind[i] <- sd(yr_stats_hind$mean_sp_hab_suit[win])
  }
  
  means_hind <- NA
  
  for(i in 6:51){
  	win <- (i - 5):(i + 5)
  	means_hind[i] <- mean(yr_stats_hind$mean_sp_hab_suit[win])
  }
  
	years_hind <- c(1970:2020)
  
	rolling_stats_hind <- as.data.frame(cbind(years_hind, means_hind, sds_hind))  %>%
  	na.omit()
  
  
  # projections ####
  
  sds_proj <- NA
  
  for(i in 6:615){
  	win <- (i - 5):(i + 5)
  	sds_proj[i] <- sd(yr_stats_proj$mean_sp_hab_suit[win])
  }
  
  means_proj <- NA
  
  for(i in 6:615){
  	win <- (i - 5):(i + 5)
  	means_proj[i] <- mean(yr_stats_proj$mean_sp_hab_suit[win])
  }
  
 	years_proj_hist <- (c(1980:2014))
	years_proj_scenario <- c(rep(c(2015:2099), 2))
	years_proj <- c(years_proj_hist, years_proj_scenario)
	years_proj_all <- rep(years_proj, 3)
	
	
	sims <- c("cesm", "gfdl", "miroc")
	simulations <- list()
	
	for(i in sims){
		simulations[[i]] <- rep(i, times = 205)
		simulations
	}
	
	simulation <- as.data.frame(bind_rows(simulations)) %>%
		gather(., key = simulation)
	
 
	scens <- c("historical", "ssp126", "ssp585")
	scenarios <- list()
	
	for(i in scens){
		scenarios[[i]] <- rep(i, times = c(35, 85, 85))
		scenarios
	}
	
	simulation <- as.data.frame(bind_rows(simulations)) %>%
		gather(., key = simulation)
	
	
	scenarios <- list()
	
	scen_name_func <- function(x){
		projs <- rep(x, 85)
		
	}
	
	list_scen <- lapply(scens, scen_name_func)
	names(list_scen) <- scens
	scenario <- as.data.frame(bind_rows(list_scen))
	scenario2 <- gather(scenario, key = scenario) 
	scenario <- scenario2 %>% dplyr::select(scenario)
	
	scenario2 <- as.data.frame(rep("historical", 35))
	
	scenario3 <- bind_rows(scenario, scenario2)

  rolling_stats_proj <- as.data.frame(cbind(sds_proj, means_proj, years_proj, simulation))

  
  #### plots ####
  
  rolling_stats_proj <- tidyr::unite(rolling_stats_proj,"sim_proj",
													simulation, projection, remove = F)

	proj_mean_lats_yr_df_plot <- proj_mean_lats_yr_df %>%
		filter(!str_detect(sim_proj, "_historical"))
	
	hist_data <- proj_mean_lats_yr_df %>%
		filter(str_detect(sim_proj, "_historical"))

	
	colors <- c("#efd966", "#b79a00", 
						  "#7fb27f", "#004700", 
						  "#6666b2", "#000059")

	sim_proj <- unique(proj_mean_lats_yr_df_plot$sim_proj)
	
	names(colors) <- unique(proj_mean_lats_yr_df_plot$sim_proj)
	
	
	mean_latitude_plot <-    
   	ggplot(data = hind_mean_lats_yr_df) +
	 	geom_line(aes(year, hist_mean_lat, alpha = 0.5),
            data = . %>% filter(sp_hab_threshold == 0.5), color = "black") +
		geom_line(data = proj_mean_lats_yr_df_plot, 
							aes(year, proj_mean_lat, color = sim_proj, group = sim_proj, alpha = 0.5)) +
		facet_grid(sp_hab_threshold ~ simulation) +
	  geom_line(aes(year, hist_mean_lat, colour = sp_hab_threshold, alpha = 0.5),
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
			strip.background = element_blank(),
  		strip.text = element_text(size = 18, face = "bold"),
			axis.text = element_text(size = 16, colour = "grey50"),
  	  axis.ticks = element_line(colour = "grey50"),
  	  axis.line = element_line(colour = "grey50"),
  	  axis.title = element_text(size=18, color = "grey30"),
  	  panel.grid.major = element_blank(),
  	  panel.grid.minor = element_blank(),
  	  panel.border = element_rect(fill = NA, color = "grey50"))
	
	
		ggsave("./output/plots/mean_latitude_plot.png",
			 mean_latitude_plot,
			 width = 10, height = 5, units = "in")
	

  
  
  
  
  
  
  
  
  
  
  
  

	# plots
	
	# mean 
	rolling_mean_plot <- 
		ggplot(rolling_stats) +
		geom_line(aes(x = years, y = means_noNA)) +
		xlab("Year") +
		ylab("11-year rolling mean") +
		pos1r1_theme()

  # sd
	rolling_sd_plot <- 
		ggplot(rolling_stats) +
		geom_line(aes(x = years, y = sds_noNA)) +
		xlab("Year") +
		ylab("11-year rolling\nstandard deviation") +
		pos1r2_theme()
	
	rolling_stats_plot <- rolling_mean_plot/rolling_sd_plot
		
	ggsave("./output/plots/rolling_stats_plot.png",
		rolling_stats_plot,
		width = 15, height = 10, units = "in")
	

	## month ####
  
	# summarize spawning habitat suitability by year 
  mo_stats <- ROMS_hindcast_dat %>%
	group_by(year, month) %>%
	summarise(mean_sp_hab_suit = mean(sp_hab_suit),
						sd_sp_hab_suit = sd(sp_hab_suit)) %>%
  filter(., year != 2021)

	# add month names
  mo_stats$month_name <- NA
  mo_stats$month_name[mo_stats$month == 1] <- "January"
  mo_stats$month_name[mo_stats$month == 2] <- "February"
	mo_stats$month_name[mo_stats$month == 3] <- "March"
	mo_stats$month_name[mo_stats$month == 4] <- "April"
	
	
	# calculate rolling mean of mean over 11-yr window and turn into df
	roll_mean_func <- function(x){
 	
		new_dat <- mo_stats %>% filter(month_name == x)
  	roll_mean_mo <- rollmean(new_dat$mean_sp_hab_suit, 11, fill = NA) 
  	roll_mean_mo
  
	}
 
	month_names <- as.character(unique(mo_stats$month_name))
	month <- 1:4
 
	mo_mean_dfs <- lapply(month_names, roll_mean_func) %>% set_names(month_names)

	mo_means <- bind_rows(mo_mean_dfs) %>%
 		gather(key = "month_name") %>%
 		mutate(year = rep(1970:2020, 4)) %>%
 		rename(roll_mean = value) %>%
		na.omit()
	
	# add month names
  mo_means$month <- NA
  mo_means$month[mo_means$month_name == "January"] <- 1
  mo_means$month[mo_means$month_name == "February"] <- 2
	mo_means$month[mo_means$month_name == "March"] <- 3
	mo_means$month[mo_means$month_name == "April"] <- 4


	# calculate rolling mean of sd over 11-yr window and turn into df
	roll_sd_func <- function(x){
 	
		new_dat <- mo_stats %>% filter(month_name == x)
		
		sds_mo <- NA
  
  	for(i in 6:47){
  	win <- (i - 5):(i + 5)
  	sds_mo[i] <- sd(new_dat$mean_sp_hab_suit[win])
  	sds_mo[i]
  	}
		sds_mo
		}
 
	mo_sd_lists <- lapply(month_names, roll_sd_func) %>% 
		set_names(month_names) 

	mo_sds <- bind_rows(mo_sd_lists) %>%
		gather(key = "month_name") %>%
		na.omit() %>%
		mutate(year = rep(1975:2015, 4)) %>%
		rename(roll_sd = value)
	
	# add month names
  mo_sds$month <- NA
  mo_sds$month[mo_sds$month_name == "January"] <- 1
  mo_sds$month[mo_sds$month_name == "February"] <- 2
	mo_sds$month[mo_sds$month_name == "March"] <- 3
	mo_sds$month[mo_sds$month_name == "April"] <- 4

 
	# combine both into one df
	mo_stats <- merge(mo_means, mo_sds, by = c("month", "month_name", "year")) %>%
 		na.omit()

	# reorder for plotting
	mo_stats$month_name <- factor(mo_stats$month_name)
  mo_stats$month_name <- fct_reorder(mo_stats$month_name, 
  																	 mo_stats$month)

 # plots 
 
  # mean
  
  #### fix order ####
  
  rolling_mean_plot_mo <- 
		ggplot(mo_stats) +
		geom_line(aes(x = year, y = roll_mean)) +
  	facet_wrap(~ month_name, ncol = 2, nrow = 2) +
		xlab("Year") +
		ylab("11-year rolling mean") +
  	facet_theme()
    	  
  ggsave("./output/plots/rolling_mean_plot_mo.png",
		rolling_mean_plot_mo,
		width = 6, height = 6, units = "in")

	# sd
	rolling_sd_plot_mo <- 
		ggplot(mo_stats) +
		geom_line(aes(x = year, y = roll_sd)) +
  	facet_wrap(~ month_name, ncol = 2, nrow = 2) +
		xlab("Year") +
		ylab("11-year rolling sd") +
	  facet_theme()

  ggsave("./output/plots/rolling_sd_plot_mo.png",
		rolling_sd_plot_mo,
		width = 6, height = 6, units = "in")

  
	# other window lengths ####
  
  # year ####
	
	# calculate rolling mean of mean 
  roll_mean_yr_func <- function(x) {
  		roll_yr_mean <- rollmean(yr_stats$mean_sp_hab_suit, x, fill = NA)
			roll_yr_mean
  }
  
  window_widths <- c(3, 5, 7, 9, 11, 13, 15)
  
  roll_means <- sapply(window_widths, roll_mean_yr_func) %>%
  	as.data.frame() %>%
  	set_names(window_widths) %>%
  	gather("window_width") %>%
  	rename(mean = value) %>%
  	mutate(year = rep((1970:2020), 7)) %>%
  	na.omit()
  
  # plot
  roll_means$window_width <- as.numeric(roll_means$window_width)
  
  mean_vary_win <- ggplot(roll_means) +
  	geom_line(aes(x = year, y = mean)) +
  	facet_wrap(~ window_width, nrow = 4) +
  	facet_theme()
  
  ggsave("./output/plots/mean_vary_win.png",
		mean_vary_win,
		width = 6, height = 6, units = "in")

  
  # calculate rolling mean of sd 
  roll_sd_yr_func <- function(x) {
  			
  	sds<- NA
  
  	for(i in 6:47){
  	win <- (i - 5):(i + x)
  	sds[i] <- sd(yr_stats$mean_sp_hab_suit[win])
  	}
  
		sds
  }
  
  nums <- c(-3, -1, 1, 3, 5, 7, 9)
  
  roll_sds <- sapply(nums, roll_sd_yr_func) %>%
  	as.data.frame() %>%
  	set_names(window_widths) %>%
  	gather("window_widths") %>%
  	rename(sd = value) %>%
  	mutate(year = rep((1970:2016), 7)) %>%
  	na.omit()
  
  #roll_sds_test_11 <- na.omit(roll_sds$'11')
  
  #roll_test <- cbind(roll_sds_test_11, rolling_stats$sds_noNA)
  
  # plot
  roll_sds$window_width <- as.numeric(roll_sds$window_width)
  
  sd_vary_win <- ggplot(roll_sds) +
  	geom_line(aes(x = year, y = sd)) +
  	facet_wrap(~ window_width, nrow = 4) +
  	facet_theme()

  ggsave("./output/plots/sd_vary_win.png",
		sd_vary_win,
		width = 6, height = 6, units = "in")


 ##### rolling mean and sd of latitude #####
  
	# year ####
	
	yr_stats <- ROMS_hindcast_dat %>%
		group_by(year) %>%
		filter(year != 2021) %>%
		summarise(mean_sp_hab_suit = mean(sp_hab_suit),
							sd_sp_hab_suit = sd(sp_hab_suit),
							CV = sd_sp_hab_suit/mean_sp_hab_suit)
	
	
	# calculate mean latitude of spawning habitat for each threshold for year year 
	mean_lat_yr <- function(x){
		
		new_dat <- ROMS_hindcast_dat %>%
			filter(., sp_hab_suit >= x)
		
		new_dat_sum <- new_dat %>%
			group_by(year) %>%
			summarise(mean_lat = mean(latitude)) 

		new_dat_sum		
	}
	
	sp_hab_thresholds <- c(0, 0.5, 0.9)
	
	mean_lats_yr <- lapply(sp_hab_thresholds, mean_lat_yr) %>% 
		bind_cols %>%
		rename(year = "year...1",
					 mean_lat_all = "mean_lat...2",
					 mean_lat_0.5 = "mean_lat...4",
					 mean_lat_0.9 = "mean_lat...6") %>%
		dplyr::select( - year...3, - year...5) %>%
		filter(year != 2021)
	
 # month ####
	
	mean_lat_mo <- function(x){
		
		new_dat <- ROMS_hindcast_dat %>%
			filter(., sp_hab_suit >= x)
		
		new_dat_sum <- new_dat %>%
			group_by(year, month_name, month) %>%
			summarise(mean_lat = mean(latitude)) 

		new_dat_sum		
	}
	
	sp_hab_thresholds <- c(0.5, 0.9)
	
	mean_lats_mo <- lapply(sp_hab_thresholds, mean_lat_mo)
	
	mean_lats_mo_0.5 <- mean_lats_mo[[1]] %>%
		rename(mean_lat_0.5 = mean_lat)
	
	mean_lats_mo_0.9 <- mean_lats_mo[[2]]	%>%
		rename(mean_lat_0.9 = mean_lat)
	
	mean_lats_mo_df <- merge(mean_lats_mo_0.5, mean_lats_mo_0.9, 
													 by = c("year", "month_name", "month")) 
	
	mean_lats_mo_df <- mean_lats_mo_df %>% filter(., year != 2021)
	
	

	# calculate rolling mean of mean for varying window lengths
  roll_mean_lat09_yr_func <- function(x) {
  		roll_yr_mean09 <- rollmean(mean_lats_yr$mean_lat_0.9, x, fill = NA)
  } 
  
  roll_mean_lat05_yr_func <- function(x) {
  		roll_yr_mean05 <- rollmean(mean_lats_yr$mean_lat_0.5, x, fill = NA)
  } 

  
  window_widths <- c(3, 5, 7, 9, 11, 13, 15)
  
  roll_mean_lats09 <- sapply(window_widths, roll_mean_lat09_yr_func) %>%
  	as.data.frame() %>%
  	set_names(window_widths) %>%
  	gather("window_width") %>%
  	rename(mean = value) %>%
  	mutate(year = rep((1970:2020), 7)) %>%
  	na.omit() %>%
  	mutate(threshold = "0.9")
  
   roll_mean_lats05 <- sapply(window_widths, roll_mean_lat05_yr_func) %>%
  	as.data.frame() %>%
  	set_names(window_widths) %>%
  	gather("window_width") %>%
  	rename(mean = value) %>%
  	mutate(year = rep((1970:2020), 7)) %>%
  	na.omit() %>%
  	mutate(threshold = "0.5")
   
   roll_mean_lats <- rbind(roll_mean_lats05, roll_mean_lats09)
   
  
  # plot
  roll_mean_lats$window_width <- as.numeric(roll_mean_lats$window_width)
  
  mean_vary_win <- ggplot(roll_mean_lats) +
  	geom_line(aes(x = year, y = mean)) +
  	facet_grid(threshold ~ window_width) +
  	scale_x_continuous(
  		breaks = c(1975, 1995, 2015),
  		labels = c(1975, 1995, 2015)
  	) +
  	facet_theme() +
  	ggtitle("mean latitude")
  
	ggsave("./output/plots/mean_lat_vary_win.png",
		mean_vary_win,
		width = 13, height = 7.5, units = "in")
	
	
	# rolling standard deviation ####
  roll_sd_lat09_yr_func <- function(x) {
  			
  	sd_lat09 <- NA
  
  	for(i in 6:47){
  	win <- (i - 5):(i + x)
  	sd_lat09[i] <- sd(mean_lats_yr$mean_lat_0.9[win])
  	}
  
		sd_lat09
  }
  
  nums <- c(-3, -1, 1, 3, 5, 7, 9)
  
    
  roll_sd_lat09 <- sapply(nums, roll_sd_lat09_yr_func) %>%
  	as.data.frame() %>%
  	set_names(window_widths) %>%
  	gather("window_width") %>%
  	rename(sd = value) %>%
  	mutate(year = rep((1970:2016), 7)) %>%
  	na.omit() %>%
  	mutate(threshold = "0.9")

  
  roll_sd_lat05_yr_func <- function(x) {
  			
  	sd_lat05 <- NA
  
  	for(i in 6:47){
  	win <- (i - 5):(i + x)
  	sd_lat05[i] <- sd(mean_lats_yr$mean_lat_0.5[win])
  	}
  
		sd_lat05
  }
  
  nums <- c(-3, -1, 1, 3, 5, 7, 9)
  
  roll_sd_lat05 <- sapply(nums, roll_sd_lat05_yr_func) %>%
  	as.data.frame() %>%
  	set_names(window_widths) %>%
  	gather("window_width") %>%
  	rename(sd = value) %>%
  	mutate(year = rep((1970:2016), 7)) %>%
  	na.omit() %>%
  	mutate(threshold = "0.5")

	roll_sd_lats <- rbind(roll_sd_lat05, roll_sd_lat09)
  
	# plot
  roll_sd_lats$window_width <- as.numeric(roll_sd_lats$window_width)

  sd_vary_win <- ggplot(roll_sd_lats) +
  	geom_line(aes(x = year, y = sd)) +
  	facet_grid(threshold ~ window_width) +
  	scale_x_continuous(
  		breaks = c(1975, 1995, 2015),
  		labels = c(1975, 1995, 2015)
  	) +
  	facet_theme() +
  	ggtitle("sd of mean latitude")
  
	ggsave("./output/plots/sd_lat_vary_win.png",
		sd_vary_win,
		width = 13, height = 7.5, units = "in")
  
	
  # calculate rolling mean of sd 
  roll_sd_yr_func <- function(x) {
  		roll_yr_sd <- rollmean(yr_stats$sd_sp_hab_suit, x, fill = NA)
			roll_yr_sd
  }
  
  roll_sds <- sapply(window_widths, roll_sd_yr_func) %>%
  	as.data.frame() %>%
  	set_names(window_widths) %>%
  	gather("window_width") %>%
  	rename(sd = value) %>%
  	mutate(year = rep((1970:2020), 7)) %>%
  	na.omit()
  
  # plot
  roll_sds$window_width <- as.numeric(roll_sds$window_width)
  
  sd_vary_win <- ggplot(roll_sds) +
  	geom_line(aes(x = year, y = sd)) +
  	facet_wrap(~ window_width, nrow = 4) +
  	facet_theme() 

  ggsave("./output/plots/sd_vary_win.png",
		sd_vary_win,
		width = 20, height = 10, units = "in")

	
	## month ####
  
	# summarize spawning habitat suitability by year 
  mo_stats <- ROMS_hindcast_dat %>%
	group_by(year, month) %>%
	summarise(mean_sp_hab_suit = mean(sp_hab_suit),
						sd_sp_hab_suit = sd(sp_hab_suit)) %>%
  filter(., year != 2021)

	# add month names
  mo_stats$month_name <- NA
  mo_stats$month_name[mo_stats$month == 1] <- "January"
  mo_stats$month_name[mo_stats$month == 2] <- "February"
	mo_stats$month_name[mo_stats$month == 3] <- "March"
	mo_stats$month_name[mo_stats$month == 4] <- "April"

	# calculate rolling mean of mean over 11-yr window and turn into df
	roll_mean_func <- function(x){
 	
		new_dat <- mo_stats %>% filter(month_name == x)
  	roll_mean_mo <- rollmean(new_dat$mean_sp_hab_suit, 11, fill = NA) 
  	roll_mean_mo
  
	}
 
	months <- as.character(unique(mo_stats$month_name))
	month_no <- 1:4
 
	mo_mean_dfs <- lapply(months, roll_mean_func) %>% set_names(months, )

	mo_means <- bind_rows(mo_mean_dfs) %>%
 		gather(key = "month") %>%
 		mutate(year = rep(1970:2020, 4)) %>%
 		rename(roll_mean = value)

	# calculate rolling mean of sd over 11-yr window and turn into df
	roll_sd_func <- function(x){
 	
		new_dat <- mo_stats %>% filter(month_name == x)
  	roll_sd_mo <- rollmean(new_dat$sd_sp_hab_suit, 11, fill = NA) 
  	roll_sd_mo
  	
	}
 
	mo_sd_dfs <- lapply(months, roll_sd_func) %>% set_names(months)

	mo_sds <- bind_rows(mo_sd_dfs) %>%
		gather(key = "month") %>%
		mutate(year = rep(1970:2020, 4),
					 month_no = rep(1:4, 51)) %>%
		rename(roll_sd = value)
 
	# combine both into one df
	mo_stats_4plot <- merge(mo_means, mo_sds, by = c("month", "year")) %>%
 		na.omit()

	# reorder for plotting
	mo_stats_4plot$month <- factor(mo_stats_4plot$month)
  mo_stats_4plot$month <- fct_reorder(mo_stats_4plot$month, 
  																		mo_stats_4plot$month_no)

 # plots 
 
  # mean
  rolling_mean_plot_mo <- 
		ggplot(mo_stats_4plot) +
		geom_line(aes(x = year, y = roll_mean)) +
  	facet_wrap(~ month, ncol = 3, nrow = 3) +
		xlab("Year") +
		ylab("11-year rolling mean") +
  	facet_theme()
    	  
  ggsave("./output/plots/rolling_mean_plot_mo.png",
		rolling_mean_plot_mo,
		width = 15, height = 10, units = "in")

	# sd
	rolling_sd_plot_mo <- 
		ggplot(mo_stats_4plot) +
		geom_line(aes(x = year, y = roll_sd)) +
  	facet_wrap(~ month, ncol = 3, nrow = 3) +
		xlab("Year") +
		ylab("11-year rolling sd") +
	  facet_theme()

  ggsave("./output/plots/rolling_sd_plot_mo.png",
		rolling_sd_plot_mo,
		width = 15, height = 10, units = "in")


	#### 
  
  
  
  
  # projections ####
  
  sds_proj <- NA
  
  for(i in 6:615){
  	win <- (i - 5):(i + 5)
  	sds_proj[i] <- sd(yr_stats_proj$mean_sp_hab_suit[win])
  }
  
  means_proj <- NA
  
  for(i in 6:615){
  	win <- (i - 5):(i + 5)
  	means_proj[i] <- mean(yr_stats_proj$mean_sp_hab_suit[win])
  }
  
 	years_proj <- c(rep(years_proj_hist, 3), rep(years_proj_scenario, 6))
 	
 	rolling_sd_proj <- as.data.frame(cbind(years_proj, sds_proj)) %>%
  	mutate(stat = "sds") %>%
  	rename(value = sds_proj)
  
	rolling_mean_proj <- as.data.frame(cbind(years_proj, means_proj))  %>%
  	mutate(stat = "means") %>%
		rename(value = means_proj)

  rolling_stats_proj <- bind_rows(rolling_mean_proj, rolling_sd_proj)
  
 rolling_stats_proj <- rolling_stats_proj  %>%
  	na.omit()
 	


	# plots
	
	# mean 
	rolling_mean_plot <- 
		ggplot(rolling_stats) +
		geom_line(aes(x = years, y = means_noNA)) +
		xlab("Year") +
		ylab("11-year rolling mean") +
		pos1r1_theme()

  # sd
	rolling_sd_plot <- 
		ggplot(rolling_stats) +
		geom_line(aes(x = years, y = sds_noNA)) +
		xlab("Year") +
		ylab("11-year rolling\nstandard deviation") +
		pos1r2_theme()
	
	rolling_stats_plot <- rolling_mean_plot/rolling_sd_plot
		
	ggsave("./output/plots/rolling_stats_plot.png",
		rolling_stats_plot,
		width = 15, height = 10, units = "in")
	
	
	 rolling_mean_plot <- 
		ggplot() +
	 	geom_line(data = rolling_stats_hind,
	 						aes(years_hind, value, alpha = 0.5)) +
		geom_line(data = rolling_stats_proj, 
							aes(years_proj, value, color = sim_proj, group = sim_proj, alpha = 0.5)) +
		facet_wrap(stat) + 
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
	
	
	