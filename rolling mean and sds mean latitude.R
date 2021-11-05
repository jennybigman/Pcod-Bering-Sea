# trying to recreate Fig 2 Litzow et al 2018 PRSB with relevant metrics


	# year ####
	
	# calculate mean latitude of spawning habitat for each threshold for year year 
	mean_lat_yr <- function(x){
		
		new_dat <- ROMS_dat_hind_trim %>%
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
		dplyr::select( - year...3, - year...5)
	

	# calculate rolling mean of mean for varying window lengths
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
  	facet_theme() +
  	ggtitle("all data")
  
	# without June
  roll_mean_yr_J_func <- function(x) {
  		roll_yr_mean <- rollmean(yr_stats_J$mean_sp_hab_suit, x, fill = NA)
			roll_yr_mean
  }
  
  roll_means_J <- sapply(window_widths, roll_mean_yr_J_func) %>%
  	as.data.frame() %>%
  	set_names(window_widths) %>%
  	gather("window_width") %>%
  	rename(mean = value) %>%
  	mutate(year = rep((1970:2020), 7)) %>%
  	na.omit()
  
  # plot
  roll_means_J$window_width <- as.numeric(roll_means_J$window_width)
  
  mean_vary_win_J <- ggplot(roll_means_J) +
  	geom_line(aes(x = year, y = mean)) +
  	facet_wrap(~ window_width, nrow = 4) +
  	facet_theme() +
  	ggtitle("no June data")
  
  # without May and June
  roll_mean_yr_MJ_func <- function(x) {
  		roll_yr_mean <- rollmean(yr_stats_MJ$mean_sp_hab_suit, x, fill = NA)
			roll_yr_mean
  }
  
  roll_means_MJ <- sapply(window_widths, roll_mean_yr_MJ_func) %>%
  	as.data.frame() %>%
  	set_names(window_widths) %>%
  	gather("window_width") %>%
  	rename(mean = value) %>%
  	mutate(year = rep((1970:2020), 7)) %>%
  	na.omit()
  
  # plot
  roll_means_MJ$window_width <- as.numeric(roll_means_MJ$window_width)
  
  mean_vary_win_MJ <- ggplot(roll_means_MJ) +
  	geom_line(aes(x = year, y = mean)) +
  	facet_wrap(~ window_width, nrow = 4) +
  	facet_theme() +
  	ggtitle("no May & June data")
  
  # plot together
  roll_mean_v_win <- mean_vary_win + mean_vary_win_J + mean_vary_win_MJ
  
  ggsave("./output/plots/mean_vary_win_plots.png",
		roll_mean_v_win,
		width = 20, height = 10, units = "in")

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
  	facet_theme() +
  	ggtitle("all data") 
  
	# without June
  roll_sd_yr_J_func <- function(x) {
  		roll_yr_sd <- rollmean(yr_stats_J$sd_sp_hab_suit, x, fill = NA)
			roll_yr_sd
  }
  
  roll_sds_J <- sapply(window_widths, roll_sd_yr_J_func) %>%
  	as.data.frame() %>%
  	set_names(window_widths) %>%
  	gather("window_width") %>%
  	rename(sd = value) %>%
  	mutate(year = rep((1970:2020), 7)) %>%
  	na.omit()
  
  # plot
  roll_sds_J$window_width <- as.numeric(roll_sds_J$window_width)
  
  sd_vary_win_J <- ggplot(roll_sds_J) +
  	geom_line(aes(x = year, y = sd)) +
  	facet_wrap(~ window_width, nrow = 4) +
  	facet_theme() +
  	ggtitle("no June data")
  
  # without May and June
  roll_sd_yr_MJ_func <- function(x) {
  		roll_yr_sd <- rollmean(yr_stats_MJ$sd_sp_hab_suit, x, fill = NA)
			roll_yr_sd
  }
  
  roll_sds_MJ <- sapply(window_widths, roll_sd_yr_MJ_func) %>%
  	as.data.frame() %>%
  	set_names(window_widths) %>%
  	gather("window_width") %>%
  	rename(sd = value) %>%
  	mutate(year = rep((1970:2020), 7)) %>%
  	na.omit()
  
  # plot
  roll_sds_MJ$window_width <- as.numeric(roll_sds_MJ$window_width)
  
  sd_vary_win_MJ <- ggplot(roll_sds_MJ) +
  	geom_line(aes(x = year, y = sd)) +
  	facet_wrap(~ window_width, nrow = 4) +
  	facet_theme() +
  	ggtitle("no May & June data")
  
  # plot together
  roll_sd_v_win <- sd_vary_win + sd_vary_win_J + sd_vary_win_MJ
  
  ggsave("./output/plots/sd_vary_win_plots.png",
		roll_sd_v_win,
		width = 20, height = 10, units = "in")

	
	## month ####
  
	# summarize spawning habitat suitability by year 
  mo_stats <- ROMS_dat_hind_trim %>%
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
	mo_stats$month_name[mo_stats$month == 5] <- "May"
	mo_stats$month_name[mo_stats$month == 6] <- "June"
	
	# calculate rolling mean of mean over 11-yr window and turn into df
	roll_mean_func <- function(x){
 	
		new_dat <- mo_stats %>% filter(month_name == x)
  	roll_mean_mo <- rollmean(new_dat$mean_sp_hab_suit, 11, fill = NA) 
  	roll_mean_mo
  
	}
 
	months <- as.character(unique(mo_stats$month_name))
	month_no <- 1:6
 
	mo_mean_dfs <- lapply(months, roll_mean_func) %>% set_names(months, )

	mo_means <- bind_rows(mo_mean_dfs) %>%
 		gather(key = "month") %>%
 		mutate(year = rep(1970:2020, 6)) %>%
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
		mutate(year = rep(1970:2020, 6),
					 month_no = rep(1:6, 51)) %>%
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

<<<<<<< HEAD
  
=======
  
  
 
>>>>>>> f545ca04b272d73d72bec7347995d58a36e6b703
