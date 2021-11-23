# rollingm mean and sd (following Litzow et al 2018)

	library(zoo)
	library(runner)
	
	#### spawning habitat suitability ####
	
	# year ####
	
	# summarize spawning habitat suitability by year 
	yr_stats <- ROMS_hindcast_dat %>%
		group_by(year) %>%
		filter(year != 2021) %>%
		summarise(mean_sp_hab_suit = mean(sp_hab_suit),
							sd_sp_hab_suit = sd(sp_hab_suit),
							CV = sd_sp_hab_suit/mean_sp_hab_suit)
	
	# plot yearly spawning habitat suitability
	annual_sp_hab_plot <- 
		ggplot(yr_stats) +
		geom_line(aes(x = year, y = mean_sp_hab_suit)) +
		xlab("Year") +
		ylab("Annual spawning\nhabitat suitability") +
		pos1r1_theme()
		
	# calculate rolling mean of mean and sd
	roll_yr_mean <- rollmean(yr_stats$mean_sp_hab_suit, 11, fill = NA)
	roll_yr_mean_sd <- rollmean(yr_stats$sd_sp_hab_suit, 11, fill = NA)
	years <- c(1970:2020)

	rolling_stats <- data.frame(years, roll_yr_mean, roll_yr_mean_sd) %>%
		na.omit()
	
	# calculate the standard deviation over 11-yr window -- not the rolling mean of the sd as above
	
	rw_sd <- runner(
		x = yr_stats$mean_sp_hab_suit,
		k = 11,
		f = function(x) { 
			sd(x)
			}
	)
	
	rw_sd <- rw_sd[11:51]
	
	rw_mean <- runner(
		x = yr_stats$mean_sp_hab_suit,
		k = 11,
		f = function(x){
			mean(x)
		}
	)
	
	rw_mean <- rw_mean[11:51]
	
	rolling_stats <- cbind(rolling_stats, rw_mean, rw_sd)
	
  
  # using Mike Litzow's code
  
  sds <- NA
  
  for(i in 6:47){
  	win <- (i - 5):(i + 5)
  	sds[i] <- sd(yr_stats$mean_sp_hab_suit[win])
  }
  
  sds_noNA <- na.omit(sds)
  
  means <- NA
  
  for(i in 6:47){
  	win <- (i - 5):(i + 5)
  	means[i] <- mean(yr_stats$mean_sp_hab_suit[win])
  }
  
  means_noNA <- na.omit(means)
	yrs_mike <- c(1975:2015)
  mike_stats <- as.data.frame(cbind(yrs_mike, means_noNA, sds_noNA))

  rolling_stats <- cbind(rolling_stats, mike_stats)
  
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
	
	# CV
	rolling_cv_plot <- 
		ggplot(yr_stats) +
		geom_line(aes(x = year, y = CV)) +
		xlab("Year") +
		ylab("CV")
	
 ### fix this #### 
	plot1 <- rolling_mean_plot + theme(plot.margin = unit(c(0.2, 0, 0.2, 0.2), "in")) 

	plot2 <- rolling_sd_plot + theme(plot.margin = unit(c(0.2, 0.2, 0.2, -0.05), "in"))
 
	plot <- plot1 + plot2 + plot_layout(widths = c(1.1, 1))

	rolling_stats_plot <- plot_top + plot_bottom
	
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
 
	months <- as.character(unique(mo_stats$month_name))
	month_no <- 1:4
 
	mo_mean_dfs <- lapply(months, roll_mean_func) %>% set_names(months)

	mo_means <- bind_rows(mo_mean_dfs) %>%
 		gather(key = "month") %>%
 		mutate(year = rep(1970:2020, 4)) %>%
 		rename(roll_mean = value) %>%
		na.omit()

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
 
	mo_sd_lists <- lapply(months, roll_sd_func) %>% 
		set_names(months) 

	mo_sds <- bind_rows(mo_sd_lists) %>%
		gather(key = "month") %>%
		na.omit() %>%
		mutate(year = rep(1975:2015, 4),
					 month_no = rep(1:4, 41)) %>%
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
  
  #### fix order ####
  
  rolling_mean_plot_mo <- 
		ggplot(mo_stats_4plot) +
		geom_line(aes(x = year, y = roll_mean)) +
  	facet_wrap(~ month, ncol = 2, nrow = 2) +
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
  	facet_wrap(~ month, ncol = 2, nrow = 2) +
		xlab("Year") +
		ylab("11-year rolling sd") +
	  facet_theme()

  ggsave("./output/plots/rolling_sd_plot_mo.png",
		rolling_sd_plot_mo,
		width = 15, height = 10, units = "in")

  
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
  	facet_theme() +
  	ggtitle("all data") 
  
	
 
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
		width = 15, height = 10, units = "in")
	
	
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
		width = 15, height = 10, units = "in")
  
	
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