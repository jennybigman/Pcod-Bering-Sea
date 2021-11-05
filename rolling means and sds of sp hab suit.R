# trying to recreate Fig 2 Litzow et al 2018 PRSB with relevant metrics

	#### spawning habitat suitability ####
	
	# year ####
	
	# summarize spawning habitat suitability by year 
	yr_stats <- ROMS_dat_hind_trim %>%
		group_by(year) %>%
		filter(year != 2021) %>%
		summarise(mean_sp_hab_suit = mean(sp_hab_suit),
							sd_sp_hab_suit = sd(sp_hab_suit))
	
	# calculate rolling mean of mean and sd
	roll_yr_mean <- rollmean(yr_stats$mean_sp_hab_suit, 11, fill = NA)
	roll_yr_sd <- rollmean(yr_stats$sd_sp_hab_suit, 11, fill = NA)
	years <- c(1970:2020)

	rolling_stats <- data.frame(years, roll_yr_mean, roll_yr_sd) %>%
		na.omit()

	# plots
	
	# mean 
	rolling_mean_plot <- 
		ggplot(rolling_stats) +
		geom_line(aes(x = years, y = roll_yr_mean)) +
		xlab("Year") +
		ylab("11-year rolling mean") +
		pos1r1_theme()

  # sd
	rolling_sd_plot <- 
		ggplot(rolling_stats) +
		geom_line(aes(x = years, y = roll_yr_sd)) +
		xlab("Year") +
		ylab("11-year rolling\nstandard deviation") +
		pos1r2_theme()

  # without June ####
  yr_stats_J <- ROMS_dat_hind_trim %>%
  	filter(., year != 2021) %>%
		filter(month_name != "June") %>%
		group_by(year) %>%
		summarise(mean_sp_hab_suit = mean(sp_hab_suit),
							sd_sp_hab_suit = sd(sp_hab_suit))
	
  # calculate rolling mean of mean and sd
	roll_yr_mean_J <- rollmean(yr_stats_J$mean_sp_hab_suit, 11, fill = NA)
	roll_yr_sd_J <- rollmean(yr_stats_J$sd_sp_hab_suit, 11, fill = NA)

	rolling_stats_J <- data.frame(years, roll_yr_mean_J, roll_yr_sd_J) %>%
		na.omit()

	# plots
	
	# mean 
	rolling_mean_J_plot <- 
		ggplot(rolling_stats_J) +
		geom_line(aes(x = years, y = roll_yr_mean_J)) +
		xlab("Year") +
		ylab("11-year rolling mean") +
		pos2r1n_theme()

  # sd
	rolling_sd_J_plot <- 
		ggplot(rolling_stats_J) +
		geom_line(aes(x = years, y = roll_yr_sd_J)) +
		xlab("Year") +
		ylab("11-year rolling\nstandard deviation") +
		pos2r2n_theme()
  
  # without May and June ####
	yr_stats_MJ <- ROMS_dat_hind_trim %>%
  	filter(., year != 2021) %>%
		filter(month_name != "June") %>%
		filter(month_name != "May") %>%
		group_by(year) %>%
		summarise(mean_sp_hab_suit = mean(sp_hab_suit),
							sd_sp_hab_suit = sd(sp_hab_suit))
	
  # calculate rolling mean of mean and sd
	roll_yr_mean_MJ <- rollmean(yr_stats_MJ$mean_sp_hab_suit, 11, fill = NA)
	roll_yr_sd_MJ <- rollmean(yr_stats_MJ$sd_sp_hab_suit, 11, fill = NA)

	rolling_stats_MJ <- data.frame(years, roll_yr_mean_MJ, roll_yr_sd_MJ) %>%
		na.omit()

	# plots
	
	# mean 
	rolling_mean_MJ_plot <- 
		ggplot(rolling_stats_MJ) +
		geom_line(aes(x = years, y = roll_yr_mean_MJ)) +
		xlab("Year") +
		ylab("11-year rolling mean") +
		pos2r1n_theme()

  # sd
	rolling_sd_MJ_plot <- 
		ggplot(rolling_stats_MJ) +
		geom_line(aes(x = years, y = roll_yr_sd_MJ)) +
		xlab("Year") +
		ylab("11-year rolling\nstandard deviation") +
		pos2r2n_theme()
	
	# plot together
	library(patchwork)
	
	plot1 <- rolling_mean_plot + theme(plot.margin = unit(c(0.2, 0, 0, 0.2), "in")) +
		ggtitle("all data")
	plot2 <- rolling_mean_J_plot + theme(plot.margin = unit(c(0.2, 0, 0, -0.05), "in")) +
		ggtitle("exclude June data")
	plot3 <- rolling_mean_MJ_plot + theme(plot.margin = unit(c(0.2, 0.2, 0, -0.05), "in")) +
		ggtitle("exclude May & June data")
	
	plot4 <- rolling_sd_plot + theme(plot.margin = unit(c(-0.05, 0, 0.2, 0.2), "in"))
	plot5 <- rolling_sd_J_plot + theme(plot.margin = unit(c(-0.05, 0, 0.2, -0.05), "in"))
	plot6 <- rolling_sd_MJ_plot + theme(plot.margin = unit(c(-0.05, 0.2, 0.2, -0.05), "in"))
  
	plot_top <- plot1 + plot2 + plot3 + plot_layout(widths = c(1.1, 1, 1))
	plot_bottom <- plot4 + plot5 + plot6 + plot_layout(widths = c(1.1, 1, 1))
	
	rolling_stats_plot <- plot_top/plot_bottom
	
	ggsave("./output/plots/rolling_stats_plot.png",
		rolling_stats_plot,
		width = 15, height = 10, units = "in")

	
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
<<<<<<< HEAD
=======

 
  
 
>>>>>>> f545ca04b272d73d72bec7347995d58a36e6b703
