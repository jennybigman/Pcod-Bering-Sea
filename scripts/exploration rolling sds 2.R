	# summarize spawning habitat suitability by year 
	yr_stats_hind <- ROMS_hindcast_dat %>%
		group_by(year) %>%
		summarise(mean_sp_hab_suit = mean(sp_hab_suit)) %>%
		mutate(projection = "historical")
	
	years_proj <- c(2021:2099)
	
	yr_stats_proj <- ROMS_projected_dat %>%
		group_by(projection, year) %>%
		summarise(mean_sp_hab_suit = mean(sp_hab_suit_var)) %>%
		filter(year %in% years_proj)
	
	yr_stats_ssp126 <- bind_rows(yr_stats_hind, yr_stats_proj) %>%
		filter(projection %in% c("ssp126", "historical"))
	
	yr_stats_ssp585 <- bind_rows(yr_stats_hind, yr_stats_proj) %>%
		filter(projection %in% c("ssp585", "historical"))
	
	## calculate rolling means and sds
 
	rolling_proj_func <- function(x){
 	
  	sds_proj <- NA
  	
  	for(i in 6:length(x$year)){
  		win <- (i - 5):(i + 5)
  		sds_proj[i] <- sd(x$mean_sp_hab_suit[win])
  	}
  	
  	means_proj <- NA
  	
  	for(i in 6:length(x$year)){
  		win <- (i - 5):(i + 5)
  		means_proj[i] <- mean(x$mean_sp_hab_suit[win]) }
  		
  	years_dat <- 1970:2099
  	
  	data.frame(sds_proj, means_proj, years_dat)
  	
  	}
	
	
	rolling_stats_proj <- lapply(list(yr_stats_ssp126, yr_stats_ssp585),
															 rolling_proj_func)
															 

	#### try with a 5 year window
	
	rolling_proj_func <- function(x){
 	
  	sds_proj <- NA
  	
  	for(i in 5:length(x$year)){
  		win <- (i - 4):(i + 4) 
  		sds_proj[i] <- sd(x$mean_sp_hab_suit[win])
  	}
  	
  	means_proj <- NA
  	
  	for(i in 5:length(x$year)){
  		win <- (i - 4):(i + 4)
  		means_proj[i] <- mean(x$mean_sp_hab_suit[win]) }
  		
  	years_dat <- 1970:2099
  	
  	data.frame(sds_proj, means_proj, years_dat)
  	
  	}
	
	
	rolling_stats_proj <- lapply(list(yr_stats_ssp126, yr_stats_ssp585),
															 rolling_proj_func)
	
	yr_stats_ssp126_2 <- rolling_stats_proj[[1]] %>%
		mutate(projection = "ssp126")

	yr_stats_ssp585_2 <- rolling_stats_proj[[2]] %>%
		mutate(projection = "ssp585")
	
	yr_stats <- bind_rows(yr_stats_ssp126_2, yr_stats_ssp585_2)

	#### plot ####
	
	# set up colors
	yr_stats$num_yr <- as.numeric(yr_stats$year)
	
	yr_stats <- yr_stats %>%
		mutate(cols = case_when(
			num_yr <= 2020 ~ "grey",
			num_yr > 2020 & projection == "ssp126" ~ "#537682",
			num_yr > 2020 & projection == "ssp585" ~ "#ffa77f"))

		# order facets

	yr_stats$scen <- NA
		
	yr_stats$scen[yr_stats$projection == "ssp126"] <- "low emission\n(ssp126)"
	yr_stats$scen[yr_stats$projection == "ssp585"] <- "high emission\n(ssp585)"
	
	yr_stats$scen_f = factor(yr_stats$scen, 
													 levels=c('low emission\n(ssp126)', 'high emission\n(ssp585)'))
	
	
									
	#### plot ####
	
	rolling_5sd_plot <-   
		ggplot(data = yr_stats) +
		geom_line(aes(years_dat, sds_proj, color = cols, group = projection)) +
		facet_wrap(~ scen_f) +
		scale_color_identity() +
		xlab("Year") +
	  scale_y_continuous(
	  	name = "5-year rolling standard deviation\nspawning habitat suitablity index",
	  	breaks = c(0, 0.04, 0.08),
	  	labels = c(0, 0.04, 0.08),
	  	limits = c(0, 0.082)) +
   	xlim(1970, 2099) +
    theme_bw() +
  	theme(legend.position = "none") +
		theme(
  		strip.background = element_blank(),
  		strip.text = element_text(size = 8, face = "bold", colour = "grey50"),
			axis.text = element_text(size = 6, colour = "grey50"),
  	  axis.ticks = element_line(colour = "grey50"),
  	  axis.line = element_blank(),
			axis.title.x = element_text(size = 8, color = "grey50"),
  	  axis.title.y = element_markdown(size=8, color = "grey50"),
  	  panel.grid.major = element_blank(),
  	  panel.grid.minor = element_blank(),
			panel.spacing = unit(0, "lines"),
  	  panel.border = element_rect(fill = NA, color = "grey50"))
	
	
	ggsave(here("./output/plots/rolling_5sd_plot.png"),
			 rolling_5sd_plot,
			 width = 5, height = 2.5, units = "in")

	
	## mean 
	
	rolling_5mean_plot <-   
		ggplot(data = yr_stats) +
		geom_line(aes(years_dat, means_proj, color = cols, group = projection)) +
		facet_wrap(~ scen_f) +
		scale_color_identity() +
		xlab("Year") +
	  scale_y_continuous(
	  	name = "5-year rolling mean\nspawning habitat suitablity index",
	  	breaks = c(0.25, 0.35, 0.45),
	  	labels = c(0.25, 0.35, 0.45),
	  	limits = c(0.25, 0.471)) +
   	xlim(1970, 2099) +
    theme_bw() +
  	theme(legend.position = "none") +
		theme(
  		strip.background = element_blank(),
  		strip.text = element_text(size = 8, face = "bold", colour = "grey50"),
			axis.text = element_text(size = 6, colour = "grey50"),
  	  axis.ticks = element_line(colour = "grey50"),
  	  axis.line = element_blank(),
			axis.title.x = element_text(size = 8, color = "grey50"),
  	  axis.title.y = element_markdown(size=8, color = "grey50"),
  	  panel.grid.major = element_blank(),
  	  panel.grid.minor = element_blank(),
			panel.spacing = unit(0, "lines"),
  	  panel.border = element_rect(fill = NA, color = "grey50"))
	
	
	ggsave(here("./output/plots/rolling_5mean_plot.png"),
			 rolling_5mean_plot,
			 width = 5, height = 2.5, units = "in")

