# figure -- correlation between spawning habitat suitability and recruitment

	# habitat suitability index
  	
	yearly_hab_dat_hind <- ROMS_hindcast_dat %>%
		group_by(year) %>%
    summarise(mean_hab_suit = mean(sp_hab_suit))
	
	yearly_hab_dat_proj <- ROMS_projected_dat %>% 
		group_by(simulation, projection, year) %>%
   	summarise(mean_hab_suit = mean(sp_hab_suit_var)) 

	### plot
	
	recruitment_dat <- read_csv(here("./data/SAFE_Pcod_EBS_2021_Recruits.csv")) %>% 
		na.omit() %>%
		mutate(raw_recruits = recruits * 1000,
					 log_raw_recruits = log(raw_recruits),
					 log_stdev = log(st_dev),
					 lower = log(raw_recruits - st_dev),
					 upper = log(raw_recruits + st_dev))
	
	years_recruit <- (min(recruitment_dat$year):max(recruitment_dat$year))
	hab_suit_recruit <- yearly_hab_dat_hind %>% filter(year %in% years_recruit)
	recruit_habsuit <- merge(hab_suit_recruit, recruitment_dat, by = "year")

	recruit_habsuit_en <- recruit_habsuit %>%
		filter(model == "ensemble")
	
	cor(recruit_habsuit_en$mean_hab_suit, recruit_habsuit_en$log_raw_recruits, 
			method = "pearson")
	
	ggscatter(recruit_habsuit_en, x = "mean_hab_suit", y = "log_raw_recruits", 
         add = "reg.line", conf.int = TRUE, 
         cor.coef = TRUE, cor.method = "pearson",
         xlab = "index of spawning\nhabitat suitability", ylab = "Log(abundance)") +
					white_theme()
	
	recruit_habsuit_plot <-
		ggplot(data = recruit_habsuit_en, aes(x = mean_hab_suit, y = log_raw_recruits)) +
		geom_point() +
		scale_x_continuous(
			breaks = c(0.2, 0.3, 0.4),
			labels = c(0.2, 0.3, 0.4),
			name = "Annual spawning habitat suitbility") +
		scale_y_continuous(
			breaks = c(18, 19, 20, 21),
			labels = c(18, 19, 20, 21),
			limits = c(18, 21),
			name = "Log(abundance)") +
		geom_errorbar(aes(x = mean_hab_suit, ymin = lower, ymax = upper), size = 4) +
		theme_bw() +
		annotate("text", label = "Pearson's correlation\ncoefficient = -0.25",
						 y = 21, x = 0.4, size = 3, alpha = 0.7) +
		theme(
			axis.text = element_text(size = 10, colour = "grey50"),
  	  axis.ticks = element_line(colour = "grey50"),
  	  axis.line = element_line(colour = "grey50"),
			axis.title.x = element_text(size = 12, color = "grey50"),
  	  axis.title.y = element_markdown(size=12, color = "grey50"),
  	  panel.grid.major = element_blank(),
  	  panel.grid.minor = element_blank(),
  	  panel.border = element_rect(fill = NA, color = "grey50"))
			

	ggsave("./output/plots/recruit_habsuit_plot.png",
			 recruit_habsuit_plot,
			 width = 5, height = 5, units = "in")
	
	
	recruit_habsuit_plot_bb <-
		ggplot(data = recruit_habsuit_en, 
					 aes(x = mean_hab_suit, y = log_raw_recruits)) +
		geom_point(color = "white") +
		scale_x_continuous(
			breaks = c(0.2, 0.3, 0.4),
			labels = c(0.2, 0.3, 0.4),
			name = "Annual spawning habitat suitability") +
		scale_y_continuous(
			breaks = c(18, 19, 20, 21),
			labels = c(18, 19, 20, 21),
			limits = c(18, 21),
			name = "Log(abundance)") +
		geom_errorbar(aes(x = mean_hab_suit, ymin = lower, ymax = upper), size = 4) +
		annotate("text", label = "Pearson's correlation\ncoefficient = -0.25",
						 y = 21, x = 0.4, size = 3, alpha = 0.7, color = "white") +
		black_theme()
			

	ggsave("./output/plots/recruit_habsuit_plot_bb.png",
			 recruit_habsuit_plot_bb,
			 width = 7, height = 5, units = "in")
	
	
	