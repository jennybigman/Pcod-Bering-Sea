# plot temp projections with diff ref periods on same plot

	# ref period 1980 - 2014
	proj_temp_dat <- fread(file = here("./data/proj_temp_dat.csv"))

	yearly_temp_dat_proj <- proj_temp_dat %>% 
		group_by(simulation, projection, year) %>%
   	summarise(mean_temp = mean(bc_temp),
   						mean_sd_temp = mean(bc_temp_sd)) %>%
		mutate(ref = "ref1")

	yearly_temp_dat_proj <- yearly_temp_dat_proj %>%
		tidyr::unite("sim_proj", simulation, projection, remove = F)

	# ref period 1990 - 2014
	proj_temp_dat_difrefper <- fread(file = here("./data/proj_temp_dat_difrefper.csv"))

	yearly_temp_dat_proj_drp <- proj_temp_dat_difrefper %>% 
		group_by(simulation, projection, year) %>%
   	summarise(mean_temp = mean(bc_temp),
   						mean_sd_temp = mean(bc_temp_sd))  %>%
		mutate(ref = "ref2")
	
	yearly_temp_dat_proj_drp <- yearly_temp_dat_proj_drp %>%
		tidyr::unite("sim_proj", simulation, projection, remove = F)

	# ref period 2206 - 2017
	proj_temp_dat_Holsman <- fread(file = here("./data/proj_temp_dat_Holsman.csv"))

	yearly_temp_dat_proj_Holsman <- proj_temp_dat_Holsman %>% 
		group_by(simulation, projection, year) %>%
   	summarise(mean_temp = mean(bc_temp),
   						mean_sd_temp = mean(bc_temp_sd)) %>%
		mutate(ref = "ref3")

	yearly_temp_dat_proj_Holsman <- yearly_temp_dat_proj_Holsman %>%
		tidyr::unite("sim_proj", simulation, projection, remove = F)

proj_temp_plots_sd_compare_all <-    
   	ggplot() +
		geom_line(data = yearly_temp_dat_proj,
							aes(year, mean_sd_temp, 
									group = sim_proj), 
									color = "#1E90FF", alpha = 0.5) +
		geom_line(data = yearly_temp_dat_proj_drp,
							aes(year, mean_sd_temp, 
									group = sim_proj), 
									color = "#ff8d1e", alpha = 0.5) +
		geom_line(data = yearly_temp_dat_proj_Holsman,
							aes(year, mean_sd_temp, 
									group = sim_proj), 
									color = "#014421", alpha = 0.5) +
		facet_wrap(projection ~ simulation, scales = "free") +
		xlab("Year") + 
	  scale_y_continuous(
	  	name = "Yearly-averaged temp",
	  	breaks = c(0,2,4,6),
	  ) +
		scale_x_continuous(
	  	name = "Year",
	  	breaks = c(1980, 2030, 2080)) +
   	theme_bw() +
  	theme(
			strip.background = element_blank(),
  		strip.text = element_text(size = 12, face = "bold"),
			axis.text = element_text(size = 10, colour = "grey50"),
  	  axis.ticks = element_line(colour = "grey50"),
  	  axis.line = element_line(colour = "grey50"),
  	  axis.title = element_text(size=10, color = "grey30"),
  	  panel.grid.major = element_blank(),
  	  panel.grid.minor = element_blank(),
  	  panel.border = element_rect(fill = NA, color = "grey50"))
	  
	 proj_temp_plots_sd_compare_all <- proj_temp_plots_sd_compare_all +
	  	geom_text(data = data.frame(x = 2011.7, y = -1.1, 
	  															simulation = "cesm", projection = "historical",
	  															label = "ref period:"),
	  					  aes(x = x, y = y, label = label), color = "black", size = 3) +
	  	geom_text(data = data.frame(x = 2012, y = -1.4, 
	  															simulation = "cesm", projection = "historical",
	  															label = "1980 - 2014"),
	  					  aes(x = x, y = y, label = label), color = "#1E90FF", size = 3) +
	  	geom_text(data = data.frame(x = 2012, y = -1.7, 
	  															simulation = "cesm", projection = "historical",
	  															label = "1990 - 2014"),
	  					  aes(x = x, y = y, label = label), color = "#ff8d1e", size = 3) +
	 	 	geom_text(data = data.frame(x = 2012, y = -2.0, 
	  															simulation = "cesm", projection = "historical",
	  															label = "2006 - 2017"),
	  					  aes(x = x, y = y, label = label), color = "#014421", size = 3)
	  
	   ggsave("./output/plots/proj_temp_plots_sd_compare_all.png",
			 proj_temp_plots_sd_compare_all,
			 width = 15, height = 8, units = "in")
	
