# plot temp projections with diff ref periods on same plot

	# ref period 1980 - 2014
	proj_temp_dat <- fread(file = here("./data/proj_temp_dat.csv"))

	yearly_temp_dat_proj <- proj_temp_dat %>% 
		group_by(simulation, projection, year) %>%
   	summarise(mean_temp = mean(bc_temp),
   						mean_sd_temp = mean(bc_temp_sd)) %>%
		mutate(ref_period = "1980-2014") %>%
		filter(projection != "historical")

	yearly_temp_dat_proj <- yearly_temp_dat_proj %>%
		tidyr::unite("sim_proj", simulation, projection, remove = F)

	# ref period 1990 - 2014
	proj_temp_dat_difrefper <- fread(file = here("./data/proj_temp_dat_difrefper.csv"))

	yearly_temp_dat_proj_drp <- proj_temp_dat_difrefper %>% 
		group_by(simulation, projection, year) %>%
   	summarise(mean_temp = mean(bc_temp),
   						mean_sd_temp = mean(bc_temp_sd))  %>%
		mutate(ref_period = "1990-2014") %>%
		filter(projection != "historical")
	
	yearly_temp_dat_proj_drp <- yearly_temp_dat_proj_drp %>%
		tidyr::unite("sim_proj", simulation, projection, remove = F)

	# ref period 2206 - 2017
	proj_temp_dat_Holsman <- fread(file = here("./data/proj_temp_dat_Holsman.csv")) 

	yearly_temp_dat_proj_Holsman <- proj_temp_dat_Holsman %>% 
		group_by(simulation, projection, year) %>%
   	summarise(mean_temp = mean(bc_temp),
   						mean_sd_temp = mean(bc_temp_sd)) %>%
		mutate(ref_period = "2006-2017") %>%
		filter(projection != "historical")

	yearly_temp_dat_proj_Holsman <- yearly_temp_dat_proj_Holsman %>%
		tidyr::unite("sim_proj", simulation, projection, remove = F)
	
	temp_dat_bias_refperiods <- bind_rows(yearly_temp_dat_proj,
																				yearly_temp_dat_proj_drp,
																				yearly_temp_dat_proj_Holsman)
	

		# plot
	compare_ref_period_plot <-    
   	ggplot() +
		geom_line(data = temp_dat_bias_refperiods,
							aes(year, mean_sd_temp, 
									group = sim_proj, 
									color = ref_period), alpha = 0.5) +
		facet_grid(projection ~ simulation) +
		xlab("Year") + 
	  scale_y_continuous(
	  	name = "Yearly-averaged temp",
	  	breaks = c(0,2,4),
	  	limits = c(-1.64, 4.78)
	  ) +
		scale_x_continuous(
	  	name = "Year",
	  	breaks = c(1980, 2030, 2080)) +
   	theme_bw() +
  	theme(
  		legend.position = "none",
			strip.background = element_blank(),
  		strip.text = element_text(size = 12, face = "bold"),
			axis.text = element_text(size = 10, colour = "grey50"),
  	  axis.ticks = element_line(colour = "grey50"),
  	  axis.line = element_line(colour = "grey50"),
  	  axis.title = element_text(size=10, color = "grey30"),
  	  panel.grid.major = element_blank(),
  	  panel.grid.minor = element_blank(),
  	  panel.border = element_rect(fill = NA, color = "grey50"))
	  
	 compare_ref_period_plot_form <- compare_ref_period_plot +
	  	geom_text(data = data.frame(x = 2025, y = 4.75, 
	  															simulation = "cesm", projection = "ssp126",
	  															label = "ref period:"),
	  					  aes(x = x, y = y, label = label), color = "black", size = 3) +
	  	geom_text(data = data.frame(x = 2025, y = 4.25, 
	  															simulation = "cesm", projection = "ssp126",
	  															label = "1980 - 2014"),
	  					  aes(x = x, y = y, label = label), color = "#1E90FF", size = 3) +
	  	geom_text(data = data.frame(x = 2025, y = 3.75, 
	  															simulation = "cesm", projection = "ssp126",
	  															label = "1990 - 2014"),
	  					  aes(x = x, y = y, label = label), color = "#ff8d1e", size = 3) +
	 	 	geom_text(data = data.frame(x = 2025, y = 3.25, 
	  															simulation = "cesm", projection = "ssp126",
	  															label = "2006 - 2017"),
	  					  aes(x = x, y = y, label = label), color = "#014421", size = 3)
	  
	   ggsave("./output/plots/compare_ref_period_plot_form.png",
			 compare_ref_period_plot_form,
			 width = 7.5, height = 4, units = "in")
